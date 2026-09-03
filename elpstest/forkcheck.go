// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"reflect"
	"regexp"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// ForkCheck is a minimal model of the template/fork pattern an embedder
// such as substrate runs: a program is loaded ONCE into a template, and
// every transaction runs on a fresh fork of it.  RunForkCheck holds that
// model to three properties, each stated against a reference that does not
// involve Fork at all, so a Fork bug cannot hide in the oracle:
//
//   - PARITY: a transaction run on a fork must produce the same result, and
//     leave the same reachable state, as the same transaction run on a cold
//     environment that loaded the program itself.  This is the whole
//     contract in one line — "a fork is indistinguishable from a full
//     load" — and it is what catches a fork that changes semantics without
//     leaking (issue #576: two names for one sorted-map became two maps in
//     the fork, so a write through one was invisible through the other).
//   - ALIASING: for every pair of reachable mutable payloads, "same object"
//     holds in the fork exactly when it holds in the template.  Parity
//     only sees an alias the transaction exercises; this sees all of them.
//   - ISOLATION: no mutable payload is shared between the template and a
//     fork, a transaction on a fork leaves the template untouched, and a
//     later fork is pristine.
//
// Every fork is also checked one level deeper (a fork of the fork), since
// a fix that survived one hop and not two has happened (issue #579).
type ForkCheck struct {
	// NewEnv builds an environment with whatever library the program needs
	// loaded and the user package selected.  It is called once for the
	// template and once per cold arm.  Nil means NewForkCheckEnv.
	NewEnv func() (*lisp.LEnv, error)
	// Program is loaded into the template, and into each cold environment.
	Program string
	// Setup, when set, runs on every environment a transaction will use:
	// each fork and each cold environment, after Program on the cold side.
	// It is the per-environment hook an embedder runs at checkout — a
	// stateful package the template must not carry, such as libtesting.
	Setup func(*lisp.LEnv) error
	// Tx are the transactions.  Each runs on its own fork, its own fork of
	// a fork, and its own cold environment.
	Tx []string
}

// NewForkCheckEnv is ForkCheck's default NewEnv: a user environment with
// the standard library loaded.
func NewForkCheckEnv() (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return env, nil
}

// RunForkCheck runs every check described on ForkCheck and reports each
// failure through t.  It never calls t.FailNow across a transaction, so one
// run reports every transaction that diverges.
func RunForkCheck(t testing.TB, c ForkCheck) {
	t.Helper()
	newEnv := c.NewEnv
	if newEnv == nil {
		newEnv = NewForkCheckEnv
	}
	build := func(what string) *lisp.LEnv {
		t.Helper()
		env, err := newEnv()
		if err != nil {
			t.Fatalf("%s: new env: %v", what, err)
		}
		if rc := env.LoadString("program.lisp", c.Program); rc.Type == lisp.LError {
			t.Fatalf("%s: program: %v", what, rc)
		}
		return env
	}
	setup := func(what string, env *lisp.LEnv) {
		t.Helper()
		if c.Setup == nil {
			return
		}
		if err := c.Setup(env); err != nil {
			t.Fatalf("%s: setup: %v", what, err)
		}
	}
	fork := func(what string, env *lisp.LEnv) *lisp.LEnv {
		t.Helper()
		f, err := env.Fork()
		if err != nil {
			t.Fatalf("%s: fork: %v", what, err)
		}
		return f
	}

	tmpl := build("template")
	tmplState := envState(tmpl)
	tmplAlias := aliasSignature(tmpl)
	tmplIDs := payloadIDs(tmpl)

	// A fresh fork before any transaction: same state, same alias
	// structure, no shared mutable payload.
	f0 := fork("fork", tmpl)
	checkFork := func(what string, f *lisp.LEnv) {
		t.Helper()
		if got := envState(f); got != tmplState {
			t.Errorf("%s: reachable state differs from the template\n%s", what, diffLines(tmplState, got))
		}
		if got := aliasSignature(f); got != tmplAlias {
			t.Errorf("%s: alias structure differs from the template (a payload reachable under two names in one is reachable under one, or under different objects, in the other)\n%s", what, diffLines(tmplAlias, got))
		}
		if shared := sharedPayloads(tmplIDs, payloadIDs(f)); len(shared) > 0 {
			t.Errorf("%s: %d mutable payload(s) shared with the template: %s", what, len(shared), strings.Join(shared, ", "))
		}
	}
	checkFork("fresh fork", f0)
	checkFork("fresh fork of a fork", fork("fork of fork", f0))

	for i, tx := range c.Tx {
		name := fmt.Sprintf("tx[%d]", i)

		cold := build(name + " cold")
		setup(name+" cold", cold)
		wantRes := renderResult(cold.LoadString("tx.lisp", tx))
		wantState := envState(cold)
		wantAlias := aliasSignature(cold)

		arms := []struct {
			what string
			env  *lisp.LEnv
		}{
			{name + " fork", fork(name, tmpl)},
			{name + " fork of fork", fork(name, fork(name, tmpl))},
		}
		for _, arm := range arms {
			setup(arm.what, arm.env)
			if got := renderResult(arm.env.LoadString("tx.lisp", tx)); got != wantRes {
				t.Errorf("%s: result differs from the cold run\n  cold: %s\n  fork: %s", arm.what, wantRes, got)
			}
			if got := envState(arm.env); got != wantState {
				t.Errorf("%s: reachable state after the transaction differs from the cold run\n%s", arm.what, diffLines(wantState, got))
			}
			if got := aliasSignature(arm.env); got != wantAlias {
				t.Errorf("%s: alias structure after the transaction differs from the cold run\n%s", arm.what, diffLines(wantAlias, got))
			}
		}

		// The template is untouched by anything the forks did, and the
		// next fork starts from the same place as the first.
		if got := envState(tmpl); got != tmplState {
			t.Errorf("%s: the template's reachable state changed\n%s", name, diffLines(tmplState, got))
		}
		checkFork(name+" fork taken afterwards", fork(name, tmpl))
	}
}

// renderResult renders a transaction result for comparison: the value's
// type and rendering, or the error text for an error.
func renderResult(v *lisp.LVal) string {
	if v.Type == lisp.LError {
		return "error: " + normalizeFunIDs(v.String())
	}
	var b strings.Builder
	w := &stateWalker{sb: &b, seen: map[*lisp.LVal]int{}}
	w.value(v)
	return v.Type.String() + " " + b.String()
}

// funIDPattern matches the environment-derived part of a lambda's name.
// Cold and fork arms allocate environment IDs on independent counters, so
// the IDs are not comparable; only that two mentions agree.
var funIDPattern = regexp.MustCompile(`_fun\d+`)

func normalizeFunIDs(s string) string {
	return funIDPattern.ReplaceAllString(s, "_fun#")
}

// roots returns every package binding in a deterministic order: package
// names sorted, symbol names sorted within each.
func roots(env *lisp.LEnv, visit func(pkg, name string, v *lisp.LVal)) {
	reg := env.Runtime.Registry
	names := reg.PackageNames()
	sort.Strings(names)
	for _, pn := range names {
		pkg := reg.Package(pn)
		if pkg == nil {
			continue
		}
		syms := pkg.SymbolNames()
		sort.Strings(syms)
		for _, sn := range syms {
			v, ok := pkg.Symbol(sn)
			if !ok || v == nil {
				continue
			}
			visit(pn, sn, v)
		}
	}
}

// envState renders every reachable value from every package binding, one
// line per binding, so two environments holding the same program state
// render the same text.  Identity is not part of the rendering: a value
// reached twice renders as a back-reference to its first rendering, which
// is what keeps cyclic structures finite and what makes this an
// alias-blind comparison (aliasSignature is the alias-aware one).
func envState(env *lisp.LEnv) string {
	var b strings.Builder
	roots(env, func(pkg, name string, v *lisp.LVal) {
		fmt.Fprintf(&b, "%s:%s = ", pkg, name)
		w := &stateWalker{sb: &b, seen: map[*lisp.LVal]int{}}
		w.value(v)
		b.WriteByte('\n')
	})
	return b.String()
}

type stateWalker struct {
	sb   *strings.Builder
	seen map[*lisp.LVal]int
}

func (w *stateWalker) value(v *lisp.LVal) {
	if v == nil {
		w.sb.WriteString("<nil>")
		return
	}
	if n, ok := w.seen[v]; ok {
		fmt.Fprintf(w.sb, "@%d", n)
		return
	}
	w.seen[v] = len(w.seen)
	switch v.Type {
	case lisp.LSortMap:
		md := v.Map()
		w.sb.WriteString("{")
		if md != nil {
			keys := md.Keys()
			for i, k := range keys.Cells {
				if i > 0 {
					w.sb.WriteString(" ")
				}
				w.sb.WriteString(k.String())
				w.sb.WriteString(":")
				val, _ := md.Get(k)
				w.value(val)
			}
		}
		w.sb.WriteString("}")
	case lisp.LBytes:
		fmt.Fprintf(w.sb, "bytes(%q)", v.Bytes())
	case lisp.LNative:
		fmt.Fprintf(w.sb, "native(%T)", v.Native)
	case lisp.LFun:
		w.sb.WriteString(normalizeFunIDs(v.String()))
	default:
		if len(v.Cells) == 0 {
			w.sb.WriteString(normalizeFunIDs(v.String()))
			return
		}
		fmt.Fprintf(w.sb, "%s[", v.Type)
		for i, c := range v.Cells {
			if i > 0 {
				w.sb.WriteString(" ")
			}
			w.value(c)
		}
		w.sb.WriteString("]")
	}
}

// aliasSignature renders the alias structure of everything reachable from
// the package bindings: every payload that can be mutated in place — a
// list or vector's cells, a sorted-map's storage, a bytes value's storage,
// a native payload held by pointer — is numbered on first visit and
// rendered as that number on every visit.  Two environments have the same
// signature exactly when, walking them in the same order, "same object" is
// true for the same pairs of positions.  A fork that de-aliases (issue
// #576) or over-aliases renders differently from its template here even
// when envState cannot tell them apart.
func aliasSignature(env *lisp.LEnv) string {
	var b strings.Builder
	w := &aliasWalker{sb: &b, ids: map[interface{}]int{}, path: map[*lisp.LVal]bool{}}
	roots(env, func(pkg, name string, v *lisp.LVal) {
		fmt.Fprintf(&b, "%s:%s = ", pkg, name)
		w.value(v)
		b.WriteByte('\n')
	})
	return b.String()
}

type aliasWalker struct {
	sb   *strings.Builder
	ids  map[interface{}]int
	path map[*lisp.LVal]bool
}

// id numbers a payload identity on first sight.
func (w *aliasWalker) id(key interface{}) int {
	n, ok := w.ids[key]
	if !ok {
		n = len(w.ids)
		w.ids[key] = n
	}
	return n
}

func (w *aliasWalker) value(v *lisp.LVal) {
	if v == nil {
		w.sb.WriteString("<nil>")
		return
	}
	if p, ok := mutablePayload(v); ok {
		fmt.Fprintf(w.sb, "#%d", w.id(p))
	} else {
		w.sb.WriteString("_")
	}
	// Recurse into children.  The walk is guarded by the path, not by
	// "seen": a payload reached twice must be rendered twice for the
	// aliasing to show, and only a cycle needs cutting.
	if w.path[v] {
		w.sb.WriteString("(cycle)")
		return
	}
	w.path[v] = true
	defer delete(w.path, v)
	switch v.Type {
	case lisp.LSortMap:
		md := v.Map()
		if md == nil {
			return
		}
		w.sb.WriteString("{")
		for i, k := range md.Keys().Cells {
			if i > 0 {
				w.sb.WriteString(" ")
			}
			val, _ := md.Get(k)
			w.value(val)
		}
		w.sb.WriteString("}")
	default:
		if len(v.Cells) == 0 {
			return
		}
		w.sb.WriteString("[")
		for i, c := range v.Cells {
			if i > 0 {
				w.sb.WriteString(" ")
			}
			w.value(c)
		}
		w.sb.WriteString("]")
	}
}

// mutablePayload returns the identity of the storage a value can be
// mutated through, when it has one.  Sealed values are immutable by
// contract and may legitimately be shared, so they carry no identity; so
// does a native payload that is not a NativeCloner, which Fork shares by
// reference by design.
func mutablePayload(v *lisp.LVal) (interface{}, bool) {
	if v.IsSealed() {
		return nil, false
	}
	switch v.Type {
	case lisp.LSortMap:
		if md := v.Map(); md != nil {
			return md, true
		}
	case lisp.LBytes:
		if p, ok := v.Native.(*[]byte); ok && p != nil {
			return p, true
		}
	case lisp.LNative:
		// A native payload is shared between template and fork by
		// reference unless it is a NativeCloner (docs/fork.md), so only a
		// cloner is a per-fork payload with an identity to keep straight.
		// A plain native still takes part in the alias signature through
		// the header that holds it.
		if _, ok := v.Native.(lisp.NativeCloner); !ok {
			return nil, false
		}
		rv := reflect.ValueOf(v.Native)
		switch rv.Kind() {
		case reflect.Ptr, reflect.Map, reflect.Slice, reflect.Chan, reflect.Func, reflect.UnsafePointer:
			if rv.IsNil() {
				return nil, false
			}
			return pointerKey{rv.Type(), rv.Pointer()}, true
		default:
			// A cloner held by value has no identity to share.
		}
	default:
		if len(v.Cells) > 0 {
			return v, true
		}
	}
	return nil, false
}

// pointerKey identifies a native payload by its type and address, so two
// distinct types at one address (a struct and its first field) stay
// distinct.
type pointerKey struct {
	t reflect.Type
	p uintptr
}

// payloadIDs collects every mutable payload identity reachable from the
// package bindings, labelled by the first path it was reached on.
func payloadIDs(env *lisp.LEnv) map[interface{}]string {
	out := map[interface{}]string{}
	var walk func(v *lisp.LVal, path string, onPath map[*lisp.LVal]bool)
	walk = func(v *lisp.LVal, path string, onPath map[*lisp.LVal]bool) {
		if v == nil || onPath[v] {
			return
		}
		if p, ok := mutablePayload(v); ok {
			if _, seen := out[p]; !seen {
				out[p] = path
			}
		}
		onPath[v] = true
		defer delete(onPath, v)
		switch v.Type {
		case lisp.LSortMap:
			md := v.Map()
			if md == nil {
				return
			}
			for _, k := range md.Keys().Cells {
				val, _ := md.Get(k)
				walk(val, path+"/"+k.String(), onPath)
			}
		default:
			for i, c := range v.Cells {
				walk(c, fmt.Sprintf("%s/%d", path, i), onPath)
			}
		}
	}
	roots(env, func(pkg, name string, v *lisp.LVal) {
		walk(v, pkg+":"+name, map[*lisp.LVal]bool{})
	})
	return out
}

// sharedPayloads lists the payload identities present in both maps, by the
// path each was first reached on in a.
func sharedPayloads(a, b map[interface{}]string) []string {
	var out []string
	for id, path := range a {
		if _, ok := b[id]; ok {
			out = append(out, path)
		}
	}
	sort.Strings(out)
	return out
}

// diffLines renders the first differing line of two multi-line renderings,
// with its line number, so a failure points at a binding rather than at a
// wall of text.
func diffLines(want, got string) string {
	wl := strings.Split(want, "\n")
	gl := strings.Split(got, "\n")
	for i := 0; i < len(wl) || i < len(gl); i++ {
		var w, g string
		if i < len(wl) {
			w = wl[i]
		}
		if i < len(gl) {
			g = gl[i]
		}
		if w != g {
			return fmt.Sprintf("  line %d\n    want: %s\n    got:  %s", i+1, clip(w), clip(g))
		}
	}
	return "  (no differing line found)"
}

func clip(s string) string {
	const maxLen = 400
	if len(s) > maxLen {
		return s[:maxLen] + "…"
	}
	return s
}
