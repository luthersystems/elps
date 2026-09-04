// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"reflect"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
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
//     fork, or between two forks; a transaction on a fork leaves the
//     template untouched; a later fork is pristine.
//
// Every fork is also checked one level deeper (a fork of the fork), as
// defence in depth against a class a single hop cannot exhibit.
//
// This line used to justify that with "a fix that survived one hop and not
// two has happened (issue #579)", which is false. #579 fails at the FIRST
// fork -- measured by reverse-applying its fix, 6ef3da5, which reddens
// TestForkPreservesValidatorCredential. The claim came from reading that
// commit's own conditional test comment ("a fix that only survived one hop
// WOULD fail here") as a historical one. The real instance, cited by
// commit and test rather than issue number: d26953a records that on a
// shared libtesting suite the fork-of-fork arm was once the only arm that
// noticed, and TestForkCheck_TestingSuitePerFork now sees it on hop one.
//
// "Reachable" means everything reachable from the package bindings: list
// and vector cells, sorted-map entries, bytes, and the environment a
// closure captured (its bindings and its parents').  What is NOT compared,
// because the oracles cannot see inside it: a native payload's contents
// (rendered by Go type only, so a stateful native that is not a
// NativeCloner is compared by the header that holds it, not by what it
// holds), and package metadata outside the symbol table (exports,
// docstrings, the function-name index).
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
	// ForkOptions are passed to every Fork call: the place to exercise
	// ForkWithNativeReplacer or ForkWithContext the way the embedder does.
	ForkOptions []lisp.ForkOption
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

// RunForkCheck runs every check described on ForkCheck.  A comparison that
// fails is reported with t.Errorf, so one run reports every transaction
// that diverges; a failure to build an environment, load the program, run
// Setup or take a fork is fatal, since nothing after it would mean
// anything.
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
		f, err := env.Fork(c.ForkOptions...)
		if err != nil {
			t.Fatalf("%s: fork: %v", what, err)
		}
		return f
	}

	tmpl := build("template")
	// The class-level oracle, over the same template (issue #598).  It
	// carries the properties this harness used to state for Fork alone:
	// the sharing-encoding fingerprint (which subsumes the alias signature
	// below, since sharing is part of the encoding) extended with the
	// per-package metadata tables, and the two-hop arm.  RunForkCheck keeps
	// its own COLD-ARM renderings, which the shared oracle has no analogue
	// for: no other walker has a "same program, loaded from scratch"
	// reference to compare against.
	for _, wit := range CheckForkTemplate(tmpl, c.ForkOptions...) {
		t.Errorf("%s", wit)
	}
	tmplState := envState(tmpl)
	tmplAlias := aliasSignature(tmpl)
	tmplIDs := payloadIDs(tmpl)

	// A fresh fork before any transaction: same state, same alias
	// structure, no shared mutable payload.
	checkFork := func(what string, f *lisp.LEnv) map[interface{}]string {
		t.Helper()
		if got := envState(f); got != tmplState {
			t.Errorf("%s: reachable state differs from the template\n%s", what, diffLines(tmplState, got))
		}
		if got := aliasSignature(f); got != tmplAlias {
			t.Errorf("%s: alias structure differs from the template (a payload reachable under two names in one is reachable under one, or under different objects, in the other)\n%s", what, diffLines(tmplAlias, got))
		}
		ids := payloadIDs(f)
		if shared := sharedPayloads(tmplIDs, ids); len(shared) > 0 {
			t.Errorf("%s: %d mutable payload(s) shared with the template: %s", what, len(shared), strings.Join(shared, ", "))
		}
		return ids
	}
	f0 := fork("fork", tmpl)
	f0IDs := checkFork("fresh fork", f0)
	checkFork("fresh fork of a fork", fork("fork of fork", f0))
	// Two forks of one template share nothing with each other either: a
	// CloneNative that hands every fork the same clone would pass the
	// template check and fail here.
	if shared := sharedPayloads(f0IDs, checkFork("second fresh fork", fork("fork", tmpl))); len(shared) > 0 {
		t.Errorf("two forks of one template share %d mutable payload(s): %s", len(shared), strings.Join(shared, ", "))
	}

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
	w := newStateWalker(&b)
	w.value(v)
	return v.Type.String() + " " + b.String()
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

// sortedBindings snapshots an environment's own bindings in key order
// (Bindings' iteration order is unspecified).
func sortedBindings(e *lisp.LEnv) (keys []string, vals map[string]*lisp.LVal) {
	vals = make(map[string]*lisp.LVal, e.NumBindings())
	for k, v := range e.Bindings() {
		keys = append(keys, k)
		vals[k] = v
	}
	sort.Strings(keys)
	return keys, vals
}

// envState renders every value reachable from every package binding, one
// line per binding, so two environments holding the same program state
// render the same text.  A closure renders with the environment it
// captured — the bindings of that environment and of its parents — since
// that is the state a fork must copy and the state a transaction can
// mutate through the closure.  Identity is not part of the rendering
// beyond what keeps it finite: within one binding, a header or environment
// reached twice renders as a back-reference to its first rendering, which
// cuts cycles; two headers over one payload, and one header reached from
// two bindings, render the payload in full each time.  aliasSignature is
// the alias-aware comparison.
func envState(env *lisp.LEnv) string {
	var b strings.Builder
	roots(env, func(pkg, name string, v *lisp.LVal) {
		fmt.Fprintf(&b, "%s:%s = ", pkg, name)
		// One walker per root: back-references cut cycles within a
		// binding, and a value reachable from two bindings renders in
		// full under each, so the rendering stays blind to header
		// identity across bindings.
		newStateWalker(&b).value(v)
		b.WriteByte('\n')
	})
	return b.String()
}

type stateWalker struct {
	sb   *strings.Builder
	seen map[*lisp.LVal]int
	envs map[*lisp.LEnv]int
}

func newStateWalker(sb *strings.Builder) *stateWalker {
	return &stateWalker{sb: sb, seen: map[*lisp.LVal]int{}, envs: map[*lisp.LEnv]int{}}
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
		w.env(funraw.Env(v))
	default:
		if len(v.Cells) == 0 {
			w.sb.WriteString(normalizeFunIDs(v.String()))
			return
		}
		fmt.Fprintf(w.sb, "%s", v.Type)
		if v.Str != "" {
			fmt.Fprintf(w.sb, "%q", v.Str)
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

// env renders a closure's captured environment chain: each environment's
// own bindings, then its parent's, up to the root.
func (w *stateWalker) env(e *lisp.LEnv) {
	if e == nil {
		return
	}
	if n, ok := w.envs[e]; ok {
		fmt.Fprintf(w.sb, " env@%d", n)
		return
	}
	w.envs[e] = len(w.envs)
	keys, vals := sortedBindings(e)
	w.sb.WriteString(" env{")
	for i, k := range keys {
		if i > 0 {
			w.sb.WriteString(" ")
		}
		w.sb.WriteString(k)
		w.sb.WriteString("=")
		w.value(vals[k])
	}
	w.sb.WriteString("}")
	w.env(e.Parent())
}

// aliasSignature renders the alias structure of everything reachable from
// the package bindings: every payload that can be mutated in place — a
// list or vector's cells, a sorted-map's storage, a bytes value's storage,
// a NativeCloner payload held by pointer, the environment a closure
// captured — is numbered on first visit and rendered as that number on
// every visit.  Two environments have the same signature exactly when,
// walking them in the same order, "same object" is true for the same pairs
// of positions.  A fork that de-aliases (issue #576) or over-aliases
// renders differently from its template here even when envState cannot
// tell them apart.
//
// A payload's contents are rendered under its first visit only: the number
// alone says "same object" on the later ones, and a shared subtree walked
// once per path in would be exponential on a diamond-shaped graph.
func aliasSignature(env *lisp.LEnv) string {
	var b strings.Builder
	w := &aliasWalker{sb: &b, ids: map[interface{}]int{}, seen: map[interface{}]bool{}}
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
	seen map[interface{}]bool
}

// id numbers an identity on first sight.
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
	var key interface{} = v
	if p, ok := mutablePayload(v); ok {
		key = p
		fmt.Fprintf(w.sb, "#%d", w.id(p))
	} else {
		w.sb.WriteString("_")
	}
	if w.seen[key] {
		return
	}
	w.seen[key] = true
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
	case lisp.LFun:
		w.env(funraw.Env(v))
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

func (w *aliasWalker) env(e *lisp.LEnv) {
	if e == nil {
		return
	}
	fmt.Fprintf(w.sb, " env#%d", w.id(e))
	if w.seen[e] {
		return
	}
	w.seen[e] = true
	keys, vals := sortedBindings(e)
	w.sb.WriteString("{")
	for i, k := range keys {
		if i > 0 {
			w.sb.WriteString(" ")
		}
		w.sb.WriteString(k)
		w.sb.WriteString("=")
		w.value(vals[k])
	}
	w.sb.WriteString("}")
	w.env(e.Parent())
}

// mutablePayload returns the identity of the storage a value can be
// mutated through, when it has one.  Sealed values are immutable by
// contract and may legitimately be shared, so they carry no identity.  So
// does a native payload unless it is a NativeCloner held by pointer: Fork
// shares every other native by reference by design (docs/fork.md), and
// keys its clone memo on pointer payloads only.  Such a native renders as
// "_" in the alias signature — its header takes part in the state
// rendering, its contents in neither.
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
		if _, ok := v.Native.(lisp.NativeCloner); !ok {
			return nil, false
		}
		rv := reflect.ValueOf(v.Native)
		if rv.Kind() != reflect.Pointer || rv.IsNil() {
			return nil, false
		}
		return v.Native, true
	default:
		if len(v.Cells) > 0 {
			return v, true
		}
	}
	return nil, false
}

// payloadIDs collects every mutable payload identity reachable from the
// package bindings — closures' captured environments included — labelled
// by the first path it was reached on.
func payloadIDs(env *lisp.LEnv) map[interface{}]string {
	out := map[interface{}]string{}
	seen := map[interface{}]bool{}
	var walk func(v *lisp.LVal, path string)
	var walkEnv func(e *lisp.LEnv, path string)
	walk = func(v *lisp.LVal, path string) {
		if v == nil {
			return
		}
		var key interface{} = v
		if p, ok := mutablePayload(v); ok {
			key = p
			if _, dup := out[p]; !dup {
				out[p] = path
			}
		}
		if seen[key] {
			return
		}
		seen[key] = true
		switch v.Type {
		case lisp.LSortMap:
			md := v.Map()
			if md == nil {
				return
			}
			for _, k := range md.Keys().Cells {
				val, _ := md.Get(k)
				walk(val, path+"/"+k.String())
			}
		case lisp.LFun:
			walkEnv(funraw.Env(v), path+"/env")
		default:
			for i, c := range v.Cells {
				walk(c, fmt.Sprintf("%s/%d", path, i))
			}
		}
	}
	walkEnv = func(e *lisp.LEnv, path string) {
		if e == nil || seen[e] {
			return
		}
		seen[e] = true
		if _, dup := out[e]; !dup {
			out[e] = path
		}
		keys, vals := sortedBindings(e)
		for _, k := range keys {
			walk(vals[k], path+"/"+k)
		}
		walkEnv(e.Parent(), path+"/parent")
	}
	roots(env, func(pkg, name string, v *lisp.LVal) {
		walk(v, pkg+":"+name)
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
