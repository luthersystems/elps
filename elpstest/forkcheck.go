// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"reflect"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/internal/stdlib"
	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
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
// Every fork is also checked one level deeper (a fork of the fork), since
// a fix that survived one hop and not two has happened (issue #579).
//
// "Reachable" means everything reachable from the package bindings: list
// and vector cells, sorted-map entries, bytes, and the environment a
// closure captured (its bindings and its parents') and explicit builtin
// captures. Direct native pointer/map/channel/slice storage is compared;
// contents require RenderNative or meaningful transaction observations.
// Go function captures and references hidden inside structs remain opaque.
// Also not compared: package metadata outside the symbol table (exports,
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
	// SharedSetupNative declares intentionally shared immutable native pointers
	// introduced by Setup or transactions. Template-phase native references and
	// Internally declared immutable struct values are already known shared; every other native
	// reference is private by default. This asserts immutability, not permission
	// to ignore mutable handles. Use RenderNative to observe native contents.
	SharedSetupNative func(any) bool
	// RenderNative provides a deterministic, side-effect-free observation of
	// native contents, used in BOTH result and reachable-state comparisons.
	// Function closures and references hidden inside structs have no general
	// Go identity oracle: callers must observe their meaningful state here or
	// in transaction effects. A Go function code address is not closure identity.
	RenderNative func(any) string
	// TemplateOptions declare the host-code and immutable-native contracts.
	// Required for a custom NewEnv; the default factory audits its fixed core
	// and standard-library registrations. Mutable natives belong in Setup.
	TemplateOptions []lisp.TemplateOption
	// ForkOptions bind per-instance context or diagnostic output.
	ForkOptions []lisp.VMOption
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
	if rc := stdlib.Load(env, false); rc.Type == lisp.LError {
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
	state := func(env *lisp.LEnv) string { return envStateWithNative(env, c.RenderNative) }
	result := func(value *lisp.LVal) string {
		if lisp.IsInternalPanic(value) {
			t.Errorf("transaction recovered an internal Go panic: %s", value)
		}
		return renderResultWithNative(value, c.RenderNative)
	}
	newEnv := c.NewEnv
	templateOptions := c.TemplateOptions
	if newEnv == nil {
		newEnv = NewForkCheckEnv
		if templateOptions == nil {
			// This factory installs only the audited stateless core/stdlib.
			// Program-created schema callbacks declare explicit captures.
			templateOptions = []lisp.TemplateOption{lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })}
		}
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
	// Reuse one published plan for each source. Recompiling on every call
	// would hide a Fork that corrupts plan-owned storage between instances.
	published := make(map[*lisp.LEnv]*lisp.Template)
	fork := func(what string, env *lisp.LEnv) *lisp.LEnv {
		t.Helper()
		tmpl := published[env]
		if tmpl == nil {
			var err error
			tmpl, err = lisp.NewTemplate(env, templateOptions...)
			if err != nil {
				t.Fatalf("%s: publish template: %v", what, err)
			}
			published[env] = tmpl
		}
		f, err := tmpl.NewVM(c.ForkOptions...)
		if err != nil {
			t.Fatalf("%s: fork: %v", what, err)
		}
		return f
	}

	tmpl := build("template")
	// NativeCloner has no bearing on the Template contract. All native values
	// admitted before Setup are immutable, even if they also implement it.
	// Keep the approved payload itself, not only its address: Setup may remove
	// a cold arm's original binding, but its exemption remains in use (#625).
	immutableNatives := make(map[nativePayloadIdentity]any)
	defer runtime.KeepAlive(immutableNatives)
	recordImmutableNatives := func(env *lisp.LEnv) {
		for id, value := range newOracleCensus(env).natives {
			immutableNatives[id] = value
		}
	}
	sharedPrivate := func(a, b oracleCensus) []string {
		return sharedOracleCensuses(a, b, func(id nativePayloadIdentity, value any) bool {
			_, recorded := immutableNatives[id]
			return recorded || oracleDeclaredImmutable(value) || (c.SharedSetupNative != nil && c.SharedSetupNative(value))
		})
	}
	recordImmutableNatives(tmpl)
	tmplState := state(tmpl)
	tmplAlias := aliasSignature(tmpl)
	tmplIDs := newOracleCensus(tmpl)

	// A fresh fork before any transaction: same state, same alias
	// structure, no shared mutable payload.
	checkFork := func(what string, f *lisp.LEnv) oracleCensus {
		t.Helper()
		if got := state(f); got != tmplState {
			t.Errorf("%s: reachable state differs from the template\n%s", what, diffLines(tmplState, got))
		}
		if got := aliasSignature(f); got != tmplAlias {
			t.Errorf("%s: alias structure differs from the template (a payload reachable under two names in one is reachable under one, or under different objects, in the other)\n%s", what, diffLines(tmplAlias, got))
		}
		ids := newOracleCensus(f)
		if shared := sharedPrivate(tmplIDs, ids); len(shared) > 0 {
			t.Errorf("%s: %d mutable payload(s) shared with the template: %s", what, len(shared), strings.Join(shared, ", "))
		}
		return ids
	}
	f0 := fork("fork", tmpl)
	f0IDs := checkFork("fresh fork", f0)
	checkFork("fresh fork of a fork", fork("fork of fork", f0))
	// Two forks of one plan must not share instance-owned mutable storage.
	if shared := sharedPrivate(f0IDs, checkFork("second fresh fork", fork("fork", tmpl))); len(shared) > 0 {
		t.Errorf("two forks of one template share %d mutable payload(s): %s", len(shared), strings.Join(shared, ", "))
	}
	// Retain observed VMs and their physical-identity snapshots through all
	// transactions, with sharing exemptions checked on both sides. A
	// Setup hook that hands every request one mutable native would otherwise
	// pass the pre-Setup checks above. Check again after execution in case a
	// transaction introduces a new reference to shared mutable state.
	type observedVM struct {
		name string
		env  *lisp.LEnv
		ids  oracleCensus
	}
	var observed []observedVM
	observePrivateState := func(what string, env *lisp.LEnv) {
		t.Helper()
		ids := newOracleCensus(env)
		if shared := sharedPrivate(tmplIDs, ids); len(shared) > 0 {
			t.Errorf("%s: %d mutable payload(s) shared with the template: %s", what, len(shared), strings.Join(shared, ", "))
		}
		own := -1
		for i, previous := range observed {
			if previous.env == env {
				own = i
				continue
			}
			if shared := sharedPrivate(previous.ids, ids); len(shared) > 0 {
				t.Errorf("%s: %d mutable payload(s) shared with %s: %s", what, len(shared), previous.name, strings.Join(shared, ", "))
			}
		}
		entry := observedVM{what, env, ids}
		if own < 0 {
			observed = append(observed, entry)
		} else {
			observed[own] = entry
		}
	}

	for i, tx := range c.Tx {
		name := fmt.Sprintf("tx[%d]", i)

		cold := build(name + " cold")
		recordImmutableNatives(cold)
		arms := []struct {
			what string
			env  *lisp.LEnv
		}{
			{name + " fork", fork(name, tmpl)},
			{name + " fork of fork", fork(name, fork(name, tmpl))},
		}
		// Keep all freshly set-up VMs live before any transaction can remove
		// a leaked binding and conceal that they originally shared storage.
		setup(name+" cold", cold)
		observePrivateState(name+" cold", cold)
		for _, arm := range arms {
			setup(arm.what, arm.env)
			observePrivateState(arm.what, arm.env)
		}
		wantRes := result(cold.LoadString("tx.lisp", tx))
		observePrivateState(name+" cold", cold)
		wantState := state(cold)
		wantAlias := aliasSignature(cold)
		for _, arm := range arms {
			if got := result(arm.env.LoadString("tx.lisp", tx)); got != wantRes {
				t.Errorf("%s: result differs from the cold run\n  cold: %s\n  fork: %s", arm.what, wantRes, got)
			}
			if got := state(arm.env); got != wantState {
				t.Errorf("%s: reachable state after the transaction differs from the cold run\n%s", arm.what, diffLines(wantState, got))
			}
			if got := aliasSignature(arm.env); got != wantAlias {
				t.Errorf("%s: alias structure after the transaction differs from the cold run\n%s", arm.what, diffLines(wantAlias, got))
			}
			observePrivateState(arm.what, arm.env)
		}

		// The template is untouched by anything the forks did, and the
		// next fork starts from the same place as the first.
		if got := state(tmpl); got != tmplState {
			t.Errorf("%s: the template's reachable state changed\n%s", name, diffLines(tmplState, got))
		}
		checkFork(name+" fork taken afterwards", fork(name, tmpl))
	}
}

// oracleDeclaredImmutable independently mirrors the automatic admission
// boundary: a pointer inherits marker methods, but can expose writable storage
// through ordinary reflection (#635). Pointer sharing needs explicit approval.
func oracleDeclaredImmutable(value any) bool {
	_, declared := value.(templatepolicy.Immutable)
	return declared && reflect.TypeOf(value).Kind() == reflect.Struct
}

// renderResult renders a transaction result for comparison: the value's
// type and rendering, or the error text for an error.
func renderResult(v *lisp.LVal) string {
	return renderResultWithNative(v, nil)
}

func renderResultWithNative(v *lisp.LVal, renderNative func(any) string) string {
	if v == nil {
		return "<nil>"
	}
	var b strings.Builder
	w := newStateWalker(&b)
	w.renderNative = renderNative
	w.value(v)
	return v.Type.String() + " " + b.String()
}

// funIDPattern matches the environment-derived part of a lambda's name.
// Cold and fork arms allocate environment IDs on independent counters, so
// the IDs are not comparable; only that two mentions agree.
var funIDPattern = regexp.MustCompile(`^_fun\d+$`)
var validationFunIDPattern = regexp.MustCompile(`^_validation_fun_\d+$`)

func normalizeFunIDs(s string) string {
	return validationFunIDPattern.ReplaceAllString(funIDPattern.ReplaceAllString(s, "_fun#"), "_validation_fun_#")
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
	return envStateWithNative(env, nil)
}

func envStateWithNative(env *lisp.LEnv, renderNative func(any) string) string {
	var b strings.Builder
	roots(env, func(pkg, name string, v *lisp.LVal) {
		fmt.Fprintf(&b, "%s:%s = ", pkg, name)
		// One walker per root: back-references cut cycles within a
		// binding, and a value reachable from two bindings renders in
		// full under each, so the rendering stays blind to header
		// identity across bindings.
		w := newStateWalker(&b)
		w.renderNative = renderNative
		w.value(v)
		b.WriteByte('\n')
	})
	return b.String()
}

type stateWalker struct {
	sb           *strings.Builder
	seen         map[*lisp.LVal]int
	envs         map[*lisp.LEnv]int
	renderNative func(any) string
	sealed       sealedOracleState
}

func newStateWalker(sb *strings.Builder) *stateWalker {
	return &stateWalker{sb: sb, seen: map[*lisp.LVal]int{}, envs: map[*lisp.LEnv]int{}}
}

func (w *stateWalker) value(v *lisp.LVal) {
	if v == nil {
		w.sb.WriteString("<nil>")
		return
	}
	if v.IsSealed() {
		w.sealed.renderNative = w.renderNative
		fmt.Fprintf(w.sb, "sealed:%x", w.sealed.digest(v))
		return
	}
	if n, ok := w.seen[v]; ok {
		fmt.Fprintf(w.sb, "@%d", n)
		return
	}
	w.seen[v] = len(w.seen)
	fmt.Fprintf(w.sb, "%s:q%t:cells-nil%t:", v.Type, v.IsQuoted(), v.Cells == nil)
	if annotation := oracleNativeAnnotation(v); annotation != nil {
		fmt.Fprintf(w.sb, "annotation(%T)", annotation)
		if w.renderNative != nil {
			fmt.Fprintf(w.sb, "%q", w.renderNative(annotation))
		}
	}
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
		data := v.Bytes()
		fmt.Fprintf(w.sb, "bytes[nil%t:%d/%d](%q)", data == nil, len(data), cap(data), data[:cap(data)])
	case lisp.LNative:
		fmt.Fprintf(w.sb, "native(%T)", v.Native)
		if w.renderNative != nil {
			fmt.Fprintf(w.sb, "%q", w.renderNative(v.Native))
		}
	case lisp.LError:
		errorValue := (*lisp.ErrorVal)(v)
		loc, located := v.Source()
		fmt.Fprintf(w.sb, "error[%q/%q/%t/%#v]", errorValue.Condition(), errorValue.ErrorMessage(), located, loc)
		if stack := v.CallStack(); stack != nil {
			fmt.Fprintf(w.sb, "stack[%d/%d/%d/panic%t]", stack.MaxHeightLogical, stack.MaxHeightPhysical, stack.MaxTailIterations, len(stack.GoStack) != 0)
			for _, frame := range stack.Frames {
				name := frame.Name
				if name == frame.FID {
					name = normalizeFunIDs(name)
				}
				fmt.Fprintf(w.sb, "frame[%#v/%q/%q/%q/%d/%t/%t/%d]", frame.Source, frame.Package, name,
					normalizeFunIDs(frame.FID), frame.HeightLogical, frame.Terminal, frame.TROBlock, frame.TailIterations)
			}
		}
		w.cells(v.Cells)
	case lisp.LFun:
		fmt.Fprintf(w.sb, "function(%s:%s:%v)", v.Package(), normalizeFunIDs(v.FID()), v.FunType)
		w.cells(v.Cells)
		w.env(funraw.Env(v))
		if captures := funraw.Captures(v); captures != nil {
			w.sb.WriteString(" captures{")
			w.value(captures)
			w.sb.WriteString("}")
		}
	default:
		if cap(v.Cells) == 0 {
			w.sb.WriteString(v.String())
			return
		}
		fmt.Fprintf(w.sb, "%s", v.Type)
		if v.Str != "" {
			fmt.Fprintf(w.sb, "%q", v.Str)
		}
		w.cells(v.Cells)
	}
}

func (w *stateWalker) cells(cells []*lisp.LVal) {
	fmt.Fprintf(w.sb, "[%d/%d:", len(cells), cap(cells))
	for i, child := range cells[:cap(cells)] {
		if i > 0 {
			w.sb.WriteString(" ")
		}
		w.value(child)
	}
	w.sb.WriteString("]")
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
// an opaque native payload held by pointer, the environment a closure
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
	// The legacy payload signature describes content aliases. Independent
	// physical-slot/header identities also see overlapping undeclared Go views.
	b.WriteString(oracleStorageSignature(env))
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
	if v.IsSealed() {
		w.sb.WriteString("_")
		return
	}
	var key interface{} = v
	if p, ok := payloadIdentity(v); ok {
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
		if captures := funraw.Captures(v); captures != nil {
			w.sb.WriteString(" captures{")
			w.value(captures)
			w.sb.WriteString("}")
		}
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

// Native reference identity matters for alias parity whether the value is
// immutable or mutable. Isolation filters known immutable identities separately;
// a copying interface must never be used as a mutability classifier.
// payloadIdentity returns the primary payload identity; oracleValueIDs also
// includes all physical slots. Sealed Lisp values have no mutable identity.
func payloadIdentity(v *lisp.LVal) (interface{}, bool) {
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
		if ids := nativeReferenceIDs(v.Native); len(ids) > 0 {
			return ids[0], true
		}
	default:
		if len(v.Cells) > 0 {
			return v, true
		}
	}
	return nil, false
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
