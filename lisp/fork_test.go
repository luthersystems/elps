// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"context"
	"testing"
)

// Tests for LEnv.Fork (issue #380).  Package lisp cannot import the parser
// (import cycle), so tests here build expressions with constructors and
// exercise the structural fork contract directly; behavioral tests over
// real parsed programs (closures, macros, labels) live in
// lisp/lisplib/fork_test.go, and ownership-checker behavior in
// lisp/fork_ownership_elpscheck_test.go.

// newForkTestEnv returns a root environment with the core language
// initialized on a private Runtime, positioned in the user package.
func newForkTestEnv(t testing.TB) *LEnv {
	t.Helper()
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		t.Fatalf("InitializeUserEnv failed: %v", rc)
	}
	if rc := env.InPackage(String(DefaultUserPackage)); rc.Type == LError {
		t.Fatalf("InPackage failed: %v", rc)
	}
	return env
}

// lambdaExpr builds the expression (lambda (x) x) with constructors.
func lambdaExpr() *LVal {
	return SExpr([]*LVal{
		Symbol("lambda"),
		SExpr([]*LVal{Symbol("x")}),
		Symbol("x"),
	})
}

func TestForkNilAndQuiescence(t *testing.T) {
	var nilEnv *LEnv
	if _, err := nilEnv.Fork(); err == nil {
		t.Errorf("fork of nil env did not error")
	}
	if _, err := (&LEnv{}).Fork(); err == nil {
		t.Errorf("fork of env with nil runtime did not error")
	}

	env := newForkTestEnv(t)

	// Non-empty call stack: rejected.
	env.Runtime.Stack.Frames = append(env.Runtime.Stack.Frames, CallFrame{FID: "_fun1", Name: "in-flight"})
	if _, err := env.Fork(); err == nil {
		t.Errorf("fork with non-empty call stack did not error")
	}
	env.Runtime.Stack.Frames = env.Runtime.Stack.Frames[:0]

	// Active eval entry: rejected.
	env.Runtime.evalDepth = 1
	if _, err := env.Fork(); err == nil {
		t.Errorf("fork with non-zero eval depth did not error")
	}
	env.Runtime.evalDepth = 0

	// Pending condition handlers: rejected.
	env.Runtime.conditionStack = append(env.Runtime.conditionStack, Nil())
	if _, err := env.Fork(); err == nil {
		t.Errorf("fork with pending condition handlers did not error")
	}
	env.Runtime.conditionStack = env.Runtime.conditionStack[:0]

	// Quiescent again: accepted.
	if _, err := env.Fork(); err != nil {
		t.Errorf("fork of quiescent template errored: %v", err)
	}
}

// TestForkQuiescenceInFlight proves the guard trips during a REAL
// evaluation, not only under synthetic stack surgery: a builtin that calls
// Fork mid-eval must get the quiescence error.
func TestForkQuiescenceInFlight(t *testing.T) {
	env := newForkTestEnv(t)
	var forkErr error
	forked := false
	env.AddBuiltins(true, &langBuiltin{
		name:    "fork-now",
		formals: Formals(),
		fun: func(env *LEnv, args *LVal) *LVal {
			_, forkErr = env.Fork()
			forked = forkErr == nil
			return Nil()
		},
	})
	res := env.Eval(SExpr([]*LVal{Symbol("fork-now")}))
	if res.Type == LError {
		t.Fatalf("eval: %v", res)
	}
	if forked {
		t.Fatalf("fork inside an active evaluation succeeded; want quiescence error")
	}
	if forkErr == nil {
		t.Fatalf("fork inside an active evaluation returned no error")
	}
}

func TestForkCounterContinuity(t *testing.T) {
	env := newForkTestEnv(t)
	// Advance the gensym counter before forking.  A template that never
	// gensym'd leaves both counters at zero, and "fork == template == 0"
	// passes whether or not the counter is carried across — the check has
	// to start from a non-zero template to mean anything.
	minted := map[string]bool{}
	for range 4 {
		minted[env.Runtime.GenSym()] = true
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	if got, want := uint64(env.Runtime.numenv), uint64(1); got < want {
		t.Fatalf("template env-ID counter is %d; continuity check would be vacuous", got)
	}
	if got, want := uint64(env.Runtime.numsym), uint64(1); got < want {
		t.Fatalf("template gensym counter is %d; continuity check would be vacuous", got)
	}
	if got, want := uint64(fork.Runtime.numenv), uint64(env.Runtime.numenv); got != want {
		t.Errorf("env-ID counter not continued: fork %d, template %d", got, want)
	}
	if got, want := uint64(fork.Runtime.numsym), uint64(env.Runtime.numsym); got != want {
		t.Errorf("gensym counter not continued: fork %d, template %d", got, want)
	}
	// The behavioral form of gensym continuity: a name the fork mints must
	// not be one the template already handed out before the fork.
	for range 4 {
		if name := fork.Runtime.GenSym(); minted[name] {
			t.Errorf("fork re-minted the load-time gensym %q", name)
		}
	}

	// Inherited FIDs: every FID recorded anywhere in the forked registry.
	inherited := make(map[string]bool)
	for _, pkg := range fork.Runtime.Registry.packages {
		for _, fid := range pkg.funNames {
			inherited[fid] = true
		}
		for _, v := range pkg.symbols {
			if v.Type == LFun {
				if fd := v.funData(); fd != nil {
					inherited[fd.fid] = true
				}
			}
		}
	}
	if len(inherited) == 0 {
		t.Fatalf("no inherited FIDs found; test is vacuous")
	}

	// A lambda minted in the fork must not collide with any inherited FID,
	// and neither must one minted in the template after the fork.
	for _, e := range []*LEnv{fork, env} {
		fun := e.Eval(lambdaExpr())
		if fun.Type != LFun {
			t.Fatalf("lambda eval: %v", fun)
		}
		fid := fun.funData().fid
		if inherited[fid] {
			t.Errorf("post-fork lambda FID %q collides with an inherited FID", fid)
		}
	}
}

// forkAuditor pairwise-walks a template value graph and its forked
// counterpart, asserting the fork sharing contract node by node: sealed
// values and singletons are pointer-shared, everything else is
// pointer-distinct with identical content, and aliasing inside the
// template maps to the same aliasing inside the fork.
type forkAuditor struct {
	t            *testing.T
	seenVal      map[*LVal]*LVal
	seenEnv      map[*LEnv]*LEnv
	sharedSealed int
	copied       int
}

func newForkAuditor(t *testing.T) *forkAuditor {
	return &forkAuditor{
		t:       t,
		seenVal: make(map[*LVal]*LVal),
		seenEnv: make(map[*LEnv]*LEnv),
	}
}

func (a *forkAuditor) val(path string, o, n *LVal) {
	if a.t.Failed() {
		return // one structural failure floods; stop at the first
	}
	if o == nil || n == nil {
		if o != n {
			a.t.Errorf("%s: nil mismatch: template %v, fork %v", path, o, n)
		}
		return
	}
	if isSingleton(o) {
		if n != o {
			a.t.Errorf("%s: singleton not shared", path)
		}
		return
	}
	if o.sealed {
		if n != o {
			a.t.Errorf("%s: sealed value not pointer-shared", path)
		}
		a.sharedSealed++
		return
	}
	if prev, ok := a.seenVal[o]; ok {
		if n != prev {
			a.t.Errorf("%s: template aliasing not preserved in fork", path)
		}
		return
	}
	a.seenVal[o] = n
	if n == o {
		a.t.Errorf("%s: mutable value pointer-shared with template (type %v)", path, o.Type)
		return
	}
	a.copied++
	if n.Type != o.Type || n.Str != o.Str || n.Int != o.Int || n.Float != o.Float ||
		n.FunType != o.FunType || n.quoted != o.quoted || n.spliced != o.spliced {
		a.t.Errorf("%s: copied value differs from template: %v vs %v", path, n, o)
		return
	}
	// Content equality is not isolation.  A Cells slice header copied from
	// the template still points at the template's backing array, and the
	// zero-length case is invisible to a length/content comparison: elps
	// grows cell slices in place (append!, and (append 'vector seq x)
	// deliberately appends into seq's spare capacity), so a shared backing
	// is a live cross-runtime write channel even when both sides read empty.
	if sameCellsBacking(o, n) {
		a.t.Errorf("%s: Cells backing array shared with template (len=%d cap=%d)",
			path, len(o.Cells), cap(o.Cells))
		return
	}
	if len(n.Cells) != len(o.Cells) {
		a.t.Errorf("%s: cell count differs: %d vs %d", path, len(n.Cells), len(o.Cells))
		return
	}
	for i := range o.Cells {
		a.val(path+".Cells", o.Cells[i], n.Cells[i])
	}
	switch o.Type {
	case LFun:
		ofd, nfd := o.funData(), n.funData()
		if ofd == nil || nfd == nil {
			if ofd != nfd {
				a.t.Errorf("%s: fun data nil mismatch", path)
			}
			return
		}
		if nfd == ofd {
			a.t.Errorf("%s: funData pointer-shared with template", path)
			return
		}
		if nfd.fid != ofd.fid || nfd.pkg != ofd.pkg {
			a.t.Errorf("%s: fun header differs: %s/%s vs %s/%s", path, nfd.fid, nfd.pkg, ofd.fid, ofd.pkg)
		}
		a.env(path+".Env", ofd.env, nfd.env)
	default:
		if omd, ok := o.Native.(*MapData); ok && omd != nil && omd.mapBacking != nil {
			nmd, ok := n.Native.(*MapData)
			if !ok || nmd == omd {
				a.t.Errorf("%s: sorted-map storage shared with template", path)
				return
			}
			oe, ne := sortedMapEntries(omd), sortedMapEntries(nmd)
			if len(oe.Cells) != len(ne.Cells) {
				a.t.Errorf("%s: sorted-map size differs: %d vs %d", path, len(ne.Cells), len(oe.Cells))
				return
			}
			for i := range oe.Cells {
				a.val(path+".MapKey", oe.Cells[i].Cells[0], ne.Cells[i].Cells[0])
				a.val(path+".MapVal", oe.Cells[i].Cells[1], ne.Cells[i].Cells[1])
			}
		}
		if ob, ok := o.Native.(*[]byte); ok && ob != nil {
			nb, ok := n.Native.(*[]byte)
			if !ok || nb == ob {
				a.t.Errorf("%s: bytes storage shared with template", path)
				return
			}
			if !bytes.Equal(*nb, *ob) {
				a.t.Errorf("%s: bytes content differs", path)
			}
		}
	}
}

func (a *forkAuditor) env(path string, o, n *LEnv) {
	if a.t.Failed() {
		return
	}
	if o == nil || n == nil {
		if o != n {
			a.t.Errorf("%s: env nil mismatch", path)
		}
		return
	}
	if prev, ok := a.seenEnv[o]; ok {
		if n != prev {
			a.t.Errorf("%s: env aliasing not preserved in fork", path)
		}
		return
	}
	a.seenEnv[o] = n
	if n == o {
		a.t.Errorf("%s: env pointer-shared with template", path)
		return
	}
	if n.ID != o.ID {
		a.t.Errorf("%s: env ID changed: %d vs %d", path, n.ID, o.ID)
	}
	if n.Runtime == o.Runtime {
		a.t.Errorf("%s: fork env shares the template Runtime", path)
	}
	if len(n.scope) != len(o.scope) {
		a.t.Errorf("%s: scope size differs: %d vs %d", path, len(n.scope), len(o.scope))
		return
	}
	for k, ov := range o.scope {
		nv, ok := n.scope[k]
		if !ok {
			a.t.Errorf("%s: scope key %q missing in fork", path, k)
			return
		}
		a.val(path+"."+k, ov, nv)
	}
	a.env(path+".Parent", o.parent, n.parent)
}

// sameCellsBacking reports whether two values' Cells slices point at the
// same backing array.  Reslicing to cap is what makes the zero-length case
// visible: a len-0/cap-2 slice has no element to compare, but its backing
// array is exactly the memory an in-place append would write.
func sameCellsBacking(a, b *LVal) bool {
	if cap(a.Cells) == 0 || cap(b.Cells) == 0 {
		return false
	}
	return &a.Cells[:cap(a.Cells)][0] == &b.Cells[:cap(b.Cells)][0]
}

// TestForkEmptyCellsSpareCapacity pins the zero-length/spare-capacity case
// directly.  Fork copies the LVal struct, which aliases the Cells slice
// HEADER; replacing the header only when len > 0 left every empty-but-
// allocated cell slice sharing the template's backing array.  (list) is
// exactly that shape (len 0, cap 2), so the leak was reachable from lisp:
// (append 'vector e x) on the template and on a fork both wrote slot 0 of
// one array and each read the other's element.
func TestForkEmptyCellsSpareCapacity(t *testing.T) {
	env := newForkTestEnv(t)
	empty := QExpr(make([]*LVal, 0, 4))
	env.PutGlobal(Symbol("emptycap"), empty)

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fempty := fork.Runtime.Registry.packages[env.Runtime.Package.Name].symbols["emptycap"]
	if fempty == nil || fempty == empty {
		t.Fatalf("value not copied: %v", fempty)
	}
	if sameCellsBacking(empty, fempty) {
		t.Errorf("fork shares the template's empty Cells backing array (cap %d)", cap(empty.Cells))
	}
	// The observable consequence, independent of the pointer check: an
	// append on each side must not overwrite the other's element.
	empty.Cells = append(empty.Cells, Symbol("template"))
	fempty.Cells = append(fempty.Cells, Symbol("fork"))
	if empty.Cells[0].Str != "template" {
		t.Errorf("fork's append overwrote the template's cell: got %q", empty.Cells[0].Str)
	}
	if fempty.Cells[0].Str != "fork" {
		t.Errorf("template's append overwrote the fork's cell: got %q", fempty.Cells[0].Str)
	}
}

// TestForkSharingContract audits the complete forked graph against the
// template: sealed shared, mutable copied, aliasing preserved.  The floor
// assertions keep the test honest — if sharing (or copying) silently
// stopped happening the audit would still "pass" vacuously without them.
func TestForkSharingContract(t *testing.T) {
	env := newForkTestEnv(t)
	// Add mutable state so the copied fraction is non-trivial: an aliased
	// vector under two names, a nested sorted map, and a self-referential
	// vector (the cycle case the memo tables must terminate).
	vec := Array(nil, []*LVal{Int(1), Int(2)})
	env.PutGlobal(Symbol("shared-a"), vec)
	env.PutGlobal(Symbol("shared-b"), vec)
	m := SortedMap()
	if lerr := m.Map().Set(String("k"), Array(nil, []*LVal{String("v")})); lerr.Type == LError {
		t.Fatalf("map set: %v", lerr)
	}
	env.PutGlobal(Symbol("config"), m)
	cyc := SExpr([]*LVal{Nil()})
	cyc.Cells[0] = cyc
	env.PutGlobal(Symbol("cyclic"), cyc)
	// A zero-length cell slice with spare capacity — the shape whose slice
	// header the struct copy would otherwise carry into the fork.  It is here
	// so the auditor's backing-array assertion has a live case to fail on.
	env.PutGlobal(Symbol("empty-cap"), QExpr(make([]*LVal, 0, 4)))

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}

	a := newForkAuditor(t)
	a.env("root", env, fork)
	oldReg, newReg := env.Runtime.Registry, fork.Runtime.Registry
	if len(oldReg.packages) != len(newReg.packages) {
		t.Fatalf("package count differs: %d vs %d", len(newReg.packages), len(oldReg.packages))
	}
	for name, opkg := range oldReg.packages {
		npkg, ok := newReg.packages[name]
		if !ok {
			t.Fatalf("package %q missing in fork", name)
		}
		if len(npkg.symbols) != len(opkg.symbols) {
			t.Fatalf("package %q symbol count differs", name)
		}
		for sym, ov := range opkg.symbols {
			a.val("pkg:"+name+":"+sym, ov, npkg.symbols[sym])
		}
	}
	if t.Failed() {
		t.FailNow()
	}

	// Anti-vacuity floors: a loaded environment's builtin formals alone
	// contribute hundreds of sealed nodes, and the mutable state added
	// above guarantees copies happened.
	if a.sharedSealed < 100 {
		t.Errorf("only %d sealed values shared; sharing has gone vacuous", a.sharedSealed)
	}
	if a.copied < 10 {
		t.Errorf("only %d mutable values copied; audit is vacuous", a.copied)
	}

	// The remapped current package must be the fork registry's instance.
	if fork.Runtime.Package == env.Runtime.Package {
		t.Errorf("fork Runtime.Package shares the template package")
	}
	if fork.Runtime.Package.Name != env.Runtime.Package.Name {
		t.Errorf("fork package %q, template %q", fork.Runtime.Package.Name, env.Runtime.Package.Name)
	}

	// The fork-side cycle must close onto the fork-side copy.
	fcyc := fork.Runtime.Registry.packages[env.Runtime.Package.Name].symbols["cyclic"]
	if fcyc == nil || fcyc == cyc {
		t.Fatalf("cyclic value not copied")
	}
	if fcyc.Cells[0] != fcyc {
		t.Errorf("cycle in fork does not close onto the fork copy")
	}
}

// statefulNative is the stateful fake for the NativeCloner protocol tests.
type statefulNative struct {
	calls  int // mutations observed on this instance
	clones int
}

func (s *statefulNative) CloneNative() interface{} {
	s.clones++
	return &statefulNative{}
}

// plainNative implements nothing: the share-by-default class.
type plainNative struct{ n int }

func TestForkNativeCloner(t *testing.T) {
	env := newForkTestEnv(t)
	stateful := &statefulNative{calls: 7}
	plain := &plainNative{n: 3}
	env.PutGlobal(Symbol("stateful"), Native(stateful))
	env.PutGlobal(Symbol("plain"), Native(plain))

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	userPkg := env.Runtime.Package.Name
	fstateful := fork.Runtime.Registry.packages[userPkg].symbols["stateful"]
	fplain := fork.Runtime.Registry.packages[userPkg].symbols["plain"]

	// The NativeCloner payload was duplicated: fresh instance, template
	// instance untouched (its state is not inherited — CloneNative decides
	// what a clone starts with).
	got, ok := fstateful.Native.(*statefulNative)
	if !ok {
		t.Fatalf("fork stateful payload has type %T", fstateful.Native)
	}
	if got == stateful {
		t.Errorf("NativeCloner payload shared with template")
	}
	if stateful.clones != 1 {
		t.Errorf("CloneNative called %d times; want 1", stateful.clones)
	}
	if stateful.calls != 7 {
		t.Errorf("template payload mutated by fork: calls=%d", stateful.calls)
	}

	// The plain payload was shared by reference: the default policy.
	if fplain.Native.(*plainNative) != plain {
		t.Errorf("plain native payload not shared by reference")
	}
	// But the LVal header carrying it was still copied.
	if fplain == env.Runtime.Registry.packages[userPkg].symbols["plain"] {
		t.Errorf("native LVal header shared with template")
	}
}

func TestForkNativeReplacer(t *testing.T) {
	env := newForkTestEnv(t)
	stateful := &statefulNative{}
	plain := &plainNative{n: 3}
	env.PutGlobal(Symbol("stateful"), Native(stateful))
	env.PutGlobal(Symbol("plain"), Native(plain))

	replacement := &plainNative{n: 99}
	fork, err := env.Fork(ForkWithNativeReplacer(func(payload interface{}) (interface{}, bool) {
		if payload == plain {
			return replacement, true
		}
		return nil, false // fall through to NativeCloner / share
	}))
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	userPkg := env.Runtime.Package.Name
	if got := fork.Runtime.Registry.packages[userPkg].symbols["plain"].Native; got != replacement {
		t.Errorf("replacer substitution not applied: got %T %v", got, got)
	}
	// Fall-through must still consult the NativeCloner protocol.
	if stateful.clones != 1 {
		t.Errorf("replacer fall-through skipped NativeCloner: clones=%d", stateful.clones)
	}
}

func TestForkWithContext(t *testing.T) {
	env := newForkTestEnv(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	fork, err := env.Fork(ForkWithContext(ctx))
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	// The fork carries the cancelled context: evaluation must refuse.
	res := fork.Eval(lambdaExpr())
	if res.Type != LError {
		t.Fatalf("eval under cancelled context succeeded: %v", res)
	}
	if res.Str != CondContextCancelled {
		t.Errorf("condition %q; want %q", res.Str, CondContextCancelled)
	}
	// The template is unaffected — its context did not change.
	if res := env.Eval(lambdaExpr()); res.Type == LError {
		t.Errorf("template eval failed after fork: %v", res)
	}
	// A fork without the option must not inherit the template's context.
	env2 := newForkTestEnv(t)
	ctx2, cancel2 := context.WithCancel(context.Background())
	env2.evalCtx = ctx2
	cancel2()
	fork2, err := env2.Fork()
	if err != nil {
		t.Fatalf("fork2: %v", err)
	}
	if res := fork2.Eval(lambdaExpr()); res.Type == LError {
		t.Errorf("fork inherited the template's cancelled context: %v", res)
	}
}

func TestForkWithStderr(t *testing.T) {
	env := newForkTestEnv(t)
	var buf bytes.Buffer
	fork, err := env.Fork(ForkWithStderr(&buf))
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	if fork.Runtime.Stderr != &buf {
		t.Errorf("ForkWithStderr not applied")
	}
	if env.Runtime.Stderr == &buf {
		t.Errorf("template Stderr mutated by fork option")
	}
	// Default: shared.
	fork2, err := env.Fork()
	if err != nil {
		t.Fatalf("fork2: %v", err)
	}
	if fork2.Runtime.Stderr != env.Runtime.Stderr {
		t.Errorf("default fork Stderr not shared with template")
	}
}

// TestForkRuntimeConfig pins the runtime fields a fork inherits and the
// ones it deliberately does not.
func TestForkRuntimeConfig(t *testing.T) {
	env := newForkTestEnv(t)
	rt := env.Runtime
	rt.MaxAlloc = 1234
	rt.MaxMacroExpansionDepth = 77
	rt.MaxEvalNesting = 88
	rt.maxSteps = 4242
	rt.Stack.MaxHeightLogical = 11
	rt.Stack.MaxHeightPhysical = 22
	rt.Stack.MaxTailIterations = 33
	rt.Profiler = nil
	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	frt := fork.Runtime
	if frt == rt {
		t.Fatalf("fork shares the template Runtime")
	}
	if frt.MaxAlloc != 1234 || frt.MaxMacroExpansionDepth != 77 || frt.MaxEvalNesting != 88 || frt.maxSteps != 4242 {
		t.Errorf("limit configuration not inherited: %+v", frt)
	}
	if frt.Stack == rt.Stack {
		t.Errorf("fork shares the template call stack")
	}
	if frt.Stack.MaxHeightLogical != 11 || frt.Stack.MaxHeightPhysical != 22 || frt.Stack.MaxTailIterations != 33 {
		t.Errorf("stack limits not inherited: %+v", frt.Stack)
	}
	if len(frt.Stack.Frames) != 0 || frt.evalDepth != 0 || len(frt.conditionStack) != 0 {
		t.Errorf("fork runtime not quiescent at birth")
	}
	if frt.Profiler != nil || frt.Debugger != nil {
		t.Errorf("profiler/debugger travelled into the fork")
	}
	if frt.Reader != rt.Reader || frt.Library != rt.Library {
		t.Errorf("reader/library not shared with template")
	}
	if frt.Registry == rt.Registry {
		t.Errorf("fork shares the template package registry")
	}
	if frt.Registry.Lang != rt.Registry.Lang {
		t.Errorf("registry Lang not carried: %q vs %q", frt.Registry.Lang, rt.Registry.Lang)
	}
	if frt.totalSteps != 0 || frt.steps != 0 {
		t.Errorf("step accounting not fresh: steps=%d total=%d", frt.steps, frt.totalSteps)
	}
}
