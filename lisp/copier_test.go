// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The controls for lisp/copier.go: each defect it fixes, as a program that
// must behave under (*LVal).Copy exactly as it behaves on a cold
// environment, and the alias guard's oracle over the walker.

func copierEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("init: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

func copierEval(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("test.lisp", src)
	if v.Type == lisp.LError {
		t.Fatalf("%s: %v", src, v)
	}
	return v
}

// copierRepro runs program on a cold environment, records what probe
// returns there (the reference), then binds a COPY of `pair` on a second
// environment under the same name and requires probe to agree, and the
// source to be untouched by it.
func copierRepro(t *testing.T, program, probe, wantOriginal string) {
	t.Helper()
	cold := copierEnv(t)
	copierEval(t, cold, program)
	want := copierEval(t, cold, probe).String()

	env := copierEnv(t)
	copierEval(t, env, program)
	src := env.GetGlobal(lisp.Symbol("pair"))
	cp := src.Copy()
	if cp.Type == lisp.LError {
		t.Fatalf("copy: %v", cp)
	}
	if rc := env.PutGlobal(lisp.Symbol("pair"), cp); rc.Type == lisp.LError {
		t.Fatalf("rebind: %v", rc)
	}
	got := copierEval(t, env, probe).String()
	if got != want {
		t.Errorf("through the copy: %s\n  cold: %s\n  copy: %s", probe, want, got)
	}
	if orig := src.String(); orig != wantOriginal {
		t.Errorf("the source moved under a write through the copy: %s, want %s", orig, wantOriginal)
	}
}

// TestCopyMemoisesMapPayloadAcrossHeaders is the #576/#585 shape under
// Copy: two names for one sorted map, a write through one, a read through
// the other.  Before lisp/copier.go the copy held two maps and read nil.
func TestCopyMemoisesMapPayloadAcrossHeaders(t *testing.T) {
	copierRepro(t,
		`(set 'a (sorted-map "k" 1)) (set 'b (quasiquote (unquote a))) (set 'pair (list a b))`,
		`(assoc! (first pair) "y" 7) (list (get (second pair) "y") (get (first pair) "y"))`,
		`'((sorted-map "k" 1) (sorted-map "k" 1))`)
}

// TestCopyMemoisesBytesPayloadAcrossHeaders is the same shape over a bytes
// value.  Before lisp/copier.go the copy SHARED the source's buffer: the
// through-the-copy read agreed with the cold run for the wrong reason, and
// the source moved.
func TestCopyMemoisesBytesPayloadAcrossHeaders(t *testing.T) {
	copierRepro(t,
		`(set 'a (to-bytes "abc")) (set 'b (quasiquote (unquote a))) (set 'pair (list a b))`,
		`(append! (first pair) 7) (list (length (second pair)) (length (first pair)))`,
		`'(#<bytes 97 98 99> #<bytes 97 98 99>)`)
}

// TestCopyRebuildsAContainerHeldAsAMapValue: a bytes value parked inside a
// map is the copy's own, so a write through the copy's map does not reach
// the source.  This is the value-walking half of the fix.
func TestCopyRebuildsAContainerHeldAsAMapValue(t *testing.T) {
	copierRepro(t,
		`(set 'buf (to-bytes "abc")) (set 'pair (list (sorted-map "raw" buf) buf))`,
		`(append! (get (first pair) "raw") 7) (list (length (second pair)) (length (get (first pair) "raw")))`,
		`'((sorted-map "raw" #<bytes 97 98 99>) #<bytes 97 98 99>)`)
}

// copierProgram is the alias guard's historical shape, as
// elpstest/aliasguard_test.go's aliasProgram: two names for one sorted map,
// two names for one bytes value, a map reaching itself through the second
// header, all nested inside a list and a map.
const copierProgram = `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
(assoc! a "self" b)
(set 'buf (to-bytes "abc"))
(set 'buf2 (quasiquote (unquote buf)))
(set 'probe (list a b buf buf2 (sorted-map "inner" a "raw" buf)))
`

// copierWalker is (*LVal).Copy as the alias guard sees a walker: a value
// copier that shares closures (no Refusal), rebuilds every container, and
// memoises what lisp/walkers.go says the copier memoises.
func copierWalker() elpstest.Walker {
	return elpstest.Walker{
		Name:     "LVal.Copy",
		Kind:     elpstest.WalkerCopy,
		Copy:     func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return v.Copy(), nil },
		Closures: elpstest.ClosuresRefused,
		Backing:  elpstest.BackingRebuilt,
		Memoises: lisp.WalkerMemoKinds("copier"),
		Doc:      "lisp/copier.go",
	}
}

// TestCopyMeetsTheAliasGuard drives (*LVal).Copy through CheckWalker: same
// values and sharing as the source, the same set of mutable payloads, and
// every probe write seen on exactly the same sites through the copy as
// through the source, with the other side untouched.  Deleting either
// payload memo in lisp/copier.go turns this red (the map memo splits the
// self-referencing map; the bytes memo splits buf from buf2), and so does
// deleting the header memo.
func TestCopyMeetsTheAliasGuard(t *testing.T) {
	t.Parallel()
	w := copierWalker()
	if len(w.Memoises) == 0 {
		t.Fatal("lisp.WalkerMemoKinds(\"copier\") is empty: the copier is not registered in lisp/walkers.go")
	}
	got, err := elpstest.CheckWalker(w, elpstest.AliasCheck{
		NewEnv: func() (*lisp.LEnv, error) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
				return nil, lisp.GoError(rc)
			}
			if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
				return nil, lisp.GoError(rc)
			}
			return env, nil
		},
		Program: copierProgram,
	})
	if err != nil {
		t.Fatalf("harness error: %v", err)
	}
	for _, wit := range got {
		t.Errorf("%s", wit)
	}
}

// TestCopyTerminatesOnACycle: a value that contains itself copies to a value
// that contains ITSELF -- the copy, not the original -- and a subtree
// reachable twice is copied once.  Copy used to recurse without bound on
// the first shape (lisp/package_admit.go classifies cycles before copying
// for that reason).
func TestCopyTerminatesOnACycle(t *testing.T) {
	t.Parallel()
	cyclic := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	cyclic.Cells = append(cyclic.Cells, cyclic)
	cp := cyclic.Copy()
	if cp == cyclic {
		t.Fatal("Copy returned the original")
	}
	if cp.Cells[1] != cp {
		t.Errorf("the copied cycle closes onto %p, want the copy %p (original %p)", cp.Cells[1], cp, cyclic)
	}

	shared := lisp.QExpr([]*lisp.LVal{lisp.Int(2)})
	pair := lisp.QExpr([]*lisp.LVal{shared, shared})
	pcp := pair.Copy()
	if pcp.Cells[0] != pcp.Cells[1] {
		t.Errorf("a subtree reachable twice was copied twice")
	}
	if pcp.Cells[0] == shared {
		t.Errorf("the copy holds the source's subtree")
	}
}

type copierCloner struct{ clones int }

func (c *copierCloner) CloneNative() interface{} { return &copierCloner{clones: c.clones + 1} }

// TestCopyClonesANativeClonerOncePerPayload: a NativeCloner reachable under
// two headers is cloned once, and the clone is shared by both copied
// headers -- the detacher's rule in copy mode.  A payload that is not a
// NativeCloner stays shared by reference.
func TestCopyClonesANativeClonerOncePerPayload(t *testing.T) {
	t.Parallel()
	payload := &copierCloner{}
	h1 := lisp.Native(payload)
	h2 := lisp.Native(payload)
	pair := lisp.QExpr([]*lisp.LVal{h1, h2})
	cp := pair.Copy()
	c0, ok0 := cp.Cells[0].Native.(*copierCloner)
	c1, ok1 := cp.Cells[1].Native.(*copierCloner)
	if !ok0 || !ok1 {
		t.Fatalf("copied natives are %T and %T", cp.Cells[0].Native, cp.Cells[1].Native)
	}
	if c0 == payload || c1 == payload {
		t.Errorf("the copy shares the NativeCloner payload with the source")
	}
	if c0 != c1 {
		t.Errorf("two headers over one NativeCloner payload were cloned twice (%p, %p)", c0, c1)
	}
	if c0.clones != 1 {
		t.Errorf("clone count %d, want 1", c0.clones)
	}

	plain := &strings.Builder{}
	ph := lisp.Native(plain)
	if got := ph.Copy().Native; got != plain {
		t.Errorf("a native payload that is not a NativeCloner must be shared by reference; got %p, want %p", got, plain)
	}
}

// TestCopySharesTheCallStack pins the one payload Copy deliberately shares:
// an LError's *CallStack, immutable by construction (CallStack.Copy
// allocates exact-length Frames at every capture site and nothing writes a
// captured stack).
func TestCopySharesTheCallStack(t *testing.T) {
	t.Parallel()
	env := copierEnv(t)
	err := env.LoadString("err.lisp", `(error 'test "boom")`)
	if err.Type != lisp.LError {
		t.Fatalf("want an error value, got %v", err)
	}
	st := err.CallStack()
	if st == nil {
		t.Skip("the error carries no call stack on this path")
	}
	if cp := err.Copy(); cp.CallStack() != st {
		t.Errorf("Copy rebuilt the call stack; it is shared by design")
	}
}

// TestCopyLeafAllocatesLikeAStructCopy pins the cost of a leaf: copying a
// value with nothing to alias allocates exactly what the struct copy
// allocates -- the copier and its inline memo live on Copy's stack.  Copy is called on leaves inside
// hot builtins (insert-sorted's binary search), and
// TestVectorBuiltinAllocations pins those counts as equalities; this is the
// same property at Copy's own boundary.
func TestCopyLeafAllocatesLikeAStructCopy(t *testing.T) {
	leaf := lisp.Int(7)
	n := testing.AllocsPerRun(200, func() { leaf.Copy() })
	if n != 1 {
		t.Errorf("copying an int allocated %v times, want 1 (the header)", n)
	}
	sym := lisp.Symbol("x")
	if n := testing.AllocsPerRun(200, func() { sym.Copy() }); n != 1 {
		t.Errorf("copying a symbol allocated %v times, want 1", n)
	}
}

// TestCopySmallWalkDoesNotAllocateAMemo: a walk that fits the inline memo
// allocates one header per node and one cells slice per list, nothing for
// the memo itself -- the copier stays on Copy's stack.
func TestCopySmallWalkDoesNotAllocateAMemo(t *testing.T) {
	list := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	n := testing.AllocsPerRun(200, func() { list.Copy() })
	// 4 headers + 1 cells slice.
	if n != 5 {
		t.Errorf("copying a 3-element list allocated %v times, want 5", n)
	}
}

// TestCopyMemoSpillsPastTheInlineArray: past the inline memo the walk
// spills to a map and stays correct -- a subtree shared across the spill
// boundary is still copied once, and a cycle still closes onto the copy.
func TestCopyMemoSpillsPastTheInlineArray(t *testing.T) {
	t.Parallel()
	shared := lisp.QExpr([]*lisp.LVal{lisp.Int(0)})
	cells := make([]*lisp.LVal, 0, 64)
	cells = append(cells, shared)
	for i := range 60 {
		cells = append(cells, lisp.Int(i))
	}
	cells = append(cells, shared)
	big := lisp.QExpr(cells)
	big.Cells = append(big.Cells, big)
	cp := big.Copy()
	if cp.Cells[0] != cp.Cells[61] {
		t.Errorf("a subtree shared across the memo spill was copied twice")
	}
	if cp.Cells[0] == shared {
		t.Errorf("the copy holds the source's subtree")
	}
	if cp.Cells[62] != cp {
		t.Errorf("the cycle closes onto %p, want the copy %p", cp.Cells[62], cp)
	}
}
