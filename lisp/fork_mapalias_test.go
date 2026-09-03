// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// newForkAliasEnv returns a user-package environment with a reader attached,
// so the fixtures below can be written in ELPS.
func newForkAliasEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

// TestForkPreservesMapDataAliasAcrossHeaders pins issue #576: two distinct
// LVal headers over ONE *MapData must map to one *MapData in the fork, the
// same way two references to one *LVal map to one copy.
//
// The shape is reachable from pure ELPS.  Quote -- which quasiquote reaches
// through doUnquoteValue -- copies an unquoted value's struct (`*cp = *v`)
// and keeps the Native, so `(quasiquote (unquote a))` yields a second header
// on a's map.  Before the fix Fork memoised per *LVal only, so the two
// headers were rebuilt as two independent maps and a write through the
// fork's `a` was invisible through the fork's `b` -- a program that behaved
// one way on the template and another way in every fork of it.
func TestForkPreservesMapDataAliasAcrossHeaders(t *testing.T) {
	env := newForkAliasEnv(t)
	src := `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
(assoc! a "x" 99)
`
	if rc := env.LoadString("alias.lisp", src); rc.Type == lisp.LError {
		t.Fatalf("fixture: %v", rc)
	}
	a := env.Runtime.Package.Get(lisp.Symbol("a"))
	b := env.Runtime.Package.Get(lisp.Symbol("b"))
	if a == b {
		t.Fatalf("fixture: a and b are one header; the test needs two headers over one map")
	}
	if a.Native != b.Native {
		t.Fatalf("fixture: a and b do not share a *MapData (%p vs %p)", a.Native, b.Native)
	}
	if got := env.LoadString("read.lisp", `(get b "x")`); got.Type != lisp.LInt || got.Int != 99 {
		t.Fatalf("fixture: template write through a not visible through b: %v", got)
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fa := fork.Runtime.Package.Get(lisp.Symbol("a"))
	fb := fork.Runtime.Package.Get(lisp.Symbol("b"))
	if fa.Native == a.Native || fb.Native == b.Native {
		t.Fatalf("fork shares a *MapData with the template")
	}
	if fa.Native != fb.Native {
		t.Errorf("fork de-aliased the shared map: a=%p b=%p", fa.Native, fb.Native)
	}

	// The observable half: a write through one name is read through the
	// other, in the fork exactly as in the template.
	if got := fork.LoadString("write.lisp", `(assoc! a "y" 7) (get b "y")`); got.Type != lisp.LInt || got.Int != 7 {
		t.Errorf("fork write through a not visible through b: got %v, want 7", got)
	}
	// And the template did not see the fork's write.
	if got := env.LoadString("read2.lisp", `(get b "y")`); got.Type != lisp.LSExpr || len(got.Cells) != 0 {
		t.Errorf("template saw the fork's write: %v", got)
	}
}

// TestForkSelfReferenceThroughAliasedHeaderStaysAliased pins the same bug
// one level down: a map that contains ITSELF through a second header.  The
// *LVal memo bounds the walk (each header is memoised before its payload is
// remapped) but not the clones: without a *MapData memo seeded before the
// entries are walked, the fork's map held a second, distinct clone under
// "self" instead of closing onto itself.
func TestForkSelfReferenceThroughAliasedHeaderStaysAliased(t *testing.T) {
	env := newForkAliasEnv(t)
	m := lisp.SortedMap()
	alias := &lisp.LVal{}
	*alias = *m // a second header, same *MapData, as quasiquote makes
	if lerr := m.Map().Set(lisp.String("self"), alias); lerr.Type == lisp.LError {
		t.Fatalf("map set: %v", lerr)
	}
	env.PutGlobal(lisp.Symbol("m"), m)

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fm := fork.Runtime.Package.Get(lisp.Symbol("m"))
	if fm.Native == m.Native {
		t.Fatalf("fork shares the template's *MapData")
	}
	self, ok := fm.Map().Get(lisp.String("self"))
	if !ok {
		t.Fatalf("forked map lost its self entry")
	}
	if self.Type != lisp.LSortMap {
		t.Fatalf("forked self entry: want sorted map, got %v", self)
	}
	if self.Native != fm.Native {
		t.Errorf("forked self entry is a different map (%p) from its container (%p)", self.Native, fm.Native)
	}
}

// TestForkPreservesBytesAliasAcrossHeaders is the LBytes face of #576: two
// headers over one *[]byte were copied once per header.
func TestForkPreservesBytesAliasAcrossHeaders(t *testing.T) {
	env := newForkAliasEnv(t)
	src := `
(set 'a (to-bytes "ab"))
(set 'b (quasiquote (unquote a)))
(append! a 99)
`
	if rc := env.LoadString("bytes.lisp", src); rc.Type == lisp.LError {
		t.Fatalf("fixture: %v", rc)
	}
	a := env.Runtime.Package.Get(lisp.Symbol("a"))
	b := env.Runtime.Package.Get(lisp.Symbol("b"))
	if a == b || a.Native != b.Native {
		t.Fatalf("fixture: want two headers over one *[]byte, got a=%p b=%p natives %p %p", a, b, a.Native, b.Native)
	}
	if got := env.LoadString("read.lisp", `(length b)`); got.Type != lisp.LInt || got.Int != 3 {
		t.Fatalf("fixture: template write through a not visible through b: %v", got)
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fa := fork.Runtime.Package.Get(lisp.Symbol("a"))
	fb := fork.Runtime.Package.Get(lisp.Symbol("b"))
	if fa.Native == a.Native {
		t.Fatalf("fork shares the template's bytes")
	}
	if fa.Native != fb.Native {
		t.Errorf("fork de-aliased the shared bytes: a=%p b=%p", fa.Native, fb.Native)
	}
	if got := fork.LoadString("write.lisp", `(append! a 7) (length b)`); got.Type != lisp.LInt || got.Int != 4 {
		t.Errorf("fork write through a not visible through b: got %v, want 4", got)
	}
	if got := env.LoadString("read2.lisp", `(length b)`); got.Type != lisp.LInt || got.Int != 3 {
		t.Errorf("template saw the fork's write: %v", got)
	}
}

// countingCloner is a NativeCloner that counts its clones.
type countingCloner struct {
	clones *int
}

func (c *countingCloner) CloneNative() interface{} {
	*c.clones++
	return &countingCloner{clones: c.clones}
}

// TestForkClonesANativePayloadOncePerPayload is the native face of #576:
// two headers over one NativeCloner accumulator were cloned once per
// header, so the fork held two independent accumulators where the template
// held one.
func TestForkClonesANativePayloadOncePerPayload(t *testing.T) {
	env := newForkAliasEnv(t)
	clones := 0
	payload := &countingCloner{clones: &clones}
	a := lisp.Native(payload)
	b := &lisp.LVal{}
	*b = *a // a second header, same payload, as quasiquote makes
	env.PutGlobal(lisp.Symbol("a"), a)
	env.PutGlobal(lisp.Symbol("b"), b)

	fork, err := env.Fork()
	if err != nil {
		t.Fatalf("fork: %v", err)
	}
	fa := fork.Runtime.Package.Get(lisp.Symbol("a"))
	fb := fork.Runtime.Package.Get(lisp.Symbol("b"))
	if fa.Native == payload {
		t.Fatalf("fork shares the template's payload")
	}
	if fa.Native != fb.Native {
		t.Errorf("fork de-aliased the shared payload: a=%p b=%p", fa.Native, fb.Native)
	}
	if clones != 1 {
		t.Errorf("payload cloned %d times, want 1", clones)
	}
}
