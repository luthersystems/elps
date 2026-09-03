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
// The shape is reachable from pure ELPS.  Quote, Splice, shallowUnquote and
// opQuasiquote all copy the struct (`*cp = *v`) and keep the Native, so
// `(quasiquote (unquote a))` yields a second header on a's map.  Before the
// fix Fork memoised per *LVal only, so the two headers were rebuilt as two
// independent maps and a write through the fork's `a` was invisible through
// the fork's `b` -- a program that behaved one way on the template and
// another way in every fork of it.
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

// TestForkSelfReferenceThroughAliasedHeaderTerminates pins the sharper edge
// of the same bug: a map that contains ITSELF through a second header.  The
// *LVal memo does not see the cycle -- the walk arrives at the map through a
// header it has not visited -- so only a *MapData memo, seeded before the
// entries are walked, closes it back onto the one clone.
func TestForkSelfReferenceThroughAliasedHeaderTerminates(t *testing.T) {
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
