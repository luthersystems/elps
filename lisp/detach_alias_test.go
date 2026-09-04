// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// These tests pin issue #585, the detach/copy face of #576: two distinct
// LVal headers over ONE payload -- a *MapData, a *[]byte or a NativeCloner
// handle -- must map to one payload in the copy, the same way two references
// to one *LVal map to one copy.
//
// The shape is reachable from pure ELPS.  Quote (which quasiquote reaches
// through doUnquoteValue) copies an unquoted value's struct (`*cp = *v`) and
// keeps the Native, so `(quasiquote (unquote a))` is a second header on a's
// map.  Before the fix the detacher memoised per *LVal only, so the two
// headers were rebuilt as two independent payloads and a write through the
// copy's first header was invisible through its second -- a program that
// behaved one way on the original and another on `(copy ...)` of it.

// aliasedHeaders binds a and b in env to two headers over one payload, from
// lisp source, and checks the fixture actually has that shape.
func aliasedHeaders(t *testing.T, env *lisp.LEnv, src string) (a, b *lisp.LVal) {
	t.Helper()
	if rc := env.LoadString("alias.lisp", src); rc.Type == lisp.LError {
		t.Fatalf("fixture: %v", rc)
	}
	a = env.GetGlobal(lisp.Symbol("a"))
	b = env.GetGlobal(lisp.Symbol("b"))
	if a == b {
		t.Fatalf("fixture: a and b are one header; the test needs two headers over one payload")
	}
	if a.Native != b.Native {
		t.Fatalf("fixture: a and b do not share a payload (%p vs %p)", a.Native, b.Native)
	}
	return a, b
}

const mapAliasFixture = `
(set 'a (sorted-map))
(set 'b (quasiquote (unquote a)))
`

// TestCopyPreservesMapDataAliasAcrossHeaders is the issue's program: a
// write through the copied pair's first element is read through its second,
// exactly as it is through the original pair.
func TestCopyPreservesMapDataAliasAcrossHeaders(t *testing.T) {
	env := copyTestEnv(t)
	a, b := aliasedHeaders(t, env, mapAliasFixture)
	mustEval(t, env, `(set 'pair (copy (list a b)))`)
	pair := env.GetGlobal(lisp.Symbol("pair"))
	if pair.Cells[0].Native == a.Native || pair.Cells[1].Native == b.Native {
		t.Fatalf("the copy shares a *MapData with the original")
	}
	if pair.Cells[0].Native != pair.Cells[1].Native {
		t.Errorf("copy de-aliased the shared map: a=%p b=%p", pair.Cells[0].Native, pair.Cells[1].Native)
	}
	mustEval(t, env, `(assoc! (nth pair 0) "q" 1)`)
	if got := mustEval(t, env, `(get (nth pair 1) "q")`); got.Type != lisp.LInt || got.Int != 1 {
		t.Errorf("copy write through (nth pair 0) not visible through (nth pair 1): got %v, want 1", got)
	}
	// The original did not see the copy's write.
	if got := mustEval(t, env, `(get b "q")`); !got.IsNil() {
		t.Errorf("original saw the copy's write: %v", got)
	}
}

// TestDetachPreservesMapDataAliasAcrossHeaders is the same shape through the
// strict, cross-runtime walker that `copy` shares its memo tables with.
func TestDetachPreservesMapDataAliasAcrossHeaders(t *testing.T) {
	env := copyTestEnv(t)
	a, b := aliasedHeaders(t, env, mapAliasFixture)
	cp, err := lisp.Detach(lisp.QExpr([]*lisp.LVal{a, b}))
	if err != nil {
		t.Fatalf("detach: %v", err)
	}
	if cp.Cells[0].Native == a.Native {
		t.Fatalf("detach shares a *MapData with the original")
	}
	if cp.Cells[0].Native != cp.Cells[1].Native {
		t.Errorf("detach de-aliased the shared map: a=%p b=%p", cp.Cells[0].Native, cp.Cells[1].Native)
	}
	if lerr := cp.Cells[0].Map().Set(lisp.String("q"), lisp.Int(1)); lerr.Type == lisp.LError {
		t.Fatalf("map set: %v", lerr)
	}
	if got, ok := cp.Cells[1].Map().Get(lisp.String("q")); !ok || got.Int != 1 {
		t.Errorf("detach write through Cells[0] not visible through Cells[1]: %v (%v)", got, ok)
	}
	if _, ok := b.Map().Get(lisp.String("q")); ok {
		t.Errorf("original saw the detached copy's write")
	}
}

// TestCopySelfReferenceThroughAliasedHeaderStaysAliased pins the same bug
// one level down: a map that contains ITSELF through a second header.  The
// *LVal memo bounds the walk (each header is memoised before its payload is
// rebuilt) but not the clones: without a *MapData memo seeded before the
// entries are walked, the copy held a second, distinct clone under "self"
// instead of closing onto itself.
func TestCopySelfReferenceThroughAliasedHeaderStaysAliased(t *testing.T) {
	m := lisp.SortedMap()
	alias := &lisp.LVal{}
	*alias = *m // a second header, same *MapData, as quasiquote makes
	if lerr := m.Map().Set(lisp.String("self"), alias); lerr.Type == lisp.LError {
		t.Fatalf("map set: %v", lerr)
	}
	for _, walk := range []struct {
		name string
		fn   func(*lisp.LVal) (*lisp.LVal, error)
	}{
		{"copy", lisp.DeepCopy},
		{"detach", lisp.Detach},
	} {
		t.Run(walk.name, func(t *testing.T) {
			cp, err := walk.fn(m)
			if err != nil {
				t.Fatalf("%s: %v", walk.name, err)
			}
			if cp.Native == m.Native {
				t.Fatalf("%s shares the original's *MapData", walk.name)
			}
			self, ok := cp.Map().Get(lisp.String("self"))
			if !ok {
				t.Fatalf("%s lost the self entry", walk.name)
			}
			if self.Type != lisp.LSortMap {
				t.Fatalf("%s self entry: want sorted map, got %v", walk.name, self)
			}
			if self.Native != cp.Native {
				t.Errorf("%s self entry is a different map (%p) from its container (%p)", walk.name, self.Native, cp.Native)
			}
		})
	}
}

const bytesAliasFixture = `
(set 'a (to-bytes "ab"))
(set 'b (quasiquote (unquote a)))
`

// TestCopyPreservesBytesAliasAcrossHeaders is the LBytes face of #585: two
// headers over one *[]byte were copied once per header.
func TestCopyPreservesBytesAliasAcrossHeaders(t *testing.T) {
	env := copyTestEnv(t)
	a, _ := aliasedHeaders(t, env, bytesAliasFixture)
	mustEval(t, env, `(set 'pair (copy (list a b)))`)
	pair := env.GetGlobal(lisp.Symbol("pair"))
	if pair.Cells[0].Native == a.Native {
		t.Fatalf("the copy shares the original's bytes")
	}
	if pair.Cells[0].Native != pair.Cells[1].Native {
		t.Errorf("copy de-aliased the shared bytes: a=%p b=%p", pair.Cells[0].Native, pair.Cells[1].Native)
	}
	if got := mustEval(t, env, `(append! (nth pair 0) 99) (length (nth pair 1))`); got.Type != lisp.LInt || got.Int != 3 {
		t.Errorf("copy write through (nth pair 0) not visible through (nth pair 1): got %v, want 3", got)
	}
	if got := mustEval(t, env, `(length b)`); got.Type != lisp.LInt || got.Int != 2 {
		t.Errorf("original saw the copy's write: %v", got)
	}
}

// TestDetachPreservesBytesAliasAcrossHeaders: the same, through the strict
// walker.
func TestDetachPreservesBytesAliasAcrossHeaders(t *testing.T) {
	env := copyTestEnv(t)
	a, b := aliasedHeaders(t, env, bytesAliasFixture)
	cp, err := lisp.Detach(lisp.QExpr([]*lisp.LVal{a, b}))
	if err != nil {
		t.Fatalf("detach: %v", err)
	}
	if cp.Cells[0].Native == a.Native {
		t.Fatalf("detach shares the original's bytes")
	}
	if cp.Cells[0].Native != cp.Cells[1].Native {
		t.Errorf("detach de-aliased the shared bytes: a=%p b=%p", cp.Cells[0].Native, cp.Cells[1].Native)
	}
	*cp.Cells[0].Native.(*[]byte) = append(cp.Cells[0].Bytes(), 'c')
	if got := cp.Cells[1].Bytes(); string(got) != "abc" {
		t.Errorf("detach write through Cells[0] not visible through Cells[1]: %q", got)
	}
	if got := b.Bytes(); string(got) != "ab" {
		t.Errorf("original saw the detached copy's write: %q", got)
	}
}

// TestCopyClonesANativePayloadOncePerPayload is the native face of #585:
// two headers over one NativeCloner accumulator were cloned once per header,
// so the copy held two independent accumulators where the original held one.
func TestCopyClonesANativePayloadOncePerPayload(t *testing.T) {
	for _, walk := range []struct {
		name string
		fn   func(*lisp.LVal) (*lisp.LVal, error)
	}{
		{"copy", lisp.DeepCopy},
		{"detach", lisp.Detach},
	} {
		t.Run(walk.name, func(t *testing.T) {
			payload := &cloneableNative{state: 4}
			a := lisp.Native(payload)
			b := &lisp.LVal{}
			*b = *a // a second header, same payload, as quasiquote makes
			cp, err := walk.fn(lisp.QExpr([]*lisp.LVal{a, b}))
			if err != nil {
				t.Fatalf("%s: %v", walk.name, err)
			}
			if cp.Cells[0].Native == payload {
				t.Fatalf("%s shares the original's payload", walk.name)
			}
			if cp.Cells[0].Native != cp.Cells[1].Native {
				t.Errorf("%s de-aliased the shared payload: a=%p b=%p", walk.name, cp.Cells[0].Native, cp.Cells[1].Native)
			}
			if payload.clones != 1 {
				t.Errorf("%s cloned the payload %d times, want 1", walk.name, payload.clones)
			}
		})
	}
}

// TestCopyClonesDistinctNativePayloadsSeparately guards the memo's key: two
// headers over two DIFFERENT payloads of one type must still get two clones.
// A memo keyed on anything coarser than payload identity would merge them.
// It passes on main too: it guards the memo key, it does not pin a regression.
func TestCopyClonesDistinctNativePayloadsSeparately(t *testing.T) {
	p1 := &cloneableNative{state: 1}
	p2 := &cloneableNative{state: 2}
	cp, err := lisp.DeepCopy(lisp.QExpr([]*lisp.LVal{lisp.Native(p1), lisp.Native(p2)}))
	if err != nil {
		t.Fatalf("copy: %v", err)
	}
	if cp.Cells[0].Native == cp.Cells[1].Native {
		t.Fatalf("two distinct payloads were merged into one clone")
	}
	if got := cp.Cells[1].Native.(*cloneableNative).state; got != 2 {
		t.Errorf("second clone carries state %d, want 2", got)
	}
	if p1.clones != 1 || p2.clones != 1 {
		t.Errorf("clone counts %d/%d, want 1/1", p1.clones, p2.clones)
	}
}
