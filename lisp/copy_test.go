// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// These tests cover the lisp `copy` builtin (issue #378): the within-env
// ownership primitive that hands lisp code a private, freely mutable
// version of data whose provenance it does not control.
//
// The properties under test are
//
//  1. deep-ness, per container type: mutating the copy at depth is never
//     observable through the original;
//  2. seal clearing: a copy of a program literal is mutable while the
//     literal itself stays bit-identical (the cached-parse fingerprint of
//     lisp/cow_seal_test.go);
//  3. leaf sharing: function and native leaves are the SAME *LVal in the
//     copy — the deliberate difference from detach, which rejects them.

// copyTestEnv returns an initialized environment with a reader, so that
// LoadString can parse (and therefore seal) program text.
func copyTestEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	return newCowTestEnv(t)
}

// mustEval evaluates src and fails the test on an error result.
func mustEval(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("copy_test.lisp", src)
	if v.Type == lisp.LError {
		t.Fatalf("eval %q: %v", src, v)
	}
	return v
}

// str renders a value the way the REPL would, for pristine-ness assertions.
func str(v *lisp.LVal) string { return v.String() }

// TestCopyIsDeepPerContainer mutates the copy at depth, once per mutable
// container type, and asserts the original is untouched.  Every mutation
// here is applied THROUGH the copy's nested container, so a copy that was
// deep at the top level and shallow underneath fails.
func TestCopyIsDeepPerContainer(t *testing.T) {
	for _, tc := range []struct {
		name   string
		setup  string // binds `orig`
		mutate string // mutates `cp` at depth
	}{
		{
			name:   "nested list sorted in place",
			setup:  `(set 'orig (list 'head (list 3 1 2)))`,
			mutate: `(stable-sort < (nth cp 1))`,
		},
		{
			name:   "nested vector appended",
			setup:  `(set 'orig (list 'head (vector 1 2)))`,
			mutate: `(append! (nth cp 1) 99)`,
		},
		{
			name:   "vector element vector appended",
			setup:  `(set 'orig (vector (vector 1 2) 'tail))`,
			mutate: `(append! (nth cp 0) 99)`,
		},
		{
			name:   "nested sorted-map assoc",
			setup:  `(set 'orig (sorted-map "k" (sorted-map "inner" 1)))`,
			mutate: `(assoc! (get cp "k") "inner" 2)`,
		},
		{
			name:   "nested sorted-map dissoc",
			setup:  `(set 'orig (sorted-map "k" (sorted-map "inner" 1)))`,
			mutate: `(dissoc! (get cp "k") "inner")`,
		},
		{
			name:   "map value vector appended",
			setup:  `(set 'orig (sorted-map "k" (vector 1 2)))`,
			mutate: `(append! (get cp "k") 99)`,
		},
		{
			name:   "nested bytes appended",
			setup:  `(set 'orig (list (to-bytes "ab")))`,
			mutate: `(append-bytes! (nth cp 0) (to-bytes "c"))`,
		},
		{
			name:   "map value bytes appended",
			setup:  `(set 'orig (sorted-map "k" (to-bytes "ab")))`,
			mutate: `(append-bytes! (get cp "k") (to-bytes "c"))`,
		},
		{
			name:   "deeply nested vector (depth 4)",
			setup:  `(set 'orig (list (list (sorted-map "k" (list (vector 1))))))`,
			mutate: `(append! (nth (get (nth (nth cp 0) 0) "k") 0) 99)`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env := copyTestEnv(t)
			mustEval(t, env, tc.setup)
			before := str(env.GetGlobal(lisp.Symbol("orig")))
			mustEval(t, env, `(set 'cp (copy orig))`)
			if got := str(env.GetGlobal(lisp.Symbol("cp"))); got != before {
				t.Fatalf("the copy does not print like the original: %s vs %s", got, before)
			}
			mustEval(t, env, tc.mutate)
			if got := str(env.GetGlobal(lisp.Symbol("orig"))); got != before {
				t.Errorf("mutating the copy at depth changed the original: got %s, want %s", got, before)
			}
			// Anti-vacuity the other way: the mutation must have landed
			// somewhere, i.e. the copy must now differ from the original.
			if got := str(env.GetGlobal(lisp.Symbol("cp"))); got == before {
				t.Errorf("the mutation did not take effect on the copy either (%s); the test proves nothing", got)
			}
		})
	}
}

// TestCopyClearsSeal is the seal half of the contract, checked against a
// shared cached parse: the copy of a quoted literal is mutable, and the
// mutation leaves the cached tree bit-identical.
func TestCopyClearsSeal(t *testing.T) {
	const src = `
(set 'g-nums '(9 3 7 1 8))
(set 'g-deep '(0 (3 1 2)))
`
	exprs := parseCached(t, src)
	before := fingerprintAST(exprs)

	env := copyTestEnv(t)
	for i, e := range exprs {
		if r := env.Eval(e); r.Type == lisp.LError {
			t.Fatalf("eval expr %d: %v", i, r)
		}
	}
	if v := env.GetGlobal(lisp.Symbol("g-nums")); !v.IsSealed() {
		t.Fatalf("anti-vacuity: the quoted literal is not sealed")
	}

	// The copy is unsealed at every depth...
	cp := mustEval(t, env, `(copy g-deep)`)
	seen := make(map[*lisp.LVal]bool)
	walkAST(cp, seen, func(v *lisp.LVal) {
		if v.IsSealed() {
			t.Errorf("copy left a sealed node in the result: %v", v)
		}
	})
	if len(seen) < 5 {
		t.Fatalf("anti-vacuity: walked only %d nodes of the copy", len(seen))
	}

	// ...and therefore sorts in place, at depth, without copy-on-write.
	pristine := str(env.GetGlobal(lisp.Symbol("g-deep")))
	mustEval(t, env, `(set 'mine (copy g-deep))`)
	sorted := mustEval(t, env, `(stable-sort < (nth mine 1))`)
	if sorted != env.GetGlobal(lisp.Symbol("mine")).Cells[1] {
		t.Errorf("stable-sort copied the nested list of a copy instead of sorting it in place")
	}
	if got := str(sorted); got != `(1 2 3)` {
		t.Errorf("in-place sort at depth on a copy: got %s", got)
	}
	mustEval(t, env, `(stable-sort > (copy g-nums))`)

	if after := fingerprintAST(exprs); after != before {
		t.Errorf("the cached parse changed after mutating copies of its literals:\n before %s\n after  %s", before, after)
	}
	if got := str(env.GetGlobal(lisp.Symbol("g-deep"))); got != pristine {
		t.Errorf("the literal itself changed: %s (was %s)", got, pristine)
	}
}

// TestCopySortsInPlaceWithoutCopyOnWrite pins the interaction with the
// sealed-mutation (copy-on-write) sites: on a copy they never trigger, so
// stable-sort returns the very value it was handed, and append 'vector
// keeps sharing backing with it.
func TestCopySortsInPlaceWithoutCopyOnWrite(t *testing.T) {
	env := copyTestEnv(t)
	mustEval(t, env, `(set 'lit '(3 1 2))`)
	mustEval(t, env, `(set 'mine (copy lit))`)

	mine := env.GetGlobal(lisp.Symbol("mine"))
	sorted := mustEval(t, env, `(stable-sort < mine)`)
	if sorted != mine {
		t.Errorf("stable-sort copied a copy: result %p is not the argument %p", sorted, mine)
	}
	if got := str(mine); got != `'(1 2 3)` {
		t.Errorf("the copy was not sorted in place: %s", got)
	}

	// The sealed original still copy-on-writes, unchanged by any of this.
	lit := env.GetGlobal(lisp.Symbol("lit"))
	cow := mustEval(t, env, `(stable-sort < lit)`)
	if cow == lit {
		t.Errorf("stable-sort mutated the sealed literal in place")
	}
	if got := str(lit); got != `'(3 1 2)` {
		t.Errorf("the sealed literal changed: %s", got)
	}
}

// TestCopySharesFunctionAndNativeLeaves is the deliberate divergence from
// detach: the shapes detach rejects are shared by identity here.
func TestCopySharesFunctionAndNativeLeaves(t *testing.T) {
	env := copyTestEnv(t)
	native := lisp.Native(new(struct{ N int }))
	env.PutGlobal(lisp.Symbol("a-native"), native)
	mustEval(t, env, `(set 'orig (list (lambda (x) x) car a-native (sorted-map "f" (lambda () 1) "n" a-native)))`)
	mustEval(t, env, `(set 'cp (copy orig))`)

	orig := env.GetGlobal(lisp.Symbol("orig"))
	cp := env.GetGlobal(lisp.Symbol("cp"))
	if cp == orig {
		t.Fatalf("copy returned its argument")
	}
	for i, name := range []string{"lambda", "builtin", "native"} {
		if orig.Cells[i] != cp.Cells[i] {
			t.Errorf("%s leaf was copied (%p vs %p); copy shares opaque leaves by reference", name, orig.Cells[i], cp.Cells[i])
		}
	}
	om, cm := orig.Cells[3], cp.Cells[3]
	if om == cm {
		t.Fatalf("the sorted-map holding them was not copied")
	}
	for _, k := range []string{"f", "n"} {
		ov, cv := om.MapGet(lisp.String(k)), cm.MapGet(lisp.String(k))
		if ov.Type == lisp.LError {
			t.Fatalf("map key %q: %v", k, ov)
		}
		if ov != cv {
			t.Errorf("map value %q was copied (%p vs %p); it is an opaque leaf", k, ov, cv)
		}
	}
	if native.Native != cp.Cells[2].Native {
		t.Errorf("the native payload itself was not preserved")
	}
}

// TestCopyPreservesInternalAliasingAndCycles asserts the walker's
// correspondence table works in copy mode: a value reachable twice is
// copied once, and a cycle becomes the same cycle in the copy — pointing
// at the COPY, never back at the original.
func TestCopyPreservesInternalAliasingAndCycles(t *testing.T) {
	env := copyTestEnv(t)
	// Shared substructure: the same vector under two keys.
	mustEval(t, env, `(set 'shared (vector 1 2))`)
	mustEval(t, env, `(set 'orig (list shared shared))`)
	mustEval(t, env, `(set 'cp (copy orig))`)
	cp := env.GetGlobal(lisp.Symbol("cp"))
	if cp.Cells[0] != cp.Cells[1] {
		t.Errorf("shared substructure was duplicated in the copy")
	}
	if cp.Cells[0] == env.GetGlobal(lisp.Symbol("shared")) {
		t.Errorf("shared substructure was not copied at all")
	}
	mustEval(t, env, `(append! (nth cp 0) 99)`)
	if got := str(env.GetGlobal(lisp.Symbol("shared"))); got != `(vector 1 2)` {
		t.Errorf("mutating the copy reached the original: %s", got)
	}
	if got := str(cp.Cells[1]); got != `(vector 1 2 99)` {
		t.Errorf("internal aliasing not preserved in the copy: %s", got)
	}

	// A cycle: a vector that contains itself.  Copying must terminate.
	mustEval(t, env, `(set 'cyc (vector 1))`)
	mustEval(t, env, `(append! cyc cyc)`)
	cyc := env.GetGlobal(lisp.Symbol("cyc"))
	ccp := mustEval(t, env, `(copy cyc)`)
	if ccp == cyc {
		t.Fatalf("copy returned the cyclic original")
	}
	inner := ccp.Cells[1].Cells[1] // vector backing: Cells[1] holds the elements
	if inner != ccp {
		t.Errorf("the copied cycle points at %p, want the copy %p (original %p)", inner, ccp, cyc)
	}
}

// TestCopyOfCopy checks the primitive composes: each copy is independent
// of every earlier one.
func TestCopyOfCopy(t *testing.T) {
	env := copyTestEnv(t)
	mustEval(t, env, `(set 'orig '(0 (1 2)))`)
	pristine := str(env.GetGlobal(lisp.Symbol("orig")))
	mustEval(t, env, `(set 'c1 (copy orig))`)
	mustEval(t, env, `(set 'c2 (copy c1))`)
	mustEval(t, env, `(set 'c3 (copy c2))`)
	mustEval(t, env, `(stable-sort > (nth c3 1))`)

	if got := str(env.GetGlobal(lisp.Symbol("c3")).Cells[1]); got != `(2 1)` {
		t.Errorf("the third copy did not sort in place: %s", got)
	}
	for _, name := range []string{"orig", "c1", "c2"} {
		if got := str(env.GetGlobal(lisp.Symbol(name))); got != pristine {
			t.Errorf("%s changed: %s (want %s)", name, got, pristine)
		}
	}
	c1 := env.GetGlobal(lisp.Symbol("c1"))
	c2 := env.GetGlobal(lisp.Symbol("c2"))
	if c1.Cells[1] == c2.Cells[1] {
		t.Errorf("copy-of-a-copy shares storage with its source")
	}
}

// TestCopyAtoms covers the leaves that carry no storage: the result must
// be equal, and (for the singletons) identical, since they are immutable.
func TestCopyAtoms(t *testing.T) {
	env := copyTestEnv(t)
	for _, tc := range []struct{ src, want string }{
		{`(copy 42)`, `42`},
		{`(copy 1.5)`, `1.5`},
		{`(copy "s")`, `"s"`},
		{`(copy 'sym)`, `'sym`},
		{`(copy :kw)`, `:kw`},
		{`(copy ())`, `()`},
		{`(copy true)`, `true`},
		{`(copy false)`, `false`},
		{`(copy (to-bytes "ab"))`, `#<bytes 97 98>`},
	} {
		if got := str(mustEval(t, env, tc.src)); got != tc.want {
			t.Errorf("%s = %s, want %s", tc.src, got, tc.want)
		}
	}
	// Singletons are shared, immutable, and unmutable from lisp.
	for _, src := range []string{`(copy ())`, `(copy true)`, `(copy false)`} {
		v := mustEval(t, env, src)
		bare := mustEval(t, env, src[6:len(src)-1])
		if v != bare {
			t.Errorf("%s allocated a new singleton (%p vs %p)", src, v, bare)
		}
	}
	// A copied string owns its own header even though Str is immutable.
	mustEval(t, env, `(set 's "abc")`)
	if a, b := env.GetGlobal(lisp.Symbol("s")), mustEval(t, env, `(copy s)`); a == b {
		t.Errorf("copy of a string returned the same *LVal")
	}
}

// TestCopyBytesDisjoint proves the bytes backing is fresh, not a reslice.
func TestCopyBytesDisjoint(t *testing.T) {
	env := copyTestEnv(t)
	mustEval(t, env, `(set 'orig (to-bytes "abc"))`)
	mustEval(t, env, `(set 'cp (copy orig))`)
	orig := env.GetGlobal(lisp.Symbol("orig"))
	cp := env.GetGlobal(lisp.Symbol("cp"))
	ob, cb := orig.Bytes(), cp.Bytes()
	if len(ob) != len(cb) {
		t.Fatalf("copy changed the length: %d vs %d", len(ob), len(cb))
	}
	if len(ob) > 0 && &ob[0] == &cb[0] {
		t.Errorf("the copy shares the original's byte backing")
	}
	mustEval(t, env, `(append-bytes! cp (to-bytes "Z"))`)
	if got := str(orig); got != `#<bytes 97 98 99>` {
		t.Errorf("appending to the copy changed the original: %s", got)
	}
}
