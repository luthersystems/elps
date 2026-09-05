// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #613: a literal bound by one load, captured by a second, and rebound
// by a third load of the first file.
//
// Under a LoadCache the third load serves the SAME sealed nodes the first
// did (the alias contract, TestLoadCacheServesTheSameNodes), so `lit` and
// `A`'s element end up one node; two fresh parses give two equal nodes.
// Nothing a program can do tells the two apart -- a sealed node cannot be
// written through and `equal?` is structural -- so the differential oracle
// must not tell them apart either.  It used to: valueFingerprint recorded
// aliasing for every node, sealed or not, and FuzzLoadCacheHostileReader
// reported seed bfe6dc31a652ced0 as a cache divergence in which `lit`
// digested as a back-reference to A's element under the cache and as a full
// walk without one.  (The per-binding digest streams were diffed and that was
// the differing line; "the only differing BYTE" overstated it, since the
// digest is a hash and the claim was never pinned at byte level.)
//
// The seed's own shape is kept verbatim as the first case, zero-width
// `slice 'list` and all, even though the minimal shape below shows the slice
// is incidental (the fuzzer's minimiser stopped at a parse boundary, not at
// the mechanism).

const (
	loadCacheRebindSeedA = "(set'lit'(0))(and'1(slice'list lit 0 0))"
	loadCacheRebindSeedB = "(set'A(list lit))"
	loadCacheRebindMinA  = "(set 'lit '(0))"
	loadCacheRebindMinB  = "(set 'A (list lit))"
)

// TestLoadCacheRebindAcrossLoadsMatchesFreshParse is the deterministic form
// of the crasher, in every transparent Reader mode: A, B, A through a cache
// must fingerprint identically to A, B, A without one.
func TestLoadCacheRebindAcrossLoadsMatchesFreshParse(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct{ name, a, b string }{
		{"seed", loadCacheRebindSeedA, loadCacheRebindSeedB},
		{"minimal", loadCacheRebindMinA, loadCacheRebindMinB},
		// Two DIFFERENT streams with IDENTICAL bytes.  Nothing about #613
		// needs it; it is here because runHostilePair digests sealed
		// PROVENANCE and this is the shape that exercises it.  Drop name and
		// loc from loadCacheKey and the second stream is served the first's
		// parse, so every error it raises names the first stream's file --
		// with this case that mutation fails on `eval 2` in all seven
		// transparent modes, and without it nothing in this test notices
		// (the A,B,A sequence rebinds the same symbols from the same bytes,
		// so the final environment converges either way).
		{"identical-bytes", loadCacheRebindMinA, loadCacheRebindMinA},
	} {
		for _, mode := range transparentReaderModes {
			t.Run(fmt.Sprintf("%s/mode=%d", tc.name, mode), func(t *testing.T) {
				t.Parallel()
				baseline, ok := runHostilePair(t, mode, []byte(tc.a), []byte(tc.b), nil)
				require.True(t, ok)
				cached, ok := runHostilePair(t, mode, []byte(tc.a), []byte(tc.b), newFuzzLoadCache())
				require.True(t, ok)
				assert.Truef(t, cached.equal(baseline),
					"A,B,A through a cache diverged from A,B,A without one\n--- baseline ---\n%s\n--- cached ---\n%s",
					baseline, cached)
			})
		}
	}
}

// TestValueFingerprintSealedIdentityIsNotState pins the oracle rule directly,
// without a cache in the picture: one sealed node reached from two bindings
// digests the same as two equal sealed nodes, and one MUTABLE node reached
// from two bindings does not.  The second half is the positive control -- it
// is what stops the fix from blinding the digest to a real cross-binding
// leak of writable storage, which is the class envStateFingerprint exists
// to catch.
func TestValueFingerprintSealedIdentityIsNotState(t *testing.T) {
	t.Parallel()
	env, _, rc := newFuzzEnv()
	require.Nil(t, rc)
	env.Runtime.Reader = parser.NewReader()

	// Two parses of the same literal: two sealed, equal, distinct nodes --
	// exactly what two uncached loads of `(set 'lit '(0))` bind.
	one := env.LoadString("one", "'(0)")
	two := env.LoadString("two", "'(0)")
	require.NotEqual(t, lisp.LError, one.Type, one)
	require.True(t, one.IsSealed(), "a quoted literal evaluates to the sealed parse node")
	require.True(t, two.IsSealed())
	require.NotSame(t, one, two, "two parses must mint two nodes for this test to mean anything")
	require.Equal(t, one.String(), two.String())

	// The #613 shape: A = (list lit) captured the first node; lit is either
	// the same node (cached rebinding) or the second (fresh parse).
	shared := []*lisp.LVal{lisp.SExpr([]*lisp.LVal{one}), one}
	fresh := []*lisp.LVal{lisp.SExpr([]*lisp.LVal{one}), two}
	assert.Equal(t, valueFingerprint(fresh), valueFingerprint(shared),
		"sealed-node identity is not lisp-observable and must not be state")

	// Positive control: the same shape over MUTABLE storage stays distinct.
	// A write through one alias is visible through the other, so these are
	// different environments.
	v1 := lisp.Vector([]*lisp.LVal{lisp.Int(0)})
	v2 := lisp.Vector([]*lisp.LVal{lisp.Int(0)})
	require.False(t, v1.IsSealed())
	sharedMut := []*lisp.LVal{lisp.SExpr([]*lisp.LVal{v1}), v1}
	freshMut := []*lisp.LVal{lisp.SExpr([]*lisp.LVal{v1}), v2}
	assert.NotEqual(t, valueFingerprint(freshMut), valueFingerprint(sharedMut),
		"aliasing of mutable storage must still be recorded")

	// Termination no longer rests on the visited set for sealed nodes, so
	// both a mutable cycle (back-reference) and a sealed one (the memo, the
	// depth cap and the node budget) must still return, and deterministically.
	cyc := lisp.SExpr([]*lisp.LVal{lisp.Int(1)})
	cyc.Cells[0] = cyc
	cycFirst := valueFingerprint([]*lisp.LVal{cyc})
	cycAgain := valueFingerprint([]*lisp.LVal{cyc})
	assert.Equal(t, cycFirst, cycAgain, "a mutable cycle must digest deterministically")
	sealedCyc := lisp.SExpr([]*lisp.LVal{lisp.Int(1)})
	sealedCyc.Cells[0] = sealedCyc
	sealedCyc.SealAST()
	require.True(t, sealedCyc.IsSealed())
	sealedFirst, sealedNodes := valueFingerprintNodes([]*lisp.LVal{sealedCyc})
	sealedAgain := valueFingerprint([]*lisp.LVal{sealedCyc})
	assert.Equal(t, sealedFirst, sealedAgain, "a sealed cycle must digest deterministically")
	assert.NotEqual(t, cycFirst, sealedFirst, "sealed-ness itself stays part of the digest")

	// A single-cell self-cycle is the CHEAP shape: one child per level, so
	// the depth cap alone stops it after ~512 nodes whether or not anything
	// is memoised.  It therefore proves nothing about the exponential case,
	// which is why the two below are here.
	assert.Lessf(t, sealedNodes, valueFPMaxNodes,
		"a one-cell sealed self-cycle must not exhaust the node budget (visited %d)", sealedNodes)

	// A BRANCHING sealed cycle: two cells, both the node itself.  With
	// content-only digesting and no memo this unfolds as 2^depth and spends
	// the entire 65536-node budget (measured: 31.4ms).  The memo makes the
	// second cell a lookup, so it costs the depth cap once (0.70ms).
	branch := lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	branch.Cells[0] = branch
	branch.Cells[1] = branch
	branch.SealAST()
	require.True(t, branch.IsSealed())
	start := time.Now()
	branchFP, branchNodes := valueFingerprintNodes([]*lisp.LVal{branch})
	branchElapsed := time.Since(start)
	branchAgain, branchNodesAgain := valueFingerprintNodes([]*lisp.LVal{branch})
	assert.Equal(t, branchFP, branchAgain, "a branching sealed cycle must digest deterministically")
	assert.Equal(t, branchNodes, branchNodesAgain, "and must cost the same both times")
	// 4*valueFPMaxDepth is slack over the ~1 node per level the memo leaves
	// while still being two orders of magnitude under the budget the
	// unmemoised walk consumed in full.
	assert.Lessf(t, branchNodes, 4*valueFPMaxDepth,
		"a branching sealed cycle must not unfold (visited %d nodes in %v)", branchNodes, branchElapsed)

	// A sealed DAG whose UNFOLDED size is 2^25.  This is the shape the memo
	// exists for: every node is reached twice, so content-only digesting
	// without a memo is exponential (measured: 25.9ms without the memo, 79us
	// with it, against 13us for the aliasing walk over the same graph
	// unsealed).
	dag := lisp.Int(0)
	for range 25 {
		dag = lisp.SExpr([]*lisp.LVal{dag, dag})
	}
	dag.SealAST()
	require.True(t, dag.IsSealed())
	start = time.Now()
	dagFP, dagNodes := valueFingerprintNodes([]*lisp.LVal{dag})
	dagElapsed := time.Since(start)
	assert.Lessf(t, dagNodes, 4*26,
		"a 26-node sealed DAG must be walked once per node, not unfolded (visited %d nodes in %v)",
		dagNodes, dagElapsed)

	// And the consequence that made the unmemoised version a CORRECTNESS
	// problem rather than only a slow one: the node budget is shared across
	// every value in one digest (envStateFingerprint hands valueFingerprint
	// every binding at once), so a value that spends it all leaves every
	// later value digesting as "trunc" -- blinding the oracle to real
	// differences behind the hog.
	hog := []*lisp.LVal{branch, lisp.Vector([]*lisp.LVal{lisp.Int(1)})}
	notHog := []*lisp.LVal{branch, lisp.Vector([]*lisp.LVal{lisp.Int(2)})}
	assert.NotEqual(t, valueFingerprint(hog), valueFingerprint(notHog),
		"a budget hog must not blind the digest to the values walked after it")
	assert.NotEmpty(t, dagFP)
}

// TestEnvStateFingerprintRecordsMutableAliasing is the positive control at
// the level the fuzz targets actually assert on.  Every other control in this
// file calls valueFingerprint directly; this one goes through
// envStateFingerprint, so a change that blinded the ENV digest -- a shared
// budget spent before the interesting binding, a walk that stopped at package
// boundaries -- would be caught here and not there.
//
// Two bindings holding ONE mutable vector is a different environment from two
// bindings holding two equal vectors: a write through either name is visible
// through the other in the first and not in the second.
func TestEnvStateFingerprintRecordsMutableAliasing(t *testing.T) {
	t.Parallel()
	fp := func(src string) string {
		env, _, rc := newFuzzEnv()
		require.Nil(t, rc)
		env.Runtime.Reader = parser.NewReader()
		res := env.LoadString("alias", src)
		require.NotEqual(t, lisp.LError, res.Type, res)
		return envStateFingerprint(env)
	}
	aliased := fp(`(set 'x (vector 1 2))(set 'y x)`)
	distinct := fp(`(set 'x (vector 1 2))(set 'y (vector 1 2))`)
	assert.Equal(t, aliased, fp(`(set 'x (vector 1 2))(set 'y x)`),
		"the env digest must be deterministic before it can be discriminating")
	assert.NotEqual(t, aliased, distinct,
		"two bindings sharing one mutable vector is not the same environment as two equal vectors")
}

// TestLoadCacheSharedLiteralIsNotLispObservable is the lisp-level probe
// behind the classification: after A, B, A the shared sealed node can be
// compared, viewed, and attacked with every guarded mutator, and every
// outcome is the same with and without the cache.  In particular every
// write attempt through `lit` is refused, so no write can reach `A` (or
// the cache entry) through it -- the channel that would have made the
// sharing observable does not exist.
//
// This is a PREMISE test, not a regression test.  It passes on origin/main
// too, and it is meant to: it establishes the fact the #613 fix RESTS on
// (the sharing is unobservable) rather than exercising the fix.  Its value is
// that it FALSIFIES cheaply -- if a future carve-out ever let a write reach
// sealed backing, this goes red and the fix's justification goes with it,
// which is why the two derived-value writes below are here.  A probe that
// only ever reads cannot do that job.
func TestLoadCacheSharedLiteralIsNotLispObservable(t *testing.T) {
	t.Parallel()
	// Element order is pinned by the names below; keep the two in step.
	const probe = `(list
		(equal? A (list lit))
		(equal? (car A) lit)
		(handler-bind ((modify-literal-error (lambda (c &rest args) 'refused))) (slice 'vector lit 0 1))
		(handler-bind ((modify-literal-error (lambda (c &rest args) 'refused))) (append 'vector lit 1))
		(handler-bind ((modify-literal-error (lambda (c &rest args) 'refused))) (stable-sort < lit))
		(slice 'list lit 0 0)
		(slice 'vector lit 0 0)
		(copy lit)
		(handler-bind ((modify-literal-error (lambda (c &rest args) 'refused))) (append! (slice 'vector lit 0 0) 99))
		(handler-bind ((modify-literal-error (lambda (c &rest args) 'refused))) (append! (slice 'vector (copy lit) 0 1) 99))
		A
		lit)`
	// probeNames labels the probe's elements so a failure names the form that
	// diverged instead of an index, and so the refusal assertions below do
	// not depend on how the whole list renders (the old assertion matched the
	// literal substring "'refused 'refused 'refused", which any change to
	// spacing or to a neighbouring element would have broken silently).
	probeNames := []string{
		"(equal? A (list lit))",
		"(equal? (car A) lit)",
		"(slice 'vector lit 0 1)",
		"(append 'vector lit 1)",
		"(stable-sort < lit)",
		"(slice 'list lit 0 0)",
		"(slice 'vector lit 0 0)",
		"(copy lit)",
		"(append! (slice 'vector lit 0 0) 99)",
		"(append! (slice 'vector (copy lit) 0 1) 99)",
		"A",
		"lit",
	}
	run := func(cache *fuzzLoadCache) []string {
		env, _, rc := newFuzzEnv()
		require.Nil(t, rc)
		env.Runtime.Reader = parser.NewReader()
		if cache != nil {
			env.Runtime.LoadCache = cache
		}
		for _, step := range []struct{ name, src string }{
			{loadCacheHostileFileA, loadCacheRebindMinA},
			{loadCacheHostileFileB, loadCacheRebindMinB},
			{loadCacheHostileFileA, loadCacheRebindMinA},
		} {
			res := env.LoadLocation(step.name, step.name, strings.NewReader(step.src))
			require.NotEqual(t, lisp.LError, res.Type, res)
		}
		out := env.LoadString("probe", probe)
		require.NotEqual(t, lisp.LError, out.Type, out)
		require.Len(t, out.Cells, len(probeNames))
		got := make([]string, len(out.Cells))
		for i, c := range out.Cells {
			got[i] = c.String()
		}
		return got
	}
	uncached := run(nil)
	cached := run(newFuzzLoadCache())
	for i, name := range probeNames {
		assert.Equalf(t, uncached[i], cached[i],
			"%s must be indistinguishable with and without the cache", name)
		t.Logf("%-46s => %s", name, cached[i])
	}

	// Every guarded write through the shared literal is refused, named one by
	// one.  These three are the reason the sharing is unobservable: with no
	// write channel into the sealed node, `A` and the cache entry cannot be
	// reached through `lit`.
	for _, i := range []int{2, 3, 4} {
		assert.Equalf(t, "'refused", cached[i], "%s must be refused", probeNames[i])
		assert.Equalf(t, "'refused", uncached[i], "%s must be refused without the cache too", probeNames[i])
	}

	// The two writes through a DERIVED value.  Neither is refused -- both are
	// legal today -- and that is the point: they are the two shapes that would
	// FALSIFY the classification if the derivation ever leaked sealed backing.
	//
	//   - `(slice 'vector lit 0 0)` is the zero-width seal carve-out
	//     (builtins.go, CondModifyLiteral): a zero-width window over sealed
	//     backing is handed out as a vector with FRESH empty backing rather
	//     than refused, so `append!` into it must not be able to reach lit's
	//     cell 0 through retained capacity.
	//   - `(copy lit)` clears the seal, so the slice of the copy is genuinely
	//     mutable; `append!` into it must write the copy and not the original.
	//
	// lit and A are re-read AFTER both writes (the last two probe elements),
	// so a leak shows up as a changed literal rather than as a changed
	// intermediate nobody looks at again.
	for _, i := range []int{8, 9} {
		assert.NotEqualf(t, "'refused", cached[i],
			"%s is expected to be legal today; if it is now refused this test's premise changed",
			probeNames[i])
	}
	assert.Equal(t, "'(0)", cached[len(cached)-1], "lit must still be the literal it was parsed as")
	assert.Equal(t, "'('(0))", cached[len(cached)-2], "A must still hold the literal it captured")
}
