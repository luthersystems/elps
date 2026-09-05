// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"hash"
	"hash/fnv"
	"io"
	"math"

	"github.com/luthersystems/elps/lisp"
)

// The RUNTIME-value fingerprint, complementing lisp.SealedASTFingerprint.
//
// # Why a second fingerprint exists at all
//
// lisp.SealedASTFingerprint (lisp/sealfp.go) is the canonical lens for the
// seal invariant, and it deliberately refuses to descend into unsealed
// nodes: an unsealed node is runtime storage the evaluator may legitimately
// rewrite, so its contents cannot be part of a stability claim about a
// PARSE.  That is the right contract for the seal oracle and the wrong one
// for the assertion the container-family targets need, which is a different
// sentence about a different object:
//
//	an operation the language documents as NON-MUTATING must leave the
//	value it was handed bit-identical, whether or not that value came from
//	a parse.
//
// `(get m "k")` promising not to touch `m` is a promise about a sorted-map,
// which SealAST never marks and SealedASTFingerprint therefore reports as a
// single opaque "hole" byte.  Under that lens every map-mutating defect in
// the language would fingerprint identically before and after.  The two
// oracles are run TOGETHER by the container targets, and neither subsumes
// the other:
//
//   - sealed fingerprint drift  => shared program storage was written; the
//     substrate#378 / elps#369 class, a defect for every op, mutating ones
//     included.
//   - value fingerprint drift on a non-mutating op => the copy contract of
//     that specific builtin failed, whatever the storage was.
//
// # What it covers that the sealed walk does not
//
//   - Unsealed nodes, descended rather than elided.
//   - LBytes payloads.  Bytes live in Native as a *[]byte; the sealed walk
//     skips Native entirely (no parser-producible type has one), so a
//     byte-level write is invisible to it.  This is not hypothetical: the
//     bytes family shares backing arrays through `slice` exactly as Go
//     slices do (issue #373), so a write through a retained-capacity alias
//     is precisely the shape worth fingerprinting.
//   - LSortMap contents, walked through the Map interface's SORTED Keys()
//     so the digest is independent of Go map iteration order (which is
//     randomised per process and would otherwise make every comparison
//     noise).
//   - LArray dimensions as well as elements: a corrupted `dims` cell with
//     intact storage is the shape elps#379's list write-back produced.
//
// # What it deliberately does NOT cover
//
//   - Pointer identity of SEALED nodes.  Two structurally equal sealed
//     values digest equally whether they are one node or two.  A sealed
//     node is frozen storage (lisp/seal.go): no lisp-reachable write can go
//     through it, and the language has no identity primitive (`equal?` is
//     structural), so no program can ask WHICH of two equal sealed nodes it
//     holds -- and which one it holds is a thing the load cache changes BY
//     CONTRACT.  A cache hit serves the same sealed nodes the miss served
//     (TestLoadCacheServesTheSameNodes is the alias proof), so a file loaded
//     twice through a cache rebinds its literals to the nodes an earlier
//     load already captured, where two fresh parses yield two equal nodes.
//     Digesting that identity turned the cache's contract into a divergence:
//     issue #613, found by FuzzLoadCacheHostileReader with `(set 'lit '(0))`
//     loaded, `(set 'A (list lit))` loaded, and the first loaded again --
//     every evaluation result equal, and the environment digest differing in
//     `lit` being a back-reference to A's element under the cache and a full
//     walk without it.  Detecting that a builtin swapped one sealed child
//     for a different-but-equal one is the -race seal watchdog's job, not a
//     content digest's.
//
//     That rule is narrower than "sealed identity is unobservable", which is
//     what this comment used to claim and is FALSE in general.  A sealed node
//     carries its parse-time source location and carries it forever --
//     SetSource is a no-op once sealed (lisp/lisp.go) -- and that location is
//     printed by every stack note and error the node raises, so two equal
//     sealed nodes minted from two different files ARE distinguishable from
//     lisp.  That is precisely why the load cache keys on the stream's name
//     and location and not on its bytes alone; the "# Keying" paragraph in
//     lisp/loadcache.go spells out the misattribution a content-only key
//     produces (the second file's errors naming the first file's lines).
//     Content-only digesting is therefore right for the IDENTITY question and
//     blind to the PROVENANCE one.  valueFingerprintProv below restores
//     provenance for the callers whose property is provenance -- today just
//     the load-cache hostile-reader pair -- and is deliberately not the
//     default: FuzzSharedProgramMultiEnv reparses one source under different
//     stream names on purpose, so a global provenance rule would make that
//     target report its own premise as a divergence.
//   - Pointer identity of UNSEALED nodes IS recorded, through the
//     back-reference marker below.  Mutable storage is where identity is
//     observable -- a write through one alias is visible through the other
//     -- so two bindings sharing one vector and two bindings holding two
//     equal vectors are different environments, and a cache (or a builtin)
//     that turned one into the other would be a real semantic change.
//   - Native payloads other than LBytes.  An arbitrary Go value has no
//     order-stable rendering (`%v` of a map is randomised), so it
//     contributes its dynamic TYPE only.  Nothing in the container families
//     under test constructs one.
//   - LFun bodies.  A function value's identity for these purposes is its
//     type, name and formals; digesting a closure would drag in the
//     captured environment, which legitimately changes.
//
// # Termination
//
// Runtime values can alias and, unlike parser output, can genuinely cycle
// (a vector pushed into itself).  The walk therefore carries a visited map
// keyed by pointer, emitting a back-reference marker on a repeat, plus a
// depth cap and a node budget.  Traversal order is fixed, so a truncated
// digest is still deterministic: an equal-before/equal-after comparison
// stays meaningful and corruption past the horizon is simply out of scope.
//
// Back-references are for UNSEALED nodes only (above), which on its own
// makes a sealed DAG exponential and a sealed cycle a budget hog -- and the
// budget is shared across every value in one envStateFingerprint, so a hog
// blinds the digest to every value walked after it.  Measured, before the
// memo below and after it: a 26-node sealed DAG (2^25 unfolded) 25.9ms ->
// 79us (13us is what the aliasing walk costs on the same graph unsealed), a
// two-cell branching sealed cycle 31.4ms -> 0.70ms, and -- the part that is
// a correctness bug and not a slow test -- `[hog, Vector(1)]` digested EQUAL
// to `[hog, Vector(2)]` before and distinct after.  The fix is memoisation,
// not bookkeeping: a sealed
// subtree's CONTENT hash is cached by pointer, so the second reach costs a
// map lookup and the digest is still the content digest.  That is a
// different claim from a back-reference -- "the same content again", not
// "the node you saw at position N" -- so it does not reintroduce identity.

const (
	// valueFPMaxNodes bounds the nodes one digest visits.  Far above
	// anything the container generators build (their own budgets are in the
	// low hundreds); the cap exists for aliased and cyclic runtime graphs.
	valueFPMaxNodes = 1 << 16

	// valueFPMaxDepth bounds recursion depth independently of the budget so
	// a deep spine cannot exhaust the walker's own Go stack.
	valueFPMaxDepth = 512
)

// valueFP is the running digest state plus the walk's termination budget.
type valueFP struct {
	// h is the accumulator the walk currently writes to: root, or the
	// private accumulator of a sealed subtree being memoised.
	h io.Writer
	// root is the digest the whole walk sums to.
	root hash.Hash64
	seen map[*lisp.LVal]int
	// memo caches a SEALED node's content hash by pointer, so a sealed DAG
	// or cycle is walked once rather than unfolded.  See walk.
	memo   map[*lisp.LVal]uint64
	budget int
	// prov mixes a sealed node's frozen source location into its digest.
	// Off by default; see valueFingerprintProv.
	prov bool
	// touchedUnsealed records whether the walk currently in progress reached
	// an unsealed node, which is what makes its digest unmemoisable.  See
	// walk.
	touchedUnsealed bool
}

// valueFingerprint returns a content digest of the runtime values reachable
// from vs.  It never mutates anything it walks.
//
// The digest is a string rather than a uint64 so a failure message can print
// it next to a sealed fingerprint without the two being confusable.
func valueFingerprint(vs []*lisp.LVal) string {
	return newValueFP(false).run(vs)
}

// valueFingerprintProv is valueFingerprint plus sealed-node PROVENANCE: a
// sealed node additionally mixes the source location it froze at.
//
// It exists because the content-only rule is blind to a real, observable
// difference.  A sealed node's location is frozen (SetSource is a no-op once
// sealed) and is printed by every stack note and error raised through it, so
// serving one file's nodes for another file's load is a lisp-observable
// misattribution -- which is exactly the failure a content-only cache key
// produces, described in loadcache.go's "# Keying" paragraph.  Mutating
// loadCacheKey to drop name and loc and then loading two byte-identical files
// makes the second file's errors name the first file's path; plain
// valueFingerprint cannot see that, and this variant can.
//
// It is NOT the default, and must not become it.  FuzzSharedProgramMultiEnv
// deliberately evaluates ONE parse under several stream names, and
// TestSharedProgramSeedsAgreeWithSharedParse and
// TestValueFingerprintSealedIdentityIsNotState compare nodes minted by
// different loads on purpose; a global provenance rule turns all three red on
// their own premise.  The one caller today is runHostilePair
// (loadcache_reader_fuzz_test.go), where "the cache served the right file's
// parse" is the property under test.
func valueFingerprintProv(vs []*lisp.LVal) string {
	return newValueFP(true).run(vs)
}

func newValueFP(prov bool) *valueFP {
	root := fnv.New64a()
	return &valueFP{
		h:      root,
		root:   root,
		seen:   make(map[*lisp.LVal]int),
		memo:   make(map[*lisp.LVal]uint64),
		budget: valueFPMaxNodes,
		prov:   prov,
	}
}

// valueFingerprintNodes is valueFingerprint plus the number of nodes the
// walk actually charged against its budget.  The count is what the sealed
// memo exists to bound, and it is exactly deterministic, so a test can pin
// "this sealed graph does not unfold" without pinning a wall-clock time.
func valueFingerprintNodes(vs []*lisp.LVal) (string, int) {
	s := newValueFP(false)
	return s.run(vs), valueFPMaxNodes - s.budget
}

func (s *valueFP) run(vs []*lisp.LVal) string {
	for _, v := range vs {
		s.walk(v, 0)
	}
	return fmt.Sprintf("%016x", s.root.Sum64())
}

func (s *valueFP) mix(format string, args ...interface{}) {
	_, _ = fmt.Fprintf(s.h, format, args...)
}

func (s *valueFP) walk(v *lisp.LVal, depth int) {
	if v == nil {
		s.mix("<nil>;")
		return
	}
	// Only UNSEALED nodes take part in the back-reference bookkeeping.  A
	// sealed node is digested by content every time it is reached: its
	// identity is not lisp-observable and the load cache legitimately
	// shares one sealed node between loads that a fresh parse would give two
	// equal nodes (issue #613; see the file comment).
	sealed := v.IsSealed()
	if !sealed {
		// Recorded BEFORE the back-reference return, because emitting
		// `back:N` is itself the visit-order-dependent thing that makes an
		// enclosing sealed digest unmemoisable.
		s.touchedUnsealed = true
		if id, ok := s.seen[v]; ok {
			// A back-reference, not a re-walk: this is what makes a cyclic
			// or heavily-aliased mutable value terminate, and it also records
			// the SHAPE of the aliasing, so collapsing two aliases of mutable
			// storage into one copy changes the digest.
			s.mix("back:%d;", id)
			return
		}
	}
	if s.budget <= 0 || depth > valueFPMaxDepth {
		s.mix("trunc;")
		return
	}
	if !sealed {
		s.walkContent(v, depth)
		return
	}

	// A sealed node is memoised by pointer.  Without this, "digest sealed
	// nodes by content every time" is exponential on a sealed DAG and burns
	// the whole node budget on a sealed cycle -- and because the budget is
	// shared across every value in one envStateFingerprint, a single hog
	// blinds the digest to every value after it.  A memo hit emits the same
	// bytes a full walk would have emitted, so nothing about the digest's
	// MEANING changes: it still says "this content", never "the node you saw
	// at position N".  Termination for sealed subgraphs rests on this memo
	// plus the depth cap and the node budget.
	if sum, ok := s.memo[v]; ok {
		s.mix("sealed:%016x;", sum)
		return
	}
	// The subtree is hashed into its own accumulator so its content hash can
	// be cached, but it SHARES seen, budget and memo with the parent: the
	// budget must still bound the whole walk, and an unsealed node reached
	// from here must take part in the parent's aliasing bookkeeping.
	sub := fnv.New64a()
	outerH, outerTouched := s.h, s.touchedUnsealed
	s.h, s.touchedUnsealed = sub, false
	s.walkContent(v, depth)
	sum, touched := sub.Sum64(), s.touchedUnsealed
	s.h, s.touchedUnsealed = outerH, outerTouched || touched
	if !touched {
		// Cached only when the subtree stayed inside sealed storage.  A
		// sealed subtree that REACHES an unsealed node must not be memoised:
		// its digest embeds `back:N` markers and `seen` ids that are valid
		// only at the visit where they were minted, so replaying them at a
		// later reach would assert an aliasing shape that was never
		// observed.  The parser cannot produce that shape -- sealAST stops
		// at the first non-sealable type, so nothing below a sealed node is
		// unsealed -- but an embedder that calls SealAST on a hand-built
		// tree can, and the memo must be correct for the tree it is handed
		// rather than for the tree the parser happens to build.
		//
		// A sum that embeds a `trunc` (the walk hit the depth cap or ran the
		// budget out inside the subtree) IS still cached, deliberately.  It
		// makes the digest of a truncated subtree depend on where it was
		// first reached rather than on where it is reused, which costs a
		// little fidelity past the horizon that is already out of scope --
		// and it is what keeps a branching sealed cycle linear: refusing to
		// cache truncated sums turns that shape back into O(depth^2), which
		// for valueFPMaxDepth=512 is over the node budget again.
		s.memo[v] = sum
	}
	s.mix("sealed:%016x;", sum)
}

// walkContent digests v itself and its children.  The caller has already
// settled the back-reference, budget, depth and memo questions.
func (s *valueFP) walkContent(v *lisp.LVal, depth int) {
	s.budget--
	if !v.IsSealed() {
		s.seen[v] = len(s.seen)
	} else if s.prov {
		// Provenance, for the callers that asked for it: a sealed node's
		// location is frozen and lisp-observable through errors and stack
		// notes.  See valueFingerprintProv.
		loc, ok := v.Source()
		s.mix("src:%s:%d:%d/%v;", loc.File, loc.Line, loc.Col, ok)
	}

	// The float mixes its BIT PATTERN rather than its value, so NaN payloads
	// and the two zeros digest distinctly and deterministically — `==` on
	// float64 says NaN != NaN and +0 == -0, neither of which is the question
	// a corruption digest asks.
	s.mix("t%d/f%d/q%v/s%d:%q/i%d/fl%x/sealed%v;",
		v.Type, v.FunType, v.IsQuoted(), len(v.Str), v.Str, v.Int,
		math.Float64bits(v.Float), v.IsSealed())

	switch v.Type { //nolint:exhaustive // every other type is fully described by the scalar line above plus its Cells, walked below
	case lisp.LBytes:
		// The payload the sealed walk cannot see.  Length-prefixed so a
		// short value cannot alias a longer one's prefix.
		b := v.Bytes()
		s.mix("bytes%d:%x;", len(b), b)
	case lisp.LSortMap:
		s.walkMap(v, depth)
		return
	case lisp.LNative:
		// No order-stable rendering exists for an arbitrary Go value, so
		// only its dynamic type participates.  See the file comment.
		s.mix("native:%T;", v.Native)
	case lisp.LFun:
		// Formals only; a closure's captured environment is not part of a
		// value-stability claim.  Cells[0] is the formals list for every
		// LFun the language constructs.
		if len(v.Cells) > 0 {
			s.mix("formals{")
			s.walk(v.Cells[0], depth+1)
			s.mix("}")
		}
		return
	}

	s.mix("n%d{", len(v.Cells))
	for _, c := range v.Cells {
		s.walk(c, depth+1)
	}
	s.mix("}")
}

// walkMap digests a sorted-map through the Map interface's SORTED key list.
// Reading pkg.symbols or the backing Go map directly would make the digest
// depend on Go's randomised map iteration order, which would turn every
// before/after comparison into noise.
func (s *valueFP) walkMap(v *lisp.LVal, depth int) {
	m := v.Map()
	if m == nil {
		s.mix("map<nil>;")
		return
	}
	keys := m.Keys()
	if keys == nil || keys.Type == lisp.LError {
		s.mix("map<keyerr>;")
		return
	}
	s.mix("map%d{", len(keys.Cells))
	for _, k := range keys.Cells {
		s.walk(k, depth+1)
		s.mix("=>")
		val, ok := m.Get(k)
		if !ok {
			// Keys() named a key Get cannot retrieve.  That is itself an
			// inconsistency worth digesting distinctly rather than eliding.
			s.mix("<missing>;")
			continue
		}
		s.walk(val, depth+1)
	}
	s.mix("}")
}
