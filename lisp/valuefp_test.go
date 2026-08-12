// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
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
//   - Pointer identity and sharing topology.  Two structurally equal values
//     digest equally.  Detecting that a builtin swapped one child for a
//     different-but-equal child is the -race seal watchdog's job, not a
//     content digest's.
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
	h      io.Writer
	seen   map[*lisp.LVal]int
	budget int
}

// valueFingerprint returns a content digest of the runtime values reachable
// from vs.  It never mutates anything it walks.
//
// The digest is a string rather than a uint64 so a failure message can print
// it next to a sealed fingerprint without the two being confusable.
func valueFingerprint(vs []*lisp.LVal) string {
	h := fnv.New64a()
	s := &valueFP{h: h, seen: make(map[*lisp.LVal]int), budget: valueFPMaxNodes}
	for _, v := range vs {
		s.walk(v, 0)
	}
	return fmt.Sprintf("%016x", h.Sum64())
}

func (s *valueFP) mix(format string, args ...interface{}) {
	_, _ = fmt.Fprintf(s.h, format, args...)
}

func (s *valueFP) walk(v *lisp.LVal, depth int) {
	if v == nil {
		s.mix("<nil>;")
		return
	}
	if id, ok := s.seen[v]; ok {
		// A back-reference, not a re-walk: this is what makes a cyclic or
		// heavily-aliased value terminate, and it also records the SHAPE of
		// the aliasing, so collapsing two aliases into one copy changes the
		// digest.
		s.mix("back:%d;", id)
		return
	}
	if s.budget <= 0 || depth > valueFPMaxDepth {
		s.mix("trunc;")
		return
	}
	s.budget--
	s.seen[v] = len(s.seen)

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
