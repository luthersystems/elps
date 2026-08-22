// Copyright © 2026 The ELPS authors

package lisp

import "math"

// The canonical sealed-AST fingerprint (issue #372).
//
// The seal design (lisp/seal.go) reduces to one checkable sentence: the
// bytes of a sealed program node never change after parsing completes.
// This file is the single lens every verification tool looks through to
// check that sentence — the fuzz corruption oracle re-fingerprints each
// parsed input after evaluating it (lisp/eval_fuzz_test.go), the
// checked-mode inspector records a fingerprint per sealed parse and
// re-verifies it later (lisp/seal_check_elpscheck.go), and the sealed
// argument oracle in the stdlib application fuzzer compares argument
// fingerprints across each call (lisp/lisplib fuzz tests).  One walker,
// shared, so a node field can never be covered by one tool and silently
// missed by another.
//
// # What the fingerprint covers, and why
//
//   - Only SEALED nodes.  An unsealed node is runtime storage that the
//     evaluator may legitimately mutate, so its contents cannot be part of
//     any stability claim; it contributes a fixed "hole" marker and is not
//     descended into.  (Parser output is sealed all the way down, so for a
//     parsed tree the fingerprint covers every node.  A hole can only
//     appear in hand-built trees where SealAST stopped at a runtime-only
//     type such as an array or sorted-map.)
//   - Structure and scalar payloads: Type, FunType, Quoted, Spliced, Str,
//     Int, Float, and the length and order of Cells.  These are the bytes
//     whose corruption was the substrate#378 class.
//   - Source-location METADATA CONTENTS, deliberately.  Issue #370 was a
//     metadata write (stampMacroExpansion re-stamping a shared node's
//     source), so leaving .source out would blind every fingerprint
//     consumer to a corruption class that has actually shipped.  The
//     walker hashes the pointed-to Location's fields, not the pointer, so
//     it detects any rewrite that changes WHERE a node claims to come
//     from.  What it cannot detect is a same-value write: replacing the
//     source pointer with a different pointer to identical contents (or
//     re-storing the value a field already holds) leaves the fingerprint
//     unchanged.  Those writes are still data races on shared storage,
//     and they are exactly what the -race seal watchdog exists to catch
//     (lisp/seal_watchdog_test.go) — the fingerprint and the watchdog
//     split the metadata problem along that line.
//   - Presence (nil vs non-nil) of Meta and MacroExpansion.  Both are
//     debug/format metadata that must never be ATTACHED to a sealed node
//     after parse (stampMacroExpansion skips sealed subtrees); their
//     inner contents are not part of the stability claim.
//   - NOT pointer identity.  Two trees with equal contents fingerprint
//     equally, so a fingerprint is stable across processes and runs and
//     can be compared before/after any activity without recording which
//     specific allocations backed the tree.  The flip side: swapping a
//     sealed node's unsealed child for a DIFFERENT unsealed child (both
//     hash as the same hole marker) is invisible here.  That write is a
//     write to sealed storage and remains the watchdog's to catch.
//   - NOT Native.  No parser-producible type stores a Native payload, so
//     a sealed node's Native is nil by construction; runtime types that
//     use it are unsealed holes.
//
// # Termination
//
// Parser output is a strict tree, but SealAST is exported and an embedder
// can seal a hand-built graph that aliases or even cycles (SealAST's
// own already-sealed check terminates on cycles, leaving a fully sealed
// cyclic graph behind).  The walk therefore carries two deterministic
// bounds — a total node budget and a depth cap — and mixes a distinct
// truncation marker when either is hit.  Traversal order is fixed, so a
// truncated fingerprint is still deterministic for a given graph: equal
// before/after fingerprints remain meaningful, corruption inside the
// truncated region is simply beyond the horizon.  Both bounds sit far
// above anything a parser can produce (parse depth is capped at 10,000).

const (
	// sealFPMaxNodes bounds the total number of sealed nodes one
	// fingerprint call visits.  1M nodes is orders of magnitude beyond any
	// real top-level expression; the bound exists for hand-built cyclic
	// graphs, not for programs.
	sealFPMaxNodes = 1 << 20

	// sealFPMaxDepth bounds recursion depth.  Parser nesting is capped at
	// 10,000 (rdparser.DefaultMaxParseDepth); 100,000 leaves 10x headroom
	// while keeping the walker's own Go-stack use bounded on adversarial
	// graphs.
	sealFPMaxDepth = 100_000

	// FNV-1a 64-bit parameters (hash/fnv's constants, inlined so the walk
	// is allocation-free and trivially deterministic).
	sealFPOffset64 = 14695981039346656037
	sealFPPrime64  = 1099511628211
)

// Structure markers mixed into the digest so that field boundaries and
// tree shape are unambiguous (a node's Str cannot bleed into a sibling's).
const (
	sealFPMarkNil       = 0x01 // nil child pointer
	sealFPMarkHole      = 0x02 // unsealed node: runtime storage, not descended
	sealFPMarkTruncated = 0x03 // node budget or depth cap reached
	sealFPMarkNode      = 0x04 // sealed node begins
	sealFPMarkNoSource  = 0x05 // sealed node with no recorded location
	sealFPMarkSource    = 0x06 // sealed node with a recorded location
	sealFPMarkEnd       = 0x07 // sealed node ends (after its cells)
)

// sealFP is the running FNV-1a state plus the walk's termination budget.
type sealFP struct {
	h      uint64
	budget int
}

func (s *sealFP) mixByte(b byte) {
	s.h = (s.h ^ uint64(b)) * sealFPPrime64
}

func (s *sealFP) mixUint64(u uint64) {
	for range 8 {
		s.mixByte(byte(u))
		u >>= 8
	}
}

// mixInt mixes a signed value's two's-complement bit pattern.  The
// sign-changing reinterpretation is deliberate: a fingerprint mixes bit
// patterns, not magnitudes, and negative values (synthetic Pos == -1
// locations, negative literals) must hash distinctly and deterministically.
func (s *sealFP) mixInt(i int) {
	s.mixUint64(uint64(i)) //nolint:gosec // G115: deliberate bit reinterpretation for hashing
}

func (s *sealFP) mixBool(b bool) {
	if b {
		s.mixByte(1)
	} else {
		s.mixByte(0)
	}
}

// mixString is length-prefixed so adjacent strings cannot alias each
// other's boundaries.
func (s *sealFP) mixString(str string) {
	s.mixInt(len(str))
	for i := range len(str) {
		s.mixByte(str[i])
	}
}

// SealedASTFingerprint computes a structural digest of the sealed nodes
// reachable from roots: types, scalar payloads, cell structure, and
// source-location contents, in traversal order.  Any in-place change to a
// sealed node changes the digest; see the file comment for the exact
// coverage (and non-coverage) contract.
//
// The digest is content-based — no pointer identity — so it is
// deterministic across processes: fingerprint a tree at parse time, store
// the value, and compare at any later point to prove the sealed bytes
// never changed.  Fingerprinting never mutates the tree and allocates
// nothing.
func SealedASTFingerprint(roots []*LVal) uint64 {
	s := sealFP{h: sealFPOffset64, budget: sealFPMaxNodes}
	for _, r := range roots {
		s.walk(r, 0)
	}
	return s.h
}

func (s *sealFP) walk(v *LVal, depth int) {
	if v == nil {
		s.mixByte(sealFPMarkNil)
		return
	}
	if !v.sealed {
		// Runtime storage: legitimately mutable, so its contents cannot
		// participate in a stability digest.  Mark the hole, do not
		// descend.  (Singletons land here too: they are never sealed.)
		s.mixByte(sealFPMarkHole)
		return
	}
	if s.budget <= 0 || depth > sealFPMaxDepth {
		s.mixByte(sealFPMarkTruncated)
		return
	}
	s.budget--

	s.mixByte(sealFPMarkNode)
	s.mixUint64(uint64(v.Type))
	s.mixUint64(uint64(v.FunType))
	s.mixBool(v.quoted)
	s.mixBool(v.spliced)
	s.mixString(v.Str)
	s.mixInt(v.Int)
	s.mixUint64(math.Float64bits(v.Float))
	if src := v.source; src == nil {
		s.mixByte(sealFPMarkNoSource)
	} else {
		s.mixByte(sealFPMarkSource)
		s.mixString(src.File)
		s.mixString(src.Path)
		s.mixInt(src.Pos)
		s.mixInt(src.Line)
		s.mixInt(src.Col)
		s.mixInt(src.EndPos)
		s.mixInt(src.EndLine)
		s.mixInt(src.EndCol)
	}
	s.mixBool(v.meta != nil)
	s.mixBool(v.macroExpansion != nil)
	s.mixInt(len(v.Cells))
	for _, c := range v.Cells {
		s.walk(c, depth+1)
	}
	s.mixByte(sealFPMarkEnd)
}
