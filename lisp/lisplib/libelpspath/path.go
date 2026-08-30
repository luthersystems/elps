package libelpspath

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// Path represents an operation on a path.
type Path interface {
	// Get evaluates a get path operation on an elps LVal.
	Get(*lisp.LVal) (*lisp.LVal, error)
	// SetMutate evaluates a mutating set path operation on an elps LVal.
	SetMutate(*lisp.LVal, *lisp.LVal) (*lisp.LVal, error)
	// Set evaluates a set path operation on an elps LVal, and returns a
	// newly constructed LVal.
	Set(*lisp.LVal, *lisp.LVal) (*lisp.LVal, error)
	// DeleteMutate evaluates a mutating delete path operation on an elps LVal.
	DeleteMutate(*lisp.LVal) (*lisp.LVal, error)
	// DeleteMutate evaluates a delete path operation on an elps LVal, and returns
	// a newly constructed LVal.
	Delete(*lisp.LVal) (*lisp.LVal, error)
	// Nil sets elements at the end of a path to be null. If the path references
	// multiple elements (i.e. a range) then all of the elements in that range
	// are set to null.
	Nil(*lisp.LVal) (*lisp.LVal, error)
	// NilMutate mutates elements at the end of a path to be null. If the path
	// references multiple elements (i.e. a range) then all of the elements in
	// that range are set to null.
	NilMutate(*lisp.LVal) (*lisp.LVal, error)
	// String returns a string representation of the path.
	String() string
}

// copyLVal creates a copy of an LVal. For non-containers this is a NOP, since
// non-containers are immutable.
//
// The copy is deep: every container reachable from v is rebuilt, so no write
// through the copy can reach the source. All three container helpers agree on
// that, which TestCopyHelpersAgreeOnNestingDepth pins — a helper that stopped
// one level short would hand back a value that looks independent and is not,
// which is what issue #395 was.
//
// The path operations do not reach it directly. They know which entry or
// which cells they are about to write, and use copyMapOffPath and
// copySeqOffPath to leave that one subtree unwalked — it is discarded on the
// next line either way. Everything OFF the path is copied here, in full:
// sharing an off-path subtree is what #395 was, one query removed. This
// entry point remains for the copies that have no path to be directed by,
// which is the iterator's fallback when an element's own operation fails.
//
// It returns an error rather than a value when v contains itself: such a value
// has no finite copy, and walking one until the goroutine stack overflows
// kills the process in a way recover() cannot intercept. See issue #393.
func copyLVal(v *lisp.LVal) (*lisp.LVal, error) {
	var st cycleState
	return copyGuarded(v, newCycleGuard(&st))
}

// copyGuarded is copyLVal continuing a walk already in progress rather than
// starting a fresh one. Every nested copy must pass g down; a fresh walk per
// level resets the bound on every lap and it never fires.
func copyGuarded(v *lisp.LVal, g cycleGuard) (*lisp.LVal, error) {
	switch v.Type {
	case lisp.LSortMap, lisp.LArray, lisp.LSExpr:
		// The three types that reach other values, and so the only ones
		// entered on the guard's path. Handled below. Entering a leaf would
		// tax every string and int in the value to bound a walk that cannot
		// recurse.
	default:
		// non-containers do not need to be copied since LVals are otherwise
		// immutable
		return v, nil
	}
	g, cyclic := g.descend(v)
	if cyclic {
		return nil, errCyclicValue
	}
	out, err := copyContainer(v, g)
	if g.tracking() {
		g.ascend(v)
	}
	return out, err
}

// copyContainer copies the container types. It is only ever called through
// copyGuarded, which has already established that v is one and put it on g's
// path.
func copyContainer(v *lisp.LVal, g cycleGuard) (*lisp.LVal, error) {
	switch v.Type {
	case lisp.LSortMap:
		return copyMapGuarded(v, g)
	case lisp.LArray:
		if v.Cells[0].Len() > 1 {
			// IMPORTANT: we cannnot recover from this!
			//
			// Unreachable through the builtins, and deliberately still so:
			// okSimpleContainerType refuses a multi-dimensional array before
			// any builtin reaches a copy. The cycle guard above adds a
			// rejection to that gate, it does not remove this one.
			return lisp.Nil(), nil
		}
		return copyVectorGuarded(v, g)
	case lisp.LSExpr:
		return copyListGuarded(v, g)
	default:
		return nil, fmt.Errorf("invalid container type: %v", v.Type)
	}
}

// copyMap creates a new map LVal that contains the same elements in the original
// map.
//
// Nothing in the package calls it any more — the map path operations know
// their key and use copyMapOffPath — but it is one of the three helpers
// TestCopyHelpersAgreeOnNestingDepth holds to a common contract, and that
// test is the drift guard issue #395 asked for. Deleting the helper deletes
// a third of the guard.
func copyMap(v *lisp.LVal) (*lisp.LVal, error) {
	var st cycleState
	return copyMapGuarded(v, newCycleGuard(&st))
}

// copyMapGuarded is copyMap continuing a walk already in progress.
func copyMapGuarded(v *lisp.LVal, g cycleGuard) (*lisp.LVal, error) {
	return copyMapExcept(v, nil, g)
}

// copyMapOffPath is copyMap with one entry left out: the entry at key, which
// the caller is about to overwrite or delete.
//
// This is the path-directed half of the copy. The copy contract is
// unchanged — the returned map shares no container with v, and the entry at
// key is replaced by a value the caller has already built independently —
// but the subtree under key is never walked, because every node of it would
// be discarded the moment the caller writes. That waste was not one subtree:
// setChain/deleteChain/nullChain rebuild the spine one level at a time, and
// each level used to deep-copy its whole subtree including the next level
// down, so the value under a d-step path was copied d times and thrown away
// d-1 of them. Skipping the on-path entry at each level makes every node in
// the value cost exactly one copy.
//
// IMPORTANT: what is skipped is the entry the caller's SetMutate/DeleteMutate
// will land on, not "the entry whose key string matches". See sameMapSlot.
func copyMapOffPath(v *lisp.LVal, key *lisp.LVal) (*lisp.LVal, error) {
	var st cycleState
	return copyMapExcept(v, key, newCycleGuard(&st))
}

// copyMapExcept is the shared body: skip is the key to leave out, or nil to
// copy every entry.
func copyMapExcept(v *lisp.LVal, skip *lisp.LVal, g cycleGuard) (*lisp.LVal, error) {
	m0 := v.Map()
	if m0 == nil {
		return nil, errors.New("first argument is not a map")
	}
	entries := sortedMapEntries(m0)
	if err := lisp.GoError(entries); err != nil {
		return nil, err
	}
	sm := lisp.SortedMap()
	m := sm.Map()
	for _, pair := range entries.Cells {
		if skip != nil && sameMapSlot(pair.Cells[0], skip) {
			continue
		}
		// IMPORTANT: maps may contain containers, in which case we need to copy
		// those containers — the rule copyList and copyVector have always
		// carried. Storing the source's own value here instead, which is what
		// this did before issue #395, produces a map that shares every nested
		// container with its source: the package's own redaction example,
		// (?nil patient "ssn") followed by a ?set! into the result, rewrote
		// the patient record it was supposed to leave alone.
		val, err := copyGuarded(pair.Cells[1], g)
		if err != nil {
			return nil, err
		}
		lerr := m.Set(pair.Cells[0], val)
		if lerr.Type == lisp.LError {
			return nil, lisp.GoError(lerr)
		}
	}
	return sameQuoting(v, sm), nil
}

// sameMapSlot reports whether two keys name the same entry of the map the
// copy is being built into.
//
// The copy is always built into a lisp.SortedMap, whose backing keys strings
// and symbols alike by their text and refuses everything else. Skipping an
// entry because its key "looks equal" by some other rule would drop a
// sibling from the copy, so this mirrors that backing exactly, and
// TestMapSlotRuleMatchesTheMap pins the two together: a key type the backing
// cannot hash is not equal to anything here, and falls through to the Set
// below that reports it, which is what this function did before it was a
// function.
func sameMapSlot(a, b *lisp.LVal) bool {
	return hashableMapKey(a) && hashableMapKey(b) && a.Str == b.Str
}

func hashableMapKey(k *lisp.LVal) bool {
	return k.Type == lisp.LString || k.Type == lisp.LSymbol
}

func sortedMapEntries(m lisp.Map) *lisp.LVal {
	cells := make([]*lisp.LVal, m.Len())
	lerr := m.Entries(cells)
	if lerr.Type == lisp.LError {
		return lerr
	}
	return lisp.QExpr(cells)
}

// copyVector creates a new LVal that contains the same elements in the
// original vector.
func copyVector(v *lisp.LVal) (*lisp.LVal, error) {
	var st cycleState
	return copyVectorGuarded(v, newCycleGuard(&st))
}

// copyVectorGuarded is copyVector continuing a walk already in progress.
func copyVectorGuarded(v *lisp.LVal, g cycleGuard) (*lisp.LVal, error) {
	cells := v.Cells[1].Cells
	cellsCopy := make([]*lisp.LVal, len(cells))
	for i := range cells {
		// IMPORTANT: vectors may contain containers, in which case we need to copy
		// those containers
		c, err := copyGuarded(cells[i], g)
		if err != nil {
			return nil, err
		}
		cellsCopy[i] = c
	}
	return sameQuoting(v, toVector(cellsCopy)), nil
}

// copyList creates a new LVal that contains the same elements in the
// original list.
func copyList(v *lisp.LVal) (*lisp.LVal, error) {
	var st cycleState
	return copyListGuarded(v, newCycleGuard(&st))
}

// copyListGuarded is copyList continuing a walk already in progress.
func copyListGuarded(v *lisp.LVal, g cycleGuard) (*lisp.LVal, error) {
	cells := v.Cells
	cellsCopy := make([]*lisp.LVal, len(cells))
	for i := range cells {
		// IMPORTANT: lists may contain containers, in which case we need to copy
		// those containers
		c, err := copyGuarded(cells[i], g)
		if err != nil {
			return nil, err
		}
		cellsCopy[i] = c
	}
	return sameQuoting(v, toList(cellsCopy)), nil
}

// copySeqOffPath copies a sequence's cells into a fresh sequence of the same
// layout, leaving the half-open range [from,to) uncopied.
//
// It is copyLVal(toVector(cells)) / copyLVal(toList(cells)) — the shape the
// index and range operations have always built their private copy with —
// minus the positions the caller is about to overwrite or splice out. As in
// copyMapOffPath the result shares no container with the source: the skipped
// positions hold nil until the caller fills or removes them, so a copy
// abandoned partway through carries no reference back into the source
// either.
//
// from == to skips nothing and is the plain deep copy.
func copySeqOffPath(in *lisp.LVal, cells []*lisp.LVal, from, to int) (*lisp.LVal, error) {
	out := make([]*lisp.LVal, len(cells))
	var st cycleState
	g := newCycleGuard(&st)
	for i := range cells {
		if i >= from && i < to {
			out[i] = lisp.Nil()
			continue
		}
		// IMPORTANT: sequences may contain containers, in which case we need
		// to copy those containers.
		c, err := copyGuarded(cells[i], g)
		if err != nil {
			return nil, err
		}
		out[i] = c
	}
	// IMPORTANT: the quoting of the wrapper is toVector's and toList's, not
	// in's. copyLVal(toVector(cells)) reached sameQuoting with two values
	// this same pair of constructors had built, so it never changed
	// anything, and the index and range operations have always handed back a
	// sequence quoted the way these two build. Reading the quoting off in
	// here instead would be a behaviour change smuggled in with a copy
	// rework. The nested values keep their own quoting, which is the part
	// #395 had to fix: copyGuarded restores it per value above.
	if in.Type == lisp.LArray {
		return toVector(out), nil
	}
	return toList(out), nil
}

// sameQuoting gives cp src's quoting.
//
// The quote flag is part of the value, not decoration: an unquoted LSExpr is
// an expression the evaluator will try to apply, and a list is a quoted one.
// toList and toVector build unquoted, so a copy silently demoted a quoted
// list to an s-expression — visible before #395 only on a list or vector
// argument, and reachable through every sorted-map value once copyMap
// started rebuilding what it used to share. Quote copies the LVal header
// rather than writing through to a shared one, so this never mutates cp's
// source (issues #333/#382).
func sameQuoting(src, cp *lisp.LVal) *lisp.LVal {
	if src.IsQuoted() && !cp.IsQuoted() {
		return lisp.Quote(cp)
	}
	return cp
}

// toCells gets a slice of LVal cells from an elps vector.
func toCells(in *lisp.LVal) ([]*lisp.LVal, error) {
	if in.IsNil() {
		return nil, errors.New("first argument is nil")
	}
	switch in.Type {
	case lisp.LArray:
		if in.Cells[0].Len() > 1 {
			return nil, errors.New("cannot index multi-dimensional array")
		}
		cells := in.Cells[1].Cells
		return cells, nil
	case lisp.LSExpr:
		cells := in.Cells
		return cells, nil
	default:
		return nil, errors.New("argument is not an array")
	}
}

// errMutateList rejects in-place path mutation of lists (LSExpr). toCells
// returns a list's live cell backing, so the in-place index/range operations
// would shift elements of — and then write LArray dims bookkeeping into —
// whatever shares that backing. When the list is a quoted program literal
// the parse cache aliases it into every warm environment, and the mutation
// corrupts the shared AST before the dims access panics (lists have no dims
// cell). Rejecting lists up front turns that panic into a catchable error
// and closes the corruption vector; arrays and sorted-maps keep their
// documented in-place semantics.
func errMutateList(in *lisp.LVal) error {
	if in != nil && in.Type == lisp.LSExpr {
		return errors.New("elpspath: in-place path operations require an array or sorted-map; got list")
	}
	return nil
}

// storeCells writes a reworked cell slice back into the sequence it came
// from, honouring that sequence's LAYOUT.
//
// IMPORTANT: the two sequence types toCells accepts do not store their cells
// the same way. An array is [dims, data] — the cells live in Cells[1] and
// the element count is bookkeeping in Cells[0] — while a list holds its
// cells directly. The del and range-set paths used to write the array shape
// unconditionally (`in.Cells[1].Cells = vals`), which is correct only
// because the MUTATING entry points reject lists (errMutateList).
//
// The non-mutating Set/Delete/Nil reach the same write-back through the
// unguarded setMutate/deleteMutate on a private copy, and that copy is a
// LIST whenever the input was one. On such a copy the array write either
// panicked — a one-element list has no Cells[1], which is how a fuzz input
// found this — or, worse, silently wrote the new cell slice into an
// ELEMENT of the list and then corrupted a second element's Int field as
// "dims". A caller asking for a copy got a mangled structure back and no
// error. Dispatching on the layout is what makes the unguarded paths
// correct for both types.
func storeCells(in *lisp.LVal, vals []*lisp.LVal) {
	if in.Type == lisp.LArray {
		// IMPORTANT: this is the documented in-place rework of a
		// caller-owned array's cell storage (?del!, ?set! range splice).
		// List inputs are rejected by errMutateList on the mutating entry
		// points, and the non-mutating ones pass a private copy.
		//elps:mutates the documented in-place rework of a caller-owned array's data cells (?del!, ?set! range splice); lists are refused by errMutateList and the copying ops pass a private copy
		in.Cells[1].Cells = vals
		dims := in.Cells[0]
		//elps:mutates dims bookkeeping for the array rework immediately above, on the same caller-owned array
		dims.Cells[0].Int = len(vals)
		return
	}
	// IMPORTANT: this writes list cell storage, and is reachable only from
	// the non-mutating Set/Delete/Nil, which pass a freshly constructed
	// private copy, never a caller-owned or program-literal list.
	//
	// A program literal cannot arrive here by the other route either: the
	// mutating entry points refuse lists outright (errMutateList), which is
	// the substrate#378 fix, so the seal is never the last line of defence
	// on this write.
	//elps:mutates writes the private copy the non-mutating ops built; the mutating ops never reach here with a list (errMutateList)
	in.Cells = vals
}

// toVector converts a slice of LVal cells into an elps vector.
//
// IMPORTANT: the cells become the vector's backing storage; they are not
// copied. Only call this with FRESH storage the caller owns. To wrap cells
// BORROWED from an existing sequence, call alias, which carries the source's
// seal across.
func toVector(cells []*lisp.LVal) *lisp.LVal {
	return lisp.Array(nil, cells)
}

// toList converts a slice of LVal cells into an elps list.
//
// IMPORTANT: as toVector — fresh storage only; use alias for borrowed cells.
func toList(cells []*lisp.LVal) *lisp.LVal {
	return lisp.SExpr(cells)
}

// alias mints a fresh LVal of in's sequence type over cells, which are
// BORROWED from in: a whole or partial window onto the same backing array
// that in stores its elements in.
//
// This is the one constructor allowed to wrap storage the package does not
// own, and it exists to enforce a single rule:
//
//	any LVal minted over borrowed backing inherits the source's constraint.
//
// The constraint is the sealed flag (lisp/seal.go). A program literal arrives
// here sealed, the parse behind it is shared by every environment the host
// runs, and the kernel's sealed-write guard sites — stable-sort, append 'vector,
// slice 'vector — are what keep those environments from treading on each
// other. Every one of them keys off the flag on the value they are handed. A
// fresh header minted by lisp.SExpr or lisp.Array has sealed == false, so
// handing one back over a literal's live cells turns all three guards off at
// once and the literal is mutated in place, permanently, process-wide (issue
// #392, and substrate#378 before it).
//
// THE THREE-INDEX CLAMP IS NOT A SUBSTITUTE, and the two solve different
// halves. Issue #373's clamp (cells[from:to:to] at the call site) stops an
// append reaching PAST the window into the source's spare capacity. It does
// nothing about a write WITHIN the window, which is where stable-sort writes,
// and which for a sealed source is a write to shared program storage. Both
// are needed; neither implies the other.
//
// The kernel hits the identical situation in builtinSlice, builtinCDR and
// builtinRest, and resolves it the same way: "a sealed input's constraint
// travels with the shared backing."
//
// Propagation, not copying, is deliberate. It is what the kernel does; it
// keeps this a genuinely O(1) query, which is what the ?-family is for and
// how substrate uses it on the transaction path; and it is sufficient,
// because the constraint is honoured by everything downstream that could
// write through the window. Copying instead would also be correct and would
// cost an allocation proportional to the window on every sealed read.
//
// For an ARRAY input this is a plain wrap: arrays are runtime values, are
// never sealed (SealAST declines to mark them), and lisp.Array always mints
// its own data holder, so there is no constraint to carry. InheritSeal
// enforces that rather than trusting it — it refuses to mark an array,
// because a "sealed" vector would be a lie: append! and assoc! write vector
// backing without consulting the flag.
func alias(in *lisp.LVal, cells []*lisp.LVal) *lisp.LVal {
	var out *lisp.LVal
	if in.Type == lisp.LArray {
		out = toVector(cells)
	} else {
		out = toList(cells)
	}
	out.InheritSeal(in)
	return out
}

// rootPath wraps the top level path. This is mainly to handle the special
// case of printing a  "."' in String() for the top level path.
type rootPath struct {
	path Path
}

// Root wraps the top level path. This is mainly to handle the special
// case of printing a  "."' in String() for the top level path.
func Root(path Path) Path {
	return &rootPath{path: path}
}

// Get is a root level proxy to an underlying path Get.
func (s *rootPath) Get(in *lisp.LVal) (*lisp.LVal, error) {
	return s.path.Get(in)
}

// SetMutate is a root level proxy to an underlying path SetMutate.
func (s *rootPath) SetMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	return s.path.SetMutate(in, newIn)
}

// Set is a root level proxy to an underlying path Set.
func (s *rootPath) Set(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	return s.path.Set(in, newIn)
}

// DeleteMutate is a root level proxy to an underlying path DeleteMutate.
func (s *rootPath) DeleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	return s.path.DeleteMutate(in)
}

// Delete is a root level proxy to an underlying path Delete.
func (s *rootPath) Delete(in *lisp.LVal) (*lisp.LVal, error) {
	return s.path.Delete(in)
}

// NilMutate is a root level proxy to an underlying path NilMutate.
func (s *rootPath) NilMutate(in *lisp.LVal) (*lisp.LVal, error) {
	return s.path.NilMutate(in)
}

// Nil is a root level proxy to an underlying path Nil.
func (s *rootPath) Nil(in *lisp.LVal) (*lisp.LVal, error) {
	return s.path.Nil(in)
}

// String is a root level proxy to an underlying path String. It prepends
// a dot "." since all root paths by convention begin with dot.
func (s *rootPath) String() string {
	var sb strings.Builder
	s.appendString(&sb)
	return sb.String()
}

func (s *rootPath) appendString(sb *strings.Builder) {
	// IMPORTANT: root always starts with "."!
	sb.WriteString(".")
	appendPathString(sb, s.path)
}

// stringAppender renders a path into a builder the caller owns.
//
// The three composites -- root, chain and iter -- must render through this
// rather than by asking each child for its own String(). Materialising a
// child's full string and copying it into the parent's costs one
// full-length allocation and copy per level of nesting, which is O(depth^2)
// in bytes. The leaves are O(1) either way.
//
// It is unexported and Path does NOT require it, so an embedder's own Path
// implementation keeps working: appendPathString falls back to String() for
// anything that does not implement this.
type stringAppender interface {
	appendString(*strings.Builder)
}

// appendPathString writes p's rendering into sb, taking the linear route
// when p is one of this package's own types.
func appendPathString(sb *strings.Builder, p Path) {
	if a, ok := p.(stringAppender); ok {
		a.appendString(sb)
		return
	}
	sb.WriteString(p.String())
}

// expandPaths removes all nested chains to construct a normalized slice
// of paths that does not contain any nested chain paths.
func expandPaths(paths ...Path) []Path {
	var newPaths []Path
	for _, path := range paths {
		switch chain := path.(type) {
		case *chainPath:
			subPaths := expandPaths(chain.paths...)
			newPaths = append(newPaths, subPaths...)
		case *iterPath:
			newPaths = append(newPaths, Iter())
			subPaths := expandPaths(chain.path)
			newPaths = append(newPaths, subPaths...)
		default:
			newPaths = append(newPaths, path)
		}
	}
	return newPaths
}

// normalizePaths constructs a normalized chain of paths. This normalization
// is necessary to properly handle nested iterator paths.
//
// TWO SHAPES ARE LOAD-BEARING HERE, both for cost. Either one reintroduced
// makes construction superlinear in an input a caller may not control,
// before any document is touched.
//
// The iterator branch builds its iterPath DIRECTLY. Iter(acc...) is
// &iterPath{path: Chain(acc...)}, and Chain calls back into this function,
// so every iterator would re-normalize the whole tail built so far -- and
// normalizing re-runs expandPaths, which flattens each nested iterator's
// chain only for the loop to rebuild it. One re-entry per iterator, each
// over what the previous rebuilt, is exponential.
//
// Skipping that re-entry is sound because the accumulator is assembled by
// this loop out of expandPaths output, so it is already normalized, and
// normalizePaths is idempotent on it: expandPaths would flatten it back to
// exactly the sequence the loop consumed. TestNormalizePathsIsIdempotent
// pins that, because it is what the shape RESTS ON -- if it stops holding,
// paths change meaning rather than merely cost, and no cost test would
// notice.
//
// The chain accumulates REVERSED and is flipped once at the end. Prepending
// -- append([]Path{step}, acc...) -- allocates a fresh slice and copies
// every element already placed, on every step. That is one slice per step
// either way, so the allocation COUNT stays linear and a count-based test
// cannot see it; the BYTES are quadratic.
//
// Cost is pinned by TestNormalizePathsIsNotExponential and
// TestNormalizePathsIsNotQuadratic, which assert allocations and bytes
// respectively rather than wall time. Equality of meaning is pinned by the
// idempotency test above, TestNormalizePathsAgreesWithIterConstruction, and
// the corpus, round-trip and iterator-collapse tests.
func normalizePaths(paths ...Path) []Path {
	paths = expandPaths(paths...)
	// Right to left, so the chain accumulates reversed; see the doc comment.
	rev := make([]Path, 0, len(paths))
	for i := len(paths) - 1; i >= 0; i-- {
		if _, isIter := paths[i].(*iterPath); !isIter {
			rev = append(rev, paths[i])
			continue
		}
		// An iterator takes everything to its right as its own chain, so
		// the accumulated tail is flipped into it and the accumulator
		// restarts holding just the iterator. inner is a fresh slice, so
		// reusing rev's storage on the next line cannot write through it.
		// Each element is flipped at most once per enclosing iterator and
		// an iterator empties the accumulator, so this stays linear.
		inner := reversedPaths(rev)
		rev = append(rev[:0], &iterPath{path: &chainPath{paths: inner}})
	}
	return reversedPaths(rev)
}

// reversedPaths returns a new slice holding in's elements in reverse order.
//
// It returns nil rather than an empty slice for an empty input, so that an
// empty chain keeps the nil paths field it had when this was built by
// prepending onto a nil slice. Nothing reads the difference -- every use is
// len() or a range -- but preserving it keeps the change to cost alone.
func reversedPaths(in []Path) []Path {
	if len(in) == 0 {
		return nil
	}
	out := make([]Path, len(in))
	for i, p := range in {
		out[len(in)-1-i] = p
	}
	return out
}

type chainPath struct {
	paths []Path
}

// Chain combines a chain of paths.
func Chain(paths ...Path) Path {
	return &chainPath{paths: normalizePaths(paths...)}
}

// Get an LVal at the end of a path chain.
func (s *chainPath) Get(in *lisp.LVal) (*lisp.LVal, error) {
	var err error
	for _, path := range s.paths {
		in, err = path.Get(in)
		if err != nil {
			return nil, err
		}
	}
	return in, nil
}

func (s *chainPath) SetMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	if len(s.paths) == 0 {
		// we have to be careful here because we cannot mutate a container from
		// one type to another (e.g., a sorted-map to an list), since LVals
		// are immutable. Similarly, we cannot mutate non containers which are
		// immutable LVals. Given these strict requirements, we simply return an
		// error for now, and we can implement this later if it really is
		// necessary.
		// We do allow non-mutating Set on root context.
		return nil, errors.New("cannot mutate root context")
	}
	var err error
	curIn := in
	for i, path := range s.paths {
		if i == (len(s.paths) - 1) {
			curIn, err = path.SetMutate(curIn, newIn)
		} else {
			curIn, err = path.Get(curIn)
		}
		if err != nil {
			return nil, err
		}
	}
	return in, nil
}

func setChain(in *lisp.LVal, newIn *lisp.LVal, paths []Path) (*lisp.LVal, error) {
	if len(paths) == 0 {
		// in this case we're replacing the entire input with a new input
		return newIn, nil
	}
	head := paths[0]
	if len(paths) > 1 {
		childIn, err := head.Get(in)
		if err != nil {
			return nil, err
		}
		newIn, err = setChain(childIn, newIn, paths[1:])
		if err != nil {
			return nil, err
		}
	}
	return head.Set(in, newIn)
}

func (s *chainPath) Set(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	return setChain(in, newIn, s.paths)
}

func (s *chainPath) DeleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	var err error
	curIn := in
	for i, path := range s.paths {
		if i == (len(s.paths) - 1) {
			curIn, err = path.DeleteMutate(curIn)
		} else {
			curIn, err = path.Get(curIn)
		}
		if err != nil {
			return nil, err
		}
	}
	return in, nil
}

func deleteChain(in *lisp.LVal, paths []Path) (*lisp.LVal, error) {
	if len(paths) == 0 {
		// Deleting the whole document leaves nothing, which is lisp nil --
		// the same answer nullChain gives for the same empty chain.
		//
		// IMPORTANT: this must be lisp.Nil() and not a bare Go nil. An
		// untyped nil *LVal escapes into the returned structure and panics
		// the first thing that touches it (json:dump-bytes, printing, a
		// further path op) rather than raising a catchable condition. The
		// empty chain is reached both by the root path ((?del v)) and by
		// every element of a bare iterator ((?del v '*)), so a Go nil here
		// poisons a whole array, not just one value.
		return lisp.Nil(), nil
	}
	head := paths[0]
	if len(paths) > 1 {
		childIn, err := head.Get(in)
		if err != nil {
			return nil, err
		}
		newIn, err := deleteChain(childIn, paths[1:])
		if err != nil {
			return nil, err
		}
		return head.Set(in, newIn)
	}
	return head.Delete(in)
}

func (s *chainPath) Delete(in *lisp.LVal) (*lisp.LVal, error) {
	return deleteChain(in, s.paths)
}

func (s *chainPath) NilMutate(in *lisp.LVal) (*lisp.LVal, error) {
	if len(s.paths) == 0 {
		// we have to be careful here because we cannot mutate a container from
		// one type to another (e.g., a sorted-map to an list), since LVals
		// are immutable. Similarly, we cannot mutate non containers which are
		// immutable LVals. Given these strict requirements, we simply return an
		// error for now, and we can implement this later if it really is
		// necessary.
		// We do allow non-mutating Set on root context.
		return nil, errors.New("cannot mutate root context")
	}
	var err error
	curIn := in
	for i, path := range s.paths {
		if i == (len(s.paths) - 1) {
			curIn, err = path.NilMutate(curIn)
		} else {
			curIn, err = path.Get(curIn)
		}
		if err != nil {
			return nil, err
		}
	}
	return in, nil
}

func nullChain(in *lisp.LVal, paths []Path) (*lisp.LVal, error) {
	if len(paths) == 0 {
		return lisp.Nil(), nil
	}
	head := paths[0]
	if len(paths) > 1 {
		childIn, err := head.Get(in)
		if err != nil {
			return nil, err
		}
		newIn, err := nullChain(childIn, paths[1:])
		if err != nil {
			return nil, err
		}
		return head.Set(in, newIn)
	}
	return head.Nil(in)
}

func (s *chainPath) Nil(in *lisp.LVal) (*lisp.LVal, error) {
	return nullChain(in, s.paths)
}

func (s *chainPath) String() string {
	var sb strings.Builder
	s.appendString(&sb)
	return sb.String()
}

func (s *chainPath) appendString(sb *strings.Builder) {
	for _, path := range s.paths {
		appendPathString(sb, path)
	}
}

type dotPath struct {
	key string
}

// Dot performs a map index operation (e.g., a["b"], a.b).
func Dot(key string) Path {
	return &dotPath{key: key}
}

func (s *dotPath) Get(in *lisp.LVal) (*lisp.LVal, error) {
	if in.IsNil() {
		return nil, errors.New("first argument is nil")
	}
	switch in.Type {
	case lisp.LSortMap:
		mmap := in.Map()
		v, ok := mmap.Get(lisp.String(s.key))
		if ok {
			return v, nil
		}
		return lisp.Nil(), nil
	default:
		return nil, errors.New("first argument is not a map")
	}
}

func (s *dotPath) SetMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	if in.IsNil() {
		return nil, errors.New("first argument is nil")
	}
	switch in.Type {
	case lisp.LSortMap:
		mmap := in.Map()
		mmap.Set(lisp.String(s.key), newIn)
		return in, nil
	default:
		return nil, errors.New("first argument is not a map")
	}
}

func (s *dotPath) Set(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	if in.IsNil() {
		return nil, errors.New("first argument is nil")
	}
	switch in.Type {
	case lisp.LSortMap:
		// The entry at this key is about to become newIn, which the chain
		// has already rebuilt independently of in. Copying the source's
		// subtree under the key first would build a value with exactly one
		// use: being overwritten on the next line.
		cp, err := copyMapOffPath(in, lisp.String(s.key))
		if err != nil {
			return nil, err
		}
		return s.SetMutate(cp, newIn)
	default:
		return nil, errors.New("first argument is not a map")
	}
}

func (s *dotPath) DeleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	if in.IsNil() {
		return nil, errors.New("first argument is nil")
	}
	switch in.Type {
	case lisp.LSortMap:
		mmap := in.Map()
		mmap.Del(lisp.String(s.key))
		return in, nil
	default:
		return nil, errors.New("first argument is not a map")
	}
}

func (s *dotPath) Delete(in *lisp.LVal) (*lisp.LVal, error) {
	if in.IsNil() {
		return nil, errors.New("first argument is nil")
	}
	switch in.Type {
	case lisp.LSortMap:
		// The entry at this key is about to be removed, so it is copied out
		// by being left out. DeleteMutate below is then a no-op on it, which
		// is the same answer it gave for an absent key before.
		cp, err := copyMapOffPath(in, lisp.String(s.key))
		if err != nil {
			return nil, err
		}
		return s.DeleteMutate(cp)
	default:
		return nil, errors.New("first argument is not a map")
	}
}

func (s *dotPath) NilMutate(in *lisp.LVal) (*lisp.LVal, error) {
	return s.SetMutate(in, lisp.Nil())
}

func (s *dotPath) Nil(in *lisp.LVal) (*lisp.LVal, error) {
	return s.Set(in, lisp.Nil())
}

func (s *dotPath) String() string {
	return fmt.Sprintf(`[%q]`, s.key)
}

func (s *dotPath) appendString(sb *strings.Builder) {
	sb.WriteString(s.String())
}

type indexPath struct {
	index int
}

// Index performs an array index operation (e.g., [i]).
func Index(index int) Path {
	return &indexPath{index: index}
}

func (s *indexPath) Get(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	index, ok := resolveIndex(len(cells), s.index)
	if !ok {
		return lisp.Nil(), nil
	}
	return cells[index], nil
}

// resolveIndex converts a (possibly negative) path index into a real offset
// into an n-element sequence, reporting whether it lands inside it.
//
// IMPORTANT: a negative index counts back from the end, so it must be
// re-checked against zero after being folded. Without that second check an
// index whose magnitude exceeds the sequence length ((? v -1) on an empty
// array, (? v -5) on a two-element one) stays negative and indexes out of
// bounds, panicking instead of raising a condition lisp code can catch.
// Both out-of-range directions take the same branch, which is the
// long-standing behaviour for indexes past the end.
func resolveIndex(n int, index int) (int, bool) {
	if index < 0 {
		index = n + index
	}
	if index < 0 || index >= n {
		return 0, false
	}
	return index, true
}

func (s *indexPath) SetMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	if err := errMutateList(in); err != nil {
		return nil, err
	}
	return s.setMutate(in, newIn)
}

// setMutate is SetMutate without the list guard. It is called directly by
// the non-mutating Set, which operates on a private copy that may be a
// list.
func (s *indexPath) setMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	index, ok := resolveIndex(len(cells), s.index)
	if !ok {
		return lisp.Nil(), nil
	}
	cells[index] = newIn
	return in, nil
}

func (s *indexPath) Set(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	from, to := skipIndex(len(cells), s.index)
	cp, err := copySeqOffPath(in, cells, from, to)
	if err != nil {
		return nil, err
	}
	return s.setMutate(cp, newIn)
}

// skipIndex is the half-open range of positions an index operation is about
// to overwrite or remove, and so the positions copySeqOffPath can leave
// alone. An index that does not land inside the sequence skips nothing: the
// copy is still made and still handed to the mutating half, which answers
// the out-of-range index exactly as it did before.
func skipIndex(n, index int) (int, int) {
	i, ok := resolveIndex(n, index)
	if !ok {
		return 0, 0
	}
	return i, i + 1
}

func (s *indexPath) Delete(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	from, to := skipIndex(len(cells), s.index)
	cp, err := copySeqOffPath(in, cells, from, to)
	if err != nil {
		return nil, err
	}
	return s.deleteMutate(cp)
}

func (s *indexPath) DeleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	if err := errMutateList(in); err != nil {
		return nil, err
	}
	return s.deleteMutate(in)
}

// deleteMutate is DeleteMutate without the list guard. It is called
// directly by the non-mutating Delete, which operates on a private copy.
func (s *indexPath) deleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	index, ok := resolveIndex(n, s.index)
	if !ok {
		return lisp.Nil(), nil
	}
	// IMPORTANT: the compaction is built in a slice this function allocates,
	// not by shifting cells down inside their own backing array.
	//
	// `append(cells[:index], cells[index+1:]...)` writes the tail one position
	// to the left THROUGH cells' array. That array does not belong to this
	// function whenever in is a VIEW over a longer sequence -- and the
	// mutating builtins accept one, because a view is an ordinary array LVal
	// (the kernel's (slice 'vector ...), or this package's own rangePath.Get).
	// The shift then lands in the aliased source, which cannot shrink, so it
	// is left scrambled rather than shortened:
	//
	//	(set 'v (vector 1 2 3 4 5))
	//	(set 'w (slice 'vector v 0 3))
	//	(?del! w 0)
	//	  view (vector 2 3)   src (vector 2 3 3 4 5)   <- 1 gone, 3 duplicated
	//
	// Nothing raised, and the ANSWER was right: a left shift copies before it
	// overwrites, so the view came out correct while the source was wrecked.
	// That is why neither the suite nor the fuzzer caught it -- see issue
	// #471, and rangePath.setMutate, which carries this same comment because
	// its overlap corrupted its own answer and so was found immediately.
	//
	// This is the same class as #369/#373 but no capacity clamp reaches it:
	// the write is WITHIN len, not past it. The capacity below is exact, so
	// the cost is one allocation on a path that previously did none.
	vals := make([]*lisp.LVal, 0, n-1)
	vals = append(vals, cells[:index]...)
	vals = append(vals, cells[index+1:]...)
	storeCells(in, vals)
	return in, nil
}

func (s *indexPath) NilMutate(in *lisp.LVal) (*lisp.LVal, error) {
	return s.SetMutate(in, lisp.Nil())
}

func (s *indexPath) Nil(in *lisp.LVal) (*lisp.LVal, error) {
	return s.Set(in, lisp.Nil())
}

func (s *indexPath) String() string {
	return "[" + strconv.Itoa(s.index) + "]"
}

func (s *indexPath) appendString(sb *strings.Builder) {
	sb.WriteString(s.String())
}

type rangePath struct {
	from       int
	to         int
	implicitTo bool
}

// Range performs array slice operations (e.g., [a:b]) .
func Range(from int, to int, implicitTo bool) Path {
	return &rangePath{from: from, to: to, implicitTo: implicitTo}
}

// Get returns a sequence view over the input's own backing array, with the
// capacity clamped to its length.
//
// IMPORTANT: the clamp is load-bearing, and it is the settlement of issues
// #369 and #373 applied to this call site.
//
// An earlier revision of this package left the slice two-index, so the view
// kept the source's spare capacity and a later (append 'vector ...) into
// that capacity wrote through to the source — including to a quoted program
// literal a parse cache shares between environments. That was defensible
// while the kernel's own (slice 'vector ...) did the same thing: clamping
// one producer would have closed the class for one producer and left every
// other one open.
//
// #373 has since settled it the other way. lisp.clampCap now runs at every
// kernel producer — "every sequence view that escapes into lisp is clamped
// where it is produced, and every non-mutating append clamps its input where
// it is read" — so an unclamped view from here would be the ONLY remaining
// producer of the aliasing, not one of many. Measured on the settled tree
// before this clamp was added:
//
//	(set 'v (vector 1 2 3 4 5))
//	(set 'w (? v '(range 0 3)))  (append! w 99)
//	  view (vector 1 2 3 99)   src (vector 1 2 3 99 5)   <- through this Get
//	(set 'w (slice 'vector v 0 3))  (append! w 99)
//	  view (vector 1 2 3 99)   src (vector 1 2 3 4 5)    <- the kernel, clamped
//
// The clamp is free: no allocation, no copy, and a no-op for the
// exact-capacity slices that are the common case. What it costs is that an
// append which would have grown into the source's spare capacity now
// reallocates, which is the point.
//
// lisp.clampCap itself is unexported, so the three-index slice is written
// out here rather than called.
func (s *rangePath) Get(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	from, to, err := validateRange(n, s.from, s.to, s.implicitTo)
	if err != nil {
		return nil, err
	}
	// Three-index, so an append through the window cannot reach the source's
	// spare capacity (issue #373); alias, so the window carries the source's
	// seal for the writes that land INSIDE it (see alias).
	cells = cells[from:to:to]
	return alias(in, cells), nil
}

func (s *rangePath) SetMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	if err := errMutateList(in); err != nil {
		return nil, err
	}
	return s.setMutate(in, newIn)
}

// setMutate is SetMutate without the list guard. It is called directly by
// the non-mutating Set and Nil, which operate on a private copy that may be
// a list.
func (s *rangePath) setMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	from, to, err := validateRange(n, s.from, s.to, s.implicitTo)
	if err != nil {
		return nil, err
	}
	setCells, err := toCells(newIn)
	if err != nil {
		return nil, err
	}
	// IMPORTANT: the splice is built in a slice this function allocates, not
	// by appending onto cells' own prefix.
	//
	// `append(cells[:from], setCells...)` writes setCells THROUGH cells'
	// backing array starting at from, and the tail read two lines later —
	// cells[to:] — comes out of that same, already-overwritten array. Whenever
	// the replacement is longer than the range it replaces, the elements in
	// [to, from+len(setCells)) are clobbered before they are read and the
	// result repeats the replacement's own tail instead of the source's:
	//
	//	(?set (vector 1 2 3 4 5) '(range 0 1) (vector 97 98 99))
	//	  gave (vector 97 98 99 98 99 4 5)
	//	  want (vector 97 98 99 2 3 4 5)
	//
	// Silent — no error, just a wrong answer. It bit deterministically on the
	// copying Set, whose private copy from copySeqOffPath always has
	// cap == len, so short splices never escaped into a reallocation.
	//
	// This is the same defect as #373/#392: appending into a backing array the
	// function does not own. The capacity below is exact, but correctness does
	// not rest on it — every element is copied into a slice nothing else
	// references, so a wrong capacity would cost an allocation, not an answer.
	vals := make([]*lisp.LVal, 0, from+len(setCells)+(n-to))
	vals = append(vals, cells[:from]...)
	vals = append(vals, setCells...)
	vals = append(vals, cells[to:]...)
	storeCells(in, vals)
	return in, nil
}

func (s *rangePath) Set(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	from, to, err := validateRange(n, s.from, s.to, s.implicitTo)
	if err != nil {
		return nil, err
	}
	// The range is about to be spliced out and replaced by newIn's cells.
	cp, err := copySeqOffPath(in, cells, from, to)
	if err != nil {
		return nil, err
	}
	return s.setMutate(cp, newIn)
}

func (s *rangePath) DeleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	if err := errMutateList(in); err != nil {
		return nil, err
	}
	return s.deleteMutate(in)
}

// deleteMutate is DeleteMutate without the list guard. It is called
// directly by the non-mutating Delete, which operates on a private copy.
func (s *rangePath) deleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	from, to, err := validateRange(n, s.from, s.to, s.implicitTo)
	if err != nil {
		return nil, err
	}
	// IMPORTANT: allocated rather than compacted in place, for exactly the
	// reason indexPath.deleteMutate above spells out -- `append(cells[:from],
	// cells[to:]...)` shifts the tail left through a backing array this
	// function does not own when in is a view, scrambling the source it
	// aliases (issue #471). Both delete paths had it; the range one reaches it
	// through '(range a b) rather than an integer step:
	//
	//	(?del! w '(range 0 1))   ->   src (vector 2 3 3 4 5)
	//
	// The old code skipped the append entirely when to == n, which is why a
	// delete of a suffix -- including the whole view -- was accidentally
	// correct: there was no tail to shift. Only a delete with a non-empty tail
	// wrote through, which narrowed the shapes that could expose it without
	// making any of them safe.
	vals := make([]*lisp.LVal, 0, n-(to-from))
	vals = append(vals, cells[:from]...)
	vals = append(vals, cells[to:]...)
	storeCells(in, vals)
	return in, nil
}

func (s *rangePath) Delete(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	from, to, err := validateRange(n, s.from, s.to, s.implicitTo)
	if err != nil {
		return nil, err
	}
	// The range is about to be removed.
	cp, err := copySeqOffPath(in, cells, from, to)
	if err != nil {
		return nil, err
	}
	return s.deleteMutate(cp)
}

func (s *rangePath) NilMutate(in *lisp.LVal) (*lisp.LVal, error) {
	if err := errMutateList(in); err != nil {
		return nil, err
	}
	return s.nilMutate(in)
}

// nilMutate is NilMutate without the list guard. It is called directly by
// the non-mutating Nil, which operates on a private copy.
func (s *rangePath) nilMutate(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	n := len(cells)
	from, to, err := validateRange(n, s.from, s.to, s.implicitTo)
	if err != nil {
		return nil, err
	}
	var newCells []*lisp.LVal
	for i := from; i < to; i++ {
		newCells = append(newCells, lisp.Nil())
	}

	var newVal *lisp.LVal
	if in.Type == lisp.LArray {
		newVal = toVector(newCells)
	} else {
		newVal = toList(newCells)
	}

	return s.setMutate(in, newVal)
}

func (s *rangePath) Nil(in *lisp.LVal) (*lisp.LVal, error) {
	cells, err := toCells(in)
	if err != nil {
		return nil, err
	}
	// nilMutate below overwrites the range with nils, so the values in it
	// are copied out by being left out. A range this validate rejects skips
	// nothing and nilMutate reports the same error it did before.
	from, to, err := validateRange(len(cells), s.from, s.to, s.implicitTo)
	if err != nil {
		from, to = 0, 0
	}
	cp, err := copySeqOffPath(in, cells, from, to)
	if err != nil {
		return nil, err
	}
	return s.nilMutate(cp)
}

// String renders the slice in the half-open [from:to) notation the jq-style
// spellings use.
//
// An implicit end renders as "[from:]" and NOT as the stored to, which is
// meaningless in that case -- validateRange overwrites it with the input
// length. Printing it would render Range(1, 0, true) as "[1:0]", an empty
// slice, and not a path that parses back to itself.
func (s *rangePath) String() string {
	if s.implicitTo {
		return "[" + strconv.Itoa(s.from) + ":]"
	}
	return "[" + strconv.Itoa(s.from) + ":" + strconv.Itoa(s.to) + "]"
}

func (s *rangePath) appendString(sb *strings.Builder) {
	sb.WriteString(s.String())
}

func validateRange(n int, from int, to int, implicitTo bool) (int, int, error) {
	if from < 0 {
		from = n + from
	}
	if implicitTo {
		to = n
	}
	if to < 0 {
		to = n + to
	}
	if from < 0 {
		return 0, 0, errors.New("index out of range")
	}
	if from > n {
		return 0, 0, errors.New("index out of range")
	}
	if to < 0 {
		return 0, 0, errors.New("index out of range")
	}
	if to > n {
		return 0, 0, errors.New("index out of range")
	}
	if from > to {
		return 0, 0, errors.New("end before start")
	}
	return from, to, nil
}

// iterPath allows executing a path query on each element of na array.
type iterPath struct {
	path Path
}

// Iter iterates over an array of chains (e.g., "a[].b").
func Iter(paths ...Path) Path {
	return &iterPath{path: Chain(paths...)}
}

// isChainToIter is a helper to check if a path is a (normalized) chain
// is a path to an iterator. This is useful to collapse nested iterators.
func isChainToIter(path Path) bool {
	switch chain := path.(type) {
	case *chainPath:
		if len(chain.paths) == 0 {
			return false
		}
		lastEle := chain.paths[len(chain.paths)-1]
		switch lastEle.(type) {
		case *iterPath:
			return true
		default:
			return false
		}
	default:
		return false
	}
}

// Get is called to iterate on elements of an array and get a chain on
// each element.
func (s *iterPath) Get(in *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	collapseIter := isChainToIter(s.path)
	var results []*lisp.LVal
	for _, item := range horizon {
		in, err := s.path.Get(item)
		if err != nil {
			// IMPORTANT: when iterating we ignore paths where query fails,
			// and return nil. This is similar, but not the same as `jq`
			// semantics which will return an error in some cases.
			in = lisp.Nil()
		}
		if collapseIter {
			// collapse results generated by a nested iterator
			childIns, err := toCells(in)
			if err != nil {
				return nil, err
			}
			results = append(results, childIns...)
		} else {
			results = append(results, in)
		}
	}

	var newVal *lisp.LVal
	if in.Type == lisp.LArray {
		newVal = toVector(results)
	} else {
		newVal = toList(results)
	}
	return newVal, nil
}

// SetMutate mutates an array by setting each item using a path. If an
// error occurs while setting an item then that item is skipped.
func (s *iterPath) SetMutate(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	for _, item := range horizon {
		_, err := s.path.SetMutate(item, newIn)
		if err != nil {
			// IMPORTANT: when iterating we ignore paths where set fails,
			// and return nil. This is similar, but not the same as `jq`
			// semantics which will return an error in some cases.
			continue
		}
	}

	return in, nil
}

// Set creates a new array by setting each item using a path. If an
// error occurs while setting an item then that item is skipped.
func (s *iterPath) Set(in *lisp.LVal, newIn *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	var results []*lisp.LVal
	for _, item := range horizon {
		in, err := s.path.Set(item, newIn)
		if err != nil {
			// IMPORTANT: when iterating we ignore paths where set fails,
			// and return orig. item. This is similar, but not the same as `jq`
			// semantics which will return an error in some cases.
			in, err = copyLVal(item)
			if err != nil {
				return nil, err
			}
		}
		results = append(results, in)
	}

	var newVal *lisp.LVal
	if in.Type == lisp.LArray {
		newVal = toVector(results)
	} else {
		newVal = toList(results)
	}
	return newVal, nil
}

func (s *iterPath) DeleteMutate(in *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	for _, item := range horizon {
		_, err := s.path.DeleteMutate(item)
		if err != nil {
			// IMPORTANT: when iterating we ignore paths where delete fails,
			// This is similar, but not the same as `jq` semantics which will
			// return an error in some cases.
			continue
		}
	}

	return in, nil
}

func (s *iterPath) Delete(in *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	var results []*lisp.LVal
	for _, item := range horizon {
		in, err := s.path.Delete(item)
		if err != nil {
			// IMPORTANT: when iterating we ignore paths where del fails,
			// and return orig. item. This is similar, but not the same as `jq`
			// semantics which will return an error in some cases.
			in, err = copyLVal(item)
			if err != nil {
				return nil, err
			}
		}
		results = append(results, in)
	}

	var newVal *lisp.LVal
	if in.Type == lisp.LArray {
		newVal = toVector(results)
	} else {
		newVal = toList(results)
	}
	return newVal, nil
}

func (s *iterPath) NilMutate(in *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	for _, item := range horizon {
		_, err := s.path.NilMutate(item)
		if err != nil {
			// IMPORTANT: when iterating we ignore paths where nil fails,
			// and do not nil the item. This is similar, but not the same as `jq`
			// semantics which will return an error in some cases.
			continue
		}
	}

	return in, nil
}

func (s *iterPath) Nil(in *lisp.LVal) (*lisp.LVal, error) {
	horizon, err := toCells(in)
	if err != nil {
		return nil, err
	}
	var results []*lisp.LVal
	for _, item := range horizon {
		in, err := s.path.Nil(item)
		if err != nil {
			// IMPORTANT: when iterating we ignore paths where nil fails,
			// and return orig. item. This is similar, but not the same as `jq`
			// semantics which will return an error in some cases.
			in, err = copyLVal(item)
			if err != nil {
				return nil, err
			}
		}
		results = append(results, in)
	}

	var newVal *lisp.LVal
	if in.Type == lisp.LArray {
		newVal = toVector(results)
	} else {
		newVal = toList(results)
	}
	return newVal, nil
}

func (s *iterPath) String() string {
	var sb strings.Builder
	s.appendString(&sb)
	return sb.String()
}

func (s *iterPath) appendString(sb *strings.Builder) {
	sb.WriteString("[]")
	appendPathString(sb, s.path)
}
