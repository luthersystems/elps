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
	return copyGuarded(v, lisp.CycleGuard{})
}

// copyGuarded is copyLVal continuing a walk already in progress rather than
// starting a fresh one. Every nested copy must pass g down; a fresh walk per
// level resets the bound on every lap and it never fires.
func copyGuarded(v *lisp.LVal, g lisp.CycleGuard) (*lisp.LVal, error) {
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
	g, cyclic := g.Descend(v)
	if cyclic {
		return nil, errCyclicValue
	}
	out, err := copyContainer(v, g)
	if g.Tracking() {
		g.Ascend(v)
	}
	return out, err
}

// copyContainer copies the container types. It is only ever called through
// copyGuarded, which has already established that v is one and put it on g's
// path.
func copyContainer(v *lisp.LVal, g lisp.CycleGuard) (*lisp.LVal, error) {
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
	return copyMapGuarded(v, lisp.CycleGuard{})
}

// copyMapGuarded is copyMap continuing a walk already in progress.
func copyMapGuarded(v *lisp.LVal, g lisp.CycleGuard) (*lisp.LVal, error) {
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
	return copyMapExcept(v, key, lisp.CycleGuard{})
}

// copyMapExcept is the shared body: skip is the key to leave out, or nil to
// copy every entry.
func copyMapExcept(v *lisp.LVal, skip *lisp.LVal, g lisp.CycleGuard) (*lisp.LVal, error) {
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
	return copyVectorGuarded(v, lisp.CycleGuard{})
}

// copyVectorGuarded is copyVector continuing a walk already in progress.
func copyVectorGuarded(v *lisp.LVal, g lisp.CycleGuard) (*lisp.LVal, error) {
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
	return copyListGuarded(v, lisp.CycleGuard{})
}

// copyListGuarded is copyList continuing a walk already in progress.
func copyListGuarded(v *lisp.LVal, g lisp.CycleGuard) (*lisp.LVal, error) {
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
	var g lisp.CycleGuard
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
	if src.Quoted && !cp.Quoted {
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
		in.Cells[1].Cells = vals
		dims := in.Cells[0]
		// dims bookkeeping for the array rework above
		dims.Cells[0].Int = len(vals)
		return
	}
	// IMPORTANT: this writes list cell storage, and is reachable only from
	// the non-mutating Set/Delete/Nil, which pass a freshly constructed
	// private copy, never a caller-owned or program-literal list.
	in.Cells = vals
}

// toVector converts a slice of LVal cells into an elps vector.
func toVector(cells []*lisp.LVal) *lisp.LVal {
	return lisp.Array(nil, cells)
}

// toList converts a slice of LVal cells into an elps list.
func toList(cells []*lisp.LVal) *lisp.LVal {
	return lisp.SExpr(cells)
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
	// IMPORTANT: root always starts with "."!
	return "." + s.path.String()
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
func normalizePaths(paths ...Path) []Path {
	paths = expandPaths(paths...)
	var curChain []Path
	for i := len(paths) - 1; i >= 0; i-- {
		path := paths[i]
		switch path.(type) {
		case *iterPath:
			// NOTE: there can be mutual recursion here between Iter/Chain.
			curChain = []Path{Iter(curChain...)}
		default:
			curChain = append([]Path{path}, curChain...)
		}
	}
	return curChain
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
	for _, path := range s.paths {
		sb.WriteString(path.String())
	}
	return sb.String()
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
	index, ok := resolveIndex(len(cells), s.index)
	if !ok {
		return lisp.Nil(), nil
	}
	vals := append(cells[:index], cells[index+1:]...)
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

type rangePath struct {
	from       int
	to         int
	implicitTo bool
}

// Range performs array slice operations (e.g., [a:b]) .
func Range(from int, to int, implicitTo bool) Path {
	return &rangePath{from: from, to: to, implicitTo: implicitTo}
}

// Get returns a sequence over the input's OWN backing array.
//
// IMPORTANT: this is a two-index slice, so the result keeps the source's
// spare capacity, and a later (append 'vector ...) into that capacity writes
// through to the source — including to a quoted program literal a parse
// cache shares between environments. That is the repo-wide aliasing class
// tracked in issues #369 and #373, not something this package opens: the
// kernel's own (slice 'vector ...) hands back exactly the same thing, and
// the identical corruption reproduces with no path operation involved at
// all. Clamping the capacity here would close #369 for one producer and
// leave every other one open. When #369 is settled — a three-index slice, a
// copy, or a documented rule — this call site is one the settlement applies
// to.
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
	cells = cells[from:to]
	var newVal *lisp.LVal
	if in.Type == lisp.LArray {
		newVal = toVector(cells)
	} else {
		newVal = toList(cells)
	}
	return newVal, nil
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
	vals := append(cells[:from], setCells...)
	if to < n {
		vals = append(vals, cells[to:]...)
	}
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
	vals := cells[:from]
	if to < n {
		vals = append(vals, cells[to:]...)
	}
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

func (s *rangePath) String() string {
	return "[" + strconv.Itoa(s.from) + ":" + strconv.Itoa(s.to) + "]"
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
	return "[]" + s.path.String()
}
