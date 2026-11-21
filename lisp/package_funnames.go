// Copyright © 2026 The ELPS authors

package lisp

import (
	"maps"
	"slices"
	"sync/atomic"
	"unsafe"

	"github.com/luthersystems/elps/internal/packagetable"
)

// NumBindings returns the number of names bound in pkg, without reading or
// materializing any binding.
func (pkg *Package) NumBindings() int {
	if pkg.base != nil {
		return pkg.base.index.Len()
	}
	return len(pkg.symbols)
}

// FunNamesByFID returns the canonical global name of every regular function
// pkg defines and binds: for each FID, the first name in sorted order under
// which pkg binds a function whose Package is pkg.Name and whose FunType is
// LFunNone.  It also returns the number of bindings it read, so a caller can
// charge for the work.
//
// It reads a binding a lazy template has not materialized from the
// template's plan, so it builds no value.  A rebinding (set, putSlot, a
// thawed table) is read as the binding it now holds.  An FID is unique
// within its package, so the FID names one function of pkg.
func (pkg *Package) FunNamesByFID() (map[string]string, int) {
	out := make(map[string]string)
	read := 0
	// The first name in sorted order is the minimum, which needs no sort:
	// the bindings are read in any order.
	visit := func(name string, identity functionIdentity) {
		read++
		if !identity.ok || identity.funType != LFunNone {
			return
		}
		fid, fpkg := "", ""
		if identity.data != nil {
			fid, fpkg = identity.data.fid, identity.data.pkg
		}
		if fpkg != pkg.Name {
			return
		}
		if prev, seen := out[fid]; !seen || name < prev {
			out[fid] = name
		}
	}
	if pkg.base != nil {
		for name, i := range pkg.base.index.Unordered() {
			v := pkg.slotValue(i)
			if v == nil && pkg.lazy.inst != nil {
				visit(name, pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value))
				continue
			}
			visit(name, funIdentity(v))
		}
		return out, read
	}
	for name, v := range pkg.symbols {
		if v == lazyPending {
			i, _ := pkg.lazy.base.index.Lookup(name)
			visit(name, pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value))
			continue
		}
		visit(name, funIdentity(v))
	}
	return out, read
}

// functionIdentity holds a function identity and its lookup status.
type functionIdentity struct {
	// data contains the function identifier and package name.
	data *funData
	// funType is the function kind.
	funType LFunType
	// ok reports a valid function identity.
	ok bool
}

// funIdentity reports the FID, package and function type of a function
// value; ok is false for anything else.
func funIdentity(v *LVal) functionIdentity {
	if v == nil || v.Type != LFun {
		return functionIdentity{}
	}
	return functionIdentity{data: v.funData(), funType: v.FunType, ok: true}
}

// peekFun reports the function identity of a binding the lazy plan has not
// materialized, reading the plan instead of building the value.
func (lazy *lazyPackage) peekFun(ref templateRef) functionIdentity {
	if ref.index == 0 {
		return funIdentity(ref.shared)
	}
	if mv := lazy.inst.values.get(ref.index - 1); mv != nil {
		// Materialized through another path; outside ref the fill queue
		// is drained, so the header is complete.
		return funIdentity(mv)
	}
	tv := &lazy.inst.p.values[ref.index-1]
	if tv.header.Type != LFun || tv.kind != templateFunction {
		return functionIdentity{}
	}
	fd := &lazy.inst.p.functions[tv.payload].header
	return functionIdentity{data: fd, funType: tv.header.FunType, ok: true}
}

// FirstNameOf returns the first name, in sorted order, under which pkg
// binds fn itself: a binding whose function data is fn's, so any header
// copy of fn (a FunRef or Copy) matches, and another function with the same
// FID does not.  It returns false when no binding holds fn, or when fn is
// not a function.  It also returns the number of bindings it read, so a
// caller can charge for the work.
//
// It reads only the names under which pkg binds a function with fn's FID
// (luthersystems/elps#847), so the count does not grow with the package.
// Like FunNamesByFID it reads a binding a lazy template has not
// materialized from the template's plan, so it builds no value.
//
//nolint:revive // exported API; changing it breaks embedders
func (pkg *Package) FirstNameOf(fn *LVal) (string, bool, int) {
	if fn == nil || fn.Type != LFun {
		return "", false, 0
	}
	fd, _ := fn.Native.(*funData)
	if fd == nil {
		return "", false, 0
	}
	names := pkg.namesOfFID(fd.fid)
	if names.set != nil {
		m := names.set.firstMatch(func(name string) bool {
			return pkg.bindingIsFun(name, fd)
		})
		return m.name, m.ok, m.read
	}
	// A short list is read with a direct loop, not through firstMatch: a
	// closure call per name made this hot lookup about 15% slower
	// (BenchmarkFirstNameLookup).
	for i := range names.len() {
		if name := names.at(i); pkg.bindingIsFun(name, fd) {
			return name, true, i + 1
		}
	}
	return "", false, names.len()
}

// FunNameByFID returns the first name, in sorted order, under which pkg
// binds a regular function that pkg defines and whose FID is fid: the name
// FunNamesByFID returns for fid, without reading the whole package.  It
// returns false when no such binding exists.  It reads at most the names pkg
// binds to a function with that FID, whatever the size of the package
// (luthersystems/elps#847).
//
// It reads a binding a lazy template has not materialized from the
// template's plan, so it builds no value.
func (pkg *Package) FunNameByFID(fid string) (string, bool) {
	name, ok, _ := pkg.funNameByFID(fid)
	return name, ok
}

// funNameByFID is FunNameByFID that also returns the number of bindings it
// read.
//
//nolint:revive // the read count is for tests; the exported form returns two results
func (pkg *Package) funNameByFID(fid string) (string, bool, int) {
	names := pkg.namesOfFID(fid)
	if names.set != nil {
		m := names.set.firstMatch(func(name string) bool {
			return pkg.bindingMatchesFID(name, fid)
		})
		return m.name, m.ok, m.read
	}
	// A direct loop, as in FirstNameOf.
	for i := range names.len() {
		if name := names.at(i); pkg.bindingMatchesFID(name, fid) {
			return name, true, i + 1
		}
	}
	return "", false, names.len()
}

// bindingMatchesFID reports whether name binds a regular function defined by
// pkg with the given FID.
func (pkg *Package) bindingMatchesFID(name, fid string) bool {
	id := pkg.bindingIdentity(name)
	return id.ok && id.funType == LFunNone && id.data != nil && id.data.fid == fid && id.data.pkg == pkg.Name
}

// namesOfFID returns a view of the names under which pkg binds a function
// with the FID fid.  The view reads them in sorted order (firstMatch).  It
// allocates nothing.
func (pkg *Package) namesOfFID(fid string) fidNameView {
	if pkg.base != nil {
		return fidIndex{over: pkg.slotFidNames, base: pkg.base.fidNames}.view(fid)
	}
	return pkg.settledFIDs().view(fid)
}

// settledFIDs returns an unfrozen package's index, live.  The first lookup
// of an index that is not live applies its pending writes to a private copy
// and stores the copy, live, atomically in place of the index, so a reader
// never writes an index another reader can see (luthersystems/elps#397).
// A reader that loses the race uses the winner's copy, which holds the same
// names.  The copy is made once per index: a live index takes its writes at
// once.
func (pkg *Package) settledFIDs() *fidIndex {
	idx := pkg.fidIndex()
	if idx.live {
		return idx
	}
	settled := &fidIndex{base: idx.base, over: cloneFIDOverlay(idx.over), pending: slices.Clone(idx.pending)}
	settled.apply()
	settled.pending = nil
	settled.live = true
	if atomic.CompareAndSwapPointer(pkg.fidsPointer(), unsafe.Pointer(idx), unsafe.Pointer(settled)) { //nolint:gosec // settled is a *fidIndex, the type of pkg.fids
		return settled
	}
	return pkg.loadFIDs()
}

// cloneFIDOverlay returns a copy of over with private copies of any mutable
// name sets, so settledFIDs never mutates a set another concurrent reader
// can still read.
func cloneFIDOverlay(over map[string]fidNames) map[string]fidNames {
	if len(over) == 0 {
		return nil
	}
	out := make(map[string]fidNames, len(over))
	for fid, entry := range over {
		if entry.set != nil {
			entry.set = entry.set.clone()
		}
		out[fid] = entry
	}
	return out
}

// fidNameView is one FID's names in an index: a large set from the overlay,
// a small sorted slice from the overlay, or the sorted span lo..hi of base.
type fidNameView struct {
	set    *fidNameSet
	names  []string
	base   packagetable.Lists
	lo, hi int
}

// len returns the number of names in v.
func (v fidNameView) len() int {
	if v.set != nil {
		return len(v.set.heap)
	}
	if v.names != nil {
		return len(v.names)
	}
	return v.hi - v.lo
}

// at returns the name at sorted position i of a slice or base view.  A set
// view keeps its names in heap order, so at panics for it: use firstMatch.
func (v fidNameView) at(i int) string {
	if v.set != nil {
		panic("lisp: fidNameView.at called on a set view; use firstMatch")
	}
	if v.names != nil {
		return v.names[i]
	}
	return v.base.ValueAt(v.lo + i)
}

// fidMatch is the result of a firstMatch lookup.
type fidMatch struct {
	// name is the first name, in sorted order, that matches.
	name string
	// read is the number of names the lookup read: the 1-based rank of
	// name, or the number of names when none matches.
	read int
	// ok reports that a name matches.
	ok bool
}

// firstMatch returns the first name of v, in sorted order, for which match
// is true, and the number of names it read.
func (v fidNameView) firstMatch(match func(string) bool) fidMatch {
	if v.set != nil {
		return v.set.firstMatch(match)
	}
	n := v.len()
	for i := range n {
		if name := v.at(i); match(name) {
			return fidMatch{name: name, ok: true, read: i + 1}
		}
	}
	return fidMatch{read: n}
}

// fidSetThreshold is the largest sorted slice one FID keeps before switching
// to a fidNameSet.  Keeping small FIDs in a short slice avoids per-FID map
// overhead while stopping O(n^2) copying when many bindings share one FID
// (luthersystems/elps#1010).
const fidSetThreshold = 16

// fidNames holds one FID's names in an overlay map.  At most fidSetThreshold
// names live in list (a sorted, never-mutated slice); larger FIDs promote to
// set.
type fidNames struct {
	set  *fidNameSet
	list []string
}

func (n fidNames) len() int {
	if n.set != nil {
		return len(n.set.heap)
	}
	return len(n.list)
}

// fidNameSet stores a FID's names as an indexed binary min-heap once the
// binding count passes fidSetThreshold.  pos maps each name to its heap
// slot, so membership, insertion and deletion run in O(log n) time and
// heap[0] always holds the smallest name for O(1) pure reads.
//
// A FID that has a set keeps it when its names drop back to
// fidSetThreshold or fewer.  The set goes only when its last name goes.
// So a set's memory is bounded by the peak number of names of its FID.
type fidNameSet struct {
	pos  map[string]int
	heap []string
}

// adoptSortedFIDNameSet builds a set from a strictly increasing, caller-owned
// slice.  A sorted slice already satisfies the min-heap invariant.
func adoptSortedFIDNameSet(sorted []string) *fidNameSet {
	pos := make(map[string]int, len(sorted))
	for i, name := range sorted {
		pos[name] = i
	}
	return &fidNameSet{pos: pos, heap: sorted}
}

// newFIDNameSetFromSorted copies a strictly increasing slice into a new set
// with room for at least capacity names.
func newFIDNameSetFromSorted(sorted []string, capacity int) *fidNameSet {
	heap := make([]string, len(sorted), max(len(sorted), capacity))
	copy(heap, sorted)
	return adoptSortedFIDNameSet(heap)
}

// newFIDNameSetFromBase builds a set from the sorted span [lo, hi) of base.
func newFIDNameSetFromBase(base packagetable.Lists, lo, hi, extraCap int) *fidNameSet {
	n := hi - lo
	heap := make([]string, n, n+extraCap)
	pos := make(map[string]int, n+extraCap)
	for i := range n {
		name := base.ValueAt(lo + i)
		heap[i] = name
		pos[name] = i
	}
	return &fidNameSet{pos: pos, heap: heap}
}

func (s *fidNameSet) clone() *fidNameSet {
	return &fidNameSet{
		pos:  maps.Clone(s.pos),
		heap: slices.Clone(s.heap),
	}
}

func (s *fidNameSet) add(name string) {
	if _, ok := s.pos[name]; ok {
		return
	}
	i := len(s.heap)
	s.heap = append(s.heap, name)
	s.pos[name] = i
	s.siftUp(i)
}

func (s *fidNameSet) remove(name string) {
	i, ok := s.pos[name]
	if !ok {
		return
	}
	delete(s.pos, name)
	last := len(s.heap) - 1
	if i == last {
		s.heap[last] = ""
		s.heap = s.heap[:last]
		return
	}
	moved := s.heap[last]
	s.heap[last] = ""
	s.heap = s.heap[:last]
	s.heap[i] = moved
	s.pos[moved] = i
	if !s.siftDown(i) {
		s.siftUp(i)
	}
}

func (s *fidNameSet) siftUp(i int) {
	for i > 0 {
		p := (i - 1) / 2
		if s.heap[p] <= s.heap[i] {
			break
		}
		s.swap(i, p)
		i = p
	}
}

func (s *fidNameSet) siftDown(i int) bool {
	start := i
	n := len(s.heap)
	for {
		left := 2*i + 1
		if left >= n {
			break
		}
		smallest := left
		if right := left + 1; right < n && s.heap[right] < s.heap[left] {
			smallest = right
		}
		if s.heap[i] <= s.heap[smallest] {
			break
		}
		s.swap(i, smallest)
		i = smallest
	}
	return i > start
}

func (s *fidNameSet) swap(i, j int) {
	s.heap[i], s.heap[j] = s.heap[j], s.heap[i]
	s.pos[s.heap[i]] = i
	s.pos[s.heap[j]] = j
}

// firstMatch returns the first name of s, in sorted order, for which match
// is true, and the number of names it read, as fidNameView.firstMatch does.
// It reads heap[0], the smallest name, before it starts a walk.
func (s *fidNameSet) firstMatch(match func(string) bool) fidMatch {
	n := len(s.heap)
	if n == 0 {
		return fidMatch{}
	}
	if match(s.heap[0]) {
		return fidMatch{name: s.heap[0], ok: true, read: 1}
	}
	if n == 1 {
		return fidMatch{read: 1}
	}
	return s.walkMatch(match)
}

// walkMatch is firstMatch after heap[0] does not match.  It walks the heap
// in sorted order without changing it.
//
// frontier holds the heap indexes whose parents the walk has read, as a
// min-heap by name, so its top is the next name in sorted order.  A match
// at rank r costs O(r log r) time, and frontier holds at most r+1 indexes.
// frontier lives in this call, because a published or settled index is
// read by many goroutines at once and a reader must not write it
// (luthersystems/elps#397).  It starts in the stack array buf, so a match
// within the first len(buf) ranks allocates nothing.
func (s *fidNameSet) walkMatch(match func(string) bool) fidMatch {
	n := len(s.heap)
	var buf [128]int
	frontier := append(buf[:0], 1)
	if n > 2 {
		frontier = s.pushFrontier(frontier, 2)
	}
	for read := 2; ; read++ {
		i := frontier[0]
		if name := s.heap[i]; match(name) {
			return fidMatch{name: name, ok: true, read: read}
		}
		if read == n {
			return fidMatch{read: n}
		}
		// i leaves the frontier and its children join it.  The left child
		// takes i's place at the top, which saves one sift.
		left := 2*i + 1
		if left >= n {
			frontier = s.popFrontier(frontier)
			continue
		}
		frontier[0] = left
		s.siftFrontier(frontier)
		if right := left + 1; right < n {
			frontier = s.pushFrontier(frontier, right)
		}
	}
}

// pushFrontier adds heap index i to frontier, a min-heap of heap indexes
// ordered by name.
func (s *fidNameSet) pushFrontier(frontier []int, i int) []int {
	frontier = append(frontier, i)
	j := len(frontier) - 1
	for j > 0 {
		p := (j - 1) / 2
		if s.heap[frontier[p]] <= s.heap[frontier[j]] {
			break
		}
		frontier[p], frontier[j] = frontier[j], frontier[p]
		j = p
	}
	return frontier
}

// popFrontier removes the top of frontier, a min-heap of heap indexes
// ordered by name.
func (s *fidNameSet) popFrontier(frontier []int) []int {
	last := len(frontier) - 1
	frontier[0] = frontier[last]
	frontier = frontier[:last]
	s.siftFrontier(frontier)
	return frontier
}

// siftFrontier moves the top of frontier down until frontier is a min-heap
// of heap indexes ordered by name again.
func (s *fidNameSet) siftFrontier(frontier []int) {
	n := len(frontier)
	j := 0
	for {
		left := 2*j + 1
		if left >= n {
			return
		}
		smallest := left
		if right := left + 1; right < n && s.heap[frontier[right]] < s.heap[frontier[left]] {
			smallest = right
		}
		if s.heap[frontier[j]] <= s.heap[frontier[smallest]] {
			return
		}
		frontier[j], frontier[smallest] = frontier[smallest], frontier[j]
		j = smallest
	}
}

// fidIndex is a package's FID index: for each FID, the names under which
// the package binds a function with that FID.  over holds entries written
// since base was taken; an entry there, even an empty one, replaces base's
// list.  Small slices in over are never written in place; large sets are
// owned by their index and cloned in settledFIDs before mutation.
type fidIndex struct {
	over map[string]fidNames
	base packagetable.Lists
	// spare backs the one-name lists insert makes, so a run of writes
	// that each bind a new function (a use-package import loop binds one
	// per export) allocates once per spare block, not once per name
	// (luthersystems/elps#855).
	// Each list it backs has its capacity cut to its length, so no append
	// reaches a neighbour, and no list is written in place.
	spare []string
	// pending holds the writes not yet applied to over, in order, while
	// the index is not live.  A thawed package's index and an eager
	// template VM's start not live: a write appends to pending, and the
	// first lookup applies the log to a copy and stores it live
	// (settledFIDs).  So a VM that never looks a function up, such as one
	// that only runs a use-package import loop into a thawed package, pays
	// one append per write, not a base search and a map write
	// (luthersystems/elps#855).  A writer applies the log itself once it
	// holds fidPendingMax writes.
	pending []fidWrite
	// live reports that a lookup has used the index.  A live index takes
	// each write at once, so a write followed by a lookup never copies the
	// index.
	live bool
}

// fidWrite is one pending change to a FID index: add or remove name from
// the list of fid.
type fidWrite struct {
	fid, name string
	add       bool
}

// fidPendingMax is the most writes an index holds before the writer
// applies them, so a VM that rebinds functions in a loop and never looks
// one up keeps a bounded log.
const fidPendingMax = 1024

// record takes a write: at once on a live index, and otherwise in the
// pending log, which it applies when it is full.  Only a writer calls it.
func (idx *fidIndex) record(w fidWrite) {
	if idx.live {
		idx.write(w)
		return
	}
	idx.pending = append(idx.pending, w)
	if len(idx.pending) >= fidPendingMax {
		idx.apply()
	}
}

// write applies one write to idx in place.
func (idx *fidIndex) write(w fidWrite) {
	if entry, ok := idx.over[w.fid]; ok {
		if entry.set != nil {
			if w.add {
				entry.set.add(w.name)
			} else {
				entry.set.remove(w.name)
				if len(entry.set.heap) == 0 {
					idx.set(w.fid, fidNames{})
				}
			}
			return
		}
		if w.add {
			if len(entry.list) == 0 {
				idx.set(w.fid, fidNames{list: idx.insert(nil, w.name)})
				return
			}
			i, found := slices.BinarySearch(entry.list, w.name)
			if found {
				return
			}
			if len(entry.list) >= fidSetThreshold {
				s := newFIDNameSetFromSorted(entry.list, len(entry.list)+1)
				s.add(w.name)
				idx.set(w.fid, fidNames{set: s})
				return
			}
			idx.set(w.fid, fidNames{list: insertSortedNameAt(entry.list, i, w.name)})
		} else {
			next := removeSortedName(entry.list, w.name)
			if len(next) == len(entry.list) {
				return
			}
			idx.set(w.fid, fidNames{list: next})
		}
		return
	}

	lo, hi := idx.base.Span(w.fid)
	n := hi - lo
	if n == 0 {
		if !w.add {
			return
		}
		idx.set(w.fid, fidNames{list: idx.insert(nil, w.name)})
		return
	}
	pos, found := baseSearchName(idx.base, lo, hi, w.name)
	if w.add == found {
		return
	}
	if n > fidSetThreshold || (w.add && n == fidSetThreshold) {
		extra := 0
		if w.add {
			extra = 1
		}
		s := newFIDNameSetFromBase(idx.base, lo, hi, extra)
		if w.add {
			s.add(w.name)
		} else {
			s.remove(w.name)
		}
		idx.set(w.fid, fidNames{set: s})
		return
	}
	if w.add {
		names := make([]string, 0, n+1)
		for j := range pos {
			names = append(names, idx.base.ValueAt(lo+j))
		}
		names = append(names, w.name)
		for j := pos; j < n; j++ {
			names = append(names, idx.base.ValueAt(lo+j))
		}
		idx.set(w.fid, fidNames{list: names})
		return
	}
	if n == 1 {
		idx.set(w.fid, fidNames{})
		return
	}
	names := make([]string, 0, n-1)
	for j := range pos {
		names = append(names, idx.base.ValueAt(lo+j))
	}
	for j := pos + 1; j < n; j++ {
		names = append(names, idx.base.ValueAt(lo+j))
	}
	idx.set(w.fid, fidNames{list: names})
}

// baseSearchName searches base's sorted span [lo, hi) for name and returns
// its offset from lo (or insertion offset) and whether it is present.
func baseSearchName(base packagetable.Lists, lo, hi int, name string) (int, bool) {
	start := lo
	for lo < hi {
		mid := lo + (hi-lo)/2
		v := base.ValueAt(mid)
		if v < name {
			lo = mid + 1
		} else if v > name {
			hi = mid
		} else {
			return mid - start, true
		}
	}
	return lo - start, false
}

// apply applies the pending writes to idx in place.  Only a writer, or a
// reader on an index no other goroutine can see, calls it.
func (idx *fidIndex) apply() {
	for _, w := range idx.pending {
		idx.write(w)
	}
	clear(idx.pending)
	idx.pending = idx.pending[:0]
}

// view returns a read-only view of fid's names.
func (idx fidIndex) view(fid string) fidNameView {
	if entry, ok := idx.over[fid]; ok {
		if entry.set != nil {
			return fidNameView{set: entry.set}
		}
		return fidNameView{names: entry.list[:len(entry.list):len(entry.list)]}
	}
	lo, hi := idx.base.Span(fid)
	return fidNameView{base: idx.base, lo: lo, hi: hi}
}

// set stores the entry for fid.  An empty entry stays in over while base has
// a list for fid, so it hides that list.
func (idx *fidIndex) set(fid string, entry fidNames) {
	if entry.len() == 0 {
		entry = fidNames{}
		if idx.base.ListLen(fid) == 0 {
			delete(idx.over, fid)
			return
		}
	}
	if idx.over == nil {
		idx.over = make(map[string]fidNames, 1)
	}
	idx.over[fid] = entry
}

// pairs returns the index as fresh (FID, name) pairs, one allocation.
func (idx *fidIndex) pairs() []packagetable.Pair {
	n := 0
	for p := range idx.base.All() {
		if _, ok := idx.over[p.Key]; !ok {
			n++
		}
	}
	for _, entry := range idx.over {
		n += entry.len()
	}
	if n == 0 {
		return nil
	}
	out := make([]packagetable.Pair, 0, n)
	for p := range idx.base.All() {
		if _, ok := idx.over[p.Key]; !ok {
			out = append(out, p)
		}
	}
	for fid, entry := range idx.over {
		if entry.set != nil {
			for _, name := range entry.set.heap {
				out = append(out, packagetable.Pair{Key: fid, Value: name})
			}
			continue
		}
		for _, name := range entry.list {
			out = append(out, packagetable.Pair{Key: fid, Value: name})
		}
	}
	return out
}

// frozenFIDIndex returns a frozen package's index: the shared base's lists
// under this VM's overlay.  A write through it goes to the overlay only
// through setSlotFIDNames.
func (pkg *Package) frozenFIDIndex() *fidIndex {
	return &fidIndex{base: pkg.base.fidNames, over: pkg.slotFidNames}
}

// fidIndex returns an unfrozen package's index, building it from the
// bindings on first use, live.  The build reads each binding once without
// materializing it, and the result is stored atomically, so concurrent
// readers of a package no goroutine is writing stay safe (issue #397): a
// reader that loses the race uses the winner's index.
func (pkg *Package) fidIndex() *fidIndex {
	if idx := pkg.loadFIDs(); idx != nil {
		return idx
	}
	over := make(map[string]fidNames)
	for name, v := range pkg.symbols {
		if fid, isFun := pkg.symbolFID(name, v); isFun {
			entry := over[fid]
			entry.list = append(entry.list, name)
			over[fid] = entry
		}
	}
	for fid, entry := range over {
		if len(entry.list) <= 1 {
			continue
		}
		slices.Sort(entry.list)
		if len(entry.list) > fidSetThreshold {
			over[fid] = fidNames{set: adoptSortedFIDNameSet(entry.list)}
		} else {
			over[fid] = fidNames{list: entry.list[:len(entry.list):len(entry.list)]}
		}
	}
	idx := &fidIndex{over: over, live: true}
	if atomic.CompareAndSwapPointer(pkg.fidsPointer(), nil, unsafe.Pointer(idx)) { //nolint:gosec // idx is a *fidIndex, the type of pkg.fids
		return idx
	}
	return pkg.loadFIDs()
}

// loadFIDs reads pkg.fids atomically, for a read path that may run beside
// another reader's build of the index.
func (pkg *Package) loadFIDs() *fidIndex {
	return (*fidIndex)(atomic.LoadPointer(pkg.fidsPointer()))
}

func (pkg *Package) fidsPointer() *unsafe.Pointer {
	return (*unsafe.Pointer)(unsafe.Pointer(&pkg.fids)) //nolint:gosec // fids is a *fidIndex; the cast only lets sync/atomic read and write it
}

// bindingIdentity reports the function identity of the binding of name,
// reading a binding a lazy plan has not materialized from the plan.
func (pkg *Package) bindingIdentity(name string) functionIdentity {
	if pkg.base != nil {
		i, ok := pkg.base.index.Lookup(name)
		if !ok {
			return functionIdentity{}
		}
		return pkg.slotIdentity(i)
	}
	v, ok := pkg.symbols[name]
	if !ok {
		return functionIdentity{}
	}
	return pkg.symbolIdentity(name, v)
}

// bindingIsFun reports whether name is bound to the function whose data is
// fd, reading a binding a lazy plan has not materialized from the plan.
func (pkg *Package) bindingIsFun(name string, fd *funData) bool {
	if pkg.base != nil {
		i, ok := pkg.base.index.Lookup(name)
		if !ok {
			return false
		}
		if v := pkg.slotValue(i); v != nil || pkg.lazy.inst == nil {
			return sameFun(v, fd)
		}
		return pkg.lazy.peekSameFun(pkg.lazy.base.bindings[i].value, fd)
	}
	v := pkg.symbols[name]
	if v == lazyPending {
		i, _ := pkg.lazy.base.index.Lookup(name)
		return pkg.lazy.peekSameFun(pkg.lazy.base.bindings[i].value, fd)
	}
	return sameFun(v, fd)
}

// slotIdentity reports the function identity of base slot i without
// materializing it.
func (pkg *Package) slotIdentity(i int) functionIdentity {
	if v := pkg.slotValue(i); v != nil || pkg.lazy.inst == nil {
		return funIdentity(v)
	}
	return pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value)
}

// symbolIdentity reports the function identity of v, the private table's
// binding of name, without materializing a pending binding.
func (pkg *Package) symbolIdentity(name string, v *LVal) functionIdentity {
	if v == lazyPending {
		i, _ := pkg.lazy.base.index.Lookup(name)
		return pkg.lazy.peekFun(pkg.lazy.base.bindings[i].value)
	}
	return funIdentity(v)
}

// identityFID returns the FID of a function identity, and false when the
// identity is not a function's.
func identityFID(id functionIdentity) (string, bool) {
	if !id.ok {
		return "", false
	}
	if id.data == nil {
		return "", true
	}
	return id.data.fid, true
}

// symbolFID returns the FID of the private table's binding v of name, and
// false when v is not a function.
func (pkg *Package) symbolFID(name string, v *LVal) (string, bool) {
	return identityFID(pkg.symbolIdentity(name, v))
}

// slotFID returns the FID of base slot i, and false when it is not a
// function.
func (pkg *Package) slotFID(i int) (string, bool) {
	return identityFID(pkg.slotIdentity(i))
}

// setSlotFIDNames applies one FID index write to this VM's overlay of a
// frozen package.  An empty entry stays in the map while base has a list for
// fid, so it hides the base list.
func (pkg *Package) setSlotFIDNames(w fidWrite) {
	idx := fidIndex{base: pkg.base.fidNames, over: pkg.slotFidNames}
	idx.write(w)
	pkg.slotFidNames = idx.over
}

// fidNameLists returns pkg's FID index as a table for template
// publication.  It builds the table in one allocation and does not keep
// an index on pkg, so publication adds no allocation per function and no
// index upkeep to the source package's later writes.
func (pkg *Package) fidNameLists() packagetable.Lists {
	if pkg.base != nil {
		return packagetable.AdoptPairs(pkg.frozenFIDIndex().pairs())
	}
	if pkg.loadFIDs() != nil {
		return packagetable.AdoptPairs(pkg.settledFIDs().pairs())
	}
	n := 0
	for name, v := range pkg.symbols {
		if _, isFun := pkg.symbolFID(name, v); isFun {
			n++
		}
	}
	if n == 0 {
		return packagetable.Lists{}
	}
	out := make([]packagetable.Pair, 0, n)
	for name, v := range pkg.symbols {
		if fid, isFun := pkg.symbolFID(name, v); isFun {
			out = append(out, packagetable.Pair{Key: fid, Value: name})
		}
	}
	return packagetable.AdoptPairs(out)
}

// insert returns names with name inserted in sorted order, as
// insertSortedName does, taking a one-name list from idx.spare.
func (idx *fidIndex) insert(names []string, name string) []string {
	if len(names) != 0 {
		return insertSortedName(names, name)
	}
	if len(idx.spare) == cap(idx.spare) {
		// Blocks double from 2 up to fidSpareBlock, so an index that
		// takes only a few first names keeps a small block.
		idx.spare = make([]string, 0, min(max(2*cap(idx.spare), 2), fidSpareBlock))
	}
	idx.spare = append(idx.spare, name)
	n := len(idx.spare)
	return idx.spare[n-1 : n : n]
}

// fidSpareBlock is the largest number of one-name lists one spare block
// backs.
const fidSpareBlock = 32

// insertSortedName returns names with name inserted in sorted order.  It
// returns names itself when it already holds name, and otherwise a new
// list: a list is never written in place.
func insertSortedName(names []string, name string) []string {
	i, found := slices.BinarySearch(names, name)
	if found {
		return names
	}
	return insertSortedNameAt(names, i, name)
}

// insertSortedNameAt returns a new slice with name inserted at index i.
func insertSortedNameAt(names []string, i int, name string) []string {
	out := make([]string, 0, len(names)+1)
	out = append(out, names[:i]...)
	out = append(out, name)
	return append(out, names[i:]...)
}

// removeSortedName returns names without name.  It returns names itself
// when it does not hold name, and otherwise a new list, nil when empty.
func removeSortedName(names []string, name string) []string {
	i, found := slices.BinarySearch(names, name)
	if !found {
		return names
	}
	if len(names) == 1 {
		return nil
	}
	out := make([]string, 0, len(names)-1)
	out = append(out, names[:i]...)
	return append(out, names[i+1:]...)
}

// sameFun reports whether v is a function whose data is fd.
func sameFun(v *LVal, fd *funData) bool {
	return v != nil && v.Type == LFun && v.Native == fd
}

// peekSameFun reports whether a binding the lazy plan has not materialized
// is the function whose data is fd, reading the plan instead of building
// the value.  A plan function this VM has not materialized cannot be fd,
// which is materialized.
func (lazy *lazyPackage) peekSameFun(ref templateRef, fd *funData) bool {
	if ref.index == 0 {
		return sameFun(ref.shared, fd)
	}
	if mv := lazy.inst.values.get(ref.index - 1); mv != nil {
		return sameFun(mv, fd)
	}
	tv := &lazy.inst.p.values[ref.index-1]
	if tv.header.Type != LFun || tv.kind != templateFunction {
		return false
	}
	return lazy.inst.functions.get(tv.payload) == fd
}
