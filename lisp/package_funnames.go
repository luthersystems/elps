// Copyright © 2026 The ELPS authors

package lisp

import (
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
	if mv := lazy.inst.values[ref.index-1]; mv != nil {
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
	for i := range names.len() {
		name := names.at(i)
		id := pkg.bindingIdentity(name)
		if id.ok && id.funType == LFunNone && id.data != nil && id.data.fid == fid && id.data.pkg == pkg.Name {
			return name, true, i + 1
		}
	}
	return "", false, names.len()
}

// namesOfFID returns, in sorted order, the names under which pkg binds a
// function with the FID fid.  It allocates nothing.
func (pkg *Package) namesOfFID(fid string) fidNameView {
	if pkg.base != nil {
		return fidIndex{over: pkg.slotFidNames, base: pkg.base.fidNames}.view(fid)
	}
	return pkg.fidIndex().view(fid)
}

// fidNameView is one FID's name list in an index: names from the overlay,
// or the pairs lo to hi of base.
type fidNameView struct {
	names  []string
	base   packagetable.Lists
	lo, hi int
}

func (v fidNameView) len() int {
	if v.names != nil {
		return len(v.names)
	}
	return v.hi - v.lo
}

func (v fidNameView) at(i int) string {
	if v.names != nil {
		return v.names[i]
	}
	return v.base.ValueAt(v.lo + i)
}

// fidIndex is a package's FID index: for each FID, the sorted names under
// which the package binds a function with that FID.  over holds the lists
// written since base was taken; an entry there, even an empty one, replaces
// base's list.  A list is never written in place, so an index may share a
// list with another.
type fidIndex struct {
	over map[string][]string
	base packagetable.Lists
}

// view returns the list of fid.
func (idx fidIndex) view(fid string) fidNameView {
	if names, ok := idx.over[fid]; ok {
		return fidNameView{names: names[:len(names):len(names)]}
	}
	lo, hi := idx.base.Span(fid)
	return fidNameView{base: idx.base, lo: lo, hi: hi}
}

// list returns the list of fid.  The caller may keep it but never writes
// it in place.
func (idx *fidIndex) list(fid string) []string {
	if names, ok := idx.over[fid]; ok {
		return names
	}
	return slices.Collect(idx.base.List(fid))
}

// set stores the list of fid.  An empty list stays as an entry while base
// has a list for fid, so it hides that list.
func (idx *fidIndex) set(fid string, names []string) {
	if len(names) == 0 {
		names = nil
		if idx.base.ListLen(fid) == 0 {
			delete(idx.over, fid)
			return
		}
	}
	if idx.over == nil {
		idx.over = make(map[string][]string, 1)
	}
	idx.over[fid] = names
}

// pairs returns the index as fresh (FID, name) pairs, one allocation.
func (idx *fidIndex) pairs() []packagetable.Pair {
	n := 0
	for p := range idx.base.All() {
		if _, ok := idx.over[p.Key]; !ok {
			n++
		}
	}
	for _, names := range idx.over {
		n += len(names)
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
	for fid, names := range idx.over {
		for _, name := range names {
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
// bindings on first use.  The build reads each binding once without
// materializing it, and the result is stored atomically, so concurrent
// readers of a package no goroutine is writing stay safe (issue #397): a
// reader that loses the race uses the winner's index.
func (pkg *Package) fidIndex() *fidIndex {
	if idx := pkg.loadFIDs(); idx != nil {
		return idx
	}
	over := make(map[string][]string)
	for name, v := range pkg.symbols {
		if fid, isFun := pkg.symbolFID(name, v); isFun {
			over[fid] = append(over[fid], name)
		}
	}
	for _, names := range over {
		slices.Sort(names)
	}
	idx := &fidIndex{over: over}
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

// setSlotFIDNames stores the name list of fid in this VM's overlay.  An
// empty list is kept as an entry, so it hides the base's list.
func (pkg *Package) setSlotFIDNames(fid string, names []string) {
	if pkg.slotFidNames == nil {
		pkg.slotFidNames = make(map[string][]string, 1)
	}
	if len(names) == 0 {
		names = nil
	}
	pkg.slotFidNames[fid] = names
}

// fidNameLists returns pkg's FID index as a table for template
// publication.  It builds the table in one allocation and does not keep
// an index on pkg, so publication adds no allocation per function and no
// index upkeep to the source package's later writes.
func (pkg *Package) fidNameLists() packagetable.Lists {
	if pkg.base != nil {
		return packagetable.AdoptPairs(pkg.frozenFIDIndex().pairs())
	}
	if idx := pkg.loadFIDs(); idx != nil {
		return packagetable.AdoptPairs(idx.pairs())
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

// insertSortedName returns names with name inserted in sorted order.  It
// returns names itself when it already holds name, and otherwise a new
// list: a list is never written in place.
func insertSortedName(names []string, name string) []string {
	i, found := slices.BinarySearch(names, name)
	if found {
		return names
	}
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
	if mv := lazy.inst.values[ref.index-1]; mv != nil {
		return sameFun(mv, fd)
	}
	tv := &lazy.inst.p.values[ref.index-1]
	if tv.header.Type != LFun || tv.kind != templateFunction {
		return false
	}
	return lazy.inst.functions[tv.payload] == fd
}
