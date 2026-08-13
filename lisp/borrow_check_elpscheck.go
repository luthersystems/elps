// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"fmt"
	"runtime"
	"sync"
	"unsafe"
)

// Mint-time provenance detection.
//
// The sealed-AST verifier (lisp/seal_check_elpscheck.go) is a
// CORRUPTION-time oracle: it fires once a program has already written a
// shared literal, so it only ever fires for a test that performs the
// write.  #392's own package had tests that performed the read half — an
// elpspath range query over a literal — and none that performed the write
// half, so the corruption oracle had nothing to see.
//
// This detector fires at the READ half.  Every parse registers the extent
// of its constrained (sealed) backing arrays; every raw constructor
// (SExpr, QExpr, Vector, Bytes) checks whether the storage it is being
// handed falls inside a registered extent while the header being minted
// carries no constraint.  That is precisely "an LVal minted over borrowed
// backing did not inherit the source's constraint" — the bug class,
// detected at the moment it is committed, with no mutation required.
//
// The borrow constructors in lisp/borrow.go transfer the constraint before
// the check runs, so they never trip it; a producer that bypasses them
// does.  Prevention is the constructor; this is the backstop that says so
// out loud when someone does not use it.
//
// COST AND SCOPE.  Compiled out untagged (lisp/borrow_check_default.go).
// Under -tags elpscheck it is a mutex plus a page-bucketed map probe,
// which is why it lives behind the tag.  The extents are compared
// as uintptr ranges: Go's collector does not move heap objects today, and
// a stale extent can only ever cause a spurious report in a diagnostic
// build, never a miss in a production one.  KeepAlive pins the arrays that
// back registered extents so an address cannot be recycled underneath a
// comparison.

// The registry is bucketed by page so a lookup is a map probe rather than
// a scan of every parse in the process.  The first implementation scanned
// a flat slice and cost 3.2x on the lisp package's checked test run
// (38.6s -> 124.1s), which is exactly the kind of cost that gets a
// diagnostic build switched off; bucketing brings it back to noise.
const extentPageShift = 12 // 4 KiB buckets

type constrainedExtent struct {
	lo, hi uintptr
	keep   any // keeps the backing array reachable so its address is not reused
}

var borrowCheck struct {
	sync.Mutex
	pages  map[uintptr][]constrainedExtent
	faults []string
}

func addExtent(lo, hi uintptr, keep any) {
	if lo == 0 || hi <= lo {
		return
	}
	e := constrainedExtent{lo, hi, keep}
	borrowCheck.Lock()
	defer borrowCheck.Unlock()
	if borrowCheck.pages == nil {
		borrowCheck.pages = make(map[uintptr][]constrainedExtent)
	}
	for pg := lo >> extentPageShift; pg <= (hi-1)>>extentPageShift; pg++ {
		bucket := borrowCheck.pages[pg]
		dup := false
		for _, x := range bucket {
			if x.lo == lo && x.hi == hi {
				dup = true
				break
			}
		}
		if !dup {
			borrowCheck.pages[pg] = append(bucket, e)
		}
	}
}

func withinExtent(p uintptr) bool {
	if p == 0 {
		return false
	}
	borrowCheck.Lock()
	defer borrowCheck.Unlock()
	for _, e := range borrowCheck.pages[p>>extentPageShift] {
		if p >= e.lo && p < e.hi {
			return true
		}
	}
	return false
}

func cellsExtent(cells []*LVal) (uintptr, uintptr) {
	if cap(cells) == 0 {
		return 0, 0
	}
	base := uintptr(unsafe.Pointer(unsafe.SliceData(cells)))
	return base, base + uintptr(cap(cells))*unsafe.Sizeof((*LVal)(nil))
}

func bytesExtent(b []byte) (uintptr, uintptr) {
	if cap(b) == 0 {
		return 0, 0
	}
	base := uintptr(unsafe.Pointer(unsafe.SliceData(b)))
	return base, base + uintptr(cap(b))
}

// recordConstrainedCells registers cells as constrained backing storage.
// SealAST calls it for every sealed container.
func recordConstrainedCells(cells []*LVal) {
	lo, hi := cellsExtent(cells)
	addExtent(lo, hi, cells)
	runtime.KeepAlive(cells)
}

// recordConstrainedBytes registers b as constrained backing storage.  No
// in-kernel producer constrains bytes today; the hook exists so that the
// day one does — a sealed embedder blob, an immutable ledger buffer — the
// detector covers it without a second mechanism.
func recordConstrainedBytes(b []byte) {
	lo, hi := bytesExtent(b)
	addExtent(lo, hi, b)
	runtime.KeepAlive(b)
}

func report(kind string, v *LVal) {
	pc, file, line, ok := runtime.Caller(3)
	where := "unknown"
	if ok {
		where = fmt.Sprintf("%s:%d (%s)", file, line, runtime.FuncForPC(pc).Name())
	}
	msg := fmt.Sprintf(
		"borrowcheck: %s — an LVal (type=%v) was minted over CONSTRAINED backing storage without inheriting the constraint.\n"+
			"  minted at: %s\n"+
			"  use the borrow constructors in lisp/borrow.go (v.SliceCells / v.SliceBytes / BorrowCells / BorrowVector / BorrowBytes),\n"+
			"  which take the source as a parameter so the transfer cannot be omitted.  This is the elps#392 class.",
		kind, v.Type, where)
	borrowCheck.Lock()
	borrowCheck.faults = append(borrowCheck.faults, msg)
	borrowCheck.Unlock()
}

// checkMintedOverConstrained reports v if its cells lie inside registered
// constrained storage while v itself carries no constraint.
func checkMintedOverConstrained(v *LVal, cells []*LVal) {
	if v == nil || v.sealed || len(cells) == 0 {
		return
	}
	base := uintptr(unsafe.Pointer(unsafe.SliceData(cells)))
	if withinExtent(base) {
		report("cells", v)
	}
	runtime.KeepAlive(cells)
}

// checkMintedOverConstrainedBytes is checkMintedOverConstrained for LBytes.
func checkMintedOverConstrainedBytes(v *LVal, b []byte) {
	if v == nil || v.sealed || len(b) == 0 {
		return
	}
	base := uintptr(unsafe.Pointer(unsafe.SliceData(b)))
	if withinExtent(base) {
		report("bytes", v)
	}
	runtime.KeepAlive(b)
}

// BorrowFaults returns the provenance faults observed so far, one string
// per fault.  Test suites call it from TestMain to fail a run in which a
// borrowed view was minted without its source's constraint.
func BorrowFaults() []string {
	borrowCheck.Lock()
	defer borrowCheck.Unlock()
	out := make([]string, len(borrowCheck.faults))
	copy(out, borrowCheck.faults)
	return out
}
