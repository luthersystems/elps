// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"fmt"
	"runtime"
	"sort"
	"strings"
	"sync"
	"unsafe"
)

// Borrowed-backing provenance detector — the READ-half oracle for the
// class behind luthersystems/elps#392 (see docs/borrowed-backing.md).
//
// # The class
//
// An LVal minted over another value's live Go backing storage is a SECOND
// header onto storage a first header already governs.  The constraint that
// governs it — today exactly one, `sealed`, the parse-cache no-write flag
// (lisp/seal.go) — is a property of the STORAGE, not of the header, so a
// header that does not inherit it is a laundering step: it turns
// constrained storage into an unconstrained value, and the next write
// through that value corrupts the original for every environment sharing
// the parse.
//
// # Why a second oracle
//
// The sealed-AST verifier (lisp/seal_check_elpscheck.go) is a
// CORRUPTION-time oracle: it re-fingerprints sealed parses and therefore
// only ever fires for a test that performs the write.  #392's own package
// had tests that performed the READ half — an elpspath range query over a
// program literal — and none that performed the write half, so the
// corruption oracle had nothing to see and the bug shipped.
//
// This detector fires at the read half.  Every parse registers the extent
// of its sealed backing arrays; the raw cell constructors (SExpr, QExpr)
// note a header minted inside a registered extent.  That is precisely "an
// LVal was minted over constrained backing", with no mutation required to
// observe it.
//
// LBytes is deliberately out of scope: SealAST cannot mark one (the parser
// cannot emit an LBytes), so there is never a constraint for a byte view to
// inherit.  #373 is the bytes shape of the class, and it is a RETAINED
// CAPACITY problem rather than a provenance one — only a three-index slice
// at the producer closes it, which is prevention, not detection.
//
// # Discharge: why the check is DEFERRED, not mint-time
//
// The obvious design checks at mint time and reports immediately.  It does
// not work here, and the reason is structural rather than incidental.
//
// Every legitimate borrow in this codebase — builtinCdr, builtinRest,
// builtinSlice in the kernel, and libelpspath's `alias` outside it — is
// written CONSTRUCT-THEN-INHERIT:
//
//	r := QExpr(v.Cells[1:])   // header exists, unconstrained
//	r.sealed = v.sealed       // constraint transferred
//
//	out := lisp.SExpr(cells)  // libelpspath, same shape
//	out.InheritSeal(in)       // via the exported method, package lisp is closed
//
// Go offers no way to make those two statements one for code outside
// package lisp, because `sealed` is unexported and (*LVal).InheritSeal —
// already exported, already merged — is necessarily a second statement.  A
// mint-time detector reports every one of those sites, and the only way to
// silence them is a per-call-site whitelist: the hand-maintained
// discipline this mechanism exists to replace, in a new costume.
//
// So discharge is defined by the POST-CONDITION, not by the route:
//
//	a header minted over constrained storage must CARRY the constraint
//	by the time the run is verified.
//
// A noted mint is held as PENDING and swept later.  A pending header that
// has since acquired the constraint is discharged and dropped; one that
// has not is a fault.  In-kernel `r.sealed = v.sealed` and out-of-kernel
// `out.InheritSeal(in)` are the same write to the same field and discharge
// identically — which is why this detector needs no borrow constructors,
// no exported API of its own, and no whitelist.  The slack between mint
// and sweep is at least borrowCheckSweepAt mints; every real discharge in
// the tree happens on the statement immediately after the mint.
//
// # Reporting
//
// Faults are folded into VerifySealedASTs' error rather than surfaced
// through a new exported function.  VerifySealedASTs is the existing
// process-teardown hook: the lisp package's TestMain, elpstest.Runner,
// elpstest.RunBenchmark and libelpspath's TestMain already call it, and an
// embedder running checked CI already calls it.  Every one of those callers
// gains the read-half oracle without a line of change, and the detector
// adds ZERO public API.
//
// # Cost and scope
//
// Compiled out untagged (lisp/borrow_check_default.go): no registry, no
// lock, no `unsafe`.  Under -tags elpscheck a mint pays one RWMutex read
// lock and one page-bucketed map probe.  Bucketing is what makes that
// affordable — a flat scan of every registered extent is quadratic in the
// number of parses.
//
// # The uintptr caveat
//
// Extents are compared as uintptr ranges, which the Go spec does not
// sanction: a moving collector could relocate a registered array and leave
// the recorded range pointing at unrelated memory.  Three things bound the
// consequence, and they are stated here rather than in a commit message
// because this is the part a reader must be able to audit:
//
//  1. The registry holds a STRONG reference to every array whose extent it
//     records (constrainedExtent.keep).  That is stronger than a
//     runtime.KeepAlive at the recording site: the storage is reachable for
//     as long as the extent is registered, so the address cannot be freed
//     and handed to an unrelated allocation underneath a later comparison.
//     Registration is monotone — extents are never dropped — so the
//     invariant holds for the life of the process.
//  2. The failure the spec permits is RELOCATION, which today's collector
//     does not perform for heap objects.  If a future Go did relocate, a
//     stale extent could make an innocent mint fall inside a registered
//     range, or make a guilty one fall outside it.
//  3. Both directions are bounded by the fact that this is diagnostic-only
//     and tag-gated.  A stale extent in the first direction costs a
//     SPURIOUS report in a checked build — a fault message naming a mint
//     site, which a human reads and dismisses.  In the second it costs a
//     MISSED report in a checked build.  Neither direction can change what
//     a production binary does, because a production binary contains none
//     of this code, and neither can change what a checked binary COMPUTES:
//     the detector only ever appends to a diagnostic slice.
//
// The residual risk is therefore "a future Go release makes the checked
// build noisy or blind", not "a future Go release breaks elps".  The
// mitigation for that day is to swap the uintptr extents for identity on
// the *LVal that owns the storage; that is a strictly better mechanism and
// the only reason it is not the mechanism today is that a borrowed
// subslice carries no pointer back to its owner's header.

// extentPageShift buckets the extent registry by 4 KiB page so a lookup is
// a map probe rather than a scan.  A flat scan is quadratic in the number
// of sealed parses, which on this repo's checked lisp suite is the
// difference between "within noise" and "several times slower".
const extentPageShift = 12

// borrowCheckSweepAt bounds the pending table.  Past this many undischarged
// notes the table is swept — discharged entries dropped, undischarged ones
// converted to faults — and reset, so memory is bounded and the sweep is
// amortised O(1) per mint.  The value is the SLACK the deferred discharge
// gets: every legitimate borrow in the tree discharges on the statement
// after the mint, so 2^16 is roughly five orders of magnitude of margin.
const borrowCheckSweepAt = 1 << 16

// borrowCheckMaxExtents caps the registry.  Past it, new sealed parses are
// not recorded.  Degrading into MISSED reports (never spurious ones) is
// the safe direction: an unrecorded extent simply is not watched, whereas
// dropping a recorded extent would release the strong reference that keeps
// its address from being reused, which is the one thing that could make a
// report wrong rather than absent.
const borrowCheckMaxExtents = 1 << 20

type constrainedExtent struct {
	keep   any // strong ref: the address cannot be reused while registered
	lo, hi uintptr
}

// pendingMint is a header noted at mint time and not yet discharged.  The
// call site is kept as raw PCs so the mint path pays no symbolisation:
// resolution happens once per fault, in the sweep.
type pendingMint struct {
	v    *LVal
	kind string
	pcs  [6]uintptr
	n    int
}


var borrowCheck struct {
	pages   map[uintptr][]constrainedExtent
	faults  map[string]int // site → number of undischarged mints
	pending []pendingMint
	mu      sync.RWMutex
	extents int
}

func addExtent(lo, hi uintptr, keep any) {
	if lo == 0 || hi <= lo {
		return
	}
	borrowCheck.mu.Lock()
	defer borrowCheck.mu.Unlock()
	if borrowCheck.extents >= borrowCheckMaxExtents {
		return
	}
	if borrowCheck.pages == nil {
		borrowCheck.pages = make(map[uintptr][]constrainedExtent)
	}
	e := constrainedExtent{lo: lo, hi: hi, keep: keep}
	recorded := false
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
			recorded = true
		}
	}
	if recorded {
		borrowCheck.extents++
	}
}

func withinExtent(p uintptr) bool {
	if p == 0 {
		return false
	}
	borrowCheck.mu.RLock()
	defer borrowCheck.mu.RUnlock()
	for _, e := range borrowCheck.pages[p>>extentPageShift] {
		if p >= e.lo && p < e.hi {
			return true
		}
	}
	return false
}

// sliceBase is the ONE place this file converts a pointer to an integer.
// It is the whole of the mechanism's `unsafe` surface: the address is
// compared against registered extents and never converted back, never
// dereferenced, and never stored anywhere a production build can see (this
// file does not exist without -tags elpscheck).  See "The uintptr caveat"
// above for why a stale address can only ever cost a spurious or missing
// diagnostic, never a wrong computation.
func sliceBase(cells []*LVal) uintptr {
	return uintptr(unsafe.Pointer(unsafe.SliceData(cells))) //nolint:gosec // G103: audited above; diagnostic-only address comparison
}

func cellsExtent(cells []*LVal) (uintptr, uintptr) {
	if cap(cells) == 0 {
		return 0, 0
	}
	base := sliceBase(cells)
	return base, base + uintptr(cap(cells))*unsafe.Sizeof((*LVal)(nil))
}

// recordConstrainedCells registers cells as constrained backing storage.
// sealAST calls it for every node it seals, so the registry covers a whole
// parse rather than only its root.
func recordConstrainedCells(cells []*LVal) {
	lo, hi := cellsExtent(cells)
	addExtent(lo, hi, cells)
	runtime.KeepAlive(cells)
}

// notePending records a header minted over constrained storage.  It is not
// yet a fault: see the discharge discussion above.
func notePending(kind string, v *LVal) {
	p := pendingMint{v: v, kind: kind}
	// skip=3: notePending → noteMintOverConstrained* → the constructor →
	// the producer we want to name.  Six frames are kept because the
	// producer is often reached through a chain of constructors that are
	// themselves uninformative — Vector → Array → QExpr, or libelpspath's
	// alias → toList → SExpr — and the site a human must edit is the one
	// past the end of that chain.
	p.n = runtime.Callers(3, p.pcs[:])
	borrowCheck.mu.Lock()
	borrowCheck.pending = append(borrowCheck.pending, p)
	if len(borrowCheck.pending) >= borrowCheckSweepAt {
		sweepPendingLocked()
	}
	borrowCheck.mu.Unlock()
}

// sweepPendingLocked resolves the pending table: a header that has since
// acquired the constraint is discharged and forgotten, one that has not is
// recorded as a fault against its mint site.  Caller holds borrowCheck.mu
// for writing.
func sweepPendingLocked() {
	for i := range borrowCheck.pending {
		p := &borrowCheck.pending[i]
		if p.v == nil || p.v.sealed {
			continue // discharged
		}
		if borrowCheck.faults == nil {
			borrowCheck.faults = make(map[string]int)
		}
		borrowCheck.faults[borrowFaultMessage(p)]++
	}
	borrowCheck.pending = borrowCheck.pending[:0]
}

func borrowFaultMessage(p *pendingMint) string {
	where := "unknown"
	if p.n > 0 {
		frames := runtime.CallersFrames(p.pcs[:p.n])
		sites := make([]string, 0, p.n)
		for {
			f, more := frames.Next()
			sites = append(sites, fmt.Sprintf("%s:%d (%s)", f.File, f.Line, f.Function))
			if !more {
				break
			}
		}
		where = strings.Join(sites, "\n      from: ")
	}
	return fmt.Sprintf(
		"borrowcheck: an LVal (%s) was minted over CONSTRAINED backing storage and never inherited the constraint.\n"+
			"    minted at: %s\n"+
			"  A header over another value's live backing must carry that value's storage-scoped\n"+
			"  constraints, or the next write through it corrupts the original (elps#392).  Inside\n"+
			"  package lisp assign `r.sealed = src.sealed`; outside it call `r.InheritSeal(src)`.\n"+
			"  See docs/borrowed-backing.md.", p.kind, where)
}

// noteMintOverConstrainedCells notes v if its cells lie inside registered
// constrained storage while v itself carries no constraint.
func noteMintOverConstrainedCells(v *LVal, cells []*LVal) {
	if v == nil || v.sealed || len(cells) == 0 {
		return
	}
	if withinExtent(sliceBase(cells)) {
		notePending("cells", v)
	}
	runtime.KeepAlive(cells)
}

// borrowProvenanceFaults sweeps the pending table and returns one message
// per distinct mint site that produced an undischarged header, sorted so a
// CI log is stable.  VerifySealedASTs calls it; nothing else does, and it
// is deliberately unexported (see the Reporting section above).
func borrowProvenanceFaults() []string {
	borrowCheck.mu.Lock()
	defer borrowCheck.mu.Unlock()
	sweepPendingLocked()
	out := make([]string, 0, len(borrowCheck.faults))
	for msg, n := range borrowCheck.faults {
		if n > 1 {
			msg = fmt.Sprintf("%s\n  (%d headers minted at this site)", msg, n)
		}
		out = append(out, msg)
	}
	sort.Strings(out)
	return out
}

// dropBorrowNotes discards every pending note and recorded fault.  Its only
// caller is DropBorrowNotesForTest (lisp/borrow_check_export_test.go); see
// that file for why one test needs it and why it is not a whitelist.
func dropBorrowNotes() {
	borrowCheck.mu.Lock()
	defer borrowCheck.mu.Unlock()
	borrowCheck.pending = borrowCheck.pending[:0]
	borrowCheck.faults = nil
}
