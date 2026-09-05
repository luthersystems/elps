// Copyright © 2026 The ELPS authors

package elpstest

import (
	"fmt"
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// Cell-view sharing across Fork: the Cells row of the walker-contract table
// (aliasguard.go, BackingRebuilt), asserted for Fork.
//
// A view -- what cdr, rest, slice and (append 'vector seq) with no values
// return -- is a header whose Cells is a window onto another value's
// backing array, and it records that relationship where it is made (PR
// #602; the convention on lisp.cellsView).  Fork rebuilds the view over ITS
// copy of the root, so an in-place write through the fork's root reaches
// the fork's view exactly as it does in the template and in a cold load.
// Before #602 every header got a private array in the fork and the two came
// apart, which the parity oracle measured from pure ELPS (`(set 'tail (cdr
// l))`, then `(stable-sort < l)` diverging between template and fork).
//
// The structural oracles could not see that: the fingerprint keys on the
// *LVal and two headers over one array share no pointer.  The link makes it
// visible, and this channel asserts it as a contract rather than
// documenting it as an exception.

// CellViewProperty is the property string cellViewWitnesses reports under.
const CellViewProperty = "a fork's view shares its slots with the fork's own root exactly as the template's view shares them with the template's root"

// CellViewDivergence is one binding where the template and a fork disagree
// about a view: rendered as a witness detail line.
type CellViewDivergence struct {
	// Path is the binding, as walkReachable renders it.
	Path string
	// What says how they disagree.
	What string
}

func (d CellViewDivergence) String() string { return d.Path + ": " + d.What }

// cellViewDivergences pairs every value reachable from the template with
// the value at the same path in the fork and compares what each says about
// being a view, through the VALIDATED resolver only (lisp.CellView; slot
// identity is not re-derived here).  Three disagreements are findings:
//
//   - the template's value is a live view and the fork's is not (a stale
//     link, or no link): the fork rebuilt the view privately -- the
//     pre-#602 shape;
//   - the fork's value is a live view and the template's is not: the fork
//     shares slots a cold load would not;
//   - both are live views but the fork's root is a TEMPLATE value: the
//     fork's view is a window onto template memory.
//
// Two stale links, or two non-views, agree, and are not reported: Fork
// copies a stale view privately by the same call this uses, so the guard
// and Fork agree by construction.
func cellViewDivergences(tmpl, fork *lisp.LEnv) []CellViewDivergence {
	tvals := reachableValues(tmpl)
	byPathT := make(map[string]*lisp.LVal, len(tvals))
	for v, path := range tvals {
		byPathT[path] = v
	}
	byPathF := map[string]*lisp.LVal{}
	for v, path := range reachableValues(fork) {
		byPathF[path] = v
	}
	paths := make([]string, 0, len(byPathT))
	for path := range byPathT {
		paths = append(paths, path)
	}
	sort.Strings(paths)
	var out []CellViewDivergence
	for _, path := range paths {
		t, f := byPathT[path], byPathF[path]
		if f == nil {
			continue
		}
		_, toff, tok := t.CellView()
		froot, foff, fok := f.CellView()
		switch {
		case tok && !fok:
			what := "the template's view is live and the fork's carries no link: the fork rebuilt it over a private array"
			if f.IsCellView() {
				what = "the template's view is live and the fork's link is stale: the fork's view no longer shares slots with its own root"
			}
			out = append(out, CellViewDivergence{Path: path, What: what})
		case !tok && fok:
			out = append(out, CellViewDivergence{Path: path,
				What: fmt.Sprintf("the fork's value is a live view (offset %d) where the template's is not: the fork shares slots a cold load would not", foff)})
		case tok && fok:
			if tpath, inTemplate := tvals[froot]; inTemplate {
				out = append(out, CellViewDivergence{Path: path,
					What: fmt.Sprintf("the fork's view is a window onto the TEMPLATE's root (%s, offset %d): a write through the fork reaches template memory", tpath, foff)})
			} else if foff != toff {
				out = append(out, CellViewDivergence{Path: path,
					What: fmt.Sprintf("the fork's view sits at offset %d of its root where the template's sits at %d", foff, toff)})
			}
		}
	}
	return out
}

// cellViewWitnesses reports every cell-view divergence between the template
// and one fork as a single witness under CellViewProperty, with the first
// diverging path as the leak.
func cellViewWitnesses(c TransactionCheck, tmpl, fork *lisp.LEnv, name string) []Witness {
	divs := cellViewDivergences(tmpl, fork)
	if len(divs) == 0 {
		return nil
	}
	lines := make([]string, 0, len(divs)+1)
	lines = append(lines, name+":")
	for _, d := range divs {
		lines = append(lines, d.String())
	}
	return []Witness{{
		Walker:   "Fork",
		Property: CellViewProperty,
		Detail:   strings.Join(lines, "\n    "),
		Leak:     divs[0].Path,
		Repro:    c.Repro,
	}}
}
