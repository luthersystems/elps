// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

// Visit walks the path p over v and calls visit once for each value that a
// segment of p applies to. It passes the segment's position: segment i is
// the i-th step of p, in the order SelectorSteps renders a selector. Visit
// returns the first error visit returns, at once, and does no more work.
//
// It exists for an embedder that charges for a path before it runs it. A
// '* over a document that shares a subtree, or contains itself, reaches a
// number of values that grows as 2^k in the number k of iterators, and the
// methods of Path have no environment to stop them (see Path). An embedder
// that prices each value can stop the walk from visit when the price is too
// high. Visit selects each value with the same helpers Get uses, so the
// count cannot drift from what Get does.
//
// A segment applies to a value when Get runs that segment's step on it:
//
//   - segment 0 applies to v;
//   - a key passes on the value the map holds at that key;
//   - an index passes on the element it lands on, a negative index counting
//     back from the end;
//   - a range passes on ONE value, the window Get returns; it does not fan
//     out, and a range Get refuses (out of bounds, end before start) passes
//     on nothing;
//   - '* passes on each element of a list or one-dimensional vector, except
//     a Go nil cell, which Get fails before the rest of the path reads it.
//
// A step Get refuses passes on nothing: a key on a value that is not a map,
// an index, range or '* on a value that is not a list or one-dimensional
// vector, any step on (). A key the map lacks and an index past the end
// pass on nothing too. Get answers () for them, but () is not a value of
// the document, and no step can enter it.
//
// Visit walks depth first and keeps one buffer per segment, which it
// reuses. Its memory follows the length of p and the width of one
// container, never the number of values it visits. It visits a value once
// for each path that reaches it, so a cyclic value is walked as far as p
// goes and no further.
//
// The segments of p are those Get runs: Visit accepts the paths this
// package builds (ParseSelector, ArgsToPath, Root, Chain, Dot, Index,
// Range, Iter). It returns an error for a Path of another type, before it
// calls visit. For "." and any other path with no step it calls visit
// never.
func Visit(p Path, v *lisp.LVal, visit func(segment int) error) error {
	steps, err := appendLeafSteps(nil, p)
	if err != nil {
		return err
	}
	n := len(steps)
	if n == 0 {
		return nil
	}
	// bufs[d] holds the values segment d applies to under the current
	// value of segment d-1; idx[d] is the next one to visit.
	bufs := make([][]*lisp.LVal, n)
	idx := make([]int, n)
	bufs[0] = []*lisp.LVal{v}
	for d := 0; d >= 0; {
		if idx[d] == len(bufs[d]) {
			d--
			continue
		}
		x := bufs[d][idx[d]]
		idx[d]++
		if err := visit(d); err != nil {
			return err
		}
		if d+1 < n {
			bufs[d+1] = appendSelected(bufs[d+1][:0], x, steps[d])
			idx[d+1] = 0
			d++
		}
	}
	return nil
}

// appendLeafSteps appends the leaf steps of p to out, in the order Get runs
// them. An iterator is one step, its own *iterPath, followed by the steps of
// the chain it holds. The recursion follows the nesting of p, which this
// package bounds by the length of the path (maxPathSteps).
func appendLeafSteps(out []Path, p Path) ([]Path, error) {
	var err error
	switch s := p.(type) {
	case *rootPath:
		return appendLeafSteps(out, s.path)
	case *chainPath:
		for _, q := range s.paths {
			if out, err = appendLeafSteps(out, q); err != nil {
				return nil, err
			}
		}
		return out, nil
	case *iterPath:
		return appendLeafSteps(append(out, s), s.path)
	case *dotPath, *indexPath, *rangePath:
		return append(out, p), nil
	default:
		return nil, fmt.Errorf("elpspath: Visit cannot walk a %T", p)
	}
}

// appendSelected appends to out the values the leaf step selects from v, as
// Visit's doc describes, and returns out. It calls the selection helpers of
// the step's Get, so the two agree by construction. A refused step selects
// nothing; its error is Get's to report, not Visit's.
func appendSelected(out []*lisp.LVal, v *lisp.LVal, step Path) []*lisp.LVal {
	if v == nil {
		// Only a root a caller passed can be nil here: '* does not pass on
		// a nil cell. No step can read it.
		return out
	}
	switch s := step.(type) {
	case *dotPath:
		if x, err := s.lookup(v); err == nil && x != nil {
			out = append(out, x)
		}
	case *indexPath:
		if x, err := s.lookup(v); err == nil && x != nil {
			out = append(out, x)
		}
	case *rangePath:
		if w, err := s.Get(v); err == nil {
			out = append(out, w)
		}
	case *iterPath:
		// The iterator itself; the chain it holds is the later steps.
		cells, err := toCells(v)
		if err != nil {
			return out
		}
		for _, c := range cells {
			if checkCell(c) == nil {
				out = append(out, c)
			}
		}
	}
	return out
}
