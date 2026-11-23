// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
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
//   - a range passes on ONE value, its window; it does not fan out;
//   - '* passes on each element of a list or one-dimensional vector, except
//     a Go nil cell, which Get fails before the rest of the path reads it.
//
// A step Get refuses passes on nothing: a key on a value that is not a map,
// an index, range or '* on a value that is not a list or one-dimensional
// vector, any step on (), a range out of bounds or with its end before its
// start. A key the map lacks and an index past the end pass on nothing too.
// Get answers () for them, but () is not a value of the document, and no
// step can enter it.
//
// A range whose window holds a Go nil cell passes on nothing. Path.Get
// returns such a window as it stands, but the read builtins refuse it
// (the ? family through getPath, and an embedder's builtin such as
// substrate's v1 get), so Visit follows the builtins here.
//
// Visit counts the values each segment applies to, not the values the last
// segment selects. A '* as the last step is one visit, of the list it runs
// over, however many elements the list has: Visit does not walk them. An
// embedder that wants to price that fan-out must count it itself.
//
// Visit walks depth first and keeps one buffer per segment, which it
// reuses. Its memory follows the length of p and the width of one
// container, never the number of values it visits. It visits a value once
// for each path that reaches it, so a cyclic value is walked as far as p
// goes and no further.
//
// Visit follows the grouping Get uses. Chain and Iter flatten their steps
// into one sequence, in which '* applies the steps after it to each
// element, so Visit numbers those steps in order. Root does not flatten: a
// Root inside a composed path is run as one unit, and the steps after it
// apply to its whole answer. Visit cannot count that without running the
// Root's path, so it returns an error, before it calls visit, for a Root
// that has a step after it. A Root with no step after it, such as the one
// ParseSelector and ArgsToPath put around every path, is the same as its
// path. Visit also returns an error for a Path of a type this package did
// not build. For "." and any other path with no step it calls visit never.
func Visit(p Path, v *lisp.LVal, visit func(segment int) error) error {
	steps, err := appendLeafSteps(nil, p, true)
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
// them, as Visit's doc describes. An iterator is one step, its own
// *iterPath, followed by the steps of the chain it holds. last reports
// whether no step runs after p; a Root that is not last is refused, because
// Get runs it as one unit. The recursion follows the nesting of p, which is
// as deep as the caller built it: ArgsToPath bounds a path's length by
// maxPathSteps, but Chain, Iter and Root bound nothing.
func appendLeafSteps(out []Path, p Path, last bool) ([]Path, error) {
	var err error
	switch s := p.(type) {
	case *rootPath:
		if !last {
			return nil, errors.New("elpspath: Visit cannot walk a Root that has steps after it")
		}
		return appendLeafSteps(out, s.path, true)
	case *chainPath:
		for i, q := range s.paths {
			if out, err = appendLeafSteps(out, q, last && i == len(s.paths)-1); err != nil {
				return nil, err
			}
		}
		return out, nil
	case *iterPath:
		// Chain normalizes its steps so that an iterator holds every step
		// after it: nothing in the same chain runs after an iterator.
		return appendLeafSteps(append(out, s), s.path, last)
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
		// The window, refused when it holds a Go nil cell, as the read
		// builtins refuse it (getPath with copyOp.checkReadCells). The
		// window is a list or one-dimensional vector, like its source.
		if w, err := s.Get(v); err == nil {
			if cells, _ := w.SeqCells(); checkCells(cells) == nil {
				out = append(out, w)
			}
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
