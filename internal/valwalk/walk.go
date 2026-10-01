// Copyright © 2026 The ELPS authors

// Package valwalk walks value graphs with an explicit frame stack.
// Visitors select children and control limits, cycle checks, and error order.
package valwalk

import (
	"reflect"
	"strings"
	"sync"

	"github.com/luthersystems/elps/lisp"
)

// Visitor selects children and builds results in traversal order.
type Visitor[R any] interface {
	// Visit runs once per value. Done returns the result without descending.
	Visit(w *Walker[R], v *lisp.LVal) (Step, R, error)
	// Child runs before each child. Walker state still describes the parent.
	Child(w *Walker[R], parent *lisp.LVal, i int) error
	// Leave builds a result. The children slice is borrowed only during this call.
	Leave(w *Walker[R], v *lisp.LVal, children []R) (R, error)
}

// EdgeFunc returns the path segment from parent to child i.
// It runs only when Path is called. An empty segment is transparent.
type EdgeFunc func(parent *lisp.LVal, i int) string

// Step selects a result and the child slices to walk.
type Step struct {
	// Done skips Child and Leave and returns the Visit result.
	Done bool
	// Children selects child storage. Walk retains this slice header until Leave.
	Children []*lisp.LVal
	// Following appends another child slice at the same depth, without copying its slots.
	Following []*lisp.LVal
	// Edge renders child path segments. Nil makes every edge transparent.
	Edge EdgeFunc
}

type frame struct {
	value     *lisp.LVal
	children  []*lisp.LVal
	following []*lisp.LVal
	edge      EdgeFunc
	start     int
	next      int
}

// Walker exposes the active ancestors of the current callback value.
// Its state is valid only during visitor callbacks.
type Walker[R any] struct {
	frames  []frame
	results []R
	active  int
	// Inline storage avoids allocations per container on shallow walks.
	frameBuf  [8]frame
	resultBuf [32]R
}

// Each result type has separate reusable storage. Walk clears all retained values.
var walkerPools sync.Map

func walkerPool[R any]() *sync.Pool {
	key := reflect.TypeFor[R]()
	if pool, ok := walkerPools.Load(key); ok {
		return pool.(*sync.Pool)
	}
	pool, _ := walkerPools.LoadOrStore(key, &sync.Pool{New: func() any { return &Walker[R]{} }})
	return pool.(*sync.Pool)
}

// Depth returns the number of container frames above the callback value.
func (w *Walker[R]) Depth() int { return w.active }

// OnPath reports whether v is an active ancestor. It excludes the current value.
func (w *Walker[R]) OnPath(v *lisp.LVal) bool {
	for i := 0; i < w.active; i++ {
		if w.frames[i].value == v {
			return true
		}
	}
	return false
}

// Ancestors returns a copy of the active ancestors, from root to parent.
func (w *Walker[R]) Ancestors() []*lisp.LVal {
	ancestors := make([]*lisp.LVal, w.active)
	for i := range ancestors {
		ancestors[i] = w.frames[i].value
	}
	return ancestors
}

// Path renders the active edges from root to the callback value.
// The root path is empty. Visitors can prepend their own root marker.
func (w *Walker[R]) Path() string {
	var path strings.Builder
	for i := 0; i < w.active; i++ {
		f := &w.frames[i]
		if f.edge != nil {
			path.WriteString(f.edge(f.value, f.next))
		}
	}
	return path.String()
}

// Walk visits root and the selected child slices in depth-first order.
// It imposes no depth, count, byte, or cycle limit. The first error stops the walk.
func Walk[R any, V Visitor[R]](root *lisp.LVal, vis V) (R, error) {
	pool := walkerPool[R]()
	w := pool.Get().(*Walker[R])
	w.frames = w.frameBuf[:0]
	w.results = w.resultBuf[:0]
	defer func() {
		clear(w.frames)
		clear(w.results)
		clear(w.frameBuf[:])
		clear(w.resultBuf[:])
		w.frames = nil
		w.results = nil
		w.active = 0
		pool.Put(w)
	}()
	var zero R
	v := root
	for {
		w.active = len(w.frames)
		step, result, err := vis.Visit(w, v)
		if err != nil {
			return zero, err
		}
		if !step.Done {
			start := len(w.results)
			count := len(step.Children) + len(step.Following)
			end := start + count
			if end > cap(w.results) {
				w.results = append(w.results, make([]R, count)...)
			} else {
				// Leave clears reused slots. New storage starts with zero values.
				w.results = w.results[:end]
			}
			w.frames = append(w.frames, frame{
				value: v, children: step.Children, following: step.Following, edge: step.Edge, start: start,
			})
		}
		for {
			if step.Done {
				if len(w.frames) == 0 {
					return result, nil
				}
				f := &w.frames[len(w.frames)-1]
				w.results[f.start+f.next] = result
				f.next++
			}
			f := &w.frames[len(w.frames)-1]
			w.active = len(w.frames) - 1
			if f.next < len(f.children)+len(f.following) {
				if err := vis.Child(w, f.value, f.next); err != nil {
					return zero, err
				}
				if f.next < len(f.children) {
					v = f.children[f.next]
				} else {
					v = f.following[f.next-len(f.children)]
				}
				break
			}
			result, err = vis.Leave(w, f.value, w.results[f.start:len(w.results):len(w.results)])
			clear(w.results[f.start:])
			w.results = w.results[:f.start]
			*f = frame{}
			w.frames = w.frames[:len(w.frames)-1]
			if err != nil {
				return zero, err
			}
			step.Done = true
		}
	}
}
