package lisp

// CycleGuard bounds a recursive walk over an LVal graph performed outside
// package lisp.
//
// It exists because the hazard cycleGuard was written for is not confined to
// this package.  assoc! and append! mutate a container in place, so a program
// can store a container inside itself, and any recursive walk over the result
// -- wherever it lives -- runs the goroutine stack out and takes the process
// down with "fatal error: stack overflow", which recover() cannot catch and
// handler-bind therefore never sees.  A lisplib package or an embedder's own
// builtin can walk a value it was handed just as this package can, and issue
// #393 is what that costs: lisp/lisplib/libelpspath, which is on substrate's
// per-transaction path, killed the host through walkers of its own while the
// kernel's guarded walks rendered the same value as #<cycle>.
//
// This is the whole protocol, deliberately: the depth constant, cycleState
// and strict mode stay unexported, and there is no rerun.  A walk that
// answers a cycle with an error needs none -- it stops at the first frame
// that finds one, so nothing below is explored and there is no exponential
// unrolling to escalate away from.  A walk that wants to keep going past a
// cycle, rendering a marker and carrying on, does need the rerun and belongs
// in this package next to String rather than out here.
//
// The zero CycleGuard is the guard for the root of a fresh walk; there is no
// constructor to forget.  A walk is written:
//
//	func walk(v *lisp.LVal, g lisp.CycleGuard) error {
//		// leaves are not entered on the path -- see below
//		if !container(v) {
//			return nil
//		}
//		g, cyclic := g.Descend(v)
//		if cyclic {
//			return errCyclicValue
//		}
//		err := walkChildren(v, g) // pass g down; never start a fresh walk
//		if g.Tracking() {
//			g.Ascend(v)
//		}
//		return err
//	}
//
// Two rules carry the cost claim.  Only a value that can reach other values
// is entered on the path: descending into a leaf taxes every int and string
// in the graph to bound a walk that cannot recurse, which is the regression
// #391 had to back out of the rendering and equality walks.  And every nested
// call passes g down rather than starting a new walk, or the bound resets on
// every lap and never fires.
//
// Cost: the walk carries an int per frame and reaches the shared state only
// once it nests past the guard depth, at which point it allocates one path
// set for the whole walk.  The state itself is allocated once, on the first
// Descend, rather than at the depth the path starts -- allocating it there
// would build one per node sitting at exactly that depth, so a value merely
// wide at the threshold would pay per node, which is the per-node allocation
// #391 moved the path off the by-value guard to remove.
type CycleGuard struct {
	g cycleGuard
}

// Descend returns the guard for a walk one level below c, entering v, and
// reports whether v is already on the walk's current path -- that is, whether
// v contains itself.
//
// A caller that gets false back must pair the call with Ascend(v) on the
// returned guard when it leaves v, unless Tracking reports there is no path
// to unwind.
func (c CycleGuard) Descend(v *LVal) (CycleGuard, bool) {
	if c.g.state == nil {
		c.g.state = new(cycleState)
	}
	g, cyclic := c.g.descend(v)
	return CycleGuard{g: g}, cyclic
}

// Ascend leaves v, removing it from the current path.  It is a no-op for a
// walk that never got deep enough to track one.
func (c CycleGuard) Ascend(v *LVal) {
	if c.g.state == nil {
		return
	}
	c.g.ascend(v)
}

// Tracking reports whether the Descend that produced c put its value on the
// path, and so whether the caller has to pair it with Ascend at all.
func (c CycleGuard) Tracking() bool {
	return c.g.tracking()
}
