// Copyright © 2026 The ELPS authors

package libelpspath

import "github.com/luthersystems/elps/lisp"

// An LVal graph is a tree in every value a program writes down, but it is not
// a tree in general: assoc! and append! mutate a container in place, so a
// program can store a container inside itself.  Every recursive walk over
// *LVal then recurses forever.
//
// The failure mode is why this is guarded rather than tolerated.  An
// unbounded walk does not raise a condition and does not return: it grows the
// goroutine stack past the runtime's limit and the process dies with "fatal
// error: stack overflow", which recover() cannot catch.  The evaluator turns
// internal panics into conditions and is powerless here.  This package runs on
// an embedder's per-transaction path, so that is a program-author-triggerable
// kill of the host.  See issue #393.
//
// # Why the guard lives here rather than in package lisp
//
// It was briefly exported from package lisp, as lisp.CycleGuard in
// lisp/cycleexport.go, on the theory that the hazard is not confined to that
// package and other walkers would want the same protocol.  Nothing else ever
// took it up: package lisp's own walks use the unexported guard in
// lisp/cycle.go, which has a third stage this one does not need, and this
// package was the export's only consumer in the tree.  An embedder that hit
// the same hazard downstream could not use it either -- the type is in no
// released elps -- and wrote its own local guard instead, which is the shape
// below.
//
// So the export bought nothing and cost the one thing an exported type always
// costs: it is public API, and removing public API is a breaking change while
// adding it is not.  The ~80 lines duplicated here are the cheaper side of
// that trade, and they are duplicated at zero risk to package lisp: nothing in
// this file is on the rendering or equality path, where a shared guard
// implementation would put this package's needs next to String's and Equal's
// hot loops.  Issue #391 measured what that costs.  Exporting a guard again
// later, if a second consumer ever appears, remains easy.
//
// # The guard
//
// Cycle detection that is always on costs a map allocation per walk, and the
// walks it would tax run per transaction.  So a walk escalates in two stages.
//
// Stage 1, the common path, carries nothing but an int.  descend increments it
// and compares it against cycleGuardDepth, which is free next to the type
// dispatch or the copy the walk exists to do, and allocates nothing beyond the
// single shared state below.
//
// Stage 2 begins if a walk nests deeper than that: it starts recording the
// nodes on its *current path* and reports a node that is already on it.  The
// set is path-scoped rather than walk-scoped -- a node is removed again when
// the walk leaves it -- because a value that merely shares a substructure with
// itself, (list x x), is a DAG and not a cycle, and must still be accepted and
// copied in full.
//
// Package lisp's guard has a third stage: a cyclic value found at stage 2
// abandons the walk so the caller can rerun it in strict mode, because
// rendering wants to keep going past a cycle and print a marker, and unrolling
// a value that holds itself under two keys visits 2^depth nodes.  Nothing here
// needs it.  Every walk in this package answers a cycle with an error and
// stops at the first frame that finds one, so nothing below is explored and
// there is no exponential unrolling to escalate away from.  The export never
// exposed strict mode either, so this is not a narrowing of what was removed.
//
// Two rules carry the cost claim below.  Only a value that can reach other
// values is entered on the path: descending into a leaf taxes every int and
// string in the graph to bound a walk that cannot recurse, which is the
// regression #391 had to back out of the kernel's rendering and equality
// walks.  And every nested call passes g down rather than starting a new walk,
// or the bound resets on every lap and never fires.
//
// The result is that acyclic values are untouched -- walked in full, at any
// depth allowed by lisp.MaxValueDepth (checked by copyGuarded) -- while a cyclic
// value is refused with an ordinary Go error the builtins turn into a
// catchable condition.
//
// cycleGuardDepth is chosen well above the nesting real documents reach and
// well below anything that troubles a goroutine stack, so neither property
// costs the other.  Nothing depends on the exact number.
const cycleGuardDepth = 64

// cycleState is what a guarded walk shares between every frame rather than
// copying into each: the path it is on once it is deep enough to record one.
//
// It is addressed rather than returned so that the guard stays pass-by-value
// and the walks keep their signatures.  The entry points declare it as a local
// and pass its address; no frame stores that pointer anywhere but its own
// stack, so escape analysis keeps the state off the heap and a walk that never
// gets deep enough to record a path allocates nothing at all.
// TestCycleGuardAllocationCost pins that.
type cycleState struct {
	// path holds the nodes on the walk's current path.  It stays nil until
	// the walk passes cycleGuardDepth, which is what keeps a shallow walk
	// down to the one allocation the state itself costs.
	//
	// It lives on the shared state rather than in the by-value guard so that
	// a walk allocates it at most once.  A guard copy that made its own would
	// make one per node sitting at exactly cycleGuardDepth, because every
	// such node inherits a nil path from its parent one level up -- a
	// per-node cost on a value that is merely wide at that depth, which is an
	// acyclic value paying for the guard.
	// TestCycleGuardAllocationDoesNotScale pins that.
	//
	// Sharing it is not a change of meaning: ascend removes a node when the
	// walk leaves it, so the set still holds exactly the nodes between the
	// walk's root and the current frame.
	path map[*lisp.LVal]struct{}

	// work and valid bound the validator over a value with nested sharing
	// -- (set! x (list x x)) repeated D times has 2^D paths but D distinct
	// containers -- by the rule lisp/sharing.go describes for the kernel's
	// walkers.  work counts the containers the walk has finished, plus the
	// cells they hold.  Once it passes sharedWalkBudget, the validator
	// records in valid each container it finished without error, and a
	// container reached again passes without being walked again.  A tree
	// never reaches a container twice, so for a tree this changes nothing
	// but the memory the map takes.
	valid map[*lisp.LVal]struct{}
	// op is the copy memo, shared by every copy walk of one operation (see
	// copyOp); nil for a walk that does not copy.  ownOp is the memo of a
	// copy walk that belongs to no operation, kept here rather than
	// allocated so such a walk costs what it did.
	op    *copyOp
	ownOp copyOp
	work  int

	// limit is the value-walk depth bound for this walk, taken from the
	// runtime the operation is running under (Runtime.ValueDepthLimit).  It
	// lives on the shared state rather than in the by-value guard because it
	// is constant for a walk: putting it in the guard would widen every
	// frame copy to carry a number none of them changes.  Zero -- the state
	// a caller that never set one leaves it in -- means lisp.MaxValueDepth,
	// which is the same answer Runtime.ValueDepthLimit gives for a nil
	// runtime.
	limit int
}

// sharedWalkBudget is lisp's budget of the same name (lisp/sharing.go): the
// work after which a walk begins to memoise, by identity, the containers it
// finishes.
const sharedWalkBudget = 4096

// copyOp is what one elpspath operation's copies share: the value depth
// limit, and the sharing memo.  An operation copies in many walks -- one
// per level of a chained path, one per element of an iterator -- and each
// would otherwise copy up to sharedWalkBudget of a shared value as a tree
// before its own memo switched on, so the memo belongs to the operation.
// Reusing an entry across walks that start at different depths stays exact,
// because the entry carries the copied container's height.
//
// work counts the containers the copies have entered, plus the cells they
// hold.  Once it passes sharedWalkBudget, copies records each container a
// copy finished, with the copy and its height, and a container reached
// again -- by the same walk or a later one -- reuses that copy, as lisp's
// copy does.  A tree never reaches a container twice.
//
// The remaining fields are the operation's iterator work budget (budget.go):
// the environment it charges and polls (nil for the exported Path API,
// which counts nothing), the condition that stopped it, the work its
// iterators have done, and how many iterators are running.
type copyOp struct {
	copies    map[*lisp.LVal]copyMemo
	env       *lisp.LEnv
	stop      *lisp.LVal
	limit     int
	work      int
	iterWork  int
	iterDepth int
}

// newCopyOp returns the copy state for one operation bounded by limit.
// Zero, and a limit below the floor WithMaxValueDepth accepts, mean
// lisp.MaxValueDepth.
func newCopyOp(limit int) *copyOp {
	return &copyOp{limit: limit}
}

// copyMemo is one container's entry in copyOp.copies: its copy, and the
// number of container levels the copy walked below it, so that a memo hit at
// depth d fails the value depth limit exactly where re-copying the container
// would: when d+height reaches the limit.
type copyMemo struct {
	cp     *lisp.LVal
	height int
}

// noteValid records, once the walk is past its budget, that in and
// everything under it passed validation.  width is the number of values in
// holds.
func (st *cycleState) noteValid(in *lisp.LVal, width int) {
	st.work += 1 + width
	if st.valid == nil && st.work > sharedWalkBudget {
		st.valid = make(map[*lisp.LVal]struct{})
	}
	if st.valid != nil {
		st.valid[in] = struct{}{}
	}
}

// cycleGuard bounds a recursive walk over an LVal graph.
//
// It is copied by value down the walk: each frame holds its own depth, while
// the state it points at -- including the path -- is shared, which is what
// makes the path the set of nodes between the root of the walk and the current
// frame rather than a per-frame set.
//
// The zero cycleGuard is not usable; newCycleGuard builds the guard for the
// root of a fresh walk from a caller-owned cycleState.
//
// That is the one deliberate departure from the lisp.CycleGuard this replaces,
// whose zero value was usable because it allocated the shared state itself on
// the first Descend -- one allocation per walk, which
// TestCycleGuardAllocationCost used to pin.  An exported type has to be usable
// without a constructor a caller can forget; an unexported one in the package
// that owns every call site does not, and taking the state from the caller
// puts it on the caller's stack instead.  The walk now costs the guard
// nothing, which the same test pins at its new number.
type cycleGuard struct {
	state *cycleState

	depth int
}

// newCycleGuard returns the guard for the root of a fresh walk, recording its
// path on state.
//
// The caller declares the state as a local:
//
//	var st cycleState
//	err := walk(v, newCycleGuard(&st))
//
// rather than the guard allocating one, so that a walk that never gets deep
// enough to record a path allocates nothing at all.
func newCycleGuard(state *cycleState) cycleGuard {
	return cycleGuard{state: state}
}

// newCycleGuardLimit is newCycleGuard for a walk whose depth bound comes from
// a runtime rather than from the MaxValueDepth default.  A limit below the
// floor WithMaxValueDepth accepts (and zero, the unset case) means the
// default, so that a caller with no runtime in reach passes 0.
func newCycleGuardLimit(state *cycleState, limit int) cycleGuard {
	state.ownOp.limit = limit
	return newCycleGuardOp(state, nil)
}

// newCycleGuardOp is newCycleGuardLimit for a copy walk belonging to op,
// which supplies the limit and the shared memo.  A nil op is a walk of its
// own, with a private memo and the limit already in state.ownOp.
func newCycleGuardOp(state *cycleState, op *copyOp) cycleGuard {
	state.op = op
	if limit := state.copyOp().limit; limit >= 1024 {
		state.limit = limit
	}
	return cycleGuard{state: state}
}

// copyOp returns the copy memo this walk uses: its operation's, or its own.
// The own memo is addressed here rather than stored in op, which would make
// the state point into itself and move it off the caller's stack.
func (st *cycleState) copyOp() *copyOp {
	if st.op != nil {
		return st.op
	}
	return &st.ownOp
}

// valueDepthLimit reports the depth bound the walk is running under.
func (g cycleGuard) valueDepthLimit() int {
	if g.state != nil && g.state.limit >= 1024 {
		return g.state.limit
	}
	return lisp.MaxValueDepth
}

// descend returns the guard for a walk one level below g, entering v, and
// reports whether v is already on the walk's current path -- that is, whether
// v contains itself.
//
// A caller that gets false back must pair the call with ascend(v) on the
// returned guard when it leaves v, unless tracking reports there is no path to
// unwind.
//
// Only a value that can reach other values may be entered here.  Descending
// into a leaf would tax every int and string in the graph to bound a walk that
// cannot recurse; every walk in this package dispatches on the container types
// before calling this.
func (g cycleGuard) descend(v *lisp.LVal) (cycleGuard, bool) {
	g.depth++
	if g.depth < cycleGuardDepth {
		return g, false
	}
	if g.state.path == nil {
		g.state.path = make(map[*lisp.LVal]struct{}, cycleGuardDepth)
	} else if _, ok := g.state.path[v]; ok {
		return g, true
	}
	g.state.path[v] = struct{}{}
	return g, false
}

// ascend leaves v, removing it from the current path.  It is a no-op for a
// walk that never got deep enough to track one.
func (g cycleGuard) ascend(v *lisp.LVal) {
	if g.state.path == nil {
		return
	}
	delete(g.state.path, v)
}

// tracking reports whether the descend that produced g put its value on the
// path, and so whether the caller has to pair it with ascend at all.  It is a
// comparison rather than a nil check on the path because the path is shared: a
// deeper frame may have created it while this frame is above the depth that
// records anything.
func (g cycleGuard) tracking() bool {
	return g.depth >= cycleGuardDepth
}
