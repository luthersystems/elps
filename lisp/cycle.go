package lisp

// An LVal graph is a tree in every value a program writes down, but it is not
// a tree in general: assoc! and append! mutate a container in place, so a
// program can store a container inside itself.  Every recursive walk over
// *LVal -- rendering, structural equality, JSON encoding, macro-expansion
// stamping -- then recurses forever.
//
// The failure mode is why this is guarded rather than tolerated.  An unbounded
// walk does not raise a condition and does not return: it grows the goroutine
// stack past the runtime's limit and the process dies with "fatal error: stack
// overflow", which recover() cannot catch.  The evaluator turns internal
// panics into conditions and is powerless here.  For an embedder running
// several tenants' programs in one process that is a denial of service
// reachable from 32 bytes of lisp.  See issue #390.
//
// # The guard
//
// Cycle detection that is always on costs a map allocation per walk, and the
// walks it would tax -- String above all -- are hot.  So a walk escalates in
// two stages.
//
// Stage 1, the common path, carries nothing but an int.  descend increments it
// and compares it against cycleGuardDepth, which is free next to the
// formatting or comparison the walk exists to do, and allocates nothing.
//
// Stage 2 begins if a walk nests deeper than that: it starts recording the
// nodes on its *current path* and reports a node that is already on it.  The
// set is path-scoped rather than walk-scoped -- a node is removed again when
// the walk leaves it -- because a value that merely shares a substructure with
// itself, (list x x), is a DAG and not a cycle, and must still render as what
// it is.
//
// Stage 3 exists because stages 1 and 2 bound the *depth* of a walk and not
// its *width*.  A map holding itself under two keys is only one node deep in
// the cycle, but unrolling it to cycleGuardDepth levels visits 2^depth nodes:
// swapping a fatal crash for a walk that will not finish this century is no
// fix at all.  So the first frame to find a cycle records that on a state
// object shared by the whole walk, every frame above it returns immediately
// (see abandoned), and the caller reruns the walk in strict mode: the path set
// is allocated up front and nothing is ever removed from it, so every node is
// visited at most once and the walk is linear in the size of the graph.
//
// The result is that acyclic values are untouched -- walked in full, at any
// depth, byte for byte the same output, equality and JSON as before the guard
// existed -- while a cyclic value terminates in time linear in the number of
// values it can reach.  Strict mode's coarser rule, that any node reached
// twice reads as a cycle, only ever applies to a value already known to
// contain one, and such a value has no finite faithful rendering anyway.
//
// cycleGuardDepth is chosen well above the nesting real values reach and well
// below anything that troubles a goroutine stack, so neither property costs
// the other.  Nothing depends on the exact number.
const cycleGuardDepth = 64

// cycleMark is what a value that contains itself renders as, at the point the
// walk reaches it for the second time.  A marker was chosen over the
// alternative of rejecting self-insertion in assoc!/append!, which would
// change those functions' documented in-place semantics and still miss cycles
// assembled by any other route.  See issue #390.
const cycleMark = "#<cycle>"

// cycleState is what a guarded walk shares between every frame rather than
// copying into each: whether the walk has found a cycle, and the path it is
// on once it is deep enough to record one.
//
// It is addressed rather than returned so that the guards stay pass-by-value
// and the walks keep their signatures.  A caller declares it as a local and
// passes its address; because no frame stores that pointer anywhere but its
// own stack, escape analysis keeps the state on the caller's stack and the
// common path allocates nothing.  TestGuardedWalksDoNotAllocate pins that.
type cycleState struct {
	// path holds the nodes on the walk's current path.  It stays nil until
	// the walk passes cycleGuardDepth, which is what keeps a shallow walk
	// allocation-free.
	//
	// It lives on the shared state rather than in the by-value guard so that
	// a walk allocates it at most once.  A guard copy that made its own would
	// make one per node sitting at exactly cycleGuardDepth, because every such
	// node inherits a nil path from its parent one level up -- a per-node cost
	// on a value that is merely wide at that depth, which is an acyclic value
	// paying for the guard.  TestGuardIsFlatAcrossTheGuardDepth pins that.
	//
	// Sharing it is not a change of meaning: ascend removes a node when the
	// walk leaves it, so the set still holds exactly the nodes between the
	// walk's root and the current frame.
	path map[*LVal]struct{}

	cyclic bool
}

// cycleGuard bounds a recursive walk over an LVal graph.
//
// It is copied by value down the walk: each frame holds its own depth, while
// the state it points at -- including the path -- is shared, which is what
// makes the path the set of nodes between the root of the walk and the current
// frame rather than a per-frame set.
type cycleGuard struct {
	state *cycleState

	depth int

	// strict makes this the rerun of a walk already known to hit a cycle:
	// path is tracked from the first frame and never unwound, so every node
	// is visited once and the walk is linear.
	strict bool
}

// strictCycleGuard returns the guard for the rerun of a walk that stage 2
// abandoned.
func strictCycleGuard() cycleGuard {
	return cycleGuard{state: new(cycleState), strict: true}
}

// descend returns the guard for a walk one level below g, entering v, and
// reports whether v is already on the walk's current path -- that is, whether
// v contains itself.
//
// A caller that gets false back must pair the call with ascend(v) on the
// returned guard when it leaves v, unless tracking reports there is no path to
// unwind.
func (g cycleGuard) descend(v *LVal) (cycleGuard, bool) {
	g.depth++
	probeRecord("value", g.depth, &probeMaxValueDepth, &probeValueDescends)
	if !g.strict && g.depth < cycleGuardDepth {
		return g, false
	}
	if g.state.path == nil {
		g.state.path = make(map[*LVal]struct{}, cycleGuardDepth)
	} else if _, ok := g.state.path[v]; ok {
		g.state.cyclic = true
		return g, true
	}
	g.state.path[v] = struct{}{}
	return g, false
}

// ascend leaves v, removing it from the current path.  It is a no-op for a
// walk that never got deep enough to track a path, and in strict mode, where
// the point of the path is that nothing leaves it.
func (g cycleGuard) ascend(v *LVal) {
	if !g.strict {
		delete(g.state.path, v)
	}
}

// tracking reports whether the descend that produced g put its value on the
// path, and so whether the caller has to pair it with ascend at all.  It is a
// comparison rather than a nil check on the path because the path is shared:
// a deeper frame may have created it while this frame is above the depth that
// records anything.
func (g cycleGuard) tracking() bool {
	return g.strict || g.depth >= cycleGuardDepth
}

// abandoned reports that some frame of this walk has found a cycle and the
// walk is being unwound so its caller can rerun it in strict mode.  A frame
// that sees this must return without descending any further; whatever it
// returns is discarded.
func (g cycleGuard) abandoned() bool {
	return !g.strict && g.state.cyclic
}

// valuePair is a pair of values under comparison, the unit (*LVal).Equal
// tracks to bound a comparison of two cyclic values.
type valuePair struct {
	a, b *LVal
}

// pairGuard is cycleGuard for a walk over two graphs at once.  Equality
// recurses into a pair of values, so what repeats when both operands are
// cyclic is a pair, not a value: a comparison can revisit a without revisiting
// b.
//
// Unlike cycleGuard its path set is never unwound, and keeping a pair on it
// forever is not an approximation.  A pair reached a second time either is
// still under comparison further up the path, where taking it to be equal is
// the co-inductive answer Equal documents, or has already been compared and
// found equal -- a pair that compared unequal returned false out of every
// frame up to the root instead of ever being reached again.
type pairGuard struct {
	state *pairState

	depth  int
	strict bool
}

// pairState is cycleState for a comparison.  The path set lives here, shared
// by every frame, for the reason cycleState.path does: a set built in a guard
// copy would be built once per pair sitting at exactly cycleGuardDepth, so a
// comparison that is merely wide there would allocate per node.
type pairState struct {
	path map[valuePair]struct{}

	cyclic bool
}

// strictPairGuard returns the guard for the rerun of a comparison that stage 2
// abandoned.
func strictPairGuard() pairGuard {
	return pairGuard{state: new(pairState), strict: true}
}

// descend returns the guard for a comparison one level below g, entering the
// pair (a, b), and reports whether the caller must stop -- because the pair is
// already on the path, or because another frame has found a cycle and the
// whole comparison is being unwound for a rerun in strict mode.  Both answers
// are "return equal and do not recurse": in the first case that is the
// co-inductive answer, in the second the result is discarded.
//
// Only a comparison that is about to recurse calls this.  A pair of leaves
// reaches nothing, so putting it on the path would tax every int and string
// comparison to bound a walk that cannot recurse.
func (g pairGuard) descend(a, b *LVal) (pairGuard, bool) {
	if !g.strict {
		if g.state.cyclic {
			return g, true
		}
		g.depth++
		probeRecord("pair", g.depth, &probeMaxPairDepth, &probePairDescends)
		if g.depth < cycleGuardDepth {
			return g, false
		}
	}
	p := valuePair{a, b}
	if g.state.path == nil {
		g.state.path = make(map[valuePair]struct{}, cycleGuardDepth)
	} else if _, ok := g.state.path[p]; ok {
		g.state.cyclic = true
		return g, true
	}
	g.state.path[p] = struct{}{}
	return g, false
}
