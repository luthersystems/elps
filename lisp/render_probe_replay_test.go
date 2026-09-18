// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// The cycle probe records the panic a malformed node raised on its first visit
// and replays it on a cache hit, so that the error whose message the node was
// part of still skips its remaining siblings. The containment for that replay
// is errorMessage's recover, which is a frame on the Go stack and not a
// property of the node: the same (value, depth) node can be reached again from
// a parent that is not an error, and there the replay had nothing to catch it.
//
// No in-tree value reaches that arm -- it needs a node that panics inside
// errorMessage but not inside nativeErrorText, reached twice at one depth --
// so this pins the bookkeeping directly: the replay happens under an
// errorMessage frame and does not happen outside one.
func TestRenderProbeReplaysPanicOnlyUnderErrorMessage(t *testing.T) {
	v := QExpr([]*LVal{Int(1)})
	const boom = "malformed descendant"
	probe := func() *renderCycleProbe {
		p := &renderCycleProbe{nodes: make(map[renderProbeNode]*renderProbeVisit)}
		// container increments the guard's depth before keying the node, so a
		// root-level visit records depth 1.
		p.nodes[renderProbeNode{value: v, depth: 1}] = &renderProbeVisit{panicVal: boom}
		return p
	}
	visit := func(r *valueRenderer) (recovered any) {
		defer func() { recovered = recover() }()
		var st cycleState
		r.container(v, false, cycleGuard{state: &st})
		return nil
	}

	off := valueRenderer{limit: -1, budget: newRenderBudget(-1, nil), probe: probe()}
	if recovered := visit(&off); recovered != nil {
		t.Fatalf("replayed a recorded panic with no errorMessage frame to contain it: %v", recovered)
	}

	under := valueRenderer{limit: -1, budget: newRenderBudget(-1, nil), probe: probe(), messageDepth: 1}
	if recovered := visit(&under); recovered != boom {
		t.Fatalf("recorded panic was not replayed under an errorMessage frame: got %v", recovered)
	}
}

// The same thing with a crafted value, driven through the probe's own entry
// point: a malformed node shared by an error's message and an ordinary list at
// the SAME depth. The first visit panics inside errorMessage, which contains
// it and records it; the second reaches the memo from a parent that is not an
// error, where the replay used to leave probeRenderCycles -- and boundedRender
// -- with nothing to catch it.
//
// Reaching this through String() as well would need boundedRender's fallback
// path, whose entry condition is a first pass that fills its budget and a
// strict pass that does not; that combination is a cycle-shaped one and is
// what "not reproduced in-tree" refers to.
func TestRenderProbeSurvivesSharedMalformedChild(t *testing.T) {
	// A zero-dimension array: nested's LArray arm reads Cells[0] and panics.
	// Cells[0] is a string, so nativeErrorText declines it without panicking
	// and the panic is raised in the message body proper.
	bad := &LVal{Type: LArray, Cells: []*LVal{}}
	failing := &LVal{Type: LError, Str: "error", Cells: []*LVal{String("boom"), bad}}
	v := QExpr([]*LVal{failing, QExpr([]*LVal{bad})})

	budget := newRenderBudget(1<<20, nil)
	probe, cyclic := probeRenderCycles(v, &budget, false)
	if cyclic {
		t.Fatal("acyclic value reported as cyclic")
	}
	if len(probe.recovered) == 0 {
		t.Fatal("probe recorded no recovered message; the value stopped exercising the arm")
	}
}

// messageDepth must describe the Go stack, so errorMessage has to restore it
// on the panicking exit as well as the ordinary one.
func TestRenderMessageDepthUnwinds(t *testing.T) {
	r := valueRenderer{limit: -1, budget: newRenderBudget(-1, nil)}
	e := (*ErrorVal)(&LVal{Type: LError, Str: "error", Cells: []*LVal{String("boom")}})
	var st cycleState
	r.errorMessage(e, cycleGuard{state: &st})
	if r.messageDepth != 0 {
		t.Fatalf("messageDepth %d after an ordinary message", r.messageDepth)
	}

	// A malformed cell AFTER the first one panics inside the message body:
	// nativeErrorText only inspects Cells[0] and recovers its own panics, so
	// this is what reaches errorMessage's recover -- the exit that must still
	// unwind the counter.
	panicking := &LVal{Type: LError, Str: "error", Cells: []*LVal{String("boom"), {Type: LArray, Cells: []*LVal{}}}}
	r = valueRenderer{limit: -1, budget: newRenderBudget(-1, nil)}
	var st2 cycleState
	r.errorMessage((*ErrorVal)(panicking), cycleGuard{state: &st2})
	if r.messageDepth != 0 {
		t.Fatalf("messageDepth %d after a recovered message", r.messageDepth)
	}
}
