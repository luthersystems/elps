// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"

	"github.com/luthersystems/elps/lisp"
)

// Iterator work budget (issue #722).
//
// An iterator step ('*) runs the rest of its path once per element, and a
// document can share one subtree along many paths: (set! v (vector v v))
// repeated n times costs n steps to build and has 2^n paths, and n iterator
// steps over it enumerate every one of them.  The validation and copy memos
// (cycle.go, lisp/sharing.go) cannot remove that work.  The output of ? and
// ?set really is 2^n cells, and ?del! on a shared item is not idempotent --
// deleting the same index twice removes two elements -- so answering a
// repeat visit from a memo would change what the program sees.  A second
// shape is linear in its output but still unmetered: ?set over n references
// to one W-wide map must build n fresh maps, n*W entries, in one step.
//
// So an operation that iterates COUNTS the work its iterators do and pays
// for it, without changing what it computes:
//
//   - one unit per element an iterator visits, and one per result a nested
//     iterator's results add to the flattened answer;
//   - for copies made under an iterator, one unit per container copied and
//     one per cell or map entry it holds -- the containers the path rebuilds
//     (the off-path copy of each map or sequence on the path) and those
//     copied whole below them;
//   - for the sequence operations that shift or splice a whole sequence
//     under an iterator -- an index or range delete, a range set or nil --
//     one unit per cell of the sequence, plus the cells a range set splices
//     in.
//
// Work outside every iterator is not counted: a path without '* visits at
// most one element per step, and its copies are linear in the document the
// program built.
//
// The count drives two things.  Every iterPollGrain units the operation
// polls the evaluation's context, which never changes a step count and
// stops a walk whose deadline has passed.  And the work beyond
// iterWorkAllowance is charged in evaluation steps (lisp.LEnv.ChargeSteps),
// one step per unit, so a step budget bounds it too.  The charge is a
// function of the document and the path alone, so it is deterministic.
//
// When the context is done or the budget is spent, the operation stops with
// the condition the evaluator itself raises (context-cancelled,
// step-limit-exceeded or step-budget-exceeded).  That stop is NOT an element
// failure: the iterator's per-element error handling -- nil for a failed
// read, the element unchanged for a failed write -- does not swallow it.  A
// mutating operation stopped part way leaves the elements it already
// reached mutated, as any error part way through a mutating loop does.

// iterWorkAllowance is the iterator work one operation may do before it is
// charged in steps.  It is quasiquote's quasiquoteRebuildAllowance (#716):
// a million units is far past what an ordinary query does -- a '* over a
// hundred thousand records costs a hundred thousand units, and a copying
// ?set over the same records a few units per field -- so every such query
// keeps its exact step count, which a consensus embedder relies on; and it
// bounds the uncharged work to the order of what one builtin step may
// already do.  It is per operation, so it is not a budget a program can
// spend across calls.
const iterWorkAllowance = 1 << 20

// iterPollGrain is how much iterator work passes between context polls.  It
// is the kernel's sharedWalkBudget, the grain equalIter polls at: a poll is
// cheap next to that much work, and a deadline is noticed within
// microseconds.
const iterPollGrain = sharedWalkBudget

// errIterStopped is what the walk returns once the operation has been
// stopped by its context or its step budget.  The condition itself is kept
// on the copyOp (copyOp.stop), and the builtins return that; the sentinel
// only unwinds the walk.
var errIterStopped = errors.New("elpspath: operation stopped")

// newQueryOp is the state of one elpspath operation run on behalf of env:
// its value depth limit, its copy memo, and its iterator work budget.
func newQueryOp(env *lisp.LEnv) *copyOp {
	op := newCopyOp(env.Runtime.ValueDepthLimit())
	op.env = env
	return op
}

// enterIter and leaveIter bracket an iterator's loop: work is counted only
// while at least one iterator is running.  Both accept a nil op, the
// exported Path API, which counts nothing.
func (op *copyOp) enterIter() {
	if op != nil {
		op.iterDepth++
	}
}

func (op *copyOp) leaveIter() {
	if op != nil {
		op.iterDepth--
	}
}

// stopped reports whether the operation has been stopped (see
// errIterStopped).  An iterator checks it before treating an element's
// error as that element's failure.
func (op *copyOp) stopped() bool {
	return op != nil && op.stop != nil
}

// charge counts n units of work if an iterator is running, and reports
// errIterStopped once the operation may not continue.
func (op *copyOp) charge(n int) error {
	if op == nil || op.iterDepth == 0 {
		return nil
	}
	return op.chargeIter(n)
}

// chargeWidth charges the number of cells in the sequence seq (see the
// sequence operations above); anything else charges nothing, and the
// operation reports its own error for it.
func (op *copyOp) chargeWidth(seq *lisp.LVal) error {
	if op == nil || op.iterDepth == 0 {
		return nil
	}
	switch seq.Type {
	case lisp.LSExpr:
		return op.chargeIter(len(seq.Cells))
	case lisp.LArray:
		if len(seq.Cells) == 2 {
			return op.chargeIter(len(seq.Cells[1].Cells))
		}
	}
	return nil
}

// chargeIter is charge with an iterator known to be running.
func (op *copyOp) chargeIter(n int) error {
	if op.stop != nil {
		return errIterStopped
	}
	before := op.iterWork
	op.iterWork += n
	env := op.env
	if env == nil {
		return nil
	}
	// The same arithmetic as quasiquote's chargeDuplicated: only the part
	// of this charge past the allowance costs steps.
	if excess := op.iterWork - max(before, iterWorkAllowance); excess > 0 {
		// ChargeSteps also polls the context when there is one.
		if lerr := env.ChargeSteps(int64(excess)); lerr.Type == lisp.LError {
			op.stop = lerr
			return errIterStopped
		}
		return nil
	}
	if op.iterWork/iterPollGrain != before/iterPollGrain {
		if err := env.Context().Err(); err != nil {
			op.stop = env.ErrorConditionf(lisp.CondContextCancelled, "context cancelled: %v", err)
			return errIterStopped
		}
	}
	return nil
}

// opResult is what a builtin returns for the result of an operation run
// under op: the stop condition itself whenever the operation was stopped --
// even if the walk that noticed it went on to return a value -- an ordinary
// error for err, and data otherwise.
func opResult(env *lisp.LEnv, op *copyOp, data *lisp.LVal, err error) *lisp.LVal {
	if op.stopped() {
		return op.stop
	}
	if err != nil {
		return env.Error(err)
	}
	return data
}

// getPath, setMutatePath, deleteMutatePath and nilMutatePath are the four
// operations that do not copy, run as part of op so that an iterator inside
// the path counts its work.  A nil op, or a Path from outside this package,
// runs the exported method unchanged.
func getPath(p Path, in *lisp.LVal, op *copyOp) (*lisp.LVal, error) {
	switch p := p.(type) {
	case *rootPath:
		return getPath(p.path, in, op)
	case *chainPath:
		return p.getOp(in, op)
	case *iterPath:
		return p.getOp(in, op)
	}
	return p.Get(in)
}

func setMutatePath(p Path, in *lisp.LVal, newIn *lisp.LVal, op *copyOp) (*lisp.LVal, error) {
	switch p := p.(type) {
	case *rootPath:
		return setMutatePath(p.path, in, newIn, op)
	case *chainPath:
		return p.setMutateOp(in, newIn, op)
	case *iterPath:
		return p.setMutateOp(in, newIn, op)
	case *rangePath:
		if err := op.chargeWidth(in); err != nil {
			return nil, err
		}
		if err := op.chargeWidth(newIn); err != nil {
			return nil, err
		}
	}
	return p.SetMutate(in, newIn)
}

func deleteMutatePath(p Path, in *lisp.LVal, op *copyOp) (*lisp.LVal, error) {
	switch p := p.(type) {
	case *rootPath:
		return deleteMutatePath(p.path, in, op)
	case *chainPath:
		return p.deleteMutateOp(in, op)
	case *iterPath:
		return p.deleteMutateOp(in, op)
	case *indexPath, *rangePath:
		if err := op.chargeWidth(in); err != nil {
			return nil, err
		}
	}
	return p.DeleteMutate(in)
}

func nilMutatePath(p Path, in *lisp.LVal, op *copyOp) (*lisp.LVal, error) {
	switch p := p.(type) {
	case *rootPath:
		return nilMutatePath(p.path, in, op)
	case *chainPath:
		return p.nilMutateOp(in, op)
	case *iterPath:
		return p.nilMutateOp(in, op)
	case *rangePath:
		if err := op.chargeWidth(in); err != nil {
			return nil, err
		}
	}
	return p.NilMutate(in)
}

// hasIterStep reports whether a builtin's path steps include an iterator.
// Only such an operation can count any work, so the non-copying builtins
// allocate their operation state only then and every other path runs as it
// always has.
func hasIterStep(steps []*lisp.LVal) bool {
	for _, s := range steps {
		if s.Type == lisp.LSymbol && s.Str == "*" {
			return true
		}
	}
	return false
}
