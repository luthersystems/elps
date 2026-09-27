// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The iterator work budget polls the context well before the allowance
// (budget.go): a deadline that passes inside a builtin stops it within
// iterPollGrain units, without charging a step.  The probe cancels the
// context from inside a builtin -- after the evaluator's own last check --
// and then runs an iterator over far fewer elements than the allowance.
func TestIterBudgetPollsBeforeTheAllowance(t *testing.T) {
	const n = 100_000 // units: well below iterWorkAllowance
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	var op *copyOp
	var got *lisp.LVal
	var stepsBefore, stepsAfter int64
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	env.AddBuiltins(true, elpsutil.Function("probe", lisp.Formals("xs"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		cancel()
		path, err := ArgsToPath([]*lisp.LVal{lisp.Symbol("*")})
		if err != nil {
			return env.Error(err)
		}
		op = newQueryOp(env)
		stepsBefore = env.Runtime.Steps()
		_, err = getPath(path, args.Cells[0], op)
		stepsAfter = env.Runtime.Steps()
		if err != nil {
			got = opResult(env, op, nil, err)
			return got
		}
		return lisp.Symbol("finished")
	}))
	rc := env.LoadStringContext(ctx, "probe.lisp", `(probe (make-sequence 0 100000))`)
	if got == nil {
		t.Fatalf("the iterator ran to completion under a cancelled context: %v", rc)
	}
	if got.Type != lisp.LError || got.Str != lisp.CondContextCancelled {
		t.Fatalf("got %v, want the context-cancelled condition", got)
	}
	if op.iterWork > iterPollGrain || op.iterWork >= n {
		t.Fatalf("stopped after %d units, want at most %d", op.iterWork, iterPollGrain)
	}
	if stepsAfter != stepsBefore {
		t.Fatalf("the poll charged %d steps", stepsAfter-stepsBefore)
	}
}

// The exported Path API has no environment, so it counts nothing and never
// stops: an embedder calling Get directly gets the answer it always did.
func TestIterBudgetExportedPathCountsNothing(t *testing.T) {
	cells := make([]*lisp.LVal, iterWorkAllowance+10)
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	path, err := ArgsToPath([]*lisp.LVal{lisp.Symbol("*")})
	if err != nil {
		t.Fatal(err)
	}
	out, err := path.Get(lisp.QExpr(cells))
	if err != nil {
		t.Fatal(err)
	}
	if len(out.Cells) != len(cells) {
		t.Fatalf("got %d elements, want %d", len(out.Cells), len(cells))
	}
}
