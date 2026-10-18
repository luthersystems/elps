// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// operandDebugger is an enabled debugger that records the expressions OnEval
// reports and never pauses.
type operandDebugger struct{ seen []*LVal }

func (d *operandDebugger) IsEnabled() bool { return true }
func (d *operandDebugger) OnEval(_ *LEnv, expr *LVal) bool {
	d.seen = append(d.seen, expr)
	return false
}
func (d *operandDebugger) WaitIfPaused(*LEnv, *LVal) DebugAction { return DebugContinue }
func (d *operandDebugger) OnFunEntry(*LEnv, *LVal, *LEnv)        {}
func (d *operandDebugger) OnFunReturn(*LEnv, *LVal, *LVal)       {}
func (d *operandDebugger) AfterFunCall(*LEnv) bool               { return false }
func (d *operandDebugger) OnError(*LEnv, *LVal) bool             { return false }

// operandOutcome is what the evaluation of one operand leaves behind.
type operandOutcome struct {
	loc     *token.Location
	errLoc  token.Location
	result  string
	steps   int64
	nesting int
	evals   int
	same    bool
}

// TestEvalOperandMatchesEval: evalOperand, which evaluates the elements of a
// call form, gives what eval gives for every kind of operand and under every
// limit: the value, the error and its location, the step count, the location
// register, the nesting count and the debugger's hooks.
func TestEvalOperandMatchesEval(t *testing.T) {
	src := &token.Location{File: "operand.lisp", Line: 3, Col: 5}
	operands := []struct {
		make func() *LVal
		name string
	}{
		{func() *LVal { return Int(7) }, "int"},
		{func() *LVal { return String("s") }, "string"},
		{func() *LVal { return QExpr(nil) }, "nil"},
		{func() *LVal { return SExpr(nil) }, "empty-call"},
		{func() *LVal { return Symbol("opnd-local") }, "local"},
		{func() *LVal { return Symbol("opnd-global") }, "global"},
		{func() *LVal { return Symbol("opnd-fun") }, "local-function"},
		{func() *LVal { return Symbol("user:opnd-global") }, "qualified"},
		{func() *LVal { return Symbol("lisp:car") }, "qualified-function"},
		{func() *LVal { return Symbol("lisp:true") }, "qualified-true"},
		{func() *LVal { return Symbol(TrueSymbol) }, "true"},
		{func() *LVal { return Symbol("opnd-nope") }, "unbound"},
		{func() *LVal { return Symbol("user:opnd-nope") }, "unbound-qualified"},
		{func() *LVal { return Symbol("opnd-pkg:x") }, "unknown-package"},
		{func() *LVal { return Symbol("a:b:c") }, "illegal"},
		{func() *LVal { return Symbol(":k") }, "keyword"},
		{func() *LVal { return Quote(Symbol("opnd-global")) }, "quoted-symbol"},
		{func() *LVal { return Quote(SExpr([]*LVal{Symbol("car")})) }, "quoted-list"},
		{func() *LVal { return Quote(Quote(Symbol("opnd-global"))) }, "quoted-quote"},
		{func() *LVal { return &LVal{Type: LQuote, Cells: []*LVal{Symbol("opnd-global")}} }, "quote"},
		{func() *LVal { return SExpr([]*LVal{Symbol("identity"), Int(1)}) }, "call"},
		{func() *LVal { return Splice(Symbol("opnd-global")) }, "spliced"},
	}
	limits := []struct {
		set  func(env *LEnv) context.Context
		name string
	}{
		{func(*LEnv) context.Context { return nil }, "none"},
		{func(env *LEnv) context.Context {
			env.Runtime.maxSteps = 100
			return nil
		}, "counting"},
		{func(env *LEnv) context.Context {
			env.Runtime.maxSteps, env.Runtime.steps = 4, 4
			return nil
		}, "step-limit"},
		{func(env *LEnv) context.Context {
			env.Runtime.SetStepBudget(1)
			env.Runtime.stepBudgetUsed = 1
			return nil
		}, "step-budget"},
		{func(*LEnv) context.Context {
			ctx, cancel := context.WithCancel(context.Background())
			cancel()
			return ctx
		}, "cancelled"},
		{func(env *LEnv) context.Context {
			env.Runtime.MaxEvalNesting, env.Runtime.evalNesting = 3, 2
			env.Runtime.resolveEvalNestingLimit()
			return nil
		}, "below-nesting-limit"},
		{func(env *LEnv) context.Context {
			env.Runtime.MaxEvalNesting, env.Runtime.evalNesting = 3, 3
			env.Runtime.resolveEvalNestingLimit()
			return nil
		}, "at-nesting-limit"},
		{func(env *LEnv) context.Context {
			// Written after the limit was resolved: the cached limit is stale.
			env.Runtime.MaxEvalNesting, env.Runtime.evalNesting = 3, 3
			return nil
		}, "written-nesting-limit"},
		{func(env *LEnv) context.Context {
			env.Runtime.Debugger = &operandDebugger{}
			return nil
		}, "debugger"},
	}
	run := func(t *testing.T, operand func() *LVal, limit func(*LEnv) context.Context, fast bool) operandOutcome {
		t.Helper()
		env := initSafetyTestEnv(t)
		require.False(t, env.PutGlobal(Symbol("opnd-global"), Int(42)).IsError())
		inner := NewEnv(env)
		require.False(t, inner.Put(Symbol("opnd-local"), String("local")).IsError())
		require.False(t, inner.Put(Symbol("opnd-fun"), inner.Lambda(Formals(), []*LVal{Int(1)})).IsError())
		// A runtime resolves its nesting limit on its first eval; until
		// then evalOperand leaves every operand to eval.
		env.Runtime.resolveEvalNestingLimit()
		ctx := limit(inner)
		x := operand()
		x.source = src
		nesting := env.Runtime.evalNesting
		var got *LVal
		if fast {
			got = inner.evalOperand(ctx, x)
		} else {
			got = inner.eval(ctx, x)
		}
		out := operandOutcome{
			result:  got.String(),
			same:    got == x,
			steps:   env.Runtime.steps,
			loc:     inner.loc,
			nesting: env.Runtime.evalNesting - nesting,
		}
		if got.Type == LError {
			out.errLoc, _ = got.Source()
		}
		if d, ok := env.Runtime.Debugger.(*operandDebugger); ok {
			out.evals = len(d.seen)
		}
		return out
	}
	for _, o := range operands {
		for _, l := range limits {
			t.Run(o.name+"/"+l.name, func(t *testing.T) {
				assert.Equal(t, run(t, o.make, l.set, false), run(t, o.make, l.set, true))
			})
		}
	}
}

// TestEvalOperandFastPath: evalOperand evaluates a leaf itself, and leaves to
// eval exactly the operands and runtime states operandNeedsEval names.
func TestEvalOperandFastPath(t *testing.T) {
	env := initSafetyTestEnv(t)
	r := env.Runtime
	require.True(t, operandNeedsEval(r, Int(1)), "an unresolved nesting limit")
	r.resolveEvalNestingLimit()
	for _, x := range []*LVal{Int(1), String("s"), Symbol("x"), Symbol("user:x"), Symbol(":k"), QExpr(nil), Quote(Symbol("x")), Quote(SExpr([]*LVal{Symbol("car")})), Quote(Quote(Symbol("x")))} {
		assert.False(t, operandNeedsEval(r, x), "%v", x)
	}
	unquoted := &LVal{Type: LQuote, Cells: []*LVal{Symbol("x")}}
	for _, x := range []*LVal{SExpr([]*LVal{Symbol("car")}), SExpr(nil), unquoted, Splice(Symbol("x"))} {
		assert.True(t, operandNeedsEval(r, x), "%v", x)
	}
	leaf := Int(1)
	r.Debugger = &operandDebugger{}
	assert.True(t, operandNeedsEval(r, leaf), "a debugger")
	r.Debugger = nil
	r.MaxEvalNesting = 3
	assert.True(t, operandNeedsEval(r, leaf), "a stale nesting limit")
	r.resolveEvalNestingLimit()
	r.evalNesting = 2
	assert.False(t, operandNeedsEval(r, leaf), "below the nesting limit")
	r.evalNesting = 3
	assert.True(t, operandNeedsEval(r, leaf), "at the nesting limit")
	r.evalNesting = 0
}

// TestPackageGetName: getName, the lookup a qualified symbol takes, returns
// what Get returns for the same name: the bound value, true and false, and
// the unbound-symbol error.
func TestPackageGetName(t *testing.T) {
	env := initSafetyTestEnv(t)
	pkg := env.Runtime.Package
	value := Int(42)
	pkg.Put(Symbol("pkg-get-name"), value)
	assert.Same(t, value, pkg.getName("pkg-get-name"))
	for _, name := range []string{"pkg-get-name", TrueSymbol, FalseSymbol, "pkg-get-unbound", ""} {
		want, got := pkg.Get(Symbol(name)), pkg.getName(name)
		assert.Equal(t, want.String(), got.String(), name)
		assert.Equal(t, want.Type, got.Type, name)
	}
	got := env.Eval(Symbol("user:pkg-get-unbound"))
	require.Equal(t, LError, got.Type)
	assert.Equal(t, "<native code>: unbound symbol: pkg-get-unbound", got.String())
}

// TestPackageGetNameFillsTemplateBinding: a qualified read of a binding a
// template VM has not built yet builds it, as an unqualified read does.
func TestPackageGetNameFillsTemplateBinding(t *testing.T) {
	vm, err := lazyFixture(t).NewVM()
	require.NoError(t, err)
	lib := vm.Runtime.Registry.Package("lib")
	i, ok := lib.base.index.Lookup("v5")
	require.True(t, ok)
	built := &lazyInstanceOf(vm).values[lib.base.bindings[i].value.index-1]
	require.Nil(t, *built, "lib:v5 is built before it is read")
	got := vm.Eval(Symbol("lib:v5"))
	require.Equal(t, LSExpr, got.Type, "%v", got)
	assert.Equal(t, 5, got.Cells[0].Int)
	assert.Same(t, got, *built)
	v, _ := lib.Symbol("v5")
	assert.Same(t, got, v)
}
