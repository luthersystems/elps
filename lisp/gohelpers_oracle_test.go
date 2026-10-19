// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"slices"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The language builtins the LEnv sequence helpers return what they return,
// called as the oracle.
var (
	refKeys   = lisp.BuiltinFunc("keys")
	refLength = lisp.BuiltinFunc("length")
	refEmpty  = lisp.BuiltinFunc("empty?")
	refFirst  = lisp.BuiltinFunc("first")
	refSecond = lisp.BuiltinFunc("second")
)

// seqHelpers pairs each helper with its builtin.  samePtr says whether the
// helper must return the very value the builtin returns.
var seqHelpers = []struct {
	name    string
	builtin lisp.BuiltinRef
	fn      func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
	samePtr bool
}{
	{"keys", refKeys, (*lisp.LEnv).MapKeys, false},
	{"length", refLength, func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
		n, lerr := env.Length(v)
		if lerr != nil {
			return lerr
		}
		return lisp.Int(n)
	}, false},
	{"empty?", refEmpty, func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
		empty, lerr := env.IsEmpty(v)
		if lerr != nil {
			return lerr
		}
		return lisp.Bool(empty)
	}, false},
	{"first", refFirst, (*lisp.LEnv).First, true},
	{"second", refSecond, (*lisp.LEnv).Second, true},
}

// TestSeqHelpersMatchBuiltins holds MapKeys, Length, IsEmpty, First and
// Second to keys, length, empty?, first and second on an input of every type:
// the same value (or the same element), or the same error condition.
func TestSeqHelpersMatchBuiltins(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for _, h := range seqHelpers {
		for _, in := range oracle.Values() {
			want := env.CallBuiltin(h.builtin, in.V)
			got := h.fn(env, in.V)
			if h.samePtr {
				oracle.SameObject(t, h.name+"/"+in.Label, got, want)
				// An error value stored in the sequence is returned as it is,
				// not raised again.
				if cells, ok := in.V.SeqCells(); ok && want.IsError() && slices.Contains(cells, want) {
					assert.Same(t, want, got, "%s/%s: the stored error value", h.name, in.Label)
				}
			} else {
				oracle.Same(t, h.name+"/"+in.Label, got, want)
			}
		}
	}
}

// TestSeqHelpersAllocLimit holds MapKeys to keys under a small allocation
// limit: a map over it raises the same condition.
func TestSeqHelpersAllocLimit(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	env.Runtime.MaxAlloc = 1
	for _, in := range oracle.Values() {
		oracle.Same(t, "keys/"+in.Label, env.MapKeys(in.V), env.CallBuiltin(refKeys, in.V))
	}
}

// TestSeqHelpersCancelled holds each helper to its builtin at every context
// check: each makes its one check before anything else.
func TestSeqHelpersCancelled(t *testing.T) {
	// Each run gets its own input: a value belongs to one runtime.
	for _, h := range seqHelpers {
		for i, in := range oracle.Values() {
			oracle.Sweep(t, h.name+"/"+in.Label,
				func(env *lisp.LEnv) *lisp.LVal { return h.fn(env, oracle.Values()[i].V) },
				func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(h.builtin, oracle.Values()[i].V) })
		}
	}
}

// inEval runs f inside a Go builtin called from Lisp, as a builtin's own Go
// code runs.  funcall and apply need the evaluator's stack.  The builtin
// go-run is registered once per environment and runs the f of its latest
// inEval.
func inEval(t *testing.T, env *lisp.LEnv, f func(env *lisp.LEnv) *lisp.LVal) *lisp.LVal {
	t.Helper()
	slot, ok := goRunSlots[env]
	if !ok {
		slot = new(func(env *lisp.LEnv) *lisp.LVal)
		goRunSlots[env] = slot
		env.AddBuiltins(false, &testBuiltinDef{name: "go-run", formals: lisp.Formals(),
			fn: func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return (*slot)(env) }})
		t.Cleanup(func() { delete(goRunSlots, env) })
	}
	*slot = f
	return env.LoadString("run", "(go-run)")
}

// goRunSlots holds the f each environment's go-run runs.  Tests that use
// inEval do not run in parallel.
var goRunSlots = map[*lisp.LEnv]*func(env *lisp.LEnv) *lisp.LVal{}

// TestCallFunctionMatchesFuncall holds CallFunction and ApplyFunction to
// funcall and apply for a function argument of every type, a symbol bound to
// a function, an unbound symbol, a symbol bound to a value, a macro and a
// special operator, and for an apply argument list of every type.
func TestCallFunctionMatchesFuncall(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", `(defun two (x) (list x x)) (set 'not-a-fun 3)`)
	require.False(t, rc.IsError(), "%v", rc)
	fns := oracle.Values()
	fns = append(fns,
		oracle.Value{V: lisp.Symbol("two"), Label: "symbol-fun"},
		oracle.Value{V: lisp.Symbol("no-such-fun"), Label: "symbol-unbound"},
		oracle.Value{V: lisp.Symbol("not-a-fun"), Label: "symbol-value"},
		oracle.Value{V: lisp.Symbol("defun"), Label: "macro"},
		oracle.Value{V: lisp.Symbol("if"), Label: "special-op"},
		oracle.Value{V: env.GetGlobal(lisp.Symbol("two")), Label: "lambda"},
	)
	arg := lisp.Int(7)
	run := func(f func(env *lisp.LEnv) *lisp.LVal) *lisp.LVal { return inEval(t, env, f) }
	for _, fn := range fns {
		oracle.Same(t, "funcall/"+fn.Label,
			run(func(env *lisp.LEnv) *lisp.LVal { return env.CallFunction(fn.V, arg) }),
			run(func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refFuncall, fn.V, arg) }))
		list := lisp.SExpr([]*lisp.LVal{arg})
		oracle.Same(t, "apply/"+fn.Label,
			run(func(env *lisp.LEnv) *lisp.LVal { return env.ApplyFunction(fn.V, list) }),
			run(func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refApply, fn.V, list) }))
	}
	two := lisp.Symbol("two")
	for _, args := range oracle.Values() {
		oracle.Same(t, "apply-args/"+args.Label,
			run(func(env *lisp.LEnv) *lisp.LVal { return env.ApplyFunction(two, args.V) }),
			run(func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refApply, two, args.V) }))
	}
}

// TestCallFunctionDoesNotWriteArgs checks that ApplyFunction leaves the
// caller's list as it was when the callee changes its &rest list.
func TestCallFunctionDoesNotWriteArgs(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", `(defun clobber (&rest xs) (set! xs (cons 0 xs)) xs)`)
	require.False(t, rc.IsError(), "%v", rc)
	list := lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	v := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return env.ApplyFunction(lisp.Symbol("clobber"), list) })
	require.False(t, v.IsError(), "%v", v)
	assert.Equal(t, "'(0 1 2)", v.String())
	assert.Equal(t, "(1 2)", list.String())
	// A &rest list the callee keeps must not follow later writes to the
	// caller's slice.
	rc = env.LoadString("keep", `(defun keep (&rest xs) xs)`)
	require.False(t, rc.IsError(), "%v", rc)
	buf := []*lisp.LVal{lisp.Int(1), lisp.Int(2)}
	kept := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return env.CallFunction(lisp.Symbol("keep"), buf...) })
	require.False(t, kept.IsError(), "%v", kept)
	buf[0] = lisp.Int(9)
	assert.Equal(t, "'(1 2)", kept.String())
	list = lisp.SExpr(buf)
	kept = inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return env.ApplyFunction(lisp.Symbol("keep"), list) })
	require.False(t, kept.IsError(), "%v", kept)
	list.Cells[1] = lisp.Int(8)
	assert.Equal(t, "'(9 2)", kept.String())
}

// ApplyFunction refuses a Go nil argument list with apply's error instead of
// panicking.
func TestApplyFunctionNilArgs(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	v := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return env.ApplyFunction(lisp.Symbol("list"), nil) })
	require.True(t, v.IsError())
	assert.Contains(t, v.String(), "last argument is not a list")
}

// CallFunction called while the calling builtin's frame is marked terminal,
// as funcall and apply mark their own frame before they call, runs a callee
// whose last form calls that same builtin: the call is not dropped as a tail
// call through Go, and the frame's mark is restored after.
func TestCallFunctionFromTerminalFrame(t *testing.T) {
	env := newLimitTestEnv(t)
	calls := 0
	env.AddBuiltins(false, &testBuiltinDef{name: "go-call-f", formals: lisp.Formals(),
		fn: func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			calls++
			if calls > 3 {
				return lisp.String("done")
			}
			env.Runtime.Stack.Top().Terminal = true
			v := env.CallFunction(lisp.Symbol("f"))
			if !env.Runtime.Stack.Top().Terminal {
				return env.Errorf("the frame's terminal mark was not restored")
			}
			env.Runtime.Stack.Top().Terminal = false
			if v.Type != lisp.LString && !v.IsError() {
				return env.Errorf("a %v came back to the builtin", v.Type)
			}
			return v
		}})
	v := env.LoadString("test", `(defun f () (go-call-f)) (f)`)
	require.False(t, v.IsError(), "%v", v)
	assert.Equal(t, `"done"`, v.String())
	assert.Equal(t, 4, calls)
}

// TestCallFunctionCancelled holds CallFunction and ApplyFunction to funcall
// and apply at every context check of a load that defines a function and
// calls it from a Go builtin.
func TestCallFunctionCancelled(t *testing.T) {
	// Each run builds its own values: a value belongs to one runtime.
	two := func() *lisp.LVal { return lisp.Symbol("two") }
	arg := func() *lisp.LVal { return lisp.Int(7) }
	load := func(f func(env *lisp.LEnv) *lisp.LVal) func(env *lisp.LEnv) *lisp.LVal {
		return func(env *lisp.LEnv) *lisp.LVal {
			if rc := env.LoadString("setup", `(defun two (x) (list x (+ x 1)))`); rc.IsError() {
				return rc
			}
			return inEval(t, env, f)
		}
	}
	oracle.Sweep(t, "funcall",
		load(func(env *lisp.LEnv) *lisp.LVal { return env.CallFunction(two(), arg()) }),
		load(func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refFuncall, two(), arg()) }))
	oracle.Sweep(t, "apply",
		load(func(env *lisp.LEnv) *lisp.LVal { return env.ApplyFunction(two(), lisp.SExpr([]*lisp.LVal{arg()})) }),
		load(func(env *lisp.LEnv) *lisp.LVal {
			return env.CallBuiltin(refApply, two(), lisp.SExpr([]*lisp.LVal{arg()}))
		}))
}

// TestCallFunctionTailCallsCaller calls, from a Go builtin, functions that
// grow the call stack and then tail-call that same builtin
// (luthersystems/elps#852).  Every call must run: no tail-call marker comes
// back to the builtin and no call is dropped.
func TestCallFunctionTailCallsCaller(t *testing.T) {
	for _, via := range []string{"call", "apply"} {
		t.Run(via, func(t *testing.T) {
			env := newLimitTestEnv(t)
			env.AddBuiltins(false, &testBuiltinDef{name: "run-hooks", formals: lisp.Formals(lisp.VarArgSymbol, "fs"),
				fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
					last := lisp.Nil()
					for _, f := range args.Cells {
						if via == "call" {
							last = env.CallFunction(f)
						} else {
							last = env.ApplyFunction(f, lisp.Nil())
						}
						if last.IsError() {
							return last
						}
						if last.Type != lisp.LInt && last.Type != lisp.LSymbol && !last.IsNil() {
							return env.Errorf("a %v came back to the builtin", last.Type)
						}
					}
					return last
				}})
			v := env.LoadString("test", `
(set 'ran 0)
(defun deep (n) (if (> n 0) (+ 1 (deep (- n 1))) 0))
(defun mark () (set 'ran (+ ran 1)) 'marked)
(run-hooks
  (lambda () (deep 2000) (run-hooks mark))
  (lambda () (deep 10) (run-hooks mark)))
ran`)
			require.False(t, v.IsError(), "%v", v)
			assert.Equal(t, "2", v.String())
		})
	}
}
