// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestHandlerDataRemainsUnevaluated(t *testing.T) {
	for _, data := range []string{
		`(car '(unbound-data))`,
		`(car '((progn (set! ran true) 42)))`,
		`'unbound-data`,
		`''unbound-data`,
		`'(1 2)`,
		`''(1 2)`,
		`(vector 1 2)`,
		`(sorted-map 'amount 17)`,
	} {
		t.Run(data, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("handler-data.lisp", `(set 'ran false)`)))
			want := env.LoadString("handler-data.lisp", data)
			require.NoError(t, lisp.GoError(want))

			result := env.LoadString("handler-data.lisp", fmt.Sprintf(`
				(handler-bind ((condition (lambda (c data) (list c data))))
				  (error 'original %s))`, data))
			assert.Equal(t, "false", env.LoadString("handler-data.lisp", "ran").String(), "condition data must never execute during handler dispatch")
			require.Equal(t, lisp.LSExpr, result.Type, "%v", result)
			require.Len(t, result.Cells, 2)
			assert.Equal(t, "'original", result.Cells[0].String())
			assert.Equal(t, want.Type, result.Cells[1].Type)
			assert.Equal(t, want.String(), result.Cells[1].String(), "condition data and quote depth must be passed unchanged")
		})
	}
}

func TestHandlerDataRethrowPreservesOriginal(t *testing.T) {
	env := newCallSemanticsEnv(t)
	data := env.LoadString("handler-data.lisp", `(sorted-map 'amount 17)`)
	require.NoError(t, lisp.GoError(data))
	var original *lisp.LVal
	env.AddBuiltins(true, elpsutil.Function("raise-original", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			original = env.ErrorCondition("original", data, lisp.String("exact message"))
			return original
		}))

	result := env.LoadString("handler-data.lisp", `
		(handler-bind ((condition (lambda (c data message)
			(assoc! data 'amount 99)
			(rethrow))))
		  (raise-original))`)
	require.Same(t, original, result, "rethrow must retain the original error and stack rather than reconstructing it")
	assert.Equal(t, "original", result.Str)
	assert.Equal(t, `(sorted-map 'amount 17)`, data.String(), "handlers retain the existing private copy of condition data")
	require.Len(t, result.Cells, 2)
	assert.Equal(t, "exact message", result.Cells[1].Str)
	assert.Nil(t, env.Runtime.CurrentCondition(), "handler dispatch must always pop the active condition")
}

func TestHandlerDataRejectsSpecialHandlers(t *testing.T) {
	for _, handler := range []string{"quote", "if", "defun", "42"} {
		t.Run(handler, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("handler-data.lisp", `(set 'ran false)`)))
			result := env.LoadString("handler-data.lisp", fmt.Sprintf(`
				(handler-bind ((condition %s))
				  (error 'original (car '((progn (set! ran true) 42)))))`, handler))
			assert.Equal(t, "false", env.LoadString("handler-data.lisp", "ran").String())
			require.Equal(t, lisp.LError, result.Type)
			if handler == "42" {
				assert.Contains(t, result.String(), "handler not a function")
			} else {
				assert.Contains(t, result.String(), "handler not a regular function")
			}
			assert.False(t, lisp.IsInternalPanic(result))
			assert.Nil(t, env.Runtime.CurrentCondition())
		})
	}
}

func TestHandlerDataPropagatesHandlerErrors(t *testing.T) {
	for _, handler := range []string{
		`(error 'handler-failure "exact failure")`,
		`(lambda (&rest args) (error 'handler-failure "exact failure"))`,
	} {
		t.Run(handler, func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			require.NoError(t, lisp.GoError(env.LoadString("handler-data.lisp", `(set 'ran false)`)))
			result := env.LoadString("handler-data.lisp", fmt.Sprintf(`
				(handler-bind ((condition %s)
				               (condition (lambda (&rest args) (set! ran true))))
				  (error 'original))`, handler))
			assert.Equal(t, "false", env.LoadString("handler-data.lisp", "ran").String(), "a failing selected handler must not invoke later handlers")
			require.Equal(t, lisp.LError, result.Type)
			assert.Equal(t, "handler-failure", result.Str)
			require.Len(t, result.Cells, 1)
			assert.Equal(t, "exact failure", result.Cells[0].Str)
			assert.Nil(t, env.Runtime.CurrentCondition())
		})
	}
}

func TestHandlerDataPanicClearsCurrentCondition(t *testing.T) {
	env := newCallSemanticsEnv(t)
	env.AddBuiltins(true, elpsutil.Function("panic-handler", lisp.Formals(lisp.VarArgSymbol, "args"),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			panic("handler host failure")
		}))
	result := env.LoadString("handler-data.lisp", `(handler-bind ((condition panic-handler)) (error 'original))`)
	assert.True(t, lisp.IsInternalPanic(result), "%v", result)
	assert.Contains(t, result.String(), "handler host failure")
	assert.Nil(t, env.Runtime.CurrentCondition(), "a panicking host handler must still pop its active condition")
}

func TestHandlerDataAllocationLimit(t *testing.T) {
	const limit = 8
	for _, kind := range []string{"list", "vector", "bytes", "map"} {
		for _, size := range []int{limit, limit + 1} {
			for _, nested := range []bool{false, true} {
				t.Run(fmt.Sprintf("%s/%d/nested=%t", kind, size, nested), func(t *testing.T) {
					env := newCallSemanticsEnv(t)
					data := copyAllocationValue(t, kind, size)
					if nested {
						data = lisp.QExpr([]*lisp.LVal{data})
					}
					before := data.String()
					env.AddBuiltins(true, elpsutil.Function("raise-data", lisp.Formals(),
						func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
							return env.ErrorCondition("original", data)
						}))
					env.Runtime.MaxAlloc = limit
					result := env.LoadString("handler-data.lisp", `
						(set 'handled false)
						(handler-bind ((condition (lambda (c data) (set! handled true) data))) (raise-data))`)
					assert.Equal(t, before, data.String(), "dispatch must not mutate original error data")
					assert.False(t, lisp.IsInternalPanic(result))
					if size > limit {
						assert.Equal(t, "false", env.LoadString("handler-data.lisp", "handled").String())
						require.Equal(t, lisp.LError, result.Type)
						assert.Contains(t, result.String(), "allocation size 9 exceeds maximum (8)")
					} else {
						assert.Equal(t, "true", env.LoadString("handler-data.lisp", "handled").String())
						require.NoError(t, lisp.GoError(result))
						assert.Equal(t, before, result.String())
						assert.NotSame(t, data, result)
					}
				})
			}
		}
	}
}

func TestHandlerDataLimitPrecedesNativeClone(t *testing.T) {
	env := newCallSemanticsEnv(t)
	env.Runtime.MaxAlloc = 8
	calls := 0
	data := copyAllocationValue(t, "list", 9)
	data.Cells[0] = lisp.Native(&copyAllocationNativeProbe{calls: &calls})
	env.AddBuiltins(true, elpsutil.Function("raise-data", lisp.Formals(),
		func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return env.ErrorCondition("original", data) }))
	result := env.LoadString("handler-data.lisp", `(handler-bind ((condition list)) (raise-data))`)
	require.Equal(t, lisp.LError, result.Type)
	assert.Contains(t, result.String(), "allocation size 9 exceeds maximum (8)")
	assert.Zero(t, calls, "an oversized cell span must be rejected before cloning its children")
	assert.Nil(t, env.Runtime.CurrentCondition())
}

func TestHandlerDataCancellationBeforeNativeHandler(t *testing.T) {
	env := newCallSemanticsEnv(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	calls := 0
	handler := lisp.Fun("native-handler", lisp.Formals(lisp.VarArgSymbol, "args"),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { calls++; return lisp.Int(42) })
	env.AddBuiltins(true, elpsutil.Function("select-handler", lisp.Formals(),
		func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { cancel(); return handler }))
	result := env.LoadStringContext(ctx, "handler-data.lisp", `(handler-bind ((condition (select-handler))) (error 'original))`)
	require.Equal(t, lisp.LError, result.Type)
	assert.Equal(t, lisp.CondContextCancelled, result.Str)
	assert.Zero(t, calls, "cancellation during handler selection must prevent the native callback")
	assert.Nil(t, env.Runtime.CurrentCondition())
}

type handlerDataCloneProbe struct {
	order *[]string
	name  string
}

func (p *handlerDataCloneProbe) CloneNative() interface{} {
	*p.order = append(*p.order, p.name)
	return &handlerDataCloneProbe{order: p.order, name: p.name}
}

func TestHandlerDataNativeCloneOrderAndFailure(t *testing.T) {
	for _, oversized := range []bool{false, true} {
		t.Run(fmt.Sprintf("oversized=%t", oversized), func(t *testing.T) {
			env := newCallSemanticsEnv(t)
			env.Runtime.MaxAlloc = 8
			var order []string
			data := lisp.SortedMap()
			first := lisp.Native(&handlerDataCloneProbe{order: &order, name: "a"})
			if oversized {
				first = copyAllocationValue(t, "list", 9)
			}
			require.NoError(t, lisp.GoError(data.Map().Set(lisp.String("z"), lisp.Native(&handlerDataCloneProbe{order: &order, name: "z"}))))
			require.NoError(t, lisp.GoError(data.Map().Set(lisp.String("a"), first)))
			env.AddBuiltins(true, elpsutil.Function("raise-data", lisp.Formals(),
				func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return env.ErrorCondition("original", data) }))
			result := env.LoadString("handler-data.lisp", `(handler-bind ((condition (lambda (c data) data))) (raise-data))`)
			if oversized {
				require.Equal(t, lisp.LError, result.Type)
				assert.Contains(t, result.String(), "allocation size 9 exceeds maximum (8)")
				assert.Empty(t, order, "the first failing map value must stop later clone hooks")
			} else {
				require.NoError(t, lisp.GoError(result))
				assert.Equal(t, []string{"a", "z"}, order, "handler copies must preserve Copy's deterministic native hook order")
				copied, ok := result.Map().Get(lisp.String("a"))
				require.True(t, ok)
				assert.NotSame(t, first.Native, copied.Native)
			}
			assert.Nil(t, env.Runtime.CurrentCondition())
		})
	}
}
