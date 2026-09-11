// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newSortErrorEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	return env
}

func TestSortErrorMissingKey(t *testing.T) {
	t.Parallel()
	for _, key := range []string{"missing-key", "missing-package:key"} {
		t.Run(key, func(t *testing.T) {
			t.Parallel()
			env := newSortErrorEnv(t)
			require.NotEqual(t, lisp.LError, env.LoadString("sort-error.lisp", `(set 'source (vector 3 1 2))`).Type)

			result := env.LoadString("sort-error.lisp", fmt.Sprintf(`(stable-sort < source '%s)`, key))
			assert.False(t, lisp.IsInternalPanic(result), "%v", result)
			assert.Equal(t, lisp.LError, result.Type, "an unresolved key function must not return the comparator as success")
			if result.Type == lisp.LError {
				assert.Contains(t, result.String(), "missing-", "the lookup error must identify the missing binding or package")
			}
			assert.Equal(t, "(vector 3 1 2)", env.LoadString("sort-error.lisp", "source").String(), "failed key resolution must not sort the input")
		})
	}
}

func TestSortErrorStopsAfterFirstCondition(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		expr string
	}{
		{"search predicate", `(search-sorted 16 fail)`},
		{"insert list predicate", `(insert-sorted 'list source fail 0)`},
		{"insert vector predicate", `(insert-sorted 'vector source fail 0)`},
		{"insert key", `(insert-sorted 'list source < 0 fail)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			env := newSortErrorEnv(t)
			// The counter only observes callback invocations; it does not
			// change the sequence being searched. Distinct errors ensure that
			// a later callback cannot silently replace the first condition.
			setup := env.LoadString("sort-error.lisp", `
				(set 'calls 0)
				(set 'source (vector 1 2 3 4 5 6 7 8))
				(defun fail (&rest args)
				  (set! calls (+ calls 1))
				  (if (= calls 1)
				    (error 'first-error "original" 17)
				    (error 'later-error "replacement" 99)))`)
			require.NotEqual(t, lisp.LError, setup.Type, "%v", setup)

			result := env.LoadString("sort-error.lisp", tc.expr)
			assert.False(t, lisp.IsInternalPanic(result), "%v", result)
			assert.Equal(t, "1", env.LoadString("sort-error.lisp", "calls").String(), "no callback may run after the first error")
			assert.Equal(t, "(vector 1 2 3 4 5 6 7 8)", env.LoadString("sort-error.lisp", "source").String())
			require.Equal(t, lisp.LError, result.Type, "the callback condition must propagate")
			assert.Equal(t, "first-error", result.Str)
			require.Len(t, result.Cells, 2)
			assert.Equal(t, lisp.LString, result.Cells[0].Type)
			assert.Equal(t, "original", result.Cells[0].Str)
			assert.Equal(t, lisp.LInt, result.Cells[1].Type)
			assert.Equal(t, 17, result.Cells[1].Int)
		})
	}
}

func TestSortErrorPreservesInternalPanic(t *testing.T) {
	t.Parallel()
	for _, operation := range []struct {
		name string
		expr string
	}{
		{"search predicate", `(search-sorted 16 host-failure)`},
		{"insert predicate", `(insert-sorted 'list '(1 2 3 4 5 6 7 8) host-failure 0)`},
		{"insert key", `(insert-sorted 'list '(1 2 3 4 5 6 7 8) < 0 host-failure)`},
	} {
		for _, containment := range []struct {
			name string
			form string
		}{
			{"raw", `%s`},
			{"catch-all handler", `(handler-bind ((condition (lambda (&rest args) "swallowed"))) %s)`},
			{"ignore-errors", `(ignore-errors %s)`},
		} {
			t.Run(operation.name+"/"+containment.name, func(t *testing.T) {
				t.Parallel()
				env := newSortErrorEnv(t)
				calls := 0
				env.AddBuiltins(true, elpsutil.Function("host-failure", lisp.Formals(lisp.VarArgSymbol, "args"),
					func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
						calls++
						if calls == 1 {
							panic("first host panic")
						}
						return env.ErrorConditionf("later-error", "replacement")
					}))

				result := env.LoadString("sort-error.lisp", fmt.Sprintf(containment.form, operation.expr))
				assert.Equal(t, 1, calls, "a recovered host panic must stop all further callback evaluation")
				assert.True(t, lisp.IsInternalPanic(result), "the original non-forgeable panic marker must survive: %v", result)
				require.Equal(t, lisp.LError, result.Type)
				assert.Contains(t, result.String(), "first host panic")
			})
		}
	}
}

func TestHandlerBindEmptyBody(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name    string
		expr    string
		wantErr string
	}{
		{"no handlers", `(handler-bind ())`, ""},
		{"one handler", `(handler-bind ((condition (lambda (&rest args) 42))))`, ""},
		{"handler is not evaluated", `(handler-bind ((condition (error 'handler-evaluated))))`, ""},
		{"invalid handler list", `(handler-bind 1)`, "first argument is not a list"},
		{"invalid binding", `(handler-bind (1))`, "first argument is not a list of bindings"},
		{"invalid binding length", `(handler-bind ((condition)))`, "first argument is not a list of bindings"},
		{"invalid condition", `(handler-bind ((1 identity)))`, "binding type is not a symbol"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			result := newSortErrorEnv(t).LoadString("handler-empty.lisp", tc.expr)
			assert.False(t, lisp.IsInternalPanic(result), "%v", result)
			if tc.wantErr != "" {
				require.Equal(t, lisp.LError, result.Type)
				assert.Contains(t, result.String(), tc.wantErr)
				return
			}
			require.NotEqual(t, lisp.LError, result.Type, "%v", result)
			assert.True(t, result.IsNil(), "an empty handler-bind body returns Lisp nil, got %v", result)
		})
	}
}
