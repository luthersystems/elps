// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/stretchr/testify/require"
)

// The parity harness (luthersystems/elps#905).  A record type defined in
// Lisp and the same type defined in Go must behave the same: the same
// results, the same conditions and the same rollback.  Each case runs on
// both, with the type bound to T, and the two outcomes must be equal to
// each other and to the case's want.

// parityLisp is the Lisp side: a counter with one int field, n, that must
// not be negative.
const parityLisp = `
(set 'T
  (s:make-record "counter"
    (s:has-key "n" s:int)
    :check (lambda (o) (when (< (o 'n) 0) (error 'wrong-type "negative")))
    (s:method inc (o) (s:set-field! o 'n (+ (o 'n) 1)) (o 'n))
    (s:method add (o k) (s:set-field! o 'n (+ (o 'n) k)) (o 'n))))
`

// parityCase is one expression and the outcome both types must give: the
// printed value, or "error:" and the condition.
type parityCase struct {
	src, want string
}

// newParityGoType is the Go side of parityLisp.
func newParityGoType(t *testing.T) *libschema.RecordType {
	t.Helper()
	isInt := func(v *lisp.LVal) error {
		if v.Type != lisp.LInt {
			return errors.New("not an int")
		}
		return nil
	}
	set := func(env *lisp.LEnv, r libschema.Record, n int) *lisp.LVal {
		if lerr := r.Set(env, 0, lisp.Int(n)); lerr != nil {
			return lerr
		}
		return r.At(0)
	}
	rt, err := libschema.NewRecordType(libschema.RecordSpec{
		Package: "user", Name: "counter", FID: fmt.Sprintf("parity-counter-%d", goTypeSeq.Add(1)),
		Fields: []libschema.FieldSpec{{Name: "n", Getter: true, Check: isInt}},
		Check: func(r libschema.Record) error {
			if r.At(0).Int < 0 {
				return errors.New("negative")
			}
			return nil
		},
		Methods: []libschema.MethodSpec{{
			Name: "inc", Formals: lisp.Formals(),
			Call: func(env *lisp.LEnv, r libschema.Record, _ []*lisp.LVal) *lisp.LVal {
				return set(env, r, r.At(0).Int+1)
			},
		}, {
			Name: "add", Formals: lisp.Formals("k"),
			Call: func(env *lisp.LEnv, r libschema.Record, args []*lisp.LVal) *lisp.LVal {
				if err := isInt(args[0]); err != nil {
					return env.ErrorConditionf(libschema.WrongType, "add: %v", err)
				}
				return set(env, r, r.At(0).Int+args[0].Int)
			},
		}},
	})
	require.NoError(t, err)
	return rt
}

// parityOutcome runs src in env and returns its outcome as parityCase.want
// writes it.
func parityOutcome(env *lisp.LEnv, src string) string {
	v := env.LoadString("parity.lisp", src)
	if v.IsError() {
		return "error:" + v.Str
	}
	return v.String()
}

// runParity runs each case on the Lisp type and on the Go type.
func runParity(t *testing.T, cases []parityCase) {
	t.Helper()
	rt := newParityGoType(t)
	for _, c := range cases {
		lispEnv := newSchemaForkEnv(t)
		mustLoad(t, lispEnv, "type.lisp", parityLisp)
		goEnv := newSchemaForkEnv(t)
		goEnv.PutGlobal(lisp.Symbol("T"), rt.Value())
		gotLisp, gotGo := parityOutcome(lispEnv, c.src), parityOutcome(goEnv, c.src)
		require.Equal(t, gotLisp, gotGo, "%s: Lisp and Go types differ", c.src)
		require.Equal(t, c.want, gotLisp, "%s", c.src)
	}
}

// TestRecordParity is the base table: construction, calls, rollback and
// the type as a validator.
func TestRecordParity(t *testing.T) {
	runParity(t, []parityCase{
		{`((T :n 1) 'inc)`, "2"},
		{`((T :n 1) 'n)`, "1"},
		{`((T (sorted-map "n" 3)) 'n)`, "3"},
		{`(T :n "x")`, "error:wrong-type"},
		{`(T :n -1)`, "error:wrong-type"},
		{`(T :m 1)`, "error:bad-arguments"},
		{`(T :n)`, "error:bad-arguments"},
		{`(T)`, "error:failed-constraint"},
		{`((T :n 1) 'nope)`, "error:" + libschema.UnknownOperation},
		{`(let ((c (T :n 0))) (ignore-errors (c 'add -5)) (c 'n))`, "0"},
		{`(let ((c (T :n 0))) (c 'add 5) (c 'n))`, "5"},
		{`(s:record? (T :n 1))`, "true"},
		{`(get (s:fields (T :n 4)) "n")`, "4"},
		{`(s:validate T (T :n 1))`, "()"},
		{`(s:validate T 5)`, "error:wrong-type"},
		{`(s:validate T (sorted-map "n" 1))`, "error:wrong-type"},
		{`(s:validate (s:has-key "c" T) (sorted-map "c" (T :n 1)))`, `"c"`},
		{`(s:validate (s:has-key "c" T) (sorted-map "c" 1))`, "error:wrong-type"},
		{`(s:validate (s:of T) (list (T :n 1) (T :n 2)))`, "()"},
		{`(s:validate (s:of T) (list (T :n 1) 5))`, "error:wrong-type"},
	})
}

// TestRecordValueNamesItsType checks that the Lisp value of a Go type
// accepts only that type's objects, and that each call returns a new
// value, so no two VMs share one.
func TestRecordValueNamesItsType(t *testing.T) {
	a, b := newParityGoType(t), newParityGoType(t)
	env := newSchemaForkEnv(t)
	env.PutGlobal(lisp.Symbol("A"), a.Value())
	env.PutGlobal(lisp.Symbol("B"), b.Value())
	require.Equal(t, "()", parityOutcome(env, `(s:validate A (A :n 1))`))
	require.Equal(t, "error:wrong-type", parityOutcome(env, `(s:validate A (B :n 1))`))
	require.NotSame(t, a.Value(), a.Value())
	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(s:defmethod A m (o) 1)`), "a Go type is sealed")
}

// TestRecordValueFork checks that the Lisp value of a Go type works in each
// fork of a template, and that a fork's objects are its own.
func TestRecordValueFork(t *testing.T) {
	rt := newParityGoType(t)
	env := newSchemaForkEnv(t)
	env.PutGlobal(lisp.Symbol("T"), rt.Value())
	evalString(t, env, `(set 'c (T :n 1)) (set 'ty (s:has-key "c" T))`)
	template := schemaTemplate(t, env)
	a := forkSchemaTemplate(t, template)
	b := forkSchemaTemplate(t, template)
	evalString(t, a, `(c 'add 10)`)
	require.Equal(t, 11, evalString(t, a, `(c 'n)`).Int)
	require.Equal(t, 1, evalString(t, b, `(c 'n)`).Int)
	require.Equal(t, `"c"`, parityOutcome(b, `(s:validate ty (sorted-map "c" (T :n 2)))`))
	require.Equal(t, "error:wrong-type", parityOutcome(b, `(s:validate ty (sorted-map "c" 2))`))
}
