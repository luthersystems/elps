// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
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
// not be negative.  The typed methods (#905) check their arguments.
const parityLisp = `
(set 'positive (s:make-validator "positive" s:int (s:gt 0)))
(set 'T
  (s:make-record "counter"
    (s:has-key "n" s:int)
    :check (lambda (o) (when (< (o 'n) 0) (error 'wrong-type "negative")))
    (s:method inc (o) (s:set-field! o 'n (+ (o 'n) 1)) (o 'n))
    (s:method add (o k) (s:set-field! o 'n (+ (o 'n) k)) (o 'n))
    (s:method addp (o [k positive] &optional [memo s:string])
      (s:set-field! o 'n (+ (o 'n) k))
      (o 'n))
    (s:method sum (o &rest [xs s:int])
      (s:set-field! o 'n (apply + (o 'n) xs))
      (o 'n))
    (s:method tagged (o &key [tag s:string]) tag)
    (s:method setn (o [k s:int]) :returns positive
      (s:set-field! o 'n k)
      k)))
(s:defmethod T merge (o [other T])
  (s:set-field! o 'n (+ (o 'n) (other 'n)))
  (o 'n))
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
	isString := func(v *lisp.LVal) error {
		if v.Type != lisp.LString {
			return errors.New("not a string")
		}
		return nil
	}
	positive := func(v *lisp.LVal) error {
		if err := isInt(v); err != nil {
			return err
		}
		if v.Int <= 0 {
			return fmt.Errorf("%w: not positive", libschema.ErrFailedConstraint)
		}
		return nil
	}
	var rt *libschema.RecordType
	isCounter := func(v *lisp.LVal) error {
		if !rt.Is(v) {
			return errors.New("not a counter")
		}
		return nil
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
		}, {
			Name: "addp", Formals: lisp.Formals("k", "&optional", "memo"),
			ArgChecks: map[string]func(*lisp.LVal) error{"k": positive, "memo": isString},
			Call: func(env *lisp.LEnv, r libschema.Record, args []*lisp.LVal) *lisp.LVal {
				return set(env, r, r.At(0).Int+args[0].Int)
			},
		}, {
			Name: "sum", Formals: lisp.Formals("&rest", "xs"),
			ArgChecks: map[string]func(*lisp.LVal) error{"xs": isInt},
			Call: func(env *lisp.LEnv, r libschema.Record, args []*lisp.LVal) *lisp.LVal {
				n := r.At(0).Int
				for _, x := range args {
					n += x.Int
				}
				return set(env, r, n)
			},
		}, {
			Name: "tagged", Formals: lisp.Formals("&key", "tag"),
			ArgChecks: map[string]func(*lisp.LVal) error{"tag": isString},
			Call: func(_ *lisp.LEnv, _ libschema.Record, args []*lisp.LVal) *lisp.LVal {
				if len(args) == 2 {
					return args[1]
				}
				return lisp.Nil()
			},
		}, {
			Name: "setn", Formals: lisp.Formals("k"),
			ArgChecks:   map[string]func(*lisp.LVal) error{"k": isInt},
			ReturnCheck: positive,
			Call: func(env *lisp.LEnv, r libschema.Record, args []*lisp.LVal) *lisp.LVal {
				return set(env, r, args[0].Int)
			},
		}, {
			Name: "merge", Formals: lisp.Formals("other"),
			ArgChecks: map[string]func(*lisp.LVal) error{"other": isCounter},
			Call: func(env *lisp.LEnv, r libschema.Record, args []*lisp.LVal) *lisp.LVal {
				other, _ := rt.Record(args[0])
				return set(env, r, r.At(0).Int+other.At(0).Int)
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

// TestTypedArgsParity checks typed method arguments (#905) on both kinds
// of type: each supplied argument is checked before the body runs, an
// absent one is not, and a failed check leaves the object as it was.
func TestTypedArgsParity(t *testing.T) {
	runParity(t, []parityCase{
		{`((T :n 1) 'addp 2)`, "3"},
		{`((T :n 1) 'addp -2)`, "error:failed-constraint"},
		{`((T :n 1) 'addp "x")`, "error:wrong-type"},
		{`((T :n 1) 'addp 2 "m")`, "3"},
		{`((T :n 1) 'addp 2 5)`, "error:wrong-type"},
		{`((T :n 1) 'addp 2 ())`, "error:wrong-type"},
		{`((T :n 1) 'addp)`, "error:bad-arguments"},
		{`((T :n 1) 'addp 1 "m" 3)`, "error:bad-arguments"},
		{`(let ((c (T :n 1))) (ignore-errors (c 'addp -2)) (c 'n))`, "1"},
		{`((T :n 1) 'sum 1 2 3)`, "7"},
		{`((T :n 1) 'sum)`, "1"},
		{`((T :n 1) 'sum 1 "x")`, "error:wrong-type"},
		{`((T :n 1) 'tagged :tag "a")`, `"a"`},
		{`((T :n 1) 'tagged)`, "()"},
		{`((T :n 1) 'tagged :tag 5)`, "error:wrong-type"},
		{`((T :n 1) 'tagged :tag ())`, "error:wrong-type"},
		{`((T :n 1) 'merge (T :n 2))`, "3"},
		{`((T :n 1) 'merge 2)`, "error:wrong-type"},
		{`((T :n 1) 'merge (sorted-map "n" 2))`, "error:wrong-type"},
		// An interface may write types; it matches on names and arity.
		{`(s:validate (s:make-interface "p" (addp ([k positive] &optional [memo s:string]))) (T :n 1))`, "()"},
		{`(s:validate (s:make-interface "p" (addp ([k positive] [memo s:string] c))) (T :n 1))`, "error:wrong-type"},
		{`(get (s:describe (s:make-interface "p" (addp ([k positive])))) "values")`, `(vector "addp ([k positive])")`},
		// An untyped method also checks the call's shape in Go.
		{`((T :n 1) 'add)`, "error:bad-arguments"},
		{`((T :n 1) 'add 1 2)`, "error:bad-arguments"},
		{`((T :n 1) 'inc 1)`, "error:bad-arguments"},
	})
}

// TestReturnsParity checks :returns (#905): the return value is checked
// after the body, and a bad one fails the call and rolls the object back.
func TestReturnsParity(t *testing.T) {
	runParity(t, []parityCase{
		{`((T :n 1) 'setn 5)`, "5"},
		{`((T :n 1) 'setn 0)`, "error:failed-constraint"},
		{`(let ((c (T :n 1))) (ignore-errors (c 'setn 0)) (c 'n))`, "1"},
		{`((T :n 1) 'setn "x")`, "error:wrong-type"},
	})
}

// TestTypedArgsLisp checks what only the Lisp side has: the error text,
// a nested call from a method, s:defmethod :replace and bad declarations.
func TestTypedArgsLisp(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "type.lisp", parityLisp)
	res := env.LoadString("t.lisp", `((T :n 1) 'addp "x")`)
	require.True(t, res.IsError())
	require.Contains(t, lisp.GoError(res).Error(), "user:counter: method addp: argument k:")

	// A call from inside another method is checked too, and the outer
	// call rolls back.
	evalString(t, env, `(s:defmethod T twice (o) (s:set-field! o 'n 50) (o 'addp -1))`)
	require.Equal(t, "error:failed-constraint", parityOutcome(env, `((T :n 1) 'twice)`))
	require.Equal(t, "1", parityOutcome(env, `(let ((c (T :n 1))) (ignore-errors (c 'twice)) (c 'n))`))

	// :self names the type being defined, in s:make-record and in
	// s:defmethod, so a reload binds the new type.
	mustLoad(t, env, "self.lisp", `
(set 'node (s:make-record "node"
  (s:has-key "v" s:int)
  (s:method plus (o [p :self]) (+ (o 'v) (p 'v)))))
(s:defmethod node minus (o [p :self]) (- (o 'v) (p 'v)))`)
	require.Equal(t, "3", parityOutcome(env, `((node :v 1) 'plus (node :v 2))`))
	require.Equal(t, "-1", parityOutcome(env, `((node :v 1) 'minus (node :v 2))`))
	require.Equal(t, "error:wrong-type", parityOutcome(env, `((node :v 1) 'plus (T :n 2))`))

	// [x 'T] resolves T on each call: two types can name each other, and a
	// reload binds the new type.
	mustLoad(t, env, "late.lisp", `
(set 'inv (s:make-record "inv" (s:has-key "n" s:int)
  (s:method attach (o [l 'led]) (l 'n))))
(set 'led (s:make-record "led" (s:has-key "n" s:int)
  (s:method add (o [i 'inv]) (+ (o 'n) (i 'n)))))`)
	require.Equal(t, "5", parityOutcome(env, `((inv :n 1) 'attach (led :n 5))`))
	require.Equal(t, "3", parityOutcome(env, `((led :n 1) 'add (inv :n 2))`))
	require.Equal(t, "error:wrong-type", parityOutcome(env, `((inv :n 1) 'attach (inv :n 5))`))
	evalString(t, env, `(set 'old-led led) (set 'led (s:make-record "led" (s:has-key "n" s:int)))`)
	require.Equal(t, "5", parityOutcome(env, `((inv :n 1) 'attach (led :n 5))`), "the new led")
	require.Equal(t, "error:wrong-type", parityOutcome(env, `((inv :n 1) 'attach (old-led :n 5))`))

	// A built-in type name builds no validator, so a typed call takes no
	// gensym and later gensym names do not move.
	gensyms := evalString(t, env, `(list (gensym) (progn ((T :n 1) 'sum 1 2) ((T :n 1) 'tagged :tag "a") (gensym)) (gensym))`)
	num := func(v *lisp.LVal) int {
		n, err := strconv.Atoi(strings.TrimLeft(v.Str, "gen"))
		require.NoError(t, err, v.Str)
		return n
	}
	a, b, c := num(gensyms.Cells[0]), num(gensyms.Cells[1]), num(gensyms.Cells[2])
	require.Equal(t, c-b, b-a, "a typed call took a gensym")

	// Each element of a typed &rest argument costs one step.  Steps are
	// counted under a limit.
	require.False(t, lisp.WithMaxSteps(1_000_000)(env).IsError())
	steps := func(src string) int64 {
		before := env.Runtime.TotalSteps()
		evalString(t, env, src)
		return env.Runtime.TotalSteps() - before
	}
	evalString(t, env, `(set 'c (T :n 0))
(s:defmethod T sumu (o &rest xs) (s:set-field! o 'n (apply + (o 'n) xs)) (o 'n))`)
	typed := steps(`(c 'sum 1 2 3 4)`) - steps(`(c 'sum 1)`)
	untyped := steps(`(c 'sumu 1 2 3 4)`) - steps(`(c 'sumu 1)`)
	require.Equal(t, int64(3), typed-untyped)

	// :returns names the method and the return value, works with :self
	// and a late type, and with :replace in either order.
	res = env.LoadString("r.lisp", `((T :n 1) 'setn 0)`)
	require.Contains(t, lisp.GoError(res).Error(), "user:counter: method setn: return value:")
	mustLoad(t, env, "ret.lisp", `
(s:defmethod node copy (o) :returns :self (node :v (o 'v)))
(s:defmethod node bad-copy (o) :returns :self 7)
(s:defmethod node to-led (o) :returns 'led (led :n (o 'v)))
(s:defmethod node copy (o) :replace true :returns :self (node :v (+ 1 (o 'v))))
(s:defmethod node copy (o) :returns :self :replace true (node :v (+ 2 (o 'v))))`)
	require.Equal(t, "3", parityOutcome(env, `(((node :v 1) 'copy) 'v)`))
	require.Equal(t, "error:wrong-type", parityOutcome(env, `((node :v 1) 'bad-copy)`))
	require.Equal(t, "4", parityOutcome(env, `(((node :v 4) 'to-led) 'n)`))
	// A nested call is checked too.
	evalString(t, env, `(s:defmethod T via (o) ((T :n 1) 'setn 0))`)
	require.Equal(t, "error:failed-constraint", parityOutcome(env, `((T :n 1) 'via)`))

	// :replace replaces the types with the method.
	evalString(t, env, `(s:defmethod T addp (o k) :replace true k)`)
	require.Equal(t, "-2", parityOutcome(env, `((T :n 1) 'addp -2)`))

	for _, src := range []string{
		`(s:make-record "r" (s:method m ([o T]) 1))`,
		`(s:make-record "r" (s:method m (o [x]) 1))`,
		`(s:make-record "r" (s:method m (o [x s:int extra]) 1))`,
		`(s:make-record "r" (s:method m (o [x (lambda (v) v)]) 1))`,
		`(s:make-record "r" (s:method m (o [&rest s:int]) 1))`,
	} {
		require.Equal(t, "error:bad-arguments", parityOutcome(env, src), "%s", src)
	}
}

// TestTypedArgsGoRefuses checks the spec errors of MethodSpec.ArgChecks.
func TestTypedArgsGoRefuses(t *testing.T) {
	call := func(*lisp.LEnv, libschema.Record, []*lisp.LVal) *lisp.LVal { return lisp.Nil() }
	ok := func(*lisp.LVal) error { return nil }
	for name, m := range map[string]libschema.MethodSpec{
		"no formals":   {Name: "m", Call: call, ArgChecks: map[string]func(*lisp.LVal) error{"x": ok}},
		"unknown name": {Name: "m", Call: call, Formals: lisp.Formals("x"), ArgChecks: map[string]func(*lisp.LVal) error{"y": ok}},
		"nil check":    {Name: "m", Call: call, Formals: lisp.Formals("x"), ArgChecks: map[string]func(*lisp.LVal) error{"x": nil}},
	} {
		_, err := libschema.NewRecordType(libschema.RecordSpec{
			Package: "user", Name: "argrefuse", FID: fmt.Sprintf("argrefuse-%d", goTypeSeq.Add(1)),
			Methods: []libschema.MethodSpec{m},
		})
		require.Error(t, err, name)
	}
}

// TestRefParity checks s:ref (#905): the type only, no field check.
func TestRefParity(t *testing.T) {
	runParity(t, []parityCase{
		{`(s:validate (s:ref T) (T :n 1))`, "()"},
		{`(s:validate (s:ref T) 5)`, "error:wrong-type"},
		{`(s:validate (s:ref T) (sorted-map "n" 1))`, "error:wrong-type"},
		{`(s:validate (s:of (s:ref T)) (list (T :n 1) (T :n 2)))`, "()"},
		{`(s:validate (s:has-key "c" (s:ref T)) (sorted-map "c" 3))`, "error:wrong-type"},
		{`(s:ref 5)`, "error:bad-arguments"},
		{`(s:ref s:int)`, "error:bad-arguments"},
		{`(get (s:describe (s:ref T)) "op")`, `"ref"`},
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
	v1, v2 := a.Value(), a.Value()
	require.NotSame(t, v1, v2)
	env.PutGlobal(lisp.Symbol("RA"), a.Ref(env))
	require.Equal(t, "()", parityOutcome(env, `(s:validate RA (A :n 1))`))
	require.Equal(t, "error:wrong-type", parityOutcome(env, `(s:validate RA (B :n 1))`))
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
