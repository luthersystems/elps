// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"errors"
	"fmt"
	"strings"
	"sync/atomic"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/stretchr/testify/require"
)

// orderV1 defines a record type in package shop.  The type is bound to the
// global shop:order, the name a durable dump writes for it.
const orderV1 = `
(in-package 'shop)
(set 'order
  (s:make-record "order"
    (s:has-key "id" s:string)
    (s:has-key "qty" s:int)
    (s:method add (o n) (s:set-field! o 'qty (+ n (o 'qty))))))
(set 'o (order :id "o1" :qty 1))
(o 'add 2)
`

// orderV2 is the next version of order: it adds a required field and a
// method, and changes add.
const orderV2 = `
(in-package 'shop)
(set 'order
  (s:make-record "order"
    (s:has-key "id" s:string)
    (s:has-key "qty" s:int)
    (s:has-key "currency" s:string)
    (s:method add (o n) (s:set-field! o 'qty (+ (* 10 n) (o 'qty))))
    (s:method label (o) (format-string "{}:{}" (o 'id) (o 'currency)))))
`

func recordRegistry(t *testing.T, codecs ...libjson.DurableEntry) *libjson.DurableRegistry {
	t.Helper()
	reg, err := libjson.NewFrozenDurableRegistry(append([]libjson.DurableEntry{libschema.RecordCodec}, codecs...)...)
	require.NoError(t, err)
	return reg
}

func evalString(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("test.lisp", src)
	require.NoError(t, lisp.GoError(v), "%s", src)
	return v
}

// TestRecordDurableRoundTrip saves an object as its type's global name and
// its fields, and loads it back into a working object.
func TestRecordDurableRoundTrip(t *testing.T) {
	env := newSchemaForkEnv(t)
	evalString(t, env, orderV1)
	o := evalString(t, env, `shop:o`)
	reg := recordRegistry(t)

	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{o, o}), reg)
	require.NoError(t, err)
	s := string(b)
	require.Contains(t, s, `"elps:s:record"`)
	require.Contains(t, s, `"shop:order"`, "the type is saved by its global's name")
	require.NotContains(t, s, "lambda", "no method code is saved")

	got, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err)
	require.NoError(t, libschema.ValidateLoaded(env, got))
	a, c := got.Cells[0], got.Cells[1]
	require.Equal(t, int64(3), int64(env.FunCall(a, lisp.QExpr([]*lisp.LVal{lisp.Symbol("qty")})).Int))
	// Both cells load over one capture list, so they share the state.
	require.NoError(t, lisp.GoError(env.FunCall(a, lisp.QExpr([]*lisp.LVal{lisp.Symbol("add"), lisp.Int(4)}))))
	require.Equal(t, 7, env.FunCall(c, lisp.QExpr([]*lisp.LVal{lisp.Symbol("qty")})).Int)
	// The loaded object is not the saved one: the saved one is unchanged.
	require.Equal(t, 3, env.FunCall(o, lisp.QExpr([]*lisp.LVal{lisp.Symbol("qty")})).Int)
}

// TestRecordLoadTakesNewType loads a v1 object into a VM that defines v2:
// the object gets v2's methods, and ValidateLoaded reports the field v2
// requires and v1 did not save.
func TestRecordLoadTakesNewType(t *testing.T) {
	v1 := newSchemaForkEnv(t)
	evalString(t, v1, orderV1)
	reg := recordRegistry(t)
	b, err := libjson.DumpDurable(v1, evalString(t, v1, `shop:o`), reg)
	require.NoError(t, err)

	v2 := newSchemaForkEnv(t)
	evalString(t, v2, orderV2)
	got, err := libjson.LoadDurable(v2, b, reg)
	require.NoError(t, err)
	err = libschema.ValidateLoaded(v2, lisp.QExpr([]*lisp.LVal{lisp.SortedMap(), got}))
	require.Error(t, err)
	require.Contains(t, err.Error(), "currency")

	// v2's add, not v1's.
	require.NoError(t, lisp.GoError(v2.FunCall(got, lisp.QExpr([]*lisp.LVal{lisp.Symbol("add"), lisp.Int(1)}))))
	require.Equal(t, 13, v2.FunCall(got, lisp.QExpr([]*lisp.LVal{lisp.Symbol("qty")})).Int)

	// A migration rebuilds the object with the new field.
	v2.PutGlobal(lisp.Symbol("shop:old"), got)
	fixed := evalString(t, v2, `(shop:order (assoc (s:fields shop:old) "currency" "USD"))`)
	require.NoError(t, libschema.ValidateLoaded(v2, fixed))
	require.Equal(t, "o1:USD", v2.FunCall(fixed, lisp.QExpr([]*lisp.LVal{lisp.Symbol("label")})).Str)
}

// TestRecordLoadRefusesRebinding loads an object whose type's global now
// holds an ordinary function.
func TestRecordLoadRefusesRebinding(t *testing.T) {
	env := newSchemaForkEnv(t)
	evalString(t, env, orderV1)
	reg := recordRegistry(t)
	b, err := libjson.DumpDurable(env, evalString(t, env, `shop:o`), reg)
	require.NoError(t, err)

	other := newSchemaForkEnv(t)
	evalString(t, other, `(in-package 'shop) (defun order (&rest args) args)`)
	_, err = libjson.LoadDurable(other, b, reg)
	require.Error(t, err)
}

// TestRecordTypeNotInGlobal saves an object whose type no global holds:
// the dump fails, because a load could not find the type.
func TestRecordTypeNotInGlobal(t *testing.T) {
	env := newSchemaForkEnv(t)
	o := evalString(t, env, `(in-package 'shop)
(defun mk () ((s:make-record "tmp" (s:has-key "a" s:int)) :a 1))
(mk)`)
	require.Equal(t, 1, env.FunCall(o, lisp.QExpr([]*lisp.LVal{lisp.Symbol("a")})).Int)
	_, err := libjson.DumpDurable(env, o, recordRegistry(t))
	require.Error(t, err)
}

// TestRecordTemplateFork defines a type and an object on a template and
// uses them in two forks: each fork has its own copy of the object's
// fields and of the type's method table.
func TestRecordTemplateFork(t *testing.T) {
	env := newSchemaForkEnv(t)
	evalString(t, env, orderV1)
	template := schemaTemplate(t, env)
	a := forkSchemaTemplate(t, template)
	b := forkSchemaTemplate(t, template)

	evalString(t, a, `(shop:o 'add 10)`)
	require.Equal(t, 13, evalString(t, a, `(shop:o 'qty)`).Int)
	require.Equal(t, 3, evalString(t, b, `(shop:o 'qty)`).Int)

	evalString(t, a, `(in-package 'shop) (s:defmethod order twice (o) (* 2 (o 'qty)))`)
	require.Equal(t, 26, evalString(t, a, `(shop:o 'twice)`).Int)
	res := b.LoadString("b.lisp", `(shop:o 'twice)`)
	require.True(t, res.IsError())
	require.Equal(t, libschema.UnknownOperation, res.Str)

	// A fork builds new objects of the template's type, and saves them by
	// the same global name.
	o := evalString(t, b, `(shop:order :id "b" :qty 5)`)
	bytes, err := libjson.DumpDurable(b, o, recordRegistry(t))
	require.NoError(t, err)
	require.Contains(t, string(bytes), `"shop:order"`)
}

// TestDefmethodFromAnotherPackage refuses a method that another package
// defines: a type takes methods only from its own package.
func TestDefmethodFromAnotherPackage(t *testing.T) {
	env := newSchemaForkEnv(t)
	evalString(t, env, orderV1)
	res := env.LoadString("other.lisp", `(in-package 'other) (s:defmethod shop:order hack (o) 1)`)
	require.True(t, res.IsError())
	require.Contains(t, res.String(), "package")
}

var goTypeSeq atomic.Int64

// newCounterType defines a Go record type with a unique FID, so that tests
// can define it more than once.
func newCounterType(t *testing.T) *libschema.RecordType {
	t.Helper()
	fid := fmt.Sprintf("counter-%d", goTypeSeq.Add(1))
	rt, err := libschema.NewRecordType(libschema.RecordSpec{
		Package: "user", Name: "counter", FID: fid,
		Fields: []libschema.FieldSpec{{
			Name: "n", Getter: true,
			Check: func(v *lisp.LVal) error {
				if v.Type != lisp.LInt {
					return errors.New("not an int")
				}
				return nil
			},
		}},
		Check: func(r libschema.Record) error {
			if r.At(0).Int < 0 {
				return errors.New("negative")
			}
			return nil
		},
		Methods: []libschema.MethodSpec{{
			Name: "inc",
			Call: func(env *lisp.LEnv, r libschema.Record, args []*lisp.LVal) *lisp.LVal {
				if lerr := r.Set(env, 0, lisp.Int(r.At(0).Int+1)); lerr != nil {
					return lerr
				}
				return r.At(0)
			},
		}},
		Default: func(env *lisp.LEnv, r libschema.Record, op *lisp.LVal, args []*lisp.LVal) *lisp.LVal {
			if op.Type == lisp.LSymbol && op.Str == "passed" {
				return lisp.String("default")
			}
			if op.Type == lisp.LSymbol && op.Str == "set-to" && len(args) == 1 {
				if lerr := r.Set(env, 0, args[0]); lerr != nil {
					return lerr
				}
				return lisp.Nil()
			}
			return env.ErrorCondition(libschema.UnknownOperation, op)
		},
	})
	require.NoError(t, err)
	return rt
}

// TestGoRecordType builds, calls, saves and loads an object of a Go type.
func TestGoRecordType(t *testing.T) {
	env := newSchemaForkEnv(t)
	rt := newCounterType(t)
	c := rt.New(env, lisp.Int(1))
	require.NoError(t, lisp.GoError(c))
	require.True(t, rt.Is(c))
	call := func(args ...*lisp.LVal) *lisp.LVal { return env.FunCall(c, lisp.QExpr(args)) }

	require.Equal(t, 2, call(lisp.Symbol("inc")).Int)
	require.Equal(t, 2, call(lisp.Symbol("n")).Int)
	require.Equal(t, "default", call(lisp.Symbol("passed")).Str)
	require.Equal(t, libschema.UnknownOperation, call(lisp.Symbol("nope")).Str)
	require.Error(t, lisp.GoError(rt.New(env, lisp.String("x"))))
	require.Error(t, lisp.GoError(rt.New(env, lisp.Int(-1))))

	env.PutGlobal(lisp.Symbol("c"), c)
	require.Equal(t, 2, evalString(t, env, `(get (s:fields c) "n")`).Int)
	require.True(t, env.LoadString("set.lisp", `(s:set-field! c 'n 5)`).IsError(), "a Go type is sealed")
	require.True(t, lisp.True(evalString(t, env, `(s:record? c)`)))

	reg := recordRegistry(t, rt.Codec())
	b, err := libjson.DumpDurable(env, c, reg)
	require.NoError(t, err)
	require.Contains(t, string(b), `"user:`+rt.Spec().FID+`"`)
	got, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err)
	require.Equal(t, 2, env.FunCall(got, lisp.QExpr([]*lisp.LVal{lisp.Symbol("n")})).Int)

	// A method that breaks Check fails and leaves the field as it was.
	require.NoError(t, lisp.GoError(rt.New(env, lisp.Int(0))))
	neg := rt.New(env, lisp.Int(0))
	r, ok := rt.Record(neg)
	require.True(t, ok)
	require.NoError(t, lisp.GoError(r.Set(env, 0, lisp.Int(-5))), "Set checks only the field")
	require.Error(t, lisp.GoError(env.FunCall(neg, lisp.QExpr([]*lisp.LVal{lisp.Symbol("inc")}))))
	require.Equal(t, -5, r.At(0).Int, "the failed method restored the field it was given")

	// Default runs under Check like a method.
	require.NoError(t, lisp.GoError(call(lisp.Symbol("set-to"), lisp.Int(7))))
	require.Equal(t, 7, call(lisp.Symbol("n")).Int)
	require.Error(t, lisp.GoError(call(lisp.Symbol("set-to"), lisp.Int(-3))))
	require.Equal(t, 7, call(lisp.Symbol("n")).Int, "the failed Default restored the field")

	// A load runs the Go checks at once.
	bad := strings.Replace(string(b), "2", "-4", 1)
	_, err = libjson.LoadDurable(env, []byte(bad), reg)
	require.Error(t, err)
	require.Contains(t, err.Error(), "negative")
}

// TestNewRecordTypeRefuses checks the spec errors.
func TestNewRecordTypeRefuses(t *testing.T) {
	call := func(env *lisp.LEnv, _ libschema.Record, _ []*lisp.LVal) *lisp.LVal { return lisp.Nil() }
	for name, spec := range map[string]libschema.RecordSpec{
		"no name":        {Package: "user"},
		"dup field":      {Package: "user", Name: "x1", Fields: []libschema.FieldSpec{{Name: "a"}, {Name: "a"}}},
		"method no call": {Package: "user", Name: "x2", Methods: []libschema.MethodSpec{{Name: "m"}}},
		"method getter":  {Package: "user", Name: "x3", Fields: []libschema.FieldSpec{{Name: "a", Getter: true}}, Methods: []libschema.MethodSpec{{Name: "a", Call: call}}},
		"bad display":    {Package: "user", Name: "x4", DisplayName: "a b"},
	} {
		_, err := libschema.NewRecordType(spec)
		require.Error(t, err, name)
	}
	spec := libschema.RecordSpec{Package: "user", Name: "twice-defined"}
	_, err := libschema.NewRecordType(spec)
	require.NoError(t, err)
	_, err = libschema.NewRecordType(spec)
	require.Error(t, err, "a second type with the same package and FID")
}

// TestGoRecordHiddenField checks that a field with no getter is hidden
// from s:fields as well as from a call.
func TestGoRecordHiddenField(t *testing.T) {
	env := newSchemaForkEnv(t)
	rt, err := libschema.NewRecordType(libschema.RecordSpec{
		Package: "user", Name: "secretive", FID: fmt.Sprintf("secretive-%d", goTypeSeq.Add(1)),
		Fields: []libschema.FieldSpec{{Name: "shown", Getter: true}, {Name: "secret"}},
	})
	require.NoError(t, err)
	env.PutGlobal(lisp.Symbol("x"), rt.New(env, lisp.Int(1), lisp.String("pin")))
	require.Equal(t, libschema.UnknownOperation, env.LoadString("t.lisp", `(x 'secret)`).Str)
	require.Equal(t, `(sorted-map "shown" 1)`, evalString(t, env, `(s:fields x)`).String())
}
