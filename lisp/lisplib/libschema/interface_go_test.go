// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/stretchr/testify/require"
)

// newPayableGoType defines a Go record type with methods pay (amount) and
// cancel (&optional why), and an untyped method audit.
func newPayableGoType(t *testing.T) *libschema.RecordType {
	t.Helper()
	ret := func(env *lisp.LEnv, _ libschema.Record, args []*lisp.LVal) *lisp.LVal { return lisp.Int(len(args)) }
	rt, err := libschema.NewRecordType(libschema.RecordSpec{
		Package: "user", Name: "goinvoice", FID: fmt.Sprintf("goinvoice-%d", goTypeSeq.Add(1)),
		Fields: []libschema.FieldSpec{{Name: "total", Getter: true}},
		Methods: []libschema.MethodSpec{
			{Name: "pay", Formals: lisp.Formals("amount"), Call: ret},
			{Name: "cancel", Formals: lisp.Formals("&optional", "why"), Call: ret},
			{Name: "audit", Call: ret},
		},
	})
	require.NoError(t, err)
	return rt
}

// TestGoMethodFormals checks that a Go method with Formals refuses a wrong
// number of arguments before Call runs.
func TestGoMethodFormals(t *testing.T) {
	env := newSchemaForkEnv(t)
	rt := newPayableGoType(t)
	o := rt.New(env, lisp.Int(5))
	call := func(args ...*lisp.LVal) *lisp.LVal { return env.FunCall(o, lisp.QExpr(args)) }

	require.Equal(t, 1, call(lisp.Symbol("pay"), lisp.Int(1)).Int)
	require.Equal(t, libschema.BadArgs, call(lisp.Symbol("pay")).Str)
	require.Equal(t, libschema.BadArgs, call(lisp.Symbol("pay"), lisp.Int(1), lisp.Int(2)).Str)
	require.Equal(t, 0, call(lisp.Symbol("cancel")).Int)
	require.Equal(t, 1, call(lisp.Symbol("cancel"), lisp.String("late")).Int)
	require.Equal(t, 3, call(lisp.Symbol("audit"), lisp.Int(1), lisp.Int(2), lisp.Int(3)).Int, "no Formals: any count")

	// Spec returns copies of the formals.
	spec := rt.Spec()
	spec.Methods[0].Formals.Cells[0] = lisp.Symbol("changed")
	require.Equal(t, "amount", rt.Spec().Methods[0].Formals.Cells[0].Str)

	_, err := libschema.NewRecordType(libschema.RecordSpec{
		Package: "user", Name: "badformals", FID: fmt.Sprintf("badformals-%d", goTypeSeq.Add(1)),
		Methods: []libschema.MethodSpec{{Name: "m", Formals: lisp.Formals(), Call: func(*lisp.LEnv, libschema.Record, []*lisp.LVal) *lisp.LVal { return lisp.Nil() }}},
	})
	require.NoError(t, err)
	_, err = libschema.NewRecordType(libschema.RecordSpec{
		Package: "user", Name: "badformals2", FID: fmt.Sprintf("badformals-%d", goTypeSeq.Add(1)),
		Methods: []libschema.MethodSpec{{Name: "m", Formals: lisp.Int(1), Call: func(*lisp.LEnv, libschema.Record, []*lisp.LVal) *lisp.LVal { return lisp.Nil() }}},
	})
	require.Error(t, err, "formals that are not a list")
}

// TestGoInterface checks NewInterface against Go and Lisp objects, and its
// validator as a field type in Lisp.
func TestGoInterface(t *testing.T) {
	env := newSchemaForkEnv(t)
	rt := newPayableGoType(t)
	payable, err := libschema.NewInterface(libschema.InterfaceSpec{
		Name: "payable",
		Methods: []libschema.InterfaceMethod{
			{Name: "pay", Formals: lisp.Formals("amount")},
			{Name: "cancel"},
		},
	})
	require.NoError(t, err)
	require.Equal(t, "payable", payable.Name())

	goObj := rt.New(env, lisp.Int(5))
	require.NoError(t, payable.Satisfies(goObj))
	require.Error(t, payable.Satisfies(lisp.Int(5)))

	strict, err := libschema.NewInterface(libschema.InterfaceSpec{
		Name:    "strict",
		Methods: []libschema.InterfaceMethod{{Name: "pay", Formals: lisp.Formals("amount", "note")}},
	})
	require.NoError(t, err)
	require.ErrorContains(t, strict.Satisfies(goObj), "method pay takes 1 argument(s)")
	audited, err := libschema.NewInterface(libschema.InterfaceSpec{
		Name:    "audited",
		Methods: []libschema.InterfaceMethod{{Name: "audit", Formals: lisp.Formals("a", "b", "&rest", "c")}},
	})
	require.NoError(t, err)
	require.NoError(t, audited.Satisfies(goObj), "a method without Formals accepts any count")

	// The Go interface as a Lisp field type, over Go and Lisp objects; a
	// Lisp interface over a Go object.
	env.PutGlobal(lisp.Symbol("payable"), payable.Validator(env))
	env.PutGlobal(lisp.Symbol("go-obj"), goObj)
	require.NoError(t, lisp.GoError(env.LoadString("t.lisp", `
(set 'lisp-payable (s:make-interface "lisp-payable" (pay (amount)) (cancel ())))
(set 'inv (s:make-record "inv" (s:has-key "n" s:int)
  (s:method pay (o amount) amount) (s:method cancel (_o &optional _why) nil)))
(set 'ledger (s:make-record "ledger" (s:has-key "payer" payable)))
(ledger :payer go-obj)
(ledger :payer (inv :n 1))
(s:validate lisp-payable go-obj)
`)))
	require.Equal(t, libschema.WrongType, env.LoadString("t.lisp", `(ledger :payer 5)`).Str)

	_, err = libschema.NewInterface(libschema.InterfaceSpec{})
	require.Error(t, err)
	_, err = libschema.NewInterface(libschema.InterfaceSpec{Name: "x", Methods: []libschema.InterfaceMethod{{Name: "a"}, {Name: "a"}}})
	require.Error(t, err)
	_, err = libschema.NewInterface(libschema.InterfaceSpec{Name: "x", Methods: []libschema.InterfaceMethod{{Name: "a", Formals: lisp.Formals(":k")}}})
	require.Error(t, err)
}
