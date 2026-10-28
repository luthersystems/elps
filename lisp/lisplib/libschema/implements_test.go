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

// TestImplementsLisp checks :implements (#905).  A method in the
// s:make-record form that does not match fails at once.  A method that is
// missing may still come from s:defmethod, so it fails when the first
// object is built.
func TestImplementsLisp(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "i.lisp", `
(set 'payable (s:make-interface "payable" (pay (amount)) (total ())))
(set 'inv (s:make-record "inv" :implements (payable)
  (s:has-key "t" s:int)
  (s:method pay (o a) (s:set-field! o 't (- (o 't) a)))
  (s:method total (o) (o 't))))
(set 'later (s:make-record "later" :implements (payable)
  (s:has-key "t" s:int)
  (s:method pay (o a) a)))`)
	require.Equal(t, "5", parityOutcome(env, `((inv :t 5) 'total)`))
	require.Equal(t, "error:wrong-type", parityOutcome(env, `(later :t 1)`), "total is missing")
	evalString(t, env, `(s:defmethod later total (o) (o 't))`)
	require.Equal(t, "1", parityOutcome(env, `((later :t 1) 'total)`))
	require.Equal(t, "error:wrong-type", parityOutcome(env, `(s:defmethod later total (o x) :replace true x)`),
		":replace that breaks an interface fails")
	require.Equal(t, "1", parityOutcome(env, `((later :t 1) 'total)`), "the failed :replace changed nothing")

	// A method in the form with the wrong arity fails when the type is
	// defined.
	require.Equal(t, "error:wrong-type", parityOutcome(env, `(s:make-record "bad" :implements (payable)
  (s:method pay (o) 1) (s:method total (o) 1))`))
	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(s:make-record "bad" :implements (5))`))
	require.Equal(t, "error:bad-arguments", parityOutcome(env, `(s:make-record "bad" :implements)`))
}

// TestImplementsGo checks RecordSpec.Implements: a Go type is sealed, so
// NewRecordType checks it at once.
func TestImplementsGo(t *testing.T) {
	payable, err := libschema.NewInterface(libschema.InterfaceSpec{Name: "payable", Methods: []libschema.InterfaceMethod{
		{Name: "pay", Formals: lisp.Formals("amount")},
	}})
	require.NoError(t, err)
	call := func(*lisp.LEnv, libschema.Record, []*lisp.LVal) *lisp.LVal { return lisp.Nil() }
	spec := func(formals *lisp.LVal) libschema.RecordSpec {
		return libschema.RecordSpec{
			Package: "user", Name: "gopay", FID: fmt.Sprintf("gopay-%d", goTypeSeq.Add(1)),
			Implements: []*libschema.Interface{payable},
			Methods:    []libschema.MethodSpec{{Name: "pay", Formals: formals, Call: call}},
		}
	}
	_, err = libschema.NewRecordType(spec(lisp.Formals("x")))
	require.NoError(t, err)
	_, err = libschema.NewRecordType(spec(lisp.Formals()))
	require.Error(t, err)
	require.Contains(t, err.Error(), "payable")
	bad := spec(lisp.Formals("x"))
	bad.Implements = []*libschema.Interface{nil}
	_, err = libschema.NewRecordType(bad)
	require.Error(t, err)
	require.False(t, errors.Is(err, libschema.ErrFailedConstraint))
}
