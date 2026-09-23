// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
)

// Issue #679: a validator's FID reaches function printing and stack traces,
// so it must not depend on what the rest of the process built earlier.

const validatorNameProgram = `(list (s:gt 1) (s:positive) (s:has-key "a" (s:gt 0)))`

func validatorFIDs(t *testing.T, env *lisp.LEnv) []string {
	t.Helper()
	res := env.LoadString("names.lisp", validatorNameProgram)
	if res.Type == lisp.LError {
		t.Fatal(res)
	}
	var ids []string
	for _, c := range res.Cells {
		ids = append(ids, c.FID())
	}
	return ids
}

func burnProcessHistory(t *testing.T) {
	t.Helper()
	other := newSchemaEnv(t)
	validatorFIDs(t, other)
	libschema.NewValidator(lisp.Formals("input"), func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() })
}

func TestValidatorNamesIndependentOfProcessHistory(t *testing.T) {
	first := validatorFIDs(t, newSchemaEnv(t))
	burnProcessHistory(t)
	second := validatorFIDs(t, newSchemaEnv(t))
	if len(first) != len(second) {
		t.Fatalf("length mismatch %v %v", first, second)
	}
	for i := range first {
		if first[i] != second[i] {
			t.Fatalf("validator %d named %q in one fresh env and %q in another", i, first[i], second[i])
		}
	}
}

func TestValidatorNamesIdenticalAcrossTemplateVMs(t *testing.T) {
	env := newSchemaForkEnv(t)
	mustLoad(t, env, "setup.lisp", `(in-package 'user) (set 'pre (s:gt 0))`)
	template := schemaTemplate(t, env)
	vm1 := forkSchemaTemplate(t, template)
	first := validatorFIDs(t, vm1)
	burnProcessHistory(t)
	vm2 := forkSchemaTemplate(t, template)
	second := validatorFIDs(t, vm2)
	for i := range first {
		if first[i] != second[i] {
			t.Fatalf("validator %d named %q in one VM and %q in another", i, first[i], second[i])
		}
	}
}

func TestNewValidatorEnvNamesPerRuntime(t *testing.T) {
	ok := func(*lisp.LEnv, *lisp.LVal) *lisp.LVal { return lisp.Nil() }
	a := libschema.NewValidatorEnv(newSchemaEnv(t), lisp.Formals("input"), ok).FID()
	burnProcessHistory(t)
	b := libschema.NewValidatorEnv(newSchemaEnv(t), lisp.Formals("input"), ok).FID()
	if a != b {
		t.Fatalf("NewValidatorEnv named %q then %q in two fresh envs", a, b)
	}
}
