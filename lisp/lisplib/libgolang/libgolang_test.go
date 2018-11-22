// Copyright © 2018 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libgolang_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	r.Loader = loadPackages
	r.RunTestFile(t, "libgolang_test.lisp")
}

func loadPackages(env *lisp.LEnv) *lisp.LVal {
	lerr := lisplib.LoadLibrary(env)
	if lerr.Type == lisp.LError {
		return lerr
	}
	lerr = loadTestPackage(env)
	if lerr.Type == lisp.LError {
		return lerr
	}
	return env.InPackage(lisp.String(lisp.DefaultUserPackage))
}

type testStruct struct {
	StringField string
	IntField    int
	FloatField  float64
}

const testPackageName = "golang-test"

func loadTestPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(testPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("make-test-struct", lisp.Formals(), builtinMakeTestStruct),
}

func builtinMakeTestStruct(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	s := &testStruct{
		StringField: "test-string",
		IntField:    123,
		FloatField:  12.34,
	}
	return lisp.Native(s)
}
