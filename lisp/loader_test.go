// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
)

func testLoaderEnv(t *testing.T) *lisp.LEnv {
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("environment initialization failure: %v", lerr)
	}
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	return env
}

func TestLoadFile(t *testing.T) {
	env := testLoaderEnv(t)
	lok := env.LoadFile("testfixtures/test1.lisp")
	if lok.Type == lisp.LError {
		t.Fatalf("unable to load test fixture: %v", lok)
	}
	fname := "test1:this-is-a-test"
	call := lisp.SExpr([]*lisp.LVal{lisp.Symbol(fname)})
	result := env.Eval(call)
	assert.Equal(t, lisp.LString, result.Type)
	assert.Equal(t, "testing", result.Str)
}

func TestLoadFile_chain(t *testing.T) {
	env := testLoaderEnv(t)
	lok := env.LoadFile("testfixtures/test2.lisp")
	if lok.Type == lisp.LError {
		t.Fatalf("unable to load test fixture: %v", lok)
	}

	// test1 is loaded indirectly while test2 is being evaluated
	fname := "test1:this-is-a-test"
	call := lisp.SExpr([]*lisp.LVal{lisp.Symbol(fname)})
	result := env.Eval(call)
	assert.Equal(t, lisp.LString, result.Type)
	assert.Equal(t, "testing", result.Str)

	fname = "test2:this-is-a-test2"
	call = lisp.SExpr([]*lisp.LVal{lisp.Symbol(fname)})
	result = env.Eval(call)
	assert.Equal(t, lisp.LString, result.Type)
	assert.Equal(t, "TESTING", result.Str)
}

func TestLoadFile_chainRecursive(t *testing.T) {
	env := testLoaderEnv(t)
	lok := env.LoadFile("testfixtures/test3.lisp")
	if lok.Type == lisp.LError {
		t.Fatalf("unable to load test fixture: %v", lok)
	}

	// test1 is loaded indirectly while test2 is being evaluated
	fname := "test1:this-is-a-test"
	call := lisp.SExpr([]*lisp.LVal{lisp.Symbol(fname)})
	result := env.Eval(call)
	assert.Equal(t, lisp.LString, result.Type)
	assert.Equal(t, "testing", result.Str)

	fname = "test2:this-is-a-test2"
	call = lisp.SExpr([]*lisp.LVal{lisp.Symbol(fname)})
	result = env.Eval(call)
	assert.Equal(t, lisp.LString, result.Type)
	assert.Equal(t, "TESTING", result.Str)

	fname = "test3:this-is-a-test3"
	call = lisp.SExpr([]*lisp.LVal{lisp.Symbol(fname)})
	result = env.Eval(call)
	assert.Equal(t, lisp.LString, result.Type)
	assert.Equal(t, "__TESTING__", result.Str)
}

// dumb test asserting that it is not possible to load files without special
// configuration (nil default SourceLibrary).
func TestLoadFile_noSourceLibrary(t *testing.T) {
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("environment initialization failure: %v", lerr)
	}
	env.Runtime.Reader = parser.NewReader()
	lok := env.LoadFile("testfixtures/test1.lisp")
	assert.Equal(t, lok.Type, lisp.LError)
}
