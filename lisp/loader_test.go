// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"io"
	"strings"
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

// plainReader wraps a lisp.Reader, exposing only the Read method so that the
// wrapper intentionally does NOT implement lisp.LocationReader.  This forces
// LoadLocation to take its fallback path.
type plainReader struct {
	inner lisp.Reader
	// lastName records the name argument from the most recent Read call.
	lastName string
}

func (r *plainReader) Read(name string, rd io.Reader) ([]*lisp.LVal, error) {
	r.lastName = name
	return r.inner.Read(name, rd)
}

// TestLoadLocation_fallbackUsesLoc verifies that LoadLocation passes the loc
// argument (not name) to Read when the Reader does not implement
// LocationReader.  This is the regression test for issue #119: stack frames
// reported the loading file instead of the loaded file.
func TestLoadLocation_fallbackUsesLoc(t *testing.T) {
	env := lisp.NewEnv(nil)
	lerr := lisp.InitializeUserEnv(env)
	if !lerr.IsNil() {
		t.Fatalf("environment initialization failure: %v", lerr)
	}

	pr := &plainReader{inner: parser.NewReader()}
	env.Runtime.Reader = pr

	src := strings.NewReader(`(+ 1 2)`)
	result := env.LoadLocation("loader.lisp", "actual-file.lisp", src)
	if result.Type == lisp.LError {
		t.Fatalf("LoadLocation failed: %v", result)
	}

	// The fallback path should pass loc ("actual-file.lisp") to Read, not
	// name ("loader.lisp").
	assert.Equal(t, "actual-file.lisp", pr.lastName,
		"LoadLocation fallback should pass loc to Read, not name")
}
