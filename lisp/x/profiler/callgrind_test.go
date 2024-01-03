package profiler_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	profiler2 "github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
)

func TestNewCallgrind(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	// Create a profiler
	profiler := profiler2.NewCallgrindProfiler(env.Runtime)
	// Tell it what to do with the output
	if err := profiler.SetFile("./callgrind.test_prof"); err != nil {
		t.Fatal(err.Error())
	}
	// Enable the profiler
	if err := profiler.Enable(); err != nil {
		t.Fatal(err.Error())
	}
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}
	testsrc := env.LoadString("test.lisp", testLisp)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type)
	// Mark the profile as complete and dump the rest of the profile
	assert.NoError(t, profiler.Complete())
}
