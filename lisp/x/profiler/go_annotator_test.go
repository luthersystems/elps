package profiler_test

import (
	"os"
	"runtime/pprof"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
)

// This is a bit of a silly test but it demonstrates the issue
// with sampling pretty well - to get a meaningful profile, you
// have to do a lot of work
func TestNewPprofAnnotator(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	ppa := profiler.NewPprofAnnotator(env.Runtime, nil)
	file, err := os.Create("./pprof")
	assert.NoError(t, err)
	assert.NoError(t, pprof.StartCPUProfile(file))
	defer pprof.StopCPUProfile()
	assert.NoError(t, ppa.Enable())
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}
	// Some spurious functions to check we get a profile out
	testsrc := env.LoadString("longtest.lisp", longTestLisp)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type, lerr.Str)
	// Mark the profile as complete and dump the rest of the profile
	assert.NoError(t, ppa.Complete())
}
