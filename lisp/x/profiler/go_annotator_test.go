package profiler_test

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/profiler"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"os"
	"runtime/pprof"
	"testing"
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
	pprof.StartCPUProfile(file)
	defer pprof.StopCPUProfile()
	assert.NoError(t, ppa.Enable())
	assert.Error(t, ppa.SetFile("./pprofout"))
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}
	var testsrc *lisp.LVal
	// Some spurious functions to check we get a profile out
	testsrc = env.LoadString("test.lisp", `
(defun print-it 
	('x)
	(debug-print x)
)
(defun add-it
	('x 'y)
	(+ x y)
)
(defun recurse-it
	('x)
	(if 
		(< x 4)
		(recurse-it (- x 1))
		(add-it x 3)
	)
)
(
	foldl
	(lambda
		('a 'x) 
		(print-it 
			(add-it 
				(recurse-it
					(foldl add-it a (vector 
						1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 2873 2 111 34 4555 22 12
						93 83 12 12 2 2 3 845 83 3 2 4 59 92 1 34 888 38 2 8 4 2 8 4
					))
				) 
				x
			)
		)
		(foldl 
			(lambda
				('m 'q)
				(foldl
					(lambda 
						('j 'b) 
						(* j b)
					) 
					a 
					(vector 83 764 2 37 34 2 1 88 7 6 3 22 1)
				)
			)
			a
			(vector a a a a a a a a a a a a a a a a a a a a a)
		)
	)
	0
	(vector 
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
		9 7 8 6 5 4 3 2 8 36 73 83 3 82 2 1 334 3 82 1 32 3 8 0 1 2 83 4 9 0 11 3 4
	)
)
`)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type, lerr.Str)
	// Mark the profile as complete and dump the rest of the profile
	ppa.Complete()
}
