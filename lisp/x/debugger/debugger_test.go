package debugger

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestNewDebugger(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	lerr := lisp.InitializeUserEnv(env)
	if lisp.GoError(lerr) != nil {
		t.Fatal(lisp.GoError(lerr))
	}
	// Create a profiler
	profiler := NewDebugger(env.Runtime, ":8883")
	var testsrc *lisp.LVal
	// Some spurious functions to check we get a profile out
	testsrc = env.LoadString("test.lisp", `
(set 'j 4)
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
		(< x j)
		(recurse-it (- x 1))
		(add-it x 3)
	)
)
(print-it "Hello")
(print-it (add-it (add-it 3 (recurse-it 5)) 8))`)
	lerr = env.Eval(testsrc)
	assert.NotEqual(t, lisp.LError, lerr.Type)
	profiler.Complete()
	t.Fail()
}
