package debugger

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestWhileUnderDebugger pins that attaching a debugger (which disables
// tail-call optimization, so the labels macro while replaced never counted
// tail iterations) does not make the while operator count them either: a
// bounded loop completes under WithMaxTailIterations(1), and a runaway loop
// is still stopped, by the physical-height-derived turn bound.
func TestWhileUnderDebugger(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisp.WithMaxTailIterations(1)(env)))
	env.Runtime.Debugger = New()
	got := env.LoadString("w.lisp", `(set 'i 0) (while (< i 3) (set! i (+ i 1))) i`)
	require.NoError(t, lisp.GoError(got))
	assert.Equal(t, "3", got.String())
	got = env.LoadString("w.lisp", `(while true)`)
	require.Equal(t, lisp.LError, got.Type)
	assert.Contains(t, got.String(), "while: loop exceeded")
}
