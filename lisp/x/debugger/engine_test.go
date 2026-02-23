package debugger

import (
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// newTestEnv creates a minimal ELPS environment for testing.
func newTestEnv(t *testing.T, dbg *Engine) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Debugger = dbg
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)
	return env
}

func TestEngine_IsEnabled(t *testing.T) {
	e := New()
	assert.False(t, e.IsEnabled())

	e.Enable()
	assert.True(t, e.IsEnabled())

	e.Disable()
	assert.False(t, e.IsEnabled())
}

func TestEngine_BreakpointPauseAndResume(t *testing.T) {
	var events []Event
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		mu.Lock()
		defer mu.Unlock()
		events = append(events, evt)
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Set a breakpoint — we'll use source name "test" with line 1.
	e.Breakpoints().Set("test", 1, "")

	// Eval in a goroutine since it will pause.
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// Wait for the engine to pause (event callback fires).
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause")

	// Verify stopped event was sent.
	mu.Lock()
	assert.True(t, len(events) > 0)
	assert.Equal(t, EventStopped, events[0].Type)
	mu.Unlock()

	// Resume execution.
	e.Resume()

	// Wait for result.
	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 3, res.Int)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_StepInto(t *testing.T) {
	e := New(WithStopOnEntry(true))
	e.Enable()
	env := newTestEnv(t, e)

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	// Should stop on entry.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")

	// Step into.
	e.StepInto()

	// Should pause again on the next expression.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after step")

	// Continue to finish.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.NotEqual(t, lisp.LError, res.Type, "unexpected error: %v", res)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_ExceptionBreakpoint(t *testing.T) {
	e := New()
	e.Enable()
	e.Breakpoints().SetExceptionBreak(ExceptionBreakAll)

	env := newTestEnv(t, e)

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		// This triggers an error through env.ErrorCondition, which has the debugger hook.
		res := env.LoadString("test", `(error 'test-error "deliberate error")`)
		resultCh <- res
	}()

	// Should pause on the error.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause on exception")

	// Resume to let the error propagate.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LError, res.Type)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_Disconnect(t *testing.T) {
	e := New(WithStopOnEntry(true))
	e.Enable()
	env := newTestEnv(t, e)

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)

	// Disconnect should resume and disable.
	e.Disconnect()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 3, res.Int)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout after disconnect")
	}

	assert.False(t, e.IsEnabled())
}

func TestEngine_DisabledHooksAreSkipped(t *testing.T) {
	e := New()
	// NOT enabled — hooks should all be no-ops.

	env := newTestEnv(t, e)

	// This should run without any debugging interference.
	res := env.LoadString("test", "(+ 1 2)")
	assert.Equal(t, lisp.LInt, res.Type)
	assert.Equal(t, 3, res.Int)
}

func TestEngine_ConditionalBreakpoint(t *testing.T) {
	e := New()
	e.Enable()

	env := newTestEnv(t, e)

	// Set a breakpoint with a condition that evaluates to false.
	e.Breakpoints().Set("test", 1, "false")

	// Should NOT pause because condition is false.
	res := env.LoadString("test", "(+ 1 2)")
	assert.Equal(t, lisp.LInt, res.Type)
	assert.Equal(t, 3, res.Int)
}

func TestEngine_ConditionalBreakpoint_True(t *testing.T) {
	e := New()
	e.Enable()

	env := newTestEnv(t, e)

	// Set a breakpoint with a condition that evaluates to true.
	e.Breakpoints().Set("test", 1, "true")

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause on true condition")

	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 3, res.Int)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout")
	}
}
