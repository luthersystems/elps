package debugger

import (
	"fmt"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
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
	t.Parallel()
	e := New()
	assert.False(t, e.IsEnabled())

	e.Enable()
	assert.True(t, e.IsEnabled())

	e.Disable()
	assert.False(t, e.IsEnabled())
}

func TestEngine_BreakpointPauseAndResume(t *testing.T) {
	t.Parallel()
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

	// Verify exactly one stopped event was sent.
	mu.Lock()
	assert.Equal(t, 1, len(events))
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
	t.Parallel()
	e := New(WithStopOnEntry(true))
	e.Enable()
	env := newTestEnv(t, e)

	resultCh := make(chan *lisp.LVal, 1)
	// Multi-line program: step-into from line 1 should advance to line 2
	// (line-level granularity skips sub-expressions on the same line).
	go func() {
		res := env.LoadString("test", "(+ 1 2)\n(+ 3 4)")
		resultCh <- res
	}()

	// Should stop on entry.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")

	// Verify paused state has valid expression with source location.
	_, entryExpr := e.PausedState()
	require.NotNil(t, entryExpr, "paused expression should not be nil")
	require.NotNil(t, entryExpr.Source, "paused expression should have source location")
	assert.Equal(t, 1, entryExpr.Source.Line, "should stop on entry at line 1")

	// Step into — should advance to line 2 (skipping sub-expressions on line 1).
	e.StepInto()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after step")

	// Verify we paused on a different line.
	_, stepExpr := e.PausedState()
	require.NotNil(t, stepExpr, "paused expression after step should not be nil")
	require.NotNil(t, stepExpr.Source, "paused expression after step should have source location")
	assert.Equal(t, 2, stepExpr.Source.Line, "step-into should advance to line 2")

	// Continue to finish.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 7, res.Int)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_ExceptionBreakpoint(t *testing.T) {
	t.Parallel()
	var stopReason StopReason
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped {
			mu.Lock()
			stopReason = evt.Reason
			mu.Unlock()
		}
	}))
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

	// Verify stop reason is exception.
	mu.Lock()
	assert.Equal(t, StopException, stopReason, "expected exception stop reason")
	mu.Unlock()

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
	t.Parallel()
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
	t.Parallel()
	e := New()
	// NOT enabled — hooks should all be no-ops.

	env := newTestEnv(t, e)

	// This should run without any debugging interference.
	res := env.LoadString("test", "(+ 1 2)")
	assert.Equal(t, lisp.LInt, res.Type)
	assert.Equal(t, 3, res.Int)
}

func TestEngine_ConditionalBreakpoint(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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

func TestEngine_ConditionalBreakpoint_RuntimeVariable(t *testing.T) {
	t.Parallel()
	// Verify that a conditional breakpoint can reference runtime-bound variables.
	// The condition (> n 2) should only fire when n > 2 during recursion.
	var hitCount int
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped && evt.Reason == StopBreakpoint {
			mu.Lock()
			hitCount++
			mu.Unlock()
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Recursive countdown from 4. Breakpoint on line 4 with condition (> n 2).
	// Line 4 executes 4 times (n=4,3,2,1). Condition (> n 2) is true for n=4,3 only.
	program := "(defun countdown (n)\n" +
		"  (if (<= n 0)\n" +
		"    0\n" +
		"    (countdown (- n 1))))\n" +
		"(countdown 4)"
	e.Breakpoints().Set("test", 4, "(> n 2)")

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Keep resuming until program finishes.
	resumeDone := make(chan struct{})
	go func() {
		ticker := time.NewTicker(10 * time.Millisecond)
		defer ticker.Stop()
		for {
			select {
			case <-resumeDone:
				return
			case <-ticker.C:
				if e.IsPaused() {
					e.Resume()
				}
			}
		}
	}()

	select {
	case res := <-resultCh:
		close(resumeDone)
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
	case <-time.After(5 * time.Second):
		close(resumeDone)
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}

	mu.Lock()
	assert.Equal(t, 2, hitCount,
		"breakpoint with condition (> n 2) should fire exactly twice (n=4 and n=3)")
	mu.Unlock()
}

func TestEngine_StepOver(t *testing.T) {
	t.Parallel()
	e := New()
	e.Enable()
	env := newTestEnv(t, e)

	// Program with nested function calls. Breakpoint inside outer function body.
	// Step-over should stay at the same stack depth, not enter inner function.
	program := "(defun inner (x) (+ x 100))\n" +
		"(defun outer (n)\n" +
		"  (inner n)\n" + // line 3: breakpoint here
		"  (+ n 1))\n" + // line 4: step-over should reach here
		"(outer 5)"
	e.Breakpoints().Set("test", 3, "")

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for breakpoint inside outer (line 3).
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")

	pausedEnv, expr := e.PausedState()
	require.NotNil(t, expr)
	require.NotNil(t, expr.Source)
	assert.Equal(t, 3, expr.Source.Line, "should pause on line 3")
	outerDepth := len(pausedEnv.Runtime.Stack.Frames)

	// Clear breakpoints so they don't re-fire.
	e.Breakpoints().ClearFile("test")

	// Step over (inner n) — should NOT enter inner's body.
	// Keep stepping over until we land on a different line at the same depth.
	e.StepOver()

	var lastExpr *lisp.LVal
	for range 20 {
		require.Eventually(t, func() bool {
			return e.IsPaused()
		}, 2*time.Second, 10*time.Millisecond, "engine did not pause after step over")

		pausedEnv, lastExpr = e.PausedState()
		require.NotNil(t, lastExpr)

		// Verify depth never went deeper than where we started (did NOT enter inner).
		currentDepth := len(pausedEnv.Runtime.Stack.Frames)
		assert.LessOrEqual(t, currentDepth, outerDepth,
			"step-over should not increase stack depth (entered nested function)")

		// If we've reached line 4, the step-over worked correctly.
		if lastExpr.Source != nil && lastExpr.Source.Line == 4 {
			break
		}

		// Keep stepping over sub-expressions on line 3.
		e.StepOver()
	}

	require.NotNil(t, lastExpr.Source)
	assert.Equal(t, 4, lastExpr.Source.Line,
		"step-over should eventually reach line 4 without entering inner")

	// Resume to finish.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 6, res.Int)
	case <-time.After(5 * time.Second):
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_StepOut(t *testing.T) {
	t.Parallel()
	e := New()
	e.Enable()
	env := newTestEnv(t, e)

	// Multi-line program: breakpoint inside function body (line 2),
	// step-out should return to caller scope. Need a top-level expression
	// AFTER (f 10) so step-out has somewhere to land.
	program := "(defun f (x)\n  (+ x 1))\n(f 10)\n(+ 1 1)"
	e.Breakpoints().Set("test", 2, "")

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for breakpoint hit inside f's body (line 2).
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")

	pausedEnv, expr := e.PausedState()
	require.NotNil(t, expr)
	require.NotNil(t, expr.Source)
	assert.Equal(t, 2, expr.Source.Line, "should be paused inside function body on line 2")

	// Record stack depth inside function.
	insideDepth := len(pausedEnv.Runtime.Stack.Frames)
	require.Greater(t, insideDepth, 0, "should have stack frames inside function call")

	// Clear breakpoints and step out — should return to caller.
	e.Breakpoints().ClearFile("test")
	e.StepOut()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after step out")

	// Verify we're back in the caller scope with lower stack depth.
	pausedEnv, expr = e.PausedState()
	require.NotNil(t, expr)
	outsideDepth := len(pausedEnv.Runtime.Stack.Frames)
	assert.Less(t, outsideDepth, insideDepth,
		"step-out should decrease stack depth (inside=%d, outside=%d)", insideDepth, outsideDepth)

	// Resume to finish.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 2, res.Int)
	case <-time.After(5 * time.Second):
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_ConditionalBreakpoint_ErrorInCondition(t *testing.T) {
	t.Parallel()
	// Regression: a conditional breakpoint whose condition errors should NOT
	// deadlock. The re-entrancy guard should prevent OnError from re-entering
	// the breakpoint check. The condition errors → treated as "not met" → no pause.
	e := New()
	e.Enable()
	e.Breakpoints().SetExceptionBreak(ExceptionBreakAll)
	// Use unbound symbol as condition — this produces an error through env.Errorf.
	e.Breakpoints().Set("test", 1, "undefined-sym-xyz")

	env := newTestEnv(t, e)

	// Run in a goroutine to avoid hanging if the re-entrancy guard fails.
	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 3, res.Int)
	case <-time.After(5 * time.Second):
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("re-entrancy guard failed: engine deadlocked on error in condition")
	}
}

func TestEngine_BreakpointInLoop(t *testing.T) {
	t.Parallel()
	// Verify that the lastContinuedKey suppression doesn't prevent a
	// breakpoint from re-firing on subsequent recursive iterations.
	// We use recursion because each recursive call evaluates the function body
	// starting at a different line than the breakpoint, which clears
	// lastContinuedKey between hits.
	var hitCount int
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped && evt.Reason == StopBreakpoint {
			mu.Lock()
			hitCount++
			mu.Unlock()
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Multi-line recursive program.
	// Line 1: defun
	// Line 2: if test
	// Line 3: base case
	// Line 4: recursive call (breakpoint here)
	// Line 5: initial call
	program := "(defun loop-fn (n total)\n" +
		"  (if (<= n 0)\n" +
		"    total\n" +
		"    (loop-fn (- n 1) (+ total 1))))\n" +
		"(loop-fn 3 0)"
	// Breakpoint on line 4 — the recursive call. Each recursion re-enters
	// the function body at line 2, clearing lastContinuedKey before line 4
	// is reached again.
	e.Breakpoints().Set("test", 4, "")

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Keep resuming until the program finishes.
	resumeDone := make(chan struct{})
	go func() {
		ticker := time.NewTicker(10 * time.Millisecond)
		defer ticker.Stop()
		for {
			select {
			case <-resumeDone:
				return
			case <-ticker.C:
				if e.IsPaused() {
					e.Resume()
				}
			}
		}
	}()

	select {
	case <-resultCh:
		close(resumeDone)
	case <-time.After(5 * time.Second):
		close(resumeDone)
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}

	mu.Lock()
	count := hitCount
	mu.Unlock()
	assert.Equal(t, 3, count, "breakpoint should fire exactly 3 times (once per recursive call where n > 0)")
}

func TestEngine_PausedState(t *testing.T) {
	t.Parallel()
	e := New(WithStopOnEntry(true))
	e.Enable()
	env := newTestEnv(t, e)

	// Before pausing, PausedState should return nil.
	pEnv, pExpr := e.PausedState()
	assert.Nil(t, pEnv)
	assert.Nil(t, pExpr)

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)

	// While paused, PausedState should return non-nil values with source info.
	pEnv, pExpr = e.PausedState()
	assert.NotNil(t, pEnv, "expected non-nil env while paused")
	require.NotNil(t, pExpr, "expected non-nil expr while paused")
	require.NotNil(t, pExpr.Source, "expected source location on paused expr")
	assert.Equal(t, "test", pExpr.Source.File, "expected paused expr from test source")

	e.Resume()

	select {
	case <-resultCh:
	case <-time.After(2 * time.Second):
		t.Fatal("timeout")
	}

	// After resume, PausedState should return nil again.
	pEnv, pExpr = e.PausedState()
	assert.Nil(t, pEnv)
	assert.Nil(t, pExpr)
}

func TestEngine_OnFunEntryReturn(t *testing.T) {
	t.Parallel()
	// OnFunEntry and OnFunReturn are currently no-ops but must not interfere
	// with program execution when the debugger is enabled.
	e := New()
	e.Enable()
	env := newTestEnv(t, e)

	// Run a program with function calls — the interpreter calls OnFunEntry/OnFunReturn.
	res := env.LoadString("test", `(defun add (a b) (+ a b)) (add 10 20)`)
	assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
	assert.Equal(t, 30, res.Int)
}

func TestEngine_SetEventCallback(t *testing.T) {
	t.Parallel()
	var called bool
	var mu sync.Mutex

	e := New(WithStopOnEntry(true))
	e.Enable()

	// Replace the callback after construction.
	e.SetEventCallback(func(evt Event) {
		mu.Lock()
		defer mu.Unlock()
		called = true
	})

	env := newTestEnv(t, e)

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", "(+ 1 2)")
		resultCh <- res
	}()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond)

	mu.Lock()
	assert.True(t, called, "replacement callback should have fired")
	mu.Unlock()

	e.Resume()

	select {
	case <-resultCh:
	case <-time.After(2 * time.Second):
		t.Fatal("timeout")
	}
}

func TestEngine_ReadyCh(t *testing.T) {
	t.Parallel()
	e := New()

	// ReadyCh should not be closed before SignalReady.
	select {
	case <-e.ReadyCh():
		t.Fatal("ReadyCh should not be closed before SignalReady")
	default:
		// expected
	}

	// Signal ready.
	e.SignalReady()

	// ReadyCh should now be closed.
	select {
	case <-e.ReadyCh():
		// expected
	case <-time.After(time.Second):
		t.Fatal("ReadyCh should be closed after SignalReady")
	}

	// Calling SignalReady again should not panic.
	e.SignalReady()
}

func TestEngine_ReadyCh_WithTimeout(t *testing.T) {
	t.Parallel()
	e := New()

	// Simulate waiting with a timeout — should timeout since we don't signal.
	timer := time.NewTimer(50 * time.Millisecond)
	defer timer.Stop()

	select {
	case <-e.ReadyCh():
		t.Fatal("ReadyCh should not have been ready")
	case <-timer.C:
		// expected: timed out waiting
	}

	// Now signal ready after a short delay.
	go func() {
		time.Sleep(20 * time.Millisecond)
		e.SignalReady()
	}()

	select {
	case <-e.ReadyCh():
		// expected
	case <-time.After(time.Second):
		t.Fatal("ReadyCh should have been closed after SignalReady")
	}
}

func TestEngine_RequestPause(t *testing.T) {
	t.Parallel()
	var stopReason StopReason
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped {
			mu.Lock()
			stopReason = evt.Reason
			mu.Unlock()
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Use a long-running program so we have time to request a pause.
	// Large countdown ensures the program is still running when RequestPause
	// takes effect — even on slow CI machines.
	program := "(defun countdown (n)\n" +
		"  (if (<= n 0)\n" +
		"    0\n" +
		"    (countdown (- n 1))))\n" +
		"(countdown 100000)"

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for eval goroutine to be well into execution, then request pause.
	// We wait for multiple evals (not just 1) so the program is solidly running
	// and won't finish before RequestPause takes effect.
	require.Eventually(t, func() bool {
		return e.EvalCount() > 10
	}, 2*time.Second, time.Millisecond, "eval goroutine did not start")
	e.RequestPause()

	// The engine should pause.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after RequestPause")

	// Verify stop reason is "pause".
	mu.Lock()
	assert.Equal(t, StopPause, stopReason, "expected pause stop reason")
	mu.Unlock()

	// Resume to let the program finish.
	e.Resume()

	// Keep resuming if needed (recursive calls may re-trigger).
	resumeDone := make(chan struct{})
	go func() {
		ticker := time.NewTicker(10 * time.Millisecond)
		defer ticker.Stop()
		for {
			select {
			case <-resumeDone:
				return
			case <-ticker.C:
				if e.IsPaused() {
					e.Resume()
				}
			}
		}
	}()

	select {
	case <-resultCh:
		close(resumeDone)
	case <-time.After(5 * time.Second):
		close(resumeDone)
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_FunctionBreakpoint(t *testing.T) {
	t.Parallel()
	var stopReason StopReason
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped {
			mu.Lock()
			stopReason = evt.Reason
			mu.Unlock()
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Set a function breakpoint on "add".
	e.SetFunctionBreakpoints([]string{"add"})

	program := `(defun add (a b) (+ a b)) (add 10 20)`

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Should pause when "add" is entered.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause on function breakpoint")

	// Verify stop reason is "function breakpoint".
	mu.Lock()
	assert.Equal(t, StopFunctionBreakpoint, stopReason)
	mu.Unlock()

	// Resume to let the program finish.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 30, res.Int)
	case <-time.After(2 * time.Second):
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_FunctionBreakpoint_QualifiedName(t *testing.T) {
	t.Parallel()
	var stopped bool
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped {
			mu.Lock()
			stopped = true
			mu.Unlock()
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Set a function breakpoint on a qualified name "user:myfn".
	e.SetFunctionBreakpoints([]string{"user:myfn"})

	program := `(defun myfn (x) (+ x 1)) (myfn 5)`

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Should pause when "myfn" is entered (matched via qualified name).
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause on qualified function breakpoint")

	mu.Lock()
	assert.True(t, stopped)
	mu.Unlock()

	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type)
		assert.Equal(t, 6, res.Int)
	case <-time.After(2 * time.Second):
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout")
	}
}

func TestEngine_FunctionBreakpoint_NoMatch(t *testing.T) {
	t.Parallel()
	e := New()
	e.Enable()

	env := newTestEnv(t, e)

	// Set a function breakpoint on a name that doesn't exist in the program.
	e.SetFunctionBreakpoints([]string{"nonexistent"})

	// Should run without pausing.
	res := env.LoadString("test", `(defun add (a b) (+ a b)) (add 10 20)`)
	assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
	assert.Equal(t, 30, res.Int)
}

func TestEngine_SourceRoot(t *testing.T) {
	t.Parallel()
	e := New(WithSourceRoot("/my/project/dir"))
	assert.Equal(t, "/my/project/dir", e.SourceRoot())

	e2 := New()
	assert.Equal(t, "", e2.SourceRoot())
}

func TestEngine_NotifyExit(t *testing.T) {
	t.Parallel()
	var events []Event
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		mu.Lock()
		defer mu.Unlock()
		events = append(events, evt)
	}))

	// Call NotifyExit and verify the event is fired.
	e.NotifyExit(42)

	mu.Lock()
	require.Len(t, events, 1, "expected exactly one event")
	assert.Equal(t, EventExited, events[0].Type)
	assert.Equal(t, 42, events[0].ExitCode)
	mu.Unlock()

	// Calling with exit code 0 should also work.
	e.NotifyExit(0)

	mu.Lock()
	require.Len(t, events, 2)
	assert.Equal(t, EventExited, events[1].Type)
	assert.Equal(t, 0, events[1].ExitCode)
	mu.Unlock()
}

func TestEngine_HitCountBreakpoint(t *testing.T) {
	t.Parallel()
	var stopCount int
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped && evt.Reason == StopBreakpoint {
			mu.Lock()
			stopCount++
			mu.Unlock()
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Recursive countdown from 5. Set a hit count breakpoint that fires on hit ==3.
	program := "(defun countdown (n)\n" +
		"  (if (<= n 0)\n" +
		"    0\n" +
		"    (countdown (- n 1))))\n" +
		"(countdown 5)"
	e.Breakpoints().SetForFileSpecs("test", []BreakpointSpec{
		{Line: 4, HitCondition: "==3"},
	})

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Should pause exactly once (on the 3rd hit).
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause on hit count")
	e.Resume()

	// Keep resuming any additional hits.
	resumeDone := make(chan struct{})
	go func() {
		ticker := time.NewTicker(10 * time.Millisecond)
		defer ticker.Stop()
		for {
			select {
			case <-resumeDone:
				return
			case <-ticker.C:
				if e.IsPaused() {
					e.Resume()
				}
			}
		}
	}()

	select {
	case <-resultCh:
		close(resumeDone)
	case <-time.After(5 * time.Second):
		close(resumeDone)
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout")
	}

	// Hit count ==3 fires only once.
	mu.Lock()
	assert.Equal(t, 1, stopCount, "breakpoint should fire exactly once for ==3")
	mu.Unlock()
}

func TestEngine_LogPoint(t *testing.T) {
	t.Parallel()
	var outputs []string
	var stopCount int
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		mu.Lock()
		defer mu.Unlock()
		switch evt.Type {
		case EventOutput:
			outputs = append(outputs, evt.Output)
		case EventStopped:
			stopCount++
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Simple program with a breakpoint that has a log message.
	program := "(defun add (a b)\n  (+ a b))\n(add 3 4)"
	e.Breakpoints().SetForFileSpecs("test", []BreakpointSpec{
		{Line: 2, LogMessage: "adding {a} + {b}"},
	})

	res := env.LoadString("test", program)
	assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
	assert.Equal(t, 7, res.Int)

	mu.Lock()
	defer mu.Unlock()
	// Log point should have emitted output but NOT paused.
	assert.Equal(t, 0, stopCount, "log point should not pause")
	require.Len(t, outputs, 1, "expected exactly one log point output")
	assert.Equal(t, "adding 3 + 4", outputs[0])
}

func TestEngine_LogPoint_WithHitCondition(t *testing.T) {
	t.Parallel()
	var outputs []string
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		mu.Lock()
		defer mu.Unlock()
		if evt.Type == EventOutput {
			outputs = append(outputs, evt.Output)
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Recursive countdown from 4. Log point with hit condition ==2.
	// The recursive call on line 4 executes 4 times (n=4,3,2,1).
	// With hit condition ==2, only the 2nd hit should emit output.
	program := "(defun countdown (n)\n" +
		"  (if (<= n 0)\n" +
		"    0\n" +
		"    (countdown (- n 1))))\n" +
		"(countdown 4)"
	e.Breakpoints().SetForFileSpecs("test", []BreakpointSpec{
		{Line: 4, HitCondition: "==2", LogMessage: "hit at n={n}"},
	})

	res := env.LoadString("test", program)
	assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)

	mu.Lock()
	defer mu.Unlock()
	require.Len(t, outputs, 1, "log point should emit exactly once for ==2")
	// Line 4 has multiple sub-expressions (e.g. (countdown (- n 1)), (- n 1)),
	// each triggering a separate hit count increment. The 2nd match happens
	// during the first recursive call (n=4).
	assert.Equal(t, "hit at n=4", outputs[0])
}

func TestEngine_EvalInContextWhilePaused(t *testing.T) {
	t.Parallel()
	e := New()
	e.Enable()

	env := newTestEnv(t, e)

	// Set a breakpoint on line 4 (the recursive branch of factorial).
	e.Breakpoints().Set("test", 4, "")

	program := "(defun factorial (n)\n" +
		"  (if (<= n 1)\n" +
		"    1\n" +
		"    (* n (factorial (- n 1)))))\n" +
		"(factorial 5)"

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for the engine to pause at the breakpoint.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")

	// Evaluate an expression while paused. This previously deadlocked
	// because OnEval would fire during evaluation and call WaitIfPaused.
	pausedEnv, _ := e.PausedState()
	require.NotNil(t, pausedEnv, "paused env should not be nil")

	result := e.EvalInContext(pausedEnv, "(+ 1 2)")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LInt, result.Type, "expected int result from eval, got %v (type %s)", result, result.Type)
	assert.Equal(t, 3, result.Int)

	// Evaluate with a variable from the paused scope.
	result = e.EvalInContext(pausedEnv, "n")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LInt, result.Type, "expected int for 'n', got %v (type %s)", result, result.Type)

	// Resume and drain remaining breakpoint hits.
	e.Resume()
	for waitForPause(t, e, 500*time.Millisecond) {
		e.Resume()
	}

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 120, res.Int)
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

func TestInspectFunctionLocals(t *testing.T) {
	t.Parallel()
	var capturedEnv *lisp.LEnv
	var mu sync.Mutex

	e := New(WithEventCallback(func(evt Event) {
		if evt.Type == EventStopped {
			mu.Lock()
			capturedEnv = evt.Env
			mu.Unlock()
		}
	}))
	e.Enable()

	env := newTestEnv(t, e)

	// Set a breakpoint inside the if form.
	e.Breakpoints().Set("test", 4, "")

	program := "(defun factorial (n)\n" +
		"  (if (<= n 1)\n" +
		"    1\n" +
		"    (* n (factorial (- n 1)))))\n" +
		"(factorial 5)"

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause")

	mu.Lock()
	testEnv := capturedEnv
	mu.Unlock()
	require.NotNil(t, testEnv)

	// InspectLocals on the immediate env may be empty (paused at sub-expression).
	// InspectFunctionLocals should find 'n' from the enclosing function scope.
	funcLocals := InspectFunctionLocals(testEnv)
	var names []string
	for _, b := range funcLocals {
		names = append(names, b.Name)
	}
	assert.Contains(t, names, "n",
		"InspectFunctionLocals should find 'n' from enclosing function scope, got: %v", names)

	// Resume and drain.
	e.Resume()
	for waitForPause(t, e, 500*time.Millisecond) {
		e.Resume()
	}

	select {
	case <-resultCh:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

// waitForPause waits for the engine to enter paused state, returning true
// if it paused within the timeout, false otherwise.
func waitForPause(t *testing.T, e *Engine, timeout time.Duration) bool {
	t.Helper()
	deadline := time.After(timeout)
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-deadline:
			return false
		case <-ticker.C:
			if e.IsPaused() {
				return true
			}
		}
	}
}

func TestEngine_RegisterFormatter(t *testing.T) {
	t.Parallel()

	type TestStruct struct{ Value int }
	typeName := fmt.Sprintf("%T", TestStruct{})

	e := New()

	// No formatter registered — FormatNative returns empty string.
	assert.Empty(t, e.FormatNative(TestStruct{Value: 42}))
	assert.Nil(t, e.NativeChildren(TestStruct{Value: 42}))

	// Register a formatter.
	e.RegisterFormatter(typeName, FormatterFunc(func(v any) string {
		s, ok := v.(TestStruct)
		if !ok {
			return ""
		}
		return fmt.Sprintf("TestStruct(%d)", s.Value)
	}))

	assert.Equal(t, "TestStruct(42)", e.FormatNative(TestStruct{Value: 42}))
	assert.Nil(t, e.NativeChildren(TestStruct{Value: 42}),
		"FormatterFunc should return nil children")

	// nil value returns empty.
	assert.Empty(t, e.FormatNative(nil))
	assert.Nil(t, e.NativeChildren(nil))
}

func TestEngine_WithFormatters(t *testing.T) {
	t.Parallel()

	type MyType struct{}
	typeName := fmt.Sprintf("%T", MyType{})

	e := New(WithFormatters(map[string]VariableFormatter{
		typeName: FormatterFunc(func(v any) string { return "custom" }),
	}))

	assert.Equal(t, "custom", e.FormatNative(MyType{}))
}

func TestEngine_NativeChildren_WithChildren(t *testing.T) {
	t.Parallel()

	typeName := fmt.Sprintf("%T", Container{})

	formatter := &testFormatterWithChildren{}
	e := New(WithFormatters(map[string]VariableFormatter{
		typeName: formatter,
	}))

	c := Container{Items: []string{"a", "b"}}
	children := e.NativeChildren(c)
	require.Len(t, children, 2)
	assert.Equal(t, "Items[0]", children[0].Name)
	assert.Equal(t, "a", children[0].Value.Str)
	assert.Equal(t, "Items[1]", children[1].Name)
	assert.Equal(t, "b", children[1].Value.Str)
}

// Container is a test type for TestEngine_NativeChildren_WithChildren.
type Container struct{ Items []string }

type testFormatterWithChildren struct{}

func (f *testFormatterWithChildren) FormatValue(v any) string {
	c, ok := v.(Container)
	if !ok {
		return "<container>"
	}
	return fmt.Sprintf("<container len=%d>", len(c.Items))
}

func (f *testFormatterWithChildren) Children(v any) []NativeChild {
	c, ok := v.(Container)
	if !ok {
		return nil
	}
	children := make([]NativeChild, len(c.Items))
	for i, item := range c.Items {
		children[i] = NativeChild{
			Name:  fmt.Sprintf("Items[%d]", i),
			Value: lisp.String(item),
		}
	}
	return children
}

func TestEngine_SourceRefRegistry(t *testing.T) {
	t.Parallel()
	e := New()

	// No refs initially.
	_, ok := e.GetSourceRef(1)
	assert.False(t, ok, "should not find non-existent ref")

	// Allocate a ref.
	ref1 := e.AllocSourceRef("test.lisp", "(+ 1 2)")
	assert.Greater(t, ref1, 0)

	content, ok := e.GetSourceRef(ref1)
	assert.True(t, ok)
	assert.Equal(t, "(+ 1 2)", content)

	// Allocate another ref — IDs should be unique.
	ref2 := e.AllocSourceRef("other.lisp", "(defun f () nil)")
	assert.NotEqual(t, ref1, ref2)

	content2, ok := e.GetSourceRef(ref2)
	assert.True(t, ok)
	assert.Equal(t, "(defun f () nil)", content2)

	// First ref still accessible.
	content1, ok := e.GetSourceRef(ref1)
	assert.True(t, ok)
	assert.Equal(t, "(+ 1 2)", content1)
}

func TestEngine_StepOutTailPosition(t *testing.T) {
	t.Parallel()
	e := New()
	e.Enable()
	env := newTestEnv(t, e)

	// All calls are in tail position: outer → middle → inner.
	// Step-out from inner should pause at the call site in middle.
	program := "(defun inner (x)\n" + //  1
		"  (+ x 100))\n" + //  2
		"(defun middle (x)\n" + //  3
		"  (inner (* x 2)))\n" + //  4
		"(defun outer ()\n" + //  5
		"  (middle 5))\n" + //  6
		"(outer)\n" //  7

	// Set a breakpoint inside inner (line 2).
	e.Breakpoints().Set("test", 2, "")

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for breakpoint inside inner.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")

	pausedEnv, expr := e.PausedState()
	require.NotNil(t, expr)
	require.NotNil(t, expr.Source)
	assert.Equal(t, 2, expr.Source.Line, "should pause on line 2")
	innerDepth := len(pausedEnv.Runtime.Stack.Frames)

	// Clear breakpoints and step out.
	e.Breakpoints().ClearFile("test")
	e.StepOut()

	// Should pause at the call site (NOT run to completion).
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "step-out from tail position should pause")

	pausedEnv, expr = e.PausedState()
	require.NotNil(t, expr)
	require.NotNil(t, expr.Source, "paused expression should have source location")
	assert.Equal(t, "test", expr.Source.File)
	assert.NotEqual(t, 2, expr.Source.Line,
		"should NOT still be on inner's body line after step-out")
	outsideDepth := len(pausedEnv.Runtime.Stack.Frames)
	assert.Less(t, outsideDepth, innerDepth,
		"step-out should decrease stack depth (inner=%d, outside=%d)", innerDepth, outsideDepth)

	// Resume to finish.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 110, res.Int) // inner(5*2) = 10+100 = 110
	case <-time.After(5 * time.Second):
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_StepOutSafetyNet(t *testing.T) {
	t.Parallel()
	e := New()
	e.Enable()
	env := newTestEnv(t, e)

	// Simulate the safety net path: stepOutReturned is set (by OnFunReturn)
	// but AfterFunCall did not consume it (e.g., the call-site expression
	// had no source location). The next OnEval should catch it.
	e.stepper.SetStepOut(3)
	e.stepOutReturned = true

	// Create a minimal expression with a source location for OnEval.
	expr := lisp.Int(42)
	expr.Source = &token.Location{File: "test", Line: 1}

	// OnEval should return true (wants to pause) and clear the flag.
	shouldPause := e.OnEval(env, expr)
	assert.True(t, shouldPause, "safety net should trigger pause when stepOutReturned is set")
	assert.False(t, e.stepOutReturned, "stepOutReturned should be cleared")
	assert.Equal(t, StepNone, e.stepper.Mode(), "stepper should reset to StepNone")
}

func TestEngine_SourceLibrary(t *testing.T) {
	t.Parallel()

	// No library by default.
	e := New()
	assert.Nil(t, e.SourceLibrary())

	// With library.
	lib := &lisp.FSLibrary{}
	e2 := New(WithSourceLibrary(lib))
	assert.Equal(t, lib, e2.SourceLibrary())

	// SetSourceLibrary after construction.
	e3 := New()
	assert.Nil(t, e3.SourceLibrary())
	lib2 := &lisp.FSLibrary{}
	e3.SetSourceLibrary(lib2)
	assert.Equal(t, lib2, e3.SourceLibrary())
}
