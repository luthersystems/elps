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
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 3, res.Int)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_ExceptionBreakpoint(t *testing.T) {
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

func TestEngine_StepOver(t *testing.T) {
	e := New(WithStopOnEntry(true))
	e.Enable()
	env := newTestEnv(t, e)

	// A program with a function call — step over should skip into f's body.
	program := `(defun f (x) (+ x 1)) (f 10)`

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Stop on entry.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not stop on entry")

	// Step over — should pause again at the same depth (next top-level expr).
	e.StepOver()

	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after step over")

	// Resume to finish.
	e.Resume()

	select {
	case res := <-resultCh:
		assert.Equal(t, lisp.LInt, res.Type, "expected int result, got %v", res)
		assert.Equal(t, 11, res.Int)
	case <-time.After(2 * time.Second):
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_StepOut(t *testing.T) {
	e := New()
	e.Enable()
	env := newTestEnv(t, e)

	// A program with a function call. Set breakpoint inside the function body
	// so we're at a non-zero stack depth when paused.
	program := `(defun f (x) (+ x 1)) (f 10)`
	// The breakpoint is on line 1 where all expressions reside. When the
	// interpreter enters function f and evaluates (+ x 1), it will hit this.
	e.Breakpoints().Set("test", 1, "")

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for first breakpoint hit.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at breakpoint")

	// Resume past the first hit (e.g., defun definition).
	e.Resume()

	// Wait for next pause (should be inside f's body at deeper stack depth).
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause again")

	// Step out — should pause at a lesser depth (back to caller).
	e.StepOut()

	// Should pause again after returning from f.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause after step out")

	// Resume to finish.
	e.Resume()

	// Keep resuming any additional breakpoint hits until program completes.
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
		assert.Equal(t, 11, res.Int)
	case <-time.After(5 * time.Second):
		close(resumeDone)
		if e.IsPaused() {
			e.Resume()
		}
		t.Fatal("timeout waiting for eval result")
	}
}

func TestEngine_ConditionalBreakpoint_ErrorInCondition(t *testing.T) {
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

	// Wait for first hit to confirm breakpoint works.
	require.Eventually(t, func() bool {
		return e.IsPaused()
	}, 2*time.Second, 10*time.Millisecond, "engine did not pause at first breakpoint")
	e.Resume()

	// Keep resuming until the program finishes or we've seen enough hits.
	for range 20 {
		time.Sleep(100 * time.Millisecond)
		if e.IsPaused() {
			e.Resume()
		}
	}

	select {
	case <-resultCh:
		// Program completed.
	case <-time.After(5 * time.Second):
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
	// Recursive countdown from 1000 gives the pause request time to take effect.
	program := "(defun countdown (n)\n" +
		"  (if (<= n 0)\n" +
		"    0\n" +
		"    (countdown (- n 1))))\n" +
		"(countdown 1000)"

	resultCh := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadString("test", program)
		resultCh <- res
	}()

	// Wait for eval goroutine to start executing, then request pause.
	require.Eventually(t, func() bool {
		return e.EvalCount() > 0
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
	e := New(WithSourceRoot("/my/project/dir"))
	assert.Equal(t, "/my/project/dir", e.SourceRoot())

	e2 := New()
	assert.Equal(t, "", e2.SourceRoot())
}

func TestEngine_NotifyExit(t *testing.T) {
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
	assert.Contains(t, outputs[0], "hit at n=")
}
