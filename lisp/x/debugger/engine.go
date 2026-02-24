// Copyright © 2018 The ELPS authors

// Package debugger implements the ELPS debugger engine (Layer 1).
// It provides breakpoint management, stepping, variable inspection,
// and debug evaluation without any external protocol dependencies.
//
// The engine implements the lisp.Debugger interface and communicates
// with external consumers (such as a DAP server) through event
// callbacks and a channel-based pause/resume mechanism.
//
// Concurrency model: The ELPS eval goroutine calls the Debugger hook
// methods (OnEval, WaitIfPaused, etc.). When paused, it blocks on a
// channel. The external consumer (DAP server goroutine) sends commands
// via Resume/StepInto/StepOver/StepOut. This is thread-safe by design.
package debugger

import (
	"sync"

	"github.com/luthersystems/elps/lisp"
)

// EventType identifies the kind of debug event.
type EventType int

const (
	// EventStopped indicates execution has paused (breakpoint, step, exception).
	EventStopped EventType = iota
	// EventContinued indicates execution has resumed.
	EventContinued
	// EventExited indicates the program has finished.
	EventExited
	// EventOutput indicates the program produced output.
	EventOutput
)

// StopReason describes why execution paused.
type StopReason string

const (
	StopBreakpoint StopReason = "breakpoint"
	StopStep       StopReason = "step"
	StopException  StopReason = "exception"
	StopEntry      StopReason = "entry"
	StopPause      StopReason = "pause"
)

// Event is sent to the event callback when the debugger state changes.
type Event struct {
	Type   EventType
	Reason StopReason
	Env    *lisp.LEnv
	Expr   *lisp.LVal
	BP     *Breakpoint // non-nil for breakpoint stops
}

// EventCallback is called when the debugger state changes. It runs on
// the eval goroutine, so it must not block.
type EventCallback func(Event)

// Engine implements lisp.Debugger and provides the core debugging
// primitives: breakpoints, stepping, variable inspection, and debug eval.
type Engine struct {
	breakpoints *BreakpointStore
	stepper     *Stepper
	onEvent     EventCallback

	mu                  sync.Mutex
	enabled             bool
	stopOnEntry         bool
	evaluatingCondition bool   // re-entrancy guard for conditional breakpoints
	lastContinuedKey    string // suppress breakpoint re-hit after continue on same line

	// pauseCh is used by the eval goroutine to block in WaitIfPaused.
	// The DAP server sends DebugAction values to resume execution.
	pauseCh chan lisp.DebugAction

	// pausedEnv/pausedExpr hold the state when paused, protected by mu.
	pausedEnv  *lisp.LEnv
	pausedExpr *lisp.LVal

	// readyCh is closed when SignalReady is called, indicating that the
	// external consumer (e.g., DAP client) has finished configuration.
	// Embedders can wait on ReadyCh() before starting evaluation.
	readyCh   chan struct{}
	readyOnce sync.Once
}

// Verify Engine implements lisp.Debugger at compile time.
var _ lisp.Debugger = (*Engine)(nil)

// Option configures an Engine.
type Option func(*Engine)

// WithEventCallback sets the function called on debugger state changes.
func WithEventCallback(cb EventCallback) Option {
	return func(e *Engine) {
		e.onEvent = cb
	}
}

// WithStopOnEntry makes the debugger pause before the first expression.
func WithStopOnEntry(stop bool) Option {
	return func(e *Engine) {
		e.stopOnEntry = stop
	}
}

// New creates a new debugger engine.
func New(opts ...Option) *Engine {
	e := &Engine{
		breakpoints: NewBreakpointStore(),
		stepper:     NewStepper(),
		pauseCh:     make(chan lisp.DebugAction, 1),
		readyCh:     make(chan struct{}),
	}
	for _, opt := range opts {
		opt(e)
	}
	return e
}

// Breakpoints returns the breakpoint store for external management
// (e.g., by a DAP server handling setBreakpoints requests).
func (e *Engine) Breakpoints() *BreakpointStore {
	return e.breakpoints
}

// SetEventCallback sets or replaces the event callback. It is safe to call
// after construction (e.g., when a DAP handler wires itself up).
func (e *Engine) SetEventCallback(cb EventCallback) {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.onEvent = cb
}

// Enable activates the debugger. Hook calls are only made when enabled.
func (e *Engine) Enable() {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.enabled = true
}

// Disable deactivates the debugger without detaching it.
func (e *Engine) Disable() {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.enabled = false
}

// IsEnabled implements lisp.Debugger.
func (e *Engine) IsEnabled() bool {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.enabled
}

// IsPaused returns true if the eval goroutine is currently blocked in
// WaitIfPaused.
func (e *Engine) IsPaused() bool {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.pausedEnv != nil
}

// PausedState returns the env and expr where execution is paused, or
// nil if not paused.
func (e *Engine) PausedState() (*lisp.LEnv, *lisp.LVal) {
	e.mu.Lock()
	defer e.mu.Unlock()
	return e.pausedEnv, e.pausedExpr
}

// SignalReady signals that the external consumer has finished its
// configuration (e.g., DAP configurationDone). Embedders waiting on
// ReadyCh() will be unblocked. Safe to call multiple times.
func (e *Engine) SignalReady() {
	e.readyOnce.Do(func() {
		close(e.readyCh)
	})
}

// ReadyCh returns a channel that is closed when SignalReady is called.
// Embedders can select on this to wait for the DAP client to finish
// setting breakpoints before starting evaluation.
func (e *Engine) ReadyCh() <-chan struct{} {
	return e.readyCh
}

// OnEval implements lisp.Debugger. Called before each expression with
// a real source location.
func (e *Engine) OnEval(env *lisp.LEnv, expr *lisp.LVal) bool {
	e.mu.Lock()
	if e.evaluatingCondition {
		e.mu.Unlock()
		return false
	}

	// Clear lastContinuedKey when we move to a different source line.
	// This allows breakpoints to re-fire when the program loops back.
	if e.lastContinuedKey != "" && expr.Source != nil {
		key := breakpointKey(expr.Source.File, expr.Source.Line)
		if key != e.lastContinuedKey {
			e.lastContinuedKey = ""
		}
	}

	// Check stop-on-entry (first expression only).
	if e.stopOnEntry {
		e.stopOnEntry = false
		e.mu.Unlock()
		return true
	}
	e.mu.Unlock()

	// Check stepping.
	depth := len(env.Runtime.Stack.Frames)
	if e.stepper.ShouldPause(depth) {
		return true
	}

	// Check breakpoints.
	bp := e.breakpoints.Match(expr.Source)
	if bp == nil {
		return false
	}

	// Suppress re-hit on the same line after continue.
	e.mu.Lock()
	if e.lastContinuedKey == bp.key() {
		e.mu.Unlock()
		return false
	}
	e.mu.Unlock()

	// Evaluate condition if present.
	if bp.Condition != "" {
		e.mu.Lock()
		e.evaluatingCondition = true
		e.mu.Unlock()
		result := EvalCondition(env, bp.Condition)
		e.mu.Lock()
		e.evaluatingCondition = false
		e.mu.Unlock()
		if !result {
			return false
		}
	}
	return true
}

// WaitIfPaused implements lisp.Debugger. Blocks the eval goroutine
// until the DAP server sends a resume command.
func (e *Engine) WaitIfPaused(env *lisp.LEnv, expr *lisp.LVal) lisp.DebugAction {
	// Determine stop reason.
	reason := StopStep
	bp := e.breakpoints.Match(expr.Source)
	if bp != nil {
		reason = StopBreakpoint
	}
	if expr.Type == lisp.LError {
		reason = StopException
	}

	e.mu.Lock()
	if e.stopOnEntry {
		reason = StopEntry
		e.stopOnEntry = false
	}
	e.pausedEnv = env
	e.pausedExpr = expr
	e.mu.Unlock()

	// Notify the DAP server that we're paused.
	e.mu.Lock()
	cb := e.onEvent
	e.mu.Unlock()
	if cb != nil {
		cb(Event{
			Type:   EventStopped,
			Reason: reason,
			Env:    env,
			Expr:   expr,
			BP:     bp,
		})
	}

	// Block until a resume command arrives.
	action := <-e.pauseCh

	e.mu.Lock()
	e.pausedEnv = nil
	e.pausedExpr = nil
	e.mu.Unlock()

	// On continue, suppress breakpoint re-hit on the same source line.
	if action == lisp.DebugContinue && expr.Source != nil {
		e.mu.Lock()
		e.lastContinuedKey = breakpointKey(expr.Source.File, expr.Source.Line)
		e.mu.Unlock()
	}

	// Configure stepper based on action.
	depth := len(env.Runtime.Stack.Frames)
	switch action {
	case lisp.DebugStepInto:
		e.stepper.SetStepInto()
	case lisp.DebugStepOver:
		e.stepper.SetStepOver(depth)
	case lisp.DebugStepOut:
		e.stepper.SetStepOut(depth)
	default:
		e.stepper.Reset()
	}

	// Notify continued.
	e.mu.Lock()
	cb = e.onEvent
	e.mu.Unlock()
	if cb != nil {
		cb(Event{Type: EventContinued})
	}
	return action
}

// OnFunEntry implements lisp.Debugger.
func (e *Engine) OnFunEntry(env *lisp.LEnv, fun *lisp.LVal, fenv *lisp.LEnv) {
	// Currently used for stack tracking. Future: function breakpoints.
}

// OnFunReturn implements lisp.Debugger.
func (e *Engine) OnFunReturn(env *lisp.LEnv, fun, result *lisp.LVal) {
	// Currently a no-op. Future: watch expressions on return values.
}

// OnError implements lisp.Debugger. Called when an error condition is
// created. Returns true if execution should pause.
func (e *Engine) OnError(env *lisp.LEnv, lerr *lisp.LVal) bool {
	e.mu.Lock()
	if e.evaluatingCondition {
		e.mu.Unlock()
		return false
	}
	e.mu.Unlock()
	return e.breakpoints.ExceptionBreak() == ExceptionBreakAll
}

// Resume sends a Continue action to the paused eval goroutine.
func (e *Engine) Resume() {
	e.pauseCh <- lisp.DebugContinue
}

// StepInto sends a StepInto action to the paused eval goroutine.
func (e *Engine) StepInto() {
	e.pauseCh <- lisp.DebugStepInto
}

// StepOver sends a StepOver action to the paused eval goroutine.
func (e *Engine) StepOver() {
	e.pauseCh <- lisp.DebugStepOver
}

// StepOut sends a StepOut action to the paused eval goroutine.
func (e *Engine) StepOut() {
	e.pauseCh <- lisp.DebugStepOut
}

// Disconnect atomically disables the debugger and resumes execution if paused.
func (e *Engine) Disconnect() {
	e.mu.Lock()
	e.enabled = false
	paused := e.pausedEnv != nil
	e.mu.Unlock()
	if paused {
		e.Resume()
	}
}
