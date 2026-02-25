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
	"fmt"
	"sync"
	"sync/atomic"

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
	StopBreakpoint         StopReason = "breakpoint"
	StopStep               StopReason = "step"
	StopException          StopReason = "exception"
	StopEntry              StopReason = "entry"
	StopPause              StopReason = "pause"
	StopFunctionBreakpoint StopReason = "function breakpoint"
)

// Event is sent to the event callback when the debugger state changes.
type Event struct {
	Type     EventType
	Reason   StopReason
	ExitCode int // set for EventExited
	Output   string       // set for EventOutput (log points)
	Env      *lisp.LEnv
	Expr     *lisp.LVal
	BP       *Breakpoint // non-nil for breakpoint stops
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
	sourceRoot  string // absolute path prefix for resolving relative Source.Path values
	evalCount   atomic.Int64

	mu                  sync.Mutex
	enabled             bool
	stopOnEntry         bool
	pauseRequested      bool              // set by RequestPause(), cleared in WaitIfPaused
	pauseReason         StopReason        // reason for pauseRequested (StopPause or StopFunctionBreakpoint)
	evaluatingCondition bool              // re-entrancy guard for conditional breakpoints
	lastContinuedKey    string            // suppress breakpoint re-hit after continue on same line
	funBreakpoints      map[string]string // qualified name → user-provided name

	stepOutReturned bool // set by OnFunReturn when step-out condition detected pre-pop

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

	// formatters maps Go type names (fmt.Sprintf("%T", v)) to custom
	// formatters for LNative values. Write-once at startup, read-only
	// during debugging. Protected by mu.
	formatters map[string]VariableFormatter

	// sourceLib provides access to source files for the source request
	// handler. Set once via WithSourceLibrary. Read-only during debugging.
	sourceLib lisp.SourceLibrary

	// sourceRefs maps integer reference IDs to source content for virtual
	// sources (e.g., go:embed files served via DAP source request).
	sourceRefs    map[int]sourceRefEntry
	nextSourceRef int
}

type sourceRefEntry struct {
	name    string
	content string
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

// WithSourceRoot sets an absolute directory path used to resolve relative
// Source.Path values into absolute paths. This allows DAP clients (VS Code)
// to open source files from stack frames. Embedders should pass the root
// directory of the ELPS source files (e.g., the phylum directory).
func WithSourceRoot(dir string) Option {
	return func(e *Engine) {
		e.sourceRoot = dir
	}
}

// SourceRoot returns the configured source root directory, or empty string
// if not set.
func (e *Engine) SourceRoot() string {
	return e.sourceRoot
}

// WithFormatters sets the custom native type formatters for LNative values.
// Keys are Go type names as returned by fmt.Sprintf("%T", value).
func WithFormatters(fmts map[string]VariableFormatter) Option {
	return func(e *Engine) {
		e.formatters = fmts
	}
}

// WithSourceLibrary sets the source library used to serve source content
// for the DAP source request handler.
func WithSourceLibrary(lib lisp.SourceLibrary) Option {
	return func(e *Engine) {
		e.sourceLib = lib
	}
}

// RegisterFormatter registers a custom formatter for a native Go type.
// typeName should match fmt.Sprintf("%T", value) for the values you want
// to format. Safe to call before debugging starts.
func (e *Engine) RegisterFormatter(typeName string, f VariableFormatter) {
	e.mu.Lock()
	defer e.mu.Unlock()
	if e.formatters == nil {
		e.formatters = make(map[string]VariableFormatter)
	}
	e.formatters[typeName] = f
}

// FormatNative returns a formatted string for a native Go value using
// the registered formatter. Returns empty string if no formatter is
// registered for the value's type.
func (e *Engine) FormatNative(v any) string {
	if v == nil {
		return ""
	}
	e.mu.Lock()
	f := e.formatters[fmt.Sprintf("%T", v)]
	e.mu.Unlock()
	if f == nil {
		return ""
	}
	return f.FormatValue(v)
}

// NativeChildren returns the expandable child bindings for a native Go
// value using the registered formatter. Returns nil if no formatter is
// registered or the formatter returns no children.
func (e *Engine) NativeChildren(v any) []NativeChild {
	if v == nil {
		return nil
	}
	e.mu.Lock()
	f := e.formatters[fmt.Sprintf("%T", v)]
	e.mu.Unlock()
	if f == nil {
		return nil
	}
	return f.Children(v)
}

// SourceLibrary returns the configured source library, or nil if not set.
func (e *Engine) SourceLibrary() lisp.SourceLibrary {
	return e.sourceLib
}

// AllocSourceRef allocates a source reference ID for virtual source content.
// Returns the reference ID that can be used in DAP source requests.
func (e *Engine) AllocSourceRef(name, content string) int {
	e.mu.Lock()
	defer e.mu.Unlock()
	if e.sourceRefs == nil {
		e.sourceRefs = make(map[int]sourceRefEntry)
	}
	e.nextSourceRef++
	id := e.nextSourceRef
	e.sourceRefs[id] = sourceRefEntry{name: name, content: content}
	return id
}

// GetSourceRef retrieves virtual source content by reference ID.
// Returns the content and true if found, or empty string and false if not.
func (e *Engine) GetSourceRef(id int) (string, bool) {
	e.mu.Lock()
	defer e.mu.Unlock()
	entry, ok := e.sourceRefs[id]
	if !ok {
		return "", false
	}
	return entry.content, true
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

// EvalCount returns the number of times OnEval has been called. This is
// useful for tests that need to wait for evaluation to start.
func (e *Engine) EvalCount() int64 {
	return e.evalCount.Load()
}

// OnEval implements lisp.Debugger. Called before each expression with
// a real source location.
func (e *Engine) OnEval(env *lisp.LEnv, expr *lisp.LVal) bool {
	e.evalCount.Add(1)

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

	// Check pause request (from DAP pause command).
	if e.pauseRequested {
		e.mu.Unlock()
		return true
	}
	e.mu.Unlock()

	// Safety net: if stepOutReturned is still set (AfterFunCall didn't
	// consume it), pause here. This handles edge cases where the post-call
	// check in Eval was not reached.
	if e.stepOutReturned {
		e.stepOutReturned = false
		e.stepper.ShouldPausePostCall(len(env.Runtime.Stack.Frames))
		return true
	}

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

	// Check hit count condition. The hit count is incremented under the
	// breakpoint store lock to avoid races.
	e.breakpoints.mu.Lock()
	hitOK := bp.IncrementHitCount()
	e.breakpoints.mu.Unlock()
	if !hitOK {
		return false
	}

	// Handle log points: emit output instead of pausing.
	if bp.LogMessage != "" {
		e.mu.Lock()
		e.evaluatingCondition = true
		e.mu.Unlock()
		output := InterpolateLogMessage(env, bp.LogMessage)
		e.mu.Lock()
		e.evaluatingCondition = false
		// Suppress re-hits on the same line (sub-expressions on the same
		// line would otherwise fire the log point repeatedly).
		e.lastContinuedKey = bp.key()
		cb := e.onEvent
		e.mu.Unlock()
		if cb != nil {
			cb(Event{
				Type:   EventOutput,
				Output: output,
				Env:    env,
				Expr:   expr,
				BP:     bp,
			})
		}
		return false // log points do not pause
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
	if e.pauseRequested {
		reason = e.pauseReason
		e.pauseRequested = false
		e.pauseReason = ""
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

// SetFunctionBreakpoints replaces the set of function breakpoints.
// Each name should be a function name as the user would type it (e.g., "add"
// or "user:add"). Returns the names that were set.
func (e *Engine) SetFunctionBreakpoints(names []string) []string {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.funBreakpoints = make(map[string]string, len(names))
	result := make([]string, len(names))
	for i, name := range names {
		e.funBreakpoints[name] = name
		result[i] = name
	}
	return result
}

// OnFunEntry implements lisp.Debugger. Checks function breakpoints.
func (e *Engine) OnFunEntry(env *lisp.LEnv, fun *lisp.LVal, fenv *lisp.LEnv) {
	if fun.Type != lisp.LFun {
		return
	}
	e.mu.Lock()
	if len(e.funBreakpoints) == 0 {
		e.mu.Unlock()
		return
	}

	// Build qualified and unqualified names from the function value.
	funData := fun.FunData()
	localName := fun.Str
	if localName == "" && funData != nil {
		localName = funData.FID
	}
	qualifiedName := localName
	if funData != nil && funData.Package != "" {
		qualifiedName = funData.Package + ":" + localName
	}

	// Check if either form matches a function breakpoint.
	_, matchQualified := e.funBreakpoints[qualifiedName]
	_, matchLocal := e.funBreakpoints[localName]
	if !matchQualified && !matchLocal {
		e.mu.Unlock()
		return
	}

	// Request a pause with function breakpoint reason.
	e.pauseRequested = true
	e.pauseReason = StopFunctionBreakpoint
	e.mu.Unlock()
}

// OnFunReturn implements lisp.Debugger. Detects the step-out condition
// before the frame is popped: if the stepper is in StepOut mode and the
// post-return depth will be less than the recorded depth, set the
// stepOutReturned flag so that AfterFunCall (or OnEval as a safety net)
// can pause at the call-site expression.
func (e *Engine) OnFunReturn(env *lisp.LEnv, fun, result *lisp.LVal) {
	if e.stepper.Mode() != StepOut {
		return
	}
	// After Pop(), depth will be currentDepth - 1. Check if that satisfies
	// the step-out condition (postReturnDepth < stepper.depth).
	currentDepth := len(env.Runtime.Stack.Frames)
	postReturnDepth := currentDepth - 1
	if postReturnDepth < e.stepper.Depth() {
		e.stepOutReturned = true
	}
}

// AfterFunCall implements lisp.Debugger. Called in Eval after EvalSExpr
// returns. Uses the stepper's ShouldPausePostCall to check if step-out
// should fire at the current (post-pop) depth, and clears the
// stepOutReturned flag.
func (e *Engine) AfterFunCall(env *lisp.LEnv) bool {
	if !e.stepOutReturned {
		return false
	}
	e.stepOutReturned = false
	depth := len(env.Runtime.Stack.Frames)
	return e.stepper.ShouldPausePostCall(depth)
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

// RequestPause requests that the eval goroutine pause at the next expression.
// This is used by the DAP pause command. The flag is cleared when the engine
// actually pauses in WaitIfPaused.
func (e *Engine) RequestPause() {
	e.mu.Lock()
	defer e.mu.Unlock()
	e.pauseRequested = true
	e.pauseReason = StopPause
}

// NotifyExit fires an EventExited event to notify the DAP server that the
// program has finished. This should be called from the eval goroutine after
// evaluation completes, while the DAP server is still running.
func (e *Engine) NotifyExit(exitCode int) {
	e.mu.Lock()
	cb := e.onEvent
	e.mu.Unlock()
	if cb != nil {
		cb(Event{Type: EventExited, ExitCode: exitCode})
	}
}

// EvalInContext evaluates an expression string in a paused environment,
// setting the evaluatingCondition guard so that the OnEval hook does not
// re-enter breakpoint logic (which would deadlock since EvalInContext
// runs on the DAP server goroutine, not the eval goroutine).
func (e *Engine) EvalInContext(env *lisp.LEnv, source string) *lisp.LVal {
	e.mu.Lock()
	e.evaluatingCondition = true
	e.mu.Unlock()
	defer func() {
		e.mu.Lock()
		e.evaluatingCondition = false
		e.mu.Unlock()
	}()
	return EvalInContext(env, source)
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
