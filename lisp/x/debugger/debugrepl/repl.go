// Copyright © 2018 The ELPS authors

// Package debugrepl provides an interactive CLI debug REPL built on top
// of the extensible repl.RunEnv function and the debugger engine.
package debugrepl

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/repl"
)

// Option configures the debug REPL.
type Option func(*debugHandler)

// WithStdin sets the reader for REPL input. This is primarily useful for
// testing, where a pipe replaces the terminal.
func WithStdin(r io.ReadCloser) Option {
	return func(h *debugHandler) {
		h.stdin = r
	}
}

// WithStderr sets the writer for debug output (prompts, status, etc.).
func WithStderr(w io.Writer) Option {
	return func(h *debugHandler) {
		h.stderr = w
	}
}

// Run starts an interactive debug REPL for the given file. It sets
// engine.SetStopOnEntry(true) so the REPL starts paused, then launches
// the eval goroutine and enters the REPL loop on the calling goroutine.
func Run(engine *debugger.Engine, env *lisp.LEnv, file string, opts ...Option) error {
	h := &debugHandler{
		engine:     engine,
		env:        env,
		file:       file,
		pausedCh:   make(chan debugger.Event, 1),
		exitCh:     make(chan struct{}),
		doneCh:     make(chan struct{}),
		stderr:     os.Stderr,
		sourceRoot: engine.SourceRoot(),
	}
	for _, opt := range opts {
		opt(h)
	}

	engine.SetStopOnEntry(true)
	engine.SetEventCallback(h.onEvent)
	engine.SignalReady()

	relFile := file
	if h.sourceRoot != "" {
		if rel, err := filepath.Rel(h.sourceRoot, file); err == nil {
			relFile = rel
		}
	}

	// Start eval goroutine.
	evalDone := make(chan *lisp.LVal, 1)
	go func() {
		res := env.LoadFile(relFile)
		exitCode := 0
		if res.Type == lisp.LError {
			exitCode = 1
		}
		engine.NotifyExit(exitCode)
		evalDone <- res
	}()

	// Wait for the first pause (stop-on-entry).
	evt := <-h.pausedCh
	h.mu.Lock()
	h.paused = true
	h.mu.Unlock()
	h.showStopBanner(evt)

	// Run the REPL with hooks.
	repl.RunEnv(env, "(dbg) ", "   .. ", h.replOptions()...)

	// Wait for eval to finish.
	res := <-evalDone
	if res.Type == lisp.LError {
		return fmt.Errorf("%v", res)
	}
	return nil
}

// debugHandler holds state for the debug REPL session.
type debugHandler struct {
	engine     *debugger.Engine
	env        *lisp.LEnv
	file       string
	sourceRoot string
	stdin      io.ReadCloser
	stderr     io.Writer
	pausedCh   chan debugger.Event
	exitCh     chan struct{} // closed when eval goroutine exits
	doneCh     chan struct{} // closed by doQuit to signal REPL to stop

	mu       sync.Mutex
	paused   bool
	exited   bool
	lastCmd  string
}

// onEvent is the debugger event callback. It runs on the eval goroutine.
func (h *debugHandler) onEvent(evt debugger.Event) {
	switch evt.Type {
	case debugger.EventStopped:
		h.pausedCh <- evt
	case debugger.EventExited:
		h.mu.Lock()
		h.exited = true
		h.mu.Unlock()
		close(h.exitCh)
	case debugger.EventOutput:
		fmt.Fprintln(h.stderr, evt.Output) //nolint:errcheck
	}
}

// replOptions returns the repl.Option values that wire up the debug REPL.
func (h *debugHandler) replOptions() []repl.Option {
	opts := []repl.Option{
		repl.WithLineHandler(h.handleLine),
		repl.WithEvalFunc(h.evalInContext),
		repl.WithCompleter(&debugCompleter{env: h.env, engine: h.engine}),
		repl.WithPromptFunc(h.prompt),
		repl.WithInterruptFunc(h.onInterrupt),
		repl.WithDoneCh(h.doneCh),
	}
	if h.stdin != nil {
		opts = append(opts, repl.WithStdin(h.stdin))
	}
	return opts
}

// prompt returns the current prompt strings.
func (h *debugHandler) prompt() (string, string) {
	h.mu.Lock()
	paused := h.paused
	h.mu.Unlock()
	if paused {
		return "(dbg) ", "   .. "
	}
	return "running> ", "         "
}

// onInterrupt handles Ctrl+C by requesting a pause.
func (h *debugHandler) onInterrupt() {
	h.mu.Lock()
	paused := h.paused
	h.mu.Unlock()
	if !paused {
		h.engine.RequestPause()
	}
}

// evalInContext evaluates a lisp expression in the debugger context.
func (h *debugHandler) evalInContext(env *lisp.LEnv, expr *lisp.LVal) *lisp.LVal {
	return h.engine.EvalInContext(env, expr.String())
}

// handleLine dispatches debug commands. Returns true if the line was consumed.
func (h *debugHandler) handleLine(line string) bool {
	line = strings.TrimSpace(line)

	// Empty input repeats last command (GDB convention).
	if line == "" {
		h.mu.Lock()
		line = h.lastCmd
		h.mu.Unlock()
		if line == "" {
			return true
		}
	}

	parts := strings.Fields(line)
	cmd := parts[0]
	args := parts[1:]

	// Check if this is a debug command.
	switch cmd {
	case "continue", "c":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doContinue()
	case "step", "s":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doStep()
	case "next", "n":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doNext()
	case "out", "o":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doOut()
	case "break", "b":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doBreak(args)
	case "delete", "d":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doDelete(args)
	case "breakpoints", "bl":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		showBreakpoints(h.stderr, h.engine.Breakpoints())
		return true
	case "backtrace", "bt":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doBacktrace()
	case "locals", "l":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doLocals()
	case "print", "p":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doPrint(args)
	case "where", "w":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doWhere()
	case "quit", "q":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		return h.doQuit()
	case "help", "h":
		h.mu.Lock()
		h.lastCmd = line
		h.mu.Unlock()
		showHelp(h.stderr)
		return true
	}

	// Not a debug command — fall through to lisp eval.
	return false
}

func (h *debugHandler) doContinue() bool {
	h.mu.Lock()
	if !h.paused {
		h.mu.Unlock()
		fmt.Fprintln(h.stderr, "not paused") //nolint:errcheck
		return true
	}
	h.paused = false
	h.mu.Unlock()

	h.engine.Resume()
	return h.waitForStop()
}

func (h *debugHandler) doStep() bool {
	h.mu.Lock()
	if !h.paused {
		h.mu.Unlock()
		fmt.Fprintln(h.stderr, "not paused") //nolint:errcheck
		return true
	}
	h.paused = false
	h.mu.Unlock()

	h.engine.StepInto()
	return h.waitForStop()
}

func (h *debugHandler) doNext() bool {
	h.mu.Lock()
	if !h.paused {
		h.mu.Unlock()
		fmt.Fprintln(h.stderr, "not paused") //nolint:errcheck
		return true
	}
	h.paused = false
	h.mu.Unlock()

	h.engine.StepOver()
	return h.waitForStop()
}

func (h *debugHandler) doOut() bool {
	h.mu.Lock()
	if !h.paused {
		h.mu.Unlock()
		fmt.Fprintln(h.stderr, "not paused") //nolint:errcheck
		return true
	}
	h.paused = false
	h.mu.Unlock()

	h.engine.StepOut()
	return h.waitForStop()
}

// waitForStop blocks until the next pause or exit event, then shows
// source context. Returns true (line consumed).
func (h *debugHandler) waitForStop() bool {
	select {
	case evt := <-h.pausedCh:
		h.mu.Lock()
		h.paused = true
		h.mu.Unlock()
		h.showStopBanner(evt)
	case <-h.exitCh:
		fmt.Fprintln(h.stderr, "program exited") //nolint:errcheck
	}
	return true
}

// showStopBanner prints the stop reason and source context.
func (h *debugHandler) showStopBanner(evt debugger.Event) {
	reason := string(evt.Reason)
	if evt.BP != nil {
		reason = fmt.Sprintf("breakpoint %d", evt.BP.ID)
	}
	fmt.Fprintf(h.stderr, "stopped: %s\n", reason) //nolint:errcheck
	if evt.Expr != nil && evt.Expr.Source != nil {
		showSourceContext(h.stderr, evt.Expr.Source.File, evt.Expr.Source.Line, h.sourceRoot)
	}
}

func (h *debugHandler) doBreak(args []string) bool {
	if len(args) == 0 {
		fmt.Fprintln(h.stderr, "usage: break <file:line> [condition]") //nolint:errcheck
		return true
	}
	loc := args[0]
	parts := strings.SplitN(loc, ":", 2)
	if len(parts) != 2 {
		fmt.Fprintln(h.stderr, "usage: break <file:line> [condition]") //nolint:errcheck
		return true
	}
	file := parts[0]
	line, err := strconv.Atoi(parts[1])
	if err != nil {
		fmt.Fprintf(h.stderr, "invalid line number: %s\n", parts[1]) //nolint:errcheck
		return true
	}
	cond := ""
	if len(args) > 1 {
		cond = strings.Join(args[1:], " ")
	}
	bp := h.engine.Breakpoints().Set(file, line, cond)
	fmt.Fprintf(h.stderr, "breakpoint %d set at %s:%d\n", bp.ID, file, line) //nolint:errcheck
	return true
}

func (h *debugHandler) doDelete(args []string) bool {
	if len(args) == 0 {
		fmt.Fprintln(h.stderr, "usage: delete <breakpoint-id>") //nolint:errcheck
		return true
	}
	id, err := strconv.Atoi(args[0])
	if err != nil {
		fmt.Fprintf(h.stderr, "invalid breakpoint id: %s\n", args[0]) //nolint:errcheck
		return true
	}
	if h.engine.Breakpoints().RemoveByID(id) {
		fmt.Fprintf(h.stderr, "breakpoint %d removed\n", id) //nolint:errcheck
	} else {
		fmt.Fprintf(h.stderr, "no breakpoint with id %d\n", id) //nolint:errcheck
	}
	return true
}

func (h *debugHandler) doBacktrace() bool {
	env, expr := h.engine.PausedState()
	if env == nil {
		fmt.Fprintln(h.stderr, "not paused") //nolint:errcheck
		return true
	}
	showBacktrace(h.stderr, env.Runtime.Stack, expr, h.sourceRoot)
	return true
}

func (h *debugHandler) doLocals() bool {
	env, _ := h.engine.PausedState()
	if env == nil {
		fmt.Fprintln(h.stderr, "not paused") //nolint:errcheck
		return true
	}
	showLocals(h.stderr, env, h.engine)
	return true
}

func (h *debugHandler) doPrint(args []string) bool {
	if len(args) == 0 {
		fmt.Fprintln(h.stderr, "usage: print <expression>") //nolint:errcheck
		return true
	}
	env, _ := h.engine.PausedState()
	if env == nil {
		fmt.Fprintln(h.stderr, "not paused") //nolint:errcheck
		return true
	}
	source := strings.Join(args, " ")
	result := h.engine.EvalInContext(env, source)
	fmt.Fprintln(h.stderr, debugger.FormatValueWith(result, h.engine)) //nolint:errcheck
	return true
}

func (h *debugHandler) doWhere() bool {
	_, expr := h.engine.PausedState()
	if expr == nil || expr.Source == nil {
		fmt.Fprintln(h.stderr, "no source location") //nolint:errcheck
		return true
	}
	showSourceContext(h.stderr, expr.Source.File, expr.Source.Line, h.sourceRoot)
	return true
}

func (h *debugHandler) doQuit() bool {
	fmt.Fprintln(h.stderr, "quitting debug session") //nolint:errcheck
	// Disconnect the debugger (resumes eval if paused, disables hooks).
	h.engine.Disconnect()
	// Signal the REPL to exit on next Read call.
	close(h.doneCh)
	return true
}

func showHelp(w io.Writer) {
	help := `Debug commands:
  continue (c)        Resume execution
  step (s)            Step into next expression
  next (n)            Step over (same depth)
  out (o)             Step out of current function
  break (b) F:L [C]   Set breakpoint at file:line [with condition]
  delete (d) N        Remove breakpoint by ID
  breakpoints (bl)    List all breakpoints
  backtrace (bt)      Show call stack
  locals (l)          Show local variables
  print (p) EXPR      Evaluate and print expression
  where (w)           Show source context
  quit (q)            End debug session
  help (h)            Show this help

Lisp expressions are evaluated in the paused scope.
Empty input repeats the last command.`
	fmt.Fprintln(w, help) //nolint:errcheck
}
