package debugrepl

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newTestEnv(t *testing.T, dbg *debugger.Engine) *lisp.LEnv {
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

func TestShowHelp(t *testing.T) {
	t.Parallel()
	var buf bytes.Buffer
	showHelp(&buf)
	out := buf.String()
	assert.Contains(t, out, "continue (c)")
	assert.Contains(t, out, "step (s)")
	assert.Contains(t, out, "next (n)")
	assert.Contains(t, out, "out (o)")
	assert.Contains(t, out, "break (b)")
	assert.Contains(t, out, "delete (d)")
	assert.Contains(t, out, "breakpoints (bl)")
	assert.Contains(t, out, "backtrace (bt)")
	assert.Contains(t, out, "locals (l)")
	assert.Contains(t, out, "print (p)")
	assert.Contains(t, out, "where (w)")
	assert.Contains(t, out, "quit (q)")
	assert.Contains(t, out, "help (h)")
}

func TestShowBreakpoints_Empty(t *testing.T) {
	t.Parallel()
	store := debugger.NewBreakpointStore()
	var buf bytes.Buffer
	showBreakpoints(&buf, store)
	assert.Contains(t, buf.String(), "(no breakpoints)")
}

func TestShowBreakpoints_WithEntries(t *testing.T) {
	t.Parallel()
	store := debugger.NewBreakpointStore()
	store.Set("hello.lisp", 10, "")
	store.Set("hello.lisp", 20, "(> x 5)")

	var buf bytes.Buffer
	showBreakpoints(&buf, store)
	out := buf.String()
	assert.Contains(t, out, "#1")
	assert.Contains(t, out, "hello.lisp:10")
	assert.Contains(t, out, "enabled")
	assert.Contains(t, out, "#2")
	assert.Contains(t, out, "hello.lisp:20")
	assert.Contains(t, out, "if (> x 5)")

	// Verify ordering: #1 should appear before #2.
	idx1 := strings.Index(out, "#1")
	idx2 := strings.Index(out, "#2")
	assert.Less(t, idx1, idx2, "breakpoints should be sorted by ID")
}

func TestShowBreakpoints_DisabledEntry(t *testing.T) {
	t.Parallel()
	store := debugger.NewBreakpointStore()
	bp := store.Set("test.lisp", 5, "")
	bp.Enabled = false

	var buf bytes.Buffer
	showBreakpoints(&buf, store)
	out := buf.String()
	assert.Contains(t, out, "disabled")
	assert.Contains(t, out, fmt.Sprintf("#%d", bp.ID))
}

func TestShowLocals_Empty(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)
	var buf bytes.Buffer
	showLocals(&buf, env, dbg)
	assert.Contains(t, buf.String(), "(no locals)")
}

func TestShowBacktrace_EmptyStack(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)
	var buf bytes.Buffer
	showBacktrace(&buf, env.Runtime.Stack, nil, "")
	assert.Contains(t, buf.String(), "(empty stack)")
}

func TestShowSourceContext_MissingFile(t *testing.T) {
	t.Parallel()
	var buf bytes.Buffer
	showSourceContext(&buf, "nonexistent_file.lisp", 5, "")
	assert.Contains(t, buf.String(), "source not available")
}

func TestShowSourceContext_ValidFile(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	content := "line1\nline2\nline3\nline4\nline5\nline6\nline7\nline8\nline9\nline10\n"
	path := filepath.Join(dir, "test.lisp")
	require.NoError(t, os.WriteFile(path, []byte(content), 0644)) //nolint:gosec

	var buf bytes.Buffer
	showSourceContext(&buf, path, 5, "")
	out := buf.String()
	// Line 5 should have the --> marker.
	assert.Contains(t, out, "-->    5  line5")
	// Adjacent lines should be present.
	assert.Contains(t, out, "   1  line1")
	assert.Contains(t, out, "  10  line10")
}

func TestShowSourceContext_FirstLine(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	content := "first\nsecond\nthird\nfourth\nfifth\nsixth\nseventh\n"
	path := filepath.Join(dir, "test.lisp")
	require.NoError(t, os.WriteFile(path, []byte(content), 0644)) //nolint:gosec

	var buf bytes.Buffer
	showSourceContext(&buf, path, 1, "")
	out := buf.String()
	// Line 1 should have the marker.
	assert.Contains(t, out, "-->    1  first")
	// Context window [1, 1+5=6] should show up to line 6.
	assert.Contains(t, out, "sixth")
	// Line 7 should be outside the window.
	assert.NotContains(t, out, "seventh")
}

func TestShowSourceContext_LastLine(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	content := "one\ntwo\nthree\n"
	path := filepath.Join(dir, "test.lisp")
	require.NoError(t, os.WriteFile(path, []byte(content), 0644)) //nolint:gosec

	var buf bytes.Buffer
	showSourceContext(&buf, path, 3, "")
	out := buf.String()
	// Line 3 should have the marker.
	assert.Contains(t, out, "-->    3  three")
	// Line 1 is within the window.
	assert.Contains(t, out, "   1  one")
}

func TestShowSourceContext_WithSourceRoot(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	subdir := filepath.Join(dir, "src")
	require.NoError(t, os.MkdirAll(subdir, 0750))
	content := "hello\nworld\n"
	path := filepath.Join(subdir, "test.lisp")
	require.NoError(t, os.WriteFile(path, []byte(content), 0644)) //nolint:gosec

	var buf bytes.Buffer
	// Use just the filename with sourceRoot pointing to parent.
	showSourceContext(&buf, "test.lisp", 1, subdir)
	out := buf.String()
	assert.Contains(t, out, "-->    1  hello")
}

func TestHandleLine_DebugCommands(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)
	var stderr bytes.Buffer

	h := &debugHandler{
		engine:   dbg,
		env:      env,
		file:     "test.lisp",
		pausedCh: make(chan debugger.Event, 1),
		exitCh:   make(chan struct{}),
		stderr:   &stderr,
	}

	// Test help command.
	assert.True(t, h.handleLine("help"))
	assert.Contains(t, stderr.String(), "Debug commands:")
	stderr.Reset()

	// Test abbreviated help.
	assert.True(t, h.handleLine("h"))
	assert.Contains(t, stderr.String(), "Debug commands:")
	stderr.Reset()

	// Test break with missing args.
	assert.True(t, h.handleLine("break"))
	assert.Contains(t, stderr.String(), "usage: break")
	stderr.Reset()

	// Test break with valid args.
	assert.True(t, h.handleLine("break test.lisp:10"))
	assert.Contains(t, stderr.String(), "breakpoint 1 set at test.lisp:10")
	stderr.Reset()

	// Test break with condition.
	assert.True(t, h.handleLine("break test.lisp:20 (> x 5)"))
	assert.Contains(t, stderr.String(), "breakpoint 2 set at test.lisp:20")
	stderr.Reset()

	// Test breakpoints listing.
	assert.True(t, h.handleLine("breakpoints"))
	out := stderr.String()
	assert.Contains(t, out, "#1")
	assert.Contains(t, out, "#2")
	stderr.Reset()

	// Test delete.
	assert.True(t, h.handleLine("delete 1"))
	assert.Contains(t, stderr.String(), "breakpoint 1 removed")
	stderr.Reset()

	// Test delete non-existent.
	assert.True(t, h.handleLine("delete 99"))
	assert.Contains(t, stderr.String(), "no breakpoint with id 99")
	stderr.Reset()

	// Test continue when not paused.
	assert.True(t, h.handleLine("continue"))
	assert.Contains(t, stderr.String(), "not paused")
	stderr.Reset()

	// Test step when not paused.
	assert.True(t, h.handleLine("step"))
	assert.Contains(t, stderr.String(), "not paused")
	stderr.Reset()

	// Test next when not paused.
	assert.True(t, h.handleLine("next"))
	assert.Contains(t, stderr.String(), "not paused")
	stderr.Reset()

	// Test out when not paused.
	assert.True(t, h.handleLine("out"))
	assert.Contains(t, stderr.String(), "not paused")
	stderr.Reset()

	// Test print with missing args.
	assert.True(t, h.handleLine("print"))
	assert.Contains(t, stderr.String(), "usage: print")
	stderr.Reset()

	// Test print when not paused.
	assert.True(t, h.handleLine("print (+ 1 2)"))
	assert.Contains(t, stderr.String(), "not paused")
	stderr.Reset()

	// Test backtrace when not paused.
	assert.True(t, h.handleLine("backtrace"))
	assert.Contains(t, stderr.String(), "not paused")
	stderr.Reset()

	// Test locals when not paused.
	assert.True(t, h.handleLine("locals"))
	assert.Contains(t, stderr.String(), "not paused")
	stderr.Reset()

	// Test where when not paused.
	assert.True(t, h.handleLine("where"))
	assert.Contains(t, stderr.String(), "no source location")
	stderr.Reset()

	// Lisp expressions should fall through.
	assert.False(t, h.handleLine("(+ 1 2)"))
}

func TestHandleLine_EmptyRepeatsLast(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)
	var stderr bytes.Buffer

	h := &debugHandler{
		engine:   dbg,
		env:      env,
		file:     "test.lisp",
		pausedCh: make(chan debugger.Event, 1),
		exitCh:   make(chan struct{}),
		stderr:   &stderr,
	}

	// First empty input with no lastCmd should be consumed.
	assert.True(t, h.handleLine(""))

	// Set lastCmd via a help command.
	h.handleLine("help")
	stderr.Reset()

	// Empty input should repeat "help".
	assert.True(t, h.handleLine(""))
	assert.Contains(t, stderr.String(), "Debug commands:")
}

func TestHandleLine_Abbreviations(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)
	var stderr bytes.Buffer

	h := &debugHandler{
		engine:   dbg,
		env:      env,
		file:     "test.lisp",
		pausedCh: make(chan debugger.Event, 1),
		exitCh:   make(chan struct{}),
		stderr:   &stderr,
	}

	// Set a breakpoint so "bl" has something to show.
	h.handleLine("b test.lisp:5")
	stderr.Reset()

	abbreviations := []struct {
		abbr     string
		expected string
	}{
		{"c", "not paused"},
		{"s", "not paused"},
		{"n", "not paused"},
		{"o", "not paused"},
		{"bl", "#1"},
		{"bt", "not paused"},
		{"l", "not paused"},
		{"w", "no source location"},
		{"h", "Debug commands:"},
	}

	for _, tc := range abbreviations {
		stderr.Reset()
		assert.True(t, h.handleLine(tc.abbr), "abbreviation %q should be handled", tc.abbr)
		assert.Contains(t, stderr.String(), tc.expected,
			"abbreviation %q should produce output containing %q", tc.abbr, tc.expected)
	}
}

func TestDebugCompleter_Commands(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)

	c := &debugCompleter{env: env, engine: dbg}

	// Complete "co" → should match "continue".
	matches, length := c.Do([]rune("co"), 2)
	assert.Equal(t, 2, length)
	var labels []string
	for _, m := range matches {
		labels = append(labels, "co"+string(m))
	}
	assert.Contains(t, labels, "continue")

	// Complete "s" → should match "step".
	matches, length = c.Do([]rune("s"), 1)
	assert.Equal(t, 1, length)
	labels = nil
	for _, m := range matches {
		labels = append(labels, "s"+string(m))
	}
	assert.Contains(t, labels, "step")
}

func TestDebugCompleter_Symbols(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)

	c := &debugCompleter{env: env, engine: dbg}

	// Complete "set" after "print " — should match ELPS builtins.
	matches, length := c.Do([]rune("print set"), 9)
	assert.Equal(t, 3, length) // "set" is 3 chars
	var labels []string
	for _, m := range matches {
		labels = append(labels, "set"+string(m))
	}
	assert.NotEmpty(t, labels, "should find symbols starting with 'set'")
	// Verify we get known ELPS symbols like "set" and "set!".
	assert.Contains(t, labels, "set", "should include the 'set' builtin")
	assert.Contains(t, labels, "set!", "should include the 'set!' builtin")
}

func TestDebugCompleter_NoDebugCmdsAfterFirstWord(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)

	c := &debugCompleter{env: env, engine: dbg}

	// Complete "co" after "print " — should NOT include "continue" since
	// it's not the first word.
	matches, length := c.Do([]rune("print co"), 8)
	assert.Equal(t, 2, length) // "co" is 2 chars
	var labels []string
	for _, m := range matches {
		labels = append(labels, "co"+string(m))
	}
	for _, label := range labels {
		assert.NotEqual(t, "continue", label,
			"debug commands should not appear for non-first-word completions")
	}
}

func TestDebugCompleter_EmptyPrefix(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)

	c := &debugCompleter{env: env, engine: dbg}
	matches, _ := c.Do([]rune(""), 0)
	assert.Empty(t, matches)
}

func TestResolveSourceFile(t *testing.T) {
	t.Parallel()
	dir := t.TempDir()
	path := filepath.Join(dir, "test.lisp")
	require.NoError(t, os.WriteFile(path, []byte("(+ 1 2)"), 0644)) //nolint:gosec

	// Direct path.
	assert.Equal(t, path, resolveSourceFile(path, ""))

	// Relative under sourceRoot.
	assert.Equal(t, path, resolveSourceFile("test.lisp", dir))

	// Non-existent.
	result := resolveSourceFile("nonexistent.lisp", dir)
	assert.Equal(t, "nonexistent.lisp", result)
}

func TestPrompt(t *testing.T) {
	t.Parallel()
	h := &debugHandler{}

	h.mu.Lock()
	h.paused = true
	h.mu.Unlock()
	p, c := h.prompt()
	assert.Equal(t, "(dbg) ", p)
	assert.Equal(t, "   .. ", c)

	h.mu.Lock()
	h.paused = false
	h.mu.Unlock()
	p, c = h.prompt()
	assert.Equal(t, "running> ", p)
	assert.Equal(t, "         ", c)
}

func TestBreakInvalidFormat(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)
	var stderr bytes.Buffer

	h := &debugHandler{
		engine:   dbg,
		env:      env,
		file:     "test.lisp",
		pausedCh: make(chan debugger.Event, 1),
		exitCh:   make(chan struct{}),
		stderr:   &stderr,
	}

	// Missing colon.
	assert.True(t, h.handleLine("break test.lisp"))
	assert.Contains(t, stderr.String(), "usage: break")
	stderr.Reset()

	// Non-numeric line.
	assert.True(t, h.handleLine("break test.lisp:abc"))
	assert.Contains(t, stderr.String(), "invalid line number")
	stderr.Reset()

	// Delete with non-numeric arg.
	assert.True(t, h.handleLine("delete abc"))
	assert.Contains(t, stderr.String(), "invalid breakpoint id")
}

func TestRemoveByID(t *testing.T) {
	t.Parallel()
	store := debugger.NewBreakpointStore()
	bp := store.Set("test.lisp", 10, "")
	assert.True(t, store.RemoveByID(bp.ID))
	assert.False(t, store.RemoveByID(bp.ID))
	assert.Empty(t, store.All())
}

// newIntegrationEnv creates a debugger engine, env, and temp lisp file for
// integration tests. Returns (engine, env, lispFilePath).
func newIntegrationEnv(t *testing.T, src string) (*debugger.Engine, *lisp.LEnv, string) {
	t.Helper()
	dir := t.TempDir()
	lispFile := filepath.Join(dir, "test.lisp")
	require.NoError(t, os.WriteFile(lispFile, []byte(src), 0644)) //nolint:gosec

	dbg := debugger.New(debugger.WithSourceRoot(dir))
	dbg.Enable()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS(dir)}
	env.Runtime.Debugger = dbg
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil())
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil())
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil())
	return dbg, env, lispFile
}

// runWithCommands pipes the given commands into a debug REPL session and
// returns the stderr output. It asserts that Run completes without hanging.
func runWithCommands(t *testing.T, dbg *debugger.Engine, env *lisp.LEnv, file string, commands string) string {
	t.Helper()
	inR, inW := io.Pipe()
	go func() {
		defer inW.Close() //nolint:errcheck
		_, _ = io.WriteString(inW, commands)
	}()

	var stderr bytes.Buffer
	err := Run(dbg, env, file,
		WithStdin(inR),
		WithStderr(&stderr),
	)
	// After quit + disconnect, eval may produce an error since the
	// debugger was disconnected mid-execution. Either nil or error is
	// acceptable; the important thing is that Run returns (no hang, no
	// os.Exit).
	if err != nil {
		t.Logf("Run returned error (expected after disconnect): %v", err)
	}
	return stderr.String()
}

// TestRunIntegration drives the full Run function with piped stdin,
// exercising the same path a user would take at the terminal.
func TestRunIntegration(t *testing.T) {
	src := "(set 'x 10)\n(set 'y 20)\n(+ x y)\n"
	dbg, env, lispFile := newIntegrationEnv(t, src)

	commands := strings.Join([]string{
		"help",
		"break test.lisp:3",
		"breakpoints",
		"continue",
		"locals",
		"where",
		"quit",
	}, "\n") + "\n"

	out := runWithCommands(t, dbg, env, lispFile, commands)

	// Verify stop-on-entry.
	assert.Contains(t, out, "stopped:")
	// Verify help output.
	assert.Contains(t, out, "Debug commands:")
	assert.Contains(t, out, "continue (c)")
	// Verify breakpoint was set.
	assert.Contains(t, out, "breakpoint 1 set at test.lisp:3")
	// Verify breakpoints listing includes the ID and location.
	assert.Contains(t, out, "#1")
	assert.Contains(t, out, "test.lisp:3")
	// Verify locals output shows x and y (set before breakpoint line 3).
	assert.Contains(t, out, "x")
	assert.Contains(t, out, "y")
	// Verify source context around breakpoint.
	assert.Contains(t, out, "(+ x y)")
	// Verify quit message.
	assert.Contains(t, out, "quitting debug session")
}

// TestRunIntegration_StepAndPrint exercises step and print commands.
func TestRunIntegration_StepAndPrint(t *testing.T) {
	// Three lines so that stepping past line 1 doesn't exit the program.
	src := "(set 'x 42)\n(set 'y 99)\n(+ x y)\n"
	dbg, env, lispFile := newIntegrationEnv(t, src)

	// Step once to execute (set 'x 42), then print x while paused at line 2.
	commands := "step\nprint x\nquit\n"
	out := runWithCommands(t, dbg, env, lispFile, commands)

	assert.Contains(t, out, "stopped:")
	// After stepping past (set 'x 42), printing x should show the exact
	// integer value 42. We use a line-based check to avoid false matches
	// with "42" appearing in other contexts (like breakpoint IDs).
	lines := strings.Split(out, "\n")
	foundPrintResult := false
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		if trimmed == "42" {
			foundPrintResult = true
			break
		}
	}
	assert.True(t, foundPrintResult,
		"expected 'print x' to output '42' on its own line, got output:\n%s", out)
	assert.Contains(t, out, "quitting debug session")
}

// TestRunIntegration_BreakpointHit sets a breakpoint and continues to it.
func TestRunIntegration_BreakpointHit(t *testing.T) {
	src := "(set 'a 1)\n(set 'b 2)\n(set 'c 3)\n"
	dbg, env, lispFile := newIntegrationEnv(t, src)

	commands := strings.Join([]string{
		"break test.lisp:2",
		"continue",
		"print a",
		"quit",
	}, "\n") + "\n"
	out := runWithCommands(t, dbg, env, lispFile, commands)

	// Verify breakpoint was set.
	assert.Contains(t, out, "breakpoint 1 set at test.lisp:2")
	// Verify we hit the breakpoint.
	assert.Contains(t, out, "breakpoint 1")
	// After hitting breakpoint at line 2, 'a' should already be set to 1.
	lines := strings.Split(out, "\n")
	foundPrintResult := false
	for _, line := range lines {
		if strings.TrimSpace(line) == "1" {
			foundPrintResult = true
			break
		}
	}
	assert.True(t, foundPrintResult,
		"expected 'print a' to show '1', got output:\n%s", out)
}

// TestRunIntegration_DoQuitLifecycle verifies quit closes doneCh and
// the REPL exits cleanly.
func TestRunIntegration_DoQuitLifecycle(t *testing.T) {
	src := "(+ 1 2)\n"
	dbg, env, lispFile := newIntegrationEnv(t, src)

	commands := "quit\n"
	out := runWithCommands(t, dbg, env, lispFile, commands)

	assert.Contains(t, out, "quitting debug session")
	// The test proves Run() returned (no hang) — if doQuit didn't close
	// doneCh properly, the test would timeout.
}

// TestRunIntegration_OnInterrupt verifies Ctrl+C is handled without crashing.
// Since io.Pipe doesn't produce readline interrupts, we test onInterrupt
// directly below and verify the integration doesn't crash.
func TestOnInterrupt(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)

	h := &debugHandler{
		engine:   dbg,
		env:      env,
		file:     "test.lisp",
		pausedCh: make(chan debugger.Event, 1),
		exitCh:   make(chan struct{}),
		doneCh:   make(chan struct{}),
		stderr:   &bytes.Buffer{},
	}

	// When paused, interrupt does nothing.
	h.mu.Lock()
	h.paused = true
	h.mu.Unlock()
	h.onInterrupt() // should not panic or call RequestPause

	// When not paused, interrupt calls RequestPause.
	h.mu.Lock()
	h.paused = false
	h.mu.Unlock()
	h.onInterrupt() // calls engine.RequestPause — no panic = success
}

// TestEvalInContext verifies the evalInContext method evaluates expressions
// via the engine's EvalInContext pathway.
func TestEvalInContext(t *testing.T) {
	t.Parallel()
	dbg := debugger.New()
	env := newTestEnv(t, dbg)

	h := &debugHandler{
		engine: dbg,
		env:    env,
	}

	// evalInContext stringifies the LVal and passes it to engine.EvalInContext.
	// With a valid env, arithmetic expressions evaluate correctly.
	expr := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("+"),
		lisp.Int(1),
		lisp.Int(2),
	})
	result := h.evalInContext(env, expr)
	assert.Equal(t, lisp.LInt, result.Type, "should return an int")
	assert.Equal(t, 3, result.Int, "should evaluate (+ 1 2) to 3")
}
