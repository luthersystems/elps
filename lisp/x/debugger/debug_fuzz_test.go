// Copyright © 2026 The ELPS authors

package debugger_test

import (
	"bytes"
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser"
)

// Fuzzing the DEBUGGER's eval path, which is not the one FuzzEval covers.
//
// Issue #318 records lisp/x/debugger as "plausibly the highest-value remaining
// target" among the unfuzzed packages, and the reason is that attaching a
// debugger does not merely add observation -- it changes evaluation. Two
// changes, both keyed off `Runtime.Debugger != nil` rather than off
// IsEnabled(), so a DORMANT debugger already gets them:
//
//  1. TAIL-RECURSION OPTIMISATION IS DISABLED GLOBALLY (lisp/env.go, the
//     `if env.Runtime.Debugger == nil` guard around TerminalFID). Every
//     tail-recursive loop that runs in constant stack space in production
//     instead pushes one frame per iteration under a debugger. So the limit
//     that bounds a runaway loop changes identity: MaxTailIterations governs
//     it without a debugger, MaxHeightPhysical governs it with one. A budget
//     tuned for the first shape does not bound the second.
//
//  2. EVERY MACRO-EXPANDED NODE IS MUTATED. stampMacroExpansion allocates a
//     MacroExpansionContext and hangs a MacroExpansionInfo off each expanded
//     LVal, carrying the call site, the qualified macro name, the definition
//     site and the UNEVALUATED arguments. That allocation and that write
//     happen on no other code path.
//
// On top of that the engine runs breakpoint matching, the stepper's
// depth arithmetic, and -- through the event callback -- the whole DAP
// inspection surface, on values the fuzzer chose.
//
// The asserted properties are FuzzEval's, restated for this path: evaluation
// terminates, and it never recovers a Go panic. `IsInternalPanic` is again the
// assertion with teeth, because lisp.eval's recover() turns any panic in
// engine, stepper or inspector code into an ordinary-looking *LVal.
//
// # Driving the debugger from one goroutine
//
// Engine.WaitIfPaused blocks on `<-e.pauseCh` until a DAP server goroutine
// sends a resume command. A fuzz target has no such goroutine, so the naive
// harness deadlocks on the first pause.
//
// The way through is the event callback, which Engine invokes INSIDE
// WaitIfPaused and BEFORE the channel receive. The callback runs on the eval
// goroutine, so it can push the next action into the (capacity-1, and at that
// moment empty) pauseCh and let the receive that follows complete
// immediately. That keeps the harness single-goroutine and deterministic --
// a crasher that does not reproduce from its own testdata file is worthless --
// while still exercising the REAL WaitIfPaused, the real stepper transitions
// and the real event dispatch, rather than a stub that skips them.
//
// The push is a non-blocking select. Nothing else sends on that channel here,
// so the default branch is unreachable in practice; it exists so that a future
// change which does send cannot turn this target into a hang.
const (
	// Budget. Deliberately the same shape as FuzzEval's (see
	// lisp/eval_fuzz_test.go for why each limit is here and why
	// MaxHeightLogical is not), with two differences forced by the debugger:
	//
	// fuzzMaxSteps is lower because each step now also runs breakpoint
	// matching, stepper arithmetic and -- at a pause -- the whole inspection
	// surface, so the per-step cost is far higher than plain evaluation.
	//
	// fuzzMaxPhysicalHeight matters much more here than in FuzzEval. With TRO
	// off it is what stops a tail-recursive loop, a job MaxTailIterations does
	// on the ordinary path.
	fuzzMaxSteps          = 200_000
	fuzzMaxTailIterations = 10_000
	fuzzMaxPhysicalHeight = 2_000
	fuzzMaxAlloc          = 1_000_000
	fuzzMacroDepth        = 100
	fuzzMaxEvalNesting    = 20_000

	fuzzDeadline    = 2 * time.Second
	watchdogTimeout = 30 * time.Second

	// maxPauses bounds how many times the harness stops and inspects. Every
	// pause runs the full inspection surface, so an unbounded count would let
	// a stepping program spend the whole deadline in the harness rather than
	// in the code under test. After the cap the engine is told to continue and
	// stepping is disarmed.
	maxPauses = 64

	// maxInspectBindings bounds how many bindings from one scope get
	// FormatValue'd. Formatting is the expensive part and the interesting
	// inputs are the first few, not the thousandth.
	maxInspectBindings = 32
)

// driver decides what a pause does. Actions are drawn from the fuzzer's own
// input so the mutator can steer the stepper, and consumed in order so the
// same input always produces the same sequence.
type driver struct {
	script []byte
	i      int
}

func (d *driver) next() lisp.DebugAction {
	if len(d.script) == 0 {
		return lisp.DebugContinue
	}
	b := d.script[d.i%len(d.script)]
	d.i++
	switch b % 4 {
	case 0:
		return lisp.DebugContinue
	case 1:
		return lisp.DebugStepInto
	case 2:
		return lisp.DebugStepOver
	default:
		return lisp.DebugStepOut
	}
}

// sendAction delivers one resume command to the engine.
//
// These are ordinary sends on Engine's capacity-1 pauseCh, and they are safe
// to make from the event callback -- which runs on the eval goroutine --
// because pushes and pops are balanced one-for-one: WaitIfPaused invokes the
// callback (one push) and then performs exactly one receive. The channel is
// therefore empty every time the callback runs, so the send completes without
// blocking. Should that invariant ever break, the send blocks the eval
// goroutine and the watchdog reports it as a non-terminating evaluation, which
// is the correct diagnosis rather than a silent hang.
func sendAction(eng *debugger.Engine, a lisp.DebugAction) {
	switch a {
	case lisp.DebugStepInto:
		eng.StepInto()
	case lisp.DebugStepOver:
		eng.StepOver()
	case lisp.DebugStepOut:
		eng.StepOut()
	default:
		eng.Resume()
	}
}

// inspectAtPause runs the surface a DAP client hits at a breakpoint. Return
// values are discarded on purpose: the property under test is that none of
// this panics or wedges on a value the fuzzer built, not that any particular
// rendering is produced.
//
// A panic here does NOT propagate as a Go panic -- lisp.eval's recover() is
// upstack -- which is exactly why the target's assertion is IsInternalPanic on
// the final result rather than an absence of panics observed here.
func inspectAtPause(env *lisp.LEnv, expr *lisp.LVal, eng *debugger.Engine, watch string, reentrant bool) {
	if env == nil {
		return
	}
	for _, set := range [][]debugger.ScopeBinding{
		debugger.InspectLocals(env),
		debugger.InspectScope(env),
		debugger.InspectFunctionLocals(env),
		debugger.InspectMacroExpansion(expr),
	} {
		for i, b := range set {
			if i >= maxInspectBindings {
				break
			}
			_ = debugger.FormatValue(b.Value)
			_ = debugger.FormatValueWith(b.Value, eng)
		}
	}
	if expr != nil {
		_ = debugger.FormatValue(expr)
	}

	// The debug console / Watch pane. This is the sharpest edge in the whole
	// surface: EvalInContext calls env.Eval on the PAUSED environment, so a
	// watch expression is a re-entrant evaluation sharing the outer one's
	// Runtime -- its step counter, its call stack and its still-enabled
	// debugger. A watch that hits a breakpoint therefore pauses inside a
	// pause.
	//
	// `reentrant` is the harness's own guard, not a property of the code under
	// test: without it a watch expression that pauses would re-enter this
	// function and could recurse until the goroutine stack gave out, and the
	// resulting failure would be the harness's rather than the debugger's.
	// One level deep is enough to cover the re-entrancy; deeper is the
	// harness testing itself.
	if !reentrant && watch != "" {
		_ = debugger.EvalInContext(env, watch)
		_ = debugger.EvalSingleInContext(env, watch)
	}

	// Completion over a prefix taken from the paused expression, so the
	// prefix is fuzzer-derived rather than a constant.
	prefix := ""
	if expr != nil && expr.Str != "" {
		prefix = expr.Str
		if len(prefix) > 8 {
			prefix = prefix[:8]
		}
	}
	_ = debugger.CompleteInContext(env, prefix)
	_ = debugger.ExtractPrefix(prefix, len(prefix))
	// A leading ':' is the only way into the keyword branch of completion.
	_ = debugger.CompleteInContext(env, ":"+prefix)
}

// newDebugEnv builds a fully-loaded environment with an engine attached and
// wired to drive itself. Returns the engine so the caller can inspect it after
// the run.
//
// Runtime.Library is left nil for the same reason FuzzEval leaves it nil: it
// is what makes load-file return an error rather than reading the filesystem,
// so fuzzer-generated source cannot reach the test machine's disk.
func newDebugEnv(script, watch []byte) (*lisp.LEnv, *debugger.Engine, *lisp.LVal) {
	eng := debugger.New()
	drv := &driver{script: script}
	pauses := 0
	inPause := false

	eng.SetEventCallback(func(ev debugger.Event) {
		if ev.Type != debugger.EventStopped {
			return
		}
		pauses++
		if pauses > maxPauses {
			// Stop inspecting and stop stepping: from here the program runs to
			// completion under its ordinary budget.
			eng.Resume()
			return
		}
		reentrant := inPause
		inPause = true
		inspectAtPause(ev.Env, ev.Expr, eng, string(watch), reentrant)
		inPause = reentrant
		sendAction(eng, drv.next())
	})

	// Stop on entry so that every program pauses at least once, including one
	// that hits no breakpoint and would otherwise never exercise a pause.
	eng.SetStopOnEntry(true)
	eng.Enable()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Debugger = eng

	stderr := &bytes.Buffer{}
	rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(stderr),
		lisp.WithMaxSteps(fuzzMaxSteps),
		lisp.WithMaxTailIterations(fuzzMaxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(fuzzMaxPhysicalHeight),
		lisp.WithMaxAlloc(fuzzMaxAlloc),
		lisp.WithMaxMacroExpansionDepth(fuzzMacroDepth),
	)
	if rc.Type == lisp.LError {
		return nil, eng, rc
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, eng, rc
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, eng, rc
	}
	return env, eng, nil
}

// fatalf is the subset of *testing.T the harness needs, so the same code
// serves the fuzz target and the ordinary corpus tests.
type fatalf interface {
	Helper()
	Fatalf(format string, args ...interface{})
	Skipf(format string, args ...interface{})
}

// debugEvalBudgeted parses and evaluates src with a debugger attached, on its
// own goroutine, under the watchdog. Returns (result, true) when the source
// evaluated and (nil, false) when it did not parse -- a parse error is the
// parser targets' business.
func debugEvalBudgeted(t fatalf, src, script, watch []byte) (*lisp.LVal, bool) {
	t.Helper()

	// Ask the reader, rather than pattern-matching an error message: both a
	// parse failure and an evaluation failure come back as an LError, and a
	// message heuristic would discard programs that legitimately mention the
	// reader's wording. Running it first also skips the expensive environment
	// build for the majority of mutator output.
	if _, err := parser.NewReader().Read("fuzz", bytes.NewReader(src)); err != nil {
		return nil, false
	}

	env, _, rc := newDebugEnv(script, watch)
	if rc != nil {
		t.Fatalf("could not build the debug fuzz environment: %v", rc)
		return nil, false
	}

	ctx, cancel := context.WithTimeout(context.Background(), fuzzDeadline)
	defer cancel()

	ch := make(chan *lisp.LVal, 1)
	go func() {
		ch <- env.LoadStringContext(ctx, "fuzz", string(src))
	}()

	budget := fuzzwatch.New(watchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case res := <-ch:
			if res == nil {
				t.Fatalf("evaluation returned a nil LVal")
				return nil, false
			}
			return res, true
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				t.Skipf("no verdict: the process was starved throughout (%s)", report)
				return nil, false
			default:
				// A hang here is the most likely SHAPE of a debugger defect --
				// a pause with nothing to resume it -- so say so, since the
				// budgets that bound plain evaluation cannot end it.
				t.Fatalf("debugged evaluation did not terminate within %s of SCHEDULED time"+
					" despite a %s context deadline and a %d-step budget; a pause with no"+
					" resume looks exactly like this (%s)"+
					"\n--- source (%d bytes) ---\n%q"+
					"\n--- action script (%d bytes) ---\n%q"+
					"\n--- watch expression (%d bytes) ---\n%q",
					budget.Total(), fuzzDeadline, int64(fuzzMaxSteps), report,
					len(src), src, len(script), script, len(watch), watch)
				return nil, false
			}
		}
	}
}

// FuzzDebugEval fuzzes evaluation with a debugger attached, stepping and
// inspecting under the fuzzer's direction.
//
// Asserted, and only this:
//
//  1. Evaluation terminates (the watchdog).
//  2. It never recovers a Go panic (IsInternalPanic), including from engine,
//     stepper or inspector code, all of which run inside lisp.eval's recover().
//  3. It never returns a nil LVal.
//  4. The result is printable.
//
// NOT asserted: that any input evaluates successfully, or that a given
// stepping script reaches any particular expression. Almost all mutator output
// is nonsense and an LError is the right answer for it.
func FuzzDebugEval(f *testing.F) {
	// A seed is a triple: the program to run, the stepping script, and the
	// text typed into the Watch pane. All three are mutated independently, so
	// the fuzzer can pair any program with any stepping pattern and any
	// console expression.
	add := func(src, script, watch string) { f.Add([]byte(src), []byte(script), []byte(watch)) }

	for _, src := range fuzzseed.EvalTerminating() {
		add(src, "\x01\x02\x03\x00", "(+ 1 2)")
	}
	for _, src := range fuzzseed.EvalAdversarial() {
		add(src, "\x01", "")
	}
	// Runaway programs matter more here than on the ordinary path: with TRO
	// disabled, a tail loop is stopped by MaxHeightPhysical rather than by
	// MaxTailIterations, and that substitution is worth exercising.
	for _, src := range fuzzseed.EvalRunaway() {
		add(src, "\x00", "")
	}

	// Shapes chosen for the two things a debugger changes about evaluation.
	// Macro-heavy programs drive stampMacroExpansion, which allocates and
	// writes MacroExpansionInfo onto every expanded node only when a debugger
	// is attached; tail loops drive the TRO-disabled path.
	add(`(defmacro m (x) (quasiquote (+ (unquote x) 1))) (m (m (m 1)))`, "\x01\x01\x01", "")
	add(`(defmacro m (n) (if (<= n 0) 1 (quasiquote (identity (m (unquote (- n 1))))))) (m 20)`, "\x01\x02", "")
	add(`(defun loop (n acc) (if (<= n 0) acc (loop (- n 1) (+ acc n)))) (loop 500 0)`, "\x00", "")
	add(`(defun f (n) (if (<= n 0) 0 (+ 1 (f (- n 1))))) (f 50)`, "\x03\x03", "")
	add(`(let ([x 1] [y "s"] [z '(1 2 3)]) (sorted-map "k" z))`, "\x01\x01", "x")
	add(`(handler-bind ([error (lambda (c e) e)]) (error 'boom "x"))`, "\x01\x02\x03", "")
	add(``, "", "")

	// Watch expressions worth having as starting points. A console expression
	// is evaluated re-entrantly in the paused environment against the SAME
	// runtime, so one that defines, mutates, errors, recurses or itself pauses
	// is the interesting case -- not one that merely reads a variable.
	add(`(defun g (n) (* n 2)) (g 3)`, "\x01\x01", `(set 'q 1)`)
	add(`(let ([a 1]) a)`, "\x01", `(defun h (n) (if (<= n 0) 0 (+ 1 (h (- n 1))))) (h 100)`)
	add(`(+ 1 2)`, "\x01", `(error 'watch-boom "x")`)
	add(`(+ 1 2)`, "\x01", `(`)

	f.Fuzz(func(t *testing.T, src, script, watch []byte) {
		res, ok := debugEvalBudgeted(t, src, script, watch)
		if !ok {
			return
		}
		if lisp.IsInternalPanic(res) {
			t.Fatalf("debugged evaluation recovered a Go panic (a host-code defect, not a lisp error)"+
				"\n--- error ---\n%v\n--- source (%d bytes) ---\n%q"+
				"\n--- action script (%d bytes) ---\n%q\n--- watch expression (%d bytes) ---\n%q",
				res, len(src), src, len(script), script, len(watch), watch)
		}
		// String() walks the whole value; a result that cannot be rendered is
		// a defect in its own right.
		_ = res.String()
	})
}

// TestDebuggerChangesTheEvalPath is the guard that keeps this target honest.
//
// Everything above is only worth running because attaching a debugger changes
// evaluation. If that stopped being true -- if TRO were re-enabled under a
// debugger, or macro stamping became unconditional -- FuzzDebugEval would
// quietly become a slower duplicate of FuzzEval, and nothing would say so.
// This pins both differences directly.
func TestDebuggerChangesTheEvalPath(t *testing.T) {
	t.Parallel()

	// A tail-recursive loop deep enough to exceed the physical stack limit
	// only if TRO is off. Without a debugger it runs in constant stack space
	// and completes; with one it pushes a frame per iteration.
	const tailLoop = `(defun loop (n) (if (<= n 0) 'done (loop (- n 1)))) (loop 5000)`
	const physCap = 500

	plain := func() *lisp.LVal {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if rc := lisp.InitializeUserEnv(env,
			lisp.WithMaximumPhysicalStackHeight(physCap),
			lisp.WithMaxTailIterations(1_000_000),
		); rc.Type == lisp.LError {
			t.Fatalf("env: %v", rc)
		}
		if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
			t.Fatalf("lib: %v", rc)
		}
		if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
			t.Fatalf("pkg: %v", rc)
		}
		return env.LoadString("t", tailLoop)
	}()

	if plain.Type == lisp.LError {
		t.Fatalf("without a debugger the tail loop must run in constant stack space"+
			" and complete under a %d-frame cap; got %v", physCap, plain)
	}

	// Same program, same cap, dormant debugger attached: NOT enabled, so no
	// hook fires. TRO is still disabled, because that decision keys off
	// Debugger != nil rather than IsEnabled().
	withDbg := func() *lisp.LVal {
		eng := debugger.New()
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		env.Runtime.Debugger = eng
		if rc := lisp.InitializeUserEnv(env,
			lisp.WithMaximumPhysicalStackHeight(physCap),
			lisp.WithMaxTailIterations(1_000_000),
		); rc.Type == lisp.LError {
			t.Fatalf("env: %v", rc)
		}
		if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
			t.Fatalf("lib: %v", rc)
		}
		if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
			t.Fatalf("pkg: %v", rc)
		}
		if eng.IsEnabled() {
			t.Fatal("the engine must be dormant for this half of the comparison")
		}
		return env.LoadString("t", tailLoop)
	}()

	if withDbg.Type != lisp.LError {
		t.Fatalf("attaching a debugger must disable TRO, so the same tail loop under the"+
			" same %d-frame cap must hit the stack limit; got %v."+
			" If TRO is now preserved under a debugger, FuzzDebugEval no longer covers"+
			" a distinct eval path and this test should be replaced, not deleted",
			physCap, withDbg)
	}
}

// TestDebugSeedsTerminate is the false-positive guard: every seeded
// program/script pair must reach a verdict under the harness. A seed that
// hangs would not fail loudly at 30s of scheduled time -- it would make every
// generation descended from it useless.
func TestDebugSeedsTerminate(t *testing.T) {
	t.Parallel()
	seeds := map[string]string{}
	for name, src := range fuzzseed.EvalTerminating() {
		seeds[name] = src
	}
	for name, src := range fuzzseed.EvalRunaway() {
		seeds["runaway/"+name] = src
	}
	for i, src := range fuzzseed.EvalAdversarial() {
		seeds[fmt.Sprintf("adversarial/%d", i)] = src
	}
	for name, src := range seeds {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			res, ok := debugEvalBudgeted(t, []byte(src), []byte("\x01\x02\x03\x00"), []byte("(+ 1 2)"))
			if !ok {
				return // did not parse; not this target's business
			}
			if lisp.IsInternalPanic(res) {
				t.Fatalf("seed %q recovered a Go panic under the debugger: %v", name, res)
			}
			_ = res.String()
		})
	}
}
