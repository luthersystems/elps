// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"context"
	"io"
	"slices"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzval"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// FuzzApplyStdlib applies every callable the standard library registers -- the
// 135 core `lisp` builtins, special operators and macros plus all 105 stdlib
// exports across base64, golang, help, json, math, regexp, s, string, testing
// and time -- to argument lists built by internal/fuzzval.
//
// WHY THIS SURFACE.  Downstream (luthersystems/substrate) a phylum is
// customer-supplied source uploaded and run as Hyperledger Fabric chaincode.
// Every one of these callables is reachable from that phylum with arguments
// the phylum author chooses, so a panic, a hang or a corrupted shared
// singleton in any of them is remotely triggerable.  Before this target the
// only fuzzed stdlib code was libjson's DECODE path.
//
// WHY DIRECT APPLICATION RATHER THAN EVALUATING SOURCE.  Two reasons.
//
//   - Special operators and macros receive UNEVALUATED arguments.  let, cond,
//     dotimes and friends walk a caller-supplied fragment by hand before
//     anything is evaluated, and MacroCall WRITES to the nodes a macro returns.
//     Handing them an arbitrary LVal tree is the calling convention they
//     actually have.
//   - Several argument shapes have no source spelling at all (natives,
//     multi-dimensional arrays, symbol-keyed sorted-maps, error values in
//     argument position).  See the internal/fuzzval package doc.
//
// A panic here is NOT converted to an LError: FunCall / SpecialOpCall /
// MacroCall have no recover(), unlike LEnv.eval.  A crashing input therefore
// surfaces with the Go stack of the offending builtin, which is the whole
// point.  (env.eval's recover is a backstop for embedders, not a licence for
// builtins to panic: it turns the crash into an `internal-panic` condition
// that handler-bind is documented NOT to catch.)
//
// INVARIANTS
//
//  1. No panic escapes the call.
//  2. The result is never a nil *LVal.  A nil return is a contract violation
//     that env.call would have to paper over with a synthesised error.
//  3. LVal.String() of the result terminates and does not panic.  Every error
//     path in the interpreter renders its operands, so an unprintable value is
//     a latent crash in the error path.
//  4. The three shared singletons (Nil(), Bool(true), Bool(false)) are
//     bit-identical afterwards.  fuzzval deliberately feeds the singletons in
//     as arguments; anything that writes through one corrupts every other
//     holder.  See lisp/singleton.go and issue #274.  Under `go test -tags
//     elpscheck` the same corruption is additionally caught at the next
//     Bool()/Nil() read, which localises it far better -- run
//     `make test-elpscheck` for that.
//  5. The call terminates.  Bounded by a context deadline, a step limit and,
//     because neither of those can see a loop that evaluates nothing, an
//     out-of-band watchdog.  Two of the three defects this target found were
//     invisible to the first two: (pow -128 <MaxInt>) allocates nothing and
//     evaluates nothing, and (dotimes (i 2147483647)) with an empty body
//     counted no steps.
//
// NOT ASSERTED
//
//   - Reflexivity.  LVal.Equal returns false for LError, LFun, LQuote, LBytes
//     and LNative even against the same object, and (= NaN NaN) is false.  An
//     `x.Equal(x)` assertion would fire on correct behaviour.
//   - Refusal is not failure.  MaxAlloc is set deliberately LOW (see
//     fuzzMaxAlloc) so the fuzzer spends its budget exploring the allocation
//     guard instead of OOM-ing the runner.  A builtin returning "allocation
//     size N exceeds maximum" is the guard working.
//   - Any particular error message or condition type.
func FuzzApplyStdlib(f *testing.F) {
	// Enumerate once outside the fuzz body: the callable set is a property of
	// the library, not of the input, and re-deriving it per iteration would
	// dominate the runtime.
	names := callableNames(f)
	if len(names) < 200 {
		// A harness that silently enumerates nothing cannot fail. The library
		// registers 240 callables today across 11 packages; 200 is a floor
		// that a package being dropped from LoadLibrary would breach.
		f.Fatalf("enumerated only %d callables; the stdlib surface should be far larger", len(names))
	}

	// The seed corpus is the cross of two axes, sampled rather than fully
	// crossed. A full cross is 240 callables x 57 value seeds = 13,680 entries,
	// and each entry builds a fresh environment (measured 1.08ms), so the full
	// cross would add ~15s to every `make test`. The sampled cross is ~890
	// entries (~1s) and still guarantees that every callable and every value
	// shape appears at least once in the corpus the fuzzer descends from.
	valueSeeds := fuzzval.Seeds()
	for i := range names {
		idx := uint16(i) //nolint:gosec // G115: i < len(names), a few hundred
		for _, j := range []int{0, len(valueSeeds) / 2, len(valueSeeds) - 1} {
			f.Add(idx, valueSeeds[j])
		}
	}
	for i, seed := range valueSeeds {
		for _, j := range []int{0, len(names) / 2, len(names) - 1} {
			f.Add(uint16((i+j)%len(names)), seed) //nolint:gosec // G115: modulo len(names)
		}
	}

	f.Fuzz(func(t *testing.T, idx uint16, data []byte) {
		name := names[int(idx)%len(names)]
		if skipCallable(name) {
			return
		}

		before := lisp.TakeSingletonSnapshot()

		ctx, cancel := context.WithTimeout(context.Background(), callDeadline)
		defer cancel()

		env := fuzzEnv(t, ctx)
		fun := env.GetGlobal(lisp.Symbol(name))
		if fun.Type != lisp.LFun {
			t.Fatalf("%s: expected a function, got %v", name, fun.Type)
		}

		gen := fuzzval.New(data, env)
		args := lisp.SExpr(genArgs(gen, fun))

		result := applyWithWatchdog(t, env, name, fun, args)

		if result == nil {
			t.Fatalf("%s returned a nil *LVal", name)
		}
		// Rendering the result exercises the same code every error path in the
		// interpreter runs when it formats an operand.
		_ = result.String()

		if drift := before.Verify(); drift != "" {
			t.Fatalf("%s mutated the shared singleton %s\n--- args ---\n%s", name, drift, args)
		}
	})
}

// callDeadline bounds one application.  Generous relative to the work any
// correct builtin does with fuzzval-sized arguments (the whole seed corpus
// runs in well under a second), tight enough that a genuine non-terminating
// loop is reported rather than eating the target's whole -fuzztime.
const callDeadline = 5 * time.Second

// watchdogGrace is how much SCHEDULED time past callDeadline the watchdog
// waits before declaring the call unbounded.  A call that has blown the
// context deadline by this much is not "slow": it is in a loop that never
// consults the context.
const watchdogGrace = 10 * time.Second

// fuzzMaxAlloc is the per-operation allocation cap.  Deliberately tiny.  The
// default is 10M elements; at that size a single (make-sequence <big>) costs
// hundreds of milliseconds and a handful of them OOM a CI runner, so the
// fuzzer would spend its budget on the allocator rather than on the code under
// test.  At 4096 the guard itself is what gets explored, which is the
// interesting boundary.
const fuzzMaxAlloc = 4096

// fuzzMaxSteps bounds evaluation work for the callables that evaluate their
// arguments (every special operator, every macro expansion).  Small enough
// that a runaway recursion is cut off in milliseconds.
const fuzzMaxSteps = 20000

// applyWithWatchdog applies fun to args on a separate goroutine and fails the
// test if the call outlives the context deadline by watchdogGrace of SCHEDULED
// time -- wall clock during which this process was not being run by the OS is
// not charged to the callable.  See internal/fuzzwatch for why: at a measured
// 0.73ms mean per input the 15s bound is already ~20,000x the work, so a
// watchdog that fires is either a real hang or a starved runner, and only the
// first is a defect.
//
// A watchdog is necessary because neither of the interpreter's own bounds sees
// every loop.  checkLimits is consulted per EVALUATION; a builtin that loops
// in Go without evaluating anything -- powInt's doubling loop, opDoTimes with
// an empty body -- consults nothing and is bounded by nothing.  `go test
// -fuzz` has no per-input timeout, so without this the failure mode is the
// whole test binary being killed by -timeout with no crasher written out.
//
// The goroutine is deliberately leaked on timeout: it cannot be interrupted
// (that is the defect being reported), and a fuzz worker that reports a
// crasher exits immediately afterwards.
func applyWithWatchdog(t *testing.T, env *lisp.LEnv, name string, fun, args *lisp.LVal) *lisp.LVal {
	t.Helper()
	done := make(chan *lisp.LVal, 1)
	panicked := make(chan any, 1)
	go func() {
		defer func() {
			if r := recover(); r != nil {
				panicked <- r
			}
		}()
		done <- apply(env, fun, args)
	}()
	// The budget is SCHEDULED time, not wall clock: see internal/fuzzwatch.  A
	// plain time.After here charges the code under test for every second this
	// process spent descheduled on a contended runner, which is a red board
	// nobody can reproduce rather than a defect.
	budget := fuzzwatch.New(callDeadline + watchdogGrace)
	wait := budget.Total()
	for {
		select {
		case v := <-done:
			return v
		case r := <-panicked:
			// Re-panic on the test goroutine so the fuzzing engine records the
			// crasher and prints the stack.
			panic(r)
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				// The machine never gave us the CPU. Whether this call
				// terminates is unknown, and guessing in either direction is
				// worse than declining to answer for one input out of millions.
				t.Skipf("%s: no verdict, the process was starved throughout (%s)", name, report)
				return nil
			default:
				t.Fatalf("%s did not terminate within %v of SCHEDULED time despite a %v context deadline and a %d-step limit (%s)\n--- args ---\n%s",
					name, budget.Total(), callDeadline, fuzzMaxSteps, report, args)
				return nil
			}
		}
	}
}

// apply dispatches to the calling convention the callable actually has.
// Special operators and macros must NOT go through FunCall: it rejects them,
// and more importantly it would evaluate nothing but also bind nothing,
// missing the unevaluated-fragment surface entirely.
func apply(env *lisp.LEnv, fun, args *lisp.LVal) *lisp.LVal {
	switch {
	case fun.IsSpecialOp():
		return env.SpecialOpCall(fun, args)
	case fun.IsMacro():
		return env.MacroCall(fun, args)
	default:
		return env.FunCall(fun, args)
	}
}

// genArgs builds an argument list sized against the callable's declared
// formals.
//
// Arity is respected on purpose.  Builtins index args.Cells directly --
// builtinPow's first statement is `args.Cells[0], args.Cells[1]` -- because
// LEnv.bind guarantees the arity before the builtin runs.  Handing a two-arg
// builtin zero arguments would report a harness bug as a builtin bug.  What is
// fuzzed is the VALUE of each argument, plus the variadic count where the
// formals declare one.
func genArgs(gen *fuzzval.Gen, fun *lisp.LVal) []*lisp.LVal {
	formals := fun.Cells[0]
	required := 0
	variadic := false
	for _, sym := range formals.Cells {
		switch sym.Str {
		case lisp.VarArgSymbol, lisp.OptArgSymbol, lisp.KeyArgSymbol:
			variadic = true
		default:
			if !variadic {
				required++
			}
		}
	}
	n := required
	if variadic {
		n += gen.Intn(4)
	}
	args := make([]*lisp.LVal, 0, n)
	for range n {
		args = append(args, gen.Value())
	}
	return args
}

// skipCallable names the callables excluded from the sweep, with the reason
// each is excluded.  The list is deliberately short: an exclusion is a hole in
// the gate.
func skipCallable(name string) bool {
	switch name {
	// time:sleep was excluded here until issue #314 was fixed: it blocked for
	// a caller-supplied duration inside a single evaluation step and no
	// budget in the interpreter could bound it.  It now selects on the
	// evaluation context and caps its wait at the context deadline, so
	// callDeadline bounds it like everything else and it is back in the
	// sweep.  Coverage of the sleeping path is nominal rather than real:
	// fuzzval has no time.Duration among its nativeValues, so generated
	// arguments reach the builtin's type check and stop there.  The bound
	// itself is asserted directly in libtime's TestSleep* tests.
	case "testing:test", "testing:test-let", "testing:test-let*",
		"testing:benchmark", "testing:benchmark-simple":
		// libtesting drives the Go testing runner through the environment and
		// reports through it. Applied outside elpstest.Runner it reports
		// against the FUZZ TARGET's own *testing.T, so a generated argument
		// can fail or skip the fuzz iteration itself -- the harness would be
		// grading its own input. Their non-runner code paths are ordinary
		// builtins covered by libtesting's own tests.
		return true
	default:
		return false
	}
}

// callableNames returns every function, macro and special operator registered
// by the standard library, as qualified symbols, sorted so the index a crasher
// records means the same thing on every machine and every run.
//
// Sorting matters more than it looks: Go map iteration is randomised, so an
// unsorted list would make a saved crasher select a DIFFERENT callable on
// replay, and the regression case would silently stop testing what it caught.
func callableNames(tb testing.TB) []string {
	tb.Helper()
	env := newStdlibEnv(tb)
	var names []string
	for pkgName, pkg := range env.Runtime.Registry.Packages {
		for sym, v := range pkg.Symbols {
			if v == nil || v.Type != lisp.LFun {
				continue
			}
			if pkgName == lisp.DefaultUserPackage {
				continue
			}
			names = append(names, pkgName+":"+sym)
		}
	}
	slices.Sort(names)
	return slices.Compact(names)
}

// fuzzEnv builds a fresh environment per iteration.
//
// Fresh, not cached, because these callables MUTATE the environment: set,
// defun, in-package, use-package and export all write to it. A cached env
// would make iteration N's result depend on iterations 0..N-1, so a saved
// crasher would not reproduce from a clean start -- which is the one property
// a regression corpus has to have.
func fuzzEnv(tb testing.TB, ctx context.Context) *lisp.LEnv {
	tb.Helper()
	env := newStdlibEnv(tb,
		lisp.WithContext(ctx),
		lisp.WithMaxSteps(fuzzMaxSteps),
		lisp.WithMaxAlloc(fuzzMaxAlloc),
	)
	// debug-print, debug-stack and trace write to the runtime's stderr.
	// Left at os.Stderr they bury the fuzzer's own output.
	env.Runtime.Stderr = io.Discard
	// load-file resolves through the source library. Nil-ing it turns every
	// filesystem read into a clean lisp-level error instead of letting a
	// generated string name a path on the runner.
	env.Runtime.Library = nil
	return env
}

func newStdlibEnv(tb testing.TB, configs ...lisp.Config) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	for _, cfg := range configs {
		if rc := cfg(env); rc.Type == lisp.LError {
			tb.Fatalf("config: %v", rc)
		}
	}
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		tb.Fatalf("load-library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}
