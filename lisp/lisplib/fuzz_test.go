// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"context"
	"fmt"
	"io"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/internal/fuzzfp"
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
//
//  2. The result is never a nil *LVal.  A nil return is a contract violation
//     that env.call would have to paper over with a synthesised error.
//
//  3. LVal.String() of the result terminates and does not panic.  Every error
//     path in the interpreter renders its operands, so an unprintable value is
//     a latent crash in the error path.
//
//  4. The three shared singletons (Nil(), Bool(true), Bool(false)) are
//     bit-identical afterwards.  fuzzval deliberately feeds the singletons in
//     as arguments; anything that writes through one corrupts every other
//     holder.  See lisp/singleton.go and issue #274.  Under `go test -tags
//     elpscheck` the same corruption is additionally caught at the next
//     Bool()/Nil() read, which localises it far better -- run
//     `make test-elpscheck` for that.
//     4a. No LNative's contents were changed.  See internal/fuzzfp for why
//     that and not "the arguments are unchanged": several callables mutate an
//     argument by design.  Issue #318 recorded it as unwatchable; it is not,
//     and until now NOTHING about an argument was checked at all -- only the
//     three singletons were, so a builtin that reached into a host's native
//     produced no signal whatsoever.
//
//     fuzzfp's OTHER half -- "no argument's source location was rewritten" --
//     is deliberately not ported.  It read the exported LVal.Source field
//     through a process-wide native-location singleton, and this branch
//     deletes both (#362), so that half no longer compiles here.
//
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

	seedCross(f, names)
	f.Fuzz(func(t *testing.T, idx uint16, data []byte) {
		applyOne(t, names, idx, data)
	})
}

// thinPackages are the three standard-library packages that had no budget of
// their own.
//
// Issue #318 recorded them as the sharpest of the thin-coverage cases:
// libtesting, libhelp and libgolang shared FuzzApplyStdlib's budget with
// libtime, libregexp, libbase64, libjson, libschema, libmath and libstring, so
// "nothing found" for them meant "nothing found in whatever fraction of 400k
// executions the mutator happened to spend on their index range".
//
// They are also the three with the most host-side surface per callable, which
// is what makes a shared budget worst for them specifically: libgolang
// reflects over arbitrary Go values, libhelp walks the package registry and
// renders docstrings, and libtesting reaches into the Go testing runner.
var thinPackages = []string{"testing:", "help:", "golang:"}

// FuzzApplyThinStdlib is FuzzApplyStdlib restricted to thinPackages.
//
// Same harness, same assertions, same generator -- the ONLY difference is the
// callable set, and therefore the budget. A separate target rather than a
// weighting inside the existing one because the fuzzing engine allocates
// -fuzztime per target: a weighting would still be spending one budget, which
// is the thing being fixed.
//
// The broad target still covers these packages. This is additive: a dedicated
// budget on top, not a partition.
func FuzzApplyThinStdlib(f *testing.F) {
	names := thinCallables(f)
	// Per package rather than a single total. The three are very unevenly
	// sized -- testing registers 11 callables, help and golang 4 each -- so a
	// total floor generous enough not to be brittle would not notice help or
	// golang vanishing from LoadLibrary entirely, and this target would stay
	// green while covering a third less than it claims to.
	for _, pkg := range thinPackages {
		n := 0
		for _, name := range names {
			if strings.HasPrefix(name, pkg) {
				n++
			}
		}
		if n == 0 {
			f.Fatalf("package %q registered no callables; it has been dropped from LoadLibrary"+
				" and this target would silently cover only %v", pkg, thinPackages)
		}
	}
	if len(names) < 15 {
		// 19 today. The floor is one package's worth below that.
		f.Fatalf("enumerated only %d callables across %v", len(names), thinPackages)
	}
	seedCross(f, names)
	f.Fuzz(func(t *testing.T, idx uint16, data []byte) {
		applyOne(t, names, idx, data)
	})
}

// thinCallables is callableNames filtered to thinPackages.
func thinCallables(tb fuzzT) []string {
	tb.Helper()
	var out []string
	for _, name := range callableNames(tb) {
		for _, pkg := range thinPackages {
			if strings.HasPrefix(name, pkg) {
				out = append(out, name)
				break
			}
		}
	}
	return out
}

// seedCross builds the seed corpus as the cross of two axes, sampled rather
// than fully crossed. A full cross is 240 callables x 132 value seeds =
// 31,680 entries, and each entry builds a fresh environment (measured 1.08ms),
// so the full cross would add ~34s to every `make test`. The sampled cross is
// ~1100 entries and still guarantees that every callable and every value shape
// appears at least once in the corpus the fuzzer descends from.
func seedCross(f *testing.F, names []string) {
	f.Helper()
	valueSeeds := fuzzval.Seeds()

	// Every callable is applied to at least one value of EVERY shape.
	//
	// This replaced sampling three fixed offsets (0, len/2, len-1) into the
	// seed list, and the replacement is not cosmetic. fuzzval.Seeds() is
	// ordered by kind tag, so a fixed offset names a kind -- and growing the
	// seed list slid those offsets onto different kinds, silently changing
	// what the entire corpus tests. Measured, the slide took libmath from
	// 73.8% to 47.5% mean per-function coverage with libmath.toFloat at zero
	// (not one math builtin received a number any more); re-sampling six
	// spread offsets instead fixed libmath and took four libjson builtins to
	// zero in exchange.
	//
	// A sampled cross always starves someone, and which someone is decided by
	// arithmetic nobody is looking at. fuzzval.KindSeeds() is the small
	// structural list that makes the guarantee explicit instead, and it is
	// derived from the kind count, so a value shape added to the generator is
	// crossed against every callable without anyone remembering to.
	kindSeeds := fuzzval.KindSeeds()
	located := fuzzval.LocatedKindSeeds()
	for i := range names {
		idx := uint16(i) //nolint:gosec // G115: i < len(names), a few hundred
		for _, seed := range kindSeeds {
			f.Add(idx, seed)
		}
		// Plus ONE value carrying a real source location, with the kind
		// rotating by callable index. Every callable therefore meets the
		// population that internal/fuzzfp's "a real location is never
		// replaced" rule needs, and across the name list every kind appears
		// located -- at one extra seed per callable rather than nineteen,
		// which measured as a doubling of the whole corpus's cost for no
		// statement-coverage gain at all.
		f.Add(idx, located[i%len(located)])
	}
	// And every value seed appears at least once, against callables spread
	// through the name list.
	for i, seed := range valueSeeds {
		for _, j := range []int{0, len(names) / 2, len(names) - 1} {
			f.Add(uint16((i+j)%len(names)), seed) //nolint:gosec // G115: modulo len(names)
		}
	}
}

// applyOne is the body both targets share.
func applyOne(t fuzzT, names []string, idx uint16, data []byte) {
	t.Helper()
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

	// Recorded BEFORE the call and while the arguments are still exactly what
	// the generator built. See internal/fuzzfp for what it watches and, more
	// importantly, for what it deliberately does not.
	guard := fuzzfp.Watch(args)

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
	if damage := guard.Check(); damage != "" {
		t.Fatalf("%s corrupted an argument\n%s\n--- args ---\n%s", name, damage, args)
	}
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
func applyWithWatchdog(t fuzzT, env *lisp.LEnv, name string, fun, args *lisp.LVal) *lisp.LVal {
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
func callableNames(tb fuzzT) []string {
	tb.Helper()
	env := newStdlibEnv(tb)
	var names []string
	for _, pkgName := range env.Runtime.Registry.PackageNames() {
		if pkgName == lisp.DefaultUserPackage {
			continue
		}
		pkg := env.Runtime.Registry.Package(pkgName)
		for _, sym := range pkg.SymbolNames() {
			v, _ := pkg.Symbol(sym)
			if v == nil || v.Type != lisp.LFun {
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
func fuzzEnv(tb fuzzT, ctx context.Context) *lisp.LEnv {
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
	if fuzzEnvHook != nil {
		fuzzEnvHook(env)
	}
	return env
}

// fuzzEnvHook is nil except while TestArgumentGuardIsWiredIn runs.
//
// It exists because "internal/fuzzfp detects a corrupted argument" and "this
// target would report one" are different claims, and only the first is
// testable in fuzzfp's own package. A guard constructed AFTER the call, or
// over a copy of the arguments, or whose result was assigned to _, would pass
// every test in internal/fuzzfp and catch nothing here. One nil check per
// input is the price of testing the wiring rather than reading it.
var fuzzEnvHook func(*lisp.LEnv)

func newStdlibEnv(tb fuzzT, configs ...lisp.Config) *lisp.LEnv {
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

// TestArgumentGuardIsWiredIn is the negative control for assertion 4a, in the
// same spirit as TestInternalPanicMarkerIsNotForgeable in
// lisp/eval_fuzz_test.go: it installs a builtin that commits the defect the
// guard claims to catch and asserts the target's own body reports it.
//
// Everything FuzzApplyStdlib says about arguments rests on applyOne recording
// state before the call and checking it after. Those few lines are executed by
// no other test, and getting them subtly wrong -- watching a copy, checking
// before applying, dropping the result -- leaves the target green and blind.
//
// TWO CASES WERE DELETED HERE, and their absence is the point rather than an
// omission. "corrupt-source" (relocating a node) and "corrupt-shared-location"
// (editing a Location many values point at) both drove the source half of
// internal/fuzzfp, which this branch removes: #362 deleted the process-wide
// native-source Location and put the field behind a value-copy accessor, so
// neither corruption can be expressed from a builtin any more. A red-proof
// for a rule that no longer exists passes by doing nothing, which is the
// failure mode this test exists to prevent.
func TestArgumentGuardIsWiredIn(t *testing.T) {
	// NOT parallel: fuzzEnvHook is package state.
	corrupt := []struct {
		name string
		want string // the substring that proves the RIGHT guard fired
		fn   lisp.LBuiltin
	}{{
		// A builtin that reaches inside a host's native value. This is the
		// case issue #318 recorded as uncatchable.
		name: "corrupt-native",
		want: "corrupted an argument",
		fn: func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			for _, a := range args.Cells {
				if a.Type == lisp.LNative {
					if m, ok := a.Native.(map[string]int); ok {
						m["injected"] = 1
						return lisp.Nil()
					}
				}
			}
			return lisp.Nil()
		},
	}}

	for _, c := range corrupt {
		t.Run(c.name, func(t *testing.T) {
			name := "user:" + c.name
			fuzzEnvHook = func(env *lisp.LEnv) {
				// A FIXED arity of one. genArgs draws the variadic count from
				// the input before generating anything, so `&rest` would shift
				// every seed by a byte and the shapes these builtins attack
				// would mostly not be generated -- which looks exactly like a
				// guard that does not work.
				env.AddBuiltins(true, elpsutil.Function(c.name, lisp.Formals("x"), c.fn))
			}
			t.Cleanup(func() { fuzzEnvHook = nil })

			// Drive the REAL target body, with the corrupting builtin as the
			// only callable it can select. Any seed that produces an argument
			// of the shape this builtin attacks must be reported.
			var reported, ran int
			var lastMsg string
			for _, seed := range fuzzval.Seeds() {
				st := &spyT{}
				func() {
					defer func() {
						if r := recover(); r != nil {
							if _, ok := r.(spyFatal); !ok {
								panic(r)
							}
						}
					}()
					applyOne(st, []string{name}, 0, seed)
				}()
				ran++
				if st.failed {
					reported++
					lastMsg = st.msg
				}
			}
			if reported == 0 {
				t.Fatalf("%s corrupted an argument and the target reported nothing across %d seeds;"+
					" the matching assertion is not wired into applyOne", name, ran)
			}
			if !strings.Contains(lastMsg, c.want) {
				t.Fatalf("%s was reported, but not by the guard under test (wanted %q) -- the failure was: %s",
					name, c.want, lastMsg)
			}
			t.Logf("%s: reported on %d of %d seeds", name, reported, ran)
		})
	}
}

// fuzzT is the subset of *testing.T and *testing.F the harness uses, so the
// same body serves both fuzz targets AND can be driven by a spy in
// TestArgumentGuardIsWiredIn. lisp/eval_fuzz_test.go carries the same
// interface for the same reason.
type fuzzT interface {
	Helper()
	Fatalf(format string, args ...interface{})
	Skipf(format string, args ...interface{})
}

// spyT records a Fatalf instead of failing the test, so the wiring test can
// assert that the harness WOULD have reported a defect.
//
// Fatalf panics with a sentinel rather than returning, because every call site
// in the harness treats Fatalf as terminal (`t.Fatalf(...); return nil` and
// the like); letting it fall through would exercise paths the real target
// never takes.
type spyT struct {
	msg    string
	failed bool
}

type spyFatal struct{}

func (s *spyT) Helper() {}
func (s *spyT) Fatalf(format string, args ...interface{}) {
	s.failed = true
	s.msg = fmt.Sprintf(format, args...)
	panic(spyFatal{})
}
func (s *spyT) Skipf(format string, args ...interface{}) { panic(spyFatal{}) }
