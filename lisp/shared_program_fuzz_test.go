// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"fmt"
	"hash/fnv"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/astraw"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The SHARED-PARSE differential target: one Program, several environments.
//
// # The hazard this exists for, stated as a property
//
// FuzzEval's corruption oracle fingerprints one sealed parse around ONE
// evaluation in ONE environment.  That is the weaker half of the promise the
// seal design makes.  The production topology it is meant to protect is a
// process-wide parse cache — substrate#375/#378: a phylum is parsed once and
// the resulting AST is handed to every warm environment that runs a
// transaction.  The property that topology actually needs is
//
//	Parsing is a pure function and evaluation is a pure consumer of the
//	parse: for a shared Program P and independent environments E1..En,
//	evaluating P in Ei must produce exactly what a FRESHLY parsed copy of P
//	produces in a virgin environment, P's sealed bytes must be unchanged
//	after every one of those evaluations, and Ei must not be able to
//	observe anything Ej wrote.
//
// Every clause of that is separately falsifiable, and the two corruption
// bugs this repository has actually shipped falsified different ones:
//
//   - elps#369's stable-sort defect sorted a quoted literal's backing array
//     in place.  The FIRST evaluation returns the right answer; the SECOND
//     sees an already-sorted literal.  A single-evaluation oracle sees the
//     sealed-fingerprint drift but nothing about the wrong ANSWER, and a
//     one-shot differential sees neither — which is why this target
//     evaluates each program 2-3 times per environment and compares the
//     whole sequence.
//   - elps#370's stampMacroExpansion re-stamped a shared node's source
//     location.  Under a shared parse cache that is one environment writing
//     metadata a second environment then reads.
//
// # Why a differential target needs a determinism control
//
// A differential assertion is only sound over DETERMINISTIC programs, and
// the mutator writes whatever it likes: `(time:utc-now)` and friends
// legitimately differ between two runs of the same source.  Declaring those
// crashers would drown the real signal, and pattern-matching the source for
// "nondeterministic builtins" is a blocklist that goes stale the day one is
// added.
//
// So the target controls for it instead of guessing.  The baseline is a
// FRESH parse in a virgin environment; a second fresh parse in a second
// virgin environment is the control.  If the two disagree, the input is
// simply not a deterministic program and no differential claim is made
// about it (the seal oracle still runs — that clause needs no determinism).
// If they agree and a SHARED-parse run then disagrees, the divergence is
// re-confirmed against another matched pair before it is reported, so a rare
// flake has to survive being isolated to the shared-parse variable twice.
// That is the discriminator: the only variable between the baseline and the
// shared runs is whether the AST came out of a cache.
//
// # Assertions
//
//  1. Sealed stability.  lisp.SealedASTFingerprint over the shared Program
//     is re-checked after EVERY evaluation in EVERY environment, not once at
//     the end: knowing which evaluation drifted is most of the triage.
//     Asserted unconditionally, deterministic program or not.
//  2. Result equality.  For every environment and every repetition, the
//     shared-parse result must digest identically to the fresh-parse
//     baseline's result for that repetition.
//  3. No cross-environment state.  After the run, the environment's whole
//     binding state — every package, every symbol, digested through the
//     sorted accessors so Go's randomised map order cannot leak in — must
//     equal the baseline environment's.  This is strictly stronger than
//     "E2 does not observe E1": it catches a leak that changes a binding
//     without changing any returned value.
//  4. Termination and no recovered Go panic, inherited from the budgeted
//     harness the other lisp targets use.
//
// # Not asserted
//
//   - That any input evaluates successfully.  An LError is the right answer
//     for most mutator output; what matters is that it is the SAME LError.
//   - Anything about programs the control shows to be nondeterministic,
//     beyond assertion 1.
//   - Concurrency.  Environments here run one after another, which is the
//     substrate topology (a warm environment serves one transaction at a
//     time) and keeps a crasher reproducible.  Concurrent shared-parse
//     evaluation is a different property and remains uncovered; see the
//     issue #372 comment.

const (
	// sharedMaxEnvs / sharedMaxReps bound the two dimensions the fuzzer
	// chooses.  Small on purpose: three environments and three repetitions
	// already separate every failure shape known (the stable-sort defect
	// needs two repetitions, a cross-environment leak needs two
	// environments), and every extra environment is another full stdlib
	// load, which is what dominates this target's per-input cost.
	sharedMaxEnvs = 3
	sharedMaxReps = 3
)

// FuzzSharedProgramMultiEnv parses a program ONCE and evaluates it in
// several independent environments, several times each, against a
// freshly-parsed baseline.  See the file comment for the property and the
// determinism control.
func FuzzSharedProgramMultiEnv(f *testing.F) {
	// Seeds are the evaluator corpus: programs that actually run are the
	// only ones that can exercise a shared parse at all, and the corpus is
	// already split into terminating and runaway halves for exactly this
	// harness's budget.  The knob byte spreads the environment/repetition
	// choice across the corpus rather than pinning every seed to one shape.
	knob := uint8(0)
	add := func(src string) {
		f.Add(knob, []byte(src))
		knob++
	}
	for _, src := range fuzzseed.EvalTerminating() {
		add(src)
	}
	for _, src := range fuzzseed.EvalAdversarial() {
		add(src)
	}
	// fuzzseed.EvalRunaway is DELIBERATELY not seeded here, unlike in
	// FuzzEval.  A runaway program is stopped by the budget, so it costs the
	// full fuzzDeadline — and this target pays that cost once per
	// repetition per environment, five to twelve times over.  Seeding the
	// runaway corpus put 26 seconds onto every `go test ./...`, and bought
	// nothing: a program that never finishes has no result to compare and
	// leaves no state behind, so both differential assertions are vacuous on
	// it.  The budgets themselves are already asserted by
	// TestEvalRunawaySeedsAreStopped.
	//
	// Hand-written shapes aimed at the property rather than at the
	// evaluator: each one mutates or reads something that a shared parse
	// would carry between environments.  They are written here rather than
	// taken from any downstream corpus.
	for _, src := range sharedProgramSeeds {
		add(src)
	}

	f.Fuzz(func(t *testing.T, knob uint8, src []byte) {
		// Parse FIRST, before any environment is built: unparsable input is
		// most of what the mutator produces, and environment construction is
		// this target's whole cost.
		shared, err := lisp.ReadProgram(parser.NewReader(), "shared", bytes.NewReader(src))
		if err != nil {
			return
		}
		sealed := astraw.Exprs(shared)
		fpSealed := lisp.SealedASTFingerprint(sealed)

		nenv := 2 + int(knob)%(sharedMaxEnvs-1)
		reps := 2 + int(knob/8)%(sharedMaxReps-1)

		// The baseline and the determinism control: two fresh parses, two
		// virgin environments.  Their agreement is the precondition for
		// every differential claim below.
		baseline, ok := runProgramFresh(t, src, reps)
		if !ok {
			return
		}
		control, ok := runProgramFresh(t, src, reps)
		if !ok {
			return
		}
		deterministic := baseline.equal(control)

		for i := range nenv {
			got, ok := runProgramShared(t, shared, sealed, fpSealed, i, reps)
			if !ok {
				return
			}
			if !sharedDivergenceReportable(deterministic, got, baseline) {
				continue
			}
			// A divergence that tracks the shared parse — but only if it
			// survives being isolated a second time.  See the file comment.
			if !confirmSharedDivergence(t, src, shared, sealed, fpSealed, i, reps, baseline) {
				continue
			}
			t.Fatalf("environment %d evaluating a SHARED parse diverged from the freshly-parsed baseline,"+
				" reproducibly and with the fresh-parse control agreeing:"+
				" one parse cannot be reused across environments (the substrate#375/#378 topology)"+
				"\n--- baseline ---\n%s\n--- shared, env %d ---\n%s"+
				"\n--- source (%d bytes) ---\n%q",
				i, baseline, i, got, len(src), src)
			return
		}
	})
}

// sharedProgramSeeds are shapes written for THIS property: each either
// mutates something a quoted literal owns, or reads state a previous
// evaluation could have written.  None comes from any downstream corpus.
var sharedProgramSeeds = []string{
	// elps#369 mechanism 1, verbatim in spirit: a literal sorted in place
	// answers differently on the second call.
	`(defun s () (let ([q '(3 1 2)]) (stable-sort < q))) (list (s) (s))`,
	// elps#369 mechanism 2: slice retains the literal's capacity and append
	// writes through it.
	`(set 'lit '(1 2 3)) (append 'vector (slice 'list lit 0 1) 99) lit`,
	// A literal reached twice through a defun, the shape a warm environment
	// actually runs.
	`(defun f () '(1 2 3)) (append! (f) 4) (f)`,
	// A quoted literal handed to the mutating map ops.
	`(defun m () '(("a" 1))) (assoc! (sorted-map) "k" (m)) (m)`,
	// Macro expansion over a shared node — the elps#370 metadata shape.
	`(defmacro twice (x) (quasiquote (list (unquote x) (unquote x)))) (twice '(1 2))`,
	// Global state a second environment must not see.
	`(if (nil? (get-default (sorted-map) 'seen ())) 'first 'again)`,
	// Nested quoting, the parse node most likely to be aliased into a value.
	`(let ([x ''(1 2 3)]) (reverse 'list (car (cdr x))))`,
	// A literal used as a foldl accumulator seed.
	`(foldl (lambda (acc x) (append 'list acc x)) '() '(1 2 3))`,
}

// sharedDivergenceReportable is the gate that separates a corruption
// finding from a nondeterministic program.  It is a named function so its
// two directions can be pinned by a test rather than only exercised by a
// fuzz run that happens to produce the right input: a gate that silently
// answers "never report" would leave the whole target green forever.
func sharedDivergenceReportable(deterministic bool, got, baseline programRun) bool {
	return deterministic && !got.equal(baseline)
}

// programRun is one environment's whole observable outcome: the digest of
// each repetition's result, in order, plus the digest of the environment's
// binding state afterwards.
type programRun struct {
	results []string
	state   string
}

func (r programRun) equal(other programRun) bool {
	if r.state != other.state || len(r.results) != len(other.results) {
		return false
	}
	for i := range r.results {
		if r.results[i] != other.results[i] {
			return false
		}
	}
	return true
}

func (r programRun) String() string {
	var sb strings.Builder
	for i, fp := range r.results {
		fmt.Fprintf(&sb, "eval %d -> %s\n", i+1, fp)
	}
	fmt.Fprintf(&sb, "env state -> %s", r.state)
	return sb.String()
}

// runProgramFresh parses src again into its OWN Program and evaluates it
// reps times in a virgin environment.  The fresh parse is the point: it is
// what makes this a baseline for "what a shared parse should have produced"
// rather than a second sample of the same shared object.
func runProgramFresh(t *testing.T, src []byte, reps int) (programRun, bool) {
	t.Helper()
	prog, err := lisp.ReadProgram(parser.NewReader(), "fresh", bytes.NewReader(src))
	if err != nil {
		// The shared parse succeeded, so this one must too: parsing is a
		// pure function of the bytes.  A disagreement is a parser defect in
		// its own right and is worth reporting rather than skipping.
		t.Fatalf("re-parsing the same source failed after the first parse succeeded: %v"+
			"\n--- source (%d bytes) ---\n%q", err, len(src), src)
		return programRun{}, false
	}
	return runProgramIn(t, prog, nil, 0, -1, reps)
}

// runProgramShared evaluates the SHARED program in a virgin environment,
// re-checking the sealed fingerprint after every repetition.
func runProgramShared(t *testing.T, shared lisp.Program, sealed []*lisp.LVal, fpSealed uint64, envIdx, reps int) (programRun, bool) {
	t.Helper()
	return runProgramIn(t, shared, sealed, fpSealed, envIdx, reps)
}

// runProgramIn is the shared body.  A nil sealed slice disables the seal
// oracle, which is correct for the fresh-parse baseline: its Program is
// private to the call and nothing else can observe it.
func runProgramIn(t *testing.T, prog lisp.Program, sealed []*lisp.LVal, fpSealed uint64, envIdx, reps int) (programRun, bool) {
	t.Helper()

	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("could not build the fuzz environment: %v", rc)
		return programRun{}, false
	}

	run := programRun{results: make([]string, 0, reps)}
	for rep := range reps {
		result, ok := loadProgramBudgeted(t, env, prog, envIdx, rep)
		if !ok {
			return programRun{}, false
		}
		run.results = append(run.results, valueFingerprint([]*lisp.LVal{result}))

		if sealed == nil {
			continue
		}
		// Assertion 1, after EVERY evaluation rather than once at the end.
		if fpAfter := lisp.SealedASTFingerprint(sealed); fpAfter != fpSealed {
			t.Fatalf("evaluation %d in environment %d corrupted the SHARED sealed parse"+
				" (fingerprint %016x -> %016x): a copy-on-write guard failed and one"+
				" environment wrote storage every other environment shares"+
				" (the substrate#378 class)",
				rep+1, envIdx, fpSealed, fpAfter)
			return programRun{}, false
		}
	}
	run.state = envStateFingerprint(env)
	return run, true
}

// loadProgramBudgeted evaluates prog once under the evaluation budget, on
// its own goroutine, with the scheduled-time watchdog running.  It is
// evalBudgeted's second half, split out because this target must reuse ONE
// environment across repetitions (that is the whole point) where evalBudgeted
// builds a fresh one per call.
func loadProgramBudgeted(t *testing.T, env *lisp.LEnv, prog lisp.Program, envIdx, rep int) (*lisp.LVal, bool) {
	t.Helper()

	ctx, cancel := context.WithTimeout(context.Background(), fuzzDeadline)
	defer cancel()

	ch := make(chan *lisp.LVal, 1)
	go func() {
		ch <- env.LoadProgramContext(ctx, prog)
	}()

	budget := fuzzwatch.New(watchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case result := <-ch:
			if result == nil {
				t.Fatalf("evaluation %d in environment %d returned a nil LVal", rep+1, envIdx)
				return nil, false
			}
			if lisp.IsInternalPanic(result) {
				t.Fatalf("evaluation %d in environment %d recovered a Go panic"+
					" (a host-code defect, not a lisp error)\n--- error ---\n%v",
					rep+1, envIdx, result)
				return nil, false
			}
			// Rendering runs the same walk every error path in the
			// interpreter runs when it formats an operand — but only when
			// the value can be rendered at all.  A program is free to build
			// a self-referential container ((set 'v (vector)) (append! v v)),
			// and LVal.String() has no cycle or depth bound, so rendering
			// one exhausts the goroutine stack: a FATAL runtime error that
			// recover() cannot intercept.  That is a separately-reported
			// defect (see containerRenderable in lisp/containergen_test.go
			// for the full note); reporting it from here as well would
			// crash the worker before this target could say anything about
			// the shared parse.  The differential assertions are unaffected
			// — valueFingerprint terminates on cycles by construction.
			if containerRenderable([]*lisp.LVal{result}) {
				_ = result.String()
			}
			return result, true
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				// Starved throughout. Nothing can be said about this input.
				t.Skipf("no verdict: the process was starved throughout (%s)", report)
				return nil, false
			default:
				t.Fatalf("evaluation %d in environment %d did not terminate within %s of SCHEDULED"+
					" time despite a %s context deadline (%s)",
					rep+1, envIdx, budget.Total(), fuzzDeadline, report)
				return nil, false
			}
		}
	}
}

// confirmSharedDivergence re-runs the divergence with a matched pair — one
// fresh parse, one shared parse — and reports true only when the fresh run
// still agrees with the baseline AND the shared run still disagrees.  A
// program whose nondeterminism the first control happened to miss fails this
// second isolation, which is the point: the target refuses to report a
// crasher it cannot attribute to the shared parse.
func confirmSharedDivergence(t *testing.T, src []byte, shared lisp.Program, sealed []*lisp.LVal, fpSealed uint64, envIdx, reps int, baseline programRun) bool {
	t.Helper()
	fresh, ok := runProgramFresh(t, src, reps)
	if !ok || !fresh.equal(baseline) {
		return false
	}
	again, ok := runProgramShared(t, shared, sealed, fpSealed, envIdx, reps)
	return ok && !again.equal(baseline)
}

// envStateFingerprint digests every binding an environment holds: each
// package, each symbol, each bound value.  Both accessors it walks
// (PackageNames, SymbolNames) return SORTED slices, which is load-bearing —
// Go randomises map iteration per process, so a digest taken over the
// underlying maps would differ between two identical environments and turn
// assertion 3 into a coin flip.
//
// Values are digested with valueFingerprint, which reduces an LFun to its
// type and formals: the stdlib's ~250 function bindings are identical in
// every environment and contribute a constant prefix, while a `defun` the
// program performed shows up as a new symbol name.
func envStateFingerprint(env *lisp.LEnv) string {
	names := fnv.New64a()
	var vals []*lisp.LVal
	for _, pkgName := range env.Runtime.Registry.PackageNames() {
		pkg := env.Runtime.Registry.Package(pkgName)
		if pkg == nil {
			continue
		}
		fmt.Fprintf(names, "pkg:%s;", pkgName)
		for _, sym := range pkg.SymbolNames() {
			fmt.Fprintf(names, "sym:%s;", sym)
			v, _ := pkg.Symbol(sym)
			vals = append(vals, v)
		}
	}
	// The current package is state too: `in-package` is one of the few ways
	// a program changes an environment without binding anything.
	fmt.Fprintf(names, "cur:%s;", env.Runtime.Package.Name)
	return fmt.Sprintf("%016x/%s", names.Sum64(), valueFingerprint(vals))
}

// TestSharedProgramSeedsAreDeterministic is the harness's own gate.
//
// Every differential assertion in this target is skipped for an input the
// control shows to be nondeterministic.  If the hand-written corpus above
// were nondeterministic — or if the control were broken in the direction
// that always reports "nondeterministic" — the target would pass on
// everything and nothing about that would be visible from a green fuzz run.
// This asserts the seeds really are usable as differential inputs.
func TestSharedProgramSeedsAreDeterministic(t *testing.T) {
	t.Parallel()
	for _, src := range sharedProgramSeeds {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			a, ok := runProgramFresh(t, []byte(src), 2)
			if !ok {
				t.Fatalf("seed did not evaluate")
			}
			b, ok := runProgramFresh(t, []byte(src), 2)
			if !ok {
				t.Fatalf("seed did not evaluate on the second run")
			}
			if !a.equal(b) {
				t.Fatalf("seed is nondeterministic, so the differential assertions"+
					" would never run on it\n--- run 1 ---\n%s\n--- run 2 ---\n%s", a, b)
			}
		})
	}
}

// TestSharedProgramSeedsAgreeWithSharedParse runs the whole property over
// the hand-written corpus at `go test` speed, so a regression in the seal
// or in the shared-parse contract is caught in milliseconds by the ordinary
// test suite rather than only by a fuzz sweep.
func TestSharedProgramSeedsAgreeWithSharedParse(t *testing.T) {
	t.Parallel()
	for _, src := range sharedProgramSeeds {
		t.Run(src, func(t *testing.T) {
			t.Parallel()
			shared, err := lisp.ReadProgram(parser.NewReader(), "shared", strings.NewReader(src))
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			sealedExprs := astraw.Exprs(shared)
			fpSealed := lisp.SealedASTFingerprint(sealedExprs)

			baseline, ok := runProgramFresh(t, []byte(src), sharedMaxReps)
			if !ok {
				t.Fatalf("baseline did not evaluate")
			}
			for i := range sharedMaxEnvs {
				got, ok := runProgramShared(t, shared, sealedExprs, fpSealed, i, sharedMaxReps)
				if !ok {
					t.Fatalf("shared run %d did not evaluate", i)
				}
				if !got.equal(baseline) {
					t.Fatalf("environment %d diverged from the freshly-parsed baseline"+
						"\n--- baseline ---\n%s\n--- shared ---\n%s", i, baseline, got)
				}
			}
		})
	}
}

// TestSharedDivergenceGate pins both directions of the gate that decides
// whether a divergence is reported.
//
// The determinism control is what stops this target reporting `(time:utc-now)`
// as AST corruption, and a control stuck at "never report" would be
// indistinguishable from no target at all on a green board.  Asserting the
// gate directly makes both failure modes visible to `go test`, with no
// dependence on the fuzzer happening to generate the right input.
func TestSharedDivergenceGate(t *testing.T) {
	t.Parallel()
	same := programRun{results: []string{"a", "b"}, state: "s"}
	diff := programRun{results: []string{"a", "c"}, state: "s"}
	otherState := programRun{results: []string{"a", "b"}, state: "t"}

	if sharedDivergenceReportable(true, same, same) {
		t.Error("an identical run was reported as a divergence")
	}
	if !sharedDivergenceReportable(true, diff, same) {
		t.Error("a differing RESULT on a deterministic program was not reported;" +
			" the target cannot find the elps#369 shape")
	}
	if !sharedDivergenceReportable(true, otherState, same) {
		t.Error("a differing ENVIRONMENT STATE on a deterministic program was not" +
			" reported; the target cannot find a cross-environment binding leak")
	}
	if sharedDivergenceReportable(false, diff, same) {
		t.Error("a divergence on a NONDETERMINISTIC program was reported;" +
			" every clock-reading program the mutator writes is now a crasher")
	}
}

// TestSharedProgramNondeterminismIsDetected is the other half of the
// control's red-proof: the control must actually be able to SEE
// nondeterminism, not merely be wired into the gate.
//
// The probe reads the wall clock at nanosecond precision and renders it as
// a string, so two runs separated by an environment construction differ
// unless the clock did not advance at all.  Retried a few times so a
// coarse-grained clock on some runner is reported as a skip rather than as
// a failure nobody can act on.
func TestSharedProgramNondeterminismIsDetected(t *testing.T) {
	t.Parallel()
	const src = `(time:format-rfc3339-nano (time:utc-now))`
	for range 5 {
		a, ok := runProgramFresh(t, []byte(src), 2)
		if !ok {
			t.Fatalf("the nondeterministic probe did not evaluate")
		}
		b, ok := runProgramFresh(t, []byte(src), 2)
		if !ok {
			t.Fatalf("the nondeterministic probe did not evaluate twice")
		}
		if !a.equal(b) {
			return // the control can see it; that is the whole assertion
		}
	}
	t.Skipf("the wall clock did not advance across five probe pairs;" +
		" nondeterminism detection cannot be demonstrated on this runner")
}

// TestEnvStateFingerprintDetectsABinding pins the half of assertion 3 that
// a value comparison cannot make: a leak that binds a symbol without
// changing any returned value.
func TestEnvStateFingerprintDetectsABinding(t *testing.T) {
	t.Parallel()
	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("could not build the fuzz environment: %v", rc)
	}
	before := envStateFingerprint(env)
	if lerr := env.LoadString("probe", `(set 'leaked-canary 42)`); lerr.Type == lisp.LError {
		t.Fatalf("probe program failed: %v", lerr)
	}
	if after := envStateFingerprint(env); after == before {
		t.Fatalf("binding a new global did not change the environment state digest (%s);"+
			" assertion 3 is blind to a cross-environment binding leak", before)
	}
}

// TestSharedProgramLoadsLibrary guards the harness against the quietest
// failure mode a fuzz target has: an environment that is not the one
// production runs.  If newFuzzEnv ever stopped loading the standard
// library, every container and quoting path this target exists to cross
// would simply be absent and the sweep would still be green.
func TestSharedProgramLoadsLibrary(t *testing.T) {
	t.Parallel()
	env, _, rc := newFuzzEnv()
	if rc != nil {
		t.Fatalf("could not build the fuzz environment: %v", rc)
	}
	for _, sym := range []string{"stable-sort", "append!", "assoc!", "slice"} {
		if fun := env.GetGlobal(lisp.Symbol(sym)); fun.Type != lisp.LFun {
			t.Fatalf("%s is not bound in the fuzz environment (got %v);"+
				" the shared-parse target is not running the production surface", sym, fun.Type)
		}
	}
}
