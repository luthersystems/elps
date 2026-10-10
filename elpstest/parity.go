package elpstest

import (
	"context"
	"fmt"
	"math/rand"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// ParityCheck compares a Go native against the verbatim Lisp definition it
// replaces (luthersystems/elps#745).  For every argument list it builds two
// fresh environments, runs Setup in each, calls LegacyFn in one and NativeFn
// in the other with the same argument expressions, and requires the two calls
// to agree on:
//
//   - the result (rendered), or the error's condition and message;
//   - the evaluation steps the call took (unless IgnoreSteps), which is what
//     an enforced step budget sees -- a native that should charge what its
//     Lisp predecessor charged is held to that on success and on every
//     failing path;
//   - the writes, observed by evaluating Observe after the call (for
//     example the map a mutating function updated).
//
// With StepBudget set the calls run under Runtime.SetStepBudget instead, so a
// budget exhausted partway through must be exhausted on both sides, with the
// same condition.  Step counts are not compared then: a native that charges
// its steps in one LEnv.ChargeSteps call overshoots the budget by the rest of
// that charge where the Lisp stopped one step past it, and both are correct.
//
// Argument lists come from Cases, then, when Gen is set, N lists drawn from
// a rand.Rand seeded with Seed, so a failure is reproducible, then Extra,
// whose cases may add Go-valued arguments and their own setup.  A native
// that deliberately charges differently declares the difference (StepDelta,
// ParityCase.StepDelta) rather than giving up the step comparison.  Arguments are
// Lisp source evaluated in each environment (after Setup), so they may name
// Setup's globals and build fresh values per side: a mutation on one side is
// never seen by the other.
type ParityCheck struct {
	// Runner builds each environment (Runner.NewEnv).  Nil uses a default
	// Runner, which loads the standard library.  Register the native in the
	// runner's LoaderFn (or NewEnvFn).
	Runner *Runner
	// Gen, when non-nil, generates argument lists.
	Gen func(r *rand.Rand) []string
	// Legacy is Lisp source loaded, verbatim, into the legacy environment
	// only: the old defun(s).
	Legacy string
	// LegacyFn and NativeFn name the functions called on each side.  They may
	// be qualified and may be the same name (each side has its own env).
	LegacyFn string
	NativeFn string
	// Setup is Lisp source loaded into both environments before each call.
	Setup string
	// Observe, when non-empty, is a Lisp expression evaluated after the call
	// on both sides; its results (or errors) must agree.
	Observe string
	// Cases are fixed argument lists, each a list of Lisp expressions.
	Cases [][]string
	// N is the number of generated lists (default 100 when Gen is set).
	N int
	// Seed seeds Gen's rand.Rand (default 1).
	Seed int64
	// StepBudget, when positive, runs each call under this step budget and
	// compares results only (see the type doc).
	StepBudget int64
	// IgnoreSteps skips the step comparison.
	IgnoreSteps bool
	// StepDelta, when non-nil, gives the steps the native is expected to
	// take beyond the legacy call (negative for fewer) for each list in
	// Cases and from Gen, for a native that deliberately charges
	// differently.  Nil expects equal counts.
	StepDelta func(args []string) int64
	// Extra are cases with more control than Cases: Go-valued arguments,
	// their own setup, their own expected step delta.
	Extra []ParityCase
}

// ParityCase is one call of a ParityCheck.  Its argument list is Args (Lisp
// expressions) followed by the values GoArgs returns.
type ParityCase struct {
	// GoArgs, when non-nil, builds Go-valued arguments -- values Lisp source
	// cannot spell, such as a map from a custom backing -- once per side,
	// after the setups, in that side's environment.  Each is bound to a
	// fresh global, so it reaches the function as a variable reference
	// costing the same step on both sides.
	GoArgs func(env *lisp.LEnv) []*lisp.LVal
	// Setup is Lisp source loaded after the check's Setup, on both sides.
	Setup string
	// Args are Lisp argument expressions, as in ParityCheck.Cases.
	Args []string
	// StepDelta is the steps the native is expected to take beyond the
	// legacy call.
	StepDelta int64
}

// Run reports every disagreement as a test error.
func (p ParityCheck) Run(t testing.TB) {
	t.Helper()
	for _, d := range p.Diff(t) {
		t.Errorf("%s", d)
	}
}

// Diff returns every disagreement, one line each, without failing t.  It
// fails t (Fatalf) only when an environment cannot be built or Setup or
// Legacy does not load, which is a broken test rather than a disagreement.
func (p ParityCheck) Diff(t testing.TB) []string {
	t.Helper()
	cases := append([][]string(nil), p.Cases...)
	if p.Gen != nil {
		n, seed := p.N, p.Seed
		if n <= 0 {
			n = 100
		}
		if seed == 0 {
			seed = 1
		}
		r := rand.New(rand.NewSource(seed)) //nolint:gosec // reproducible test inputs, not security
		for range n {
			cases = append(cases, p.Gen(r))
		}
	}
	all := make([]ParityCase, 0, len(cases)+len(p.Extra))
	for _, args := range cases {
		c := ParityCase{Args: args}
		if p.StepDelta != nil {
			c.StepDelta = p.StepDelta(args)
		}
		all = append(all, c)
	}
	all = append(all, p.Extra...)
	var diffs []string
	for i, c := range all {
		legacy := p.side(t, true, c)
		native := p.side(t, false, c)
		call := strings.Join(legacy.args, " ")
		if d := compareOutcome("result", legacy.result, native.result); d != "" {
			diffs = append(diffs, fmt.Sprintf("case %d (%s): %s", i, call, d))
		}
		if !p.IgnoreSteps && p.StepBudget <= 0 && native.steps-legacy.steps != c.StepDelta {
			d := fmt.Sprintf("case %d (%s): steps: legacy %d, native %d", i, call, legacy.steps, native.steps)
			if c.StepDelta != 0 {
				d += fmt.Sprintf(" (expected native %+d)", c.StepDelta)
			}
			diffs = append(diffs, d)
		}
		if p.Observe != "" {
			if d := compareOutcome("observed writes", legacy.observed, native.observed); d != "" {
				diffs = append(diffs, fmt.Sprintf("case %d (%s): %s", i, call, d))
			}
		}
	}
	return diffs
}

type parityOutcome struct {
	result, observed *lisp.LVal
	args             []string
	steps            int64
}

func (p ParityCheck) side(t testing.TB, legacy bool, c ParityCase) parityOutcome {
	t.Helper()
	runner := p.Runner
	if runner == nil {
		runner = &Runner{}
	}
	env, err := runner.NewEnv(t)
	if err != nil {
		t.Fatalf("parity: build environment: %v", err)
	}
	fn := p.NativeFn
	if legacy {
		fn = p.LegacyFn
		if p.Legacy != "" {
			if lerr := lisp.GoError(env.LoadString("parity-legacy", p.Legacy)); lerr != nil {
				t.Fatalf("parity: load Legacy: %v", lerr)
			}
		}
	}
	if p.Setup != "" {
		if lerr := lisp.GoError(env.LoadString("parity-setup", p.Setup)); lerr != nil {
			t.Fatalf("parity: load Setup: %v", lerr)
		}
	}
	if c.Setup != "" {
		if lerr := lisp.GoError(env.LoadString("parity-case-setup", c.Setup)); lerr != nil {
			t.Fatalf("parity: load case Setup: %v", lerr)
		}
	}
	args := append([]string(nil), c.Args...)
	if c.GoArgs != nil {
		for i, v := range c.GoArgs(env) {
			name := fmt.Sprintf("elpstest-parity-go-arg-%d", i)
			if lerr := lisp.GoError(env.PutGlobal(lisp.Symbol(name), v)); lerr != nil {
				t.Fatalf("parity: bind Go argument %d: %v", i, lerr)
			}
			args = append(args, name)
		}
	}
	if p.StepBudget > 0 {
		env.Runtime.SetStepBudget(p.StepBudget)
	}
	src := "(" + strings.Join(append([]string{fn}, args...), " ") + ")"
	// A context makes the runtime count steps even without a limit.
	out := parityOutcome{args: args, result: env.LoadStringContext(context.Background(), "parity-call", src)}
	out.steps = env.Runtime.Steps()
	if p.StepBudget > 0 {
		env.Runtime.SetStepBudget(0)
	}
	if p.Observe != "" {
		out.observed = env.LoadString("parity-observe", p.Observe)
	}
	return out
}

func compareOutcome(what string, legacy, native *lisp.LVal) string {
	if (legacy.IsError()) != (native.IsError()) {
		return fmt.Sprintf("%s: legacy %s, native %s", what, renderOutcome(legacy), renderOutcome(native))
	}
	if renderOutcome(legacy) != renderOutcome(native) {
		return fmt.Sprintf("%s: legacy %s, native %s", what, renderOutcome(legacy), renderOutcome(native))
	}
	return ""
}

// renderOutcome renders a value, or an error as its condition and message
// (not its stack, which names different functions on each side).
func renderOutcome(v *lisp.LVal) string {
	if v.IsError() {
		return fmt.Sprintf("error[%s] %q", v.Str, (*lisp.ErrorVal)(v).ErrorMessage())
	}
	return v.String()
}
