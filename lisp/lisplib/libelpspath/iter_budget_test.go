// Copyright © 2026 The ELPS authors

package libelpspath_test

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/stdlib"
	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Issue #722: iterator steps over a document that shares a subtree along
// many paths.  (set! v (vector v v)) repeated D times has 2^D paths, and D
// iterator steps enumerate every one of them inside one builtin step.  The
// iterator work budget (budget.go) polls the context and charges the work
// past its allowance in steps, so a deadline and a step budget stop it.

// iterBombDepth makes the unfixed walks take seconds to minutes (the
// cheapest, ?set!, about 7s; ?set several minutes), far past the bounds
// below, while the fixed walk stops within milliseconds of its limit.
const iterBombDepth = 26

// iterEnv returns a fresh user environment with the standard library, under
// the given step limit (zero for none), holding setup's bindings.
func iterEnv(t *testing.T, maxSteps int64, setup string) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if maxSteps > 0 {
		lisp.WithMaxSteps(maxSteps)(env)
	}
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := stdlib.Load(env, false); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.LoadString("setup.lisp", setup); rc.Type == lisp.LError {
		t.Fatalf("setup: %v", rc)
	}
	return env
}

// iterBombSetup binds v to a depth-level sharing chain of two-cell vectors
// whose leaf is a one-entry map, and stars to depth iterator steps.
func iterBombSetup(depth int) string {
	return fmt.Sprintf(`(set 'v (sorted-map "k" 1))
(dotimes (i %d) (set! v (vector v v)))
(set 'stars (map 'list (lambda (i) '*) (make-sequence 0 %d)))
()`, depth, depth)
}

// iterBombCalls is every elpspath builtin that takes an iterator, applied
// to v through every level of its sharing.
var iterBombCalls = []string{
	`(apply elpspath:? (concat 'list (list v) stars (list "k")))`,
	`(apply elpspath:?set (concat 'list (list v) stars (list "k" 2)))`,
	`(apply elpspath:?del (concat 'list (list v) stars (list "k")))`,
	`(apply elpspath:?nil (concat 'list (list v) stars (list "k")))`,
	`(apply elpspath:?set! (concat 'list (list v) stars (list "k" 2)))`,
	`(apply elpspath:?del! (concat 'list (list v) stars (list "k")))`,
	`(apply elpspath:?nil! (concat 'list (list v) stars (list "k")))`,
}

// requireCondition fails unless rc is the condition cond.
func requireCondition(t *testing.T, rc *lisp.LVal, cond string) {
	t.Helper()
	if rc.Type != lisp.LError || rc.Str != cond {
		t.Fatalf("got %v %v, want the %s condition", rc.Type, rc, cond)
	}
}

// A 100ms deadline stops every iterator builtin over the bomb, from inside
// the builtin: the call returns promptly with context-cancelled.  Unfixed,
// the builtin ran to completion -- seconds to minutes -- before the
// evaluator next looked at the context.
func TestIterBudgetSharedDocumentHonoursDeadline(t *testing.T) {
	for _, src := range iterBombCalls {
		t.Run(src, func(t *testing.T) {
			env := iterEnv(t, 0, iterBombSetup(iterBombDepth))
			var rc *lisp.LVal
			var elapsed time.Duration
			testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
				ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
				defer cancel()
				start := time.Now()
				rc = env.LoadStringContext(ctx, "probe.lisp", src)
				elapsed = time.Since(start)
			})
			requireCondition(t, rc, lisp.CondContextCancelled)
			if limit := testdeadline.Scale(3 * time.Second); elapsed > limit {
				t.Fatalf("the builtin ignored its deadline: returned after %v (limit %v)", elapsed, limit)
			}
		})
	}
}

// A step budget stops every iterator builtin over the bomb: the work past
// the allowance is charged in steps.  Unfixed, the builtin cost one step
// however much it did.
//
// Each call is also run over (vector v), one iterator step deeper: there the
// stop happens inside the outer iterator's last element, where an element's
// ordinary failure is swallowed and the loop simply ends.  The stop must
// still reach the caller.
func TestIterBudgetSharedDocumentHonoursStepBudget(t *testing.T) {
	calls := slices.Clone(iterBombCalls)
	for _, src := range iterBombCalls {
		calls = append(calls, strings.NewReplacer("(list v)", "(list (vector v))", "stars", "(cons '* stars)").Replace(src))
	}
	for _, src := range calls {
		t.Run(src, func(t *testing.T) {
			env := iterEnv(t, 200_000, iterBombSetup(iterBombDepth))
			var rc *lisp.LVal
			testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
				rc = env.LoadString("probe.lisp", src)
			})
			requireCondition(t, rc, lisp.CondStepLimitExceeded)
		})
	}
}

// The stop is catchable like the evaluator's own: handler-bind sees the
// step-limit condition the builtin raised.
func TestIterBudgetStopIsTheEvaluatorsCondition(t *testing.T) {
	env := iterEnv(t, 200_000, iterBombSetup(iterBombDepth))
	var rc *lisp.LVal
	src := `(handler-bind ((step-limit-exceeded (lambda (c &rest _) 'caught)))
  ` + iterBombCalls[0] + `)`
	testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
		rc = env.LoadString("probe.lisp", src)
	})
	requireCondition(t, rc, lisp.CondStepLimitExceeded)
}

// The linear shape from the issue's second report: ?set over n references
// to one W-wide map must build n fresh maps, n*W entries, in one step.  The
// copies under the iterator are iterator work.
func TestIterBudgetWideSharedMap(t *testing.T) {
	setup := func(n int) string {
		return fmt.Sprintf(`(set 'm (sorted-map))
(dotimes (i %d) (assoc! m (to-string i) i))
(set 'v (map 'vector (lambda (i) m) (make-sequence 0 %d)))
()`, n, n)
	}
	for _, src := range []string{
		`(elpspath:?set v '* "k" 1)`,
		`(elpspath:?del v '* "0")`,
		`(elpspath:?nil v '* "0")`,
	} {
		t.Run(src, func(t *testing.T) {
			t.Run("step budget", func(t *testing.T) {
				// 2048 x 2049 units: about three million past the allowance.
				env := iterEnv(t, 1_000_000, setup(2048))
				var rc *lisp.LVal
				testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
					rc = env.LoadString("probe.lisp", src)
				})
				requireCondition(t, rc, lisp.CondStepLimitExceeded)
			})
			t.Run("deadline", func(t *testing.T) {
				env := iterEnv(t, 0, setup(4096))
				var rc *lisp.LVal
				var elapsed time.Duration
				testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
					ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
					defer cancel()
					start := time.Now()
					rc = env.LoadStringContext(ctx, "probe.lisp", src)
					elapsed = time.Since(start)
				})
				requireCondition(t, rc, lisp.CondContextCancelled)
				if limit := testdeadline.Scale(3 * time.Second); elapsed > limit {
					t.Fatalf("the builtin ignored its deadline: returned after %v (limit %v)", elapsed, limit)
				}
			})
		})
	}
}

// iterSteps evaluates src in env and returns its result and the steps it
// took.
func iterSteps(t *testing.T, env *lisp.LEnv, src string) (*lisp.LVal, int64) {
	t.Helper()
	rc := env.LoadString("probe.lisp", src)
	if rc.Type == lisp.LError {
		t.Fatalf("%s: %v", src, rc)
	}
	return rc, env.Runtime.Steps()
}

// Ordinary iterator queries, well inside the allowance, cost exactly the
// steps they always did: the call's own evaluation and nothing for the
// iterator.  Each query is compared with the same call over an index step
// bound the same way (star is '*, zero is 0), which does the same
// evaluation and visits one element.  The results are checked against the
// same answer computed without elpspath.
func TestIterBudgetOrdinaryQueriesKeepTheirSteps(t *testing.T) {
	setup := `(set 'star '*)
(set 'zero 0)
(set 'recs (map 'vector
  (lambda (i)
    (sorted-map "id" i
                "name" (format-string "rec-{}" i)
                "tags" (vector "a" "b" "c")
                "meta" (sorted-map "x" 1 "y" (list 1 2 3))))
  (make-sequence 0 5000)))
(set 'doc (sorted-map "recs" recs))
()`
	env := iterEnv(t, 1<<40, setup)
	type query struct{ src, want string }
	for _, q := range []query{
		{`(elpspath:? recs %s "id")`, `(map 'vector (lambda (r) (get r "id")) recs)`},
		{`(elpspath:? doc "recs" %s "tags" star)`, `(apply concat 'vector (map 'list (lambda (r) (get r "tags")) recs))`},
		{`(elpspath:? doc "recs" %s "meta" "y" '(range 1))`, ``},
		{`(elpspath:?set doc "recs" %s "name" "x")`, ``},
		{`(elpspath:?del doc "recs" %s "meta")`, ``},
		{`(elpspath:?nil doc "recs" %s "tags" 0)`, ``},
		{`(elpspath:?set doc "recs" %s "tags" '(range 0 1) (vector "z"))`, ``},
		{`(elpspath:?del doc "recs" %s "tags" '(range 1))`, ``},
	} {
		t.Run(q.src, func(t *testing.T) {
			star := strings.Replace(q.src, "%s", "star", 1)
			zero := strings.Replace(q.src, "%s", "zero", 1)
			got, starSteps := iterSteps(t, env, star)
			_, zeroSteps := iterSteps(t, env, zero)
			if starSteps != zeroSteps {
				t.Fatalf("an ordinary iterator query changed its step count: %d steps with '*, %d with an index", starSteps, zeroSteps)
			}
			if q.want != "" {
				want, _ := iterSteps(t, env, q.want)
				if !lisp.True(got.Equal(want)) {
					t.Fatalf("got %v, want %v", got, want)
				}
			}
		})
	}
	// The mutating forms, each on a copy of its own, compared the same way.
	for _, op := range []string{
		`(elpspath:?set! %s %s "name" "x")`,
		`(elpspath:?del! %s %s "meta")`,
		`(elpspath:?nil! %s %s "id")`,
		`(elpspath:?del! %s %s "tags" 0)`,
		`(elpspath:?set! %s %s "tags" '(range 0 1) (vector "z"))`,
		`(elpspath:?nil! %s %s "tags" '(range 1))`,
	} {
		t.Run(op, func(t *testing.T) {
			if rc := env.LoadString("copies.lisp", `(set 'r1 (elpspath:?set recs star "id" 0)) (set 'r2 (elpspath:?set recs star "id" 0)) ()`); rc.Type == lisp.LError {
				t.Fatal(rc)
			}
			_, starSteps := iterSteps(t, env, fmt.Sprintf(op, "r1", "star"))
			_, zeroSteps := iterSteps(t, env, fmt.Sprintf(op, "r2", "zero"))
			if starSteps != zeroSteps {
				t.Fatalf("an ordinary iterator query changed its step count: %d steps with '*, %d with an index", starSteps, zeroSteps)
			}
		})
	}
}

// At the allowance the charge begins, one step per unit of work past it,
// deterministically.  A bare iterator over a flat n-element list is n units.
func TestIterBudgetChargesExactlyTheExcess(t *testing.T) {
	const allowance = 1 << 20
	for _, tc := range []struct{ n, excess int }{
		{allowance - 1, 0},
		{allowance, 0},
		{allowance + 1, 1},
		{allowance + 1000, 1000},
	} {
		t.Run(fmt.Sprint(tc.n), func(t *testing.T) {
			env := iterEnv(t, 1<<40, fmt.Sprintf(`(set 'star '*) (set 'zero 0) (set 'xs (make-sequence 0 %d)) ()`, tc.n))
			got, starSteps := iterSteps(t, env, `(length (elpspath:? xs star))`)
			_, zeroSteps := iterSteps(t, env, `(length (list (elpspath:? xs zero)))`)
			if got.Int != tc.n {
				t.Fatalf("got %d elements, want %d", got.Int, tc.n)
			}
			// The two differ only in the builtin call's argument and the
			// wrapping (list ...), which costs the same as length's
			// argument evaluation on the other side; calibrate on the
			// allowance-free case below it.
			_, baseStar := iterSteps(t, env, `(length (elpspath:? (list 1) star))`)
			_, baseZero := iterSteps(t, env, `(length (list (elpspath:? (list 1) zero)))`)
			if d := (starSteps - zeroSteps) - (baseStar - baseZero); d != int64(tc.excess) {
				t.Fatalf("charged %d steps past the allowance, want %d", d, tc.excess)
			}
			// Deterministic: the same query charges the same again.
			if _, again := iterSteps(t, env, `(length (elpspath:? xs star))`); again != starSteps {
				t.Fatalf("the same query cost %d steps, then %d", starSteps, again)
			}
		})
	}
}

// A copy made under an iterator is iterator work however deep it goes: one
// element holding a map whose off-path entry is a list past the allowance
// costs the steps a flat list that long would.
func TestIterBudgetChargesNestedCopies(t *testing.T) {
	const allowance = 1 << 20
	env := iterEnv(t, 1<<40, fmt.Sprintf(`(set 'star '*) (set 'zero 0)
(set 'v (vector (sorted-map "k" 0 "big" (make-sequence 0 %d))))
()`, allowance))
	_, starSteps := iterSteps(t, env, `(length (elpspath:?set v star "k" 1))`)
	_, zeroSteps := iterSteps(t, env, `(length (elpspath:?set v zero "k" 1))`)
	// One unit for the element, 1+2 for the map, 1+allowance for the list:
	// five past the allowance.
	if d := starSteps - zeroSteps; d != 5 {
		t.Fatalf("the nested copy charged %d steps past the allowance, want 5", d)
	}
}

// The in-place sequence operations that shift or splice a whole vector are
// iterator work in proportion to its width: over n references to one
// W-wide vector each element costs about W, n*W in one step.
func TestIterBudgetWideSharedVector(t *testing.T) {
	const setup = `(set 'w (apply vector (make-sequence 0 4096)))
(set 'v (map 'vector (lambda (i) w) (make-sequence 0 4096)))
()`
	for _, src := range []string{
		`(elpspath:?del! v '* 0)`,
		`(elpspath:?del! v '* '(range 0 1))`,
		`(elpspath:?set! v '* '(range 0 0) (vector 1))`,
		`(elpspath:?nil! v '* '(range 0))`,
		`(elpspath:?set v '* '(range 0 0) w)`,
	} {
		t.Run(src, func(t *testing.T) {
			env := iterEnv(t, 1_000_000, setup)
			var rc *lisp.LVal
			testdeadline.Watch(src, 20*time.Second, 1<<30, func() {
				rc = env.LoadString("probe.lisp", src)
			})
			requireCondition(t, rc, lisp.CondStepLimitExceeded)
		})
	}
}
