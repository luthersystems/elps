package elpstest_test

import (
	"fmt"
	"math/rand"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/stretchr/testify/assert"
)

var (
	parityAdd   = lisp.BuiltinFunc("+")
	parityGet   = lisp.BuiltinFunc("get")
	parityAssoc = lisp.BuiltinFunc("assoc!")
)

// nativeRunner registers (native-add3 a b c) and (native-bump m k) in the
// user package, built with the given step charges.
func nativeRunner(add3Steps, bumpSteps int64, wrongBump bool) *elpstest.Runner {
	return &elpstest.Runner{LoaderFn: func(env *lisp.LEnv) *lisp.LVal {
		if lerr := lisplib.LoadLibrary(env); lerr.Type == lisp.LError {
			return lerr
		}
		if lerr := env.InPackage(lisp.Symbol(lisp.DefaultUserPackage)); lerr.Type == lisp.LError {
			return lerr
		}
		return env.BindBuiltins(lisp.BindOpts{},
			elpsutil.Function("native-add3", lisp.Formals("a", "b", "c"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				if lerr := env.ChargeSteps(add3Steps); lerr.Type == lisp.LError {
					return lerr
				}
				return env.CallBuiltin(parityAdd, args.Cells...)
			}),
			elpsutil.Function("native-bump", lisp.Formals("m", "k"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				if lerr := env.ChargeSteps(bumpSteps); lerr.Type == lisp.LError {
					return lerr
				}
				n := lisp.Int(1)
				if wrongBump {
					n = lisp.Int(2)
				}
				m, k := args.Cells[0], args.Cells[1]
				cur := env.CallBuiltin(parityGet, m, k)
				if cur.Type == lisp.LError {
					return cur
				}
				if cur.IsNil() {
					cur = lisp.Int(0)
				}
				sum := env.CallBuiltin(parityAdd, cur, n)
				if sum.Type == lisp.LError {
					return sum
				}
				return env.CallBuiltin(parityAssoc, m, k, sum)
			}))
	}}
}

const parityLegacy = `
(defun add3 (a b c) (+ a b c))
(defun bump (m k) (assoc! m k (+ (let ([x (get m k)]) (if (nil? x) 0 x)) 1)))
`

func genAdd3(r *rand.Rand) []string {
	vals := []string{"1", "2.5", `"x"`, "()", "-7", "'s"}
	return []string{vals[r.Intn(len(vals))], vals[r.Intn(len(vals))], vals[r.Intn(len(vals))]}
}

// measure finds the native charge that matches the legacy body, the way a
// migration author does, so the test does not hard-code evaluator costs.
func matchingCharge(t *testing.T, check func(int64) elpstest.ParityCheck) int64 {
	for n := range int64(40) {
		if len(check(n).Diff(t)) == 0 {
			return n
		}
	}
	t.Fatal("no charge matches the legacy definition")
	return -1
}

func TestParityCheckPassesMatchingNative(t *testing.T) {
	add3 := func(n int64) elpstest.ParityCheck {
		return elpstest.ParityCheck{
			Runner: nativeRunner(n, 0, false), Legacy: parityLegacy,
			LegacyFn: "add3", NativeFn: "native-add3",
			Cases: [][]string{{"1", "2", "3"}, {"1", `"x"`, "3"}},
			Gen:   genAdd3, N: 30, Seed: 7,
		}
	}
	n := matchingCharge(t, add3)
	add3(n).Run(t)
	// The budget path agrees too: exhaustion lands on the same side of the
	// call on both.
	for budget := int64(1); budget < 12; budget++ {
		c := add3(n)
		c.StepBudget = budget
		c.Run(t)
	}
}

func TestParityCheckReportsDisagreements(t *testing.T) {
	bump := func(steps int64, wrong bool) elpstest.ParityCheck {
		return elpstest.ParityCheck{
			Runner: nativeRunner(0, steps, wrong), Legacy: parityLegacy,
			LegacyFn: "bump", NativeFn: "native-bump",
			Setup:   `(set 'm (sorted-map "a" 1))`,
			Observe: `m`,
			Cases:   [][]string{{"m", `"a"`}, {"m", `"b"`}, {"m", "'c"}},
		}
	}
	n := matchingCharge(t, func(n int64) elpstest.ParityCheck { return bump(n, false) })
	bump(n, false).Run(t)

	// Wrong step count: every case reports steps.
	diffs := bump(n+1, false).Diff(t)
	assert.Len(t, diffs, 3)
	for _, d := range diffs {
		assert.Contains(t, d, "steps: legacy")
	}

	// Wrong value: the result and the observed write both differ.
	diffs = bump(n, true).Diff(t)
	joined := strings.Join(diffs, "\n")
	assert.Contains(t, joined, `case 0 (m "a"): result`)
	assert.Contains(t, joined, `case 0 (m "a"): observed writes`, fmt.Sprint(diffs))

	// Wrong error text is reported as a result difference.
	c := bump(n, false)
	c.NativeFn, c.Cases = "native-add3", [][]string{{"m", `"a"`, "1"}}
	c.IgnoreSteps, c.Observe = true, ""
	c.LegacyFn = "add3"
	assert.Empty(t, c.Diff(t), "same builtin, same error")
	c.Legacy += `(defun add3 (a b c) (error 'boom "different"))`
	diffs = c.Diff(t)
	assert.Len(t, diffs, 1)
	assert.Contains(t, strings.Join(diffs, ""), `error[boom] "different"`)
}
