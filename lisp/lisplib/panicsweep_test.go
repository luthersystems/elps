// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"bytes"
	"fmt"
	"math"
	"sort"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// TestBuiltinRegistryNeverPanics asserts the invariant of issue #367 over the
// whole registered surface: no evaluated program, over any data, may take the
// host process down with a Go panic.
//
// # What it does
//
// It walks every builtin, special operator and macro reachable from the loaded
// standard library and calls each one THROUGH THE EVALUATOR, with a matrix of
// adversarial argument values in every position: values of the wrong type for
// what the function reads, empty and multi-dimensional containers, extreme
// integers, NaN and Inf, an error value, a native value, a tagged value and a
// function value.  Roughly 800k calls.
//
// # Why through the evaluator, and not by calling the LBuiltin directly
//
// Because "lisp-reachable" is the criterion.  Calling the Go function directly
// bypasses arity checking, so it manufactures argument lists the evaluator
// never produces, and it answers a DIFFERENT question -- the one
// TestOptionalArgBuiltinsTolerateShortArgLists already answers for short
// argument lists.  Here the arity is what a program could actually write and
// only the argument VALUES are hostile, so a failure is a reproduction rather
// than a hypothetical.
//
// # The load-bearing assertion
//
// env.eval recovers every Go panic into an ordinary-looking *LVal, so "the
// test process survived" proves nothing at all.  The assertion with teeth is
// lisp.IsInternalPanic(result) == false, exactly as in lisp/eval_fuzz_test.go:
// it keys off the Go-stack snapshot the recover handler attaches to the error,
// which nothing reachable from lisp can forge.
//
// # What it is for
//
// The panic sites left in the interpreter -- the type-asserting accessors
// (LVal.Bytes, LVal.Map, LVal.FunData, LVal.CallStack), seqCells, toFloat --
// are unreachable from lisp only because every CALLER guards.  Each one now
// carries that argument in a comment.  A comment does not stay true on its
// own: this test is what re-checks the arguments on every run, so the next
// builtin that validates its type specifier but not its sequence -- which is
// what `(append 'bytes 0)` was -- is found mechanically instead of in an
// embedder's production process.  That is the shape #355 established for
// CondMissingArgument, applied to type confusion instead of arity.
//
// # What it does not cover
//
// One call, at the arity a program would write, over a fixed value matrix.  It
// does not compose calls, mutate a value between them, or drive the special
// operators' control structure.  FuzzEval covers those, over program TEXT; the
// two are complementary and neither subsumes the other.
func TestBuiltinRegistryNeverPanics(t *testing.T) {
	names := registeredCallables(t)
	require.NotEmpty(t, names, "found no builtins to check; the registry walk is broken")
	t.Logf("sweeping %d registered functions", len(names))

	// The skip list must not rot. A name that leaves the registry, or is
	// renamed, would otherwise sit here looking like coverage while excluding
	// nothing -- and the reverse mistake, a skip that silently stopped
	// applying, would let the sweep call something it must not.
	registered := make(map[string]bool, len(names))
	for _, name := range names {
		registered[name] = true
	}
	for name := range panicSweepSkips {
		require.Truef(t, registered[name],
			"the skip list names %s, which is not registered; drop the entry or fix the name", name)
	}

	var evaluated, answered int
	for _, name := range names {
		if reason, skip := panicSweepSkips[name]; skip {
			t.Logf("skipping %s: %s", name, reason)
			continue
		}
		// A fresh environment per function. Evaluation mutates global state
		// (set, defun, in-package, deftype), so a shared one would make a
		// failure depend on every call that ran before it, and a reproduction
		// that needs 800k predecessors is not a reproduction.
		env := panicSweepEnv(t)
		vals := panicSweepValues(env)
		n := namedFormalCount(lookupFormals(t, env, name))
		if n == 0 {
			continue
		}
		// Only the first three positions are varied exhaustively; beyond that
		// the cross product stops being affordable and the remaining cells
		// rotate through the same choices. Three is enough for every shape
		// this is aimed at: a type specifier, the value it mistypes, and an
		// index or count that indexes it.
		vary := min(n, 3)
		idx := make([]int, vary)
		for {
			cells := make([]*lisp.LVal, 0, n+1)
			cells = append(cells, lisp.Symbol(name))
			for i := range n {
				// Quote every argument: these are VALUES, and an unquoted
				// symbol or list would be evaluated into something else
				// before the function ever saw it.
				cells = append(cells, lisp.Quote(vals[idx[i%vary]].Copy()))
			}
			res := env.Eval(lisp.SExpr(cells))
			evaluated++
			if res.Type != lisp.LError {
				answered++
			}
			if lisp.IsInternalPanic(res) {
				args := make([]string, 0, n)
				for i := range n {
					args = append(args, safeString(vals[idx[i%vary]]))
				}
				t.Fatalf("(%s %s) panicked the host: %v"+
					"\na lisp program must not be able to do this (#367): the failure must be"+
					"\nan ordinary condition the caller can handler-bind, so the builtin needs a"+
					"\ntype check where it currently trusts its argument",
					name, strings.Join(args, " "), res)
			}
			k := vary - 1
			for k >= 0 {
				idx[k]++
				if idx[k] < len(vals) {
					break
				}
				idx[k] = 0
				k--
			}
			if k < 0 {
				break
			}
		}
	}

	// Guard against a vacuous pass. Every call above could be rejected on
	// arity or on the first argument's type without ever reaching the code
	// this is aimed at, and the test would report success having exercised
	// nothing. The floor is well under the measured count (~837k evaluated,
	// ~34k answered) so ordinary growth does not have to retune it, but a
	// registry walk that silently stops matching, or a change that makes
	// every call fail early, fails here instead of passing.
	t.Logf("evaluated %d calls, %d of which produced a value rather than an error", evaluated, answered)
	require.Greater(t, evaluated, 100_000, "the sweep evaluated too few calls to be meaningful")
	require.Greater(t, answered, 5_000,
		"almost every call in the sweep errored; the arguments are being rejected before"+
			" reaching the code paths this is aimed at")
}

// safeString renders a value for a failure message without trusting it to be
// renderable.  Rendering is itself interpreter code reachable from lisp, so a
// value that panics a builtin may well panic String too -- and a report that
// dies while describing the defect it found reports nothing.  The recovered
// case still names the type, which is the part that identifies the input.
func safeString(v *lisp.LVal) (s string) {
	defer func() {
		if r := recover(); r != nil {
			s = fmt.Sprintf("<unrenderable %v: %v>", v.Type, r)
		}
	}()
	return v.String()
}

// panicSweepSkips names the registered functions the sweep must not call, and
// why. Each is a function whose EFFECT, not whose failure mode, makes it
// unsuitable -- none is excluded for being suspected of panicking.
var panicSweepSkips = map[string]string{
	// The loaders evaluate their argument as a program. That is FuzzEval's
	// job, done there under a step and time budget this test does not set up;
	// running it here would make the sweep's cost depend on what the matrix
	// happens to spell.
	"lisp:load-file":   "evaluates its argument as a program",
	"lisp:load-string": "evaluates its argument as a program",
	"lisp:load-bytes":  "evaluates its argument as a program",
	"time:sleep":       "blocks for the duration in its argument",
	// The testing package runs its body as a subtest against the ambient
	// *testing.T, so calling it from inside one reports failures for programs
	// this test made up.
	"testing:test":      "runs its body as a test",
	"testing:test-let":  "runs its body as a test",
	"testing:benchmark": "runs its body as a benchmark",
}

// panicSweepEnv builds a fully loaded environment under a bounded evaluation
// budget, with debug output captured rather than written to the test's
// streams.
//
// Runtime.Library is left nil on purpose, as in newFuzzEnv: that is what makes
// load-file return an error instead of reading the filesystem, so a call this
// test generates cannot escape onto the test machine's disk.
func panicSweepEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(&bytes.Buffer{}),
		lisp.WithMaxSteps(200_000),
		lisp.WithMaxTailIterations(10_000),
		lisp.WithMaximumPhysicalStackHeight(500),
		lisp.WithMaxAlloc(100_000),
	)
	require.NotEqual(t, lisp.LError, rc.Type, "initialize-user-env: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.NotEqual(t, lisp.LError, rc.Type, "load-library: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.NotEqual(t, lisp.LError, rc.Type, "in-package: %v", rc)
	return env
}

// panicSweepValues is the adversarial matrix. Every entry is a value a lisp
// program can hold, and every entry is the wrong type for most of the places
// it will be offered.
func panicSweepValues(env *lisp.LEnv) []*lisp.LVal {
	m := lisp.SortedMap()
	m.MapSet("a", lisp.Int(1))
	nested := lisp.SortedMap()
	nested.MapSet("a", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.SortedMap()}))
	nested.MapSet("b", lisp.Bytes([]byte("z")))
	return []*lisp.LVal{
		lisp.Nil(),
		lisp.Bool(true),
		lisp.Int(0),
		lisp.Int(2),
		lisp.Int(-1),
		lisp.Int(math.MaxInt),
		lisp.Int(math.MinInt),
		lisp.Float(1.5),
		lisp.Float(math.NaN()),
		lisp.Float(math.Inf(1)),
		lisp.String(""),
		lisp.String("x"),
		lisp.Symbol("x"),
		// The type specifiers the sequence and byte builtins dispatch on.
		// Offering them is what gets past the FIRST argument's validation and
		// into the code that reads the second one -- the shape
		// `(append 'bytes 0)` had.
		lisp.Symbol("bytes"),
		lisp.Symbol("list"),
		lisp.Symbol("vector"),
		lisp.Symbol("string"),
		lisp.Bytes([]byte("ab")),
		lisp.Bytes(nil),
		m,
		nested,
		// A MULTI-dimensional array: not a sequence, and the value that
		// reaches seqCells' first panic if a caller forgets isSeq.
		lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(2)}),
			[]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}),
		lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}),
		lisp.Vector(nil),
		// An array built the way an embedder builds one, with no backing
		// storage supplied. Every element is Nil; it was a Go nil *LVal
		// before #367, and reading one panicked the host.
		lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(3)}), nil),
		lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}),
		lisp.QExpr([]*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.String("a"), lisp.Int(1)})}),
		lisp.Errorf("boom"),
		lisp.Native(struct{}{}),
		env.GetGlobal(lisp.Symbol("car")),
		env.TaggedValue(lisp.Symbol("sweep-type"), lisp.Int(1)),
	}
}

// registeredCallables returns the qualified name of every builtin, special
// operator and macro in the registry, in a stable order.
//
// The `user` package is skipped: it re-exports `lisp` rather than registering
// anything of its own, so including it would double the run for no new code.
func registeredCallables(t *testing.T) []string {
	t.Helper()
	env, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	var names []string
	for pkgName, pkg := range env.Runtime.Registry.Packages {
		if pkgName == lisp.DefaultUserPackage {
			continue
		}
		for sym, v := range pkg.Symbols {
			if v == nil || v.Type != lisp.LFun || v.Builtin() == nil || len(v.Cells) == 0 {
				continue
			}
			names = append(names, pkgName+":"+sym)
		}
	}
	sort.Strings(names)
	return names
}

// lookupFormals returns the formal argument list bound to a qualified name.
func lookupFormals(t *testing.T, env *lisp.LEnv, qualified string) *lisp.LVal {
	t.Helper()
	pkgName, sym, ok := strings.Cut(qualified, ":")
	require.True(t, ok, "not a qualified symbol: %s", qualified)
	pkg := env.Runtime.Registry.Packages[pkgName]
	require.NotNil(t, pkg, "package not in registry: %s", pkgName)
	v := pkg.Symbols[sym]
	require.NotNil(t, v, "symbol not in package: %s", qualified)
	require.NotEmpty(t, v.Cells, "function has no formals: %s", qualified)
	return v.Cells[0]
}
