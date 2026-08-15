// Copyright © 2026 The ELPS authors

package elpstest

import (
	"flag"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// This file covers issue #365: RunBenchmark parsed its source once, outside
// the loop, and then evaluated that one tree in every b.N iteration. Each
// iteration got a fresh Runtime but the same AST nodes, so iteration k saw
// whatever iterations 1..k-1 left behind in them.
//
// That is not hypothetical in ELPS. A quoted literal in a source expression is
// an AST node, and `stable-sort` is documented to sort in place and is pinned
// as doing so through a literal by lisp.TestSliceViewSharesElements. So a
// benchmark whose source sorts a literal sorts unsorted input on iteration 1
// and already-sorted input on every iteration after it -- the harness quietly
// substitutes a different workload for the one the benchmark declares, and the
// reported number depends on b.N.
//
// The same reasoning is why lisp.TextLoader copies: it calls expr.Copy() on
// every load precisely so two loads of one file cannot share nodes.
const staleASTSource = `
  ;; A quoted literal inside a function body is part of the parsed tree.
  (defun literal () '(3 1 2))

  ;; Detector: the literal must be pristine when an iteration begins. On a
  ;; shared tree it is not, because the sort below already reordered it.
  (if (equal? (literal) '(3 1 2))
      ()
      (error 'stale-ast "iteration began with an AST a previous iteration mutated"))

  ;; Ordinary ELPS that mutates the tree: stable-sort sorts in place.
  (stable-sort < (literal))
`

// staleASTIterations bounds the benchmark this test drives. Two iterations
// would be enough to observe the sharing, but a benchmark measured by time
// would run for -benchtime of *timed* work while paying the untimed
// per-iteration environment build on top, which for a source this small is
// several seconds of wall clock for a property that shows up on iteration 2.
const staleASTIterations = "20x"

// TestRunBenchmarkGivesEachIterationAFreshAST is the catch for #365. On main
// the second iteration evaluates the literal the first one sorted, the
// detector in staleASTSource raises stale-ast, RunBenchmark calls b.Fatalf,
// and testing.Benchmark reports a zero result.
func TestRunBenchmarkGivesEachIterationAFreshAST(t *testing.T) {
	if f := flag.Lookup("test.benchtime"); f != nil {
		prev := f.Value.String()
		if err := f.Value.Set(staleASTIterations); err != nil {
			t.Fatalf("set benchtime: %v", err)
		}
		defer func() {
			if err := f.Value.Set(prev); err != nil {
				t.Errorf("restore benchtime: %v", err)
			}
		}()
	}
	res := testing.Benchmark(func(b *testing.B) {
		RunBenchmark(b, staleASTSource)
	})
	if res.N == 0 {
		// testing.Benchmark discards the failed benchmark's output, so
		// reproduce the failure here to say what actually went wrong.
		t.Fatalf("RunBenchmark failed: an iteration evaluated an AST that an earlier iteration had already mutated.\n"+
			"testing.Benchmark reports N=0 when the benchmark function calls Fatal and discards its output, so the\n"+
			"same source evaluated twice against one parsed tree was re-run here, and the second pass reported:\n\n  %s",
			sharedTreeSecondPass(t, staleASTSource))
	}
	if res.N < 2 {
		t.Fatalf("benchmark ran %d iteration(s); this test cannot observe cross-iteration sharing with fewer than 2", res.N)
	}
}

// sharedTreeSecondPass parses source once and evaluates that one tree in two
// successive environments, exactly as the unfixed RunBenchmark did, and
// returns what the second pass reported. It exists only to make the failure
// message above say something.
func sharedTreeSecondPass(tb testing.TB, source string) string {
	tb.Helper()
	p := parser.NewReader()
	exprs, err := p.Read("benchmark", strings.NewReader(source))
	if err != nil {
		return "parse error: " + err.Error()
	}
	var last string
	for pass := range 2 {
		env := lisp.NewEnv(nil)
		if err := lisp.GoError(lisp.InitializeUserEnv(env,
			lisp.WithReader(p),
			lisp.WithStderr(io.Discard),
		)); err != nil {
			return "environment: " + err.Error()
		}
		last = "no error"
		for _, expr := range exprs {
			if lerr := env.Eval(expr); lerr.Type == lisp.LError {
				last = lerr.String()
				break
			}
		}
		if pass == 1 {
			return last
		}
	}
	return last
}

// TestQuotedLiteralIsMutatedByEval is a GUARD, not a catch: it passes on main.
// It pins the premise the test above depends on -- that evaluating a tree can
// leave state in the tree -- without going through RunBenchmark. If ELPS ever
// copies quoted literals on evaluation, this guard fails first and says so,
// rather than leaving the detector above passing for a reason it did not
// intend.
func TestQuotedLiteralIsMutatedByEval(t *testing.T) {
	// concat copies, so `before` records the literal's state on entry rather
	// than aliasing the node the sort is about to reorder.
	const src = `(defun literal () '(3 1 2))
	             (set 'before (concat 'list (literal)))
	             (stable-sort < (literal))
	             before`
	p := parser.NewReader()
	exprs, err := p.Read("guard", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	eval := func() string {
		env := lisp.NewEnv(nil)
		if err := lisp.GoError(lisp.InitializeUserEnv(env,
			lisp.WithReader(p),
			lisp.WithStderr(io.Discard),
		)); err != nil {
			t.Fatal(err)
		}
		var last *lisp.LVal
		for i, expr := range exprs {
			last = env.Eval(expr)
			if last.Type == lisp.LError {
				t.Fatalf("expr %d: %v", i, last)
			}
		}
		return last.String()
	}
	first := eval()
	second := eval()
	if first == second {
		t.Skipf("evaluating a quoted literal no longer mutates it (both runs returned %s); TestRunBenchmarkGivesEachIterationAFreshAST needs a new detector", first)
	}
	t.Logf("premise holds: same tree, two runtimes, run 1 returned %s and run 2 returned %s", first, second)
}
