// Copyright © 2026 The ELPS authors

package fuzzseed

import (
	"strings"
	"testing"
)

// TestLispSourcesFound guards the repoRoot() path derivation.  Every fuzz
// target seeds from LispSources; if the path ever silently resolves to
// nothing, the targets keep passing while fuzzing only random bytes, which is
// close to useless against a grammar.  This is the one place that failure is
// visible, so it must be an assertion and not a skip.
func TestLispSourcesFound(t *testing.T) {
	sources := LispSources()
	if len(sources) == 0 {
		t.Fatal("no repository .lisp seeds found; repoRoot() or lispDirs is wrong")
	}
	// The sicp fixtures the parser benchmarks use are the corpus's backbone.
	// Six files live there today; require most of them so a rename is noticed
	// without making the test brittle about the exact count.
	if len(sources) < 10 {
		t.Errorf("only %d .lisp seeds found; expected the _examples and lisplib trees", len(sources))
	}
	for i, src := range sources {
		if len(src) == 0 {
			t.Errorf("seed %d is empty", i)
		}
	}
}

func TestAdversarialSeedsAreDistinct(t *testing.T) {
	seen := make(map[string]int, len(Adversarial()))
	for i, src := range Adversarial() {
		if prev, ok := seen[string(src)]; ok {
			t.Errorf("adversarial seed %d duplicates seed %d", i, prev)
		}
		seen[string(src)] = i
	}
}

func TestAllIncludesBothSources(t *testing.T) {
	if got, want := len(All()), len(Adversarial())+len(LispSources()); got != want {
		t.Errorf("All() returned %d seeds, want %d", got, want)
	}
}

// TestEvalCorporaAreDisjoint guards the property the eval split rests on: a
// program cannot both be required to run to completion and required to be
// stopped by a budget.  The two corpora are asserted in opposite directions by
// the harness, so an entry appearing in both makes one of those tests fail for
// a reason that has nothing to do with the interpreter.
func TestEvalCorporaAreDisjoint(t *testing.T) {
	runaway := EvalRunaway()
	terminating := EvalTerminating()

	bySrc := make(map[string]string, len(runaway))
	for name, src := range runaway {
		bySrc[src] = "EvalRunaway/" + name
	}
	for name, src := range terminating {
		if prev, ok := bySrc[src]; ok {
			t.Errorf("EvalTerminating/%s has the same source as %s;"+
				" a program cannot be required both to complete and to be stopped", name, prev)
		}
	}
}

// TestEvalSeedsAreNonEmpty catches the silent-corpus failure mode: an entry
// whose source is blank or whitespace evaluates to nil, passes every
// assertion, and tests nothing.  The one legitimate exception is the
// deliberately-empty boundary seed in the terminating corpus.
func TestEvalSeedsAreNonEmpty(t *testing.T) {
	for name, src := range EvalRunaway() {
		if strings.TrimSpace(src) == "" {
			t.Errorf("EvalRunaway/%s is blank; a blank program cannot run away", name)
		}
	}
	for _, src := range EvalAdversarial() {
		if src == "" {
			t.Error("EvalAdversarial contains an empty seed")
		}
	}
}

func TestEvalAdversarialSeedsAreDistinct(t *testing.T) {
	seen := make(map[string]int, len(EvalAdversarial()))
	for i, src := range EvalAdversarial() {
		if prev, ok := seen[src]; ok {
			t.Errorf("eval adversarial seed %d (%q) duplicates seed %d", i, src, prev)
		}
		seen[src] = i
	}
}
