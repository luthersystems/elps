// Copyright © 2026 The ELPS authors

package fuzzseed

import (
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
