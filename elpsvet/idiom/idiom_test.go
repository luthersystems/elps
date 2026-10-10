// Copyright © 2026 The ELPS authors

package idiom_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsvet/idiom"
	"golang.org/x/tools/go/analysis/analysistest"
)

func TestAnalyzer(t *testing.T) {
	analysistest.RunWithSuggestedFixes(t, analysistest.TestData(), idiom.Analyzer, "idiomcase")
}

// TestFixOnly runs the analyzer with -fixonly, as make elpsvet does: only
// the diagnostics that carry a fix are reported.
func TestFixOnly(t *testing.T) {
	if err := idiom.Analyzer.Flags.Set("fixonly", "true"); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = idiom.Analyzer.Flags.Set("fixonly", "false") })
	analysistest.RunWithSuggestedFixes(t, analysistest.TestData(), idiom.Analyzer, "idiomfixonly")
}
