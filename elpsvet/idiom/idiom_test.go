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
