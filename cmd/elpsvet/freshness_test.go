// Copyright © 2026 The ELPS authors

package main

import (
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestFreshnessAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), freshnessAnalyzer, "a")
}
