// Copyright © 2026 The ELPS authors

package builtinstate_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsvet/builtinstate"
	"golang.org/x/tools/go/analysis/analysistest"
)

func TestAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), builtinstate.Analyzer, "builtinstate")
}

func TestJustifiedSharedAllow(t *testing.T) {
	for text, want := range map[string]bool{
		"//elpsvet:allow-shared guarded by the mutex":  true,
		"//elpsvet:allow-shared too short":             false,
		"//elpsvet:allow-shared":                       false,
		"//elpsvet:allow-sharedx guarded by the mutex": false,
		"//elpsvet:allow guarded by the mutex":         false,
		"//elpsvet:allow-native guarded by the mutex":  false,
	} {
		if got := builtinstate.JustifiedSharedAllow(text); got != want {
			t.Errorf("JustifiedSharedAllow(%q) = %v, want %v", text, got, want)
		}
	}
}
