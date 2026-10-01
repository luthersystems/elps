// Copyright © 2026 The ELPS authors

package main

import (
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestMarkerFieldsAnalyzer(t *testing.T) {
	analysistest.Run(t, analysistest.TestData(), markerFieldsAnalyzer, "github.com/luthersystems/elps/markerfields")
}

func TestJustifiedMarkerAllow(t *testing.T) {
	for text, want := range map[string]bool{
		"//elpsvet:allow-marker pointee is frozen":  true,
		"//elpsvet:allow-marker too short":          false,
		"//elpsvet:allow-marker":                    false,
		"//elpsvet:allow-markerx pointee is frozen": false,
		"//elpsvet:allow-native pointee is frozen":  false,
		"//elpsvet:allow pointee is frozen":         false,
	} {
		if got := justifiedMarkerAllow(text); got != want {
			t.Errorf("justifiedMarkerAllow(%q) = %v, want %v", text, got, want)
		}
	}
}
