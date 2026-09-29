// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestCallerPackageAnalyzer(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "callerpackage"), callerPackageAnalyzer, "callerpackage")
}

func TestJustifiedCallerPkgAllow(t *testing.T) {
	for text, want := range map[string]bool{
		"//elpsvet:allow-callerpkg intentionally shared lisp namespace": true,
		"//elpsvet:allow-callerpkg too short":                           false,
		"//elpsvet:allow-callerpkg":                                     false,
		"//elpsvet:allow-callerpkgx guarded elsewhere for good reason":  false,
		"//elpsvet:allow guarded elsewhere for a good reason":           false,
		"//elpsvet:allow-shared guarded elsewhere for a good reason":    false,
		"//elpsvet:allow-native guarded elsewhere for a good reason":    false,
	} {
		if got := justifiedCallerPkgAllow(text); got != want {
			t.Errorf("justifiedCallerPkgAllow(%q) = %v, want %v", text, got, want)
		}
	}
}

func TestIsBuiltinSignatureRejectsNilAndVariadic(t *testing.T) {
	if isBuiltinSignature(nil) {
		t.Error("isBuiltinSignature(nil) = true, want false")
	}
}
