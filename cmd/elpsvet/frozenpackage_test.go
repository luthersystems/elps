// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestFrozenPackageAnalyzer(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "frozenpackage"), frozenPackageAnalyzer, lispPkgPath)
}

func TestPackageWriteAllowlist(t *testing.T) {
	want := []string{
		"Package.putName", "Package.setSymbolDoc", "Package.Export", "Package.Exports", "Package.exportSorted", "Package.thaw", "Package.putSlot",
		"NewPackage", "admitPackage", "templatePlan.instantiateEager", "templatePlan.instantiateLazy", "Package.fillSymbol", "Package.fillBaseValue", "templateCompiler.packageDescriptor", "packageBase.publish",
	}
	if len(packageWriteFunctions) != len(want) {
		t.Fatalf("package write allowlist changed: %v", packageWriteFunctions)
	}
	for _, name := range want {
		if len(strings.Fields(packageWriteFunctions[name])) < 3 {
			t.Errorf("missing audited justification for %s", name)
		}
	}
}
