// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestLazyReadAnalyzer(t *testing.T) {
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "lazyread"), lazyReadAnalyzer, lispPkgPath)
}

// TestLazyTableAllowlist pins the audit: adding a direct reader of a lazily
// filled table is a two-file change a reviewer sees, and every row carries a
// reason long enough to read.
func TestLazyTableAllowlist(t *testing.T) {
	want := []string{
		"Package.baseValue", "Package.symbol", "Package.materializeSymbols",
		"sortedmap.entry", "sortedmap.forceAll", "sortedmap.discardPending",
		"Package.symbolTable", "Package.thaw", "sortedmap.Entries", "sortedmap.copyInto",
		"LVal.AppendSortedPairs", "copier.mapData", "templateInventory.mapData",
		"templateCompiler.mapData", "templateCompiler.packageDescriptor", "admitPackage",
		"sortedmap.Set", "sortedmap.Del", "sortedmap.Len", "sortedmap.Keys", "sortedmap.emptyLike",
		"Package.SymbolNames", "Package.putName", "Package.putSlot", "LVal.copyMapData",
		"templatePlan.instantiateEager", "templatePlan.instantiateLazy", "lazyInstance.fillBacking",
	}
	if len(lazyTableFunctions) != len(want) {
		t.Fatalf("lazy table allowlist changed: have %d rows, want %d", len(lazyTableFunctions), len(want))
	}
	for _, name := range want {
		if len(strings.Fields(lazyTableFunctions[name])) < 5 {
			t.Errorf("missing audited justification for %s", name)
		}
	}
}
