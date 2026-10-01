// Copyright © 2026 The ELPS authors

package main

import (
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/analysis/analysistest"
)

func TestValWalkerAnalyzer(t *testing.T) {
	// The fixture exception stays local to this test.
	valueWalkerFunctions["walkers.allowlisted"] = "oracle: fixture validates an audited exception"
	t.Cleanup(func() { delete(valueWalkerFunctions, "walkers.allowlisted") })
	analysistest.Run(t, filepath.Join(analysistest.TestData(), "valwalker"), valWalkerAnalyzer,
		"walkers", "github.com/luthersystems/elps/internal/valwalk")
}

func TestValueWalkerAllowlistReasons(t *testing.T) {
	if len(valueWalkerFunctions) != 143 {
		t.Fatalf("walker audit changed: have %d rows, want 143", len(valueWalkerFunctions))
	}
	for _, name := range []string{
		"lisp.copier.copy", "lisp.detacher.detach", "lisp.detacher.detachNode",
		"lisp.templateInventory.val", "lisp.containsCycle", "lisp.checkContainerDepth",
		"lisp.sealChildren", "lisp.stampMacroExpansion", "lisp.sealFP.walk",
	} {
		if valueWalkerFunctions["github.com/luthersystems/elps/"+name] == "" {
			t.Errorf("missing audited walker family %s", name)
		}
	}
	prefixes := []string{"syntax walker:", "hot path:", "oracle:", "dropped-per-plan:", "pending-migration:"}
	for name, reason := range valueWalkerFunctions {
		if !strings.HasPrefix(name, "github.com/luthersystems/elps/") {
			t.Errorf("unqualified walker %q", name)
		}
		classified := false
		for _, prefix := range prefixes {
			classified = classified || strings.HasPrefix(reason, prefix)
		}
		if !classified || len(strings.Fields(reason)) < 5 {
			t.Errorf("missing classification for %s: %q", name, reason)
		}
	}
	for _, name := range []string{
		"lisp/lisplib/libjson.tagWalker.value", "lisp/lisplib/libjson.untagWalker.value",
		"lisp/lisplib/libjson.canonWalker.value", "lisp/lisplib/libjson.typedEncoder.value",
		"lisp/lisplib/libelpspath.okSimpleContainerTypeGuarded", "lisp/lisplib/libelpspath.copyContainer",
	} {
		if !strings.HasPrefix(valueWalkerFunctions["github.com/luthersystems/elps/"+name], "pending-migration:") {
			t.Errorf("missing pending migration for %s", name)
		}
	}
}
