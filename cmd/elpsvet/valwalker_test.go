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
		"walkers", "github.com/luthersystems/elps/internal/walkfixture")
}

func TestValueWalkerAllowlistReasons(t *testing.T) {
	if len(valueWalkerFunctions) != 144 {
		t.Fatalf("walker audit changed: have %d rows, want 144", len(valueWalkerFunctions))
	}
	for _, name := range []string{
		"lisp.copier.copy", "lisp.detacher.detach", "lisp.detacher.detachNode",
		"lisp.CodeWalker.runtimeForm",
		"lisp.templateInventory.val", "lisp.containsCycle", "lisp.checkContainerDepth",
		"lisp.sealChildren", "lisp.stampMacroExpansion", "lisp.sealFP.walk",
	} {
		if valueWalkerFunctions["github.com/luthersystems/elps/"+name] == "" {
			t.Errorf("missing audited walker family %s", name)
		}
	}
	prefixes := []string{"syntax walker:", "hot path:", "oracle:", "specialized traversal:", "hand-rolled JSON walker:", "hand-rolled path walker:"}
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
		"lisp/lisplib/libelpspath.okSimpleContainerContents",
		"lisp/lisplib/libelpspath.okSimpleContainerTypeGuarded",
		"lisp/lisplib/libelpspath.okSimpleTypeGuarded",
		"lisp/lisplib/libelpspath.copyContainer",
		"lisp/lisplib/libelpspath.copySeqOffPath",
	} {
		if !strings.HasPrefix(valueWalkerFunctions["github.com/luthersystems/elps/"+name], "hand-rolled path walker:") {
			t.Errorf("missing path traversal contract for %s", name)
		}
	}
	for _, name := range []string{
		"tagWalker.object", "tagWalker.value",
		"canonWalker.nativeMap", "canonWalker.sortedMap", "canonWalker.value",
		"untagWalker.object", "untagWalker.tagged", "untagWalker.value",
		"typedEncoder.array", "typedEncoder.sortedMap", "typedEncoder.value",
	} {
		reason := valueWalkerFunctions["github.com/luthersystems/elps/lisp/lisplib/libjson."+name]
		if !strings.HasPrefix(reason, "hand-rolled JSON walker:") || !strings.Contains(reason, "goldens") {
			t.Errorf("walker needs its JSON traversal contract: %s: %q", name, reason)
		}
	}
}
