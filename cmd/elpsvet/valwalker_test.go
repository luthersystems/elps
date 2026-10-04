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
	if len(valueWalkerFunctions) != 164 {
		t.Fatalf("walker audit changed: have %d rows, want 164", len(valueWalkerFunctions))
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
		"typedEncoder.array", "typedEncoder.mapMembers", "typedEncoder.sortedMap", "typedEncoder.value",
		"durableEncoder.body", "durableEncoder.scan", "durableEncoder.scanNative",
	} {
		reason := valueWalkerFunctions["github.com/luthersystems/elps/lisp/lisplib/libjson."+name]
		if !strings.HasPrefix(reason, "hand-rolled JSON walker:") || !strings.Contains(reason, "goldens") {
			t.Errorf("walker needs its JSON traversal contract: %s: %q", name, reason)
		}
	}
}

func TestValWalkerInElpsModule(t *testing.T) {
	for _, c := range []struct {
		module, pkg string
		want        bool
	}{
		{"github.com/luthersystems/elps", "github.com/luthersystems/elps/lisp", true},
		{"github.com/luthersystems/elps", "github.com/luthersystems/elps", true},
		{"github.com/luthersystems/elps/extensions", "github.com/luthersystems/elps/extensions/walk", false},
		{"github.com/luthersystems/elps-foo", "github.com/luthersystems/elps-foo", false},
		{"", "github.com/luthersystems/elps/internal/walkfixture", true},
		{"", "github.com/luthersystems/elps-foo/walk", false},
		{"", "walkers", false},
	} {
		if got := valWalkerInElpsModule(c.module, c.pkg); got != c.want {
			t.Errorf("valWalkerInElpsModule(%q, %q) = %v, want %v", c.module, c.pkg, got, c.want)
		}
	}
}
