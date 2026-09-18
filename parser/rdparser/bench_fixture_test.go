// Copyright © 2026 The ELPS authors

package rdparser_test

import (
	"crypto/sha256"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// Issue #657: compare the same parser workload across revisions, even when
// runnable examples gain correctness fixes or additional assertions.
func TestParserBenchmarkFixtures(t *testing.T) {
	const sourceCommit = "6d267f827c727072afd2b7d7cca4786e5755b150"
	if fixtureDir != "testdata/bench/sicp" {
		t.Errorf("parser benchmarks must use the frozen corpus, got %q", fixtureDir)
	}
	want := map[string]struct {
		bytes  int
		sha256 string
	}{
		"approx.lisp":      {1745, "b63cffdf28b5a30e64c12167e7aa4986bdfcf79b8cdd61468dcce055333fd3dd"},
		"complex.lisp":     {5318, "52d3971e7cabbb8fad9e800b7a1a78b461f62cc794a06a02949f9ab7217df44e"},
		"diff.lisp":        {1774, "760a20200cca9df3ab4ed5845612bdd4ad4cda53f9a1b51957efb6ffa6166d92"},
		"scheme-math.lisp": {12076, "b449bb077e92a3ae1aa41c825c099309411070ac51ed96d7c4cb46c960ffe9c4"},
		"sicp.lisp":        {7235, "efb243d625b331e3d3af428f6070a88c00d1f5a03aab513afc1f071572c896c9"},
		"stream.lisp":      {7116, "a6ab578d7f1d97dcaf41900797f1b268b1e5cfb7c1d8f743c6e97cb4a308e4d2"},
	}
	files, err := filepath.Glob(filepath.Join(fixtureDir, "*.lisp"))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) != len(want) {
		t.Errorf("parser benchmark corpus has %d inputs, want %d", len(files), len(want))
	}
	for _, path := range files {
		name := filepath.Base(path)
		expected, ok := want[name]
		if !ok {
			t.Errorf("unexpected parser benchmark input %q; update the reviewed corpus and provenance together", name)
			continue
		}
		data, err := os.ReadFile(path) //#nosec G304 -- fixed benchmark corpus paths
		if err != nil {
			t.Error(err)
			continue
		}
		digest := fmt.Sprintf("%x", sha256.Sum256(data))
		if len(data) != expected.bytes || digest != expected.sha256 {
			t.Errorf("parser benchmark input %s drifted from %s: %d bytes, sha256 %s", name, sourceCommit, len(data), digest)
		}
	}
	readme, err := os.ReadFile("testdata/bench/sicp/README.md")
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(readme), sourceCommit) {
		t.Error("benchmark provenance must name the pinned source commit")
	}
	for name, expected := range want {
		row := fmt.Sprintf("| %s | %d | %s |", name, expected.bytes, expected.sha256)
		if !strings.Contains(string(readme), row) {
			t.Errorf("benchmark provenance is missing the pinned checksum for %s", name)
		}
	}
}
