// Copyright © 2026 The ELPS authors

package vetpolicy

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// TestOneDefinition fails when an analyzer declares its own copy of a check
// this package holds.  Two copies drift apart: the marker and header checks
// must give the same answer in every elps analyzer.
func TestOneDefinition(t *testing.T) {
	def := regexp.MustCompile(`(?mi)^func (declaresTemplateImmutable|isLValTypeField)\(`)
	root := os.DirFS(filepath.Join("..", ".."))
	var copies []string
	err := fs.WalkDir(root, ".", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "testdata" || strings.HasPrefix(d.Name(), ".") && path != "." {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") {
			return nil
		}
		src, err := fs.ReadFile(root, path)
		if err != nil {
			return err
		}
		for _, m := range def.FindAllString(string(src), -1) {
			copies = append(copies, path+": "+m)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, c := range copies {
		if !strings.HasPrefix(c, "internal/vetpolicy/") {
			t.Errorf("%s: use vetpolicy instead of a local copy", c)
		}
	}
	if len(copies) != 2 {
		t.Errorf("found %d definitions, want the 2 in vetpolicy: %v", len(copies), copies)
	}
}
