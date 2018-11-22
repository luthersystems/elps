// Copyright © 2018 The ELPS authors

package regexparser_test

import (
	"path/filepath"
	"sort"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/parser/regexparser"
)

const fixtureDir = "../../_examples/sicp"

func BenchmarkParser(b *testing.B) {
	files, err := filepath.Glob(filepath.Join(fixtureDir, "*.lisp"))
	if err != nil {
		b.Fatalf("Failed to list test fixtures: %v", err)
	}
	sort.Strings(files) // should be redundant
	for _, path := range files {
		b.Run(filepath.Base(path), elpstest.BenchmarkParse(path, regexparser.NewReader))
	}
}
