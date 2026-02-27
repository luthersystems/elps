// Copyright © 2018 The ELPS authors

package rdparser_test

import (
	"bytes"
	"os"
	"path/filepath"
	"sort"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

const fixtureDir = "../../_examples/sicp"

func BenchmarkParser(b *testing.B) {
	files, err := filepath.Glob(filepath.Join(fixtureDir, "*.lisp"))
	if err != nil {
		b.Fatalf("Failed to list test fixtures: %v", err)
	}
	sort.Strings(files) // should be redundant
	for _, path := range files {
		b.Run(filepath.Base(path), elpstest.BenchmarkParse(path, rdparser.NewReader))
	}
}

func BenchmarkParserFaultTolerant(b *testing.B) {
	files, err := filepath.Glob(filepath.Join(fixtureDir, "*.lisp"))
	if err != nil {
		b.Fatalf("Failed to list test fixtures: %v", err)
	}
	sort.Strings(files)
	for _, path := range files {
		b.Run(filepath.Base(path), func(b *testing.B) {
			buf, err := os.ReadFile(path) //#nosec G304
			if err != nil {
				b.Fatalf("Unable to read source file %v: %v", path, err)
			}
			b.SetBytes(int64(len(buf)))
			for i := 0; i < b.N; i++ {
				s := token.NewScanner("test", bytes.NewReader(buf))
				p := rdparser.New(s)
				result := p.ParseProgramFaultTolerant()
				if len(result.Errors) > 0 {
					b.Fatalf("Parse failure: %v", result.Errors[0])
				}
			}
		})
	}
}
