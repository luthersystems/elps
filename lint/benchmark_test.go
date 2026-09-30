// Copyright © 2026 The ELPS authors

package lint

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/internal/analysisbench"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// BenchmarkLintCorpus includes parsing, resolution and all DefaultAnalyzers.
func BenchmarkLintCorpus(b *testing.B) {
	source := analysisbench.Source()
	linter := &Linter{Analyzers: DefaultAnalyzers()}
	b.ReportAllocs()
	b.SetBytes(int64(len(source)))
	for b.Loop() {
		if _, err := linter.LintFileWithAnalysis(source, "corpus.lisp", nil); err != nil {
			b.Fatal(err)
		}
	}
}

// BenchmarkLintAnalyzers isolates analyzer costs from parsing and resolution.
func BenchmarkLintAnalyzers(b *testing.B) {
	source := analysisbench.Source()
	exprs, err := rdparser.New(token.NewScanner("corpus.lisp", bytes.NewReader(source))).ParseProgram()
	if err != nil {
		b.Fatal(err)
	}
	sem := analysis.Analyze(exprs, nil)
	for _, analyzer := range DefaultAnalyzers() {
		b.Run(analyzer.Name, func(b *testing.B) {
			b.ReportAllocs()
			b.SetBytes(int64(len(source)))
			for b.Loop() {
				pass := &Pass{Analyzer: analyzer, Filename: "corpus.lisp", Exprs: exprs, Semantics: sem}
				if err := analyzer.Run(pass); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
