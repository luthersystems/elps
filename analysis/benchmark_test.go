// Copyright © 2026 The ELPS authors

package analysis

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/internal/analysisbench"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

func BenchmarkAnalyze(b *testing.B) {
	source := analysisbench.Source()
	exprs, err := rdparser.New(token.NewScanner("corpus.lisp", bytes.NewReader(source))).ParseProgram()
	if err != nil {
		b.Fatal(err)
	}
	b.ReportAllocs()
	b.SetBytes(int64(len(source)))
	for b.Loop() {
		Analyze(exprs, &Config{Filename: "corpus.lisp"})
	}
}
