// Copyright © 2026 The ELPS authors

// Package analysisbench supplies a fixed source corpus shared by the analysis
// and lint benchmarks. It is only imported by tests.
package analysisbench

import (
	"bytes"
	_ "embed"
)

// corpus.txt concatenates unrelated sample files (including test files) into
// one input. It is benchmark data, not a program, so it is not named .lisp:
// the repository-wide elps fmt/lint gates would otherwise lint it as one.
//
//go:embed corpus.txt
var corpus []byte

// Source repeats the checked-in sample to approximately one MiB. Both arms of
// a comparison must use this same fixture, rather than their own source trees.
func Source() []byte {
	return bytes.Repeat(corpus, (1<<20)/len(corpus)+1)
}
