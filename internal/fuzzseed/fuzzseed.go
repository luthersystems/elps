// Copyright © 2026 The ELPS authors

// Package fuzzseed supplies the shared seed corpus for the repository's
// native go fuzz targets (parser, lexer, formatter, minifier, json).
//
// Seeds come from two places:
//
//   - Real ELPS source already committed to the repository -- the _examples
//     tree (including the sicp files the parser benchmarks use) and the
//     lisplib .lisp sources.  Grammar-valid seeds are what let a
//     coverage-guided fuzzer reach the interesting parser states at all;
//     starting from random bytes the fuzzer spends its whole budget failing
//     at the first token.
//
//   - Hand-written adversarial input (Adversarial) aimed at the boundaries a
//     recursive-descent parser and a byte scanner actually break on: nesting
//     depth, unterminated literals, numeric overflow, invalid UTF-8, NUL
//     bytes, and tokens larger than the scanner buffer.
//
// The package is only imported from _test files.  It is a normal package
// rather than a _test file so that every fuzz target in every package can
// share one corpus instead of copying the fixtures into each testdata tree.
package fuzzseed

import (
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"sync"
)

// scannerBufSize mirrors the fixed buffer token.NewScanner allocates.  Input
// built around this size exercises the scanner's buffer-extension path, where
// a token that never fits cannot be scanned and the lexer has to report an
// error instead of spinning.
const scannerBufSize = 128 << 10

// repoRoot returns the root of the elps source tree.  It is derived from this
// file's own compile-time path so that it resolves identically no matter which
// package's test binary is calling (test binaries run with their own package
// directory as the working directory).
func repoRoot() string {
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		return ""
	}
	// <root>/internal/fuzzseed/fuzzseed.go
	return filepath.Dir(filepath.Dir(filepath.Dir(file)))
}

// lispDirs are the repository directories walked for real-source seeds.
var lispDirs = []string{
	"_examples",
	filepath.Join("lisp", "lisplib"),
}

var (
	lispOnce    sync.Once
	lispSources [][]byte
)

// LispSources returns the contents of every .lisp file in the repository
// directories listed in lispDirs, sorted by path so the corpus is stable.
// It returns nil if the source tree cannot be located, which the callers
// assert against in a plain (non-fuzz) test.
func LispSources() [][]byte {
	lispOnce.Do(func() {
		root := repoRoot()
		if root == "" {
			return
		}
		var paths []string
		for _, dir := range lispDirs {
			//nolint:errcheck // the walk func never returns an error
			_ = filepath.WalkDir(filepath.Join(root, dir), func(path string, d fs.DirEntry, err error) error {
				if err != nil {
					// A missing or unreadable directory is not fatal here:
					// the callers' assertion on the total seed count is the
					// real guard against a broken path.
					return nil //nolint:nilerr // best-effort walk
				}
				if d.IsDir() || !strings.HasSuffix(path, ".lisp") {
					return nil
				}
				paths = append(paths, path)
				return nil
			})
		}
		// WalkDir already yields lexical order per root; sorting the combined
		// list keeps the corpus byte-identical across roots and platforms.
		slices.Sort(paths)
		for _, path := range paths {
			b, err := os.ReadFile(path) //nolint:gosec // G304: fixed in-repo fixture paths
			if err != nil {
				continue
			}
			lispSources = append(lispSources, b)
		}
	})
	return lispSources
}

// Adversarial returns hand-written inputs targeting the failure modes a lisp
// scanner and recursive-descent parser are actually vulnerable to.  Each entry
// is annotated with what it is trying to break.
//
// Every entry is deliberately small.  Seed size is the dominant cost in a
// coverage-guided run: the mutator descends from the seeds it is given, so one
// 128KiB seed drags every generation after it down with it (measured: whole
// seconds at 0 exec/sec).  The genuinely oversized regression inputs live in
// Pathological instead, where ordinary unit tests run them once each.
func Adversarial() [][]byte {
	src := []string{
		// --- trivial / boundary ---
		"",
		" ",
		"\n\n\n",
		"()",
		"[]",
		"'()",

		// --- nesting.  Unbounded recursion in ParseExpression would overflow
		// the goroutine stack, which is a FATAL crash recover() cannot
		// intercept.  These are small enough to fuzz cheaply; Pathological
		// carries the inputs that cross DefaultMaxParseDepth. ---
		strings.Repeat("(", 100),
		strings.Repeat("(", 100) + strings.Repeat(")", 100),
		strings.Repeat("[", 100),
		strings.Repeat("'", 100) + "a",
		strings.Repeat("#^", 50) + "a",

		// --- unbalanced / mismatched brackets ---
		"(",
		")",
		"(]",
		"[)",
		"((((",
		"))))",
		"(a (b (c",
		"(a) ) (b)",

		// --- unterminated and malformed literals ---
		`"`,
		`"abc`,
		`"abc` + "\n" + `def"`,
		`"\`,
		`"\q"`,
		`"""`,
		`"""abc`,
		`""""""`,
		`"""""`,
		`""`,

		// --- numeric edge cases: overflow, radix macros, bad exponents ---
		"9223372036854775807",
		"9223372036854775808",
		"-9223372036854775809",
		strings.Repeat("9", 64),
		"1e",
		"1e+",
		"1.",
		"1.2.3",
		"0x",
		"0xFFFFFFFFFFFFFFFFFF",
		"#x",
		"#xG",
		"#xFFFFFFFFFFFFFFFFFF",
		"#o",
		"#o9",
		"#o777777777777777777777777",
		"#",
		"#z",
		"#' ",
		"#^",
		"#^(a (b))",
		"-",
		"- ",
		"-a",
		"--",

		// --- symbols: qualified, over-qualified, empty package ---
		"a:b",
		"a:b:c",
		"a:",
		":a",
		"::",
		strings.Repeat("a", 256),

		// --- comments and hash-bang ---
		";",
		";comment with no newline",
		"#!",
		"#!/usr/bin/env elps",
		"#!\n(a)",
		"(a ; comment\n)",
		"(a ;",

		// --- NUL bytes and control characters ---
		"\x00",
		"(\x00)",
		"a\x00b",
		`"a\x00b"`,
		"\x00\x00\x00",
		"(a)\x00(b)",

		// --- invalid and truncated UTF-8.  "abc\xff" is the minimised input
		// for the ParseProgram hang fixed alongside these targets: the lexer
		// re-emitted an empty SYMBOL token forever because it could neither
		// decode the byte nor recognise EOF. ---
		"\xff",
		"abc\xff",
		"(\xff",
		"'\xff",
		"-\xff",
		"\xe4\xb8",
		"\"a\xff\"",
		";\xff",
		"#\xff",
		"(a\xffb)",
		"\xed\xa0\x80", // UTF-16 surrogate half, invalid in UTF-8
		"\xf4\x90\x80\x80",

		// --- realistic-shaped programs that still stress the AST walkers ---
		"(defun f (x) (+ x 1))",
		"(defun f (x &optional y &rest z) (list x y z))",
		"(let ([a 1] [b 2]) (+ a b))",
		"(handler-bind ((condition (lambda (c) c))) (error 'x \"y\"))",
		"(in-package 'foo)\n(export 'bar)\n(defun bar () ())",
		"(defmacro m (x) (quasiquote (list (unquote x))))",
		"#^(+ % 1)",
		"'(1 2 3)",
		"[1 2 3]",
		"(a . b)",
	}
	out := make([][]byte, 0, len(src))
	for _, s := range src {
		out = append(out, []byte(s))
	}
	return out
}

// Pathological returns inputs that are too large to be useful fuzz seeds but
// that must keep working: nesting beyond DefaultMaxParseDepth (10000), a
// literal larger than the scanner's fixed 128KiB buffer, and a numeric literal
// far past what an int can hold.  These are the shapes that turn a recursive
// parser into a fatal stack overflow or a scanner into a spin, so they are run
// once each by ordinary unit tests rather than being handed to the mutator.
func Pathological() map[string][]byte {
	return map[string][]byte{
		"nesting-beyond-max-depth":   []byte(strings.Repeat("(", 10001)),
		"balanced-just-under-max":    []byte(strings.Repeat("(", 9999) + strings.Repeat(")", 9999)),
		"brace-nesting-beyond-max":   []byte(strings.Repeat("[", 10001)),
		"quote-nesting-beyond-max":   []byte(strings.Repeat("'", 10001) + "a"),
		"unbound-nesting-beyond-max": []byte(strings.Repeat("#^", 5001) + "a"),
		"symbol-exceeds-scanner-buf": []byte(strings.Repeat("a", scannerBufSize+1)),
		"comment-exceeds-scanner-buf": []byte(";" +
			strings.Repeat("a", scannerBufSize+1)),
		"string-exceeds-scanner-buf": []byte(`"` +
			strings.Repeat("a", scannerBufSize+1) + `"`),
		"int-literal-4k-digits": []byte(strings.Repeat("9", 4096)),
	}
}

// All returns the complete seed corpus: adversarial inputs first (they are
// small and cheap, so they run first as regression cases under a plain
// `go test`), followed by the repository's real .lisp sources.
func All() [][]byte {
	seeds := Adversarial()
	return append(seeds, LispSources()...)
}
