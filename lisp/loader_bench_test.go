// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// synthLoaderSource builds a representative ELPS source of at least n bytes —
// the shape of a phylum: many function definitions over lists and
// string/number literals.  Kept to the shapes the admission walk accepts (no
// reference types at toplevel).
func synthLoaderSource(n int) string {
	var sb strings.Builder
	for i := 0; sb.Len() < n; i++ {
		fmt.Fprintf(&sb, `
(defun loader-bench-handler-%d (arg-one arg-two)
  (let ((m (list "id" arg-one "n" %d))
        (v (map 'list (lambda (x) (+ x %d)) '(1 2 3 4 5))))
    (if (equal? arg-one "case-%d")
        (list 1 2 3 %d)
        (list m v "tail-%d" 0.125))))
`, i, i, i, i, i, i)
	}
	return sb.String()
}

// BenchmarkTextLoaderLoad measures one Loader invocation into a fresh
// environment — the per-environment cost of a loaded source.  Before the
// sealed-share (#379) every invocation deep-copied the whole parse first.
func BenchmarkTextLoaderLoad(b *testing.B) {
	src := synthLoaderSource(50 * 1024)
	loader, err := lisp.TextLoader(parser.NewReader(), "bench.lisp", strings.NewReader(src))
	if err != nil {
		b.Fatalf("TextLoader: %v", err)
	}
	envs := make([]*lisp.LEnv, b.N)
	for i := range envs {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = parser.NewReader()
		if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
			b.Fatalf("InitializeUserEnv: %v", lerr)
		}
		envs[i] = env
	}
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	b.ResetTimer()
	for i := range b.N {
		if lerr := loader(envs[i]); lerr.Type == lisp.LError {
			b.Fatalf("loader: %v", lerr)
		}
	}
}

// BenchmarkReadProgramAdmit and BenchmarkTextLoaderAdmit measure ADMISSION —
// the parse plus the walk that decides whether reader output may become a
// Program or a Loader — with NO cache installed.
//
// They exist because CI could not see a regression on this path.
// BenchmarkTextLoaderLoad above measures a Loader INVOCATION, not the
// TextLoader construction that admits the parse, so a per-parse cost added to
// admission was invisible to the benchmark gate: the load-cache hook's own
// admission state made these 21% slower and 27% heavier per parse and no
// benchmark moved (issue #536 round-three review, blocker 1).  docs/embed.md
// promises that a nil LoadCache leaves the load path exactly what it was
// before the hook existed; these two are what hold that promise to account,
// because they are the public constructors on it.
func BenchmarkReadProgramAdmit(b *testing.B) {
	src := synthLoaderSource(50 * 1024)
	rd := parser.NewReader()
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		p, err := lisp.ReadProgram(rd, "bench.lisp", strings.NewReader(src))
		if err != nil {
			b.Fatalf("ReadProgram: %v", err)
		}
		if p.Len() == 0 {
			b.Fatal("empty program")
		}
	}
}

func BenchmarkTextLoaderAdmit(b *testing.B) {
	src := synthLoaderSource(50 * 1024)
	rd := parser.NewReader()
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		loader, err := lisp.TextLoader(rd, "bench.lisp", strings.NewReader(src))
		if err != nil {
			b.Fatalf("TextLoader: %v", err)
		}
		if loader == nil {
			b.Fatal("nil loader")
		}
	}
}
