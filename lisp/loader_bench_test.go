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
// string/number literals.  Kept to the shapes checkLoaderExpr accepts (no
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
