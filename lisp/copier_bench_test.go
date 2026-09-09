// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The Copy-path benchmarks for the callers the copier changes the cost of:
// stable-sort's comparator and insert-sorted's probe copy the ELEMENTS they
// compare on every comparison (lisp/builtins.go, lvalByFun.Less and
// builtinInsertSorted), so a list of sorted maps pays per map per
// comparison -- before the copier a structural clone sharing the values,
// after it a walk of the values as well.  Scalar values are one header
// each way; these benchmarks use maps of scalars, the common shape.

func benchEnv(b *testing.B) *lisp.LEnv {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		b.Fatalf("init: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		b.Fatalf("in-package: %v", rc)
	}
	return env
}

// benchMapList binds `maps` to n sorted maps of eight scalar entries each,
// keyed for sorting by "k", in reverse order so a sort moves every element.
func benchMapList(b *testing.B, env *lisp.LEnv, n int) {
	b.Helper()
	var sb strings.Builder
	sb.WriteString("(set 'maps (list")
	for i := n; i > 0; i-- {
		fmt.Fprintf(&sb, ` (sorted-map "k" %d "a" 1 "b" 2 "c" 3 "d" 4 "e" 5 "f" 6 "g" "s")`, i)
	}
	sb.WriteString("))")
	if rc := env.LoadString("bench.lisp", sb.String()); rc.Type == lisp.LError {
		b.Fatalf("setup: %v", rc)
	}
}

func BenchmarkStableSortMaps(b *testing.B) {
	env := benchEnv(b)
	benchMapList(b, env, 64)
	prog := `(stable-sort < maps (lambda (m) (get m "k")))`
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if rc := env.LoadString("sort.lisp", prog); rc.Type == lisp.LError {
			b.Fatal(rc)
		}
		b.StopTimer()
		benchMapList(b, env, 64)
		b.StartTimer()
	}
}

func BenchmarkInsertSortedMaps(b *testing.B) {
	env := benchEnv(b)
	benchMapList(b, env, 64)
	if rc := env.LoadString("presort.lisp", `(stable-sort < maps (lambda (m) (get m "k")))`); rc.Type == lisp.LError {
		b.Fatal(rc)
	}
	prog := `(insert-sorted 'list maps < (sorted-map "k" 33 "a" 1 "b" 2 "c" 3 "d" 4 "e" 5 "f" 6 "g" "s") (lambda (m) (get m "k")))`
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if rc := env.LoadString("insert.lisp", prog); rc.Type == lisp.LError {
			b.Fatal(rc)
		}
	}
}
