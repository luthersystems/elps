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

// benchCloner is a NativeCloner held BY VALUE, so it is cloned per header
// rather than memoised by payload: the shape that makes the walk order of a
// map's values observable to a host, and so the shape the copier's ordering
// exists for.
type benchCloner struct{ seq int }

func (c benchCloner) CloneNative() interface{} { return benchCloner{seq: c.seq + 1} }

// benchSortedMap builds a sorted map of n entries in one of three shapes:
//
//	scalar -- string values, every one a leaf, so no value can reach a host
//	          hook and the copier skips the key sort entirely;
//	nested -- each value a one-element list holding a benchCloner, so the
//	          hook runs per value and the sort is live;
//	custom -- nested values behind a custom Map implementation, which puts
//	          the copy on the generic Entries arm with its sort live, the
//	          arm a host map reaches and the one whose order the second
//	          review found unspecified.
func benchSortedMap(b *testing.B, shape string, n int) *lisp.LVal {
	b.Helper()
	switch shape {
	case "scalar", "nested":
		m := lisp.SortedMap()
		for i := range n {
			v := lisp.String(fmt.Sprintf("v%05d", i))
			if shape == "nested" {
				v = lisp.QExpr([]*lisp.LVal{lisp.Native(benchCloner{})})
			}
			if rc := m.MapSet(fmt.Sprintf("k%05d", i), v); rc.Type == lisp.LError {
				b.Fatalf("set: %v", rc)
			}
		}
		return m
	case "custom":
		kv := make(map[string]*lisp.LVal, n)
		for i := range n {
			kv[fmt.Sprintf("k%05d", i)] = lisp.QExpr([]*lisp.LVal{lisp.Native(benchCloner{})})
		}
		return lisp.SortedMapFromData(lisp.NewMapData(newCopierStringMap(kv)))
	}
	b.Fatalf("unknown shape %q", shape)
	return nil
}

// BenchmarkCopySortedMap is the row the gate did not have.  The two
// benchmarks above copy sorted maps only through stable-sort's and
// insert-sorted's comparators, which stopped copying their elements in
// #604, so nothing the CI gate watches drives (*LVal).Copy over a map's
// payload at all -- which is why the key ordering's +52 %/+73 % on the map
// copy was invisible to it, and why the scan that now skips that ordering
// would have been too.
//
// The `scalar` shape is the common one and the one the leaf scan is for; the
// `nested` shape forces the sort on the stock arm; `custom` forces it on the
// generic Entries arm.
func BenchmarkCopySortedMap(b *testing.B) {
	for _, shape := range []string{"scalar", "nested", "custom"} {
		for _, n := range []int{64, 512} {
			b.Run(fmt.Sprintf("%s/%d", shape, n), func(b *testing.B) {
				m := benchSortedMap(b, shape, n)
				b.ReportAllocs()
				b.ResetTimer()
				for range b.N {
					if cp := m.Copy(); cp.Type == lisp.LError {
						b.Fatal(cp)
					}
				}
			})
		}
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
