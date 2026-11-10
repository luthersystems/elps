// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// lazyTableTemplate publishes a program of n definitions: for each index a
// function and a map it reads, the shape of a large phylum.
func lazyTableTemplate(tb testing.TB, n int) *lisp.Template {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatal(rc)
	}
	var src strings.Builder
	for i := range n {
		fmt.Fprintf(&src, "(set 'data%d (sorted-map \"key\" (list %d 2 1) \"name\" \"abcdefghijklmnop\"))\n", i, i)
		fmt.Fprintf(&src, "(defun fn%d (x) (+ x %d (length (get data%d \"key\"))))\n", i, i, i)
	}
	if rc := env.LoadString("definitions.lisp", src.String()); rc.Type == lisp.LError {
		tb.Fatal(rc)
	}
	tmpl, err := lisp.NewTemplate(env, lisp.TemplateWithBuiltinPolicy(func(v *lisp.LVal) bool { return v.Builtin() != nil }))
	if err != nil {
		tb.Fatal(err)
	}
	return tmpl
}

// BenchmarkLazyVMTables measures what a lazy NewVM costs for a plan of 2000
// and of 8000 definitions, and what the VM costs when it then calls 1%, 10%
// or all of the functions (luthersystems/elps#944).  A host that forks one
// VM for each transaction pays the first number every time and reads only a
// small part of the plan.
//
// A touch calls every k-th function, spread over the whole plan.  That is
// the worst case for tables that are allocated in pages: the calls land in
// as many pages as they can.  The clustered runs call one block of
// neighbours instead, as one endpoint and its helpers do.
func BenchmarkLazyVMTables(b *testing.B) {
	for _, n := range []int{2000, 8000} {
		tmpl := lazyTableTemplate(b, n)
		touch := func(b *testing.B, calls []string) {
			b.Helper()
			b.ReportAllocs()
			b.ResetTimer()
			for range b.N {
				vm, err := tmpl.NewVM()
				if err != nil {
					b.Fatal(err)
				}
				for _, call := range calls {
					if rc := vm.FunCall(vm.Get(lisp.Symbol(call)), lisp.QExpr([]*lisp.LVal{lisp.Int(1)})); rc.Type == lisp.LError {
						b.Fatal(rc)
					}
				}
			}
		}
		spread := func(every int) []string {
			var calls []string
			for i := 0; i < n; i += every {
				calls = append(calls, fmt.Sprintf("fn%d", i))
			}
			return calls
		}
		cluster := func(count int) []string {
			calls := make([]string, count)
			for i := range calls {
				calls[i] = fmt.Sprintf("fn%d", n/2+i)
			}
			return calls
		}
		b.Run(fmt.Sprintf("defs=%d/touch=0", n), func(b *testing.B) { touch(b, nil) })
		b.Run(fmt.Sprintf("defs=%d/touch=1pct-spread", n), func(b *testing.B) { touch(b, spread(100)) })
		b.Run(fmt.Sprintf("defs=%d/touch=1pct-cluster", n), func(b *testing.B) { touch(b, cluster(n/100)) })
		b.Run(fmt.Sprintf("defs=%d/touch=10pct-spread", n), func(b *testing.B) { touch(b, spread(10)) })
		b.Run(fmt.Sprintf("defs=%d/touch=10pct-cluster", n), func(b *testing.B) { touch(b, cluster(n/10)) })
		b.Run(fmt.Sprintf("defs=%d/touch=100pct", n), func(b *testing.B) { touch(b, spread(1)) })
	}
}
