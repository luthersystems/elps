// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func templatePlanBenchmarkFixture(tb testing.TB, functions int) *lisp.LEnv {
	tb.Helper()
	var src strings.Builder
	src.WriteString(`(set 'xs (list 3 1 2)) (set 'tail (cdr xs))`)
	for n := range functions {
		if n%2 == 0 {
			fmt.Fprintf(&src, `(set 'data%d (sorted-map "key" (list %d 2 1) "bytes" (to-bytes "abcdefghijklmnop")))`, n, n)
		} else {
			fmt.Fprintf(&src, `(set 'data%d (json:load-string "{\"key\":[%d,2,1],\"text\":\"abcdefghijklmnop\"}" :exact-integers true))`, n, n)
		}
		fmt.Fprintf(&src, `(defun fn%d (x) (+ x %d (length (get data%d "key"))))`, n, n, n)
	}
	return loadTemplateFixture(tb, src.String())
}

// BenchmarkTemplatePlan separates publication from per-VM instantiation.
// Checks outside timing reject no-op constructors and broken backing aliases.
func BenchmarkTemplatePlan(b *testing.B) {
	for _, functions := range []int{0, 250} {
		b.Run(fmt.Sprintf("functions=%d", functions), func(b *testing.B) {
			env := templatePlanBenchmarkFixture(b, functions)
			compiled, err := lisp.NewTemplate(env, templateFixturePolicy())
			if err != nil {
				b.Fatal(err)
			}
			check := func(b *testing.B, env *lisp.LEnv) {
				b.Helper()
				assertTemplateValue(b, env, `(stable-sort < xs) (first tail)`, lisp.Int(2))
				if functions > 0 {
					assertTemplateValue(b, env, fmt.Sprintf("(fn%d 10)", functions-1), lisp.Int(10+functions-1+3))
				}
			}
			b.Run("phase=publish", func(b *testing.B) {
				var result *lisp.Template
				b.ReportAllocs()
				b.ResetTimer()
				for range b.N {
					result, err = lisp.NewTemplate(env, templateFixturePolicy())
					if err != nil {
						b.Fatal(err)
					}
				}
				b.StopTimer()
				vm, err := result.NewVM()
				if err != nil {
					b.Fatal(err)
				}
				check(b, vm)
			})
			b.Run("phase=fork", func(b *testing.B) {
				var result *lisp.LEnv
				b.ReportAllocs()
				b.ResetTimer()
				for range b.N {
					result, err = compiled.NewVM()
					if err != nil {
						b.Fatal(err)
					}
				}
				b.StopTimer()
				check(b, result)
			})
		})
	}
}
