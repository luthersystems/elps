// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"fmt"
	"runtime"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkTemplatePackageCount distinguishes shell costs from binding costs.
// The extra packages are unused, as when an embedder adds a stdlib package.
func BenchmarkTemplatePackageCount(b *testing.B) {
	for _, kind := range []string{"empty", "shared", "pending"} {
		for _, extra := range []int{0, 1, 8} {
			b.Run(fmt.Sprintf("kind=%s/extra=%d", kind, extra), func(b *testing.B) {
				env := templatePlanBenchmarkFixture(b, 0)
				for n := range extra {
					pkg := env.Runtime.Registry.DefinePackage(fmt.Sprintf("extra%d", n))
					switch kind {
					case "shared":
						pkg.Put(lisp.Symbol("value"), lisp.Nil())
					case "pending":
						pkg.Put(lisp.Symbol("value"), lisp.Int(1))
					}
				}
				tmpl := snapshotFixture(b, env)
				b.Run("phase=fork", func(b *testing.B) {
					b.ReportAllocs()
					for b.Loop() {
						vm := forkTemplateFixture(b, tmpl)
						runtime.KeepAlive(vm)
					}
				})
				b.Run("phase=publish", func(b *testing.B) {
					b.ReportAllocs()
					for b.Loop() {
						runtime.KeepAlive(snapshotFixture(b, env))
					}
				})
			})
		}
	}
}
