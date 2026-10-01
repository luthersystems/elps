// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"fmt"
	"runtime"
	"testing"
)

// BenchmarkTemplateValueSettings measures borrowing and the first value setting write in a VM.
func BenchmarkTemplateValueSettings(b *testing.B) {
	for _, values := range []int{0, 1} {
		b.Run(fmt.Sprintf("values=%d", values), func(b *testing.B) {
			env := templatePlanBenchmarkFixture(b, 0)
			if values == 1 {
				if err := env.Runtime.SetSettingValue("id", "published"); err != nil {
					b.Fatal(err)
				}
			}
			tmpl := snapshotFixture(b, env)
			b.Run("phase=fork", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					runtime.KeepAlive(forkTemplateFixture(b, tmpl))
				}
			})
			b.Run("phase=fork-write", func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					vm := forkTemplateFixture(b, tmpl)
					if err := vm.Runtime.SetSettingValue("id", "vm"); err != nil {
						b.Fatal(err)
					}
					runtime.KeepAlive(vm)
				}
			})
		})
	}
}
