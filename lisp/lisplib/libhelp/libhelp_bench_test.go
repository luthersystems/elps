// Copyright © 2026 The ELPS authors

package libhelp_test

import (
	"fmt"
	"io"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
)

// Keep the corpus independent of repository documentation (elps#659). At the
// time this fixture was introduced, the real corpus had 13 packages, 266 symbols
// (137 in lisp), and about 58 KB of docs. Use 13 packages with 128 exports in one,
// 9 in the rendered package, and 10 in the others (247 total), with 128-byte
// docstrings (about 33 KB), to retain the same scale and a similarly uneven
// package-size distribution.
// The rendered package has 9 exports, matching string at the time. These sizes
// are fixed workload parameters: do not update them as the real corpus grows.
const benchPackageCount = 13

func newBenchEnv(b *testing.B) *lisp.LEnv {
	b.Helper()
	env := lisp.NewEnv(nil)
	// No InitializeUserEnv or LoadLibrary: even core docs must stay out of the
	// fixture. Avoid the special "lisp" package name used by QueryPackages.
	doc := strings.Repeat("  Fixed documentation for a synthetic benchmark function.      \n", 2)
	for i := range benchPackageCount {
		pkg := env.Runtime.Registry.DefinePackage(fmt.Sprintf("bench%02d", i))
		env.Runtime.Package = pkg
		pkg.Doc = doc
		n := 10
		switch i {
		case 0:
			n = 128
		case 1:
			n = 9
		}
		for j := range n {
			name := fmt.Sprintf("symbol%03d", j)
			fun := lisp.FunInPackage(pkg.Name, name, lisp.Formals("value"),
				func(_ *lisp.LEnv, _ *lisp.LVal) *lisp.LVal { return lisp.Nil() })
			if rc := pkg.Put(lisp.Symbol(name), fun); !rc.IsNil() {
				b.Fatalf("register %s:%s: %v", pkg.Name, name, rc)
			}
			env.SetSymbolDoc(name, doc)
			pkg.Export(name)
		}
	}
	return env
}

func BenchmarkQueryPackages(b *testing.B) {
	env := newBenchEnv(b)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		pkgs := libhelp.QueryPackages(env)
		if len(pkgs) != benchPackageCount {
			b.Fatalf("got %d packages, want %d", len(pkgs), benchPackageCount)
		}
	}
}

func BenchmarkRenderPkgExported(b *testing.B) {
	env := newBenchEnv(b)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if err := libhelp.RenderPkgExported(io.Discard, env, "bench01"); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkRenderPackageList(b *testing.B) {
	env := newBenchEnv(b)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if err := libhelp.RenderPackageList(io.Discard, env); err != nil {
			b.Fatal(err)
		}
	}
}
