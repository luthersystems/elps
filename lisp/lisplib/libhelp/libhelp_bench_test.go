// Copyright © 2026 The ELPS authors

package libhelp_test

import (
	"io"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/parser"
)

func newBenchEnv(b *testing.B) *lisp.LEnv {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
		b.Fatalf("init: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); !rc.IsNil() {
		b.Fatalf("load library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); !rc.IsNil() {
		b.Fatalf("in-package: %v", rc)
	}
	return env
}

func BenchmarkQueryPackages(b *testing.B) {
	env := newBenchEnv(b)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		pkgs := libhelp.QueryPackages(env)
		if len(pkgs) == 0 {
			b.Fatal("no packages")
		}
	}
}

func BenchmarkRenderPkgExported(b *testing.B) {
	env := newBenchEnv(b)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if err := libhelp.RenderPkgExported(io.Discard, env, "string"); err != nil {
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
