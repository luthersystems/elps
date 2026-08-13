// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The #394 seal check costs something on two different paths and they must be
// measured separately.
//
//   - ReadProgramSealed50KB is the HOT path: a normal, sealing reader.  The
//     constructor's only new work is the sealedThroughout walk, which
//     terminates at the first unsealed node and — on a sealed tree — visits
//     every node once.  Nothing is copied.  This is the number that decides
//     whether the fix is affordable, because it is what every embedder using
//     the standard reader pays on every parse.
//   - ReadProgramUnsealed50KB is the REPAIR path: a reader that does not
//     seal.  Here the walk fails immediately at the root and the constructor
//     pays a deep Copy plus SealAST.  It is paid ONCE per Program, not once
//     per load, which is the whole reason a Program exists.
//
// Both use the same 50KB synthetic corpus as the detach benchmarks
// (synthesizeSource in detach_test.go): interleaved defuns, let bindings,
// sorted-map and vector calls, quoted lists, string and float literals.

func BenchmarkReadProgramSealed50KB(b *testing.B) {
	src := synthesizeSource(50 * 1024)
	reader := parser.NewReader()
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		p, err := lisp.ReadProgram(reader, "bench.lisp", strings.NewReader(src))
		if err != nil {
			b.Fatalf("ReadProgram: %v", err)
		}
		if p.Len() == 0 {
			b.Fatal("no exprs")
		}
	}
}

func BenchmarkReadProgramUnsealed50KB(b *testing.B) {
	src := synthesizeSource(50 * 1024)
	reader := parser.NewReader(parser.WithFormatPreserving())
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		p, err := lisp.ReadProgram(reader, "bench.lisp", strings.NewReader(src))
		if err != nil {
			b.Fatalf("ReadProgram: %v", err)
		}
		if p.Len() == 0 {
			b.Fatal("no exprs")
		}
	}
}

// BenchmarkLoadProgram50KB is the amortization argument, measured rather
// than asserted: the repair is construction-time, so the per-load cost — the
// thing an embedder's parse cache actually repeats — is untouched by #394.
func BenchmarkLoadProgram50KB(b *testing.B) {
	src := synthesizeSource(50 * 1024)
	p, err := lisp.ReadProgram(parser.NewReader(), "bench.lisp", strings.NewReader(src))
	if err != nil {
		b.Fatalf("ReadProgram: %v", err)
	}
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		b.Fatalf("initialize-user-env: %v", rc)
	}
	b.SetBytes(int64(len(src)))
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if rc := env.LoadProgram(p); rc.Type == lisp.LError {
			b.Fatalf("load: %v", rc)
		}
	}
}
