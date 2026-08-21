// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// BenchmarkEnvConstructionCore measures lisp.NewEnv + lisp.InitializeUserEnv:
// the lisp package's own macros, special operators and builtins, and nothing
// else.
//
// BenchmarkEnvConstructionFull adds lisplib.LoadLibrary, which is where the
// stdlib's package-level definition tables are registered.
//
// These two exist because issue #363's fix -- copying each definition's formal
// argument list at registration so environments stop sharing one *LVal per
// definition process-wide -- puts its cost exactly here: once per definition
// per environment load, and nowhere on the evaluation path.  Environment
// construction had no benchmark at all, so the cost of the fix could only be
// asserted, and the issue asked for it to be measured.  Every other benchmark
// in the repository builds its environment in a helper outside the timed loop,
// which is correct for what those measure and blind to this.
func BenchmarkEnvConstructionCore(b *testing.B) {
	for b.Loop() {
		env := newBenchEnv(b)
		if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
			b.Fatalf("initialize-user-env: %v", rc)
		}
	}
}

func BenchmarkEnvConstructionFull(b *testing.B) {
	for b.Loop() {
		env := newBenchEnv(b)
		if rc := lisp.InitializeUserEnv(env); !rc.IsNil() {
			b.Fatalf("initialize-user-env: %v", rc)
		}
		if rc := lisplib.LoadLibrary(env); !rc.IsNil() {
			b.Fatalf("load-library: %v", rc)
		}
	}
}

func newBenchEnv(b *testing.B) *lisp.LEnv {
	b.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	env.Runtime.Stderr = &bytes.Buffer{}
	return env
}
