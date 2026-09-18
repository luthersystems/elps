// Copyright © 2026 The ELPS authors

package lisp

import "testing"

// selectBenchInput builds an n-element vector and the arguments the filtering
// builtins take over it. The predicate is Go-implemented so the measurement is
// the builtin's own work rather than the evaluator's.
func selectBenchInput(n int, keep func(int) bool) *LVal {
	vec := MakeVector(n)
	for i := range vec.Cells[1].Cells {
		vec.Cells[1].Cells[i] = Int(i)
	}
	pred := FunInPackage(DefaultUserPackage, "keep?", Formals("x"), func(env *LEnv, args *LVal) *LVal {
		return Bool(keep(args.Cells[0].Int))
	})
	return QExpr([]*LVal{Symbol("vector"), pred, vec})
}

func benchmarkFilter(b *testing.B, call func(*LEnv, *LVal) *LVal, n int, keep func(int) bool) {
	b.Helper()
	env := NewEnv(nil)
	if rc := InitializeUserEnv(env); rc.Type == LError {
		b.Fatalf("initialize-user-env: %v", rc)
	}
	args := selectBenchInput(n, keep)
	if got := call(env, args); got.Type != LArray {
		b.Fatalf("filter did not build a vector: %v", got)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for b.Loop() {
		call(env, args)
	}
}

func never(int) bool  { return false }
func always(int) bool { return true }
func even(i int) bool { return i%2 == 0 }

func BenchmarkSelectVectorKeepNone(b *testing.B) {
	benchmarkFilter(b, builtinSelect, 100_000, never)
}

func BenchmarkSelectVectorKeepAll(b *testing.B) {
	benchmarkFilter(b, builtinSelect, 100_000, always)
}

func BenchmarkSelectVectorKeepHalf(b *testing.B) {
	benchmarkFilter(b, builtinSelect, 100_000, even)
}

func BenchmarkRejectVectorKeepNone(b *testing.B) {
	benchmarkFilter(b, builtinReject, 100_000, always)
}

func BenchmarkRejectVectorKeepAll(b *testing.B) {
	benchmarkFilter(b, builtinReject, 100_000, never)
}

func BenchmarkRejectVectorKeepHalf(b *testing.B) {
	benchmarkFilter(b, builtinReject, 100_000, even)
}
