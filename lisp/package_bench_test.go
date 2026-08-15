// Copyright © 2025 The ELPS authors

package lisp

import (
	"strconv"
	"testing"
)

// benchPackage builds a package with a realistic number of bindings, a
// mix of functions and non-functions, plus one function value bound
// under two names (the shape that used to make Package.Get write
// FunNames on every other lookup — see issue #397).
func benchPackage() (*Package, []*LVal, []*LVal) {
	pkg := NewPackage("bench")
	shared := FunInPackage("bench", "fid-shared", Formals(), func(env *LEnv, args *LVal) *LVal {
		return Nil()
	})
	pkg.Put(Symbol("alpha"), shared)
	pkg.Put(Symbol("beta"), shared)

	funSyms := []*LVal{Symbol("alpha"), Symbol("beta")}
	valSyms := make([]*LVal, 0, 128)
	for i := range 128 {
		name := "fn-" + strconv.Itoa(i)
		pkg.Put(Symbol(name), FunInPackage("bench", "fid-"+strconv.Itoa(i), Formals(),
			func(env *LEnv, args *LVal) *LVal { return Nil() }))
		funSyms = append(funSyms, Symbol(name))

		vname := "val-" + strconv.Itoa(i)
		pkg.Put(Symbol(vname), Int(i))
		valSyms = append(valSyms, Symbol(vname))
	}
	return pkg, funSyms, valSyms
}

// BenchmarkPackageGetFun measures the hot path: looking up a symbol
// bound to a function.  This is the path issue #397 lived on.
func BenchmarkPackageGetFun(b *testing.B) {
	pkg, funSyms, _ := benchPackage()
	b.ReportAllocs()
	b.ResetTimer()
	for i := range b.N {
		pkg.Get(funSyms[i%len(funSyms)])
	}
}

// BenchmarkPackageGetFunAliased hammers only the two names bound to the
// same function value.  Pre-fix every iteration wrote FunNames; post-fix
// it is a pure read.
func BenchmarkPackageGetFunAliased(b *testing.B) {
	pkg, _, _ := benchPackage()
	alpha, beta := Symbol("alpha"), Symbol("beta")
	b.ReportAllocs()
	b.ResetTimer()
	for i := range b.N {
		if i%2 == 0 {
			pkg.Get(alpha)
		} else {
			pkg.Get(beta)
		}
	}
}

// BenchmarkPackageGetVal is the control: looking up a non-function
// binding never touched FunNames, so this arm should not move.
func BenchmarkPackageGetVal(b *testing.B) {
	pkg, _, valSyms := benchPackage()
	b.ReportAllocs()
	b.ResetTimer()
	for i := range b.N {
		pkg.Get(valSyms[i%len(valSyms)])
	}
}

// BenchmarkPackageGetFunParallel measures the concurrent case the fix
// exists for: many goroutines reading one shared *Package.  Pre-fix this
// benchmark is not merely slow, it is a fatal runtime error.
func BenchmarkPackageGetFunParallel(b *testing.B) {
	pkg, funSyms, _ := benchPackage()
	b.ReportAllocs()
	b.ResetTimer()
	b.RunParallel(func(pb *testing.PB) {
		i := 0
		for pb.Next() {
			pkg.Get(funSyms[i%len(funSyms)])
			i++
		}
	})
}
