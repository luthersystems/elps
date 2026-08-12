// Copyright © 2025 The ELPS authors

package lisp

import (
	"fmt"
)

// Singleton LVals for Nil, true, and false.
//
// These are pre-allocated, shared, immutable values returned by Nil(),
// Bool(true), and Bool(false). They exist because these three values are
// constructed on nearly every evaluation cycle — Nil() is returned by
// ~130 call sites (every builtin/op that "returns nothing"), and Bool()
// is returned by every comparison, predicate, and type check.
//
// SAFETY: A full audit of all 127 Nil() and 63 Bool() call sites
// confirmed that no call site mutates the returned *LVal. The only
// historical mutation vector was stampMacroExpansion in macro.go (see
// issue #274); it is now guarded by isSingleton/assertNotSingleton.
// The ELPS language has no destructive list operations (no nconc,
// rplaca, etc.) — append! only works on vectors (LArray), not lists
// (LSExpr), and cons/append create new lists without mutating their
// inputs.
//
// INVARIANT: Code that receives a Nil(), Bool(true), or Bool(false)
// return value MUST NOT mutate any field on the returned *LVal. If you
// need a mutable nil value (e.g., to build up a list), use SExpr(nil)
// directly instead of Nil(). Tree walkers that mutate LVal fields MUST
// guard with isSingleton(v) / assertNotSingleton(v) before writing.
// The //elpsvet:allow markers below suppress cmd/elpsvet's rule against
// package-level LVals: these three are the deliberate, guarded exception —
// shared across every Runtime by design, immutable by decree, snapshotted by
// checkSingleton in elpscheck builds, and exempt from the runtime ownership
// checker (lisp/ownership_check_elpscheck.go).  Nothing else earns this.
var (
	singletonNil   = &LVal{Type: LSExpr}                    //elpsvet:allow guarded singleton
	singletonTrue  = &LVal{Type: LSymbol, Str: TrueSymbol}  //elpsvet:allow guarded singleton
	singletonFalse = &LVal{Type: LSymbol, Str: FalseSymbol} //elpsvet:allow guarded singleton
)

// isSingleton reports whether v is one of the three shared, immutable
// singleton LVal values (Nil(), Bool(true), Bool(false)).
//
// Identity-based — not type-based — because singletonTrue/singletonFalse
// are LSymbol values that would otherwise pass type-based "synthetic
// node" checks. See issue #274.
func isSingleton(v *LVal) bool {
	return v == singletonNil || v == singletonTrue || v == singletonFalse
}

// assertNotSingleton panics if v is a shared singleton LVal. Intended as
// a guard at the entry of any function that walks an LVal tree and
// writes to LVal fields. Cheap (one pointer compare against each of
// three constants), documents intent, and surfaces regressions loudly.
func assertNotSingleton(v *LVal) {
	if isSingleton(v) {
		panic(fmt.Sprintf("internal error: attempt to mutate singleton LVal (%s)", v.Type))
	}
}

// SingletonSnapshot captures the current bit pattern of the three
// singleton LVals. It is used by test infrastructure to detect
// inadvertent mutations to shared singletons — see TestMain in lisp_
// and the per-test guard in elpstest.Runner.
type SingletonSnapshot struct {
	nilV   LVal
	trueV  LVal
	falseV LVal
}

// TakeSingletonSnapshot captures the current state of the three
// singleton LVals so callers can later verify they were not mutated.
func TakeSingletonSnapshot() SingletonSnapshot {
	return SingletonSnapshot{
		nilV:   *singletonNil,
		trueV:  *singletonTrue,
		falseV: *singletonFalse,
	}
}

// Verify compares the current singleton state to the snapshot. If any
// singleton has drifted it returns the name of the offender; otherwise
// it returns the empty string.
//
// Verify detects drift in VALUES. A write that stores the value a field
// already holds leaves no drift and Verify reports nothing, even though
// such a write still races with every concurrent reader — that was issue
// #333. Do not read an empty return as "no code mutated a singleton".
// See the "What this does NOT catch" section on checkSingleton in
// singleton_check_elpscheck.go for what covers that case.
func (s SingletonSnapshot) Verify() string {
	if !singletonLValEqual(&s.nilV, singletonNil) {
		return "Nil()"
	}
	if !singletonLValEqual(&s.trueV, singletonTrue) {
		return "Bool(true)"
	}
	if !singletonLValEqual(&s.falseV, singletonFalse) {
		return "Bool(false)"
	}
	return ""
}

// singletonLValEqual reports whether two LVal struct values are
// equivalent for singleton-corruption detection. It compares all
// scalar/pointer fields exactly; slice fields are compared by length
// (singletons should always have len == 0). The Native interface field
// is compared with == (singletons should always be nil).
func singletonLValEqual(a, b *LVal) bool {
	return a.source == b.source &&
		a.Type == b.Type &&
		a.Str == b.Str &&
		a.Int == b.Int &&
		a.Float == b.Float &&
		a.FunType == b.FunType &&
		a.quoted == b.quoted &&
		a.spliced == b.spliced &&
		a.Meta == b.Meta &&
		a.macroExpansion == b.macroExpansion &&
		a.Native == b.Native &&
		len(a.Cells) == len(b.Cells)
}
