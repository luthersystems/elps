// Copyright © 2026 The ELPS authors

package lisp

// Test-only exports for the macro-expansion cache POC (issue #381).
// Compiled only into the lisp test binary.

// ProveUserMacroPureForTest runs the purity prover on a macro function
// value (as returned by evaluating the macro's name symbol).
func ProveUserMacroPureForTest(fun *LVal) bool {
	if fun.Type != LFun || len(fun.Cells) < 2 {
		return false
	}
	return proveUserMacroPure(fun)
}

// MacroCacheIdentityForTest reports whether fun is admissible for caching.
func MacroCacheIdentityForTest(fun *LVal) bool {
	_, ok := macroCacheIdentity(fun)
	return ok
}

// SealedForTest reports the sealed flag of v.
func SealedForTest(v *LVal) bool { return v != nil && v.sealed }

// SharedMacroCacheAllSealedForTest walks every entry in the process-shared
// cache and reports the entry count and whether every cached expansion tree
// is fully sealed (singletons excepted).
func SharedMacroCacheAllSealedForTest() (entries int, allSealed bool) {
	allSealed = true
	sharedMacroCache.m.Range(func(_, v any) bool {
		entries++
		if !expansionFullySealed(v.(*macroCacheEntry).exp) {
			allSealed = false
		}
		return true
	})
	return entries, allSealed
}
