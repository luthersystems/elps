// Copyright © 2026 The ELPS authors

package lisp

// Test-only exports for the macro-expansion cache POC (issue #381).
// Compiled only into the lisp test binary.

// ProveUserMacroPureForTest runs the SYNTACTIC half of the purity prover on
// a macro function value (as returned by evaluating the macro's name
// symbol).  It deliberately ignores the name-resolution obligations — use
// MacroCacheIdentityForTest for the full admission test.
func ProveUserMacroPureForTest(fun *LVal) bool {
	if fun.Type != LFun || len(fun.Cells) < 2 {
		return false
	}
	return proveUserMacroPure(fun).pure
}

// MacroCacheKernelRefsForTest returns the operator spellings the syntactic
// proof interpreted, as "defining-env" and "calling-env" obligation lists.
func MacroCacheKernelRefsForTest(fun *LVal) (def, call []string) {
	if fun.Type != LFun || len(fun.Cells) < 2 {
		return nil, nil
	}
	p := proveUserMacroPure(fun)
	for _, r := range p.defRefs {
		def = append(def, r.spelling)
	}
	for _, r := range p.callRefs {
		call = append(call, r.spelling)
	}
	return def, call
}

// MacroCacheIdentityForTest reports whether fun is admissible for caching in
// env (which stands in for both the callsite environment and, for macros
// defined there, the defining environment).
func MacroCacheIdentityForTest(env *LEnv, fun *LVal) bool {
	_, ok := macroCacheIdentity(env, fun)
	return ok
}

// MacroCacheFIDForTest builds the FID the admission path expects for a
// registration kind ("macro", "special-op", "builtin") and name.
func MacroCacheFIDForTest(kind, name string) string {
	switch kind {
	case "macro":
		return builtinMacroFID(name)
	case "special-op":
		return specialOpFID(name)
	case "builtin":
		return builtinFunFID(name)
	}
	return ""
}

// FunFIDForTest and FunPackageForTest expose a function value's registration
// identity.
func FunFIDForTest(fun *LVal) string {
	fd := fun.funData()
	if fd == nil {
		return ""
	}
	return fd.fid
}

func FunPackageForTest(fun *LVal) string {
	fd := fun.funData()
	if fd == nil {
		return ""
	}
	return fd.pkg
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

// MacroRegistrationIDForTest exposes the registration identity a builtin
// macro was bound with (funData.impl; 0 for user macros and for macro
// values built outside LEnv.AddMacros).
func MacroRegistrationIDForTest(fun *LVal) uint64 {
	fd := fun.funData()
	if fd == nil {
		return 0
	}
	return fd.impl
}
