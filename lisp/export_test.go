// Copyright © 2026 The ELPS authors

package lisp

import "github.com/luthersystems/elps/parser/token"

// Test-only bridges to the unexported detach machinery (lisp/detach.go).
// detach has no production consumers and stays unexported until a real
// embedder consumer materializes; the external test battery in package
// lisp_test keeps exercising the full contract through these functions,
// which exist only in test builds.

// Detach exposes (*LVal).detach to package lisp_test.
func Detach(v *LVal) (*LVal, error) { return v.detach() }

// ProgramDetach exposes Program.detach to package lisp_test.
func ProgramDetach(p Program) ([]*LVal, error) { return p.detach() }

// SplicedFlag exposes the unexported spliced flag to package lisp_test.
// The field has no production accessor (issue #382): splicing is evaluator
// plumbing, but the seal fingerprint tests hash it to prove sealed trees
// survive evaluation bit-identically.
func SplicedFlag(v *LVal) bool { return v.spliced }

// MapBacking exposes MapData's unexported backing field to package
// lisp_test.  The field went unexported in issue #382 (the backing is fixed
// at construction); the detach tests still nil-probe it to walk degenerate
// MapData values.
func MapBacking(md *MapData) Map { return md.mapBacking }

// --- test-only reads of the LVal fields issue #382 unexported ---
//
// WHY THESE HAVE TO EXIST.  #382 unexported LVal.source and made Source()
// return a value COPY, which is the point: a caller cannot obtain, and so
// cannot write through, a Location another value holds.  That also makes the
// aliasing property issue #446 is about ("the copy and the original hold ONE
// mutable Location") unobservable from outside the package.  The property did
// not stop mattering -- LVal.Copy CLEARS the seal, so a copy of a parsed node
// is mutable storage whose SetSource is live, and sharing the pointer would
// let a write through the copy move a position in the sealed tree every
// environment in the process is evaluating -- so its regression tests need the
// field, and a same-directory test build is the sanctioned way to have it.
//
// Do NOT promote these to production accessors.  Handing out the stored
// *token.Location is precisely what #362 removed.

// SourceRefForTest returns the *token.Location v stores, by reference, or nil
// when v records no position.
func SourceRefForTest(v *LVal) *token.Location {
	if v == nil {
		return nil
	}
	return v.source
}

// IsSealedForTest reports v's seal bit without going through the exported
// IsSealed, so a test can assert on the bit itself while IsSealed's own
// contract is under test.
func IsSealedForTest(v *LVal) bool {
	return v != nil && v.sealed
}

// SetEnvLocForTest sets env.loc, which eval owns and #382 unexported along
// with the rest of LEnv's mutable state.  It exists so a test can drive a
// macro or an error constructor from a KNOWN caller position and then check
// what the callee did with it -- the whole subject of issues #366 and #431.
func SetEnvLocForTest(env *LEnv, loc *token.Location) {
	env.loc = loc
}

// EnvLocForTest reads env.loc back, by reference, for the same reason.
func EnvLocForTest(env *LEnv) *token.Location {
	return env.loc
}

// --- test-only bridges into the sealed load cache (lisp/loadcache.go) ---
//
// WHY THESE HAVE TO EXIST.  CachedSource is opaque BY DESIGN: no exported
// member yields a *LVal, and the only constructor (newCachedSource) routes
// every parse through newProgram's admission, so a legally-minted entry is
// sealed throughout.  Both properties are exactly what the tests need to
// get around:
//
//   - The alias proof needs the nodes themselves, to assert that two loads
//     of one key evaluate the SAME *LVal rather than two equal ones.
//   - The seal-fingerprint proofs need the roots, to show a cached tree's
//     bytes are identical before and after another environment evaluated
//     it.
//   - The ownership red-proof needs an ILLEGAL entry — one carrying an
//     unsealed node — which no production path can build.  A guard that has
//     never been shown to fire is not known to work.
//
// Do NOT promote any of these.  CachedSourceForTest in particular
// constructs precisely the object the admission exists to make
// unconstructible.

// CachedSourceExprs exposes a cache entry's sealed expressions to the test
// battery.  Read-only by contract: the nodes ARE the cached program.
func CachedSourceExprs(s *CachedSource) []*LVal {
	if s == nil {
		return nil
	}
	return s.prog.exprs
}

// CachedSourceForTest mints a CachedSource around exprs WITHOUT the
// admission walk newCachedSource performs.  Test-only, and its only purpose
// is to build the entry the ownership red-proof needs.
func CachedSourceForTest(key, name, loc string, exprs []*LVal) *CachedSource {
	return &CachedSource{
		key:  key,
		name: name,
		loc:  loc,
		prog: Program{exprs: exprs},
		fp:   SealedASTFingerprint(exprs),
	}
}

// LoadCacheKeyForTest exposes the key derivation so a test can pre-seed a
// cache, or assert what elps will ask for.
func LoadCacheKeyForTest(name, loc, readerID string, byLoc bool, src []byte) string {
	return loadCacheKey(name, loc, readerID, byLoc, src)
}

// ReaderIdentityForTest exposes the reader-identity derivation so a test can
// build the exact key elps will ask for a given reader.  The second result is
// false when the reader declined to state an identity (an empty
// ReaderIdentity token), in which case no key is derivable at all.
func ReaderIdentityForTest(r Reader) (string, bool) {
	return readerIdentity(r)
}
