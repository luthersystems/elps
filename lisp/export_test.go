// Copyright © 2026 The ELPS authors

package lisp

import "github.com/luthersystems/elps/parser/token"

// SplicedFlag exposes the unexported spliced flag to package lisp_test.
// The field has no production accessor (issue #382): splicing is evaluator
// plumbing.
func SplicedFlag(v *LVal) bool { return v.spliced }

// MapBacking exposes MapData's unexported backing field to package
// lisp_test.  The field went unexported in issue #382 (the backing is fixed
// at construction); tests still nil-probe it to walk degenerate MapData
// values.
func MapBacking(md *MapData) Map { return md.mapBacking }

// --- test-only reads of the LVal fields issue #382 unexported ---
//
// WHY THESE HAVE TO EXIST.  #382 unexported LVal.source and made Source()
// return a value COPY, which is the point: a caller cannot obtain, and so
// cannot write through, a Location another value holds.  That also makes the
// aliasing property issue #446 is about ("the copy and the original hold ONE
// mutable Location") unobservable from outside the package.  The property did
// not stop mattering -- a copy of a parsed node is mutable storage whose
// SetSource is live, and sharing the pointer would let a write through the
// copy move a position in the tree every environment in the process is
// evaluating -- so its regression tests need the field, and a
// same-directory test build is the sanctioned way to have it.
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
