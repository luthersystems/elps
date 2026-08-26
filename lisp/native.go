// Copyright © 2026 The ELPS authors

package lisp

// NativeValue reads the Go payload of an LNative value as a T, reporting
// whether the value actually was one.  It replaces the hand-rolled pair
// every embedder writes at each boundary where lisp hands Go data back —
//
//	if v.Type != lisp.LNative {
//		return fmt.Errorf("not a native value: %v", v.Type)
//	}
//	h, ok := v.Native.(*Handle)
//
// — which is unremarkable once and a liability eleven times (issue #546):
// the two halves are separable, and the half that gets dropped when a
// caller is in a hurry is the type check, because the assertion alone
// *looks* like it is doing the work.
//
// The three gates run in this order, and any failure returns the zero T and
// false.  NativeValue never panics:
//
//  1. v is non-nil;
//  2. v.Type == LNative;
//  3. v.Native asserts to T.
//
// Gate 2 preceding gate 3 is the point of the function, not a stylistic
// preference, because LVal.Native is NOT reserved for embedder payloads.
// It is also the interpreter's own backing storage for three lisp-reachable
// types: an LBytes value keeps its *[]byte there, an LSortMap its
// *MapData, and an LError its *CallStack.  A bare `v.Native.(*[]byte)`
// therefore succeeds on any ordinary lisp bytes value and hands the caller
// the pointer the interpreter is itself holding — a write through it
// mutates a live value behind the kernel's back, past the ownership and
// seal invariants that make sharing safe (docs/sealed-ast.md, lisp/seal.go)
// and past `copy`'s promise that a copy shares no storage with its original
// (issue #378).  The same shape reaches a sorted map's whole backing
// implementation, and an error's captured call stack, whose Go-stack
// snapshot is the non-forgeable marker IsInternalPanic keys off.  Gate 2
// makes all three unreachable through this accessor.
//
// The gate is on the LVAL's type, never on the payload's, and the
// difference is deliberate: an embedder who stores a *[]byte of their own
// with Native(&b) gets it back from NativeValue[*[]byte], because that
// value is an LNative and the slice is theirs.  What is refused is the
// interpreter's storage, not a Go type.
//
// T may be an interface: a payload stored as a concrete type is retrieved
// through any interface it implements, since gate 3 is an ordinary type
// assertion.  A T of `any` succeeds for every non-nil payload of an
// LNative and is the way to ask "is there anything here" without naming the
// type.
func NativeValue[T any](v *LVal) (T, bool) {
	var zero T
	if v == nil || v.Type != LNative {
		return zero, false
	}
	x, ok := v.Native.(T)
	if !ok {
		return zero, false
	}
	return x, true
}

// NativeOf is the typed constructor counterpart of Native: it wraps x in an
// LNative exactly as Native does, and is implemented as a call to it, so the
// two produce indistinguishable values.
//
// What it buys is a compiler check at the write end of a boundary whose read
// end is NativeValue.  Native takes an interface{}, so it accepts whatever
// the caller happens to be holding and any mismatch with what readers ask
// for surfaces later as a `false` from a type assertion, at a call site far
// from the mistake.  Writing NativeOf[Handle](h) instead states the stored
// type in the source and makes storing something else a build failure —
// worth doing wherever the payload type is spelled out again in the
// NativeValue[Handle] that reads it back (issue #546).
//
// A payload whose state must not be shared across a Fork should also
// implement NativeCloner; NativeOf stores the value as-is and takes no
// position on that.
func NativeOf[T any](x T) *LVal {
	return Native(x)
}
