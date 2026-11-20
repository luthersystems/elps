// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"time"
)

// NativeCloner supplies an independent payload for Lisp copy and strict detach.
// CloneNative must not mutate its receiver. A detached clone must retain no
// reference into the source Runtime or LEnv tree; a within-VM copy may retain
// immutable references belonging to that same VM.
//
// This is not a VM construction protocol. NewTemplate rejects mutable native
// payloads, including NativeCloner implementations. Approved immutable payloads
// are shared without invoking CloneNative. Create mutable services per VM.
type NativeCloner interface {
	CloneNative() any
}

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
// seal invariants that make sharing safe (docs/internals/sealed-ast.md, lisp/seal.go)
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
//
// A caller whose false branch only builds an error message wants
// RequireNative, which runs these same three gates and renders the failure
// as an LError.
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

// RequireNative reads the Go payload of an LNative value as a T, rendering
// the failure as an LError rather than as a bool.  On success it returns the
// payload and a nil second value; the second value is nil ON SUCCESS ONLY,
// so a caller's whole failure branch is
//
//	h, err := lisp.RequireNative[*Handle](v)
//	if err != nil {
//		return lisp.Error(err)
//	}
//
// which is the message the accessor pair was introduced to supply once
// instead of at each boundary (issue #546).  NativeValue's `false` says only
// that something did not match, so every caller of it that reports a failure
// writes its own wording, and the substrate accumulated eleven of them —
// differing in which of the three gates they name, and none naming the type
// that was expected, which is the one fact the caller has and the value does
// not.
//
// The gate sequence is NativeValue's, because this is a call to it: nil v,
// then v.Type == LNative, then the assertion, with the LVal type checked
// before the payload for the reasons documented there.  RequireNative only
// re-examines v afterwards to say which gate refused it, so there is no
// second copy of the check to drift.
//
// The three messages name the expected type and what was found:
//
//	expected native *pkg.Handle value, got nil
//	expected native *pkg.Handle value, got bytes
//	expected native *pkg.Handle value, got native *pkg.Other
//
// The expected type comes from reflect.TypeFor[T] rather than a %T verb
// because T is a type parameter, not a value: %T of the zero T renders an
// interface T as <nil>, which loses precisely the type the message exists to
// report.  The reflection is confined to the failure path.
//
// The error carries no call stack of its own: with no *LEnv parameter
// there is no environment to capture one from, so it is built with the
// package-level Errorf (the env.Errorf methods attach a stack eagerly;
// this accessor cannot).  Nothing is lost — an error returned up through
// eval acquires the current stack from LEnv.ErrorAssociate, which fills
// one in for any error still lacking it.
func RequireNative[T any](v *LVal) (T, error) {
	x, ok := NativeValue[T](v)
	if ok {
		return x, nil
	}
	var zero T
	want := reflect.TypeFor[T]()
	switch {
	case v == nil:
		return zero, (*ErrorVal)(Errorf("expected native %v value, got nil", want))
	case v.Type != LNative:
		return zero, (*ErrorVal)(Errorf("expected native %v value, got %v", want, v.Type))
	default:
		return zero, (*ErrorVal)(Errorf("expected native %v value, got native %T", want, v.Native))
	}
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
// Mutable native payloads must be created per VM, after Template
// instantiation. NativeOf stores the value as-is; it does not declare that
// the payload is immutable or suitable for a Template.
func NativeOf[T any](x T) *LVal {
	return Native(x) //elpsvet:allow-native the typed constructor: T is the caller's, and every NativeOf call is checked at its own call site
}

// NativeEqualer lets a native payload define equal? for LNative values.
// equal? calls EqualNative on the first operand's payload, or on the second
// operand's when only that one implements it, and passes the other payload.
// When both implement it with different types, both calls must return true.
// Two payloads that are the same pointer are equal without a call.
//
// EqualNative must be an equivalence: reflexive, symmetric and transitive.
// It must be deterministic and must not mutate either payload, because
// every endorser of a transaction must compute the same answer.
type NativeEqualer interface {
	EqualNative(other any) bool
}

// opaqueEqual is equal? for an error, a function or a native value, the
// value types equal? cannot walk (luthersystems/elps#997):
//
//   - A value is equal? to itself.
//   - Two functions are equal? when they hold the same function: the same
//     builtin registration or the same closure.  Two evaluations of one
//     lambda form make two functions.
//   - Two errors are equal? only when they are the same value.
//   - Two native values are equal? when a payload's NativeEqualer says so.
//     Without one, payloads of the same scalar type (a bool, number or
//     string kind, such as time.Duration) compare with ==, two time.Time
//     payloads compare with Time.Equal, and pointer payloads compare by
//     address.
func opaqueEqual(a, b *LVal) bool {
	if a == b {
		return true
	}
	if a.Type != b.Type {
		return false
	}
	if a.Type == LFun {
		fa, aok := a.Native.(*funData)
		fb, bok := b.Native.(*funData)
		return aok && bok && fa == fb
	}
	if a.Type == LNative {
		return nativeEqual(a.Native, b.Native)
	}
	// An error, or any type equal? does not compare, is equal only to itself.
	return false
}

func nativeEqual(x, y any) bool {
	if x == nil || y == nil {
		return x == nil && y == nil
	}
	xt, yt := reflect.TypeOf(x), reflect.TypeOf(y)
	if xt == yt && isAddressKind(xt.Kind()) {
		// The same pointer is the same payload, whatever its EqualNative
		// says. A *T gets a value-receiver EqualNative by promotion, and a
		// nil *T would panic in it.
		if sameAddress(x, y) {
			return true
		}
		if reflect.ValueOf(x).IsNil() || reflect.ValueOf(y).IsNil() {
			return false
		}
	}
	ex, xok := x.(NativeEqualer)
	ey, yok := y.(NativeEqualer)
	switch {
	case xok && yok && xt != yt:
		// Two packages' types: both must agree, so the answer does not
		// depend on the order of the arguments.
		return ex.EqualNative(y) && ey.EqualNative(x)
	case xok:
		return ex.EqualNative(y)
	case yok:
		return ey.EqualNative(x)
	}
	if tx, ok := x.(time.Time); ok {
		// A host time.Time is what libtime's own payload compares with,
		// so two of them must compare the same way to keep equal?
		// transitive.
		ty, ok := y.(time.Time)
		return ok && tx.Equal(ty)
	}
	if xt != yt {
		return false
	}
	switch xt.Kind() {
	case reflect.Bool,
		reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr,
		reflect.Float32, reflect.Float64, reflect.Complex64, reflect.Complex128,
		reflect.String:
		return x == y
	case reflect.Pointer, reflect.UnsafePointer, reflect.Chan:
		// sameAddress already answered.
		return false
	case reflect.Invalid, reflect.Array, reflect.Func, reflect.Interface,
		reflect.Map, reflect.Slice, reflect.Struct:
		// Comparing these would walk host data, or panic on an
		// uncomparable field. Only the same LVal is equal.
		return false
	}
	return false
}

func isAddressKind(k reflect.Kind) bool {
	return k == reflect.Pointer || k == reflect.UnsafePointer || k == reflect.Chan
}

// sameAddress reports whether two payloads of one address kind hold the same
// address. Go does not say whether pointers to two distinct zero-size
// variables are equal, so endorsers built differently could disagree. Such a
// pointer is never the same address here.
func sameAddress(x, y any) bool {
	xv, yv := reflect.ValueOf(x), reflect.ValueOf(y)
	if xv.Kind() == reflect.Pointer && xv.Type().Elem().Size() == 0 {
		return false
	}
	return xv.Pointer() == yv.Pointer()
}
