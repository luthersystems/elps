// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// These tests cover the checked native accessors of issue #546:
// NativeValue, which reads an LNative's Go payload as a T, RequireNative,
// which is NativeValue with the failure rendered as an LError, and NativeOf,
// the typed constructor counterpart.
//
// Two properties are under test, and they are not equally interesting.
// The round-trip ones (1-7 below) say the accessor is usable: pointers,
// value types and interface type parameters all survive, and every
// no-match shape answers (zero, false) instead of panicking.
//
// The load-bearing ones are TestNativeValueRefusesInternalStorage and its
// companion TestNativeValueGatesOnLValTypeNotPayloadType.  LVal.Native is
// the interpreter's OWN backing storage for LBytes (*[]byte), LSortMap
// (*MapData) and LError (*CallStack), so an accessor that asserted on the
// payload without first checking v.Type would hand an embedder the live
// storage of an ordinary lisp value.  Those tests are what fails if the
// type gate is ever removed or reordered after the assertion; each one
// first proves the internal payload really is present, so a false from
// NativeValue is the gate refusing it rather than an assertion that had
// nothing to match.

// nativeTestEnv returns an initialized environment with a reader, so that
// LoadString can parse and evaluate program text.
func nativeTestEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	return newCowTestEnv(t)
}

// handle is a stateful payload of the shape embedders actually store: an
// opaque Go object reached through a pointer.
type handle struct {
	name string
	seq  int
}

// String makes *handle an fmt.Stringer, for the interface-type-parameter
// case.
func (h *handle) String() string { return fmt.Sprintf("handle(%s/%d)", h.name, h.seq) }

// point is a value-type payload: stored by copy, not by reference.
type point struct {
	X, Y int
}

// other is an unrelated payload type, for the wrong-type case.
type other struct {
	id string
}

// TestNativeValueRoundTripsPointer stores a pointer payload with Native and
// reads back the SAME instance, not an equal copy.
func TestNativeValueRoundTripsPointer(t *testing.T) {
	h := &handle{name: "conn", seq: 7}
	v := lisp.Native(h)
	got, ok := lisp.NativeValue[*handle](v)
	if !ok {
		t.Fatalf("NativeValue[*handle] returned ok=false for %v", v)
	}
	if got != h {
		t.Errorf("NativeValue returned %p, want the stored instance %p", got, h)
	}
	// Identity, not equality: a mutation through the retrieved pointer is
	// visible through the original.
	got.seq = 8
	if h.seq != 8 {
		t.Errorf("retrieved value is a copy: h.seq = %d, want 8", h.seq)
	}
}

// TestNativeValueRoundTripsThroughNativeOf is the same round trip with the
// typed constructor at the write end.  NativeOf must produce a value
// NativeValue accepts on identical terms.
func TestNativeValueRoundTripsThroughNativeOf(t *testing.T) {
	h := &handle{name: "conn", seq: 1}
	v := lisp.NativeOf[*handle](h)
	if v.Type != lisp.LNative {
		t.Fatalf("NativeOf produced type %v, want %v", v.Type, lisp.LNative)
	}
	got, ok := lisp.NativeValue[*handle](v)
	if !ok {
		t.Fatalf("NativeValue[*handle] returned ok=false for a NativeOf value")
	}
	if got != h {
		t.Errorf("NativeValue returned %p, want the stored instance %p", got, h)
	}
}

// TestNativeValueRoundTripsValueType covers a non-pointer struct payload,
// which travels by copy.  The retrieved value must equal what was stored.
func TestNativeValueRoundTripsValueType(t *testing.T) {
	p := point{X: 3, Y: 4}
	got, ok := lisp.NativeValue[point](lisp.NativeOf(p))
	if !ok {
		t.Fatalf("NativeValue[point] returned ok=false")
	}
	if got != p {
		t.Errorf("NativeValue returned %+v, want %+v", got, p)
	}
	// Asking for the pointer type when a value was stored is a different
	// type and must not match.
	if _, ok := lisp.NativeValue[*point](lisp.NativeOf(p)); ok {
		t.Error("NativeValue[*point] matched a payload stored as point")
	}
}

// TestNativeValueThroughInterface asserts T may be an interface: a payload
// stored as a concrete type is retrievable through any interface it
// implements, because the third gate is an ordinary type assertion.
func TestNativeValueThroughInterface(t *testing.T) {
	h := &handle{name: "iface", seq: 2}
	v := lisp.NativeOf[*handle](h)
	s, ok := lisp.NativeValue[fmt.Stringer](v)
	if !ok {
		t.Fatalf("NativeValue[fmt.Stringer] returned ok=false for a *handle payload")
	}
	if s.String() != h.String() {
		t.Errorf("Stringer returned %q, want %q", s.String(), h.String())
	}
	// `any` is the way to ask "is there a payload at all".
	if _, ok := lisp.NativeValue[any](v); !ok {
		t.Error("NativeValue[any] returned ok=false for a non-nil payload")
	}
}

// TestNativeValueWrongType asks for a type the payload is not, and requires
// both a false and a zero value — a caller that ignores ok must not get
// something usable.
func TestNativeValueWrongType(t *testing.T) {
	v := lisp.NativeOf[*handle](&handle{name: "conn"})
	got, ok := lisp.NativeValue[*other](v)
	if ok {
		t.Error("NativeValue[*other] matched a *handle payload")
	}
	if got != nil {
		t.Errorf("NativeValue returned %v on failure, want the zero value", got)
	}
	gotVal, ok := lisp.NativeValue[point](v)
	if ok {
		t.Error("NativeValue[point] matched a *handle payload")
	}
	if gotVal != (point{}) {
		t.Errorf("NativeValue returned %+v on failure, want the zero value", gotVal)
	}
	// The other direction, so the test cannot pass by refusing everything:
	// the same two type parameters against a payload of the other type.
	w := lisp.NativeOf[*other](&other{id: "xyz"})
	if _, ok := lisp.NativeValue[*handle](w); ok {
		t.Error("NativeValue[*handle] matched an *other payload")
	}
	o, ok := lisp.NativeValue[*other](w)
	if !ok {
		t.Fatal("NativeValue[*other] returned ok=false for an *other payload")
	}
	if o.id != "xyz" {
		t.Errorf("retrieved payload has id %q, want %q", o.id, "xyz")
	}
}

// TestNativeValueNilLVal covers the first gate.  A nil *LVal reaches this
// accessor whenever a caller forwards an un-checked result, and it must
// answer false rather than panic.
func TestNativeValueNilLVal(t *testing.T) {
	got, ok := lisp.NativeValue[*handle](nil)
	if ok {
		t.Error("NativeValue returned ok=true for a nil *LVal")
	}
	if got != nil {
		t.Errorf("NativeValue returned %v for a nil *LVal, want nil", got)
	}
	if _, ok := lisp.NativeValue[any](nil); ok {
		t.Error("NativeValue[any] returned ok=true for a nil *LVal")
	}
}

// TestNativeValueNilPayload covers an LNative whose payload is nil.  The
// LVal passes the first two gates; the assertion is what refuses it,
// including for T = any, because a type assertion on a nil interface never
// succeeds.
func TestNativeValueNilPayload(t *testing.T) {
	v := lisp.Native(nil)
	if v.Type != lisp.LNative {
		t.Fatalf("Native(nil) produced type %v, want %v", v.Type, lisp.LNative)
	}
	if _, ok := lisp.NativeValue[any](v); ok {
		t.Error("NativeValue[any] returned ok=true for a nil payload")
	}
	got, ok := lisp.NativeValue[*handle](v)
	if ok {
		t.Error("NativeValue[*handle] returned ok=true for a nil payload")
	}
	if got != nil {
		t.Errorf("NativeValue returned %v for a nil payload, want nil", got)
	}
}

// TestNativeValueRefusesInternalStorage is the reason the type gate runs
// before the payload assertion (issue #546).  Each subtest builds a real
// lisp value of a type whose backing the interpreter keeps in LVal.Native,
// proves the payload is there, and then requires NativeValue to refuse it.
//
// Removing the v.Type == LNative gate makes every subtest here fail: the
// assertions all succeed on the raw payload, which is exactly the leak —
// an embedder holding a lisp bytes value's *[]byte, a sorted map's
// *MapData, or an error's *CallStack can write through it and corrupt a
// live value past the kernel's ownership and seal invariants.
func TestNativeValueRefusesInternalStorage(t *testing.T) {
	t.Run("LBytes backing", func(t *testing.T) {
		for _, v := range []*lisp.LVal{
			lisp.Bytes([]byte("ABCD")),
			mustEvalNative(t, `(to-bytes "ABCD")`),
		} {
			if v.Type != lisp.LBytes {
				t.Fatalf("value has type %v, want %v", v.Type, lisp.LBytes)
			}
			// Positive control: the *[]byte really is in Native, so a
			// false below is the gate and not an empty payload.
			if got := string(v.Bytes()); got != "ABCD" {
				t.Fatalf("bytes payload is %q, want %q", got, "ABCD")
			}
			b, ok := lisp.NativeValue[*[]byte](v)
			if ok {
				t.Errorf("NativeValue[*[]byte] exposed the backing of a lisp bytes value: %q", string(*b))
			}
			if b != nil {
				t.Errorf("NativeValue returned %v for a bytes value, want nil", b)
			}
			// The same leak asked for through `any`.
			if x, ok := lisp.NativeValue[any](v); ok {
				t.Errorf("NativeValue[any] exposed a bytes value's payload: %T", x)
			}
		}
	})

	t.Run("LSortMap backing", func(t *testing.T) {
		for _, v := range []*lisp.LVal{
			lisp.SortedMap(),
			mustEvalNative(t, `(sorted-map "k" 1)`),
		} {
			if v.Type != lisp.LSortMap {
				t.Fatalf("value has type %v, want %v", v.Type, lisp.LSortMap)
			}
			// Positive control: the *MapData really is in Native.
			if v.Map() == nil {
				t.Fatal("sorted-map has no MapData payload")
			}
			md, ok := lisp.NativeValue[*lisp.MapData](v)
			if ok {
				t.Error("NativeValue[*lisp.MapData] exposed the backing of a lisp sorted-map")
			}
			if md != nil {
				t.Errorf("NativeValue returned %v for a sorted-map, want nil", md)
			}
			if x, ok := lisp.NativeValue[any](v); ok {
				t.Errorf("NativeValue[any] exposed a sorted-map's payload: %T", x)
			}
		}
	})

	t.Run("LError call stack", func(t *testing.T) {
		env := nativeTestEnv(t)
		v := env.LoadString("native_test.lisp", `(car 1)`)
		if v.Type != lisp.LError {
			t.Fatalf("evaluating (car 1) produced %v, want an error", v.Type)
		}
		// Positive control: the *CallStack really is in Native.  It is
		// also what IsInternalPanic keys off, so handing it out lets an
		// embedder forge the interpreter's own panic marker.
		if v.CallStack() == nil {
			t.Fatal("error value carries no call stack")
		}
		stack, ok := lisp.NativeValue[*lisp.CallStack](v)
		if ok {
			t.Error("NativeValue[*lisp.CallStack] exposed a lisp error's call stack")
		}
		if stack != nil {
			t.Errorf("NativeValue returned %v for an error value, want nil", stack)
		}
		if x, ok := lisp.NativeValue[any](v); ok {
			t.Errorf("NativeValue[any] exposed an error value's payload: %T", x)
		}
	})
}

// TestNativeValueGatesOnLValTypeNotPayloadType is the other half of the
// contract.  The gate refuses the interpreter's storage, not a Go type: an
// embedder who stores a *[]byte of their own gets it back, because that
// value is an LNative and the slice is theirs.
func TestNativeValueGatesOnLValTypeNotPayloadType(t *testing.T) {
	b := []byte("mine")
	v := lisp.Native(&b)
	got, ok := lisp.NativeValue[*[]byte](v)
	if !ok {
		t.Fatalf("NativeValue[*[]byte] refused an embedder's own *[]byte in an LNative")
	}
	if got != &b {
		t.Fatalf("NativeValue returned %p, want the stored instance %p", got, &b)
	}
	// It is the caller's own slice, so writing through it is theirs to do.
	(*got)[0] = 'M'
	if string(b) != "Mine" {
		t.Errorf("write through the retrieved pointer did not reach the original: %q", string(b))
	}
	// Same payload type stored via NativeOf.
	if _, ok := lisp.NativeValue[*[]byte](lisp.NativeOf(&b)); !ok {
		t.Error("NativeValue[*[]byte] refused a NativeOf-stored *[]byte")
	}
	// And an embedder's own *MapData in an LNative is likewise theirs.
	md := lisp.NewMapData(nil)
	if _, ok := lisp.NativeValue[*lisp.MapData](lisp.NativeOf(md)); !ok {
		t.Error("NativeValue[*lisp.MapData] refused an embedder's own MapData in an LNative")
	}
}

// RequireNative shares NativeValue's three gates and adds only the failure
// message, so the tests below pin the messages in full.  A substring match
// would pass on a message that named the wrong gate or omitted the expected
// type, which is the whole content of the addition.
//
// Message renderings are reflect's, not fmt's %T: an unexported type in this
// package renders as *lisp_test.handle, and []byte renders through its
// element's underlying name as []uint8.

// requireNativeErrorMessage asserts lerr is an LError and returns its bare
// message, without the source-location prefix Error() adds.
func requireNativeErrorMessage(t *testing.T, lerr *lisp.LVal) string {
	t.Helper()
	if lerr == nil {
		t.Fatal("RequireNative returned a nil error on a failing call")
	}
	if lerr.Type != lisp.LError {
		t.Fatalf("RequireNative returned type %v, want %v", lerr.Type, lisp.LError)
	}
	var ev *lisp.ErrorVal
	if !errors.As(lisp.GoError(lerr), &ev) {
		t.Fatalf("RequireNative error does not convert to *ErrorVal: %v", lerr)
	}
	return ev.ErrorMessage()
}

// TestRequireNativeSuccess is the nil-on-success half of the contract: a
// matching payload comes back with a nil second return, through both
// constructors.
func TestRequireNativeSuccess(t *testing.T) {
	h := &handle{name: "conn", seq: 7}
	for _, v := range []*lisp.LVal{lisp.Native(h), lisp.NativeOf[*handle](h)} {
		got, lerr := lisp.RequireNative[*handle](v)
		if lerr != nil {
			t.Fatalf("RequireNative[*handle] failed on a *handle payload: %v", lerr)
		}
		if got != h {
			t.Errorf("RequireNative returned %p, want the stored instance %p", got, h)
		}
	}
	// A value type travels by copy, as with NativeValue.
	p := point{X: 3, Y: 4}
	gotVal, lerr := lisp.RequireNative[point](lisp.NativeOf(p))
	if lerr != nil {
		t.Fatalf("RequireNative[point] failed on a point payload: %v", lerr)
	}
	if gotVal != p {
		t.Errorf("RequireNative returned %+v, want %+v", gotVal, p)
	}
}

// TestRequireNativeThroughInterface asserts an interface type parameter
// reaches a payload stored as a concrete type, since the gate it inherits is
// an ordinary type assertion.
func TestRequireNativeThroughInterface(t *testing.T) {
	h := &handle{name: "iface", seq: 2}
	s, lerr := lisp.RequireNative[fmt.Stringer](lisp.NativeOf[*handle](h))
	if lerr != nil {
		t.Fatalf("RequireNative[fmt.Stringer] failed on a *handle payload: %v", lerr)
	}
	if s.String() != h.String() {
		t.Errorf("Stringer returned %q, want %q", s.String(), h.String())
	}
	// The message for an interface T names the interface.  This is what a
	// %T verb over the zero T cannot do: it renders <nil>.
	_, lerr = lisp.RequireNative[fmt.Stringer](nil)
	if msg := requireNativeErrorMessage(t, lerr); msg != "expected native fmt.Stringer value, got nil" {
		t.Errorf("message = %q, want the interface named", msg)
	}
}

// TestRequireNativeNilLVal covers the first gate.  A nil *LVal reaches this
// accessor whenever a caller forwards an un-checked result.
func TestRequireNativeNilLVal(t *testing.T) {
	got, lerr := lisp.RequireNative[*handle](nil)
	if msg := requireNativeErrorMessage(t, lerr); msg != "expected native *lisp_test.handle value, got nil" {
		t.Errorf("message = %q", msg)
	}
	if got != nil {
		t.Errorf("RequireNative returned %v on failure, want the zero value", got)
	}
	gotVal, lerr := lisp.RequireNative[point](nil)
	if msg := requireNativeErrorMessage(t, lerr); msg != "expected native lisp_test.point value, got nil" {
		t.Errorf("message = %q", msg)
	}
	if gotVal != (point{}) {
		t.Errorf("RequireNative returned %+v on failure, want the zero value", gotVal)
	}
}

// TestRequireNativeRefusesInternalStorage is the load-bearing case.  Each
// subject is a real lisp value whose backing the interpreter keeps in
// LVal.Native, and each is refused by the SECOND gate — so the message names
// the lisp type found, and the payload never leaves.  An implementation that
// asserted on v.Native without checking v.Type would return the interpreter's
// live storage here with a nil error.
func TestRequireNativeRefusesInternalStorage(t *testing.T) {
	t.Run("LBytes backing", func(t *testing.T) {
		for _, v := range []*lisp.LVal{
			lisp.Bytes([]byte("ABCD")),
			mustEvalNative(t, `(to-bytes "ABCD")`),
		} {
			// Positive control: the *[]byte really is in Native, so the
			// failure below is the gate and not an empty payload.
			if got := string(v.Bytes()); got != "ABCD" {
				t.Fatalf("bytes payload is %q, want %q", got, "ABCD")
			}
			b, lerr := lisp.RequireNative[*[]byte](v)
			if b != nil {
				t.Errorf("RequireNative[*[]byte] exposed the backing of a lisp bytes value: %q", string(*b))
			}
			if msg := requireNativeErrorMessage(t, lerr); msg != "expected native *[]uint8 value, got bytes" {
				t.Errorf("message = %q", msg)
			}
			// The same leak asked for through `any`.
			x, lerr := lisp.RequireNative[any](v)
			if x != nil {
				t.Errorf("RequireNative[any] exposed a bytes value's payload: %T", x)
			}
			if msg := requireNativeErrorMessage(t, lerr); msg != "expected native interface {} value, got bytes" {
				t.Errorf("message = %q", msg)
			}
		}
	})

	t.Run("LSortMap backing", func(t *testing.T) {
		v := mustEvalNative(t, `(sorted-map "k" 1)`)
		if v.Map() == nil {
			t.Fatal("sorted-map has no MapData payload")
		}
		md, lerr := lisp.RequireNative[*lisp.MapData](v)
		if md != nil {
			t.Error("RequireNative[*lisp.MapData] exposed the backing of a lisp sorted-map")
		}
		if msg := requireNativeErrorMessage(t, lerr); msg != "expected native *lisp.MapData value, got sorted-map" {
			t.Errorf("message = %q", msg)
		}
	})

	t.Run("LError call stack", func(t *testing.T) {
		env := nativeTestEnv(t)
		v := env.LoadString("native_test.lisp", `(car 1)`)
		if v.Type != lisp.LError {
			t.Fatalf("evaluating (car 1) produced %v, want an error", v.Type)
		}
		// Positive control: the *CallStack really is in Native.  It is also
		// what IsInternalPanic keys off.
		if v.CallStack() == nil {
			t.Fatal("error value carries no call stack")
		}
		stack, lerr := lisp.RequireNative[*lisp.CallStack](v)
		if stack != nil {
			t.Error("RequireNative[*lisp.CallStack] exposed a lisp error's call stack")
		}
		if msg := requireNativeErrorMessage(t, lerr); msg != "expected native *lisp.CallStack value, got error" {
			t.Errorf("message = %q", msg)
		}
	})
}

// TestRequireNativeWrongPayloadType covers the third gate: the LVal is an
// LNative, so the message names both the type asked for and the type stored.
func TestRequireNativeWrongPayloadType(t *testing.T) {
	v := lisp.NativeOf[*handle](&handle{name: "conn"})
	got, lerr := lisp.RequireNative[*other](v)
	if got != nil {
		t.Errorf("RequireNative returned %v on failure, want the zero value", got)
	}
	if msg := requireNativeErrorMessage(t, lerr); msg != "expected native *lisp_test.other value, got native *lisp_test.handle" {
		t.Errorf("message = %q", msg)
	}
	// The other direction, so the test cannot pass by refusing everything.
	w := lisp.NativeOf[*other](&other{id: "xyz"})
	o, lerr := lisp.RequireNative[*other](w)
	if lerr != nil {
		t.Fatalf("RequireNative[*other] failed on an *other payload: %v", lerr)
	}
	if o.id != "xyz" {
		t.Errorf("retrieved payload has id %q, want %q", o.id, "xyz")
	}
	// An embedder's own *[]byte in an LNative is theirs, and reaching it is
	// not a wrong-payload failure: the gate is on the LVal's type.
	b := []byte("mine")
	if _, lerr := lisp.RequireNative[*[]byte](lisp.Native(&b)); lerr != nil {
		t.Errorf("RequireNative[*[]byte] refused an embedder's own *[]byte in an LNative: %v", lerr)
	}
}

// TestRequireNativeNilPayload covers an LNative carrying nothing.  It passes
// the first two gates and fails the assertion, including for T = any, so the
// message is the third one with a <nil> payload type.
func TestRequireNativeNilPayload(t *testing.T) {
	v := lisp.Native(nil)
	if v.Type != lisp.LNative {
		t.Fatalf("Native(nil) produced type %v, want %v", v.Type, lisp.LNative)
	}
	got, lerr := lisp.RequireNative[*handle](v)
	if got != nil {
		t.Errorf("RequireNative returned %v for a nil payload, want nil", got)
	}
	if msg := requireNativeErrorMessage(t, lerr); msg != "expected native *lisp_test.handle value, got native <nil>" {
		t.Errorf("message = %q", msg)
	}
	if _, lerr := lisp.RequireNative[any](v); lerr == nil {
		t.Error("RequireNative[any] succeeded on a nil payload")
	}
}

// mustEvalNative evaluates src in a fresh environment and fails the test on
// an error result.  It exists so the internal-storage tests can build their
// subjects the way lisp programs do, not only through the Go constructors.
func mustEvalNative(t *testing.T, src string) *lisp.LVal {
	t.Helper()
	env := nativeTestEnv(t)
	v := env.LoadString("native_test.lisp", src)
	if v.Type == lisp.LError {
		t.Fatalf("eval %q: %v", src, v)
	}
	return v
}
