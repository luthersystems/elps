// Package nativepaired is the ANALYZER half of
// TestNativePayloadAnalyzerMirrorsTemplateAdmission
// (cmd/elpsvet/nativepayload_runtime_test.go).  Every function here spells
// one construction, and that test pairs each with the SAME construction run
// through a real lisp.NewTemplate in-process: the analyzer's verdict on the
// source and publication's verdict on the value have to agree, case by case.
//
// The rule exists because publication is the expensive place to find out.  A
// static gate that is quieter than the runtime lets a payload through review
// and fails in whatever request first publishes the value; a gate that is
// louder makes authors annotate what the runtime already admits.  Three
// constructions were in the first category, which is why they are the first
// three here:
//
//   - PairUintptr: the basic tier once passed every basic kind but
//     unsafe.Pointer, and reflect.Uintptr is on the runtime's REFUSED arm;
//   - PairNestedLValSlice: the lisp.Value fall-through check had a
//     []*LVal fast path that looked through one pointer too many, so
//     []**LVal -- which Value has no arm for -- read as Value's own arm;
//   - PairByteHeader: the *[]byte allowlist row exempted the payload TYPE at
//     every spelling, but a constructor always builds an LNative header,
//     whose payload templateInventory.val hands to native(), which refuses
//     it.  The row is true only where the KERNEL writes its own storage.
//   - PairByteFieldWrite and PairNativeHeaderLiteral: the same row still
//     exempted the two KERNEL-SLOT spellings by shape alone, so a *[]byte
//     stored onto an LNative header -- through the field, or in a literal
//     that names Type: LNative outright -- was silently exempt while val
//     routed it to native() all the same.  A row is a claim about a HEADER:
//     it exempts a keyed literal in package lisp whose Type key names the
//     row's own header, and nothing else.
//
// The last two are the positive controls, without which a rule tightened
// until it reported everything would look identical to a rule that mirrors
// the runtime.
//
// It lives under github.com/luthersystems/elps/ so it can import
// internal/templatepolicy for the marker control, the same reason the
// nativemarker fixture does.  Note that these fixtures compile against the
// stub lisp package in testdata, while the paired runtime half uses the real
// one -- the stub's Native, NativeOf and Value mirror the real signatures
// and the real type switch, and the fixtures for lisp.Value's arms
// (nativepayload.valueDirect, nativepayload.valueNotQuiteAnArm) are what
// keeps that mirror honest.
//
// analysistest checks absence as strictly as presence: a construction with
// no want-expectation comment asserts NO diagnostic there.
package nativepaired

import (
	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
)

// ownedStamp is the marker tier's shape: a struct VALUE embedding
// templatepolicy.Marker.  It stands in for libtime's ownedTime, which the
// runtime half publishes -- an unexported type in another package, which no
// fixture can spell.  What the two share is the only thing the tier reads:
// the marker on a struct value.
type ownedStamp struct {
	templatepolicy.Marker
	n int64
}

// PairUintptr: reflect.Uintptr is named in the arm templateInventory.native
// REFUSES, so publication rejects this payload.
func PairUintptr() *lisp.LVal {
	return lisp.Native(uintptr(1)) // want `lisp\.Native payload type uintptr is not a known-safe value type`
}

// PairNestedLValSlice: lisp.Value has a `case []*LVal` arm and no
// `case []**LVal`, so this falls through to Native and publication sees an
// opaque slice payload.
func PairNestedLValSlice() *lisp.LVal {
	return lisp.Value([]**lisp.LVal{}) // want `lisp\.Value payload type \[\]\*\*lisp\.LVal is not a known-safe value type`
}

// PairByteHeader is the construction as a caller writes it.  lisp.Bytes
// builds the same payload as a keyed literal on an LBytes header, which
// templateInventory.val routes to its own byte-span arm; this one is an
// LNative, and val hands an LNative's payload to native().
func PairByteHeader() *lisp.LVal {
	b := []byte{1}
	return lisp.Native(&b) // want `lisp\.Native payload type \*\[\]byte is a kernel representation slot`
}

// PairByteFieldWrite stores an allowlisted row payload onto a header that is
// already an LNative.  The field write shows no header at all, which is why
// the tier trusts one only inside package lisp -- and this package is not
// package lisp.
func PairByteFieldWrite() *lisp.LVal {
	b := []byte{1}
	v := lisp.Native(int64(0))
	v.Native = &b // want `LVal\.Native assignment payload type \*\[\]byte is a kernel representation slot`
	return v
}

// PairNativeHeaderLiteral is the same payload in the kernel's own literal
// shape, with the Type key spelling out the one header the row is NOT about.
// It was exempt before the narrowing because the spelling was a kernel slot;
// publication refused it because val hands an LNative's payload to native().
func PairNativeHeaderLiteral() *lisp.LVal {
	b := []byte{1}
	return &lisp.LVal{Type: lisp.LNative, Native: &b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// PairScalarControl is the runtime's scalar arm: reflect.Int64.
func PairScalarControl() *lisp.LVal {
	return lisp.Native(int64(1))
}

// PairMarkerControl is the runtime's marker arm: a struct value whose method
// set carries templatepolicy.Immutable's unexported method.
func PairMarkerControl() *lisp.LVal {
	return lisp.Native(ownedStamp{n: 1})
}
