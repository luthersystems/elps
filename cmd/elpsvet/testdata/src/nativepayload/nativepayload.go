// Package nativepayload exercises the elpsnativepayload analyzer: every
// construction spelling, the basic-type tier, the kernel-slot allowlist, the
// diagnostic-stack ban, the interface-typed report, and the allow marker with
// and without a justification.  The templatepolicy.Marker tier and the
// NativeCloner-only reports live in the nativemarker fixture, which is under
// github.com/luthersystems/elps/ so it can import the internal package.
//
// analysistest checks absence as strictly as presence: a construction with
// no want-expectation comment asserts NO diagnostic there.
package nativepayload

import (
	"regexp"
	"time"
	"unsafe"

	"github.com/luthersystems/elps/lisp"
	l "github.com/luthersystems/elps/lisp"
)

// handle is the plausibly-mutable payload every reported shape uses.
type handle struct{ n int }

// counter has a basic underlying type, so a value of it is immutable inside
// an interface.
type counter int

// blob is a defined type over []byte: lisp.Value's `case []byte` does not
// match it, so it falls through to Native.
type blob []byte

// address is a defined type over uintptr, whose reflect.Kind is Uintptr --
// the kind the runtime's scalar arm names in the half it REFUSES.
type address uintptr

// suite declares lisp.NativeCloner on its POINTER receiver.  Under the
// template contract that is not an exemption: NewTemplate rejects mutable
// payloads including NativeCloner implementations (lisp/native.go), so every
// construction below is reported.
type suite struct{ tests []string }

func (s *suite) CloneNative() interface{} { return &suite{tests: append([]string(nil), s.tests...)} }

var _ lisp.NativeCloner = (*suite)(nil)

// valueCloner declares it on the VALUE receiver, so both a value and a
// pointer satisfy the protocol -- and neither is exempt.
type valueCloner struct{ n int }

func (valueCloner) CloneNative() interface{} { return valueCloner{} }

// wrongCloner has a method of the right name and the wrong shape.  It is
// reported like every other unmarked pointer; it is kept as a fixture so the
// rule never grows a CloneNative tier back by accident.
type wrongCloner struct{ n int }

func (*wrongCloner) CloneNative(deep bool) interface{} { return nil }

// notAnLVal has a field named Native that is nothing of the kind.
type notAnLVal struct{ Native *handle }

// --- the spellings ---------------------------------------------------------

func native(h *handle) *lisp.LVal {
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func nativeOfInferred(h *handle) *lisp.LVal {
	return lisp.NativeOf(h) // want `lisp\.NativeOf payload type \*nativepayload\.handle is not a known-safe value type`
}

func nativeOfExplicit(h *handle) *lisp.LVal {
	return lisp.NativeOf[*handle](h) // want `lisp\.NativeOf payload type \*nativepayload\.handle is not a known-safe value type`
}

func nativeAliasedImport(h *handle) *lisp.LVal {
	return l.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func nativeParenthesised(h *handle) *lisp.LVal {
	return (lisp.Native)(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func valueFallthrough(h *handle) *lisp.LVal {
	return lisp.Value(h) // want `lisp\.Value payload type \*nativepayload\.handle is not a known-safe value type`
}

func valueNamedBytes(b blob) *lisp.LVal {
	return lisp.Value(b) // want `lisp\.Value payload type nativepayload\.blob is not a known-safe value type`
}

// valueNotQuiteAnArm holds the shapes that LOOK like one of lisp.Value's
// arms and are not, so Value falls through to Native and publication then
// sees an opaque payload.  `[]**LVal` is the one that used to slip through:
// the old isLValType looked through a pointer, so it read the extra layer as
// Value's `[]*LVal` arm and exempted a construction the runtime rejects
// (paired with TestNativePayloadAnalyzerMirrorsTemplateAdmission).
func valueNotQuiteAnArm(deep []**lisp.LVal, errs []*lisp.ErrorVal, vals []lisp.LVal, nested [][]*lisp.LVal) {
	_ = lisp.Value(deep)   // want `lisp\.Value payload type \[\]\*\*lisp\.LVal is not a known-safe value type`
	_ = lisp.Value(errs)   // want `lisp\.Value payload type \[\]\*lisp\.ErrorVal is not a known-safe value type`
	_ = lisp.Value(vals)   // want `lisp\.Value payload type \[\]lisp\.LVal is not a known-safe value type`
	_ = lisp.Value(nested) // want `lisp\.Value payload type \[\]\[\]\*lisp\.LVal is not a known-safe value type`
	// Native never had a Value arm to be confused by: the same slice is
	// reported through the plain constructor too.
	_ = lisp.Native(deep) // want `lisp\.Native payload type \[\]\*\*lisp\.LVal is not a known-safe value type`
}

// valueDirect covers every arm of lisp.Value's type switch: none of these
// falls through to Native, so none is a construction.
func valueDirect(s string, b []byte, i int, f float64, ok bool, cells []*lisp.LVal) {
	_ = lisp.Value(s)
	_ = lisp.Value(b)
	_ = lisp.Value(i)
	_ = lisp.Value(f)
	_ = lisp.Value(ok)
	_ = lisp.Value(cells)
	_ = lisp.Value("literal")
	_ = lisp.Value(42)
}

func literalPointer(h *handle) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LError, Native: h} // want `LVal\.Native literal payload type \*nativepayload\.handle is not a known-safe value type`
}

func literalValue(h *handle) lisp.LVal {
	return lisp.LVal{Native: h} // want `LVal\.Native literal payload type \*nativepayload\.handle is not a known-safe value type`
}

func literalOtherFields(cells []*lisp.LVal) *lisp.LVal {
	return &lisp.LVal{Cells: cells, Str: "no payload"}
}

func fieldWrite(v *lisp.LVal, h *handle) {
	v.Native = h // want `LVal\.Native assignment payload type \*nativepayload\.handle is not a known-safe value type`
}

func fieldWriteThroughValue(v lisp.LVal, h *handle) lisp.LVal {
	v.Native = h // want `LVal\.Native assignment payload type \*nativepayload\.handle is not a known-safe value type`
	return v
}

func fieldWriteNotAnLVal(o *notAnLVal, h *handle) {
	o.Native = h
}

// --- the basic tier --------------------------------------------------------

func basics(s string, i int, f float64, ok bool, c counter, r rune) {
	_ = lisp.Native(s)
	_ = lisp.Native(i)
	_ = lisp.Native(f)
	_ = lisp.Native(ok)
	_ = lisp.Native(c)
	_ = lisp.Native(r)
	_ = lisp.Native(nil)
	_ = lisp.NativeOf(c)
	_ = lisp.NativeOf[counter](c)
	_ = &lisp.LVal{Native: c}
}

// widerScalars pins the rest of runtimeScalarKinds against the runtime's
// own reflect.Kind list: every kind the runtime names in its admitting arm
// is exempt here, so an author never annotates what publication admits.
func widerScalars(i8 int8, i16 int16, i32 int32, i64 int64,
	u uint, u8 uint8, u16 uint16, u32 uint32, u64 uint64,
	f32 float32, c64 complex64, c128 complex128) {
	_ = lisp.Native(i8)
	_ = lisp.Native(i16)
	_ = lisp.Native(i32)
	_ = lisp.Native(i64)
	_ = lisp.Native(u)
	_ = lisp.Native(u8)
	_ = lisp.Native(u16)
	_ = lisp.Native(u32)
	_ = lisp.Native(u64)
	_ = lisp.Native(f32)
	_ = lisp.Native(c64)
	_ = lisp.Native(c128)
}

// addressesWearingBasicClothes is the pair of basic KINDS the runtime's
// scalar arm refuses by name (reflect.Uintptr, reflect.UnsafePointer).  Both
// have a *types.Basic underlying type, so a tier that merely asked "is the
// underlying type basic" exempted a payload publication rejects -- which is
// exactly what uintptr did before runtimeScalarKinds spelled the runtime's
// list out.  Each is an address a VM can convert back through unsafe and
// follow to whatever the publisher was pointing at.
func addressesWearingBasicClothes(p unsafe.Pointer, u uintptr, a address) {
	_ = lisp.Native(p) // want `lisp\.Native payload type unsafe\.Pointer is not a known-safe value type`
	_ = lisp.Native(u) // want `lisp\.Native payload type uintptr is not a known-safe value type`
	// A DEFINED type over uintptr is the same kind and the same hazard: the
	// tier reads the underlying type, and reflect.Kind does too.
	_ = lisp.Native(a)                   // want `lisp\.Native payload type nativepayload\.address is not a known-safe value type`
	_ = lisp.NativeOf(u)                 // want `lisp\.NativeOf payload type uintptr is not a known-safe value type`
	_ = lisp.Value(u)                    // want `lisp\.Value payload type uintptr is not a known-safe value type`
	_ = &lisp.LVal{Native: u}            // want `LVal\.Native literal payload type uintptr is not a known-safe value type`
	_ = &lisp.LVal{Native: uintptr(0x1)} // want `LVal\.Native literal payload type uintptr is not a known-safe value type`
}

func composites(m map[string]int, sl []int, ch chan int, fn func(), arr [2]int, st struct{ n int }) {
	_ = lisp.Native(m)   // want `lisp\.Native payload type map\[string\]int is not a known-safe value type`
	_ = lisp.Native(sl)  // want `lisp\.Native payload type \[\]int is not a known-safe value type`
	_ = lisp.Native(ch)  // want `lisp\.Native payload type chan int is not a known-safe value type`
	_ = lisp.Native(fn)  // want `lisp\.Native payload type func\(\) is not a known-safe value type`
	_ = lisp.Native(arr) // want `lisp\.Native payload type \[2\]int is not a known-safe value type`
	_ = lisp.Native(st)  // want `lisp\.Native payload type struct\{n int\} is not a known-safe value type`
}

// --- NativeCloner is no longer a tier ---------------------------------------

func cloner(s *suite) {
	_ = lisp.Native(s)           // want `lisp\.Native payload type \*nativepayload\.suite is not a known-safe value type`
	_ = lisp.NativeOf[*suite](s) // want `lisp\.NativeOf payload type \*nativepayload\.suite is not a known-safe value type`
	_ = &lisp.LVal{Native: s}    // want `LVal\.Native literal payload type \*nativepayload\.suite is not a known-safe value type`
}

func clonerValueOfPointerReceiver(s suite) *lisp.LVal {
	return lisp.Native(s) // want `lisp\.Native payload type nativepayload\.suite is not a known-safe value type`
}

func clonerValueReceiver(v valueCloner) {
	_ = lisp.Native(v)  // want `lisp\.Native payload type nativepayload\.valueCloner is not a known-safe value type`
	_ = lisp.Native(&v) // want `lisp\.Native payload type \*nativepayload\.valueCloner is not a known-safe value type`
}

func clonerWrongShape(w *wrongCloner) *lisp.LVal {
	return lisp.Native(w) // want `lisp\.Native payload type \*nativepayload\.wrongCloner is not a known-safe value type`
}

// --- the audited allowlist ---------------------------------------------------

// kernelSlotsOutsideTheKernel is the whole point of the narrowing: this
// package is NOT github.com/luthersystems/elps/lisp, and the allowlist rows
// describe the kernel's own representation storage.  Every spelling here is
// reported, the right Type key included -- an embedder holding an *LVal has
// no business building an LBytes header by hand, and if it does, it is
// building a header the kernel's own constructors already build correctly.
//
// The in-kernel positive controls -- the same literals with the same Type
// keys, exempt because the package under analysis IS lisp -- live in
// testdata/nativelisp and are run by TestNativePayloadAnalyzerInKernelPackage.
func kernelSlotsOutsideTheKernel(v *lisp.LVal, b *[]byte, m *lisp.MapData) {
	_ = &lisp.LVal{Native: b}                      // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
	_ = &lisp.LVal{Type: lisp.LBytes, Native: b}   // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
	_ = &lisp.LVal{Type: lisp.LSortMap, Native: m} // want `LVal\.Native literal payload type \*lisp\.MapData is a kernel representation slot`
	v.Native = b                                   // want `LVal\.Native assignment payload type \*\[\]byte is a kernel representation slot`
	v.Native = m                                   // want `LVal\.Native assignment payload type \*lisp\.MapData is a kernel representation slot`
}

// nativeHeaderLiteral is the shape the second adversarial review named: a
// keyed literal is a kernel-slot SPELLING, but the header it builds is
// LNative, which is exactly the arm templateInventory.val hands to native().
// Before the narrowing this was exempt statically and refused at
// publication; the paired fixture spells it again beside a real
// lisp.NewTemplate (nativepaired.PairNativeHeaderLiteral).
func nativeHeaderLiteral(b *[]byte) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LNative, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// kernelSlotsThroughAConstructor is the SAME payload types through the
// constructor spellings, and they are reported.  lisp.Native, lisp.NativeOf
// and a falling-through lisp.Value all build an LNative header, and
// templateInventory.val's LNative arm hands the payload straight to
// native(), which knows nothing about kernel storage and refuses a *[]byte
// or a *MapData like any other pointer.  Exempting these by TYPE is what let
// `b := []byte{1}; lisp.Native(&b)` pass a static gate the runtime fails
// (paired with TestNativePayloadAnalyzerMirrorsTemplateAdmission).
//
// *lisp.funData, the third row, cannot appear here: it is unexported, so no
// package but lisp can spell a construction of one, and inside lisp the only
// spellings are keyed literals.
func kernelSlotsThroughAConstructor(b *[]byte, m *lisp.MapData) {
	_ = lisp.Native(b)   // want `lisp\.Native payload type \*\[\]byte is a kernel representation slot`
	_ = lisp.Native(m)   // want `lisp\.Native payload type \*lisp\.MapData is a kernel representation slot`
	_ = lisp.NativeOf(b) // want `lisp\.NativeOf payload type \*\[\]byte is a kernel representation slot`
	_ = lisp.Value(b)    // want `lisp\.Value payload type \*\[\]byte is a kernel representation slot`
}

// bytesAddressLocal is the reviewer's exact reproduction, spelled as a
// caller would write it rather than as a parameter type.
func bytesAddressLocal() *lisp.LVal {
	b := []byte{1}
	return lisp.Native(&b) // want `lisp\.Native payload type \*\[\]byte is a kernel representation slot`
}

// kernelSlotConstructorAnnotated pins that the site annotation is still the
// way out when an author can say why the value never reaches a template --
// the rows narrowing does not remove the escape hatch, it removes the
// SILENT exemption.
func kernelSlotConstructorAnnotated(b *[]byte) *lisp.LVal {
	return lisp.Native(b) //elpsvet:allow-native fixture: a scratch header the caller discards before any template could publish it
}

// notRows covers the types the port allowlisted and the re-audit dropped:
// time.Time and *regexp.Regexp are refused by publication itself (the marker
// tier and lisp/lisplib/template_natives_test.go), and a *lisp.CallStack is
// banned outright by checkDiagnosticPayload.
func notRows(re *regexp.Regexp, t time.Time, d time.Duration, s *lisp.CallStack) {
	_ = lisp.Native(re)       // want `lisp\.Native payload type \*regexp\.Regexp is not a known-safe value type`
	_ = lisp.Native(t)        // want `lisp\.Native payload type time\.Time is not a known-safe value type`
	_ = lisp.Native(d)        // a defined type over int64: the basic tier, not a row
	_ = lisp.Value(re)        // want `lisp\.Value payload type \*regexp\.Regexp is not a known-safe value type`
	_ = lisp.Native(s)        // want `lisp\.Native payload type \*lisp\.CallStack is a retained diagnostic stack`
	_ = lisp.Native(*s)       // want `lisp\.Native payload type lisp\.CallStack is a retained diagnostic stack`
	_ = &lisp.LVal{Native: s} // want `LVal\.Native literal payload type \*lisp\.CallStack is a retained diagnostic stack`
}

func callStackFieldWrite(v *lisp.LVal, s *lisp.CallStack) {
	v.Native = s // want `LVal\.Native assignment payload type \*lisp\.CallStack is a retained diagnostic stack`
}

// errorPayload is an interface, so the rule reports it as unclassifiable
// rather than allowlisting it: publication reads a native's DYNAMIC type,
// and the kernel's own error cells carry site annotations instead.
func errorPayload(err error) *lisp.LVal {
	return lisp.Native(err) // want `lisp\.Native payload type error is not statically known`
}

func kernelSlotsByValueNotPointer(b []byte, m lisp.MapData) {
	// The rows are keyed on the exact type, pointer-ness included.  A []byte
	// is lisp.Value's own arm, but a native construction of one is not a row.
	_ = lisp.Native(b) // want `lisp\.Native payload type \[\]byte is not a known-safe value type`
	_ = lisp.Native(m) // want `lisp\.Native payload type lisp\.MapData is not a known-safe value type`
}

// --- the positive control ----------------------------------------------------

// stillPasses is the negative-control test's other half: the constructions
// publication ADMITS must keep passing, or a rule tightened until it reports
// everything would look identical to a rule that mirrors the runtime.
// lisp.Native(int64(1)) is the runtime's scalar arm; the marker tier's
// positive control is markedValue in the nativemarker fixture, which has to
// live under the module path to import internal/templatepolicy.  Both are
// asserted against a real lisp.NewTemplate in
// TestNativePayloadAnalyzerMirrorsTemplateAdmission.
func stillPasses() {
	_ = lisp.Native(int64(1))
	_ = lisp.Value([]*lisp.LVal{})
	_ = lisp.Value([]byte{1})
}

// --- interface-typed payloads ----------------------------------------------

func dynamicEmptyInterface(v interface{}) *lisp.LVal {
	return lisp.Native(v) // want `lisp\.Native payload type interface\{\} is not statically known`
}

func dynamicAny(v any) *lisp.LVal {
	return lisp.Native(v) // want `lisp\.Native payload type any is not statically known`
}

func dynamicValue(v interface{}) *lisp.LVal {
	return lisp.Value(v) // want `lisp\.Value payload type interface\{\} is not statically known`
}

func dynamicMethodSet(v interface{ Close() error }) *lisp.LVal {
	return lisp.Native(v) // want `lisp\.Native payload type interface\{Close\(\) error\} is not statically known`
}

func dynamicTypeParam[T any](x T) *lisp.LVal {
	return lisp.Native(x) // want `lisp\.Native payload type T is not statically known`
}

func dynamicFieldWrite(v *lisp.LVal, payload interface{}) {
	v.Native = payload // want `LVal\.Native assignment payload type interface\{\} is not statically known`
}

func dynamicAllowed(v interface{}) *lisp.LVal {
	return lisp.Native(v) //elpsvet:allow-native fixture: a pass-through constructor whose callers are checked at their own sites
}

// --- the allow marker --------------------------------------------------------

func allowTrailing(h *handle) *lisp.LVal {
	return lisp.Native(h) //elpsvet:allow-native fixture: the handle is immutable after construction
}

func allowStandalone(h *handle) *lisp.LVal {
	//elpsvet:allow-native fixture: the handle is immutable after construction
	return lisp.Native(h)
}

func allowStandaloneReachesOneLine(h *handle) {
	//elpsvet:allow-native fixture: covers only the statement below
	_ = lisp.Native(h)
	_ = lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowTrailingReachesItsLineOnly(h *handle) {
	_ = lisp.Native(h) //elpsvet:allow-native fixture: covers this line
	_ = lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowEmptyStandalone(h *handle) *lisp.LVal {
	//elpsvet:allow-native
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowEmptyWithTrailingSpace(h *handle) *lisp.LVal {
	//elpsvet:allow-native
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowTooShort(h *handle) *lisp.LVal {
	//elpsvet:allow-native two words
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowOwnershipMarkerIsNotThisRules(h *handle) *lisp.LVal {
	//elpsvet:allow the ownership rule's marker, however justified, is a different rule's
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowOneLineCoversEveryConstructionOnIt(h *handle) {
	_, _ = lisp.Native(h), lisp.NativeOf(h) //elpsvet:allow-native fixture: one justification covers the whole line
}

func allowMarkerPrefixOnly(h *handle) *lisp.LVal {
	//elpsvet:allow-natives by nobody -- a different marker sharing the prefix
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

// allowDoc has its whole body exempted by a justified marker in its doc
// comment.
//
//elpsvet:allow-native fixture: every native this function mints is a fresh, unshared handle
func allowDoc(h *handle) *lisp.LVal {
	return lisp.Native(h)
}

// allowDocEmpty carries a bare marker in its doc, which is not an audit.
//
//elpsvet:allow-native
func allowDocEmpty(h *handle) *lisp.LVal {
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

// --- package scope -----------------------------------------------------------

var marker = lisp.Native(&handle{}) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`

var markerAllowed = lisp.Native(&handle{}) //elpsvet:allow-native fixture: identity-only credential, never written

var makeNative = func(h *handle) *lisp.LVal {
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

var _ = marker
var _ = markerAllowed
var _ = makeNative
