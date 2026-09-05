// Package nativepayload exercises the elpsnativepayload analyzer: every
// construction spelling, the basic-type tier, the NativeCloner rule, the
// audited allowlist, the interface-typed report, and the allow marker with
// and without a justification.
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

// suite declares lisp.NativeCloner on its POINTER receiver.
type suite struct{ tests []string }

func (s *suite) CloneNative() interface{} { return &suite{tests: append([]string(nil), s.tests...)} }

var _ lisp.NativeCloner = (*suite)(nil)

// valueCloner declares it on the VALUE receiver, so both a value and a
// pointer satisfy the protocol.
type valueCloner struct{ n int }

func (valueCloner) CloneNative() interface{} { return valueCloner{} }

// wrongCloner has a method of the right name and the wrong shape; a
// type assertion to lisp.NativeCloner would fail, and so must the rule.
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
	return &lisp.LVal{Type: lisp.LError, Native: h} // want `lisp\.LVal literal payload type \*nativepayload\.handle is not a known-safe value type`
}

func literalValue(h *handle) lisp.LVal {
	return lisp.LVal{Native: h} // want `lisp\.LVal literal payload type \*nativepayload\.handle is not a known-safe value type`
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

func unsafePointer(p unsafe.Pointer) *lisp.LVal {
	return lisp.Native(p) // want `lisp\.Native payload type unsafe\.Pointer is not a known-safe value type`
}

func composites(m map[string]int, sl []int, ch chan int, fn func(), arr [2]int, st struct{ n int }) {
	_ = lisp.Native(m)   // want `lisp\.Native payload type map\[string\]int is not a known-safe value type`
	_ = lisp.Native(sl)  // want `lisp\.Native payload type \[\]int is not a known-safe value type`
	_ = lisp.Native(ch)  // want `lisp\.Native payload type chan int is not a known-safe value type`
	_ = lisp.Native(fn)  // want `lisp\.Native payload type func\(\) is not a known-safe value type`
	_ = lisp.Native(arr) // want `lisp\.Native payload type \[2\]int is not a known-safe value type`
	_ = lisp.Native(st)  // want `lisp\.Native payload type struct\{n int\} is not a known-safe value type`
}

// --- the NativeCloner rule -------------------------------------------------

func cloner(s *suite) {
	_ = lisp.Native(s)
	_ = lisp.NativeOf[*suite](s)
	_ = &lisp.LVal{Native: s}
}

func clonerValueOfPointerReceiver(s suite) *lisp.LVal {
	// A suite VALUE does not satisfy lisp.NativeCloner (the method has a
	// pointer receiver), so an assertion on the payload would fail at fork
	// time; the rule must not accept it either.
	return lisp.Native(s) // want `lisp\.Native payload type nativepayload\.suite is not a known-safe value type`
}

func clonerValueReceiver(v valueCloner) {
	_ = lisp.Native(v)
	_ = lisp.Native(&v)
}

func clonerWrongShape(w *wrongCloner) *lisp.LVal {
	return lisp.Native(w) // want `lisp\.Native payload type \*nativepayload\.wrongCloner is not a known-safe value type`
}

// --- the audited allowlist ---------------------------------------------------

func allowlisted(re *regexp.Regexp, t time.Time, d time.Duration, err error, s *lisp.CallStack) {
	_ = lisp.Native(re)
	_ = lisp.Native(t)
	_ = lisp.Native(d) // a defined type over int64: the basic tier, not a row
	_ = lisp.Native(err)
	_ = lisp.Native(s)
	_ = &lisp.LVal{Native: s}
	_ = lisp.Value(re)
}

func allowlistedByValueNotPointer(re regexp.Regexp, t *time.Time) {
	// The rows are keyed on the exact type, pointer-ness included.
	_ = lisp.Native(re) // want `lisp\.Native payload type regexp\.Regexp is not a known-safe value type`
	_ = lisp.Native(t)  // want `lisp\.Native payload type \*time\.Time is not a known-safe value type`
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
	return lisp.Native(v) //elpsvet:allow fixture: a pass-through constructor whose callers are checked at their own sites
}

// --- the allow marker --------------------------------------------------------

func allowTrailing(h *handle) *lisp.LVal {
	return lisp.Native(h) //elpsvet:allow fixture: the handle is immutable after construction
}

func allowStandalone(h *handle) *lisp.LVal {
	//elpsvet:allow fixture: the handle is immutable after construction
	return lisp.Native(h)
}

func allowStandaloneReachesOneLine(h *handle) {
	//elpsvet:allow fixture: covers only the statement below
	_ = lisp.Native(h)
	_ = lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowTrailingReachesItsLineOnly(h *handle) {
	_ = lisp.Native(h) //elpsvet:allow fixture: covers this line
	_ = lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowEmptyStandalone(h *handle) *lisp.LVal {
	//elpsvet:allow
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowEmptyWithTrailingSpace(h *handle) *lisp.LVal {
	//elpsvet:allow
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

func allowMarkerPrefixOnly(h *handle) *lisp.LVal {
	//elpsvet:allowed by nobody -- a different marker sharing the prefix
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

// allowDoc has its whole body exempted by a justified marker in its doc
// comment.
//
//elpsvet:allow fixture: every native this function mints is a fresh, unshared handle
func allowDoc(h *handle) *lisp.LVal {
	return lisp.Native(h)
}

// allowDocEmpty carries a bare marker in its doc, which is not an audit.
//
//elpsvet:allow
func allowDocEmpty(h *handle) *lisp.LVal {
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

// --- package scope -----------------------------------------------------------

var marker = lisp.Native(&handle{}) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`

var markerAllowed = lisp.Native(&handle{}) //elpsvet:allow fixture: identity-only credential, never written

var makeNative = func(h *handle) *lisp.LVal {
	return lisp.Native(h) // want `lisp\.Native payload type \*nativepayload\.handle is not a known-safe value type`
}

var _ = marker
var _ = markerAllowed
var _ = makeNative
