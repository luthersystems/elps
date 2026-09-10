// Package nativemarker exercises the elpsnativepayload rule's marker tier:
// the shape (*templateInventory).native admits automatically, the pointer
// form it refuses, the method-name lookalike it must not accept, and the
// NativeCloner-only type that used to be a tier and is now a report.
//
// It sits under github.com/luthersystems/elps/ because Go's internal rule
// lets only packages in that tree import internal/templatepolicy -- which is
// the same reason a downstream embedder cannot claim this tier and must use
// lisp.TemplateWithNativePolicy instead.
//
// analysistest checks absence as strictly as presence: a construction with
// no want-expectation comment asserts NO diagnostic there.
package nativemarker

import (
	"regexp"
	"time"

	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
)

// ownedStamp is the audited shape: a struct VALUE embedding the marker.  The
// three real ones are libtime's ownedTime, libregexp's compiledRegexp and
// libschema's validatorTag.
type ownedStamp struct {
	templatepolicy.Marker
	t time.Time
}

var _ templatepolicy.Immutable = ownedStamp{}

// ownedRegexp is libregexp's shape: a private wrapper whose only field is the
// mutable host value the wrapper never hands out.
type ownedRegexp struct {
	templatepolicy.Marker
	re *regexp.Regexp
}

// tag is libschema's shape: a zero-size credential whose whole content is its
// type.
type tag struct{ templatepolicy.Marker }

// lookalike declares a method of the same NAME in this package rather than
// inheriting templatepolicy's.  A real assertion to templatepolicy.Immutable
// fails on it -- an unexported method is only satisfiable from its declaring
// package -- and so must the rule.
type lookalike struct{ n int }

func (lookalike) templateImmutable() {}

// suite is the NativeCloner-only type: no marker, a clone method.  Under the
// template contract CloneNative is not an admission protocol (lisp/native.go),
// so declaring it is evidence the payload needs cloning, not a reason to
// exempt it.
type suite struct{ tests []string }

func (s *suite) CloneNative() interface{} { return &suite{tests: append([]string(nil), s.tests...)} }

var _ lisp.NativeCloner = (*suite)(nil)

// --- the marker tier admits a struct VALUE ---------------------------------

func markedValue(s ownedStamp, r ownedRegexp, v tag) {
	_ = lisp.Native(s)
	_ = lisp.NativeOf(r)
	_ = lisp.NativeOf[tag](v)
	_ = lisp.Value(s)
	_ = &lisp.LVal{Native: v}
}

func markedValueFieldWrite(l *lisp.LVal, s ownedStamp) {
	l.Native = s
}

// markedAnonymousStruct pins that the tier is about the SHAPE, not about a
// named type: the runtime asserts the payload's dynamic type and reads its
// reflect.Kind, and an unnamed struct embedding the marker satisfies both.
func markedAnonymousStruct(s struct{ templatepolicy.Marker }) {
	_ = lisp.Native(s)
}

// --- and refuses every neighbouring shape ----------------------------------

func markedPointer(s *ownedStamp) *lisp.LVal {
	// A pointer's method set inherits the value's marker, so an assertion to
	// templatepolicy.Immutable succeeds -- but the runtime also requires
	// reflect.Struct, because a caller holding the pointer can replace the
	// whole pointee however private its fields are.
	return lisp.Native(s) // want `lisp\.Native payload type \*nativemarker\.ownedStamp is not a known-safe value type`
}

func markedPointerLiteral(s *ownedStamp) *lisp.LVal {
	return &lisp.LVal{Native: s} // want `LVal\.Native literal payload type \*nativemarker\.ownedStamp is not a known-safe value type`
}

func markerLookalike(l lookalike) *lisp.LVal {
	return lisp.Native(l) // want `lisp\.Native payload type nativemarker\.lookalike is not a known-safe value type`
}

func markedSliceOfMarked(s []ownedStamp) *lisp.LVal {
	return lisp.Native(s) // want `lisp\.Native payload type \[\]nativemarker\.ownedStamp is not a known-safe value type`
}

// --- NativeCloner is not a tier --------------------------------------------

func clonerPointer(s *suite) *lisp.LVal {
	return lisp.Native(s) // want `lisp\.Native payload type \*nativemarker\.suite is not a known-safe value type`
}

func clonerValue(s suite) *lisp.LVal {
	return lisp.Native(s) // want `lisp\.Native payload type nativemarker\.suite is not a known-safe value type`
}

// --- the host types the marker exists to wrap ------------------------------

func rawHostValues(t time.Time, re *regexp.Regexp) {
	// Refuted by lisp/lisplib/template_natives_test.go: a raw time.Time's
	// *Location and a *regexp.Regexp are both host-mutable after admission.
	_ = lisp.Native(t)  // want `lisp\.Native payload type time\.Time is not a known-safe value type`
	_ = lisp.Native(re) // want `lisp\.Native payload type \*regexp\.Regexp is not a known-safe value type`
}
