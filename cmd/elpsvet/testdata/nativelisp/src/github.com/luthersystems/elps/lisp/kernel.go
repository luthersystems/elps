// Package lisp is the IN-KERNEL fixture for the elpsnativepayload rule's
// allowlist tier.  Its import path is github.com/luthersystems/elps/lisp,
// which is the whole reason it exists: the rows on allowedPayloadTypes
// describe the kernel's own representation storage, so the tier's first
// condition is that the package under analysis IS this one, and no fixture
// under another path can exercise the exempt side of it.
//
// It is a SEPARATE testdata root (testdata/nativelisp, run by
// TestNativePayloadAnalyzerInKernelPackage) rather than a file in the
// existing testdata/src stub, because that stub carries expectation comments
// for the escape rule and analysistest checks every expectation in a package
// against the ONE analyzer it is running -- a shared fixture would fail both
// tests.  testdata/ownership already uses a private root the same way.
//
// The shapes here are the kernel's own: lisp.Bytes, SortedMapFromData and
// the funData constructors write exactly these literals (lisp/lisp.go,
// lisp/env.go), and (*templateInventory).val routes each of them by the
// header's Type to an arm that is not native().  Everything that does NOT
// show one of those headers is reported, even here.
//
// analysistest checks absence as strictly as presence: a construction with
// no expectation comment asserts NO diagnostic there, so the exempt half is
// pinned as firmly as the reported half.
package lisp

type LType int

const (
	LError LType = iota
	LBytes
	LSortMap
	LFun
	LNative
)

// untypedHeader is an UNTYPED constant: assignable to an LType field, so it
// compiles, but its own type is untyped int rather than lisp.LType.  The
// resolver checks the constant's type as well as its declaring package, so
// this names no header.
const untypedHeader = 1

// LVal is the kernel header.  Only the two fields the rule reads are
// modelled: Type, the discriminant templateInventory.val switches on, and
// Native, the payload slot that doubles as backing storage for several
// non-LNative types.
type LVal struct {
	Native interface{}
	Type   LType
}

// MapData is the LSortMap backing store: an allowlist row whose header is
// LSortMap.
type MapData struct{ n int }

// funData is the LFun payload: an allowlist row whose header is LFun.  It is
// unexported in the real package too, which is why no fixture outside this
// path can spell a construction of one.
type funData struct{ fid string }

// Native is the kernel's LNative constructor, mirrored so the fixture can
// pin that a constructor is reported even INSIDE the kernel: it always
// builds an LNative, and val hands an LNative's payload to native().
//
//elpsvet:allow-native fixture mirror of the real constructor: the payload type is the caller's, and every call is checked at its own site
func Native(v interface{}) *LVal {
	return &LVal{Type: LNative, Native: v}
}

// --- the exempt half: the kernel building its own storage --------------------

// Bytes mirrors lisp.Bytes.  A *[]byte on an LBytes header is the row, and
// val's *[]byte arm records a byte span instead of calling native().
func Bytes(b []byte) *LVal {
	return &LVal{
		Type:   LBytes,
		Native: &b,
	}
}

// SortedMapFromData mirrors lisp.SortedMapFromData: a *MapData on an
// LSortMap header, which val routes to mapData.
func SortedMapFromData(data *MapData) *LVal {
	return &LVal{
		Type:   LSortMap,
		Native: data,
	}
}

// FunInPackage mirrors the funData constructors: a *funData on an LFun
// header, which val walks as function data.
func FunInPackage(pkg, fid string) *LVal {
	return &LVal{
		Type: LFun,
		Native: &funData{
			fid: fid,
		},
	}
}

// keyOrderIsIrrelevant pins that the Type key is found wherever it sits in
// the literal: the rule reads the whole element list before deciding, so a
// literal that spells Native first is treated like one that spells Type
// first.
func keyOrderIsIrrelevant(b *[]byte) *LVal {
	return &LVal{Native: b, Type: LBytes}
}

// fieldWriteInsideTheKernel is the residual the rule documents rather than
// closes: a `.Native` write shows no header at all, and every such write in
// the real kernel (lisp/copier.go, lisp/detach.go, lisp/template_plan.go) is
// guarded a few lines up by a check of the header's own Type that this rule
// does not model.  Inside package lisp a row payload written through the
// field is therefore still exempt -- and only inside it: the same write in
// any other package is reported (nativepayload.kernelSlotsOutsideTheKernel).
func fieldWriteInsideTheKernel(v *LVal, b *[]byte, m *MapData) {
	v.Native = b
	v.Native = m
}

// --- the reported half: a row payload on a header the row is not about -------

// nativeHeaderLiteral is the defect the narrowing closes, spelled where it
// is most tempting: inside the kernel, in the kernel's own literal shape,
// with a Type key that names the ONE header val hands to native().
func nativeHeaderLiteral(b *[]byte) *LVal {
	return &LVal{Type: LNative, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// noTypeKey shows no header at all, so the row cannot be true of it.  The
// diagnostic says so rather than reciting the rule.
func noTypeKey(b *[]byte) *LVal {
	return &LVal{Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// wrongTypeKey names a real header, just not this row's: a *[]byte on an
// LSortMap header is not what val's mapData arm expects, and a *MapData on
// an LBytes header is not what its byte-span arm expects.
func wrongTypeKey(b *[]byte, m *MapData) {
	_ = &LVal{Type: LSortMap, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
	_ = &LVal{Type: LBytes, Native: m}   // want `LVal\.Native literal payload type \*lisp\.MapData is a kernel representation slot`
	_ = &LVal{Type: LError, Native: m}   // want `LVal\.Native literal payload type \*lisp\.MapData is a kernel representation slot`
}

// computedTypeKey pins that only a NAMED constant resolves: an expression is
// not a header the rule can read, so the literal shows none.
func computedTypeKey(b *[]byte, t LType) *LVal {
	return &LVal{Type: t, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// untypedTypeKey pins the constant's TYPE half of the resolver: an untyped
// constant assigns to the field happily, and names no header at all.
func untypedTypeKey(b *[]byte) *LVal {
	return &LVal{Type: untypedHeader, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// aliasedTypeKey pins the fail-closed direction: a re-declared alias is a
// different constant object with a different name, so the header does not
// resolve and the site is reported.  A false positive, and the direction
// this rule fails in everywhere else -- write the header constant the kernel
// writes, or annotate.
func aliasedTypeKey(b *[]byte) *LVal {
	const bytesHeader = LBytes
	return &LVal{Type: bytesHeader, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// shadowedTypeKey is the defect the IDENTITY check closes, and it is
// spelled where nothing else could reach: a function-local constant inside
// package lisp, of type LType, named exactly like a row's header.  Every
// property a NAME comparison can see agrees with the kernel's own LBytes --
// declaring package lisp, type lisp.LType, name "LBytes" -- while the header
// the literal actually builds is an LNative, which val hands to native(),
// where a *[]byte is refused like any other pointer.  Looking the name back
// up in the PACKAGE scope finds the real constant, not this one, so the
// header does not resolve and the site is reported.
func shadowedTypeKey(b *[]byte) *LVal {
	const LBytes LType = LNative
	return &LVal{Type: LBytes, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// shadowedTypeKeySameValue documents that the criterion is IDENTITY, not
// value.  A local ConstSpec's own name is not in scope until the end of the
// spec, so the right-hand side here is the package-level LBytes and the
// shadow carries the kernel constant's exact value -- and it is still
// reported, because it is still not the object package lisp declares.  That
// is why headerTypeNamed compares objects and does not compare konst.Val():
// once identity holds the value follows, and where identity fails the value
// proves nothing about which constant the author actually named.
func shadowedTypeKeySameValue(b *[]byte) *LVal {
	const LBytes LType = LBytes
	return &LVal{Type: LBytes, Native: b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// constructorInsideTheKernel is reported here too.  The kernel is not
// exempt from what its own constructor builds: Native makes an LNative, and
// native() refuses a *[]byte like any other pointer.
func constructorInsideTheKernel(b *[]byte) *LVal {
	return Native(b) // want `lisp\.Native payload type \*\[\]byte is a kernel representation slot`
}

// nonRowPayloadIsUnaffected pins that the narrowing changed the ROW tier and
// nothing else: a payload with no row is reported at every spelling, in the
// kernel as anywhere, and a scalar passes at every spelling.
func nonRowPayloadIsUnaffected(v *LVal, h *handle) {
	v.Native = h                       // want `LVal\.Native assignment payload type \*lisp\.handle is not a known-safe value type`
	_ = &LVal{Type: LBytes, Native: h} // want `LVal\.Native literal payload type \*lisp\.handle is not a known-safe value type`
	v.Native = int64(1)
	_ = &LVal{Type: LNative, Native: int64(1)}
}

type handle struct{ n int }
