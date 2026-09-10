// Package lisp is the IN-KERNEL half of
// TestNativePayloadAnalyzerMirrorsTemplateAdmission
// (cmd/elpsvet/nativepayload_runtime_test.go).  Every function here spells
// one construction, and that test pairs each with the SAME construction run
// through a real lisp.NewTemplate in-process.
//
// It exists as its own testdata root, at the kernel's import path and
// holding nothing but paired constructions, for two reasons that pull in the
// same direction.  The allowlist tier's first condition is that the site is
// IN package github.com/luthersystems/elps/lisp, so a construction outside
// that path can never reach the tier's third condition -- which header the
// literal names -- and a paired case spelled anywhere else would pass no
// matter what that condition said.  And the paired test asserts an exact
// diagnostic COUNT, so it needs a package whose every report belongs to a
// case in its table; testdata/nativelisp, which pins the tier's own shapes,
// carries a dozen reports that do not.
//
// The pair below is what makes the third condition checkable at all: two
// literals differing only in the LType constant they name, one exempt and
// one reported, both spelled inside the kernel, both published for real.
package lisp

type LType int

const (
	LError LType = iota
	LBytes
	LSortMap
	LFun
	LNative
)

// LVal is the kernel header; only the two fields the rule reads are here.
type LVal struct {
	Native interface{}
	Type   LType
}

// MapData is the LSortMap backing store, one of the allowlist rows.
type MapData struct{ n int }

// PairKernelSortMapLiteral is the tier's positive control, and the shape
// lisp.SortedMapFromData actually writes: a *MapData on an LSortMap header,
// which (*templateInventory).val routes to its own mapData arm and rebuilds
// per VM.  Publication admits it, so the rule must stay silent here -- a
// rule tightened until nothing passes mirrors nothing.
//
// Its payload type differs from the reported case's on purpose: the control
// asserts the analyzer's silence by the string a diagnostic about it would
// have had to contain, so the two cases must not share a payload spelling.
func PairKernelSortMapLiteral() *LVal {
	return &LVal{Type: LSortMap, Native: &MapData{n: 1}}
}

// PairKernelNativeHeaderLiteral is the control's literal shape, in the same
// package, differing in the header the Type key names.  LNative is the one
// arm val hands to native(), and native() refuses a *[]byte like any other
// pointer.  Before the narrowing this was exempt --
// the site was a kernel-slot spelling and the row was read off the payload
// type alone -- and publication refused it all the same.
func PairKernelNativeHeaderLiteral() *LVal {
	b := []byte{1}
	return &LVal{Type: LNative, Native: &b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}

// PairShadowedHeaderConst is the same literal again, wearing the control's
// spelling.  A function-local constant inside package lisp, of type LType
// and named LBytes, satisfies every property a NAME comparison can check --
// which is what the rule used to check -- while the header it builds is the
// LNative the case above builds.  So the value published for this case is
// the same value, and publication refuses it for the same reason: the
// analyzer must not go quiet just because the Type key is SPELLED like the
// row's header.
func PairShadowedHeaderConst() *LVal {
	const LBytes LType = LNative
	b := []byte{1}
	return &LVal{Type: LBytes, Native: &b} // want `LVal\.Native literal payload type \*\[\]byte is a kernel representation slot`
}
