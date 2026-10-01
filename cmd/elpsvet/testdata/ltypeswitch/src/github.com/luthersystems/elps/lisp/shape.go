// Copyright © 2026 The ELPS authors
package lisp

type LType uint

const (
	LInvalid LType = iota
	LInt
	LFloat
	LError
	LSymbol
	LSExpr
	LFun
	LQuote
	LString
	LBytes
	LSortMap
	LArray
	LNative
	LTaggedVal
	LMarkTerminal
	LMarkTailRec
	LMarkMacExpand
	LTypeMax
)

type LVal struct {
	Type  LType
	Cells []*LVal
}
type Shape uint8

func ShapeOf(t LType) Shape { return Shape(t) }

type Alias = LType

const IntAlias = LInt

func exhaustive(t LType) {
	switch t {
	case LInvalid, LInt, LFloat, LError, LSymbol, LSExpr, LFun, LQuote, LString, LBytes, LSortMap, LArray, LNative, LTaggedVal, LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
	}
}
func (v *LVal) Tag() LType { return v.Type }
func local(v *LVal) {
	t := v.Type
	switch t { // want "lisp.LType switch misses constants:"
	case LInt:
	}
}
func result(v *LVal) {
	switch v.Tag() { // want "lisp.LType switch misses constants:"
	case LFloat:
	}
}
func alias(t Alias) {
	switch t { // want "lisp.LType switch misses constants:"
	case IntAlias:
	default: // want "lisp.LType switch has a default arm"
	}
}
func completeDefault(t LType) {
	switch t {
	case LInvalid, LInt, LFloat, LError, LSymbol, LSExpr, LFun, LQuote, LString, LBytes, LSortMap, LArray, LNative, LTaggedVal, LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
	default: // want "lisp.LType switch has a default arm"
	}
}
func unrelated(t uint) {
	switch t {
	default:
	}
}

type Foreign uint

func foreign(t Foreign) {
	switch t {
	default:
	}
}
func tagless(v *LVal) {
	switch {
	case v.Type == LInt:
	}
}
