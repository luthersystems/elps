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

// Cross calls its callback across the package boundary.
func Cross(v *LVal, fn func(*LVal)) { fn(v) }
