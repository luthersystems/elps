// Copyright © 2026 The ELPS authors

package lisp

// Shape is how a value of an LType holds other values.
type Shape uint8

const (
	// ShapeLeaf holds no logical child values.
	ShapeLeaf Shape = iota + 1
	// ShapeList holds list or quote cells.
	ShapeList
	// ShapeArray holds dimension and data lists in Cells[0] and Cells[1].
	ShapeArray
	// ShapeMap holds map entries.
	ShapeMap
	// ShapeTagged holds a payload in Cells[0].
	ShapeTagged
	// ShapeError holds a condition in Str and error data in Cells.
	ShapeError
	// ShapeFun is opaque to value walks.
	ShapeFun
	// ShapeNative is opaque to value walks.
	ShapeNative
	// ShapeMark holds internal evaluator cells.
	ShapeMark
	// ShapeInvalid denotes LInvalid and out-of-range type tags.
	ShapeInvalid
)

// ShapeOf classifies t. Each walker decides which children to visit.
func ShapeOf(t LType) Shape {
	switch t {
	case LInt, LFloat, LString, LSymbol, LBytes:
		return ShapeLeaf
	case LSExpr, LQuote:
		return ShapeList
	case LArray:
		return ShapeArray
	case LSortMap:
		return ShapeMap
	case LTaggedVal:
		return ShapeTagged
	case LError:
		return ShapeError
	case LFun:
		return ShapeFun
	case LNative:
		return ShapeNative
	case LMarkTerminal, LMarkTailRec, LMarkMacExpand:
		return ShapeMark
	case LInvalid, LTypeMax:
	}
	return ShapeInvalid
}
