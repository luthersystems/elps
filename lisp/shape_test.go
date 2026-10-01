// Copyright © 2026 The ELPS authors

package lisp

import "testing"

func TestShapeOfCoversEveryLType(t *testing.T) {
	want := map[LType]Shape{
		LInt: ShapeLeaf, LFloat: ShapeLeaf, LString: ShapeLeaf,
		LSymbol: ShapeLeaf, LBytes: ShapeLeaf,
		LSExpr: ShapeList, LQuote: ShapeList, LArray: ShapeArray,
		LSortMap: ShapeMap, LTaggedVal: ShapeTagged, LError: ShapeError,
		LFun: ShapeFun, LNative: ShapeNative,
		LMarkTerminal: ShapeMark, LMarkTailRec: ShapeMark, LMarkMacExpand: ShapeMark,
	}
	for typ := LInt; typ < LTypeMax; typ++ {
		shape, covered := want[typ]
		if !covered || ShapeOf(typ) != shape || shape == ShapeInvalid {
			t.Errorf("ShapeOf(%d) = %d, want %d (covered: %t)", typ, ShapeOf(typ), shape, covered)
		}
	}
	for _, typ := range []LType{LInvalid, LTypeMax, LTypeMax + 1, ^LType(0)} {
		if got := ShapeOf(typ); got != ShapeInvalid {
			t.Errorf("ShapeOf(%d) = %d, want ShapeInvalid", typ, got)
		}
	}
}
