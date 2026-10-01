package lisp

import (
	"fmt"
	"testing"
)

func TestCopyDefaultTypes(t *testing.T) {
	for _, typ := range []LType{
		LInvalid, LInt, LFloat, LError, LSymbol, LSExpr, LFun, LQuote,
		LArray, LTaggedVal, LMarkTerminal, LMarkTailRec, LMarkMacExpand,
		LTypeMax, LTypeMax + 1, ^LType(0),
	} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			child := String("child")
			v := &LVal{
				Type: typ, Int: 7, Float: 1.5, Str: "payload",
				Cells: []*LVal{child}, Native: "opaque",
			}
			cp := v.Copy()
			if cp == v || cp.Type != typ || cp.Int != 7 || cp.Float != 1.5 || cp.Str != "payload" || cp.Native != v.Native {
				t.Fatal("copy changed the header payload")
			}
			if len(cp.Cells) != 1 || cp.Cells[0] == child || cp.Cells[0].Str != "child" {
				t.Fatal("copy did not copy the child")
			}
			cp.Cells[0].Str = "changed"
			if child.Str != "child" {
				t.Fatal("copy shares the source child")
			}
		})
	}
}
