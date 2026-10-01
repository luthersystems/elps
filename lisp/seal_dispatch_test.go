package lisp

import (
	"fmt"
	"testing"
)

func TestSealableNodeDefaultTypes(t *testing.T) {
	for _, typ := range []LType{
		LInvalid, LInt, LFloat, LError, LSymbol, LSExpr, LFun, LQuote,
		LString, LBytes, LSortMap, LArray, LNative, LTaggedVal,
		LMarkTerminal, LMarkTailRec, LMarkMacExpand,
		LTypeMax, LTypeMax + 1, ^LType(0),
	} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			want := typ == LSExpr || typ == LQuote || typ == LSymbol || typ == LString || typ == LInt || typ == LFloat
			if got := sealableNodeType(typ); got != want {
				t.Fatalf("sealableNodeType = %v, want %v", got, want)
			}
		})
	}
}
