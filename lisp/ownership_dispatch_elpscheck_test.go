//go:build elpscheck

package lisp

import (
	"fmt"
	"testing"
	"weak"
)

func TestOwnershipKeyDefaultTypes(t *testing.T) {
	for _, typ := range []LType{
		LInvalid, LInt, LFloat, LError, LSymbol, LSExpr, LFun, LQuote,
		LString, LBytes, LSortMap, LArray, LNative, LTaggedVal,
		LMarkTerminal, LMarkTailRec, LMarkMacExpand,
		LTypeMax, LTypeMax + 1, ^LType(0),
	} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			v := &LVal{Type: typ}
			if got := ownershipKey(v); got != weak.Make(v) {
				t.Fatal("ownershipKey changed the header identity")
			}
		})
	}
}
