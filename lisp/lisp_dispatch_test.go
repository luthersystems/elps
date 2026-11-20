package lisp

import (
	"fmt"
	"testing"
)

func TestEqualDefaultTypes(t *testing.T) {
	for _, v := range []*LVal{Int(7), Float(1.5), String("text"), Symbol("name"), Bytes([]byte("x"))} {
		budget := equalShallowBudget
		if got := v.equalShallow(v, 0, &budget); got != Bool(true) {
			t.Fatalf("equalShallow(type=%d) = %v, want true", v.Type, got)
		}
		for _, strict := range []bool{false, true} {
			if got, repeated := v.equalIter(v, MaxValueDepth, strict, nil); got != Bool(true) || repeated {
				t.Fatalf("equalIter(type=%d, strict=%v) = %v, %v, want true, false", v.Type, strict, got, repeated)
			}
		}
	}
	// Errors, functions and natives compare by identity (luthersystems/elps#997):
	// a value equals itself, and a copy with the same cells does not.
	for _, typ := range []LType{LError, LFun, LNative} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			v := &LVal{Type: typ, Cells: []*LVal{Int(7)}}
			w := &LVal{Type: typ, Cells: []*LVal{Int(7)}}
			if typ == LNative {
				// Distinct pointers: nativeEqual compares them by address.
				v.Native, w.Native = new(int), new(int)
			}
			for _, tc := range []struct {
				other *LVal
				want  *LVal
			}{{v, Bool(true)}, {w, Bool(false)}} {
				budget := equalShallowBudget
				if got := v.equalShallow(tc.other, 0, &budget); got != tc.want {
					t.Fatalf("equalShallow = %v, want %v", got, tc.want)
				}
				for _, strict := range []bool{false, true} {
					if got, repeated := v.equalIter(tc.other, MaxValueDepth, strict, nil); got != tc.want || repeated {
						t.Fatalf("equalIter(strict=%v) = %v, %v, want %v, false", strict, got, repeated, tc.want)
					}
				}
				for _, depth := range []int{0, cycleGuardDepth + 1} {
					if got := nestList(depth, v).Equal(nestList(depth, tc.other)); got != tc.want {
						t.Fatalf("Equal at depth %d = %v, want %v", depth, got, tc.want)
					}
				}
			}
		})
	}
	for _, typ := range []LType{
		LInvalid, LMarkTerminal, LMarkTailRec,
		LMarkMacExpand, LTypeMax, LTypeMax + 1, ^LType(0),
	} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			v := &LVal{Type: typ, Cells: []*LVal{Int(7)}}
			budget := equalShallowBudget
			if got := v.equalShallow(v, 0, &budget); got != Bool(false) {
				t.Fatalf("equalShallow = %v, want false", got)
			}
			for _, strict := range []bool{false, true} {
				if got, repeated := v.equalIter(v, MaxValueDepth, strict, nil); got != Bool(false) || repeated {
					t.Fatalf("equalIter(strict=%v) = %v, %v, want false, false", strict, got, repeated)
				}
			}
			for _, depth := range []int{0, cycleGuardDepth + 1} {
				root := nestList(depth, v)
				if got := root.Equal(root); got != Bool(false) {
					t.Fatalf("Equal at depth %d = %v, want false", depth, got)
				}
			}
		})
	}
}
