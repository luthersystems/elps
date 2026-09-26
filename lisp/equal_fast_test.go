// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"math"
	"testing"
)

func TestEqualShallowHandoffSiblings(t *testing.T) {
	for _, depth := range []int{0, cycleGuardDepth - 1, cycleGuardDepth, cycleGuardDepth + 1, 128} {
		left := SExpr([]*LVal{nestList(depth, Int(7)), String("tail")})
		right := SExpr([]*LVal{nestList(depth, Int(7)), String("tail")})
		if left.Equal(right) != Bool(true) {
			t.Fatalf("depth %d: equal siblings differ", depth)
		}
		right.Cells[1] = String("different")
		if left.Equal(right) != Bool(false) {
			t.Fatalf("depth %d: handoff lost the remaining sibling", depth)
		}
	}
	// Neither object identity nor the handoff may hide a non-reflexive leaf.
	for _, leaf := range []*LVal{Float(math.NaN()), Native(7)} {
		v := SExpr([]*LVal{nestList(128, Int(7)), leaf})
		if leaf.Equal(leaf) != Bool(false) || v.Equal(v) != Bool(false) {
			t.Fatalf("unsupported or NaN leaf compared equal: %s", leaf.Type)
		}
	}
}

func TestEqualRuntimeDepthBoundary(t *testing.T) {
	rt := &Runtime{MaxValueDepth: 1024}
	// Equality charges containers, including empty containers, but not scalars.
	for _, leaf := range []*LVal{Int(7), SExpr(nil)} {
		left, right := nestList(1024, leaf), nestList(1024, leaf)
		got := left.EqualWithRuntime(right, rt)
		if leaf.Type == LInt {
			if got != Bool(true) {
				t.Fatal("scalar at the boundary was rejected")
			}
		} else {
			var depthErr ValueDepthError
			if !errors.As(GoError(got), &depthErr) || depthErr != 1024 {
				t.Fatalf("expected typed depth error, got %v", got)
			}
		}
	}
}

// Bytes and reader quotes compare by content in both walkers: the shallow
// pass near the root, and the iterative walker it hands off to below
// cycleGuardDepth.
func TestEqualBytesAndQuoteBothWalkers(t *testing.T) {
	// Quote twice: the first layer is the quoted flag, the second an LQuote.
	quote := func(v *LVal) *LVal { return Quote(Quote(v)) }
	for _, depth := range []int{0, cycleGuardDepth - 1, cycleGuardDepth, cycleGuardDepth + 1, 128} {
		for _, tc := range []struct {
			name      string
			a, b      *LVal
			wantEqual bool
		}{
			{"same bytes", Bytes([]byte("ab")), Bytes([]byte("ab")), true},
			{"nil and empty bytes", Bytes(nil), Bytes([]byte{}), true},
			{"different bytes", Bytes([]byte("ab")), Bytes([]byte("ac")), false},
			{"same quote", quote(Int(7)), quote(Int(7)), true},
			{"different quote", quote(Int(7)), quote(Int(8)), false},
			{"quote depth differs", quote(Int(7)), quote(quote(Int(7))), false},
			{"quoted bytes", quote(Bytes([]byte("x"))), quote(Bytes([]byte("x"))), true},
		} {
			left := SExpr([]*LVal{nestList(depth, tc.a), String("tail")})
			right := SExpr([]*LVal{nestList(depth, tc.b), String("tail")})
			if got := left.Equal(right); got != Bool(tc.wantEqual) {
				t.Errorf("depth %d, %s: got %v, want %v", depth, tc.name, got, tc.wantEqual)
			}
			if got := left.Equal(left); got != Bool(true) {
				t.Errorf("depth %d, %s: value not equal to itself: %v", depth, tc.name, got)
			}
		}
	}
}
