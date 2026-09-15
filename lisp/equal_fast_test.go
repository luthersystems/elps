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
	for _, leaf := range []*LVal{Float(math.NaN()), Bytes(nil), Native(7), {Type: LQuote, Cells: []*LVal{Int(7)}}} {
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
