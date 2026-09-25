// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"reflect"
	"strings"
	"testing"
)

// Force continuation growth, then visit siblings through both map backings,
// captured values, lexical scope and a hidden capacity tail. Policy callbacks
// must retain their deterministic depth-first order after each return.
func TestTemplateContinuationPolicyOrder(t *testing.T) {
	env := templateOwnershipEnv()
	var calls []int
	leaf := func(n int) *LVal { return Native(&n) }
	stock := SortedMap()
	stock.MapSetString("b", leaf(2))
	stock.MapSetString("a", leaf(1))
	decoded := SortedMapFromData(NewMapData(jsonMap{"b": leaf(4), "a": leaf(3)}))
	lexical := NewEnv(env)
	lexical.scope = map[string]*LVal{"b": leaf(7)}
	lexical.scope["a"] = leaf(6)
	closure := lexical.Lambda(Formals(), []*LVal{Nil()})
	capture := newCapturedBuiltin(capturedBuiltin{
		Package: "user", FID: "capture", Formals: Formals(), Captures: leaf(5),
		Eval: func(_ *LEnv, _, values *LVal) *LVal { return values },
	})
	cells := []*LVal{stock, decoded, capture, closure, leaf(8)}
	v := QExpr(cells[:4])
	for range 80 {
		v = QExpr([]*LVal{v})
	}
	env.scope = map[string]*LVal{"subject": v}
	_, err := NewTemplate(env, TemplateWithNativePolicy(func(payload any) bool {
		calls = append(calls, *payload.(*int))
		return true
	}))
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(calls, []int{1, 2, 3, 4, 5, 6, 7, 8}) {
		t.Fatalf("policy order = %v", calls)
	}
}

func TestTemplateContinuationDepthBoundary(t *testing.T) {
	for _, sealed := range []bool{false, true} {
		env := templateOwnershipEnv()
		env.Runtime.MaxValueDepth = 1024
		depth := 1022 // root environment and final scalar also count
		if sealed {
			depth-- // entering the shared graph counts as another edge
		}
		v := depthTestValue(depth)
		if sealed {
			v.SealAST()
		}
		env.scope = map[string]*LVal{"subject": v}
		if _, err := NewTemplate(env); err != nil {
			t.Fatalf("sealed=%t: valid depth rejected: %v", sealed, err)
		}
		v = QExpr([]*LVal{v})
		if sealed {
			v.SealAST()
		}
		env.scope["subject"] = v
		_, err := NewTemplate(env)
		var depthErr ValueDepthError
		if !errors.As(err, &depthErr) || depthErr != 1024 || !strings.Contains(err.Error(), "scope subject:") {
			t.Fatalf("sealed=%t: expected ordinary depth error with scope, got %v", sealed, err)
		}
	}
}
