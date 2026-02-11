// Copyright © 2024 The ELPS authors

package astutil

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
)

func TestHeadSymbol_Empty(t *testing.T) {
	v := &lisp.LVal{Type: lisp.LSExpr}
	assert.Equal(t, "", HeadSymbol(v))
}

func TestHeadSymbol_NonSExpr(t *testing.T) {
	v := &lisp.LVal{Type: lisp.LInt}
	assert.Equal(t, "", HeadSymbol(v))
}

func TestHeadSymbol_NonSymbolHead(t *testing.T) {
	v := &lisp.LVal{
		Type:  lisp.LSExpr,
		Cells: []*lisp.LVal{{Type: lisp.LInt}},
	}
	assert.Equal(t, "", HeadSymbol(v))
}

func TestHeadSymbol_SymbolHead(t *testing.T) {
	v := &lisp.LVal{
		Type:  lisp.LSExpr,
		Cells: []*lisp.LVal{{Type: lisp.LSymbol, Str: "foo"}},
	}
	assert.Equal(t, "foo", HeadSymbol(v))
}

func TestArgCount_Empty(t *testing.T) {
	v := &lisp.LVal{Type: lisp.LSExpr}
	assert.Equal(t, 0, ArgCount(v))
}

func TestArgCount_HeadOnly(t *testing.T) {
	v := &lisp.LVal{
		Type:  lisp.LSExpr,
		Cells: []*lisp.LVal{{Type: lisp.LSymbol, Str: "foo"}},
	}
	assert.Equal(t, 0, ArgCount(v))
}

func TestArgCount_WithArgs(t *testing.T) {
	v := &lisp.LVal{
		Type: lisp.LSExpr,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "foo"},
			{Type: lisp.LInt, Int: 1},
			{Type: lisp.LInt, Int: 2},
		},
	}
	assert.Equal(t, 2, ArgCount(v))
}

func TestSourceOf_PreferOwnSource(t *testing.T) {
	v := &lisp.LVal{
		Source: &token.Location{File: "test.lisp", Line: 5},
		Cells: []*lisp.LVal{
			{Source: &token.Location{File: "test.lisp", Line: 10}},
		},
	}
	result := SourceOf(v)
	assert.Equal(t, 5, result.Source.Line)
}

func TestSourceOf_FallbackToChild(t *testing.T) {
	v := &lisp.LVal{
		Source: nil,
		Cells: []*lisp.LVal{
			{Source: &token.Location{File: "test.lisp", Line: 10}},
		},
	}
	result := SourceOf(v)
	assert.Equal(t, 10, result.Source.Line)
}

func TestSourceOf_FallbackToSelf(t *testing.T) {
	v := &lisp.LVal{}
	result := SourceOf(v)
	assert.Same(t, v, result)
}

func TestWalk_VisitsAllNodes(t *testing.T) {
	// Build (foo (bar baz))
	inner := &lisp.LVal{
		Type: lisp.LSExpr,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "bar"},
			{Type: lisp.LSymbol, Str: "baz"},
		},
	}
	outer := &lisp.LVal{
		Type: lisp.LSExpr,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "foo"},
			inner,
		},
	}

	var visited []string
	Walk([]*lisp.LVal{outer}, func(node *lisp.LVal, parent *lisp.LVal, depth int) {
		if node.Type == lisp.LSymbol {
			visited = append(visited, node.Str)
		}
	})
	assert.Equal(t, []string{"foo", "bar", "baz"}, visited)
}

func TestWalkSExprs_SkipsQuoted(t *testing.T) {
	quoted := &lisp.LVal{
		Type:   lisp.LSExpr,
		Quoted: true,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "set"},
			{Type: lisp.LSymbol, Str: "x"},
		},
	}
	unquoted := &lisp.LVal{
		Type: lisp.LSExpr,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "foo"},
		},
	}

	var heads []string
	WalkSExprs([]*lisp.LVal{quoted, unquoted}, func(sexpr *lisp.LVal, depth int) {
		heads = append(heads, HeadSymbol(sexpr))
	})
	assert.Equal(t, []string{"foo"}, heads)
}

func TestUserDefined_Defun(t *testing.T) {
	// (defun my-fn (x y) body)
	defun := &lisp.LVal{
		Type: lisp.LSExpr,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "defun"},
			{Type: lisp.LSymbol, Str: "my-fn"},
			{Type: lisp.LSExpr, Cells: []*lisp.LVal{
				{Type: lisp.LSymbol, Str: "x"},
				{Type: lisp.LSymbol, Str: "y"},
			}},
			{Type: lisp.LSymbol, Str: "body"},
		},
	}

	defs := UserDefined([]*lisp.LVal{defun})
	assert.True(t, defs["my-fn"])
	assert.True(t, defs["x"])
	assert.True(t, defs["y"])
}

func TestCollectFormals_SkipsMarkers(t *testing.T) {
	formals := &lisp.LVal{
		Type: lisp.LSExpr,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "a"},
			{Type: lisp.LSymbol, Str: "&optional"},
			{Type: lisp.LSymbol, Str: "b"},
			{Type: lisp.LSymbol, Str: "&rest"},
			{Type: lisp.LSymbol, Str: "c"},
		},
	}

	defs := make(map[string]bool)
	CollectFormals(formals, defs)
	assert.True(t, defs["a"])
	assert.True(t, defs["b"])
	assert.True(t, defs["c"])
	assert.False(t, defs["&optional"])
	assert.False(t, defs["&rest"])
}
