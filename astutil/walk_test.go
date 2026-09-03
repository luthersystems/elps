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
	assert.Empty(t, HeadSymbol(v))
}

func TestHeadSymbol_NonSExpr(t *testing.T) {
	v := &lisp.LVal{Type: lisp.LInt}
	assert.Empty(t, HeadSymbol(v))
}

func TestHeadSymbol_NonSymbolHead(t *testing.T) {
	v := &lisp.LVal{
		Type:  lisp.LSExpr,
		Cells: []*lisp.LVal{{Type: lisp.LInt}},
	}
	assert.Empty(t, HeadSymbol(v))
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
		Cells: []*lisp.LVal{{}},
	}
	v.SetSource(&token.Location{File: "test.lisp", Line: 5})
	v.Cells[0].SetSource(&token.Location{File: "test.lisp", Line: 10})
	result := SourceOf(v)
	loc, ok := result.Source()
	assert.True(t, ok)
	assert.Equal(t, 5, loc.Line)
}

func TestSourceOf_FallbackToChild(t *testing.T) {
	v := &lisp.LVal{
		Cells: []*lisp.LVal{{}},
	}
	v.Cells[0].SetSource(&token.Location{File: "test.lisp", Line: 10})
	result := SourceOf(v)
	loc, ok := result.Source()
	assert.True(t, ok)
	assert.Equal(t, 10, loc.Line)
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
	quoted := lisp.QExpr([]*lisp.LVal{
		{Type: lisp.LSymbol, Str: "set"},
		{Type: lisp.LSymbol, Str: "x"},
	})
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

// TestSourceOf_Nil covers the nil parent that Walk documents it will pass for
// top-level expressions (issue #354).
func TestSourceOf_Nil(t *testing.T) {
	assert.Nil(t, SourceOf(nil))
}

// TestHeadSymbol_Nil and TestArgCount_Nil cover the same nil-parent shape for
// the sibling accessors, which are exported alongside Walk from lint/walk.go.
func TestHeadSymbol_Nil(t *testing.T) {
	assert.Empty(t, HeadSymbol(nil))
}

func TestArgCount_Nil(t *testing.T) {
	assert.Zero(t, ArgCount(nil))
}

// TestWalk_NilParentAccessors is the issue #354 reproducer: pairing Walk with
// the astutil accessors on the parent must not panic on top-level forms.
func TestWalk_NilParentAccessors(t *testing.T) {
	exprs := []*lisp.LVal{{
		Type: lisp.LSExpr,
		Cells: []*lisp.LVal{
			{Type: lisp.LSymbol, Str: "defun"},
			{Type: lisp.LSymbol, Str: "f"},
		},
	}}

	var sawNilParent bool
	assert.NotPanics(t, func() {
		Walk(exprs, func(_ *lisp.LVal, parent *lisp.LVal, _ int) {
			if parent == nil {
				sawNilParent = true
			}
			_ = SourceOf(parent)
			_ = HeadSymbol(parent)
			_ = ArgCount(parent)
		})
	})
	assert.True(t, sawNilParent, "Walk must pass a nil parent for top-level forms")
}

// TestSymbolLoc is the table for the two node shapes whose recorded span is
// WIDER than the name they carry: a symbol written with a reader quote, whose
// node also stands for the quote (elps#577), and a string literal used as a
// name by a def-like form, whose span includes its delimiters.
//
// Every row states an exact location, because a consumer builds a rename edit
// out of it and applies it to the user's file unread; an approximately right
// span is a program the user did not write.
func TestSymbolLoc(t *testing.T) {
	quoted := func(s string) *lisp.LVal {
		v := lisp.Symbol(s)
		return lisp.Quote(v)
	}

	for _, tc := range []struct {
		name string
		node *lisp.LVal
		loc  *token.Location // nil means "carries no location"
		want *token.Location // nil means "expect nil back"
	}{{
		// (set x 1): the span is the name and nothing is subtracted.
		name: "unquoted symbol is its own name",
		node: lisp.Symbol("x"),
		loc:  &token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6, EndPos: 6, EndLine: 1, EndCol: 7},
		want: &token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6, EndPos: 6, EndLine: 1, EndCol: 7},
	}, {
		// (set 'x 1): the span starts on the ', the name starts after it.
		name: "quoted symbol drops the reader quote",
		node: quoted("x"),
		loc:  &token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6, EndPos: 7, EndLine: 1, EndCol: 8},
		want: &token.Location{File: "t.lisp", Pos: 6, Line: 1, Col: 7, EndPos: 7, EndLine: 1, EndCol: 8},
	}, {
		// A multi-byte name is subtracted in BYTES, like every other column
		// in a token.Location.  This is what makes elps#577 distinct from
		// elps#463: nothing here counts runes.
		name: "quoted symbol measures the name in bytes",
		node: quoted("é"),
		loc:  &token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6, EndPos: 8, EndLine: 1, EndCol: 9},
		want: &token.Location{File: "t.lisp", Pos: 6, Line: 1, Col: 7, EndPos: 8, EndLine: 1, EndCol: 9},
	}, {
		// The gap between ' and the name is not assumed to be one byte: the
		// name is measured back from the end, so "'  x" works out too.
		name: "quoted symbol tolerates a gap after the quote",
		node: quoted("x"),
		loc:  &token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6, EndPos: 9, EndLine: 1, EndCol: 10},
		want: &token.Location{File: "t.lisp", Pos: 8, Line: 1, Col: 9, EndPos: 9, EndLine: 1, EndCol: 10},
	}, {
		name: "quoted symbol with no recorded end is left alone",
		node: quoted("x"),
		loc:  &token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6},
		want: &token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6},
	}, {
		// (s:deftype "myint" ...): the name is the interior, so BOTH ends
		// move in by one delimiter.
		name: "string literal drops its delimiters",
		node: lisp.String("myint"),
		loc:  &token.Location{File: "t.lisp", Pos: 11, Line: 1, Col: 12, EndPos: 18, EndLine: 1, EndCol: 19},
		want: &token.Location{File: "t.lisp", Pos: 12, Line: 1, Col: 13, EndPos: 17, EndLine: 1, EndCol: 18},
	}, {
		// The decoded value is shorter than the literal that produced it, so
		// the interior cannot be found by arithmetic: refuse rather than
		// guess, and hand back a span that is at worst too wide (which is
		// what every consumer already copes with) instead of one that is
		// wrong in an unpredictable direction.
		name: "escaped string literal is left alone",
		node: lisp.String("a\"b"),
		loc:  &token.Location{File: "t.lisp", Pos: 0, Line: 1, Col: 1, EndPos: 6, EndLine: 1, EndCol: 7},
		want: &token.Location{File: "t.lisp", Pos: 0, Line: 1, Col: 1, EndPos: 6, EndLine: 1, EndCol: 7},
	}, {
		name: "multi-line string literal is left alone",
		node: lisp.String("ab"),
		loc:  &token.Location{File: "t.lisp", Pos: 0, Line: 1, Col: 1, EndPos: 4, EndLine: 2, EndCol: 2},
		want: &token.Location{File: "t.lisp", Pos: 0, Line: 1, Col: 1, EndPos: 4, EndLine: 2, EndCol: 2},
	}, {
		name: "node without a location yields nil",
		node: quoted("x"),
		loc:  nil,
		want: nil,
	}} {
		t.Run(tc.name, func(t *testing.T) {
			if tc.loc != nil {
				tc.node.SetSource(tc.loc)
			}
			got := SymbolLoc(tc.node)
			if tc.want == nil {
				assert.Nil(t, got)
				return
			}
			if assert.NotNil(t, got) {
				assert.Equal(t, *tc.want, *got)
			}
		})
	}
}

// TestSymbolLocDoesNotMutateTheNode pins that the narrowing happens on
// SourceLoc's private copy.  A consumer that narrowed the node's own location
// would move every OTHER consumer's idea of where the form starts.
func TestSymbolLocDoesNotMutateTheNode(t *testing.T) {
	v := lisp.Quote(lisp.Symbol("x"))
	v.SetSource(&token.Location{File: "t.lisp", Pos: 5, Line: 1, Col: 6, EndPos: 7, EndLine: 1, EndCol: 8})

	_ = SymbolLoc(v)

	loc, ok := v.Source()
	assert.True(t, ok)
	assert.Equal(t, 5, loc.Pos, "SymbolLoc must not narrow the node's own span")
	assert.Equal(t, 6, loc.Col, "SymbolLoc must not narrow the node's own span")
}
