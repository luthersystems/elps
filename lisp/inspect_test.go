// Copyright © 2024 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestInspectFunction_Defun(t *testing.T) {
	// (defun add (a b) "Add two numbers." (+ a b))
	node := &LVal{
		Type: LSExpr,
		Source: &token.Location{File: "test.lisp", Line: 1, Col: 1},
		Cells: []*LVal{
			{Type: LSymbol, Str: "defun"},
			{Type: LSymbol, Str: "add"},
			{Type: LSExpr, Cells: []*LVal{
				{Type: LSymbol, Str: "a"},
				{Type: LSymbol, Str: "b"},
			}},
			{Type: LString, Str: "Add two numbers."},
			{Type: LSExpr, Cells: []*LVal{
				{Type: LSymbol, Str: "+"},
				{Type: LSymbol, Str: "a"},
				{Type: LSymbol, Str: "b"},
			}},
		},
	}

	info := InspectFunction(node)
	require.NotNil(t, info)
	assert.Equal(t, "add", info.Name)
	assert.Equal(t, "defun", info.Kind)
	assert.Equal(t, "Add two numbers.", info.DocString)
	assert.Equal(t, "test.lisp", info.Source.File)
	require.Len(t, info.Params, 2)
	assert.Equal(t, ParamInfo{Name: "a", Kind: ParamRequired}, info.Params[0])
	assert.Equal(t, ParamInfo{Name: "b", Kind: ParamRequired}, info.Params[1])
}

func TestInspectFunction_DefunNoDocstring(t *testing.T) {
	// (defun noop ())
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "defun"},
			{Type: LSymbol, Str: "noop"},
			{Type: LSExpr},
		},
	}

	info := InspectFunction(node)
	require.NotNil(t, info)
	assert.Equal(t, "noop", info.Name)
	assert.Equal(t, "defun", info.Kind)
	assert.Equal(t, "", info.DocString)
	assert.Empty(t, info.Params)
}

func TestInspectFunction_DefunAllParamKinds(t *testing.T) {
	// (defun f (a b &optional c d &rest args &key k1 k2) body)
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "defun"},
			{Type: LSymbol, Str: "f"},
			{Type: LSExpr, Cells: []*LVal{
				{Type: LSymbol, Str: "a"},
				{Type: LSymbol, Str: "b"},
				{Type: LSymbol, Str: "&optional"},
				{Type: LSymbol, Str: "c"},
				{Type: LSymbol, Str: "d"},
				{Type: LSymbol, Str: "&rest"},
				{Type: LSymbol, Str: "args"},
				{Type: LSymbol, Str: "&key"},
				{Type: LSymbol, Str: "k1"},
				{Type: LSymbol, Str: "k2"},
			}},
			{Type: LSExpr, Cells: []*LVal{{Type: LSymbol, Str: "body"}}},
		},
	}

	info := InspectFunction(node)
	require.NotNil(t, info)
	assert.Equal(t, "f", info.Name)
	require.Len(t, info.Params, 7)
	assert.Equal(t, ParamInfo{Name: "a", Kind: ParamRequired}, info.Params[0])
	assert.Equal(t, ParamInfo{Name: "b", Kind: ParamRequired}, info.Params[1])
	assert.Equal(t, ParamInfo{Name: "c", Kind: ParamOptional}, info.Params[2])
	assert.Equal(t, ParamInfo{Name: "d", Kind: ParamOptional}, info.Params[3])
	assert.Equal(t, ParamInfo{Name: "args", Kind: ParamRest}, info.Params[4])
	assert.Equal(t, ParamInfo{Name: "k1", Kind: ParamKey}, info.Params[5])
	assert.Equal(t, ParamInfo{Name: "k2", Kind: ParamKey}, info.Params[6])
}

func TestInspectFunction_Defmacro(t *testing.T) {
	// (defmacro when (test &rest body) "When macro." body)
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "defmacro"},
			{Type: LSymbol, Str: "when"},
			{Type: LSExpr, Cells: []*LVal{
				{Type: LSymbol, Str: "test"},
				{Type: LSymbol, Str: "&rest"},
				{Type: LSymbol, Str: "body"},
			}},
			{Type: LString, Str: "When macro."},
			{Type: LSExpr, Cells: []*LVal{{Type: LSymbol, Str: "body"}}},
		},
	}

	info := InspectFunction(node)
	require.NotNil(t, info)
	assert.Equal(t, "when", info.Name)
	assert.Equal(t, "defmacro", info.Kind)
	assert.Equal(t, "When macro.", info.DocString)
	require.Len(t, info.Params, 2)
	assert.Equal(t, ParamInfo{Name: "test", Kind: ParamRequired}, info.Params[0])
	assert.Equal(t, ParamInfo{Name: "body", Kind: ParamRest}, info.Params[1])
}

func TestInspectFunction_Lambda(t *testing.T) {
	// (lambda (x) (+ x 1))
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "lambda"},
			{Type: LSExpr, Cells: []*LVal{
				{Type: LSymbol, Str: "x"},
			}},
			{Type: LSExpr, Cells: []*LVal{
				{Type: LSymbol, Str: "+"},
				{Type: LSymbol, Str: "x"},
				{Type: LInt, Int: 1},
			}},
		},
	}

	info := InspectFunction(node)
	require.NotNil(t, info)
	assert.Equal(t, "", info.Name)
	assert.Equal(t, "lambda", info.Kind)
	assert.Equal(t, "", info.DocString)
	require.Len(t, info.Params, 1)
	assert.Equal(t, ParamInfo{Name: "x", Kind: ParamRequired}, info.Params[0])
}

func TestInspectFunction_LambdaWithDocstring(t *testing.T) {
	// (lambda (x) "Inc x." (+ x 1))
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "lambda"},
			{Type: LSExpr, Cells: []*LVal{{Type: LSymbol, Str: "x"}}},
			{Type: LString, Str: "Inc x."},
			{Type: LSExpr, Cells: []*LVal{
				{Type: LSymbol, Str: "+"},
				{Type: LSymbol, Str: "x"},
				{Type: LInt, Int: 1},
			}},
		},
	}

	info := InspectFunction(node)
	require.NotNil(t, info)
	assert.Equal(t, "Inc x.", info.DocString)
}

func TestInspectFunction_NonFunction(t *testing.T) {
	// (+ 1 2) — not a function definition
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "+"},
			{Type: LInt, Int: 1},
			{Type: LInt, Int: 2},
		},
	}
	assert.Nil(t, InspectFunction(node))
}

func TestInspectFunction_Nil(t *testing.T) {
	assert.Nil(t, InspectFunction(nil))
}

func TestInspectFunction_Atom(t *testing.T) {
	node := &LVal{Type: LInt, Int: 42}
	assert.Nil(t, InspectFunction(node))
}

func TestInspectFunction_QuotedSExpr(t *testing.T) {
	// '(defun foo () 42) — quoted, not a real definition
	node := &LVal{
		Type:   LSExpr,
		Quoted: true,
		Cells: []*LVal{
			{Type: LSymbol, Str: "defun"},
			{Type: LSymbol, Str: "foo"},
			{Type: LSExpr},
			{Type: LInt, Int: 42},
		},
	}
	assert.Nil(t, InspectFunction(node))
}

func TestInspectFunction_EmptyFormals(t *testing.T) {
	// (defun noop () 42)
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "defun"},
			{Type: LSymbol, Str: "noop"},
			{Type: LSExpr},
			{Type: LInt, Int: 42},
		},
	}

	info := InspectFunction(node)
	require.NotNil(t, info)
	assert.Equal(t, "noop", info.Name)
	assert.Empty(t, info.Params)
}

func TestInspectFunction_DefunTooFewArgs(t *testing.T) {
	// (defun foo) — missing formals
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "defun"},
			{Type: LSymbol, Str: "foo"},
		},
	}
	assert.Nil(t, InspectFunction(node))
}

func TestInspectFunction_DefunNonSymbolName(t *testing.T) {
	// (defun 42 () body)
	node := &LVal{
		Type: LSExpr,
		Cells: []*LVal{
			{Type: LSymbol, Str: "defun"},
			{Type: LInt, Int: 42},
			{Type: LSExpr},
		},
	}
	assert.Nil(t, InspectFunction(node))
}

func TestParseFormals_Nil(t *testing.T) {
	assert.Nil(t, ParseFormals(nil))
}

func TestParseFormals_NonList(t *testing.T) {
	assert.Nil(t, ParseFormals(&LVal{Type: LSymbol, Str: "x"}))
}

func TestParseFormals_Empty(t *testing.T) {
	result := ParseFormals(&LVal{Type: LSExpr})
	assert.Empty(t, result)
}

func TestParamKind_String(t *testing.T) {
	assert.Equal(t, "required", ParamRequired.String())
	assert.Equal(t, "optional", ParamOptional.String())
	assert.Equal(t, "rest", ParamRest.String())
	assert.Equal(t, "key", ParamKey.String())
	assert.Equal(t, "unknown", ParamKind(99).String())
}
