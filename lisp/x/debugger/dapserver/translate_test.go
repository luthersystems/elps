package dapserver

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestResolveSourcePath(t *testing.T) {
	tests := []struct {
		name       string
		path       string
		file       string
		sourceRoot string
		want       string
	}{
		{
			name:       "absolute path unchanged",
			path:       "/abs/path/file.lisp",
			file:       "file.lisp",
			sourceRoot: "/root",
			want:       "/abs/path/file.lisp",
		},
		{
			name:       "relative path resolved with source root",
			path:       "file.lisp",
			file:       "file.lisp",
			sourceRoot: "/my/project",
			want:       "/my/project/file.lisp",
		},
		{
			name:       "relative subdir path resolved",
			path:       "subdir/file.lisp",
			file:       "file.lisp",
			sourceRoot: "/my/project",
			want:       "/my/project/subdir/file.lisp",
		},
		{
			name:       "no source root returns relative path as-is",
			path:       "file.lisp",
			file:       "file.lisp",
			sourceRoot: "",
			want:       "file.lisp",
		},
		{
			name:       "empty path uses file as fallback",
			path:       "",
			file:       "main.lisp",
			sourceRoot: "/root",
			want:       "/root/main.lisp",
		},
		{
			name:       "both empty returns empty",
			path:       "",
			file:       "",
			sourceRoot: "/root",
			want:       "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := resolveSourcePath(tt.path, tt.file, tt.sourceRoot)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestTranslateStackFrames_CrossFileSource(t *testing.T) {
	t.Parallel()
	// When the paused expression is in a different file than the top call
	// frame (e.g., stepping into a function defined in another file), the
	// top frame should use the paused expression's file, not the caller's.
	stack := &lisp.CallStack{
		Frames: []lisp.CallFrame{
			{
				Source: &token.Location{
					File: "caller.lisp",
					Path: "caller.lisp",
					Line: 10,
					Col:  1,
				},
				Name:    "my-func",
				Package: "user",
			},
		},
	}

	// Paused expression is in a different file.
	pausedExpr := &lisp.LVal{
		Type: lisp.LSExpr,
		Source: &token.Location{
			File: "callee.lisp",
			Path: "callee.lisp",
			Line: 3,
			Col:  5,
		},
	}

	frames := translateStackFrames(stack, pausedExpr, "/root")
	require.Len(t, frames, 1)

	// The top frame should reflect the paused expression's source file.
	assert.Equal(t, "callee.lisp", frames[0].Source.Name,
		"top frame source should use paused expression's file")
	assert.Equal(t, "/root/callee.lisp", frames[0].Source.Path)
	assert.Equal(t, 3, frames[0].Line)
	assert.Equal(t, 5, frames[0].Column)
}

func TestTranslateStackFrames_SameFileOverride(t *testing.T) {
	t.Parallel()
	// Even when in the same file, the paused expression's line/col should
	// override the frame's call-site location.
	stack := &lisp.CallStack{
		Frames: []lisp.CallFrame{
			{
				Source: &token.Location{
					File: "test.lisp",
					Path: "test.lisp",
					Line: 1,
					Col:  1,
				},
				Name:    "add",
				Package: "user",
			},
		},
	}

	pausedExpr := &lisp.LVal{
		Type: lisp.LSExpr,
		Source: &token.Location{
			File: "test.lisp",
			Path: "test.lisp",
			Line: 5,
			Col:  10,
		},
	}

	frames := translateStackFrames(stack, pausedExpr, "")
	require.Len(t, frames, 1)
	assert.Equal(t, 5, frames[0].Line, "top frame line should use paused expr's line")
	assert.Equal(t, 10, frames[0].Column, "top frame column should use paused expr's col")
}

func TestTranslateStackFrames_NilPausedExpr(t *testing.T) {
	t.Parallel()
	// When pausedExpr is nil, the frame's own source should be preserved.
	stack := &lisp.CallStack{
		Frames: []lisp.CallFrame{
			{
				Source: &token.Location{
					File: "test.lisp",
					Path: "test.lisp",
					Line: 1,
					Col:  1,
				},
				Name:    "fn",
				Package: "user",
			},
		},
	}

	frames := translateStackFrames(stack, nil, "/root")
	require.Len(t, frames, 1)
	assert.Equal(t, "test.lisp", frames[0].Source.Name,
		"should keep frame's own source when pausedExpr is nil")
	assert.Equal(t, 1, frames[0].Line)
}

func TestTranslateVariables_WithRefAllocator(t *testing.T) {
	nextRef := 5000
	allocRef := func(v *lisp.LVal) int {
		if v != nil && v.Type == lisp.LSExpr && !v.IsNil() {
			nextRef++
			return nextRef
		}
		return 0
	}

	bindings := []debugger.ScopeBinding{
		{Name: "x", Value: lisp.Int(42)},
		{Name: "items", Value: lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})},
	}

	vars := translateVariables(bindings, allocRef, nil)
	require.Len(t, vars, 2)

	assert.Equal(t, "x", vars[0].Name)
	assert.Equal(t, "42", vars[0].Value)
	assert.Equal(t, 0, vars[0].VariablesReference, "scalar should have ref=0")

	assert.Equal(t, "items", vars[1].Name)
	assert.Equal(t, "(1 2)", vars[1].Value)
	assert.Greater(t, vars[1].VariablesReference, 0, "list should have expandable ref")
}

func TestExpandVariable_List(t *testing.T) {
	list := lisp.SExpr([]*lisp.LVal{lisp.Int(10), lisp.String("hi"), lisp.Float(3.14)})
	noRef := func(v *lisp.LVal) int { return 0 }

	children := expandVariable(list, noRef, nil)
	require.Len(t, children, 3)
	assert.Equal(t, "[0]", children[0].Name)
	assert.Equal(t, "10", children[0].Value)
	assert.Equal(t, "[1]", children[1].Name)
	assert.Equal(t, `"hi"`, children[1].Value)
	assert.Equal(t, "[2]", children[2].Name)
	assert.Equal(t, "3.14", children[2].Value)
}

func TestExpandVariable_SortedMap(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSet(lisp.String("alpha"), lisp.Int(1))
	m.MapSet(lisp.String("beta"), lisp.Int(2))
	noRef := func(v *lisp.LVal) int { return 0 }

	children := expandVariable(m, noRef, nil)
	require.Len(t, children, 2)

	vals := make(map[string]string)
	for _, ch := range children {
		vals[ch.Name] = ch.Value
	}
	assert.Equal(t, "1", vals[`"alpha"`])
	assert.Equal(t, "2", vals[`"beta"`])
}

func TestExpandVariable_Array(t *testing.T) {
	arr := lisp.Array(nil, []*lisp.LVal{lisp.String("a"), lisp.String("b")})
	noRef := func(v *lisp.LVal) int { return 0 }

	children := expandVariable(arr, noRef, nil)
	require.Len(t, children, 2)
	assert.Equal(t, "[0]", children[0].Name)
	assert.Equal(t, `"a"`, children[0].Value)
	assert.Equal(t, "[1]", children[1].Name)
	assert.Equal(t, `"b"`, children[1].Value)
}

func TestExpandVariable_TaggedVal(t *testing.T) {
	inner := lisp.Int(42)
	tagged := &lisp.LVal{Type: lisp.LTaggedVal, Str: "my-type", Cells: []*lisp.LVal{inner}}
	noRef := func(v *lisp.LVal) int { return 0 }

	children := expandVariable(tagged, noRef, nil)
	require.Len(t, children, 1)
	assert.Equal(t, "data", children[0].Name)
	assert.Equal(t, "42", children[0].Value)
}

func TestExpandVariable_NativeNilEngine(t *testing.T) {
	native := lisp.Native(struct{ X int }{X: 42})
	noRef := func(v *lisp.LVal) int { return 0 }

	children := expandVariable(native, noRef, nil)
	assert.Nil(t, children, "LNative with nil engine should return nil children")
}

func TestExpandVariable_Nil(t *testing.T) {
	assert.Nil(t, expandVariable(nil, func(v *lisp.LVal) int { return 0 }, nil))
	assert.Nil(t, expandVariable(lisp.Int(42), func(v *lisp.LVal) int { return 0 }, nil))
}

func TestChildInfo(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name        string
		val         *lisp.LVal
		wantIndexed int
		wantNamed   int
	}{
		{
			name:        "nil value",
			val:         nil,
			wantIndexed: 0,
			wantNamed:   0,
		},
		{
			name:        "scalar int",
			val:         lisp.Int(42),
			wantIndexed: 0,
			wantNamed:   0,
		},
		{
			name:        "empty list",
			val:         lisp.Nil(),
			wantIndexed: 0,
			wantNamed:   0,
		},
		{
			name:        "list with 3 elements",
			val:         lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)}),
			wantIndexed: 3,
			wantNamed:   0,
		},
		{
			name:        "array with 2 elements",
			val:         lisp.Array(nil, []*lisp.LVal{lisp.String("a"), lisp.String("b")}),
			wantIndexed: 2,
			wantNamed:   0,
		},
		{
			name: "sorted-map with 2 entries",
			val: func() *lisp.LVal {
				m := lisp.SortedMap()
				m.MapSet(lisp.String("x"), lisp.Int(1))
				m.MapSet(lisp.String("y"), lisp.Int(2))
				return m
			}(),
			wantIndexed: 0,
			wantNamed:   2,
		},
		{
			// Tagged values have a single "data" child shown by expandVariable,
			// but childInfo returns (0,0) because pagination hints are not
			// useful for a single child.
			name:        "tagged value with scalar inner",
			val:         &lisp.LVal{Type: lisp.LTaggedVal, Str: "my-type", Cells: []*lisp.LVal{lisp.Int(1)}},
			wantIndexed: 0,
			wantNamed:   0,
		},
		{
			// LArray with only dimensions (no data slice) — defensive guard.
			name:        "array with missing data slice",
			val:         &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.SExpr(nil)}},
			wantIndexed: 0,
			wantNamed:   0,
		},
		{
			name:        "string value",
			val:         lisp.String("hello"),
			wantIndexed: 0,
			wantNamed:   0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			indexed, named := childInfo(tt.val)
			assert.Equal(t, tt.wantIndexed, indexed, "indexed children")
			assert.Equal(t, tt.wantNamed, named, "named children")
		})
	}
}

func TestTranslateVariables_PaginationHints(t *testing.T) {
	t.Parallel()
	noRef := func(v *lisp.LVal) int { return 0 }

	bindings := []debugger.ScopeBinding{
		{Name: "x", Value: lisp.Int(42)},
		{Name: "items", Value: lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})},
		{Name: "m", Value: func() *lisp.LVal {
			m := lisp.SortedMap()
			m.MapSet(lisp.String("a"), lisp.Int(1))
			return m
		}()},
	}

	vars := translateVariables(bindings, noRef, nil)
	require.Len(t, vars, 3)

	// Scalar: no hints set.
	assert.Equal(t, 0, vars[0].IndexedVariables, "scalar should have 0 indexed")
	assert.Equal(t, 0, vars[0].NamedVariables, "scalar should have 0 named")

	// List: indexed hints set.
	assert.Equal(t, 3, vars[1].IndexedVariables, "list should have 3 indexed")
	assert.Equal(t, 0, vars[1].NamedVariables, "list should have 0 named")

	// Sorted-map: named hints set.
	assert.Equal(t, 0, vars[2].IndexedVariables, "map should have 0 indexed")
	assert.Equal(t, 1, vars[2].NamedVariables, "map should have 1 named")
}

func TestExpandVariable_List_Hints(t *testing.T) {
	t.Parallel()
	// Create a list where one child is itself a list (expandable).
	inner := lisp.SExpr([]*lisp.LVal{lisp.Int(10), lisp.Int(20)})
	outer := lisp.SExpr([]*lisp.LVal{lisp.Int(1), inner, lisp.String("x")})
	noRef := func(v *lisp.LVal) int { return 0 }

	children := expandVariable(outer, noRef, nil)
	require.Len(t, children, 3)

	// First child is a scalar — no hints.
	assert.Equal(t, 0, children[0].IndexedVariables)
	assert.Equal(t, 0, children[0].NamedVariables)

	// Second child is a 2-element list — indexed hint should be set.
	assert.Equal(t, 2, children[1].IndexedVariables, "nested list child should have indexed hint")
	assert.Equal(t, 0, children[1].NamedVariables)

	// Third child is a string scalar — no hints.
	assert.Equal(t, 0, children[2].IndexedVariables)
	assert.Equal(t, 0, children[2].NamedVariables)
}
