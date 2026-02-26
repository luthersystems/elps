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
