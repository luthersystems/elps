package debugger

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestInspectLocals(t *testing.T) {
	parent := lisp.NewEnv(nil)
	parent.Put(lisp.Symbol("x"), lisp.Int(42))

	child := lisp.NewEnv(parent)
	child.Put(lisp.Symbol("y"), lisp.String("hello"))
	child.Put(lisp.Symbol("z"), lisp.Float(3.14))

	locals := InspectLocals(child)
	assert.Len(t, locals, 2)
	// Sorted by name.
	assert.Equal(t, "y", locals[0].Name)
	assert.Equal(t, "z", locals[1].Name)

	// InspectLocals does NOT include parent bindings.
	names := make(map[string]bool)
	for _, b := range locals {
		names[b.Name] = true
	}
	assert.False(t, names["x"])
}

func TestInspectScope(t *testing.T) {
	parent := lisp.NewEnv(nil)
	parent.Put(lisp.Symbol("x"), lisp.Int(42))

	child := lisp.NewEnv(parent)
	child.Put(lisp.Symbol("y"), lisp.String("hello"))

	scope := InspectScope(child)
	assert.Len(t, scope, 2)

	names := make(map[string]bool)
	for _, b := range scope {
		names[b.Name] = true
	}
	assert.True(t, names["x"])
	assert.True(t, names["y"])
}

func TestInspectScope_Shadowing(t *testing.T) {
	parent := lisp.NewEnv(nil)
	parent.Put(lisp.Symbol("x"), lisp.Int(1))

	child := lisp.NewEnv(parent)
	child.Put(lisp.Symbol("x"), lisp.Int(2))

	scope := InspectScope(child)
	// x appears once (child shadows parent).
	xCount := 0
	var xVal *lisp.LVal
	for _, b := range scope {
		if b.Name == "x" {
			xCount++
			xVal = b.Value
		}
	}
	assert.Equal(t, 1, xCount)
	require.NotNil(t, xVal)
	assert.Equal(t, 2, xVal.Int)
}

func TestInspectLocals_Nil(t *testing.T) {
	assert.Nil(t, InspectLocals(nil))
	assert.Nil(t, InspectScope(nil))
}

func TestFormatValue(t *testing.T) {
	tests := []struct {
		name string
		val  *lisp.LVal
		want string
	}{
		{"nil val", nil, "<nil>"},
		{"int", lisp.Int(42), "42"},
		{"float", lisp.Float(3.14), "3.14"},
		{"string", lisp.String("hello"), `"hello"`},
		{"symbol", lisp.Symbol("foo"), "foo"},
		{"nil list", lisp.Nil(), "()"},
		{"true", lisp.Symbol("true"), "true"},
		{"false", lisp.Symbol("false"), "false"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := FormatValue(tt.val)
			assert.Equal(t, tt.want, got)
		})
	}
}
