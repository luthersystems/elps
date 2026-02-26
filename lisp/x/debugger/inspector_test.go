package debugger

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/token"
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

	// Value assertions.
	assert.Equal(t, "hello", locals[0].Value.Str)
	assert.InDelta(t, 3.14, locals[1].Value.Float, 0.001)

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

	nameVals := make(map[string]*lisp.LVal)
	for _, b := range scope {
		nameVals[b.Name] = b.Value
	}
	assert.Contains(t, nameVals, "x")
	assert.Contains(t, nameVals, "y")

	// Value assertions.
	assert.Equal(t, 42, nameVals["x"].Int)
	assert.Equal(t, "hello", nameVals["y"].Str)
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
		name     string
		val      *lisp.LVal
		want     string
		contains string // if non-empty, use Contains instead of Equal
	}{
		{"nil val", nil, "<nil>", ""},
		{"int", lisp.Int(42), "42", ""},
		{"float", lisp.Float(3.14), "3.14", ""},
		{"string", lisp.String("hello"), `"hello"`, ""},
		{"symbol", lisp.Symbol("foo"), "foo", ""},
		{"nil list", lisp.Nil(), "()", ""},
		{"true", lisp.Symbol("true"), "true", ""},
		{"false", lisp.Symbol("false"), "false", ""},
		{"function", lisp.Fun("test-fn", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			return lisp.Nil()
		}), "<function test-fn>", ""},
		{"error", lisp.Errorf("test error"), "<error: error>", ""},
		{"native nil", lisp.Native(nil), "<native nil>", ""},
		{"native int", lisp.Native(42), "<native int>", ""},
		{"array", lisp.Array(nil, []*lisp.LVal{lisp.Int(1), lisp.Int(2)}), "<array len=2>", ""},
		{"sorted-map", lisp.SortedMap(), "<sorted-map len=0>", ""},
		{"qsymbol", lisp.QSymbol("pkg:sym"), "'pkg:sym", ""},
		{"bytes", lisp.Bytes([]byte{0x01, 0x02}), "<bytes len=2>", ""},
		{"sexpr list", lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)}), "(1 2 3)", ""},
		{"sexpr quoted", func() *lisp.LVal {
			v := lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
			v.Quoted = true
			return v
		}(), "[1 2 3]", ""},
		{"long list", func() *lisp.LVal {
			cells := make([]*lisp.LVal, 12)
			for i := range cells {
				cells[i] = lisp.Int(i)
			}
			return lisp.SExpr(cells)
		}(), "(12 elements)", ""},
		{"tagged value", &lisp.LVal{Type: lisp.LTaggedVal, Str: "my-type"}, "<tagged my-type>", ""},
		{"macro", func() *lisp.LVal {
			v := lisp.Fun("test-macro", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return lisp.Nil()
			})
			v.FunType = lisp.LFunMacro
			return v
		}(), "<macro test-macro>", ""},
		{"special-op", func() *lisp.LVal {
			v := lisp.Fun("test-op", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				return lisp.Nil()
			})
			v.FunType = lisp.LFunSpecialOp
			return v
		}(), "<special-op test-op>", ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := FormatValue(tt.val)
			if tt.contains != "" {
				assert.Contains(t, got, tt.contains)
			} else {
				assert.Equal(t, tt.want, got)
			}
		})
	}
}

// newInspectorTestEnv creates a minimal ELPS environment for inspector tests.
func newInspectorTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env)
	require.True(t, rc.IsNil(), "InitializeUserEnv failed: %v", rc)
	rc = lisplib.LoadLibrary(env)
	require.True(t, rc.IsNil(), "LoadLibrary failed: %v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.True(t, rc.IsNil(), "InPackage failed: %v", rc)
	return env
}

func TestEvalInContext(t *testing.T) {
	env := newInspectorTestEnv(t)
	result := EvalInContext(env, "(+ 1 2)")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 3, result.Int)
}

func TestEvalInContext_Error(t *testing.T) {
	env := newInspectorTestEnv(t)
	// Unbound symbol should produce an error.
	result := EvalInContext(env, "undefined-symbol-xyz")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LError, result.Type)
}

func TestEvalInContext_Empty(t *testing.T) {
	env := newInspectorTestEnv(t)
	result := EvalInContext(env, "")
	require.NotNil(t, result)
	// Empty string produces nil (no expressions parsed).
	assert.True(t, result.IsNil(), "expected nil for empty input, got %v", result)
}

func TestEvalInContext_ParseError(t *testing.T) {
	env := newInspectorTestEnv(t)
	// Malformed expression — unclosed paren.
	result := EvalInContext(env, "(+ 1")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LError, result.Type)
	// Error condition type is in Str; actual message is in Cells.
	// Just verify we got an error LVal back.
	formatted := FormatValue(result)
	assert.Contains(t, formatted, "error", "expected error in formatted output, got: %s", formatted)
}

func TestEvalInContext_NoReader(t *testing.T) {
	env := lisp.NewEnv(nil)
	// Don't set a reader — should return error.
	result := EvalInContext(env, "(+ 1 2)")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LError, result.Type)
}

func TestEvalInContext_MultiExpression(t *testing.T) {
	env := newInspectorTestEnv(t)
	// All expressions are evaluated; the last result is returned.
	result := EvalInContext(env, "(+ 1 2) (+ 3 4)")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 7, result.Int, "should return the last expression result")
}

func TestEvalInContext_MultiExpression_ErrorShortCircuit(t *testing.T) {
	env := newInspectorTestEnv(t)
	// Pre-set a sentinel. If the third expression runs, it will change it.
	EvalInContext(env, "(set 'sc-guard 0)")
	// Error in second expression short-circuits — third is not evaluated.
	result := EvalInContext(env, "(+ 1 2) undefined-sym-xyz (set 'sc-guard 99)")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LError, result.Type)
	// Verify the third expression was NOT evaluated.
	guard := EvalInContext(env, "sc-guard")
	assert.Equal(t, 0, guard.Int, "third expression should not have been evaluated")
}

func TestEvalInContext_MultiExpression_Mutation(t *testing.T) {
	env := newInspectorTestEnv(t)
	// First expression sets a variable, second reads it.
	result := EvalInContext(env, "(set 'debug-x 10) debug-x")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 10, result.Int)
}

func TestEvalSingleInContext(t *testing.T) {
	env := newInspectorTestEnv(t)
	result := EvalSingleInContext(env, "(+ 1 2)")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 3, result.Int)
}

func TestEvalSingleInContext_MultiExpression(t *testing.T) {
	env := newInspectorTestEnv(t)
	// Pre-set a sentinel to verify the second expression is NOT evaluated.
	EvalInContext(env, "(set 'single-guard 0)")
	result := EvalSingleInContext(env, "(+ 1 2) (set 'single-guard 99)")
	require.NotNil(t, result)
	assert.Equal(t, lisp.LInt, result.Type)
	assert.Equal(t, 3, result.Int, "only the first expression should be evaluated")
	guard := EvalInContext(env, "single-guard")
	assert.Equal(t, 0, guard.Int, "second expression should not have been evaluated")
}

func TestFormatValueWith_CustomFormatter(t *testing.T) {
	type MyType struct{ Name string }

	eng := New(WithFormatters(map[string]VariableFormatter{
		"debugger.MyType": FormatterFunc(func(v any) string {
			m, ok := v.(MyType)
			if !ok {
				return "<bad>"
			}
			return "MyType:" + m.Name
		}),
	}))

	// LNative with registered formatter.
	native := lisp.Native(MyType{Name: "test"})
	assert.Equal(t, "MyType:test", FormatValueWith(native, eng))

	// LNative without registered formatter falls back to FormatValue.
	unregistered := lisp.Native(42)
	assert.Equal(t, "<native int>", FormatValueWith(unregistered, eng))

	// Non-native types always use FormatValue.
	assert.Equal(t, "42", FormatValueWith(lisp.Int(42), eng))

	// Nil engine falls back to FormatValue.
	assert.Equal(t, FormatValue(native), FormatValueWith(native, nil))
}

func TestFormatterFunc(t *testing.T) {
	f := FormatterFunc(func(v any) string { return "formatted" })
	assert.Equal(t, "formatted", f.FormatValue(nil))
	assert.Nil(t, f.Children(nil), "FormatterFunc should return nil children")
}

func TestInspectMacroExpansion_Nil(t *testing.T) {
	// Nil expression.
	assert.Nil(t, InspectMacroExpansion(nil))

	// Expression without macro expansion info.
	expr := lisp.Symbol("x")
	assert.Nil(t, InspectMacroExpansion(expr))
}

func TestInspectMacroExpansion_WithContext(t *testing.T) {
	callSite := &lisp.MacroExpansionContext{
		Name: "lisp:defun",
		Args: []*lisp.LVal{lisp.Symbol("my-fn"), lisp.SExpr([]*lisp.LVal{lisp.Symbol("x")})},
	}
	expr := lisp.Symbol("+")
	expr.MacroExpansion = &lisp.MacroExpansionInfo{
		MacroExpansionContext: callSite,
		ID:                   42,
	}

	bindings := InspectMacroExpansion(expr)
	require.NotNil(t, bindings)

	// Should have: (macro), arg[0], arg[1].
	assert.Len(t, bindings, 3)
	assert.Equal(t, "(macro)", bindings[0].Name)
	assert.Equal(t, "lisp:defun", bindings[0].Value.Str)

	// arg[0] is symbol "my-fn".
	assert.Equal(t, "arg[0]", bindings[1].Name)
	require.NotNil(t, bindings[1].Value)
	assert.Equal(t, lisp.LSymbol, bindings[1].Value.Type)
	assert.Equal(t, "my-fn", bindings[1].Value.Str)

	// arg[1] is the formals list (x).
	assert.Equal(t, "arg[1]", bindings[2].Name)
	require.NotNil(t, bindings[2].Value)
	assert.Equal(t, lisp.LSExpr, bindings[2].Value.Type)
	require.Len(t, bindings[2].Value.Cells, 1)
	assert.Equal(t, "x", bindings[2].Value.Cells[0].Str)
}

func TestInspectMacroExpansion_WithCallSite(t *testing.T) {
	ctx := &lisp.MacroExpansionContext{
		Name:     "lisp:defun",
		CallSite: &token.Location{File: "test.lisp", Line: 5, Col: 1},
	}
	expr := lisp.Symbol("+")
	expr.MacroExpansion = &lisp.MacroExpansionInfo{
		MacroExpansionContext: ctx,
		ID:                   1,
	}

	bindings := InspectMacroExpansion(expr)
	require.NotNil(t, bindings)

	// Should have: (macro), (call-site).
	assert.Len(t, bindings, 2)
	assert.Equal(t, "(macro)", bindings[0].Name)
	assert.Equal(t, "(call-site)", bindings[1].Name)
	assert.Contains(t, bindings[1].Value.Str, "test.lisp")
}

func TestInspectMacroExpansion_NilContext(t *testing.T) {
	expr := lisp.Symbol("+")
	expr.MacroExpansion = &lisp.MacroExpansionInfo{
		MacroExpansionContext: nil,
		ID:                   1,
	}
	assert.Nil(t, InspectMacroExpansion(expr))
}
