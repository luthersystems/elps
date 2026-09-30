// Copyright © 2026 The ELPS authors

package lisp

import (
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func locatedSym(name string, line int) *LVal {
	v := Symbol(name)
	v.source = &token.Location{File: "user.lisp", Line: line, Col: 3, Pos: 10 * line}
	return v
}

// getDefaultTestArgs is (get-default m "k" 0) with each argument on its own
// line, as parsed source would locate them.
func getDefaultTestArgs() *LVal {
	key, def := String("k"), Int(0)
	key.source = &token.Location{File: "user.lisp", Line: 3, Col: 3, Pos: 30}
	def.source = &token.Location{File: "user.lisp", Line: 4, Col: 3, Pos: 40}
	return SExpr([]*LVal{locatedSym("m", 2), key, def})
}

func findKeyCall(t *testing.T, v *LVal) *LVal {
	t.Helper()
	// (lisp:let (...) (lisp:if (lisp:if nil-check false KEY-CALL) ...))
	outerIf := v.Cells[2]
	innerIf := outerIf.Cells[1]
	keyCall := innerIf.Cells[3]
	require.Equal(t, "lisp:key?", keyCall.Cells[0].Str)
	return keyCall
}

// Only the key? call, which rejects a non-map value, is located at the map
// form; the let and if wrappers keep the macro call site so stack notes
// still name the call.
func TestGetDefaultLocatesOnlyKeyCall(t *testing.T) {
	env := NewEnv(nil)
	args := getDefaultTestArgs()
	callSite := &token.Location{File: "user.lisp", Line: 1, Col: 1, Pos: 0}
	r := macroGetDefault(env, args)
	locateExpansionTree(r, callSite, args.Cells)
	loc, ok := r.Source()
	require.True(t, ok)
	assert.Equal(t, 1, loc.Line, "let wrapper keeps the call site")
	assert.Equal(t, 1, r.Cells[2].source.Line, "if wrapper keeps the call site")
	keyCall := findKeyCall(t, r)
	assert.Equal(t, 2, keyCall.source.Line, "key? call is located at the map form")
	assert.Equal(t, 2, keyCall.Cells[0].source.Line)
}

// Under a debugger every node a Go macro created carries expansion
// metadata, including nodes ExpandAt already located; they keep their own
// location.  The macro's arguments are the caller's and are not copied.
func TestGoMacroExpandAtKeepsDebuggerMetadata(t *testing.T) {
	rt := StandardRuntime()
	callSite := &token.Location{File: "user.lisp", Line: 1, Col: 1, Pos: 0}
	ctx := &macroExpansionContext{CallSite: callSite, Name: "lisp:get-default"}

	t.Run("reviewer case: ExpandAt root", func(t *testing.T) {
		at := locatedSym("at", 7)
		arg := locatedSym("arg", 8)
		tmpl := MustFormTemplate(`(lisp:list ,arg (lisp:car ,arg))`, "arg")
		got := stampGoMacroExpansion(tmpl.ExpandAt(at, arg), callSite, ctx, rt, []*LVal{arg})
		_, ok := got.MacroExpansion()
		assert.True(t, ok, "root created by ExpandAt must carry expansion metadata")
		assert.Equal(t, 7, got.source.Line, "and keep its ExpandAt location")
		assert.Same(t, arg, got.Cells[1], "arguments are not copied")
	})

	t.Run("get-default", func(t *testing.T) {
		args := getDefaultTestArgs()
		got := stampGoMacroExpansion(macroGetDefault(NewEnv(nil), args), callSite, ctx, rt, args.Cells)
		isArg := map[*LVal]bool{}
		for _, a := range args.Cells {
			isArg[a] = true
		}
		var walk func(*LVal)
		walk = func(v *LVal) {
			if isArg[v] || isSingleton(v) {
				return
			}
			_, ok := v.MacroExpansion()
			assert.True(t, ok, "generated node %v lacks expansion metadata", v)
			for _, c := range v.Cells {
				walk(c)
			}
		}
		walk(got)
		assert.Equal(t, 2, findKeyCall(t, got).source.Line)
	})
}

func TestFormTemplateExpandAtSyntheticLocation(t *testing.T) {
	at := Symbol("at")
	at.source = &token.Location{File: "<native code>", Pos: -1}
	tmpl := MustFormTemplate(`(f (g x))`)
	got := tmpl.ExpandAt(at)
	assert.Nil(t, got.source, "a synthetic location (Pos < 0) is not a location")
	assert.Nil(t, got.Cells[1].source)
}

func TestFormTemplateExpandAtQuotedArgKeepsLocation(t *testing.T) {
	at, arg := locatedSym("at", 7), locatedSym("arg", 8)
	got := MustFormTemplate(`(f ',arg 'sym)`, "arg").ExpandAt(at, arg)
	assert.Equal(t, 8, got.Cells[1].source.Line, "quoting a located argument keeps its location")
	assert.Equal(t, 7, got.Cells[2].source.Line, "a template-created quote takes at's location")
}
