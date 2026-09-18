// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// Errors are first-class VALUES in ELPS, so a Go macro may splice one into
// its expansion as data -- a table of conditions, a default a handler
// returns, a quoted literal.  The stamp's walk must carry it through like
// any other value node: it is not the walk's own failure signal.
func TestGoMacroExpansionCarriesErrorValue(t *testing.T) {
	for _, debugger := range []lisp.Debugger{nil, dormantDebugger{}} {
		name := "no debugger"
		if debugger != nil {
			name = "debugger attached"
		}
		t.Run(name, func(t *testing.T) {
			env := newGoMacroEnv(t)
			env.Runtime.Debugger = debugger
			errval := lisp.ErrorCondition("my-condition", errors.New("payload"))
			def := &goMacroDef{name: "m", formals: lisp.Formals(), fun: func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
				return lisp.SExpr([]*lisp.LVal{
					lisp.Symbol("lisp:quote"),
					lisp.SExpr([]*lisp.LVal{lisp.Int(1), errval, lisp.Int(3)}),
				})
			}}
			env.AddMacros(true, def)
			got := env.LoadString("go-macro.lisp", "(macroexpand-1 '(m))")
			if got.Type != lisp.LSExpr || len(got.Cells) != 2 {
				t.Fatalf("expansion %v: want the two-cell quote form the macro returned", got)
			}
			data := got.Cells[1]
			if data.Type != lisp.LSExpr || len(data.Cells) != 3 {
				t.Fatalf("expansion data %v: the error value truncated the walk", data)
			}
			if data.Cells[0].Int != 1 || data.Cells[2].Int != 3 {
				t.Fatalf("expansion data %v: want (1 <error> 3)", data)
			}
			if data.Cells[1].Type != lisp.LError || data.Cells[1].Str != "my-condition" {
				t.Fatalf("expansion data %v: the error value was not carried through", data)
			}
			// The stamp must still have reached the whole container: every
			// synthesized node carries the macro call site.
			for i, n := range []*lisp.LVal{got, data, data.Cells[1]} {
				if _, ok := n.Source(); !ok {
					t.Errorf("expansion node %d (%v) was not stamped", i, n)
				}
			}
		})
	}
}

// The walk's own failure is still reported: a Go macro returning an
// expansion nested past the value depth limit yields that error, not a
// truncated expansion.  This is the signal the LError type sniff used to
// stand in for.
func TestGoMacroExpansionDepthFailureIsReported(t *testing.T) {
	env := newGoMacroEnv(t)
	env.Runtime.MaxValueDepth = 1024
	def := &goMacroDef{name: "deep", formals: lisp.Formals(), fun: func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
		v := lisp.Int(1)
		for range 1100 {
			v = lisp.SExpr([]*lisp.LVal{lisp.Symbol("lisp:quote"), v})
		}
		return v
	}}
	env.AddMacros(true, def)
	got := env.LoadString("deep.lisp", "(macroexpand-1 '(deep))")
	if got.Type != lisp.LError {
		t.Fatalf("expansion %v: want the value depth error", got)
	}
	if msg := lisp.GoError(got).Error(); !strings.Contains(msg, "depth") {
		t.Fatalf("error %q: want the value depth error", msg)
	}
}
