// Copyright © 2024 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/lisp"
)

// MacroExpander expands a macro call form at analysis time.
// The form includes the macro name as Cells[0] and arguments as Cells[1:].
// Returns the expanded AST, or nil if expansion is not possible (macro not
// found, expansion error, wrong arity, etc.). Returning nil causes the
// analyzer to fall back to treating the call as an opaque macro invocation.
type MacroExpander interface {
	ExpandMacro(form *lisp.LVal) *lisp.LVal
}

// EnvMacroExpander uses a live LEnv to expand user-defined macros.
// Expansion errors and panics are caught — the analyzer falls back to
// opaque analysis when ExpandMacro returns nil.
type EnvMacroExpander struct {
	Env *lisp.LEnv
}

// ExpandMacro looks up the head symbol in the environment, verifies it is
// a macro, and calls MacroCall to expand it. Returns the expanded AST or
// nil on any failure.
func (e *EnvMacroExpander) ExpandMacro(form *lisp.LVal) (result *lisp.LVal) {
	defer func() {
		if r := recover(); r != nil {
			result = nil
		}
	}()

	if e.Env == nil || len(form.Cells) == 0 || form.Cells[0].Type != lisp.LSymbol {
		return nil
	}

	mac := e.Env.Get(form.Cells[0])
	if mac.Type != lisp.LFun || !mac.IsMacro() {
		return nil
	}

	args := lisp.SExpr(form.Cells[1:])
	mark := e.Env.MacroCall(mac, args)
	if mark.Type == lisp.LError || mark.Type != lisp.LMarkMacExpand {
		return nil
	}

	return mark.Cells[0]
}
