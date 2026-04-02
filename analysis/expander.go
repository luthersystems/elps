// Copyright © 2026 The ELPS authors

package analysis

import (
	"fmt"
	"sync"

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
//
// Thread-safe: a mutex serializes expansion calls since MacroCall mutates
// shared Runtime state (call stack, package pointer). The mutex is only
// contended when multiple LSP handlers trigger concurrent analyses.
type EnvMacroExpander struct {
	Env *lisp.LEnv

	mu       sync.Mutex
	notMacro map[string]bool // cache: symbols that are not macros in the env
}

// ExpandMacro looks up the head symbol in the environment, verifies it is
// a macro, and calls MacroCall to expand it. Returns the expanded AST or
// nil on any failure. Results for non-macro symbols are cached to avoid
// repeated env lookups on every unresolved call.
func (e *EnvMacroExpander) ExpandMacro(form *lisp.LVal) (result *lisp.LVal) {
	defer func() {
		if r := recover(); r != nil {
			result = nil
		}
	}()

	if e.Env == nil || len(form.Cells) == 0 || form.Cells[0].Type != lisp.LSymbol {
		return nil
	}

	name := form.Cells[0].Str

	e.mu.Lock()
	defer e.mu.Unlock()

	// Fast path: already know this symbol is not a macro.
	if e.notMacro != nil && e.notMacro[name] {
		return nil
	}

	mac := e.Env.Get(form.Cells[0])
	if mac.Type != lisp.LFun || !mac.IsMacro() {
		// Cache the negative result to avoid future lookups.
		if e.notMacro == nil {
			e.notMacro = make(map[string]bool)
		}
		e.notMacro[name] = true
		return nil
	}

	args := lisp.SExpr(form.Cells[1:])
	mark := e.Env.MacroCall(mac, args)
	if mark.Type == lisp.LError || mark.Type != lisp.LMarkMacExpand {
		return nil
	}

	return mark.Cells[0]
}

// LoadWorkspaceMacros evaluates workspace defmacro AST nodes into the
// given environment so that EnvMacroExpander can expand them. A defmacro
// evaluation registers the macro in the env's package scope with no
// other side effects.
//
// Malformed macros are skipped — the returned errors slice contains one
// entry per failed defmacro with the macro name and cause. Callers
// should log these for visibility (e.g. as warnings in the LSP or CLI).
func LoadWorkspaceMacros(env *lisp.LEnv, defs []*lisp.LVal) []error {
	var errs []error
	for _, def := range defs {
		if err := loadOneMacro(env, def); err != nil {
			errs = append(errs, err)
		}
	}
	return errs
}

func loadOneMacro(env *lisp.LEnv, def *lisp.LVal) (retErr error) {
	defer func() {
		if r := recover(); r != nil {
			retErr = fmt.Errorf("panic loading macro %s: %v", macroDefName(def), r)
		}
	}()
	result := env.Eval(def)
	if result.Type == lisp.LError {
		return fmt.Errorf("error loading macro %s: %v", macroDefName(def), result)
	}
	return nil
}

// macroDefName extracts the name from a (defmacro name ...) AST node.
func macroDefName(def *lisp.LVal) string {
	if def != nil && len(def.Cells) > 1 && def.Cells[1].Type == lisp.LSymbol {
		return def.Cells[1].Str
	}
	return "<unknown>"
}
