// Copyright © 2026 The ELPS authors

package analysis

import (
	"fmt"
	"sync"

	"github.com/luthersystems/elps/astutil"
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
	// NOTE: notMacro assumes the env's macro set is stable for this
	// expander's lifetime. If macros are added to the env after creation
	// (e.g. LoadWorkspaceMacros), create a new EnvMacroExpander or call
	// Reset() to clear the cache.
}

// Reset clears the not-a-macro cache. Call this after loading new macros
// into the env (e.g. via LoadWorkspaceMacros) if the expander is reused.
func (e *EnvMacroExpander) Reset() {
	e.mu.Lock()
	e.notMacro = nil
	e.mu.Unlock()
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

// LoadWorkspaceMacros replays workspace preamble forms (in-package,
// use-package, export, defmacro) into the given environment. This mirrors
// what the runtime's (load) does — forms are eval'd in source order so
// package context, imports, and macro definitions build up naturally.
//
// Workspace packages that don't exist in the boot env are auto-created.
// The env's active package is saved and restored after loading.
//
// Malformed forms are skipped — the returned errors slice contains one
// entry per failure. Callers should log these for visibility.
func LoadWorkspaceMacros(env *lisp.LEnv, preamble []*lisp.LVal) []error {
	// Save and restore the env's active package, same as env.load().
	currPkg := env.Runtime.Package
	defer func() { env.Runtime.Package = currPkg }()

	var errs []error
	for _, form := range preamble {
		if err := evalPreambleForm(env, form); err != nil {
			errs = append(errs, err)
		}
	}
	return errs
}

func evalPreambleForm(env *lisp.LEnv, form *lisp.LVal) (retErr error) {
	defer func() {
		if r := recover(); r != nil {
			retErr = fmt.Errorf("panic in preamble form: %v", r)
		}
	}()
	// Auto-create workspace packages so in-package doesn't fail.
	// Import the lang package so builtins (defmacro, use-package, etc.) work.
	head := astutil.HeadSymbol(form)
	if head == "in-package" && len(form.Cells) > 1 {
		if name := preamblePkgName(form.Cells[1]); name != "" {
			env.Runtime.Registry.DefinePackage(name)
			// Temporarily switch to the new package to import lang builtins,
			// then let the actual (in-package ...) eval do the real switch.
			env.InPackage(lisp.String(name))
			env.UsePackage(lisp.Symbol(env.Runtime.Registry.Lang))
		}
	}
	result := env.Eval(form)
	if result.Type == lisp.LError {
		if head == "defmacro" {
			return fmt.Errorf("error loading macro %s: %v", preambleDefName(form), result)
		}
		return fmt.Errorf("error in preamble (%s): %v", head, result)
	}
	return nil
}

// preamblePkgName extracts a package name from a quoted or bare symbol.
func preamblePkgName(v *lisp.LVal) string {
	if v.Type == lisp.LSymbol {
		return v.Str
	}
	// (quote sym) or 'sym
	if v.Type == lisp.LSExpr && v.Quoted && len(v.Cells) > 0 && v.Cells[0].Type == lisp.LSymbol {
		return v.Cells[0].Str
	}
	return ""
}

// preambleDefName extracts the name from a (defmacro name ...) AST node.
func preambleDefName(def *lisp.LVal) string {
	if def != nil && len(def.Cells) > 1 && def.Cells[1].Type == lisp.LSymbol {
		return def.Cells[1].Str
	}
	return "<unknown>"
}
