// Copyright © 2024 The ELPS authors

package analysis

import "github.com/luthersystems/elps/lisp"

// builtinDocumented is the interface for builtins that have docstrings.
type builtinDocumented interface {
	Docstring() string
}

// populateBuiltins adds all known builtin functions, special operators,
// and macros to the given scope.
func populateBuiltins(scope *Scope) {
	for _, b := range lisp.DefaultBuiltins() {
		sym := &Symbol{
			Name:      b.Name(),
			Kind:      SymBuiltin,
			Signature: signatureFromFormals(b.Formals()),
		}
		if doc, ok := b.(builtinDocumented); ok {
			sym.DocString = doc.Docstring()
		}
		scope.Define(sym)
	}

	for _, op := range lisp.DefaultSpecialOps() {
		sym := &Symbol{
			Name:      op.Name(),
			Kind:      SymSpecialOp,
			Signature: signatureFromFormals(op.Formals()),
		}
		if doc, ok := op.(builtinDocumented); ok {
			sym.DocString = doc.Docstring()
		}
		scope.Define(sym)
	}

	for _, m := range lisp.DefaultMacros() {
		sym := &Symbol{
			Name:      m.Name(),
			Kind:      SymMacro,
			Signature: signatureFromFormals(m.Formals()),
		}
		if doc, ok := m.(builtinDocumented); ok {
			sym.DocString = doc.Docstring()
		}
		scope.Define(sym)
	}

	// Define well-known symbols: true, false, nil
	for _, name := range []string{"true", "false"} {
		scope.Define(&Symbol{
			Name: name,
			Kind: SymVariable,
		})
	}
}

// signatureFromFormals creates a Signature from a formals LVal.
func signatureFromFormals(formals *lisp.LVal) *Signature {
	params := lisp.ParseFormals(formals)
	if params == nil {
		return &Signature{}
	}
	return &Signature{Params: params}
}
