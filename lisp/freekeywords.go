// Copyright © 2026 The ELPS authors

package lisp

// FreeKeywords marks a Go builtin so that the keyword literals naming its
// &key arguments cost no evaluation step (luthersystems/elps#745).  It is
// strictly opt-in, per builtin: nothing changes for a builtin registered
// without it, for any Lisp function, or for any existing program.
//
// Today every argument of a call form is evaluated, and evaluating the
// keyword literal :k -- which just returns :k -- is one step, so
// (f x :a 1 :b 2) costs two steps more than a positional (f x 1 2).  For a
// builtin wrapped with FreeKeywords, a keyword literal written directly in a
// call form at a key-name position of its &key section is passed as itself,
// unevaluated and uncharged.  The value the builtin receives is identical
// (a keyword evaluates to itself), so only the step count differs.
//
// Precisely, the discount applies only when all of these hold:
//
//   - the call form's head evaluates to a function value created by
//     registering FreeKeywords(def) (AddBuiltins, BindBuiltins or an
//     elpsutil package); the flag lives on that function value, so every
//     binding, copy and template VM of it carries it identically;
//   - the argument expression is a bare, unquoted keyword symbol (:k), not
//     an expression producing one (a variable bound to :k, ':k, a call);
//   - its position is a key-name position: index r, r+2, r+4, ... of the
//     arguments, where r is the number of required formals.
//
// Everything else is evaluated and charged as always: values in the key
// section, keywords in required positions, and every argument of a call
// that reaches the builtin through funcall or apply (those arguments belong
// to funcall's call form).  A keyword literal the discount skips is also not
// seen by a debugger's per-expression hook.
//
// def's formals must be required names followed by &key and at least one key
// name, with no &optional or &rest; AddBuiltins panics and BindBuiltins
// returns an error otherwise.  Adopting FreeKeywords for an existing builtin
// changes the step count of every program calling it with keywords, so for a
// metered embedder it is a coordinated upgrade, like any step change; a NEW
// builtin can adopt it freely.
//
// The returned definition wraps def: its dynamic type is not def's, so a type
// assertion to def's concrete type fails on it, and reflection-based tooling
// that walks definitions sees one more level (the wrapped def is its
// embedded LBuiltinDef field).  Name, Formals, Eval and Docstring forward to
// def.
func FreeKeywords(def LBuiltinDef) LBuiltinDef {
	return freeKeywordsDef{def}
}

type freeKeywordsDef struct {
	LBuiltinDef
}

// Docstring forwards the wrapped definition's docstring.
func (d freeKeywordsDef) Docstring() string {
	return builtinDocstring(d.LBuiltinDef)
}

// freeKeywordsMarker is unexported so only FreeKeywords can opt a builtin in.
func (freeKeywordsDef) freeKeywordsMarker() {}

type freeKeywordsBuiltin interface {
	freeKeywordsMarker()
}

// freeKeysOf returns the funData.freeKeys value for def -- zero unless def
// was wrapped with FreeKeywords -- or a message when def is wrapped but its
// formals do not have the required shape.
func freeKeysOf(def LBuiltinDef) (int, string) {
	if _, ok := def.(freeKeywordsBuiltin); !ok {
		return 0, ""
	}
	formals := def.Formals()
	if formals == nil || formals.Type != LSExpr {
		return 0, "free-keyword builtin " + def.Name() + " must declare &key formals"
	}
	nreq := -1
	for i, f := range formals.Cells {
		if f == nil || f.Type != LSymbol {
			return 0, "free-keyword builtin " + def.Name() + " has a non-symbol formal"
		}
		switch f.Str {
		case KeyArgSymbol:
			if nreq >= 0 {
				return 0, "free-keyword builtin " + def.Name() + " declares &key twice"
			}
			nreq = i
		case OptArgSymbol, VarArgSymbol:
			return 0, "free-keyword builtin " + def.Name() + " may declare only required formals and &key"
		}
	}
	if nreq < 0 || nreq == len(formals.Cells)-1 {
		return 0, "free-keyword builtin " + def.Name() + " must declare at least one &key formal"
	}
	return nreq + 1, ""
}

// freeKeywordLiteral reports whether argument i of a call form whose head
// evaluated to f is a keyword literal the evaluator passes unevaluated.  The
// caller has already checked that expr is a symbol.  For any function not
// registered through FreeKeywords it is false after one field read.
func freeKeywordLiteral(f, expr *LVal, i int) bool {
	if expr.quoted || expr.spliced || len(expr.Str) == 0 || expr.Str[0] != ':' {
		return false
	}
	fd := f.funData()
	if fd == nil || fd.freeKeys == 0 {
		return false
	}
	off := i - (fd.freeKeys - 1)
	return off >= 0 && off%2 == 0
}
