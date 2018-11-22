// Copyright © 2018 The ELPS authors

package lisp

var userMacros []*langBuiltin
var langMacros = []*langBuiltin{
	{"defmacro", Formals("name", "formals", "expr"), macroDefmacro},
	{"defun", Formals("name", "formals", VarArgSymbol, "expr"), macroDefun},
	{"curry-function", Formals("fun", VarArgSymbol, "args"), macroCurryFun},
	// get-default is a macro because we only want to evaluate the expression
	// bound to default if the key doesn't exist in the map.
	{"get-default", Formals("map", "key", "default"), macroGetDefault},
	{"trace", Formals("expr", OptArgSymbol, "message"), macroTrace},
}

// RegisterDefaultMacro adds the given function to the list returned by
// DefaultMacros.
func RegisterDefaultMacro(name string, formals *LVal, fn LBuiltin) {
	userMacros = append(userMacros, &langBuiltin{name, formals.Copy(), fn})
}

// DefaultMacros returns the default set of LBuiltinDef added to LEnv objects
// when LEnv.AddMacros is called without arguments.
func DefaultMacros() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langMacros)+len(userMacros))
	for i := range langMacros {
		ops[i] = langMacros[i]
	}
	offset := len(langMacros)
	for i := range userMacros {
		ops[offset+i] = langMacros[i]
	}
	return ops
}

func macroDefmacro(env *LEnv, args *LVal) *LVal {
	sym, formals, bodyForms := args.Cells[0], args.Cells[1], args.Cells[2]
	if sym.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", sym.Type)
	}
	fun := env.Lambda(formals, []*LVal{bodyForms})
	if fun.Type == LError {
		fun.SetCallStack(env.Runtime.Stack.Copy())
		return fun
	}
	fun.FunType = LFunMacro // evaluate as a macro
	return SExpr([]*LVal{
		Symbol("lisp:progn"),
		SExpr([]*LVal{
			Symbol("lisp:set"),
			Quote(sym),
			fun,
		}),
		Nil(),
	})
}

func macroDefun(env *LEnv, args *LVal) *LVal {
	sym, formals, body := args.Cells[0], args.Cells[1], args.Cells[2:]
	if sym.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", sym.Type)
	}
	fun := env.Lambda(formals, body)
	if fun.Type == LError {
		fun.SetCallStack(env.Runtime.Stack.Copy())
		return fun
	}
	return SExpr([]*LVal{
		Symbol("lisp:progn"),
		SExpr([]*LVal{
			Symbol("lisp:set"),
			Quote(sym),
			fun,
		}),
		Nil(),
	})
}

func macroCurryFun(env *LEnv, args *LVal) *LVal {
	funExpr, argExprs := args.Cells[0], args.Cells[1:]
	argsym := env.GenSym()
	callCells := []*LVal{
		Symbol("lisp:apply"),
		funExpr,
	}
	callCells = append(callCells, argExprs...)
	callCells = append(callCells, argsym)
	return SExpr([]*LVal{
		Symbol("lambda"),
		SExpr([]*LVal{
			Symbol(VarArgSymbol),
			argsym,
		}),
		SExpr(callCells),
	})
}

func macroGetDefault(env *LEnv, args *LVal) *LVal {
	mapExpr, keyExpr, defExpr := args.Cells[0], args.Cells[1], args.Cells[2]
	mapSym, keySym := env.GenSym(), env.GenSym()
	let := QExpr([]*LVal{
		Symbol("lisp:let"),
		SExpr([]*LVal{
			SExpr([]*LVal{
				mapSym,
				mapExpr,
			}),
			SExpr([]*LVal{
				keySym,
				keyExpr,
			}),
		}),
		SExpr([]*LVal{
			Symbol("lisp:if"),
			SExpr([]*LVal{
				Symbol("lisp:key?"),
				mapSym,
				keySym,
			}),
			SExpr([]*LVal{
				Symbol("lisp:get"),
				mapSym,
				keySym,
			}),
			defExpr,
		}),
	})
	return let
}

func isUnquote(v *LVal) bool {
	if v.Type != LSExpr {
		return false
	}
	if len(v.Cells) < 1 {
		return false
	}
	if v.Cells[0].Type != LSymbol {
		return false
	}
	return v.Cells[0].Str == "unquote" || v.Cells[0].Str == "unquote-splicing"
}

// NOTE:  There are almost surely bugs with this implementation un unquoting,
// especially when dealing with complex macros using multiple levels of
// quasiquotes/unquotes.  I need to find these examples, understand how they
// are supposed to work, and then fix these test cases.
func findAndUnquote(env *LEnv, v *LVal, depth int) *LVal {
	if v.Type != LSExpr {
		return v
	}
	if isUnquote(v) {
		spliceType := v.Cells[0].Str
		if len(v.Cells) != 2 {
			return env.Errorf("%v: one argument expected (got %d)", v.Cells[0].Str, len(v.Cells)-1)
		}
		// The v looks like ``(unquote EXPR)'' or ``(unquote-splicing expr)''
		x := env.Eval(v.Cells[1])
		if spliceType == "unquote-splicing" {
			if depth == 0 {
				return env.Errorf("unquote-splicing used in an invalid context")
			}
			x = Splice(x)
		}
		return x
	}
	// findAndUnquote all child expressions
	cells := make([]*LVal, v.Len())
	for i := range v.Cells {
		cells[i] = findAndUnquote(env, v.Cells[i], depth+1)
		if cells[i].Type == LError {
			return cells[i]
		}
	}
	// splice in grandchildren of children that were unquoted with
	// ``unquote-splicing''
	for i := len(cells) - 1; i >= 0; i-- {
		if cells[i].Spliced {
			splice := cells[i]
			if splice.Type != LSExpr {
				// TODO:  I believe it is incorrect to error out here.  But
				// splicing non-lists is not a major concern at the moment.
				return env.Errorf("%s: cannot splice non-list: %s", "unquote-splicing", splice.Type)
			}
			// TODO:  Be clever and don't force allocation here.  Grow newcells and shift v.Cells[i+1:]
			newcells := make([]*LVal, 0, len(v.Cells)+len(splice.Cells)-1)
			newcells = append(newcells, cells[:i]...)
			newcells = append(newcells, splice.Cells...)
			newcells = append(newcells, cells[i+1:]...)
			cells = newcells
		}
	}
	expr := SExpr(cells)
	expr.Quoted = v.Quoted
	return expr
}

func macroTrace(env *LEnv, args *LVal) *LVal {
	expr, msg := args.Cells[0], args.Cells[1]
	sym := env.GenSym()
	if msg.IsNil() {
		msg = String("TRACE")
	}
	return SExpr([]*LVal{
		Symbol("lisp:let"),
		SExpr([]*LVal{
			SExpr([]*LVal{sym, expr})},
		),
		SExpr([]*LVal{Symbol("lisp:debug-print"), msg, sym}),
		sym,
	})
}
