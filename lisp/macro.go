// Copyright © 2018 The ELPS authors

package lisp

import "fmt"

var userMacros []*langBuiltin
var langMacros = []*langBuiltin{
	{"defmacro", Formals("name", "formals", "expr"), macroDefmacro, ""},
	{"defun", Formals("name", "formals", VarArgSymbol, "expr"), macroDefun,
		`Defines a named function in the current package.`},
	{"deftype", Formals("name", "constructor-formals", VarArgSymbol, "constructor-exprs"), macroDeftype, ""},
	{"curry-function", Formals("fun", VarArgSymbol, "args"), macroCurryFun, ""},
	// get-default is a macro because we only want to evaluate the expression
	// bound to default if the key doesn't exist in the map.
	{"get-default", Formals("map", "key", "default"), macroGetDefault, ""},
	{"trace", Formals("expr", OptArgSymbol, "message"), macroTrace, ""},
}

// RegisterDefaultMacro adds the given function to the list returned by
// DefaultMacros.
func RegisterDefaultMacro(name string, formals *LVal, fn LBuiltin) {
	userMacros = append(userMacros, &langBuiltin{name, formals.Copy(), fn, ""})
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

func macroDeftype(env *LEnv, args *LVal) *LVal {
	pkg := env.Runtime.Registry.Lang
	psymbol := func(s string) *LVal {
		return Symbol(fmt.Sprintf("%s:%s", pkg, s))
	}
	name := args.Cells[0]
	formals := args.Cells[1]
	exprs := SExpr(args.Cells[2:])
	if name.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", GetType(name))
	}
	if formals.Type != LSExpr {
		return env.Errorf("second argument is not a list: %v", GetType(formals))
	}
	fqname := env.GenSym()
	lambda := SExpr([]*LVal{
		psymbol("lambda"),
		formals,
	})
	lambda.Cells = append(lambda.Cells, exprs.Cells...)
	return QExpr([]*LVal{
		psymbol("let*"),
		SExpr([]*LVal{
			QExpr([]*LVal{
				fqname,
				SExpr([]*LVal{
					psymbol("qualified-symbol"),
					name,
				}),
			}),
		}),
		SExpr([]*LVal{
			psymbol("set"),
			fqname,
			SExpr([]*LVal{
				psymbol("new"),
				psymbol("typedef"),
				fqname,
				lambda,
			}),
		}),
		fqname,
	})
}

type unquoteType int

const (
	unquoteNone unquoteType = iota
	unquoteValue
	unquoteSpliced
	unquoteInvalid
)

func getUnquoteType(v *LVal) (unquoteType, error) {
	if v.Type != LSExpr {
		return unquoteNone, nil
	}
	if len(v.Cells) < 1 {
		return unquoteNone, nil
	}
	if v.Cells[0].Type != LSymbol {
		return unquoteNone, nil
	}
	if v.Cells[0].Str == "unquote" {
		if len(v.Cells) != 2 {
			return unquoteValue, fmt.Errorf("%s: one argument expected (got %d)", v.Cells[0].Str, len(v.Cells)-1)
		}
		return unquoteValue, nil
	}
	if v.Cells[0].Str == "unquote-splicing" {
		if len(v.Cells) != 2 {
			return unquoteSpliced, fmt.Errorf("%s: one argument expected (got %d)", v.Cells[0].Str, len(v.Cells)-1)
		}
		return unquoteSpliced, nil
	}
	return unquoteNone, nil
}

func findAndUnquote(env *LEnv, v *LVal, depth int) *LVal {
	inner := v
	quoteLevel := 0
	if inner.Quoted {
		quoteLevel += 1
	}
	for inner.Type == LQuote {
		quoteLevel += 1
		inner = inner.Cells[0]
	}
	if inner.Type != LSExpr {
		// back out of the entire quote chain and return v to leave the value
		// unchanged in the quasiquote.
		return v
	}
	v = inner

	unquote, err := getUnquoteType(v)
	if err != nil {
		return env.Error(err)
	}
	if unquote == unquoteSpliced {
		// v looks like ``(unquote-splicing expr)''
		expr := v.Cells[1]
		if depth == 0 || quoteLevel > 0 {
			return env.Errorf("unquote-splicing used in an invalid context")
		}
		return doUnquoteSpliced(env, expr)
	}
	if unquote == unquoteValue {
		// v looks like ``(unquote expr)''
		return doUnquoteValue(env, v.Cells[1], quoteLevel)
	}
	return doUnquoteSExpr(env, v, depth, quoteLevel)
}

func doUnquoteSpliced(env *LEnv, v *LVal) *LVal {
	x := env.Eval(v)
	if x.Type == LError {
		return x
	}
	x = Splice(x)
	return x
}

func doUnquoteValue(env *LEnv, v *LVal, quoteLevel int) *LVal {
	x := env.Eval(v)
	if x.Type == LError {
		return x
	}
	for i := 0; i < quoteLevel; i++ {
		x = Quote(x)
	}
	return x
}

func doUnquoteSExpr(env *LEnv, v *LVal, depth int, quoteLevel int) *LVal {
	// findAndUnquote all child expressions
	numSpliced := 0
	numExtended := 0
	cells := make([]*LVal, v.Len())
	for i := range v.Cells {
		cells[i] = findAndUnquote(env, v.Cells[i], depth+1)
		if cells[i].Type == LError {
			return cells[i]
		}
		if cells[i].Spliced {
			numSpliced += 1
			numExtended += len(cells[i].Cells)
		}
	}
	// splice in children of children that were unquoted with
	// ``unquote-splicing''
	if numSpliced > 0 {
		newlen := len(cells) - numSpliced + numExtended
		newcells := make([]*LVal, 0, newlen)
		for _, v := range cells {
			if v.Spliced {
				if v.Type != LSExpr {
					// TODO:  I believe it is incorrect to error out here.  But
					// splicing non-lists is not a major concern at the moment.
					return env.Errorf("%s: cannot splice non-list: %s", "unquote-splicing", v.Type)
				}
				newcells = append(newcells, v.Cells...)
			} else {
				newcells = append(newcells, v)
			}
		}
		cells = newcells
	}
	expr := SExpr(cells)
	expr.Source = v.Source
	for i := 0; i < quoteLevel; i++ {
		expr = Quote(expr)
	}
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
