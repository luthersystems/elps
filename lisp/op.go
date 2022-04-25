// Copyright © 2018 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

var userSpecialOps []*langBuiltin
var langSpecialOps = []*langBuiltin{
	{"function", Formals("name"), opFunction, ""},
	{"set!", Formals("name", "expr"), opSetUpdate, ""},
	{"assert", Formals("expr", VarArgSymbol, "message-format-args"), opAssert, ""},
	{"quote", Formals("expr"), opQuote, ""},
	{"quasiquote", Formals("expr"), opQuasiquote, ""},
	{"lambda", Formals("formals", VarArgSymbol, "expr"), opLambda,
		`Returns an anonymous function.`},
	{"expr", Formals("pattern"), opExpr, ""},
	{"thread-first", Formals("value", VarArgSymbol, "exprs"), opThreadFirst, ""},
	{"thread-last", Formals("value", VarArgSymbol, "exprs"), opThreadLast, ""},
	{"dotimes", Formals("control-sequence", VarArgSymbol, "exprs"), opDoTimes, ""},
	{"labels", Formals("bindings", VarArgSymbol, "expr"), opLabels, ""},
	{"macrolet", Formals("bindings", VarArgSymbol, "expr"), opMacrolet, ""},
	{"flet", Formals("bindings", VarArgSymbol, "expr"), opFlet, ""},
	{"let*", Formals("bindings", VarArgSymbol, "expr"), opLetSeq, ""},
	{"let", Formals("bindings", VarArgSymbol, "expr"), opLet, ""},
	{"progn", Formals(VarArgSymbol, "expr"), opProgn, ""},
	{"handler-bind", Formals("bindings", VarArgSymbol, "forms"), opHandlerBind, ""},
	{"ignore-errors", Formals(VarArgSymbol, "exprs"), opIgnoreErrors, ""},
	{"cond", Formals(VarArgSymbol, "branch"), opCond, ""},
	{"if", Formals("condition", "then", "else"), opIf, ""},
	{"or", Formals(VarArgSymbol, "expr"), opOr, ""},
	{"and", Formals(VarArgSymbol, "expr"), opAnd, ""},
	{"qualified-symbol", Formals("symbol"), opQualifiedSymbol, ""},
}

// RegisterDefaultSpecialOp adds the given function to the list returned by
// DefaultSpecialOps.
func RegisterDefaultSpecialOp(name string, formals *LVal, fn LBuiltin) {
	userSpecialOps = append(userSpecialOps, &langBuiltin{name, formals.Copy(), fn, ""})
}

// DefaultSpecialOps returns the default set of LBuiltinDef added to LEnv
// objects when LEnv.AddSpecialOps is called without arguments.
func DefaultSpecialOps() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langSpecialOps)+len(userSpecialOps))
	for i := range langSpecialOps {
		ops[i] = langSpecialOps[i]
	}
	offset := len(langSpecialOps)
	for i := range userSpecialOps {
		ops[offset+i] = langSpecialOps[i]
	}
	return ops
}

func opFunction(env *LEnv, args *LVal) *LVal {
	name := args.Cells[0]
	return env.GetFun(name)
}

func opSetUpdate(env *LEnv, args *LVal) *LVal {
	key, expr := args.Cells[0], args.Cells[1]
	if key.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", key.Type)
	}
	val := env.Eval(expr)
	if val.Type == LError {
		return val
	}
	return env.Update(key, val)
}

func opAssert(env *LEnv, args *LVal) *LVal {
	test := args.Cells[0]
	var formatStr *LVal
	var formatArgs []*LVal
	if len(args.Cells) > 1 {
		formatStr = args.Cells[1]
		formatArgs = args.Cells[2:]
		if formatStr.Type != LString {
			return env.Errorf("second argument is not a string: %v", formatStr.Type)
		}
	}
	ok := env.Eval(test.Copy())
	if ok.Type == LError {
		return ok
	}
	if True(ok) {
		return Nil()
	}
	if formatStr == nil {
		return env.Errorf("assertion failure: %s", test)
	}
	for i := range formatArgs {
		formatArgs[i] = env.Eval(formatArgs[i])
		if formatArgs[i].Type == LError {
			return formatArgs[i]
		}
	}
	msg := builtinFormatString(env, SExpr(args.Cells[1:]))
	if msg.Type == LError {
		return msg
	}
	return env.Error(errors.New(msg.Str))
}

func opQuote(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) != 1 {
		return env.Errorf("one argument expected (got %d)", len(args.Cells))
	}
	// NOTE:  Racket seems to detect nested (quote ...) expressions when
	// quoting things.  That is, (quote (quote 3)) in Racket evaluates to ''3,
	// not '(quote 3).  We could try to dig into the quoted arguments to
	// determine if that were possible but it is unclear whether it's possible
	// for ``quote'' to resolve differently or for this macro to be called
	// under a different name.
	return Quote(args.Cells[0])
}

func opQuasiquote(env *LEnv, args *LVal) *LVal {
	// We need to find and unquote values in expr (possibly expr itself).
	expr := args.Cells[0]

	result := findAndUnquote(env, expr, 0)
	if result.Type == LError {
		return result
	}

	// quasiquote will always return a quoted result.  It is a quoting
	// operation overall.
	return Quote(result)
}

func opLambda(env *LEnv, args *LVal) *LVal {
	formals, body := args.Cells[0], args.Cells[1:]
	for _, sym := range formals.Cells {
		if sym.Type != LSymbol {
			return env.Errorf("first argument contains a non-symbol: %v", sym.Type)
		}
	}
	// Construct the LVal and add env to the LEnv chain to get lexical scoping
	// (I think... -bmatsuo)
	lval := env.Lambda(formals, body)
	if lval.Type == LError {
		lval.SetCallStack(env.Runtime.Stack.Copy())
	}
	return lval
}

func opExpr(env *LEnv, args *LVal) *LVal {
	if args.Len() != 1 {
		return env.Errorf("one argument expected (got %d)", args.Len())
	}
	body := args.Cells[0]
	n, short, nopt, vargs, err := countExprArgs(body)
	if err != nil {
		return env.Errorf("%s", err)
	}
	formals := SExpr(nil)
	if short {
		formals.Cells = make([]*LVal, 1, 3)
		formals.Cells[0] = Symbol("%")
	} else {
		formals.Cells = make([]*LVal, n, n+2)
		for i := range formals.Cells {
			formals.Cells[i] = Symbol(fmt.Sprintf("%%%d", i+1))
		}
	}
	if nopt == 1 {
		formals.Cells = append(formals.Cells, Symbol(OptArgSymbol), Symbol("%"+OptArgSymbol))
	} else {
		// multple optional args aren't supported currently
	}
	if vargs {
		formals.Cells = append(formals.Cells, Symbol(VarArgSymbol), Symbol("%"+VarArgSymbol))
	}
	return env.Lambda(formals, []*LVal{body})
}

func countExprArgs(expr *LVal) (nargs int, short bool, nopt int, vargs bool, err error) {
	if expr.Quoted {
		return 0, false, 0, false, nil
	}
	switch expr.Type {
	case LSymbol:
		if !strings.HasPrefix(expr.Str, "%") {
			return 0, false, 0, false, nil
		}
		numStr := expr.Str[1:]
		if numStr == "" {
			return 1, true, 0, false, nil
		}
		if numStr == VarArgSymbol {
			return 0, false, 0, true, nil
		}
		if numStr == OptArgSymbol {
			// multple optional args aren't supported currently
			return 0, false, 1, false, nil
		}
		num, err := strconv.Atoi(numStr)
		if err != nil {
			return 0, false, 0, false, fmt.Errorf("invalid expr argument symbol: %s", expr.Str)
		}
		return num, false, 0, false, nil
	case LSExpr:
		short := false
		for _, cell := range expr.Cells {
			if cell.Quoted {
				continue
			}
			if !strings.HasPrefix(cell.Str, "%") {
				continue
			}
			numStr := cell.Str[1:]
			if numStr == "" {
				if !short {
					if nargs > 0 {
						err := fmt.Errorf("invalid mixing of expr argument symbols: %s and %s",
							fmt.Sprintf("%%%d", nargs),
							cell.Str)
						return 0, false, 0, false, err
					}
					short = true
				}
				continue
			}
			if numStr == OptArgSymbol {
				nopt = 1 // multple optional args aren't supported currently
				continue
			}
			if numStr == VarArgSymbol {
				vargs = true
				continue
			}
			if strings.HasPrefix(numStr, MetaArgPrefix) {
				return 0, false, 0, false, fmt.Errorf("invalid expr argument symbol: %v", expr.Str)
			}
			num, err := strconv.Atoi(numStr)
			if err != nil {
				return 0, false, 0, false, fmt.Errorf("invalid expr argument symbol: %s", expr.Str)
			}
			if short {
				err := fmt.Errorf("invalid mix of expr argument symbols: %s and %s", "%", cell.Str)
				return 0, false, 0, false, err
			}
			if num > nargs {
				nargs = num
			}
		}
		return nargs, short, nopt, vargs, nil
	case LInt, LFloat, LString:
		return 0, false, 0, false, nil
	default:
		return 0, false, 0, false, fmt.Errorf("invalid internal expression type: %s", expr.Type)
	}
}

func opThreadLast(env *LEnv, args *LVal) *LVal {
	val, exprs := args.Cells[0], args.Cells[1:]
	for _, expr := range exprs {
		if expr.Type != LSExpr || expr.Quoted {
			return env.Errorf("expression argument is not a function call")
		}
		if expr.Len() < 1 {
			return env.Errorf("expression argument is nil")
		}
	}
	if len(exprs) == 0 {
		return env.Terminal(val)
	}
	for i, expr := range exprs {
		cells := make([]*LVal, 0, len(expr.Cells)+1)
		cells = append(cells, expr.Cells...)
		cells = append(cells, val)
		if i == len(exprs)-1 {
			return env.Terminal(SExpr(cells))
		}
		val = env.Eval(SExpr(cells))
		if val.Type == LError {
			return val
		}
	}
	return val
}

func opThreadFirst(env *LEnv, args *LVal) *LVal {
	val, exprs := args.Cells[0], args.Cells[1:]
	for _, expr := range exprs {
		if expr.Type != LSExpr || expr.Quoted {
			return env.Errorf("expression argument is not a function call")
		}
		if expr.Len() < 1 {
			return env.Errorf("expression argument is nil")
		}
	}
	if len(exprs) == 0 {
		return env.Terminal(val)
	}
	for i, expr := range exprs {
		cells := make([]*LVal, 0, len(expr.Cells)+1)
		cells = append(cells, expr.Cells[0])
		cells = append(cells, val)
		cells = append(cells, expr.Cells[1:]...)
		if i == len(exprs)-1 {
			return env.Terminal(SExpr(cells))
		}
		val = env.Eval(SExpr(cells))
		if val.Type == LError {
			return val
		}
	}
	return val
}

func opFlet(env *LEnv, args *LVal) *LVal {
	fletenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return env.Errorf("first argument is not a list: %s", bindlist.Type)
	}
	for _, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of function definitions")
		}
		if len(bind.Cells) < 2 {
			return env.Errorf("first argument is not a list of function definitions")
		}
		fenv := NewEnv(env) // lambdas in a flet get their own little environment
		name, formals, body := bind.Cells[0], bind.Cells[1], bind.Cells[2:]
		lval := fenv.Lambda(formals, body)
		if lval.Type == LError {
			return lval
		}
		lerr := fletenv.Put(name, lval)
		if lerr.Type == LError {
			return lerr
		}
	}
	return opProgn(fletenv, args)
}

func opDoTimes(env *LEnv, args *LVal) *LVal {
	ctrlseq := args.Cells[0]
	body := args.Cells[1:]
	if ctrlseq.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", ctrlseq.Type)
	}
	if ctrlseq.Len() > 3 {
		return env.Errorf("too many elements in control-sequence: %d", ctrlseq.Len())
	}
	if ctrlseq.Len() == 0 {
		return env.Errorf("missing symbol in control-sequence")
	}
	symbol := ctrlseq.Cells[0]
	if symbol.Type != LSymbol {
		return env.Errorf("control-sequence does not start with a symbol: %v", symbol.Type)
	}
	if ctrlseq.Len() < 2 {
		return env.Errorf("missing iteration count in control-sequence")
	}
	countexpr := ctrlseq.Cells[1]
	returnexpr := Nil()
	if ctrlseq.Len() == 3 {
		returnexpr = ctrlseq.Cells[2]
	}

	count := env.Eval(countexpr)
	if count.Type == LError {
		return count
	}
	if count.Type != LInt {
		return env.Errorf("count did not evaluate to an int: %v", count.Type)
	}
	loopenv := NewEnv(env)
	n := 0
	for i := 0; i < count.Int; i++ {
		n++
		lerr := loopenv.Put(symbol, Int(i))
		if lerr.Type == LError {
			return lerr
		}
		for _, expr := range body {
			result := loopenv.Eval(expr)
			if result.Type == LError {
				return result
			}
		}
	}
	lerr := loopenv.Put(symbol, Int(n))
	if lerr.Type == LError {
		return lerr
	}
	return loopenv.Terminal(returnexpr)
}

// NOTE: Labels is similar to what you might image a flet* being but flet* is
// not appropriate because the defined lambdas can have "forward references" in
// labels.
//		(labels [(f1 () (f2))
//		         (f2 () (debug-print "f2"))])
//		  (f1))
func opLabels(env *LEnv, args *LVal) *LVal {
	fletenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return env.Errorf("first argument is not a list: %s", bindlist.Type)
	}
	for _, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of function definitions")
		}
		if len(bind.Cells) < 2 {
			return env.Errorf("first argument is not a list of function definitions")
		}
		name, formals, body := bind.Cells[0], bind.Cells[1], bind.Cells[2:]
		// The lambda's lexical scope includes all lambdas that labels defines.
		lval := fletenv.Lambda(formals, body)
		if lval.Type == LError {
			return lval
		}
		// Bind name for the function body and to allow cross-references
		// between label lambdas.
		lerr := fletenv.Put(name, lval)
		if lerr.Type == LError {
			return lerr
		}
	}
	return opProgn(fletenv, args)
}

// macrolet functions similar to flet -- there are no cross-refrences between
// the set of macros defined in the local macrolet.
//
// NOTE:  macrolet functions will not be portable to other lisp implementations
// if they attempt to reference lexically bound symbols outside of the macro
// expansion.  That is, while the _expanded_ macro may reference local
// variables and functions, _during_ its exansion the macro may only make use
// of other macros and global symbols.
func opMacrolet(env *LEnv, args *LVal) *LVal {
	fletenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return env.Errorf("first argument is not a list: %s", bindlist.Type)
	}
	for _, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of function definitions")
		}
		if len(bind.Cells) < 2 {
			return env.Errorf("first argument is not a list of function definitions")
		}
		fenv := NewEnv(env) // lambdas in a macrolet get their own little environment
		name, formals, body := bind.Cells[0], bind.Cells[1], bind.Cells[2:]
		lval := fenv.Lambda(formals, body)
		if lval.Type == LError {
			return lval
		}
		lval.FunType = LFunMacro // evaluate as a macro
		lerr := fletenv.Put(name, lval)
		if lerr.Type == LError {
			return lerr
		}
	}
	return opProgn(fletenv, args)
}

func opLet(env *LEnv, args *LVal) *LVal {
	letenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return env.Errorf("first argument is not a list: %s", bindlist.Type)
	}
	vals := make([]*LVal, len(bindlist.Cells))
	for i, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return env.Errorf("first argument is not a list of pairs")
		}
		vals[i] = letenv.Eval(bind.Cells[1])
		if vals[i].Type == LError {
			return vals[i]
		}
	}
	for i, bind := range bindlist.Cells {
		lerr := letenv.Put(bind.Cells[0], vals[i])
		if lerr.Type == LError {
			return lerr
		}
	}
	return opProgn(letenv, args)
}

func opLetSeq(env *LEnv, args *LVal) *LVal {
	letenv := NewEnv(env)
	bindlist := args.Cells[0]
	args.Cells = args.Cells[1:] // decap so we can call builtinProgn on args.
	if bindlist.Type != LSExpr {
		return env.Errorf("first argument is not a list: %s", bindlist.Type)
	}
	for _, bind := range bindlist.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of pairs")
		}
		if len(bind.Cells) != 2 {
			return env.Errorf("first argument is not a list of pairs")
		}
		val := letenv.Eval(bind.Cells[1])
		if val.Type == LError {
			return val
		}
		// BUG:  A function defined in a let* is not supposed to be able to
		// reference itself (recursively) or any bindings defined following its
		// entry in bindlist during a funcall.  So we should create a new
		// environment to hold the actual function binding for this cell along
		// with any following bindings (provided they don't also bind functions
		// and cause further fracturing of the lexical scope).  Something like
		// the following:
		//
		//if val.Type == LFun {
		//	// NOTE:  The function val may not have been created during the
		//	// evaluation of bind.Cells[1], but it isn't clear how to detect a
		//	// newly created lambda vs one that was merely the result of, say,
		//	// symbol resolution inside the bind.Cells[1] expression.  So, we
		//	// assume for now that this is a newly created function.
		//	letenv = NewEnv(letenv)
		//}
		lerr := letenv.Put(bind.Cells[0], val)
		if lerr.Type == LError {
			return lerr
		}
	}
	return opProgn(letenv, args)
}

func opProgn(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		return Nil()
	}
	term := args.Cells[len(args.Cells)-1]
	var val *LVal
	for _, c := range args.Cells[:len(args.Cells)-1] {
		val = env.Eval(c)
		if val.Type == LError {
			return val
		}
	}
	return env.Terminal(term)
}

func opHandlerBind(env *LEnv, args *LVal) *LVal {
	lbinds, forms := args.Cells[0], args.Cells[1:]
	if lbinds.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", lbinds.Type)
	}
	for _, bind := range lbinds.Cells {
		if bind.Type != LSExpr {
			return env.Errorf("first argument is not a list of bindings: %v", bind.Type)
		}
		if len(bind.Cells) != 2 {
			return env.Errorf("first argument is not a list of bindings")
		}
		sym := bind.Cells[0]
		if sym.Type != LSymbol {
			return env.Errorf("binding type is not a symbol: %v", sym.Type)
		}
	}
	if len(args.Cells) == 0 {
		return Nil()
	}
	var val *LVal
	// We can't call opProgn because we can't mark any expressions as
	// terminal.  If an expression were marked as terminal env might try to
	// invoke tail recursion optimizations.
	env.Runtime.Stack.Top().TROBlock = true
	for _, c := range forms {
		val = env.Eval(c)
		if val.Type == LError {
			for _, bind := range lbinds.Cells {
				sym, handler := bind.Cells[0], bind.Cells[1]
				// Compare the error condition to the handler type specifier.
				if sym.Str != val.Str && sym.Str != "condition" {
					continue
				}
				// The condition matches so we evaluate the handler and then
				// call it, passing the error.
				hval := env.Eval(handler)
				if hval.Type == LError {
					// Well, we're boned
					return hval
				}
				if hval.Type != LFun {
					return env.Errorf("handler not a function for condition type %s: %v", sym.Str, hval.Type)
				}
				// Is this a Terminal expression? Probably not...
				expr := []*LVal{hval, Quote(Symbol(val.Str))}
				expr = append(expr, val.Copy().Cells...)
				return env.Eval(SExpr(expr))
			}
			return val
		}
	}
	return val
}

func opIgnoreErrors(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		return Nil()
	}
	var val *LVal
	// We can't call opProgn because we can't mark any expressions as
	// terminal.  If an expression were marked as terminal env might try to
	// invoke tail recursion optimizations.
	env.Runtime.Stack.Top().TROBlock = true
	for _, c := range args.Cells {
		val = env.Eval(c)
		if val.Type == LError {
			return Nil()
		}
	}
	return val
}

// (cond (test-form then-form)*)
func opCond(env *LEnv, args *LVal) *LVal {
	last := len(args.Cells) - 1
	for i, branch := range args.Cells {
		if branch.Type != LSExpr {
			return env.Errorf("argument is not a list: %v", branch.Type)
		}
		if len(branch.Cells) == 0 {
			return env.Errorf("argument is not a pair (length %d)", len(branch.Cells))
		}
		var test *LVal
		if branch.Cells[0].Type == LSymbol && branch.Cells[0].Str == "else" {
			if i != last {
				return env.Errorf("invalid syntax: else")
			}
			test = branch.Cells[0] // the value here doesn't matter as long as it isn't nil
		} else {
			test = env.Eval(branch.Cells[0])
		}
		if test.Type == LError {
			return test
		}
		if Not(test) {
			continue
		}
		return opProgn(env, SExpr(branch.Cells[1:]))
	}
	return Nil()
}

// (if test-form then-form else-form)
func opIf(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) != 3 {
		return env.Errorf("three arguments expected (got %d)", len(s.Cells))
	}
	r := env.Eval(s.Cells[0])
	if r.Type == LError {
		return r
	}
	if Not(r) {
		// test-form evaluated to nil (false)
		return env.Terminal(s.Cells[2])
	}
	// test-form evaluated to something non-nil (true)
	return env.Terminal(s.Cells[1])
}

func opOr(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) == 0 {
		// Common lisp returns a nil value here. But logical operators
		// operating on booleans should return booleans.  And here all
		// arguments are booleans (vacuously true without any arguments).
		return Bool(false)
	}
	term := s.Cells[len(s.Cells)-1]
	for _, c := range s.Cells[:len(s.Cells)-1] {
		r := env.Eval(c)
		if r.Type == LError {
			return r
		}
		if True(r) {
			return r
		}
	}
	// In the common lisp standard the ``or'' function returns the evaluated
	// result of its final argument if no arguments evaluated true.
	//		(or) == nil
	//		(or x) == x
	//		(or x1 x2 ... xn) == (cond (x1 x1) (x2 x2) ... (t xn))
	return env.Terminal(term)
}

func opAnd(env *LEnv, s *LVal) *LVal {
	if len(s.Cells) == 0 {
		// The identity for ``and'' is a true value.
		return Bool(true)
	}
	// NOTE:  Because it is unknown which argument will be the last one
	// evaluated ``and'' cannot use a Terminal expression (unlike ``or'').
	var r *LVal
	for _, c := range s.Cells {
		r = env.Eval(c)
		if r.Type == LError {
			return r
		}
		if !True(r) {
			return r
		}
	}
	// In the common lisp standard the ``and'' function returns the evaluated
	// result of its final argument if all arguments evaluated true.
	//		(and) == nil
	//		(and x) == x
	//		(and x1 x2 ... xn) == (cond
	//		                       ((not x1) nil)
	//		                       ((not x2) nil)
	//		                       ...
	//		                       (t xn))
	return r
}

func opQualifiedSymbol(env *LEnv, args *LVal) *LVal {
	sym := args.Cells[0]
	if sym.Type != LSymbol {
		return env.Errorf("argument is not a symbol: %v", GetType(sym))
	}
	pieces := SplitSymbol(sym)
	if pieces.Type == LError {
		env.ErrorAssociate(pieces)
		return pieces
	}
	if pieces.Len() == 2 {
		if sym.Quoted {
			return sym
		}
		return Quote(sym)
	}
	pkg := env.Runtime.Package.Name
	return Quote(Symbol(fmt.Sprintf("%v:%v", pkg, sym.Str)))
}
