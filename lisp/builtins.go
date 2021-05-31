// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"math"
	"sort"
	"strconv"
	"strings"
)

// LBuiltin is a function that performs executes a lisp function.
type LBuiltin func(env *LEnv, args *LVal) *LVal

// LBuiltinDef is a built-in function
type LBuiltinDef interface {
	Name() string
	Formals() *LVal
	Eval(env *LEnv, args *LVal) *LVal
}

type langBuiltin struct {
	name    string
	formals *LVal
	fun     LBuiltin
}

func (fun *langBuiltin) Name() string {
	return fun.name
}

func (fun *langBuiltin) Formals() *LVal {
	return fun.formals
}

func (fun *langBuiltin) Eval(env *LEnv, args *LVal) *LVal {
	return fun.fun(env, args)
}

var userBuiltins []*langBuiltin
var langBuiltins = []*langBuiltin{
	{"load-string", Formals("source-code", KeyArgSymbol, "name"), builtinLoadString},
	{"load-bytes", Formals("source-code", KeyArgSymbol, "name"), builtinLoadBytes},
	{"load-file", Formals("source-location"), builtinLoadFile},
	{"in-package", Formals("package-name"), builtinInPackage},
	{"use-package", Formals(VarArgSymbol, "package-name"), builtinUsePackage},
	{"export", Formals(VarArgSymbol, "symbol"), builtinExport},
	{"set", Formals("sym", "val"), builtinSet},
	{"gensym", Formals(), builtinGensym},
	{"identity", Formals("value"), builtinIdentity},
	{"macroexpand", Formals("quoted-form"), builtinMacroExpand},
	{"macroexpand-1", Formals("quoted-form"), builtinMacroExpand1},
	{"funcall", Formals("fun", VarArgSymbol, "args"), builtinFunCall},
	{"apply", Formals("fun", VarArgSymbol, "args"), builtinApply},
	{"to-string", Formals("value"), builtinToString},
	{"to-bytes", Formals("value"), builtinToBytes},
	{"to-int", Formals("value"), builtinToInt},
	{"to-float", Formals("value"), builtinToFloat},
	{"eval", Formals("expr"), builtinEval},
	{"error", Formals("condition", VarArgSymbol, "args"), builtinError},
	{"car", Formals("lis"), builtinCAR},
	{"cdr", Formals("lis"), builtinCDR},
	{"rest", Formals("lis"), builtinRest},
	{"first", Formals("seq"), builtinFirst},
	{"second", Formals("seq"), builtinSecond},
	{"nth", Formals("seq", "n"), builtinNth},
	{"map", Formals("type-specifier", "fn", "seq"), builtinMap},
	{"foldl", Formals("fn", "z", "seq"), builtinFoldLeft},
	{"foldr", Formals("fn", "z", "seq"), builtinFoldRight},
	{"compose", Formals("f", "g"), builtinCompose},
	{"unpack", Formals("f", "lis"), builtinUnpack},
	{"flip", Formals("binary-function"), builtinFlip},
	{"assoc", Formals("map", "key", "value"), builtinAssoc},
	{"assoc!", Formals("map", "key", "value"), builtinAssocMutate},
	{"dissoc", Formals("map", "key"), builtinDissoc},
	{"dissoc!", Formals("map", "key"), builtinDissocMutate},
	{"get", Formals("map", "key"), builtinGet},
	{"keys", Formals("map"), builtinKeys},
	{"key?", Formals("map", "key"), builtinIsKey},
	{"sorted-map", Formals(VarArgSymbol, "args"), builtinSortedMap},
	{"concat", Formals("type-specifier", VarArgSymbol, "args"), builtinConcat},
	{"insert-index", Formals("type-specifier", "seq", "index", "item"), builtinInsertIndex},
	{"stable-sort", Formals("less-predicate", "list", VarArgSymbol, "key-fun"), builtinSortStable},
	{"insert-sorted", Formals("type-specifier", "list", "predicate", "item", VarArgSymbol, "key-fun"), builtinInsertSorted},
	{"search-sorted", Formals("n", "predicate"), builtinSearchSorted},
	{"select", Formals("type-specifier", "predicate", "seq"), builtinSelect},
	{"reject", Formals("type-specifier", "predicate", "seq"), builtinReject},
	{"zip", Formals("type-specifier", "list", VarArgSymbol, "lists"), builtinZip},
	{"make-sequence", Formals("start", "stop", VarArgSymbol, "step"), builtinMakeSequence},
	{"format-string", Formals("format", VarArgSymbol, "values"), builtinFormatString},
	{"reverse", Formals("type-specifier", "seq"), builtinReverse},
	{"slice", Formals("type-specifier", "seq", "start", "end"), builtinSlice},
	{"list", Formals(VarArgSymbol, "args"), builtinList},
	{"vector", Formals(VarArgSymbol, "args"), builtinVector},
	{"append!", Formals("vec", VarArgSymbol, "values"), builtinAppendMutate},
	{"append", Formals("type-specifier", "vec", VarArgSymbol, "values"), builtinAppend},
	{"append-bytes!", Formals("bytes", "values"), builtinAppendBytesMutate},
	// NOTE:  ``append-bytes'' does not accept a type-specifier argument
	// because 'string would be equivalent to (concat 'string ...)
	{"append-bytes", Formals("bytes", "byte-sequence"), builtinAppendBytes},
	{"aref", Formals("a", VarArgSymbol, "indices"), builtinARef},
	{"length", Formals("seq"), builtinLength},
	{"empty?", Formals("seq"), builtinIsEmpty},
	{"cons", Formals("head", "tail"), builtinCons},
	{"not", Formals("expr"), builtinNot},
	// true? is potentially needed as a boolean conversion function (for json)
	{"true?", Formals("expr"), builtinIsTrue},
	{"type", Formals("value"), builtinType},
	{"type?", Formals("type-specifier", "value"), builtinIsType},
	{"tagged-value?", Formals("value"), builtinIsTaggedVal},
	{"user-data", Formals("value"), builtinUserData},
	{"new", Formals("type-specifier", VarArgSymbol, "arguments"), builtinNew},
	{"nil?", Formals("expr"), builtinIsNil},
	{"list?", Formals("expr"), builtinIsList},
	{"sorted-map?", Formals("expr"), builtinIsSortedMap},
	{"array?", Formals("expr"), builtinIsArray},
	{"vector?", Formals("expr"), builtinIsVector},
	{"bool?", Formals("expr"), builtinIsBool},
	{"number?", Formals("expr"), builtinIsNumber},
	{"int?", Formals("expr"), builtinIsInt},
	{"float?", Formals("expr"), builtinIsFloat},
	{"symbol?", Formals("expr"), builtinIsSymbol},
	{"string?", Formals("expr"), builtinIsString},
	{"bytes?", Formals("expr"), builtinIsBytes},
	{"equal?", Formals("a", "b"), builtinEqual},
	{"all?", Formals("predicate", "seq"), builtinAllP},
	{"any?", Formals("predicate", "seq"), builtinAnyP},
	{"max", Formals("real", VarArgSymbol, "rest"), builtinMax},
	{"min", Formals("real", VarArgSymbol, "rest"), builtinMin},
	{"string>=", Formals("a", "b"), builtinStringGEq},
	{"string>", Formals("a", "b"), builtinStringGT},
	{"string<=", Formals("a", "b"), builtinStringLEq},
	{"string<", Formals("a", "b"), builtinStringLT},
	{"string=", Formals("a", "b"), builtinStringEq},
	{"symbol=", Formals("a", "b"), builtinSymbolEq},
	{">=", Formals("a", "b"), builtinGEq},
	{">", Formals("a", "b"), builtinGT},
	{"<=", Formals("a", "b"), builtinLEq},
	{"<", Formals("a", "b"), builtinLT},
	{"=", Formals("a", "b"), builtinEqNum},
	{"pow", Formals("a", "b"), builtinPow},
	{"mod", Formals("a", "b"), builtinMod},
	{"+", Formals(VarArgSymbol, "x"), builtinAdd},
	{"-", Formals(VarArgSymbol, "x"), builtinSub},
	{"/", Formals(VarArgSymbol, "x"), builtinDiv},
	{"*", Formals(VarArgSymbol, "x"), builtinMul},
	{"debug-print", Formals(VarArgSymbol, "args"), builtinDebugPrint},
	{"debug-stack", Formals(), builtinDebugStack},
}

// RegisterDefaultBuiltin adds the given function to the list returned by
// DefaultBuiltins.
func RegisterDefaultBuiltin(name string, formals *LVal, fn LBuiltin) {
	userBuiltins = append(userBuiltins, &langBuiltin{name, formals.Copy(), fn})
}

// DefaultBuiltins returns the default set of LBuiltinDefs added to LEnv
// objects when LEnv.AddBuiltins is called without arguments.
func DefaultBuiltins() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langBuiltins)+len(userBuiltins))
	for i := range langBuiltins {
		ops[i] = langBuiltins[i]
	}
	offset := len(langBuiltins)
	for i := range userBuiltins {
		ops[offset+i] = langBuiltins[i]
	}
	return ops
}

func builtinLoadString(env *LEnv, args *LVal) *LVal {
	source, name := args.Cells[0], args.Cells[1]
	if source.Type != LString {
		return env.Errorf("first argument is not a string: %v", source.Type)
	}
	if !name.IsNil() && name.Type != LString {
		return env.Errorf("name is not a string: %v", name.Type)
	}
	_name := "load-string"
	if name.Str != "" {
		_name = name.Str
	}

	// Load the source in the root environment so the loaded code does not
	// share the current lexical environment.  The loaded source will share a
	// stack but the stack frame TROBlock will prevent tail recursion
	// optimization from unwinding the stack to/beyond this point.
	env.Runtime.Stack.Top().TROBlock = true
	v := env.root().LoadString(_name, source.Str)
	if v.Type == LError && v.CallStack() == nil {
		v.SetCallStack(env.Runtime.Stack.Copy())
	}
	return v
}

func builtinLoadBytes(env *LEnv, args *LVal) *LVal {
	source, name := args.Cells[0], args.Cells[1]
	if source.Type != LBytes {
		return env.Errorf("first argument is not bytes: %v", source.Type)
	}
	if !name.IsNil() && name.Type != LString {
		return env.Errorf("name is not a string: %v", name.Type)
	}
	_name := "load-bytes"
	if name.Str != "" {
		_name = name.Str
	}

	// Load the source in the root environment so the loaded code does not
	// share the current lexical environment.  The loaded source will share a
	// stack but the stack frame TROBlock will prevent tail recursion
	// optimization from unwinding the stack to/beyond this point.
	env.Runtime.Stack.Top().TROBlock = true
	v := env.root().Load(_name, bytes.NewReader(source.Bytes()))
	if v.Type == LError && v.CallStack() == nil {
		v.SetCallStack(env.Runtime.Stack.Copy())
	}
	return v
}

func builtinLoadFile(env *LEnv, args *LVal) *LVal {
	loc := args.Cells[0]
	if loc.Type != LString {
		return env.Errorf("first argument is not a string: %v", loc.Type)
	}

	// Load the source in the root environment so the loaded code does not
	// share the current lexical environment.  The loaded source will share a
	// stack but the stack frame TROBlock will prevent tail recursion
	// optimization from unwinding the stack to/beyond this point.
	env.Runtime.Stack.Top().TROBlock = true
	v := env.root().LoadFile(loc.Str)
	if v.Type == LError && v.CallStack() == nil {
		v.SetCallStack(env.Runtime.Stack.Copy())
	}
	return v
}

func builtinInPackage(env *LEnv, args *LVal) *LVal {
	if args.Cells[0].Type != LSymbol && args.Cells[0].Type != LString {
		return env.Errorf("first argument is not a symbol or a string: %v", args.Cells[0].Type)
	}
	name := args.Cells[0].Str
	pkg := env.Runtime.Registry.Packages[name]
	newpkg := false
	if pkg == nil {
		newpkg = true
		env.Runtime.Registry.DefinePackage(name)
		pkg = env.Runtime.Registry.Packages[name]
	}
	env.Runtime.Package = pkg
	if newpkg && env.Runtime.Registry.Lang != "" {
		// For now, all packages use the lisp package.  The ``in-package''
		// builtin doesn't provide syntax to simply use lisp (calling
		// ``lisp:use-package'' from a package that doesn't use lisp hard to
		// remember).
		env.UsePackage(Symbol(env.Runtime.Registry.Lang))
	}
	return Nil()
}

func builtinUsePackage(env *LEnv, args *LVal) *LVal {
	if args.Cells[0].Type != LSymbol && args.Cells[0].Type != LString {
		return env.Errorf("first argument is not a symbol or a string: %v", args.Cells[0].Type)
	}
	return env.UsePackage(args.Cells[0])
}

func builtinExport(env *LEnv, args *LVal) *LVal {
	for _, arg := range args.Cells {
		switch {
		case arg.Type == LSymbol || arg.Type != LString:
			env.Runtime.Package.Exports(arg.Str)
		case arg.Type == LSExpr:
			builtinExport(env, arg)
		default:
			return env.Errorf("argument is not a symbol, a string, or a list of valid types: %v", arg.Type)
		}
	}
	return Nil()
}

func builtinSet(env *LEnv, v *LVal) *LVal {
	if v.Cells[0].Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", v.Cells[0].Type)
	}

	lerr := env.PutGlobal(v.Cells[0], v.Cells[1])
	if lerr.Type == LError {
		return lerr
	}
	return env.GetGlobal(v.Cells[0])
}

func builtinGensym(env *LEnv, args *LVal) *LVal {
	return env.GenSym()
}

func builtinIdentity(env *LEnv, args *LVal) *LVal {
	return args.Cells[0]
}

func builtinMacroExpand(env *LEnv, args *LVal) *LVal {
	form := args.Cells[0]
	if form.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", form.Type)
	}
	for {
		if form.IsNil() {
			return form
		}
		macsym, macargs := form.Cells[0], form.Cells[1:]
		if macsym.Type != LSymbol {
			return form
		}
		mac := env.Get(macsym)
		r, ok := macroExpand1(env, mac, SExpr(macargs))
		if !ok {
			return form
		}
		if r.Type != LSExpr {
			return r
		}
		form = r
	}
}

func builtinMacroExpand1(env *LEnv, args *LVal) *LVal {
	form := args.Cells[0]
	if form.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", form.Type)
	}
	if form.IsNil() {
		return form
	}
	macsym, macargs := form.Cells[0], form.Cells[1:]
	if macsym.Type != LSymbol {
		return form
	}
	mac := env.Get(macsym)
	r, ok := macroExpand1(env, mac, SExpr(macargs))
	if !ok {
		return form
	}
	return r

}

func macroExpand1(env *LEnv, mac *LVal, args *LVal) (*LVal, bool) {
	if mac.Type != LFun {
		return nil, false
	}
	if !mac.IsMacro() {
		return nil, false
	}
	// NOTE:  Any return from this point on should return a true second value
	// because mac is a macro.

	mark := env.MacroCall(mac, args)
	if mark.Type == LError {
		return mark, true
	}
	if mark.Type != LMarkMacExpand {
		panic("macro did not return LMarkMacExpand: " + mark.Type.String())
	}
	// MacroCall unquotes its result so that it can work properly in normal
	// evaluation cycle.  So we need to re-quote the value here.
	return Quote(mark.Cells[0]), true
}

func builtinFunCall(env *LEnv, args *LVal) *LVal {
	fun, fargs := args.Cells[0], args.Cells[1:]
	fun = env.GetFunGlobal(fun)
	if fun.Type == LError {
		return fun
	}
	if fun.IsSpecialFun() {
		return env.Errorf("not a regular function: %v", fun.FunType)
	}
	// Because funcall and apply do not actually call env.Eval they cannot use
	// the standard method of signaling a terminal expression to the LEnv.  We
	// need to set the flag explicitly before env.funCall is invoked
	env.Runtime.Stack.Top().Terminal = true
	return env.funCall(fun, SExpr(fargs))
}

func builtinApply(env *LEnv, args *LVal) *LVal {
	fun, fargs := args.Cells[0], args.Cells[1:]
	if len(fargs) == 0 {
		return env.Errorf("last argument must be a list")
	}
	argtail := fargs[len(fargs)-1]
	fargs = fargs[:len(fargs)-1]

	fun = env.GetFunGlobal(fun)
	if fun.Type == LError {
		return fun
	}
	if argtail.Type != LSExpr {
		return env.Errorf("last argument is not a list: %v", argtail.Type)
	}
	if fun.Type != LFun {
		return env.Errorf("first argument is not a function: %v", fun.Type)
	}
	if fun.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", fun.FunType)
	}

	// Because funcall and apply do not actually call env.Eval they cannot use
	// the standard method of signaling a terminal expression to the LEnv.  We
	// need to set the flag explicitly before env.funCall is invoked
	env.Runtime.Stack.Top().Terminal = true

	argcells := make([]*LVal, 0, len(fargs)+argtail.Len())
	argcells = append(argcells, fargs...)
	argcells = append(argcells, argtail.Cells...)
	return env.funCall(fun, SExpr(argcells))
}

func builtinToString(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	s, err := toString(val)
	if err != nil {
		return env.Error(err)
	}
	return String(s)
}

func builtinToBytes(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	if val.Type == LBytes {
		return val
	}
	if val.Type == LString {
		return Bytes([]byte(val.Str))
	}
	// TODO:  Allow sequences of integers to be turned into bytes?
	return env.Errorf("cannot convert type to string: %v", val.Type)
}

func toString(val *LVal) (string, error) {
	switch val.Type {
	case LString:
		return val.Str, nil
	case LSymbol:
		return val.Str, nil
	case LBytes:
		return string(val.Bytes()), nil
	case LInt:
		i := strconv.Itoa(val.Int)
		return i, nil
	case LFloat:
		f := strconv.FormatFloat(val.Float, 'g', -1, 64)
		return f, nil
	default:
		return "", fmt.Errorf("cannot convert type to string: %v", val.Type)
	}
}

func builtinToInt(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	switch val.Type {
	case LString:
		x, err := strconv.Atoi(val.Str)
		if err != nil {
			return env.Error(err)
		}
		return Int(x)
	case LInt:
		return val
	case LFloat:
		return Int(int(val.Float))
	default:
		return env.Errorf("cannot convert type to string: %v", val.Type)
	}
}

func builtinToFloat(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	switch val.Type {
	case LString:
		x, err := strconv.ParseFloat(val.Str, 64)
		if err != nil {
			return env.Error(err)
		}
		return Float(x)
	case LInt:
		return Float(float64(val.Int))
	case LFloat:
		return val
	default:
		return env.Errorf("cannot convert type to string: %v", val.Type)
	}
}

func builtinEval(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type == LQuote {
		return v.Cells[0]
	}
	return env.Eval(shallowUnquote(v))
}

func builtinError(env *LEnv, args *LVal) *LVal {
	condition, rest := args.Cells[0], args.Cells[1:]
	if condition.Type != LSymbol {
		return env.Errorf("condition type is not a symbol: %v", condition.Type)
	}

	iargs := make([]interface{}, len(rest))
	for i, arg := range rest {
		iargs[i] = arg
	}
	return env.ErrorCondition(condition.Str, iargs...)
}

func builtinCAR(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type != LSExpr {
		return env.Errorf("argument is not a list %v", v.Type)
	}
	if len(v.Cells) == 0 {
		return Nil()
	}

	return v.Cells[0]
}

func builtinCDR(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type != LSExpr {
		return env.Errorf("argument is not a list %v", v.Type)
	}
	if len(v.Cells) < 2 {
		return Nil()
	}
	// TODO:  Copy cells into a new list of cells?
	return QExpr(v.Cells[1:])
}

func builtinRest(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if !isSeq(v) {
		return env.Errorf("argument is not a proper sequence %v", v.Type)
	}
	cells := seqCells(v)
	if len(cells) < 2 {
		return Nil()
	}
	// TODO:  Copy cells into a new list of cells?
	return QExpr(cells[1:])
}

func builtinFirst(env *LEnv, args *LVal) *LVal {
	list := args.Cells[0]
	if !isSeq(list) {
		return env.Errorf("argument is not a proper sequence: %v", list.Type)
	}
	cells := seqCells(list)
	if len(cells) == 0 {
		return Nil()
	}
	return cells[0]
}

func builtinSecond(env *LEnv, args *LVal) *LVal {
	list := args.Cells[0]
	if !isSeq(list) {
		return env.Errorf("argument is not a proper sequence: %v", list.Type)
	}
	cells := seqCells(list)
	if len(cells) < 2 {
		return Nil()
	}
	return cells[1]
}

func builtinNth(env *LEnv, args *LVal) *LVal {
	list, n := args.Cells[0], args.Cells[1]
	if !isSeq(list) {
		return env.Errorf("first argument is not a proper sequence: %v", list.Type)
	}
	if n.Type != LInt {
		return env.Errorf("second argument is not an integer: %v", n.Type)
	}
	if n.Int < 0 {
		return env.Errorf("index cannot be negative: %d", n.Int)
	}
	cells := seqCells(list)
	if len(cells) <= n.Int {
		return Nil()
	}
	return cells[n.Int]
}

func builtinMap(env *LEnv, args *LVal) *LVal {
	typespec, f, lis := args.Cells[0], args.Cells[1], args.Cells[2]
	nilReturn := false
	if typespec.IsNil() {
		nilReturn = true
	} else if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("second argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	var v *LVal
	var cells []*LVal
	if nilReturn {
		v = Nil()
	} else {
		switch typespec.Str {
		case "vector":
			v = Array(QExpr([]*LVal{Int(lis.Len())}), nil)
			cells = seqCells(v)
		case "list":
			cells = make([]*LVal, lis.Len())
			v = QExpr(cells)
		default:
			return env.Errorf("type specifier is invalid: %v", typespec)
		}
	}
	for i, c := range seqCells(lis) {
		fargs := QExpr([]*LVal{c})
		fret := env.FunCall(f, fargs)
		if fret.Type == LError {
			return fret
		}
		if !nilReturn {
			cells[i] = fret
		}
	}
	return v
}

func builtinFoldLeft(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	for _, c := range seqCells(lis) {
		fargs := QExpr([]*LVal{
			// args reversed from foldr function invocation
			acc,
			c,
		})
		fret := env.FunCall(f, fargs)
		if fret.Type == LError {
			return fret
		}
		acc = fret
	}
	return acc
}

func builtinFoldRight(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	cells := seqCells(lis)
	for i := len(cells) - 1; i >= 0; i-- {
		c := cells[i]
		fargs := QExpr([]*LVal{
			// args reversed from foldl function invocation
			c,
			acc,
		})
		fret := env.FunCall(f, fargs)
		if fret.Type == LError {
			return fret
		}
		acc = fret
	}
	return acc
}

// NOTE: Compose requires concat and unpack in order to work with varargs.
func builtinCompose(env *LEnv, args *LVal) *LVal {
	f, g := args.Cells[0], args.Cells[1]
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	g = env.GetFunGlobal(g)
	if g.Type == LError {
		return f
	}
	if g.Type != LFun {
		return env.Errorf("second argument is not a function: %s", g.Type)
	}
	if g.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", g.FunType)
	}
	formals := g.Cells[0].Copy()
	gcall := SExpr(make([]*LVal, 0, len(formals.Cells)+1))
	body := SExpr([]*LVal{Symbol("lisp:funcall"), f, gcall})
	gcall.Cells = append(gcall.Cells, Symbol("lisp:apply"), g)
	var restSym *LVal
	for i, argSym := range formals.Cells {
		if argSym.Type != LSymbol {
			// This should not happen.  The list of formals should be checked
			// when the g function was created.
			return env.Errorf("invalid list of formals: %s", formals)
		}
		if argSym.Str == OptArgSymbol {
			continue
		}
		if argSym.Str == KeyArgSymbol {
			continue
		}
		if argSym.Str == VarArgSymbol {
			if len(formals.Cells) != i+2 {
				// This should not happen.  The list of formals should be checked
				// when the g function was created.
				return env.Errorf("invalid list of formals: %s", formals)
			}
			restSym = formals.Cells[i+1]
			break
		}
		gcall.Cells = append(gcall.Cells, argSym)
	}
	if restSym != nil {
		gcall.Cells = append(gcall.Cells, restSym)
	} else {
		gcall.Cells = append(gcall.Cells, Nil())
	}
	newfun := env.Lambda(formals, []*LVal{body})
	return newfun
}

func builtinUnpack(env *LEnv, args *LVal) *LVal {
	return builtinApply(env, args)
}

func builtinFlip(env *LEnv, args *LVal) *LVal {
	fun := args.Cells[0]
	fun = env.GetFunGlobal(fun)
	if fun.Type == LError {
		return fun
	}
	if fun.Type != LFun {
		return env.Errorf("argument is not a function: %s", fun.Type)
	}
	if fun.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", fun.FunType)
	}
	formals := fun.Cells[0]
	if len(formals.Cells) < 2 {
		return env.Errorf("argument is not a function of two arguments: %s", formals)
	}
	call := SExpr([]*LVal{fun, Symbol("y"), Symbol("x")})
	return env.Lambda(Formals("x", "y"), []*LVal{call})
}

func builtinAssoc(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	v := args.Cells[2]
	if m.IsNil() {
		m = SortedMap()
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	} else {
		mdata, err := m.copyMapData()
		if err != nil {
			return env.Error(err)
		}
		m = SortedMapFromData(mdata)
	}
	err := m.Map().Set(k, v)
	if !err.IsNil() {
		return env.Errorf("%s", err)
	}
	return m
}

func builtinAssocMutate(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	v := args.Cells[2]
	if m.IsNil() {
		return env.Errorf("first argument is nil: %v", m.Type)
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	err := m.Map().Set(k, v)
	if !err.IsNil() {
		return env.Error(err.String())
	}
	return m
}

func builtinDissoc(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	if m.IsNil() {
		m = SortedMap()
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	} else {
		mdata, err := m.copyMapData()
		if err != nil {
			return env.Error(err)
		}
		m = SortedMapFromData(mdata)
	}
	err := m.Map().Del(k)
	if !err.IsNil() {
		return env.Errorf("%s", err)
	}
	return m
}

func builtinDissocMutate(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	if m.IsNil() {
		return env.Errorf("first argument is nil: %v", m.Type)
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	err := m.Map().Del(k)
	if !err.IsNil() {
		return env.Error(err.String())
	}
	return m
}

func builtinGet(env *LEnv, args *LVal) *LVal {
	m, k := args.Cells[0], args.Cells[1]
	if m.IsNil() {
		return Nil()
	}
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	v, _ := m.Map().Get(k)
	return v
}

func builtinKeys(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	return m.Map().Keys()
}

func builtinIsKey(env *LEnv, args *LVal) *LVal {
	m, k := args.Cells[0], args.Cells[1]
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	v, ok := m.Map().Get(k)
	if v.Type == LError {
		v.SetCallStack(env.Runtime.Stack.Copy())
		return v
	}
	return Bool(ok)
}

func builtinSortedMap(env *LEnv, args *LVal) *LVal {
	m := SortedMap()
	if len(args.Cells)%2 != 0 {
		return env.Errorf("uneven number of arguments: %d", len(args.Cells))
	}
	data := m.Map()
	for len(args.Cells) >= 2 {
		k := args.Cells[0]
		v := args.Cells[1]
		err := data.Set(k, v)
		if !err.IsNil() {
			return err
		}
		args.Cells = args.Cells[2:]
	}
	return m
}

func builtinConcat(env *LEnv, args *LVal) *LVal {
	typespec := args.Cells[0]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	switch typespec.Str {
	case "vector", "list":
		return builtinConcatSeq(env, args)
	case "string":
		return builtinConcatString(env, args)
	case "bytes":
		return builtinConcatBytes(env, args)
	default:
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
}

func builtinConcatString(env *LEnv, args *LVal) *LVal {
	// typespec is already known to be 'string here
	_, rest := args.Cells[0], args.Cells[1:]
	size := 0
	for _, v := range rest {
		n := v.Len()
		if n < 0 {
			// A type with no length cannot be a byte-sequence.
			return env.Errorf("argument is not sequence of bytes: %v", v.Type)
		}
		size += n
	}
	buf := bytes.NewBuffer(make([]byte, 0, size))
	for _, v := range rest {
		switch v.Type {
		case LBytes:
			buf.Write(v.Bytes())
		case LString:
			buf.WriteString(v.Str)
		default:
			err := appendBytes(env, v, func(x byte) {
				buf.WriteByte(x)
			})
			if err != nil {
				return env.Error(err)
			}
		}
	}
	return String(buf.String())
}

func builtinConcatBytes(env *LEnv, args *LVal) *LVal {
	// typespec is already known to be 'bytes here
	_, rest := args.Cells[0], args.Cells[1:]
	size := 0
	for _, v := range rest {
		n := v.Len()
		if n < 0 {
			// A type with no length cannot be a byte-sequence.
			return env.Errorf("argument is not sequence of bytes: %v", v.Type)
		}
		size += n
	}
	buf := make([]byte, 0, size)
	for _, v := range rest {
		switch v.Type {
		case LBytes:
			buf = append(buf, v.Bytes()...)
		case LString:
			buf = append(buf, v.Str...)
		default:
			err := appendBytes(env, v, func(x byte) {
				buf = append(buf, x)
			})
			if err != nil {
				return env.Error(err)
			}
		}
	}
	return Bytes(buf)
}

func appendBytes(env *LEnv, seq *LVal, fn func(x byte)) error {
	if !isSeq(seq) {
		return fmt.Errorf("argument is not a sequence of bytes: %v", seq.Type)
	}
	cells := seqCells(seq)
	// Check all cells before appending any bytes.
	for _, v := range cells {
		if v.Type != LInt {
			return fmt.Errorf("value not a byte: %v", v.Type)
		}
		if v.Int < 0 || v.Int > 0xFF {
			return fmt.Errorf("value overflows byte: %v", v)
		}
	}
	for _, v := range cells {
		fn(byte(v.Int))
	}
	return nil
}

func builtinConcatSeq(env *LEnv, args *LVal) *LVal {
	typespec, rest := args.Cells[0], args.Cells[1:]
	size := 0
	for _, v := range rest {
		if !isSeq(v) {
			return env.Errorf("argument is not a proper sequence: %v", v.Type)
		}
		size += v.Len()
	}
	if size == 0 {
		switch typespec.Str {
		case "vector":
			return Array(QExpr([]*LVal{Int(0)}), nil)
		case "list":
			return Nil()
		}
	}
	var ret *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		ret = Array(QExpr([]*LVal{Int(size)}), nil)
		cells = seqCells(ret)
		cells = cells[0:0:size]
	case "list":
		ret = QExpr(make([]*LVal, size))
		cells = ret.Cells[0:0:size]
	default:
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
	for _, v := range rest {
		cells = append(cells, seqCells(v)...)
	}
	return ret
}

func builtinSortStable(env *LEnv, args *LVal) *LVal {
	less, list, optArgs := args.Cells[0], args.Cells[1], args.Cells[2:]
	var keyFun *LVal
	less = env.GetFunGlobal(less)
	if less.Type == LError {
		return less
	}
	if less.Type != LFun {
		return env.Errorf("first argument is not a function: %v", less.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second arument is not a proper list: %v", list.Type)
	}
	if len(optArgs) > 1 {
		return env.Errorf("too many optional arguments provided")
	}
	if len(optArgs) > 0 {
		keyFun = optArgs[0]
		keyFun = env.GetFunGlobal(keyFun)
		if keyFun.Type == LError {
			return less
		}
		if keyFun.Type != LFun {
			return env.Errorf("third argument is not a function: %v", keyFun.Type)
		}
	}
	cells := seqCells(list)
	sortCells := &lvalByFun{
		env:    env,
		fun:    less,
		keyfun: keyFun,
		cells:  cells,
	}
	sort.Stable(sortCells)
	if sortCells.err != nil {
		return sortCells.err
	}
	return list
}

type lvalByFun struct {
	env    *LEnv
	fun    *LVal
	keyfun *LVal
	err    *LVal
	cells  []*LVal
}

func (s *lvalByFun) Len() int {
	return len(s.cells)
}
func (s *lvalByFun) Swap(i, j int) {
	if s.err != nil {
		return
	}
	s.cells[i], s.cells[j] = s.cells[j], s.cells[i]
}
func (s *lvalByFun) Less(i, j int) bool {
	if s.err != nil {
		return false
	}
	a, b := s.cells[i], s.cells[j]
	// Functions are always copied when being invoked. But the arguments
	// are not copied in general.
	var expr *LVal
	if s.keyfun == nil {
		expr = SExpr([]*LVal{s.fun, a.Copy(), b.Copy()})
	} else {
		expr = SExpr([]*LVal{s.fun,
			SExpr([]*LVal{s.keyfun, a.Copy()}),
			SExpr([]*LVal{s.keyfun, b.Copy()}),
		})
	}
	ok := s.env.Eval(expr)
	if ok.Type == LError {
		s.err = ok
		return false
	}
	return True(ok)
}

func builtinInsertIndex(env *LEnv, args *LVal) *LVal {
	typespec, list, index, item := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if index.Type != LInt {
		return env.Errorf("third arument is not a integer: %v", index.Type)
	}
	if index.Int > list.Len() {
		return env.Errorf("index out of bounds")
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(1 + list.Len())}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, 1+list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	inCells := seqCells(list)
	copy(cells[:index.Int], inCells[:index.Int])
	copy(cells[index.Int+1:], inCells[index.Int:])
	cells[index.Int] = item
	return v
}

func builtinInsertSorted(env *LEnv, args *LVal) *LVal {
	typespec, list, p, item, optArgs := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3], args.Cells[4:]
	var keyFun *LVal
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if p.Type != LFun {
		return env.Errorf("third arument is not a function: %v", p.Type)
	}
	if len(optArgs) > 1 {
		return env.Errorf("too many optional arguments provided")
	}
	if len(optArgs) > 0 {
		keyFun = optArgs[0]
		keyFun = env.GetFunGlobal(keyFun)
		if keyFun.Type == LError {
			return keyFun
		}
		if keyFun.Type != LFun {
			return env.Errorf("last argument is not a function: %v", keyFun.Type)
		}
	}
	sortErr := Nil()
	inCells := seqCells(list)
	i := sort.Search(len(inCells), func(i int) bool {
		var expr *LVal
		if keyFun == nil {
			expr = SExpr([]*LVal{p, item.Copy(), inCells[i].Copy()})
		} else {
			expr = SExpr([]*LVal{
				p,
				SExpr([]*LVal{
					keyFun,
					item.Copy(),
				}),
				SExpr([]*LVal{
					keyFun,
					inCells[i].Copy(),
				}),
			})
		}
		ok := env.Eval(expr)
		if ok.Type == LError {
			sortErr = ok
			return false
		}
		return True(ok)
	})
	if !sortErr.IsNil() {
		return sortErr
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(1 + list.Len())}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, 1+list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	copy(cells[:i], inCells[:i])
	copy(cells[i+1:], inCells[i:])
	cells[i] = item
	return v
}

func builtinSearchSorted(env *LEnv, args *LVal) *LVal {
	n, p := args.Cells[0], args.Cells[1]
	if n.Type != LInt {
		return env.Errorf("first argument is not an integer: %v", n.Type)
	}
	p = env.GetFunGlobal(p)
	if p.Type == LError {
		return p
	}
	if p.Type != LFun {
		return env.Errorf("second arument is not a function: %v", p.Type)
	}
	sortErr := Nil()
	i := sort.Search(n.Int, func(i int) bool {
		expr := SExpr([]*LVal{p, Int(i)})
		ok := env.Eval(expr)
		if ok.Type == LError {
			sortErr = ok
			return false
		}
		return True(ok)
	})
	if !sortErr.IsNil() {
		return sortErr
	}
	return Int(i)
}

func builtinSelect(env *LEnv, args *LVal) *LVal {
	typespec, pred, list := args.Cells[0], args.Cells[1], args.Cells[2]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("second argument is not a function: %v", pred.Type)
	}
	if pred.IsSpecialFun() {
		return env.Errorf("second argument is not a regular function")
	}
	if !isSeq(list) {
		return env.Errorf("third argument is not a proper sequence: %v", list.Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = seqCells(v)
		cells = cells[0:0:list.Len()]
	case "list":
		v = QExpr(nil)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for _, v := range seqCells(list) {
		ok := env.FunCall(pred, SExpr([]*LVal{v}))
		if ok.Type == LError {
			return ok
		}
		if True(ok) {
			cells = append(cells, v)
		}
	}
	switch typespec.Str {
	case "list":
		v.Cells = cells
	case "vector":
		v.Cells[0].Cells[0].Int = len(cells)
		v.Cells[1].Cells = cells
	}
	return v
}

func builtinReject(env *LEnv, args *LVal) *LVal {
	typespec, pred, list := args.Cells[0], args.Cells[1], args.Cells[2]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("second argument is not a function: %v", pred.Type)
	}
	if pred.IsSpecialFun() {
		return env.Errorf("second argument is not a regular function")
	}
	if !isSeq(list) {
		return env.Errorf("third argument is not a proper sequence: %v", list.Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = seqCells(v)
		cells = cells[0:0:list.Len()]
	case "list":
		v = QExpr(nil)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for _, v := range seqCells(list) {
		ok := env.FunCall(pred, SExpr([]*LVal{v}))
		if ok.Type == LError {
			return ok
		}
		if !True(ok) {
			cells = append(cells, v)
		}
	}
	switch typespec.Str {
	case "list":
		v.Cells = cells
	case "vector":
		v.Cells[0].Cells[0].Int = len(cells)
		v.Cells[1].Cells = cells
	}
	return v
}

func builtinZip(env *LEnv, args *LVal) *LVal {
	typespec, lists := args.Cells[0], args.Cells[1:]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	n := lists[0].Len()
	for _, list := range lists {
		if !isSeq(list) {
			return env.Errorf("argument is not a proper list: %v", list.Type)
		}
		m := list.Len()
		if m < n {
			n = m
		}
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(n)}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, n)
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for i := range cells {
		var elem *LVal
		var elemCells []*LVal
		switch typespec.Str {
		case "vector":
			elem = Array(QExpr([]*LVal{Int(len(lists))}), nil)
			elemCells = seqCells(elem)
		case "list":
			elemCells = make([]*LVal, len(lists))
			elem = QExpr(elemCells)
		default:
			return env.Errorf("type specifier is invalid: %v", typespec)
		}
		for j, list := range lists {
			elemCells[j] = seqCells(list)[i]
		}
		cells[i] = elem
	}
	return v
}

func builtinMakeSequence(env *LEnv, args *LVal) *LVal {
	start, stop := args.Cells[0], args.Cells[1]
	if !start.IsNumeric() {
		return env.Errorf("first argument is not numeric: %v", start.Type)
	}
	if !stop.IsNumeric() {
		return env.Errorf("second argument is not numeric: %v", stop.Type)
	}
	var step *LVal
	if len(args.Cells) == 2 {
		if start.Type == LInt {
			step = Int(1)
		} else {
			step = Float(1.0)
		}
	} else {
		if len(args.Cells) > 3 {
			return env.Errorf("too many arguments provided")
		}
		step = args.Cells[2]
		if !step.IsNumeric() {
			return env.Errorf("third argument is not numeric: %v", step.Type)
		}
		if !lessNumeric(Float(0), step) {
			return env.Errorf("third argument is not positive")
		}
	}
	list := QExpr(nil)
	for x := start; lessNumeric(x, stop); x = addNumeric(x, step) {
		list.Cells = append(list.Cells, x.Copy())
	}
	return list
}

func builtinReverse(env *LEnv, args *LVal) *LVal {
	typespec, list := args.Cells[0], args.Cells[1]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("first argument is not a proper sequence: %v", args.Cells[0].Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for i, v := range seqCells(list) {
		cells[len(cells)-1-i] = v
	}
	return v
}

func builtinSlice(env *LEnv, args *LVal) *LVal {
	typespec, list, start, end := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) && list.Type != LString && list.Type != LBytes {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if start.Type != LInt {
		return env.Errorf("third argument is not an integer: %v", start.Type)
	}
	if end.Type != LInt {
		return env.Errorf("forth argument is not an integer: %v", end.Type)
	}
	n := list.Len()
	i := start.Int
	j := end.Int
	if i < 0 {
		return env.Errorf("index out of range")
	}
	if i > n {
		return env.Errorf("index out of range")
	}
	if j < 0 {
		return env.Errorf("index out of range")
	}
	if j > n {
		return env.Errorf("index out of range")
	}
	if i > j {
		return env.Errorf("end before start")
	}

	// Create an intermediate sliced list with a similar type
	switch list.Type {
	case LString:
		list = String(list.Str[i:j])
	case LBytes:
		list = Bytes(list.Bytes()[i:j])
	default: // isSeq(list)
		list = QExpr(seqCells(list)[i:j])
	}

	// Convert the intermediate sliced value into the desired type
	switch typespec.Str {
	case "string":
		switch list.Type {
		case LString:
			return list
		case LBytes:
			return String(string(list.Bytes()))
		default: // LSExpr
			var b []byte
			err := appendBytes(env, list, func(x byte) {
				b = append(b, x)
			})
			if err != nil {
				return env.Error(err)
			}
			return String(string(b))
		}
	case "bytes":
		switch list.Type {
		case LString:
			return Bytes([]byte(list.Str))
		case LBytes:
			return list
		default: // LSExpr
			var b []byte
			err := appendBytes(env, list, func(x byte) {
				b = append(b, x)
			})
			if err != nil {
				return env.Error(err)
			}
			return Bytes(b)
		}
	case "list":
		if list.Type == LString || list.Type == LBytes {
			list = makeByteSeq(list)
		}
		// list is now known to be LSExpr
		return QExpr(list.Cells)
	case "vector":
		if list.Type == LString || list.Type == LBytes {
			list = makeByteSeq(list)
		}
		// list is now known to be LSExpr
		return Array(QExpr([]*LVal{Int(len(list.Cells))}), list.Cells)
	default:
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
}

func builtinList(env *LEnv, v *LVal) *LVal {
	return QExpr(v.Cells)
}

func builtinVector(env *LEnv, args *LVal) *LVal {
	return Array(nil, args.Cells)
}

func builtinAppendMutate(env *LEnv, args *LVal) *LVal {
	vec, vals := args.Cells[0], args.Cells[1:]
	if vec.Type == LBytes {
		return appendMutateBytes(env, args)
	}
	if !isVec(vec) {
		return env.Errorf("first argument is not a vector: %v", vec.Type)
	}
	dims := vec.Cells[0]
	dims.Cells[0].Int += len(vals)
	vec.Cells[1].Cells = append(vec.Cells[1].Cells, vals...)
	return vec
}

func appendMutateBytes(env *LEnv, args *LVal) *LVal {
	lbytes, xs := args.Cells[0], args.Cells[1:]
	b := lbytes.Bytes()
	err := appendBytes(env, QExpr(xs), func(x byte) {
		b = append(b, x)
	})
	if err != nil {
		return env.Error(err)
	}
	*lbytes.Native.(*[]byte) = b
	return lbytes
}

func builtinAppendBytesMutate(env *LEnv, args *LVal) *LVal {
	lbytes, byteseq := args.Cells[0], args.Cells[1]
	if lbytes.Type != LBytes {
		return env.Errorf("first argument is not bytes: %v", lbytes.Type)
	}
	b := lbytes.Bytes()
	switch byteseq.Type {
	case LString:
		b = append(b, byteseq.Str...)
	case LBytes:
		b = append(b, byteseq.Bytes()...)
	default:
		err := appendBytes(env, byteseq, func(x byte) {
			b = append(b, x)
		})
		if err != nil {
			return env.Error(err)
		}
	}
	*lbytes.Native.(*[]byte) = b
	return lbytes
}

func builtinAppend(env *LEnv, args *LVal) *LVal {
	typespec, seq, vals := args.Cells[0], args.Cells[1], args.Cells[2:]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	if typespec.Str == "bytes" {
		return builtinAppend_Bytes(env, args)
	}
	if !isSeq(seq) {
		return env.Errorf("second argument is not a proper sequence: %v", seq.Type)
	}
	cells := seqCells(seq)
	switch typespec.Str {
	case "list":
		// The Cells of the returned list must not intersect with seqCells(seq)
		// (in regards to slice memory regions, not referenced LVals).  This is
		// particularly relevent if seq is a vector.  This makes the following
		// code more verbose.
		list := make([]*LVal, 0, len(cells)+len(vals))
		list = append(list, cells...)
		list = append(list, vals...)
		return QExpr(list)
	case "vector":
		// The Cells of the returned vector may intersect with seqCells(seq)
		// when chaining calls to ``append'' so that vectors may be used in a
		// manner akin to go slices.
		return Array(nil, append(cells, vals...))
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
}

// NOTE:  The name of this function is funky because the append-bytes function
// will have other behavior, and there is another func in this file called
// appendBytes.
func builtinAppend_Bytes(env *LEnv, args *LVal) *LVal {
	// the type sequence has already been validated.
	_, lbytes, xs := args.Cells[0], args.Cells[1], args.Cells[2:]
	b := lbytes.Bytes()
	err := appendBytes(env, QExpr(xs), func(x byte) {
		b = append(b, x)
	})
	if err != nil {
		return env.Error(err)
	}
	return Bytes(b)
}

func builtinAppendBytes(env *LEnv, args *LVal) *LVal {
	lbytes, byteseq := args.Cells[0], args.Cells[1]
	if lbytes.Type != LBytes {
		return env.Errorf("first argument is not bytes: %v", lbytes.Type)
	}
	b := lbytes.Bytes()
	switch byteseq.Type {
	case LString:
		b = append(b, byteseq.Str...)
	case LBytes:
		b = append(b, byteseq.Bytes()...)
	default:
		err := appendBytes(env, byteseq, func(x byte) {
			b = append(b, x)
		})
		if err != nil {
			return env.Error(err)
		}
	}
	return Bytes(b)
}

func builtinARef(env *LEnv, args *LVal) *LVal {
	array, indices := args.Cells[0], args.Cells[1:]
	if array.Type != LArray {
		return env.Errorf("first argument is not an array: %v", array.Type)
	}
	v := array.ArrayIndex(indices...)
	if v.Type == LError {
		return env.Error(v)
	}
	return v
}

func builtinLength(env *LEnv, args *LVal) *LVal {
	seq := args.Cells[0]
	n := seq.Len()
	if n < 0 {
		return env.Errorf("first argument is not a list, map, vector, bytes, or a string: %v", seq.Type)
	}
	return Int(n)
}

func builtinIsEmpty(env *LEnv, args *LVal) *LVal {
	seq := args.Cells[0]
	n := seq.Len()
	if n < 0 {
		return env.Errorf("first argument is not a list, map, vector, bytes, or a string: %v", seq.Type)
	}
	return Bool(n == 0)
}

func builtinCons(env *LEnv, args *LVal) *LVal {
	head, tail := args.Cells[0], args.Cells[1]
	if tail.Type != LSExpr {
		return env.Errorf("second argument is not a list: %s", tail.Type)
	}
	cells := make([]*LVal, 0, 1+args.Len())
	cells = append(cells, head)
	cells = append(cells, tail.Cells...)
	return QExpr(cells)
}

func builtinNot(env *LEnv, args *LVal) *LVal {
	return Bool(Not(args.Cells[0]))
}

func builtinIsTrue(env *LEnv, args *LVal) *LVal {
	return Bool(True(args.Cells[0]))
}

func builtinType(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return GetType(v)
}

func builtinIsType(env *LEnv, args *LVal) *LVal {
	typespec := args.Cells[0]
	v := args.Cells[1]
	if typespec.Type != LSymbol && typespec.Type != LTaggedVal {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	typesym := typespec.Str
	if typespec.Type == LTaggedVal {
		if typesym != fmt.Sprintf("%s:typedef", env.Runtime.Registry.Lang) {
			return env.Errorf("first argument is not a valid type specifier: %v", typesym)
		}
		typesym = typespec.Cells[0].Cells[0].Str
	}
	t := GetType(v)
	return Bool(t.Str == typesym)
}

func builtinIsTaggedVal(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LTaggedVal)
}

func builtinUserData(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type != LTaggedVal {
		return env.Errorf("argument is not a tagged value: %v", GetType(v))
	}
	return v.UserData()
}

func builtinNew(env *LEnv, args *LVal) *LVal {
	typespec := args.Cells[0]
	cargs := args.Cells[1:]
	if typespec.Type == LSymbol {
		typespec = env.GetGlobal(typespec)
		if typespec.Type == LError {
			return typespec
		}
	}
	return env.New(typespec, SExpr(cargs))
}

func builtinIsNil(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.IsNil() {
		return Bool(true)
	}
	return Bool(false)
}

func builtinIsList(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSExpr)
}

func builtinIsSortedMap(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSortMap)
}

func builtinIsArray(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LArray)
}

func builtinIsVector(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LArray && v.Cells[0].Len() == 1)
}

func builtinIsBool(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSymbol && (v.Str == TrueSymbol || v.Str == FalseSymbol))
}

func builtinIsNumber(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.IsNumeric())
}

func builtinIsInt(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LInt)
}

func builtinIsFloat(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LFloat)
}

func builtinIsSymbol(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSymbol)
}

func builtinIsString(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LString)
}

func builtinIsBytes(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LBytes)
}

func builtinEqual(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	return a.Equal(b)
}

func builtinAllP(env *LEnv, args *LVal) *LVal {
	pred, list := args.Cells[0], args.Cells[1]
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("first argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if !True(ok) {
			return Bool(false)
		}
	}
	return Bool(true)
}

func builtinAnyP(env *LEnv, args *LVal) *LVal {
	pred, list := args.Cells[0], args.Cells[1]
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("first argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a list: %v", list.Type)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if True(ok) {
			return ok
		}
	}
	return Bool(false)
}

func builtinMax(env *LEnv, args *LVal) *LVal {
	max := args.Cells[0]
	if !max.IsNumeric() {
		return env.Errorf("argument is not a number: %s", max.Type)
	}
	for _, x := range args.Cells[1:] {
		if !x.IsNumeric() {
			return env.Errorf("argument is not a number: %s", x.Type)
		}
		if lessNumeric(max, x) {
			max = x
		}
	}
	return max
}

func builtinMin(env *LEnv, args *LVal) *LVal {
	min := args.Cells[0]
	if !min.IsNumeric() {
		return env.Errorf("argument is not a number: %s", min.Type)
	}
	for _, x := range args.Cells[1:] {
		if !x.IsNumeric() {
			return env.Errorf("argument is not a number: %s", x.Type)
		}
		if lessNumeric(x, min) {
			min = x
		}
	}
	return min
}

func builtinStringLEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str <= b.Str)
}

func builtinStringLT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str < b.Str)
}

func builtinStringGEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str >= b.Str)
}

func builtinStringGT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str > b.Str)
}

// BUG:  Symbol equality is not well defined and so this function's
// implemantion may need to change in an incompatible way.
func builtinSymbolEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", a.Type)
	}
	if b.Type != LSymbol {
		return env.Errorf("second argument is not a symbol: %s", b.Type)
	}
	return Bool(a.Str == b.Str)
}

func builtinStringEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str == b.Str)
}

func builtinLEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int <= b.Int)
	}
	return Bool(toFloat(a) <= toFloat(b))
}

func builtinLT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int < b.Int)
	}
	return Bool(toFloat(a) < toFloat(b))
}

func builtinGEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int >= b.Int)
	}
	return Bool(toFloat(a) >= toFloat(b))
}

func builtinGT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int > b.Int)
	}
	return Bool(toFloat(a) > toFloat(b))
}

func builtinEqNum(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	return a.equalNum(b)
}

func builtinPow(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return powInt(a.Int, b.Int)
	}
	return Float(math.Pow(toFloat(a), toFloat(b)))
}

func powInt(a, b int) *LVal {
	if b == 0 {
		return Int(1)
	}
	if b < 0 {
		return Float(math.Pow(float64(a), float64(b)))
	}
	n := 1
	atob := a
	for 2*n < b {
		atob *= atob
		n *= 2
	}
	for n < b {
		atob *= a
		n++
	}
	return Int(atob)
}

func builtinMod(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LInt {
		return env.Errorf("first argument is not an int: %s", a.Type)
	}
	if b.Type != LInt {
		return env.Errorf("second argument is not an int: %s", b.Type)
	}
	if b.Int == 0 {
		return env.Errorf("second argument is zero")
	}
	return Int(a.Int % b.Int)
}

func builtinAdd(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(0)
	}
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}
	elemt := numericListType(v.Cells)
	if elemt == LInt {
		sum := 0
		for _, c := range v.Cells {
			sum += c.Int
		}
		return Int(sum)
	}
	sum := 0.0
	for _, c := range v.Cells {
		sum += toFloat(c)
	}
	return Float(sum)
}

func builtinSub(env *LEnv, v *LVal) *LVal {
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}

	if len(v.Cells) == 0 {
		return Int(0)
	}

	if len(v.Cells) == 1 {
		x := v.Cells[0]
		switch v.Cells[0].Type {
		case LInt:
			return Int(-x.Int)
		case LFloat:
			return Float(-x.Float)
		default:
			// indicates some bug in IsNumeric or this switch statement
			return env.Errorf("invalid numeric type: %v", x.Type)
		}
	}

	elemt := numericListType(v.Cells)
	if elemt == LInt {
		diff := v.Cells[0].Int
		for _, c := range v.Cells[1:] {
			diff -= c.Int
		}
		return Int(diff)
	}
	diff := toFloat(v.Cells[0])
	for _, c := range v.Cells[1:] {
		diff -= toFloat(c)
	}
	return Float(diff)
}

func builtinDiv(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(1)
	}

	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}

	if len(v.Cells) == 1 {
		// This is kind of dumb but it catches the case 1/1 which should return
		// an int.
		return divInt(Int(1), v)
	}

	// Attempt to perform integer division when all operands are integers and
	// division is exact.
	if v.Cells[0].Type == LInt {
		return divInt(v.Cells[0], SExpr(v.Cells[1:]))
	}
	return divFloat(v.Cells[0], SExpr(v.Cells[1:]))
}

// divInt tries to perform division as int if all quotients divide the previous
// result.
func divInt(x, args *LVal) *LVal {
	if x.Type != LInt {
		panic("non-integer first argument")
	}
	ys := args.Cells
	for i := range ys {
		y := ys[i]
		switch {
		case y.Type != LInt:
			return divFloat(x, SExpr(ys[i:]))
		case y.Int == 0:
			// The result should be NaN, +Inf, or -Inf depending on the
			// remaining ys.  These computations should be carried out in
			// IEEE754 floating point for accuracy.
			return divFloat(x, SExpr(ys[i:]))
		case x.Int%y.Int != 0:
			return divFloat(x, SExpr(ys[i:]))
		default:
			// y divides x
			x = Int(x.Int / y.Int)
		}
	}
	return x
}

// divFloat performs float point division (the default).  divFloat assumes that
// there is at least one argument.
func divFloat(x, args *LVal) *LVal {
	div := x.Float
	if x.Type == LInt {
		div = float64(x.Int)
	}
	for _, c := range args.Cells {
		if c.Type == LInt {
			div /= float64(c.Int)
		} else {
			div /= c.Float
		}
	}
	return Float(div)
}

func builtinMul(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(1)
	}
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}
	return mulInt(Int(1), v)
}

// mulInt tries to perform multiplication as int if all arguments are int.
func mulInt(x, args *LVal) *LVal {
	if x.Type != LInt {
		panic("non-integer first argument")
	}
	ys := args.Cells
	for i := range ys {
		y := ys[i]
		switch {
		case y.Type != LInt:
			return mulFloat(x, SExpr(ys[i:]))
		default:
			x = Int(x.Int * y.Int)
		}
	}
	return x
}

// mulFloat performs floating point multiplication.
func mulFloat(x, args *LVal) *LVal {
	// NOTE:  We can't short-circuit upon seeing the value 0 because of
	// interactions when multplying 0 by Inf.  It is possible that
	// short-circuiting on NaN is viable, though this is probably not common
	// and the benefits are unclear.
	prod := x.Float
	if x.Type == LInt {
		prod = float64(x.Int)
	}
	for _, c := range args.Cells {
		if c.Type == LInt {
			prod *= float64(c.Int)
		} else {
			prod *= c.Float
		}
	}
	return Float(prod)
}

func builtinDebugPrint(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		fmt.Println()
		return Nil()
	}
	fmtargs := make([]interface{}, len(args.Cells))
	for i := range args.Cells {
		fmtargs[i] = args.Cells[i]
	}
	fmt.Fprintln(env.Runtime.getStderr(), fmtargs...)
	return Nil()
}

func builtinDebugStack(env *LEnv, args *LVal) *LVal {
	_, err := env.Runtime.Stack.DebugPrint(env.Runtime.getStderr())
	if err != nil {
		return env.Error(err)
	}
	return Nil()
}

func addNumeric(a, b *LVal) *LVal {
	if bothInt(a, b) {
		return Int(a.Int + b.Int)
	}
	return Float(toFloat(a) + toFloat(b))
}

func lessNumeric(a, b *LVal) bool {
	if bothInt(a, b) {
		return a.Int < b.Int
	}
	return toFloat(a) < toFloat(b)
}

func bothInt(a, b *LVal) bool {
	if a.Type == LInt && b.Type == LInt {
		return true
	}
	return false
}

func allInt(vs []*LVal) bool {
	for _, v := range vs {
		if v.Type != LInt {
			return false
		}
	}
	return true
}

func numericListType(cells []*LVal) LType {
	if len(cells) == 0 {
		return LInvalid
	}
	if !cells[0].IsNumeric() {
		return cells[0].Type
	}
	t := cells[0].Type
	for _, c := range cells[1:] {
		if t == c.Type {
			continue
		}
		if c.IsNumeric() {
			t = LFloat
		}
	}
	return t
}

func toFloat(x *LVal) float64 {
	if !x.IsNumeric() {
		panic("toFloat called with non-numeric argument: " + x.String())
	}
	if x.Type == LInt {
		return float64(x.Int)
	}
	return x.Float
}

func builtinFormatString(env *LEnv, args *LVal) *LVal {
	format := args.Cells[0]
	fvals := args.Cells[1:]
	if format.Type != LString {
		return env.Errorf("first argument is not a string")
	}
	parts, err := parseFormatString(format.Str)
	if err != nil {
		return env.Error(err)
	}
	var buf bytes.Buffer
	anonIndex := 0
	for _, p := range parts {
		if strings.HasPrefix(p, "{") && strings.HasSuffix(p, "}") {
			p = strings.Join(strings.Fields(p), "")
			// TODO:  Allow non-empty formatting directives
			if p != "{}" {
				return env.Errorf("formatting direcives must be empty")
			}
			if anonIndex >= len(fvals) {
				return env.Errorf("too many formatting direcives for supplied values")
			}
			val := fvals[anonIndex]
			if val.Type == LString && !val.Quoted {
				buf.WriteString(val.Str)
			} else {
				buf.WriteString(val.String())
			}
			anonIndex++
		} else {
			buf.WriteString(p)
		}
	}

	return String(buf.String())
}

func parseFormatString(f string) ([]string, error) {
	var s []string
	tokens := tokenizeFormatString(f)
	for len(tokens) > 0 {
		tok := tokens[0]
		if tok.typ == formatText {
			s = append(s, tok.text)
			tokens = tokens[1:]
			continue
		}
		if tok.typ == formatClose {
			if len(tokens) == 0 || tokens[0].typ != formatClose {
				return nil, fmt.Errorf("unexpected closing brace '}' outside of formatting direcive")
			}
			s = append(s, "}")
			tokens = tokens[2:]
		}
		if len(tokens) < 2 {
			return nil, fmt.Errorf("unclosed formatting directive")
		}
		switch tokens[1].typ {
		case formatOpen:
			s = append(s, "{")
			tokens = tokens[2:]
			continue
		case formatClose:
			s = append(s, "{}")
			tokens = tokens[2:]
			continue
		case formatText:
			if len(tokens) < 3 {
				return nil, fmt.Errorf("unclosed formatting directive")
			}
			if tokens[2].typ != formatClose {
				return nil, fmt.Errorf("invalid formatting directive")
			}
			s = append(s, "{"+tokens[1].text+"}")
			tokens = tokens[3:]
			continue
		default:
			panic("unknown type")
		}
	}
	return s, nil
}

func tokenizeFormatString(f string) []formatToken {
	var tokens []formatToken
	for {
		i := strings.IndexAny(f, "{}")
		if i < 0 {
			tokens = append(tokens, formatToken{formatText, f})
			return tokens
		}
		if i > 0 {
			tokens = append(tokens, formatToken{formatText, f[:i]})
			f = f[i:]
		}
		if f[0] == '{' {
			tokens = append(tokens, formatToken{formatOpen, "{"})
			f = f[1:]
		} else {
			tokens = append(tokens, formatToken{formatClose, "}"})
			f = f[1:]
		}
	}
}

type formatTokenType uint

const (
	formatText formatTokenType = iota
	formatOpen
	formatClose
)

type formatToken struct {
	typ  formatTokenType
	text string
}
