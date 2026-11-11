package lisp

type LVal struct {
	Cells []*LVal
	Type  int
}

func (v *LVal) Len() int { return len(v.Cells) } // want Len:"callListSafe\\[-1\\]"

func (v *LVal) Keep() {
	keep = v.Cells // want "is stored where it outlives the call .reached from badMethod"
}

func (v *LVal) Arg(i int) *LVal { return v.Cells[i] } // want Arg:"callListSafe\\[-1\\]"

type LEnv struct{ kept *LVal }

func (env *LEnv) Eval(v *LVal) *LVal { return v }

func (env *LEnv) Errorf(format string, args ...interface{}) *LVal { return nil }

type LBuiltin func(env *LEnv, args *LVal) *LVal

type LBuiltinDef interface {
	Name() string
	Eval(env *LEnv, args *LVal) *LVal
}

func DropsArgList(def LBuiltinDef) LBuiltinDef { return def }

func FreeKeywords(def LBuiltinDef) LBuiltinDef { return def }

// ReadArgs keeps its list in the value it returns.
func ReadArgs(env *LEnv, args *LVal) *LEnv {
	return &LEnv{kept: args}
}

// CellOf keeps nothing.
func CellOf(args *LVal, i int) *LVal { return args.Cells[i] } // want CellOf:"callListSafe\\[0\\]"

var keep []*LVal

var callListDroppers = []LBuiltin{
	okIndex,
	okRangeAndSlices,
	okLocalAlias,
	okDecap,
	okHelper,
	okUnnamed,
	okMethod,
	okRecursion,
	badReturnsList,
	badReturnsCells,
	badStoresSlice,
	badStoresList,
	badClosure,
	badMethod,
	badMethodValue,
	badFieldAddress,
	badUnknownCallee,
	badHelper,
	badVariadic,
	badAppendTo,
	badCompositeLiteral,
	badRecursion,
	notAFunction, // want "callListDroppers entry is not a top-level function of package lisp"
}

var notAFunction LBuiltin

// A single cell may go anywhere: the list does not own it.
func okIndex(env *LEnv, args *LVal) *LVal { // want okIndex:"callListSafe\\[1\\]"
	x, y := args.Cells[0], args.Cells[1]
	if args == nil || args.Cells == nil || args.Type == 0 {
		return y
	}
	keep = append(keep, x)
	args.Cells[1] = x
	args.Type = 1
	return env.Eval(args.Cells[len(args.Cells)-1])
}

func okRangeAndSlices(env *LEnv, args *LVal) *LVal { // want okRangeAndSlices:"callListSafe\\[1\\]"
	var last *LVal
	for _, c := range args.Cells[:len(args.Cells)-1] {
		last = env.Eval(c)
	}
	for i := range args.Cells {
		_ = i
	}
	out := make([]*LVal, 0, cap(args.Cells))
	out = append(out, args.Cells[1:]...)
	copy(out, (args.Cells))
	return &LVal{Cells: out[:1], Type: last.Type}
}

func okLocalAlias(env *LEnv, s *LVal) *LVal { // want okLocalAlias:"callListSafe\\[1\\]"
	cond, body := s.Cells[0], s.Cells[1:]
	var rest = body[1:]
	list := s
	for _, c := range rest {
		env.Eval(c)
	}
	_ = list.Cells[0]
	return env.Eval(body[len(body)-1]).Cells[cond.Type]
}

func okDecap(env *LEnv, args *LVal) *LVal { // want okDecap:"callListSafe\\[1\\]"
	args.Cells = args.Cells[1:]
	return okIndex(env, args)
}

func okHelper(env *LEnv, args *LVal) *LVal { // want okHelper:"callListSafe\\[1\\]"
	return env.Eval(helperOK(args.Cells, 0))
}

func helperOK(cells []*LVal, i int) *LVal { // want helperOK:"callListSafe\\[0\\]"
	if len(cells) == 0 {
		return nil
	}
	return helperOK(cells[1:], i+1)
}

func okUnnamed(_ *LEnv, _ *LVal) *LVal { return nil } // want okUnnamed:"callListSafe\\[1\\]"

// A method call is read like a call: the list is the receiver.
func okMethod(env *LEnv, args *LVal) *LVal { // want okMethod:"callListSafe\\[1\\]"
	if args.Len() == 0 {
		return nil
	}
	return args.Arg(0)
}

// A recursion is safe when no function in it keeps the list.
func okRecursion(env *LEnv, args *LVal) *LVal { // want okRecursion:"callListSafe\\[1\\]"
	if len(args.Cells) == 0 {
		return nil
	}
	return okRecursionHelper(env, args)
}

func okRecursionHelper(env *LEnv, args *LVal) *LVal { // want okRecursionHelper:"callListSafe\\[1\\]"
	return okRecursion(env, args)
}

func badReturnsList(env *LEnv, args *LVal) *LVal {
	return args // want "the argument list, or a slice of its cells, is returned"
}

func badReturnsCells(env *LEnv, args *LVal) *LVal {
	return &LVal{Cells: args.Cells[1:]} // want "a slice of the argument list's cells is used in a way this rule does not allow"
}

func badStoresSlice(env *LEnv, args *LVal) *LVal {
	rest := args.Cells[1:]
	keep = rest // want "is stored where it outlives the call"
	return nil
}

func badStoresList(env *LEnv, args *LVal) *LVal {
	env.kept = args // want "is stored where it outlives the call"
	return nil
}

func badClosure(env *LEnv, args *LVal) *LVal {
	f := func() *LVal { return args.Cells[0] } // want "captured by a function literal"
	return f()
}

func badMethod(env *LEnv, args *LVal) *LVal {
	args.Keep()
	return args.Cells[0]
}

var keepLen func() int

var keepType *int

func badMethodValue(env *LEnv, args *LVal) *LVal {
	keepLen = args.Len // want "a method value is bound to the argument list"
	return nil
}

func badFieldAddress(env *LEnv, args *LVal) *LVal {
	keepType = &args.Type // want "the address of a field of the argument list is taken"
	return nil
}

func badUnknownCallee(env *LEnv, args *LVal) *LVal {
	f := env.Eval
	return f(args) // want "which this rule cannot read"
}

func badHelper(env *LEnv, args *LVal) *LVal {
	return helperKeeps(args)
}

func helperKeeps(list *LVal) *LVal {
	keep = list.Cells // want "is stored where it outlives the call .reached from badHelper"
	return nil
}

func badVariadic(env *LEnv, args *LVal) *LVal {
	return env.Errorf("bad arguments: %v", args) // want "passed as a variadic argument of Errorf"
}

func badAppendTo(env *LEnv, args *LVal) *LVal {
	grown := append(args.Cells, nil) // want "passed to append, whose result aliases it"
	return grown[0]
}

func badCompositeLiteral(env *LEnv, args *LVal) *LVal {
	return []*LVal{args}[0].Cells[0] // want "the argument list is used in a way this rule does not allow"
}

// A recursion is unsafe when one function in it keeps the list.
func badRecursion(env *LEnv, args *LVal) *LVal {
	if len(args.Cells) == 0 {
		return badRecursionHelper(env, args)
	}
	return badRecursion(env, args)
}

func badRecursionHelper(env *LEnv, args *LVal) *LVal {
	env.kept = args // want "is stored where it outlives the call .reached from badRecursion"
	return nil
}

// ReadArgsLike is an exported builtin that keeps its list.
func ReadArgsLike(env *LEnv, args *LVal) *LVal {
	env.kept = args
	return nil
}

// CellBuiltin is an exported builtin that keeps nothing.
func CellBuiltin(env *LEnv, args *LVal) *LVal { return args.Cells[0] } // want CellBuiltin:"callListSafe\\[1\\]"
