package lisp

type LVal struct {
	Cells []*LVal
	Type  int
}

func (v *LVal) Len() int { return len(v.Cells) }

type LEnv struct{ kept *LVal }

func (env *LEnv) Eval(v *LVal) *LVal { return v }

func (env *LEnv) Errorf(format string, args ...interface{}) *LVal { return nil }

type LBuiltin func(env *LEnv, args *LVal) *LVal

var keep []*LVal

var callListDroppers = []LBuiltin{
	okIndex,
	okRangeAndSlices,
	okLocalAlias,
	okDecap,
	okHelper,
	okUnnamed,
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
	notAFunction, // want "callListDroppers entry is not a top-level function of package lisp"
}

var notAFunction LBuiltin

// A single cell may go anywhere: the list does not own it.
func okIndex(env *LEnv, args *LVal) *LVal {
	x, y := args.Cells[0], args.Cells[1]
	if args == nil || args.Cells == nil || args.Type == 0 {
		return y
	}
	keep = append(keep, x)
	args.Cells[1] = x
	args.Type = 1
	return env.Eval(args.Cells[len(args.Cells)-1])
}

func okRangeAndSlices(env *LEnv, args *LVal) *LVal {
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

func okLocalAlias(env *LEnv, s *LVal) *LVal {
	cond, body := s.Cells[0], s.Cells[1:]
	var rest = body[1:]
	list := s
	for _, c := range rest {
		env.Eval(c)
	}
	_ = list.Cells[0]
	return env.Eval(body[len(body)-1]).Cells[cond.Type]
}

func okDecap(env *LEnv, args *LVal) *LVal {
	args.Cells = args.Cells[1:]
	return okIndex(env, args)
}

func okHelper(env *LEnv, args *LVal) *LVal {
	return env.Eval(helperOK(args.Cells, 0))
}

func helperOK(cells []*LVal, i int) *LVal {
	if len(cells) == 0 {
		return nil
	}
	return helperOK(cells[1:], i+1)
}

func okUnnamed(_ *LEnv, _ *LVal) *LVal { return nil }

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
	if args.Len() == 0 { // want "a method is called on the argument list"
		return nil
	}
	return args.Cells[0]
}

var keepLen func() int

var keepType *int

func badMethodValue(env *LEnv, args *LVal) *LVal {
	keepLen = args.Len // want "a method is called on the argument list, or bound to it"
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
