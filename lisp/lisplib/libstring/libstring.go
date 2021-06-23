// Copyright © 2018 The ELPS authors

package libstring

import (
	"bytes"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "string"

// LoadPackage adds the math package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.Function("lowercase", lisp.Formals("str"), builtinLower),
	libutil.Function("uppercase", lisp.Formals("str"), builtinUpper),
	libutil.Function("split", lisp.Formals("str", "sep"), builtinSplit),
	libutil.Function("join", lisp.Formals("list", "sep"), builtinJoin),
	libutil.Function("repeat", lisp.Formals("str", "n"), builtinRepeat),
	libutil.Function("trim-space", lisp.Formals("str"), builtinTrimSpace),
	libutil.Function("trim", lisp.Formals("str", "cutset"), builtinTrim),
	libutil.Function("trim-left", lisp.Formals("str", "cutset"), builtinTrimLeft),
	libutil.Function("trim-right", lisp.Formals("str", "cutset"), builtinTrimRight),
	libutil.Function("prefix?", lisp.Formals("str", "prefix"), builtinHasPrefix),
	libutil.Function("suffix?", lisp.Formals("str", "suffix"), builtinHasSuffix),
	libutil.Function("trim-prefix", lisp.Formals("str", "prefix"), builtinTrimPrefix),
	libutil.Function("trim-suffix", lisp.Formals("str", "suffix"), builtinTrimSuffix),
}

func builtinLower(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	if str.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", str.Type)
	}
	return lisp.String(strings.ToLower(str.Str))
}

func builtinUpper(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	if str.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", str.Type)
	}
	return lisp.String(strings.ToUpper(str.Str))
}

func builtinSplit(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str, sep := args.Cells[0], args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if sep.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", sep.Type)
	}
	slice := strings.Split(str.Str, sep.Str)
	cells := make([]*lisp.LVal, len(slice))
	for i, s := range slice {
		cells[i] = lisp.String(s)
	}
	return lisp.QExpr(cells)
}

func builtinJoin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	list, sep := args.Cells[0], args.Cells[1]
	if list.Type != lisp.LSExpr {
		return env.Errorf("first argument is not a list: %v", list.Type)
	}
	if sep.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", sep.Type)
	}
	var buf bytes.Buffer
	for i, cell := range list.Cells {
		if cell.Type != lisp.LString {
			return env.Errorf("first argument is not a list of strings: %v", cell.Type)
		}
		buf.WriteString(cell.Str)
		if i < len(list.Cells)-1 {
			buf.WriteString(sep.Str)
		}
	}
	return lisp.String(buf.String())
}

func builtinRepeat(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	n := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if n.Type != lisp.LInt {
		return env.Errorf("second argument is not an int: %v", n.Type)
	}
	if n.Int < 0 {
		return env.Errorf("count is negative: %v", n.Int)
	}
	return lisp.String(strings.Repeat(str.Str, n.Int))
}

func builtinTrimSpace(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	return lisp.String(strings.TrimSpace(str.Str))
}

func builtinTrim(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	cutset := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if cutset.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", cutset.Type)
	}
	return lisp.String(strings.Trim(str.Str, cutset.Str))
}

func builtinTrimLeft(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	cutset := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if cutset.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", cutset.Type)
	}
	return lisp.String(strings.TrimLeft(str.Str, cutset.Str))
}

func builtinTrimRight(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	cutset := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if cutset.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", cutset.Type)
	}
	return lisp.String(strings.TrimRight(str.Str, cutset.Str))
}

func builtinHasPrefix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	prefix := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if prefix.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", prefix.Type)
	}
	return lisp.Bool(strings.HasPrefix(str.Str, prefix.Str))
}

func builtinHasSuffix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	prefix := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if prefix.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", prefix.Type)
	}
	return lisp.Bool(strings.HasSuffix(str.Str, prefix.Str))
}

func builtinTrimPrefix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	prefix := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if prefix.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", prefix.Type)
	}
	return lisp.String(strings.TrimPrefix(str.Str, prefix.Str))
}

func builtinTrimSuffix(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	str := args.Cells[0]
	prefix := args.Cells[1]
	if str.Type != lisp.LString {
		return env.Errorf("first argument is not a string: %v", str.Type)
	}
	if prefix.Type != lisp.LString {
		return env.Errorf("second argument is not a string: %v", prefix.Type)
	}
	return lisp.String(strings.TrimSuffix(str.Str, prefix.Str))
}
