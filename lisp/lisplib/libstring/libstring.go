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
	env.SetPackageDoc(`String manipulation: case conversion, splitting, joining,
		repetition, and trimming.`)
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

var builtins = []*libutil.Builtin{
	libutil.FunctionDoc("lowercase", lisp.Formals("str"), builtinLower,
		`Returns a copy of str with all Unicode characters converted to
		lowercase.`),
	libutil.FunctionDoc("uppercase", lisp.Formals("str"), builtinUpper,
		`Returns a copy of str with all Unicode characters converted to
		uppercase.`),
	libutil.FunctionDoc("split", lisp.Formals("str", "sep"), builtinSplit,
		`Splits str on each occurrence of the separator string sep and
		returns a list of the substrings between separators. If sep is
		empty, splits after each UTF-8 character.`),
	libutil.FunctionDoc("join", lisp.Formals("list", "sep"), builtinJoin,
		`Concatenates a list of strings with sep inserted between each
		element. Returns a single string. All elements of list must be
		strings.`),
	libutil.FunctionDoc("repeat", lisp.Formals("str", "n"), builtinRepeat,
		`Returns a new string consisting of n copies of str concatenated
		together. n must be a non-negative integer.`),
	libutil.FunctionDoc("trim-space", lisp.Formals("str"), builtinTrimSpace,
		`Returns str with all leading and trailing whitespace removed
		(spaces, tabs, newlines, etc.).`),
	libutil.FunctionDoc("trim", lisp.Formals("str", "cutset"), builtinTrim,
		`Returns str with all leading and trailing characters found in
		cutset removed. The cutset is a string of individual characters
		to trim, not a prefix/suffix.`),
	libutil.FunctionDoc("trim-left", lisp.Formals("str", "cutset"), builtinTrimLeft,
		`Returns str with all leading characters found in cutset removed.
		The cutset is a string of individual characters to trim.`),
	libutil.FunctionDoc("trim-right", lisp.Formals("str", "cutset"), builtinTrimRight,
		`Returns str with all trailing characters found in cutset removed.
		The cutset is a string of individual characters to trim.`),
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
