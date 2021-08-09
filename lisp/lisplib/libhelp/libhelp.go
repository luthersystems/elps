// Copyright © 2021 The ELPS authors

package libhelp

import (
	"fmt"
	"io"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
	"github.com/muesli/reflow/dedent"
	"github.com/muesli/reflow/indent"
	"github.com/muesli/reflow/wordwrap"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "help"

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
	for _, op := range ops {
		env.AddSpecialOps(true, op)
	}
	return lisp.Nil()
}

var ops = []*libutil.Builtin{
	libutil.FunctionDoc("help", lisp.Formals("var-name"), opHelp,
		`
		Prints documentation for the given variable name.  Functions have their
		signature and any docstring rendered.  Other variables have their types
		and current values printed.
		`),
	libutil.FunctionDoc("help-package", lisp.Formals("pkg-name"), opHelpPackage,
		`
		Prints documentation for exported symbols in the specified package.
		`),
	libutil.FunctionDoc("help-package-symbols", lisp.Formals("pkg-name", lisp.OptArgSymbol, "all"), opPackageSymbols,
		`
		Prints symbols defined in the specified package.  If a second argument
		is given which evaluates as true then unexported symbols in the package
		will also be printed.
		`),
}

func opHelp(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name := args.Cells[0]
	if name.Type != lisp.LSymbol {
		return env.Errorf("argument is not a symbol: %v", lisp.GetType(name))
	}
	err := RenderVar(env.Runtime.Stderr, env, name.Str)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func opHelpPackage(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name := args.Cells[0]
	if name.Type != lisp.LSymbol {
		return env.Errorf("argument is not a symbol: %v", lisp.GetType(name))
	}
	err := RenderPkgExported(env.Runtime.Stderr, env, name.Str)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Nil()
}

func opPackageSymbols(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name := args.Cells[0]
	if name.Type != lisp.LSymbol {
		return env.Errorf("argument is not a symbol: %v", lisp.GetType(name))
	}
	printAll := env.Eval(args.Cells[1])
	if printAll.Type == lisp.LError {
		return printAll
	}
	pkg := env.Runtime.Registry.Packages[name.Str]
	if pkg == nil {
		return env.Errorf("no package: %q", name)
	}
	if lisp.True(printAll) {
		for sym := range pkg.Symbols {
			_, err := fmt.Fprintln(env.Runtime.Stderr, sym)
			if err != nil {
				return env.Error(err)
			}
		}
	} else {
		exports := pkg.Externals
		for _, exsym := range exports {
			_, err := fmt.Fprintln(env.Runtime.Stderr, exsym)
			if err != nil {
				return env.Error(err)
			}
		}
	}
	return lisp.Nil()
}

func FunDocstring(v *lisp.LVal) string {
	if v.Builtin() != nil {
		if len(v.Cells) > 1 {
			return v.Cells[1].Str
		}
		return ""
	}
	// Functions of the form (lambda (x) "abc") are considered constant string
	// functions without documentation so there must be a length check on the
	// function body.
	if len(v.Cells) > 2 && v.Cells[1].Type == lisp.LString {
		return v.Cells[1].Str
	}
	return ""
}

func RenderPkgExported(w io.Writer, env *lisp.LEnv, query string) error {
	pkg := env.Runtime.Registry.Packages[query]
	if pkg == nil {
		return fmt.Errorf("no package: %q", query)
	}
	exports := pkg.Externals
	for i, exsym := range exports {
		if i > 0 {
			_, err := fmt.Fprintln(w)
			if err != nil {
				return err
			}
		}
		v := pkg.Get(lisp.Symbol(exsym))
		if v.Type != lisp.LFun {
			continue
		}
		err := RenderFun(w, exsym, v)
		if err != nil {
			return err
		}
	}
	return nil
}

func RenderVar(w io.Writer, env *lisp.LEnv, sym string) error {
	v := env.Get(lisp.Symbol(sym))
	err := lisp.GoError(v)
	if err != nil {
		return err
	}
	if v.Type != lisp.LFun {
		_, err := fmt.Fprintf(w, "%v %v\n", lisp.GetType(v).Str, v)
		return err
	}
	return RenderFun(w, sym, v)
}

func RenderFun(w io.Writer, sym string, v *lisp.LVal) error {
	_, err := fmt.Fprintf(w, "%s ", v.FunType)
	if err != nil {
		return fmt.Errorf("rendering function type: %w", err)
	}
	args := v.Cells[0]
	siglist := lisp.SExpr(make([]*lisp.LVal, 1+args.Len()))
	siglist.Cells[0] = lisp.Symbol(sym)
	copy(siglist.Cells[1:], args.Cells)
	_, err = fmt.Fprintln(w, siglist)
	if err != nil {
		return fmt.Errorf("rendering signature: %w", err)
	}
	doc := cleanDocstring(FunDocstring(v))
	if doc != "" {
		_, err = fmt.Fprintln(w, doc)
		return err
	}
	return nil
}

func cleanDocstring(doc string) string {
	if doc == "" {
		return ""
	}
	if doc[0] == '\n' {
		doc = doc[1:]
	}
	doc = indent.String(wordwrap.String(dedent.String(doc), 72), 2)
	if strings.HasSuffix(doc, "\n") {
		doc = doc[:len(doc)-1]
	}
	return doc
}
