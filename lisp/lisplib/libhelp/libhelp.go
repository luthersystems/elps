// Copyright © 2021 The ELPS authors

package libhelp

import (
	"fmt"
	"io"
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
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
	env.SetPackageDoc("Interactive documentation: inspect functions, variables, and package exports.")
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
		for _, sym := range sortedSymbols(pkg.Symbols) {
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

// NOTE:  A good symbol sorting function may want to specially handle
// package-namespaced symbols (containing ":").  This function is sorting a
// symbols within a single package so it uses a symbol lexical sort function.
func sortedSymbols(smap map[string]*lisp.LVal) []string {
	symbols := make([]string, 0, len(smap))
	for s := range smap {
		symbols = append(symbols, s)
	}
	sort.Strings(symbols)
	return symbols
}

// RenderPkgExported writes to w formatted documentation for exported symbols
// in the query package within env.  The exact formatting of the rendered
// documentation is subject to change across elps versions.
func RenderPkgExported(w io.Writer, env *lisp.LEnv, query string) error {
	pkg := env.Runtime.Registry.Packages[query]
	if pkg == nil {
		return fmt.Errorf("no package: %q", query)
	}
	_, err := fmt.Fprintf(w, "package %s\n", pkg.Name)
	if err != nil {
		return err
	}
	if pkg.Doc != "" {
		doc := cleanDocstring(pkg.Doc)
		_, err = fmt.Fprintln(w, doc)
		if err != nil {
			return err
		}
	}
	_, err = fmt.Fprintln(w)
	if err != nil {
		return err
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
		switch v.Type {
		case lisp.LError:
			fmt.Fprintln(w, v) //nolint:errcheck // best-effort error display
		case lisp.LFun:
			err := renderFun(w, exsym, v)
			if err != nil {
				return fmt.Errorf("function %s: %w", exsym, err)
			}
		default:
			err := renderVal(w, exsym, v, pkg.SymbolDocs[exsym])
			if err != nil {
				return fmt.Errorf("variable %s: %w", exsym, err)
			}
		}
	}
	return nil
}

// RenderVar writes to w formatted documentation for the object referenced by
// sym in the context of env.  The exact formatting of the rendered
// documentation is subject to change across elps versions.
func RenderVar(w io.Writer, env *lisp.LEnv, sym string) error {
	v := env.Get(lisp.Symbol(sym))
	err := lisp.GoError(v)
	if err != nil {
		return err
	}
	if v.Type != lisp.LFun {
		return renderVal(w, sym, v, lookupSymbolDoc(env, sym))
	}
	return renderFun(w, sym, v)
}

// lookupSymbolDoc resolves a symbol's documentation from its package.
// Handles qualified names (pkg:sym) and unqualified names (current package).
func lookupSymbolDoc(env *lisp.LEnv, sym string) string {
	if i := strings.Index(sym, ":"); i >= 0 {
		pkgName := sym[:i]
		symName := sym[i+1:]
		if pkg := env.Runtime.Registry.Packages[pkgName]; pkg != nil {
			return pkg.SymbolDocs[symName]
		}
		return ""
	}
	return env.Runtime.Package.SymbolDocs[sym]
}

func renderVal(w io.Writer, sym string, v *lisp.LVal, doc string) error {
	_, err := fmt.Fprintf(w, "%v %s %v\n", lisp.GetType(v).Str, sym, v)
	if err != nil {
		return err
	}
	if doc != "" {
		cleaned := cleanDocstring(doc)
		_, err = fmt.Fprintln(w, cleaned)
	}
	return err
}

func renderFun(w io.Writer, sym string, v *lisp.LVal) error {
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
	doc := cleanDocstring(v.Docstring())
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
	doc = indent.String(wordwrap.String(dedentDoc(doc), 72), 2)
	doc = strings.TrimSuffix(doc, "\n")
	return doc
}

// dedentDoc removes common leading whitespace from all non-empty lines.
// It handles Go raw string literals where the first line may have less
// indentation than continuation lines (which inherit the source code's
// tab indentation). Tabs are normalized to spaces before processing.
func dedentDoc(s string) string {
	s = strings.ReplaceAll(s, "\t", "    ")
	lines := strings.Split(s, "\n")

	// Find minimum leading spaces across non-empty lines, skipping
	// the first line (which in raw strings often has no indentation).
	minWS := -1
	start := 0
	if len(lines) > 1 {
		start = 1
	}
	for _, line := range lines[start:] {
		trimmed := strings.TrimLeft(line, " ")
		if trimmed == "" {
			continue
		}
		ws := len(line) - len(trimmed)
		if minWS < 0 || ws < minWS {
			minWS = ws
		}
	}
	if minWS <= 0 {
		return strings.TrimLeft(lines[0], " ") + "\n" + strings.Join(lines[1:], "\n")
	}

	lines[0] = strings.TrimLeft(lines[0], " ")
	for i := 1; i < len(lines); i++ {
		if strings.TrimSpace(lines[i]) == "" {
			lines[i] = ""
		} else if len(lines[i]) >= minWS {
			lines[i] = lines[i][minWS:]
		}
	}
	return strings.Join(lines, "\n")
}
