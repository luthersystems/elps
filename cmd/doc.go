// Copyright © 2021 The ELPS authors

package cmd

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/luthersystems/elps/docs"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var docPackage bool
var docSourceFile string
var docListPackages bool
var docMissing bool
var docGuide bool

// docCmd represents the doc command
var docCmd = &cobra.Command{
	Use:   "doc [flags] QUERY",
	Short: "Show elps documentation for functions, packages, and symbols",
	Long: `Show built-in documentation for ELPS functions, macros, operators,
constants, and packages.

By default, looks up a function or variable by name. Use -p to list all
exported symbols in a package. Use -f to load a source file first (useful
for documenting your own code).

Documentation is generated from doc strings embedded in function definitions
and package declarations. All built-in and standard library functions have
documentation.

Examples:
  elps doc map                     Show docs for the map function
  elps doc defun                   Show docs for the defun macro
  elps doc math:sin                Show docs for a qualified symbol
  elps doc -p math                 List all exports in the math package
  elps doc -p lisp                 List core language functions
  elps doc -f mylib.lisp my-func   Load a file, then show docs for my-func

Use -l to list all available packages in the runtime:
  elps doc -l

Use -m to find functions missing documentation (useful in CI):
  elps doc -m

Use --guide to print the full ELPS language reference:
  elps doc --guide

Common packages to explore:
  lisp      Core language (140+ builtins: map, filter, reduce, car, cdr, ...)
  math      Trigonometry, logarithms, constants (pi, inf)
  string    String operations (concat, split, join, ...)
  json      JSON serialization (json:encode, json:decode, json:null)`,
	Run: func(cmd *cobra.Command, args []string) {
		if docGuide {
			fmt.Print(docs.LangGuide)
			return
		}
		if docMissing {
			if err := docCheckMissing(); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
			return
		}
		if docListPackages {
			if err := docListPkgs(); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
			return
		}
		if len(args) != 1 {
			_ = cmd.Help()
			os.Exit(1)
		}
		err := docExec(args[0])
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			lerr := &lisp.ErrorVal{}
			if errors.As(err, &lerr) {
				_, _ = lerr.WriteTrace(os.Stderr)
			}
			os.Exit(1)
		}
	},
}

func docListPkgs() error {
	env := lisp.NewEnv(nil)
	reader := parser.NewReader()
	env.Runtime.Reader = reader
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	errbuf := &bytes.Buffer{}
	env.Runtime.Stderr = errbuf
	dumperr := func() { _, _ = os.Stderr.Write(errbuf.Bytes()) }
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		dumperr()
		return fmt.Errorf("initialize-user-env returned non-nil: %v", rc)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		dumperr()
		return fmt.Errorf("load-library returned non-nil: %v", rc)
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		dumperr()
		return fmt.Errorf("in-package returned non-nil: %v", rc)
	}
	if docSourceFile != "" {
		res := env.LoadFile(docSourceFile)
		if res.Type == lisp.LError {
			dumperr()
			_, _ = (*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
			os.Exit(1)
		}
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit
	return libhelp.RenderPackageList(out, env)
}

func docExec(query string) error {
	env := lisp.NewEnv(nil)
	reader := parser.NewReader()
	env.Runtime.Reader = reader
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	// environment output is typically discarded but a buffer is maintained in
	// case of an error during initialization (important when loading user
	// source files).
	errbuf := &bytes.Buffer{}
	env.Runtime.Stderr = errbuf
	dumperr := func() { _, _ = os.Stderr.Write(errbuf.Bytes()) }
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		dumperr()
		return fmt.Errorf("initialize-user-env returned non-nil: %v", rc)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		dumperr()
		return fmt.Errorf("load-library returned non-nil: %v", rc)
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		dumperr()
		return fmt.Errorf("in-package returned non-nil: %v", rc)
	}
	if docSourceFile != "" {
		res := env.LoadFile(docSourceFile)
		if res.Type == lisp.LError {
			dumperr()
			_, _ = (*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
			os.Exit(1)
		}
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit
	if docPackage {
		// NOTE:  There is no function to list/document unexported functions
		// because of the way use-package works, redefining symbols in the
		// local package.  Listing unexported functions produces a lot of
		// redundant symbols and is probably not what users would actually
		// want.
		return libhelp.RenderPkgExported(out, env, query)
	}
	return libhelp.RenderVar(out, env, query)
}

func docCheckMissing() error {
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit

	var missing []string

	// Check core builtins.
	for _, b := range lisp.DefaultBuiltins() {
		if builtinDocstring(b) == "" {
			missing = append(missing, fmt.Sprintf("  builtin     %s", b.Name()))
		}
	}

	// Check special operators.
	for _, op := range lisp.DefaultSpecialOps() {
		if builtinDocstring(op) == "" {
			missing = append(missing, fmt.Sprintf("  special-op  %s", op.Name()))
		}
	}

	// Check macros.
	for _, m := range lisp.DefaultMacros() {
		if builtinDocstring(m) == "" {
			missing = append(missing, fmt.Sprintf("  macro       %s", m.Name()))
		}
	}

	// Check library package exports.
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	env.Runtime.Stderr = &bytes.Buffer{}
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		return fmt.Errorf("initialize-user-env returned non-nil: %v", rc)
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		return fmt.Errorf("load-library returned non-nil: %v", rc)
	}

	// Check package-level documentation.
	allPkgNames := make([]string, 0, len(env.Runtime.Registry.Packages))
	for name := range env.Runtime.Registry.Packages {
		if name == "user" {
			continue // user package is the default workspace, no doc needed.
		}
		allPkgNames = append(allPkgNames, name)
	}
	sort.Strings(allPkgNames)

	for _, pkgName := range allPkgNames {
		pkg := env.Runtime.Registry.Packages[pkgName]
		if strings.TrimSpace(pkg.Doc) == "" {
			missing = append(missing, fmt.Sprintf("  package     %s", pkgName))
		}
	}

	// Check library package exports (skip "lisp" — already covered above
	// via DefaultBuiltins/DefaultSpecialOps/DefaultMacros).
	pkgNames := make([]string, 0, len(env.Runtime.Registry.Packages))
	for name := range env.Runtime.Registry.Packages {
		if name == "lisp" || name == "user" {
			continue
		}
		pkgNames = append(pkgNames, name)
	}
	sort.Strings(pkgNames)

	for _, pkgName := range pkgNames {
		pkg := env.Runtime.Registry.Packages[pkgName]
		for _, sym := range pkg.Externals {
			v := pkg.Get(lisp.Symbol(sym))
			if v.Type == lisp.LFun && v.Docstring() == "" {
				missing = append(missing, fmt.Sprintf("  %-10s  %s:%s", v.FunType, pkgName, sym))
			}
			if v.Type != lisp.LFun && v.Type != lisp.LError {
				if pkg.SymbolDocs[sym] == "" {
					missing = append(missing, fmt.Sprintf("  %-10s  %s:%s", lisp.GetType(v).Str, pkgName, sym))
				}
			}
		}
	}

	if len(missing) == 0 {
		_, _ = fmt.Fprintln(out, "All functions and exports are documented.")
		return nil
	}

	_, _ = fmt.Fprintf(out, "Found %d symbols with missing documentation:\n\n", len(missing))
	for _, m := range missing {
		_, _ = fmt.Fprintln(out, m)
	}
	_, _ = fmt.Fprintln(out)
	return fmt.Errorf("%d symbols with missing documentation", len(missing))
}

// builtinDocstring extracts the docstring from an LBuiltinDef, returning ""
// if the definition does not implement the builtinDocumented interface.
func builtinDocstring(defn lisp.LBuiltinDef) string {
	type documented interface {
		Docstring() string
	}
	if doc, ok := defn.(documented); ok {
		return doc.Docstring()
	}
	return ""
}

func init() {
	rootCmd.AddCommand(docCmd)

	// Here flags for the doc command are defined
	docCmd.Flags().BoolVarP(&docPackage, "package", "p", false,
		"Interpret the argument as a package name.")
	docCmd.Flags().StringVarP(&docSourceFile, "source-file", "f", "",
		"Evaluate a lisp source file before querying documentation (presumably desired docs are in source code).")
	docCmd.Flags().BoolVarP(&docListPackages, "list-packages", "l", false,
		"List all packages loaded in the runtime with descriptions.")
	docCmd.Flags().BoolVarP(&docMissing, "missing", "m", false,
		"List all builtins, special ops, macros, and library functions with missing docstrings. Exits with code 1 if any are found.")
	docCmd.Flags().BoolVar(&docGuide, "guide", false,
		"Print the full ELPS language reference guide.")
}
