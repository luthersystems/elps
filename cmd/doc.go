// Copyright © 2021 The ELPS authors

package cmd

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"os"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var docPackage bool
var docSourceFile string
var docListPackages bool

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

Common packages to explore:
  lisp      Core language (140+ builtins: map, filter, reduce, car, cdr, ...)
  math      Trigonometry, logarithms, constants (pi, inf)
  string    String operations (concat, split, join, ...)
  json      JSON serialization (json:encode, json:decode, json:null)`,
	Run: func(cmd *cobra.Command, args []string) {
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

func init() {
	rootCmd.AddCommand(docCmd)

	// Here flags for the doc command are defined
	docCmd.Flags().BoolVarP(&docPackage, "package", "p", false,
		"Interpret the argument as a package name.")
	docCmd.Flags().StringVarP(&docSourceFile, "source-file", "f", "",
		"Evaluate a lisp source file before querying documentation (presumably desired docs are in source code).")
	docCmd.Flags().BoolVarP(&docListPackages, "list-packages", "l", false,
		"List all packages loaded in the runtime with descriptions.")
}
