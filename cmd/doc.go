// Copyright © 2021 The ELPS authors

package cmd

import (
	"bufio"
	"errors"
	"fmt"
	"os"

	"github.com/luthersystems/elps/docs"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
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
	env, err := lisplib.NewDocEnv()
	if err != nil {
		return err
	}
	if docSourceFile != "" {
		res := env.LoadFile(docSourceFile)
		if res.Type == lisp.LError {
			_, _ = (*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
			os.Exit(1)
		}
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit
	return libhelp.RenderPackageList(out, env)
}

func docExec(query string) error {
	env, err := lisplib.NewDocEnv()
	if err != nil {
		return err
	}
	if docSourceFile != "" {
		res := env.LoadFile(docSourceFile)
		if res.Type == lisp.LError {
			_, _ = (*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
			os.Exit(1)
		}
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit
	if docPackage {
		return libhelp.RenderPkgExported(out, env, query)
	}
	return libhelp.RenderVar(out, env, query)
}

func docCheckMissing() error {
	env, err := lisplib.NewDocEnv()
	if err != nil {
		return err
	}

	missing := libhelp.CheckMissing(env)
	if len(missing) == 0 {
		out := bufio.NewWriter(os.Stdout)
		defer out.Flush() //nolint:errcheck // best-effort flush on exit
		_, _ = fmt.Fprintln(out, "All functions and exports are documented.")
		return nil
	}

	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit
	_, _ = fmt.Fprintf(out, "Found %d symbols with missing documentation:\n\n", len(missing))
	for _, m := range missing {
		_, _ = fmt.Fprintf(out, "  %-10s  %s\n", m.Kind, m.Name)
	}
	_, _ = fmt.Fprintln(out)
	return fmt.Errorf("%d symbols with missing documentation", len(missing))
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
