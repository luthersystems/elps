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

// DocCommand creates the "doc" cobra command with optional embedder
// configuration. Embedders can pass WithEnv to supply a fully configured
// environment (preferred) or WithRegistry to merge Go-registered packages
// into the default doc environment.
func DocCommand(opts ...Option) *cobra.Command {
	var cfg cmdConfig
	for _, o := range opts {
		o(&cfg)
	}

	var (
		pkgFlag      bool
		sourceFile   string
		listPackages bool
		missing      bool
		guide        bool
		debugGuide   bool
		lspGuide     bool
	)

	// makeEnv returns an LEnv suitable for documentation queries. When
	// the embedder provided a full env via WithEnv it is returned as-is.
	// Otherwise a standard doc env is created and any embedder registry
	// packages are merged in.
	makeEnv := func() (*lisp.LEnv, error) {
		if cfg.env != nil {
			return cfg.env, nil
		}
		env, err := lisplib.NewDocEnv()
		if err != nil {
			return nil, err
		}
		// Merge embedder registry packages that don't already exist.
		if cfg.registry != nil {
			for name, pkg := range cfg.registry.Packages {
				if _, exists := env.Runtime.Registry.Packages[name]; !exists {
					env.Runtime.Registry.Packages[name] = pkg
				}
			}
		}
		return env, nil
	}

	cmd := &cobra.Command{
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

Use --debug-guide to print the debugging guide:
  elps doc --debug-guide

Use --lsp-guide to print the Language Server Protocol guide:
  elps doc --lsp-guide

Common packages to explore:
  lisp      Core language (140+ builtins: map, filter, reduce, car, cdr, ...)
  math      Trigonometry, logarithms, constants (pi, inf)
  string    String operations (concat, split, join, ...)
  json      JSON serialization (json:encode, json:decode, json:null)`,
		Run: func(cmd *cobra.Command, args []string) {
			if guide {
				fmt.Print(docs.LangGuide)
				return
			}
			if debugGuide {
				fmt.Print(docs.DebuggingGuide)
				return
			}
			if lspGuide {
				fmt.Print(docs.LSPGuide)
				return
			}
			if missing {
				if err := docCheckMissing(makeEnv); err != nil {
					fmt.Fprintln(os.Stderr, err)
					os.Exit(1)
				}
				return
			}
			if listPackages {
				if err := docListPkgs(makeEnv, sourceFile); err != nil {
					fmt.Fprintln(os.Stderr, err)
					os.Exit(1)
				}
				return
			}
			if len(args) != 1 {
				_ = cmd.Help()
				os.Exit(1)
			}
			err := docExec(makeEnv, sourceFile, pkgFlag, args[0])
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

	cmd.Flags().BoolVarP(&pkgFlag, "package", "p", false,
		"Interpret the argument as a package name.")
	cmd.Flags().StringVarP(&sourceFile, "source-file", "f", "",
		"Evaluate a lisp source file before querying documentation (presumably desired docs are in source code).")
	cmd.Flags().BoolVarP(&listPackages, "list-packages", "l", false,
		"List all packages loaded in the runtime with descriptions.")
	cmd.Flags().BoolVarP(&missing, "missing", "m", false,
		"List all builtins, special ops, macros, and library functions with missing docstrings. Exits with code 1 if any are found.")
	cmd.Flags().BoolVar(&guide, "guide", false,
		"Print the full ELPS language reference guide.")
	cmd.Flags().BoolVar(&debugGuide, "debug-guide", false,
		"Print the debugging guide.")
	cmd.Flags().BoolVar(&lspGuide, "lsp-guide", false,
		"Print the Language Server Protocol guide.")

	return cmd
}

func docListPkgs(makeEnv func() (*lisp.LEnv, error), sourceFile string) error {
	env, err := makeEnv()
	if err != nil {
		return err
	}
	if sourceFile != "" {
		res := env.LoadFile(sourceFile)
		if res.Type == lisp.LError {
			_, _ = (*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
			os.Exit(1)
		}
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit
	return libhelp.RenderPackageList(out, env)
}

func docExec(makeEnv func() (*lisp.LEnv, error), sourceFile string, pkgFlag bool, query string) error {
	env, err := makeEnv()
	if err != nil {
		return err
	}
	if sourceFile != "" {
		res := env.LoadFile(sourceFile)
		if res.Type == lisp.LError {
			_, _ = (*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
			os.Exit(1)
		}
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush() //nolint:errcheck // best-effort flush on exit
	if pkgFlag {
		return libhelp.RenderPkgExported(out, env, query)
	}
	return libhelp.RenderVar(out, env, query)
}

func docCheckMissing(makeEnv func() (*lisp.LEnv, error)) error {
	env, err := makeEnv()
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
	rootCmd.AddCommand(DocCommand())
}
