// Copyright © 2021 The ELPS authors

package cmd

import (
	"fmt"
	"os"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/tools/lint"
	"github.com/spf13/cobra"
)

// lintCmd represents the repl command
var lintCmd = &cobra.Command{
	Use:   "lint [PATH ...]",
	Short: "lint checks elps code for style problems",
	Long: `
Lint is a reference implementation for a lisp style checker.  It checks a
minimal set of stylistic recommendations that cannot be enfonced at the
language level.
`,
	Run: func(cmd *cobra.Command, args []string) {
		ok := lintPaths(args)
		if ok {
			os.Exit(0)
		}
		os.Exit(1)
	},
}

func lintPaths(paths []string) (ok bool) {
	env := lisp.NewEnv(nil)
	reader := parser.NewReader()
	env.Runtime.Reader = reader
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	if !rc.IsNil() {
		printerr(rc)
		return false
	}
	rc = lisplib.LoadLibrary(env)
	if !rc.IsNil() {
		printerr(rc)
		return false
	}
	rc = lint.Load(env)
	if !rc.IsNil() {
		printerr(rc)
		return false
	}
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if !rc.IsNil() {
		printerr(rc)
		return false
	}
	rc = env.UsePackage(lisp.Symbol("lint"))
	if !rc.IsNil() {
		printerr(rc)
		return false
	}

	ok = true
	for _, p := range paths {
		src, err := os.ReadFile(p)
		if err != nil {
			printerr(err)
			return false
		}

		lints := env.FunCall(env.GetFun(lisp.Symbol("lint-source")), lisp.QExpr([]*lisp.LVal{
			lisp.String(p),
			lisp.Bytes(src),
		}))
		err = lisp.GoError(lints)
		if err != nil {
			printerr(err)
			return false
		}
		if lints.Type != lisp.LArray || lints.ArrayDims().Len() != 1 {
			printerr("unexpected lint result", lints)
			return false
		}
		cells := lints.Cells[1].Cells
		for _, lnt := range cells {
			repr := env.FunCall(env.GetFun(lisp.Symbol("format-lint")), lisp.QExpr([]*lisp.LVal{lnt}))
			err := lisp.GoError(lints)
			if err != nil {
				printerr(err)
				return false
			}
			if repr.Type != lisp.LString {
				printerr("unexpected lint representation", repr)
				return false
			}
			// I guess the lints themselves should go to stdout and not stderr
			fmt.Println(repr.Str)
		}
		ok = ok && len(cells) == 0
	}
	return ok
}

func printerr(v ...interface{}) {
	fmt.Fprintln(os.Stderr, v...)
}

func init() {
	rootCmd.AddCommand(lintCmd)

	// Here you will define your flags and configuration settings.
}
