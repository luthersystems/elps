// Copyright © 2018 The ELPS authors

package cmd

import (
	"fmt"
	"os"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var (
	runExpression bool
	runPrint      bool
)

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run [flags] [files...]",
	Short: "Run elps source files or expressions",
	Long: `Run ELPS Lisp code from files or command-line expressions.

With file arguments, each file is loaded and executed in order. With -e,
arguments are interpreted as Lisp expressions and evaluated directly.

The runtime loads all standard library packages automatically. User code
starts in the "user" package and can import other packages with use-package.

Examples:
  elps run hello.lisp              Run a source file
  elps run lib.lisp app.lisp       Load files in order (lib first)
  elps run -e '(+ 1 2)'            Evaluate an expression
  elps run -e -p '(* 6 7)'         Evaluate and print the result

Exit codes:
  0  Success
  1  Runtime error (use elps lint to catch common mistakes before running)`,
	Run: func(cmd *cobra.Command, args []string) {
		env := lisp.NewEnv(nil)
		reader := parser.NewReader()
		env.Runtime.Reader = reader
		env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
		rc := lisp.InitializeUserEnv(env)
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		rc = lisplib.LoadLibrary(env)
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		for i := range args {
			res := env.LoadFile(args[i])
			if res.Type == lisp.LError {
				renderLispError(res, args[i])
				os.Exit(1)
			}
		}
	},
}

func init() {
	rootCmd.AddCommand(runCmd)

	// Here flags for the run command are defined
	runCmd.Flags().BoolVarP(&runExpression, "expression", "e", false,
		"Interpret arguments as lisp expressions")
	runCmd.Flags().BoolVarP(&runPrint, "print", "p", false,
		"Print expression values to stdout")
}
