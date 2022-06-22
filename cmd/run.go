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
	Use:   "run",
	Short: "Run lisp code",
	Long:  `Run lisp code provided supplied via the command line or a file.`,
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
				_, _ = (*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
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
