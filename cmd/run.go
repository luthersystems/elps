// Copyright © 2018 The ELPS authors

package cmd

import (
	"fmt"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"io/ioutil"
	"os"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var (
	runExpression   bool
	runPrint        bool
	runDebugger     bool
	runDebuggerPort int
	runDebuggerType string
)

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Run lisp code",
	Long:  `Run lisp code provided supplied via the command line or a file.`,
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) == 0 {
			fmt.Fprintln(os.Stderr, "No file specified")
			os.Exit(1)
		}

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

		var profiler lisp.Profiler
		if runDebugger {
			var mode debugger.DebugMode
			switch debugger.DebugMode(runDebuggerType) {
			case debugger.DebugModeDelve:
				mode = debugger.DebugModeDelve
			case debugger.DebugModeDAP:
				mode = debugger.DebugModeDAP
			default:
				fmt.Fprintln(os.Stderr, "Invalid debugger. Specify delve or dap")
				os.Exit(1)
			}
			profiler = debugger.NewDebugger(env.Runtime, fmt.Sprintf(":%d", runDebuggerPort), mode)
		}
		for i := range args {
			res := env.LoadFile(args[i])
			if res.Type == lisp.LError {
				(*lisp.ErrorVal)(res).WriteTrace(os.Stderr)
				os.Exit(1)
			}
		}
		if runDebugger {
			profiler.Complete()
		}
	},
}

func runReadExpressions(args []string) ([][]byte, error) {
	exprs := make([][]byte, len(args))
	if runExpression {
		for i := range args {
			exprs[i] = []byte(args[i])
		}
		return exprs, nil
	}
	for i, path := range args {
		b, err := ioutil.ReadFile(path)
		if err != nil {
			return nil, err
		}
		exprs[i] = b
	}
	return exprs, nil
}

func init() {
	rootCmd.AddCommand(runCmd)

	// Here flags for the run command are defined
	runCmd.Flags().BoolVarP(&runExpression, "expression", "e", false,
		"Interpret arguments as lisp expressions")
	runCmd.Flags().BoolVarP(&runPrint, "print", "p", false,
		"Print expression values to stdout")
	runCmd.Flags().BoolVarP(&runDebugger, "debugger", "d", false,
		"Enable the debugger")
	runCmd.Flags().IntVarP(&runDebuggerPort, "debugger.listen", "l", 8883,
		"Port for the debugger")
	runCmd.Flags().StringVarP(&runDebuggerType, "debugger.type", "t", "dap",
		"Type of debugger (delve or dap)")
}
