// Copyright © 2018 The ELPS authors

package cmd

import (
	"os"
	"path/filepath"

	"github.com/luthersystems/elps/repl"
	"github.com/spf13/cobra"
)

var (
	replJSON  bool
	replBatch bool
	replEval  string
)

// replCmd represents the repl command
var replCmd = &cobra.Command{
	Use:   "repl",
	Short: "Start an interactive ELPS Lisp REPL",
	Long: `Start an interactive read-eval-print loop for ELPS Lisp.

All standard library packages are loaded automatically. Line editing and
in-session command history are supported via readline. Use Ctrl-D or
Ctrl-C to exit.

Flags:
  --json          Output each result as a single-line JSON object to stdout.
  --batch         Suppress readline and read raw lines from stdin (auto-enables --json).
  -e, --eval EXPR Evaluate a single expression, print the result, and exit.

Example REPL session:
  elps> (+ 1 2)
  3
  elps> (defun square (x) (* x x))
  ()
  elps> (square 5)
  25
  elps> (use-package 'math)
  ()
  elps> math:pi
  3.141592653589793
  elps> (help 'map)
  ...
  elps> (apropos "sort")
  ...

Example batch/JSON usage:
  echo '(+ 1 2)' | elps repl --batch
  elps repl -e '(* 6 7)'
  elps repl -e '(* 6 7)' --json`,
	Run: func(cmd *cobra.Command, args []string) {
		jsonFlag := replJSON
		// --batch auto-enables --json unless --json was explicitly set.
		if replBatch && !cmd.Flags().Changed("json") {
			jsonFlag = true
		}

		opts := []repl.Option{
			repl.WithJSON(jsonFlag),
			repl.WithBatch(replBatch),
		}
		if replEval != "" {
			opts = append(opts, repl.WithEval(replEval))
		}
		repl.RunRepl(filepath.Base(os.Args[0])+"> ", opts...)
	},
}

func init() {
	rootCmd.AddCommand(replCmd)

	replCmd.Flags().BoolVar(&replJSON, "json", false,
		"Output each result as a single-line JSON object to stdout")
	replCmd.Flags().BoolVar(&replBatch, "batch", false,
		"Suppress readline prompt; read raw lines from stdin (auto-enables --json)")
	replCmd.Flags().BoolVar(&replBatch, "no-prompt", false,
		"Alias for --batch")
	replCmd.Flags().StringVarP(&replEval, "eval", "e", "",
		"Evaluate a single expression, print result, and exit")
}
