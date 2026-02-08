// Copyright © 2018 The ELPS authors

package cmd

import (
	"os"
	"path/filepath"

	"github.com/luthersystems/elps/repl"
	"github.com/spf13/cobra"
)

// replCmd represents the repl command
var replCmd = &cobra.Command{
	Use:   "repl",
	Short: "Start an interactive ELPS Lisp REPL",
	Long: `Start an interactive read-eval-print loop for ELPS Lisp.

All standard library packages are loaded automatically. Line editing and
in-session command history are supported via readline. Use Ctrl-D or
Ctrl-C to exit.

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
  ...`,
	Run: func(cmd *cobra.Command, args []string) {
		repl.RunRepl(filepath.Base(os.Args[0]) + "> ")
	},
}

func init() {
	rootCmd.AddCommand(replCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// replCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// replCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}
