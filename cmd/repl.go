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
	Short: "Run a lisp repl",
	Long:  `Run a lisp repl.`,
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
