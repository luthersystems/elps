// Copyright © 2018 The ELPS authors

package cmd

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

var (
	cfgFile   string
	colorFlag string
)

// rootCmd represents the base command when called without any subcommands
var rootCmd = &cobra.Command{
	Use:   "elps",
	Short: "ELPS — Embedded Lisp interpreter",
	Long: `ELPS is an embedded Lisp interpreter implemented in Go. It provides a
standalone CLI for running, linting, formatting, and exploring ELPS Lisp code.

Getting started:
  elps run file.lisp           Run a Lisp source file
  elps run -e '(+ 1 2)'       Evaluate an expression
  elps repl                    Start an interactive REPL
  elps doc map                 Show documentation for a function
  elps doc -p math             List all exports in a package
  elps lint file.lisp          Run static analysis checks
  elps fmt file.lisp           Format source code

Language overview:
  ELPS is a Lisp-1 dialect (single namespace for functions and values).
  Booleans are the symbols true and false. The empty list () is nil/falsey.
  Functions are defined with (defun name (args) body) and called as (name args).
  Packages provide namespacing: (in-package 'my-pkg), (use-package 'math).
  Error handling uses (handler-bind ...) and (ignore-errors ...).

Standard library packages (use with use-package or qualified names):
  math      Mathematical functions, constants (pi, inf)
  string    String manipulation (concat, split, join, upper, lower, ...)
  json      JSON encoding/decoding
  regexp    Regular expression matching
  time      Date/time operations
  base64    Base64 encoding/decoding
  testing   Test framework (assert-equal, assert-nil, test, test-let)
  help      Documentation introspection (doc, doc-string, apropos)
  golang    Go interop utilities
  s         Schema type predicates

Documentation is built in: use (help 'symbol) in the REPL or elps doc <name>
from the command line. Use elps doc -p <package> to explore a package.

More information:
  Source code:     https://github.com/luthersystems/elps
  Documentation:   https://github.com/luthersystems/docs`,
}

// Execute adds all child commands to the root command and sets flags appropriately.
// This is called by main.main(). It only needs to happen once to the rootCmd.
func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func init() {
	cobra.OnInitialize(initConfig)

	// Here you will define your flags and configuration settings.
	// Cobra supports persistent flags, which, if defined here,
	// will be global for your application.
	rootCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file (default is $HOME/.elps.yaml)")
	rootCmd.PersistentFlags().StringVar(&colorFlag, "color", "auto",
		`Control colored output: "auto", "always", or "never".`)

	// Cobra also supports local flags, which will only run
	// when this action is called directly.
	rootCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}

// initConfig reads in config file and ENV variables if set.
func initConfig() {
	if cfgFile != "" {
		// Use config file from the flag.
		viper.SetConfigFile(cfgFile)
	} else {
		// Find home directory.
		home, err := os.UserHomeDir()
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		// Search config in home directory with name ".elps" (without extension).
		viper.AddConfigPath(home)
		viper.SetConfigName(".elps")
	}

	viper.AutomaticEnv() // read in environment variables that match

	// If a config file is found, read it in.
	if err := viper.ReadInConfig(); err == nil {
		fmt.Println("Using config file:", viper.ConfigFileUsed())
	}
}
