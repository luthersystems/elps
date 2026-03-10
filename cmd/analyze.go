// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"os"

	"github.com/luthersystems/elps/analysis/perf"
	"github.com/spf13/cobra"
)

// AnalyzeCommand creates the "analyze" cobra command with optional embedder
// configuration. Embedders can append expensive function patterns via options.
func AnalyzeCommand(opts ...Option) *cobra.Command {
	var cfg cmdConfig
	for _, o := range opts {
		o(&cfg)
	}

	var (
		jsonOutput  bool
		configFile  string
		excludes    []string
	)

	cmd := &cobra.Command{
		Use:   "analyze [flags] [files...]",
		Short: "Run performance analysis on elps source files",
		Long: `Run call-graph-based performance analysis on ELPS source files.

The analyzer detects hot paths, scaling risks, expensive-in-loop patterns,
and recursive cycles by building a cross-function call graph and propagating
costs.

Rules:
  PERF001   Hot path — total propagated score exceeds threshold
  PERF002   Scaling risk — O(N^k) complexity at or above threshold
  PERF003   Expensive call in loop — known-expensive function inside a loop form
  PERF004   Recursive cycle — mutual or self-recursion detected
  UNKNOWN001  Dynamic dispatch — callee cannot be statically resolved (info)

Configuration is read from .elps-analyze.yaml (searched from the working
directory upward). Override with --config.

To suppress analysis for a specific function, add a leading comment:
  ;; elps-analyze-disable
  (defun my-function () ...)

Exit codes:
  0  No issues found (or only info-level diagnostics)
  1  One or more warning/error-level issues found
  2  Bad invocation (invalid flags, unreadable files)

Examples:
  elps analyze file.lisp              # Analyze a single file
  elps analyze ./...                  # Analyze all .lisp files recursively
  elps analyze --json ./...           # Output as JSON
  elps analyze --config=my.yaml ./... # Use custom config`,
		Run: func(_ *cobra.Command, args []string) {
			if len(args) == 0 {
				fmt.Fprintln(os.Stderr, "elps analyze: no files specified")
				os.Exit(2)
			}

			// Load config
			var analyzeConfig *perf.Config
			if configFile != "" {
				var err error
				analyzeConfig, err = perf.LoadConfigFile(configFile)
				if err != nil {
					fmt.Fprintf(os.Stderr, "elps analyze: loading config: %v\n", err)
					os.Exit(2)
				}
			} else {
				// Search for config file
				wd, _ := os.Getwd()
				found := perf.FindConfigFile(wd)
				if found != "" {
					var err error
					analyzeConfig, err = perf.LoadConfigFile(found)
					if err != nil {
						fmt.Fprintf(os.Stderr, "elps analyze: loading config %s: %v\n", found, err)
						os.Exit(2)
					}
				} else {
					analyzeConfig = perf.DefaultConfig()
				}
			}

			expanded, err := expandArgs(args, excludes)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(2)
			}

			if len(expanded) == 0 {
				fmt.Fprintln(os.Stderr, "elps analyze: no .lisp files found")
				os.Exit(2)
			}

			result, err := perf.AnalyzeFiles(expanded, analyzeConfig)
			if err != nil {
				fmt.Fprintf(os.Stderr, "elps analyze: %v\n", err)
				os.Exit(2)
			}

			if len(result.Issues) == 0 {
				return
			}

			if jsonOutput {
				if err := perf.FormatJSON(os.Stdout, result.Issues); err != nil {
					fmt.Fprintln(os.Stderr, err)
					os.Exit(2)
				}
			} else {
				perf.FormatText(os.Stderr, result.Issues)
			}

			// Exit 1 if any warning or error level issues
			for _, issue := range result.Issues {
				if issue.Severity <= perf.SeverityWarning {
					os.Exit(1)
				}
			}
		},
	}

	cmd.Flags().BoolVar(&jsonOutput, "json", false,
		"Output issues as JSON.")
	cmd.Flags().StringVar(&configFile, "config", "",
		"Path to .elps-analyze.yaml config file.")
	cmd.Flags().StringArrayVar(&excludes, "exclude", nil,
		"Glob pattern for files to exclude (may be repeated).")

	return cmd
}

func init() {
	rootCmd.AddCommand(AnalyzeCommand())
}
