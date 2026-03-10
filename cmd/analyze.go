// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis/perf"
	"github.com/spf13/cobra"
)

// AnalyzeOption configures the analyze command for embedder integration.
type AnalyzeOption func(*perf.Config)

// WithExpensiveFunctions appends expensive function patterns to the config.
// Embedders use this to inject domain-specific patterns (e.g., "statedb:*").
func WithExpensiveFunctions(patterns ...string) AnalyzeOption {
	return func(cfg *perf.Config) {
		cfg.ExpensiveFunctions = append(cfg.ExpensiveFunctions, patterns...)
	}
}

// WithLoopKeywords appends loop keyword symbols to the config.
// Embedders use this for domain-specific iteration forms.
func WithLoopKeywords(keywords ...string) AnalyzeOption {
	return func(cfg *perf.Config) {
		cfg.LoopKeywords = append(cfg.LoopKeywords, keywords...)
	}
}

// WithFunctionCosts sets per-function cost overrides.
func WithFunctionCosts(costs map[string]int) AnalyzeOption {
	return func(cfg *perf.Config) {
		if cfg.FunctionCosts == nil {
			cfg.FunctionCosts = make(map[string]int)
		}
		for k, v := range costs {
			cfg.FunctionCosts[k] = v
		}
	}
}

// WithSuppressionPrefix sets the suppression comment prefix.
func WithSuppressionPrefix(prefix string) AnalyzeOption {
	return func(cfg *perf.Config) {
		cfg.SuppressionPrefix = prefix
	}
}

// AnalyzeCommand creates the "analyze" cobra command with optional embedder
// configuration. Embedders can inject domain-specific expensive functions,
// loop keywords, and cost overrides via AnalyzeOption functions.
func AnalyzeCommand(analyzeOpts ...AnalyzeOption) *cobra.Command {
	var (
		jsonOutput bool
		configFile string
		excludes   []string
		rules      string
		failOn     string
		topN       int
	)

	cmd := &cobra.Command{
		Use:   "analyze [flags] [files...]",
		Short: "Run performance analysis on elps source files",
		Long: `Run call-graph-based performance analysis on ELPS source files.

The analyzer detects hot paths, scaling risks, expensive-in-loop patterns,
and recursive cycles by building a cross-function call graph and propagating
costs.

Rules:
  PERF001     Hot path — total propagated score exceeds threshold
  PERF002     Scaling risk — O(N^k) complexity at or above threshold
  PERF003     Expensive call in loop — known-expensive function inside a loop form
  PERF004     Recursive cycle — mutual or self-recursion detected
  UNKNOWN001  Dynamic dispatch — callee cannot be statically resolved (info)

Configuration is read from .elps-analyze.yaml (searched from the working
directory upward). Override with --config.

To suppress analysis for a specific function, add a leading comment:
  ;; elps-analyze-disable
  (defun my-function () ...)

Exit codes:
  0  No issues found, or only diagnostics below the --fail-on threshold
  1  One or more diagnostics at or above the --fail-on threshold
  2  Bad invocation (invalid flags, unreadable files)

Examples:
  elps analyze file.lisp                       # Analyze a single file
  elps analyze ./...                           # Analyze all .lisp files recursively
  elps analyze --json ./...                    # Output as JSON
  elps analyze --rules=PERF003,PERF004 ./...   # Run only specific rules
  elps analyze --fail-on=error ./...           # Only fail on errors, not warnings
  elps analyze --top=5 ./...                   # Show top 5 hottest functions
  elps analyze --config=my.yaml ./...          # Use custom config`,
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

			// Apply embedder options
			for _, opt := range analyzeOpts {
				opt(analyzeConfig)
			}

			// Apply --rules flag
			if rules != "" {
				var ruleList []string
				for _, r := range strings.Split(rules, ",") {
					r = strings.TrimSpace(r)
					if r != "" {
						ruleList = append(ruleList, r)
					}
				}
				analyzeConfig.Rules = ruleList
			}

			// Parse --fail-on threshold
			threshold := perf.SeverityWarning // default: fail on warnings
			if failOn != "" {
				switch failOn {
				case "error":
					threshold = perf.SeverityError
				case "warning":
					threshold = perf.SeverityWarning
				case "info":
					threshold = perf.SeverityInfo
				default:
					fmt.Fprintf(os.Stderr, "elps analyze: invalid --fail-on value: %q (valid: error, warning, info)\n", failOn)
					os.Exit(2)
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

			// Show top N hottest functions if requested
			if topN > 0 && result.Solved != nil {
				showTopFunctions(result.Solved, topN)
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

			// Exit 1 if any issue meets the severity threshold
			for _, issue := range result.Issues {
				if issue.Severity <= threshold {
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
	cmd.Flags().StringVar(&rules, "rules", "",
		"Comma-separated list of rules to run (default: all). Valid: PERF001,PERF002,PERF003,PERF004,UNKNOWN001.")
	cmd.Flags().StringVar(&failOn, "fail-on", "warning",
		"Minimum severity to cause a non-zero exit code (error, warning, info).")
	cmd.Flags().IntVar(&topN, "top", 0,
		"Show the top N hottest functions by total score.")

	return cmd
}

func showTopFunctions(solved []*perf.SolvedFunction, n int) {
	// Sort by total score descending
	sorted := make([]*perf.SolvedFunction, len(solved))
	copy(sorted, solved)
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].TotalScore > sorted[j].TotalScore
	})
	if n > len(sorted) {
		n = len(sorted)
	}
	fmt.Fprintf(os.Stderr, "Top %d functions by score:\n", n) //nolint:errcheck
	for i := 0; i < n; i++ {
		sf := sorted[i]
		loc := "unknown"
		if sf.Source != nil {
			loc = sf.Source.String()
		}
		fmt.Fprintf(os.Stderr, "  %d. %s (score=%d, scaling=O(N^%d)) at %s\n", //nolint:errcheck
			i+1, sf.Name, sf.TotalScore, sf.ScalingOrder, loc)
	}
	fmt.Fprintln(os.Stderr) //nolint:errcheck
}

func init() {
	rootCmd.AddCommand(AnalyzeCommand())
}
