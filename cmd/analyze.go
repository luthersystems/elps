// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis/perf"
	"github.com/spf13/cobra"
)

// AnalyzeOption configures the analyze command for embedder integration.
type AnalyzeOption func(*perf.Config)

type analyzeRunConfig struct {
	jsonOutput   bool
	configFile   string
	excludes     []string
	includeTests bool
	rules        string
	failOn       string
	topN         int
}

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
		jsonOutput   bool
		configFile   string
		excludes     []string
		includeTests bool
		rules        string
		failOn       string
		topN         int
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
  elps analyze --include-tests ./...           # Include *_test.lisp files
  elps analyze --rules=PERF003,PERF004 ./...   # Run only specific rules
  elps analyze --fail-on=error ./...           # Only fail on errors, not warnings
  elps analyze --top=5 ./...                   # Show top 5 hottest functions
  elps analyze --config=my.yaml ./...          # Use custom config`,
		Run: func(_ *cobra.Command, args []string) {
			code, err := runAnalyze(args, os.Stdout, os.Stderr, analyzeRunConfig{
				jsonOutput:   jsonOutput,
				configFile:   configFile,
				excludes:     excludes,
				includeTests: includeTests,
				rules:        rules,
				failOn:       failOn,
				topN:         topN,
			}, analyzeOpts...)
			if err != nil {
				fmt.Fprintf(os.Stderr, "elps analyze: %v\n", err)
				os.Exit(code)
			}
			if code != 0 {
				os.Exit(code)
			}
		},
	}

	cmd.Flags().BoolVar(&jsonOutput, "json", false,
		"Output issues as JSON.")
	cmd.Flags().StringVar(&configFile, "config", "",
		"Path to .elps-analyze.yaml config file.")
	cmd.Flags().StringArrayVar(&excludes, "exclude", nil,
		"Glob pattern for files to exclude (may be repeated).")
	cmd.Flags().BoolVar(&includeTests, "include-tests", false,
		"Include *_test.lisp files in analysis results.")
	cmd.Flags().StringVar(&rules, "rules", "",
		"Comma-separated list of rules to run (default: all). Valid: PERF001,PERF002,PERF003,PERF004,UNKNOWN001.")
	cmd.Flags().StringVar(&failOn, "fail-on", "warning",
		"Minimum severity to cause a non-zero exit code (error, warning, info).")
	cmd.Flags().IntVar(&topN, "top", 0,
		"Show the top N hottest functions by total score.")

	return cmd
}

func runAnalyze(args []string, stdout, stderr io.Writer, runCfg analyzeRunConfig, analyzeOpts ...AnalyzeOption) (int, error) {
	if len(args) == 0 {
		return 2, fmt.Errorf("no files specified")
	}

	analyzeConfig, err := loadAnalyzeConfig(runCfg.configFile)
	if err != nil {
		return 2, err
	}

	for _, opt := range analyzeOpts {
		opt(analyzeConfig)
	}

	if runCfg.rules != "" {
		var ruleList []string
		for _, r := range strings.Split(runCfg.rules, ",") {
			r = strings.TrimSpace(r)
			if r != "" {
				ruleList = append(ruleList, r)
			}
		}
		analyzeConfig.Rules = ruleList
	}

	threshold, err := parseAnalyzeFailOn(runCfg.failOn)
	if err != nil {
		return 2, err
	}

	expanded, err := expandArgs(args, analyzeExcludePatterns(analyzeConfig, runCfg.excludes, runCfg.includeTests))
	if err != nil {
		return 2, err
	}
	if len(expanded) == 0 {
		return 2, fmt.Errorf("no .lisp files found")
	}

	result, err := perf.AnalyzeFiles(expanded, analyzeConfig)
	if err != nil {
		return 2, err
	}

	if runCfg.topN > 0 && result.Solved != nil {
		showTopFunctions(result.Solved, runCfg.topN)
	}

	if len(result.Issues) == 0 {
		return 0, nil
	}

	if runCfg.jsonOutput {
		if err := perf.FormatJSON(stdout, result.Issues); err != nil {
			return 2, err
		}
	} else {
		perf.FormatText(stderr, result.Issues)
	}

	for _, issue := range result.Issues {
		if issue.Severity <= threshold {
			return 1, nil
		}
	}
	return 0, nil
}

func loadAnalyzeConfig(configFile string) (*perf.Config, error) {
	if configFile != "" {
		cfg, err := perf.LoadConfigFile(configFile)
		if err != nil {
			return nil, fmt.Errorf("loading config: %w", err)
		}
		return cfg, nil
	}

	wd, _ := os.Getwd()
	found := perf.FindConfigFile(wd)
	if found == "" {
		return perf.DefaultConfig(), nil
	}

	cfg, err := perf.LoadConfigFile(found)
	if err != nil {
		return nil, fmt.Errorf("loading config %s: %w", found, err)
	}
	return cfg, nil
}

func parseAnalyzeFailOn(failOn string) (perf.Severity, error) {
	switch failOn {
	case "", "warning":
		return perf.SeverityWarning, nil
	case "error":
		return perf.SeverityError, nil
	case "info":
		return perf.SeverityInfo, nil
	default:
		return perf.SeverityWarning, fmt.Errorf("invalid --fail-on value: %q (valid: error, warning, info)", failOn)
	}
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

func analyzeExcludePatterns(cfg *perf.Config, cliExcludes []string, includeTests bool) []string {
	var excludes []string
	if cfg != nil {
		for _, pattern := range cfg.ExcludeFiles {
			if effectiveIncludeTests(cfg, includeTests) && excludeTargetsTestFiles(pattern) {
				continue
			}
			excludes = append(excludes, pattern)
		}
	}
	excludes = append(excludes, cliExcludes...)
	if !effectiveIncludeTests(cfg, includeTests) {
		excludes = append(excludes, "*_test.lisp")
	}
	return excludes
}

func effectiveIncludeTests(cfg *perf.Config, includeTests bool) bool {
	return includeTests || (cfg != nil && cfg.IncludeTests)
}

func excludeTargetsTestFiles(pattern string) bool {
	testSamples := []string{
		"example_test.lisp",
		filepath.Join("pkg", "example_test.lisp"),
	}
	nonTestSamples := []string{
		"example.lisp",
		filepath.Join("pkg", "example.lisp"),
	}

	matchesTest := false
	for _, sample := range testSamples {
		if pathMatchesPattern(sample, pattern) {
			matchesTest = true
			break
		}
	}
	if !matchesTest {
		return false
	}
	for _, sample := range nonTestSamples {
		if pathMatchesPattern(sample, pattern) {
			return false
		}
	}
	return true
}

func pathMatchesPattern(path, pattern string) bool {
	if matched, _ := filepath.Match(pattern, path); matched {
		return true
	}
	if matched, _ := filepath.Match(pattern, filepath.Base(path)); matched {
		return true
	}
	for _, component := range splitPath(path) {
		if matched, _ := filepath.Match(pattern, component); matched {
			return true
		}
	}
	return false
}
