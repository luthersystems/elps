// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"strings"

	"github.com/luthersystems/elps/lint"
	"github.com/spf13/cobra"
)

// LintCommand creates the "lint" cobra command with optional embedder
// configuration. Embedders can pass WithRegistry or WithEnv to inject
// Go-registered symbols for semantic analysis.
func LintCommand(opts ...Option) *cobra.Command {
	var cfg cmdConfig
	for _, o := range opts {
		o(&cfg)
	}

	var (
		jsonOutput  bool
		checks      string
		listAll     bool
		excludes    []string
		workspace   string
		noWorkspace bool
	)

	cmd := &cobra.Command{
		Use:   "lint [flags] [files...]",
		Short: "Run static analysis checks on elps source files",
		Long: `Run static analysis checks on elps source files.

The linter reports likely mistakes in ELPS code, similar to "go vet" for Go.
Each check is an independent analyzer that examines the parsed AST and reports
diagnostics. The linter does NOT report style issues — use "elps fmt" for that.

With no files, reads from stdin. With files, analyzes each file and reports
all findings to stderr.

Semantic analysis (--workspace): When a workspace directory is specified,
the linter performs scope-aware analysis that enables additional checks:
undefined-symbol, unused-variable, unused-function, shadowing, and user-arity.
Without --workspace, these checks are disabled and the linter only runs
syntactic checks.

Exit codes:
  0  No problems found
  1  One or more problems were reported
  2  Bad invocation (invalid flags, unreadable files)

To suppress a specific diagnostic, add a comment on the same line:
  (set x 42) ; nolint:set-usage

To suppress all checks on a line:
  (set x 42) ; nolint

Available checks (use --checks to select specific ones):
` + lint.AnalyzerDoc() + `
Examples:
  elps lint file.lisp                                 # Lint a single file
  elps lint *.lisp                                    # Lint multiple files
  elps lint --json file.lisp                          # Output diagnostics as JSON
  elps lint --checks=if-arity file.lisp               # Run only specific checks
  elps lint --list                                    # List available checks
  elps lint --exclude='shirocore.lisp' ./...          # Exclude a file by name
  elps lint --exclude='build' --exclude='vendor' ./...  # Exclude directories
  elps lint --workspace=. ./...                       # Enable semantic analysis
  cat file.lisp | elps lint                           # Lint from stdin`,
		Run: func(_ *cobra.Command, args []string) {
			if listAll {
				for _, name := range lint.AnalyzerNames() {
					fmt.Println(name)
				}
				return
			}

			analyzers := lint.DefaultAnalyzers()
			if checks != "" {
				selected := make(map[string]bool)
				for _, name := range strings.Split(checks, ",") {
					selected[strings.TrimSpace(name)] = true
				}
				var filtered []*lint.Analyzer
				for _, a := range analyzers {
					if selected[a.Name] {
						filtered = append(filtered, a)
						delete(selected, a.Name)
					}
				}
				for name := range selected {
					fmt.Fprintf(os.Stderr, "elps lint: unknown check: %s\n", name)
					os.Exit(2)
				}
				analyzers = filtered
			}

			l := &lint.Linter{Analyzers: analyzers}

			// Build LintConfig from CLI flags
			var lintCfg *lint.LintConfig
			if workspace != "" && !noWorkspace {
				lintCfg = &lint.LintConfig{
					Workspace: workspace,
					Excludes:  excludes,
					Registry:  cfg.resolveRegistry(),
				}
			}

			if len(args) == 0 {
				if err := lintStdin(l, lintCfg, jsonOutput); err != nil {
					fmt.Fprintln(os.Stderr, err)
					os.Exit(2)
				}
				return
			}

			expanded, err := expandArgs(args, excludes)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(2)
			}

			allDiags, err := l.LintFiles(lintCfg, expanded)
			if err != nil {
				fmt.Fprintf(os.Stderr, "elps lint: %v\n", err)
				os.Exit(2)
			}

			if len(allDiags) == 0 {
				return
			}

			if jsonOutput {
				if err := lint.FormatJSON(os.Stdout, allDiags); err != nil {
					fmt.Fprintln(os.Stderr, err)
					os.Exit(2)
				}
			} else {
				renderLintDiagnostics(allDiags)
			}
			os.Exit(1)
		},
	}

	cmd.Flags().BoolVar(&jsonOutput, "json", false,
		"Output diagnostics as JSON.")
	cmd.Flags().StringVar(&checks, "checks", "",
		"Comma-separated list of checks to run (default: all).")
	cmd.Flags().BoolVar(&listAll, "list", false,
		"List available checks and exit.")
	cmd.Flags().StringArrayVar(&excludes, "exclude", nil,
		"Glob pattern for files to exclude (may be repeated).")
	cmd.Flags().StringVar(&workspace, "workspace", "",
		"Workspace root directory for cross-file symbol resolution and semantic analysis.")
	cmd.Flags().BoolVar(&noWorkspace, "no-workspace", false,
		"Disable workspace scanning (overrides --workspace).")

	return cmd
}

func lintStdin(l *lint.Linter, cfg *lint.LintConfig, jsonOutput bool) error {
	src, err := readStdin()
	if err != nil {
		return fmt.Errorf("reading stdin: %w", err)
	}
	var diags []lint.Diagnostic
	if cfg != nil && cfg.Workspace != "" {
		acfg, buildErr := lint.BuildAnalysisConfig(cfg)
		if buildErr != nil {
			return buildErr
		}
		diags, err = l.LintFileWithAnalysis(src, "<stdin>", acfg)
	} else {
		diags, err = l.LintFile(src, "<stdin>")
	}
	if err != nil {
		return err
	}
	if len(diags) == 0 {
		return nil
	}
	if jsonOutput {
		if err := lint.FormatJSON(os.Stdout, diags); err != nil {
			return err
		}
	} else {
		renderLintDiagnostics(diags)
	}
	os.Exit(1)
	return nil
}

func readStdin() ([]byte, error) {
	return os.ReadFile("/dev/stdin")
}

func init() {
	rootCmd.AddCommand(LintCommand())
}
