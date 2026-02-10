// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var (
	lintJSON        bool
	lintChecks      string
	lintListAll     bool
	lintExcludes    []string
	lintWorkspace   string
	lintNoWorkspace bool
)

var lintCmd = &cobra.Command{
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
	Run: func(cmd *cobra.Command, args []string) {
		if lintListAll {
			for _, name := range lint.AnalyzerNames() {
				fmt.Println(name)
			}
			return
		}

		analyzers := lint.DefaultAnalyzers()
		if lintChecks != "" {
			selected := make(map[string]bool)
			for _, name := range strings.Split(lintChecks, ",") {
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

		// Resolve workspace configuration
		var analysisCfg *analysis.Config
		if lintWorkspace != "" && !lintNoWorkspace {
			syms, err := analysis.ScanWorkspace(lintWorkspace)
			if err != nil {
				fmt.Fprintf(os.Stderr, "elps lint: scanning workspace %s: %v\n", lintWorkspace, err)
				os.Exit(2)
			}

			// Extract stdlib package exports from a loaded env
			pkgExports := extractStdlibExports()

			// Merge workspace package exports
			wsPkgs, err := analysis.ScanWorkspacePackages(lintWorkspace)
			if err != nil {
				fmt.Fprintf(os.Stderr, "elps lint: scanning workspace packages %s: %v\n", lintWorkspace, err)
				os.Exit(2)
			}
			for pkg, wsSyms := range wsPkgs {
				pkgExports[pkg] = append(pkgExports[pkg], wsSyms...)
			}

			analysisCfg = &analysis.Config{
				ExtraGlobals:   syms,
				PackageExports: pkgExports,
			}
		}

		if len(args) == 0 {
			if err := lintStdin(l, analysisCfg); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(2)
			}
			return
		}

		expanded, err := expandArgs(args, lintExcludes)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(2)
		}

		var allDiags []lint.Diagnostic
		for _, path := range expanded {
			diags, err := lintFilePath(l, path, analysisCfg)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(2)
			}
			allDiags = append(allDiags, diags...)
		}

		if len(allDiags) == 0 {
			return
		}

		if lintJSON {
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

func lintStdin(l *lint.Linter, cfg *analysis.Config) error {
	src, err := readStdin()
	if err != nil {
		return fmt.Errorf("reading stdin: %w", err)
	}
	var diags []lint.Diagnostic
	if cfg != nil {
		diags, err = l.LintFileWithAnalysis(src, "<stdin>", cfg)
	} else {
		diags, err = l.LintFile(src, "<stdin>")
	}
	if err != nil {
		return err
	}
	if len(diags) == 0 {
		return nil
	}
	if lintJSON {
		if err := lint.FormatJSON(os.Stdout, diags); err != nil {
			return err
		}
	} else {
		renderLintDiagnostics(diags)
	}
	os.Exit(1)
	return nil
}

func lintFilePath(l *lint.Linter, path string, cfg *analysis.Config) ([]lint.Diagnostic, error) {
	src, err := os.ReadFile(path) //nolint:gosec // CLI tool reads user-specified files
	if err != nil {
		return nil, fmt.Errorf("%s: %w", path, err)
	}
	if cfg != nil {
		return l.LintFileWithAnalysis(src, path, cfg)
	}
	return l.LintFile(src, path)
}

func readStdin() ([]byte, error) {
	return os.ReadFile("/dev/stdin")
}

// extractStdlibExports creates a temporary ELPS env, loads the standard
// library, and extracts package exports. This allows the linter to resolve
// symbols imported via use-package from Go-defined stdlib packages.
func extractStdlibExports() map[string][]analysis.ExternalSymbol {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	lisp.InitializeUserEnv(env)
	lisplib.LoadLibrary(env)
	return analysis.ExtractPackageExports(env.Runtime.Registry)
}

func init() {
	rootCmd.AddCommand(lintCmd)

	lintCmd.Flags().BoolVar(&lintJSON, "json", false,
		"Output diagnostics as JSON.")
	lintCmd.Flags().StringVar(&lintChecks, "checks", "",
		"Comma-separated list of checks to run (default: all).")
	lintCmd.Flags().BoolVar(&lintListAll, "list", false,
		"List available checks and exit.")
	lintCmd.Flags().StringArrayVar(&lintExcludes, "exclude", nil,
		"Glob pattern for files to exclude (may be repeated).")
	lintCmd.Flags().StringVar(&lintWorkspace, "workspace", "",
		"Workspace root directory for cross-file symbol resolution and semantic analysis.")
	lintCmd.Flags().BoolVar(&lintNoWorkspace, "no-workspace", false,
		"Disable workspace scanning (overrides --workspace).")
}
