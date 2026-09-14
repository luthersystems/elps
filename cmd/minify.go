// Copyright © 2026 The ELPS authors

package cmd

import (
	"errors"
	"fmt"
	"io"
	"os"

	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/minifier"
	"github.com/spf13/cobra"
)

var (
	minifyWrite          bool
	minifyMapPath        string
	minifyExcludeFiles   []string
	minifyExcludes       []string
	minifyWorkspace      string
	minifyRenameExports  bool
	minifyPreserveParams bool
)

var minifyCmd = &cobra.Command{
	Use:   "minify [flags] [files...]",
	Short: "Minify ELPS source files with a symbol map",
	Long: `Minify ELPS source files using deterministic, scope-aware symbol renaming.

With no files, reads from stdin and writes the minified source to stdout.
With one file, prints minified output to stdout unless -w is given.
With multiple files, use -w to rewrite files in place.

The command can also emit a machine-readable JSON symbol map for downstream
tooling via --map. Minified output uses the formatter's compact mode, which
removes redundant whitespace and strips comments.

Any symbol quoted anywhere in the inputs, including quoted lists (also [...]) and quasiquote
templates, is excluded from renaming in every scope and package. Quoted names
are not shortened, even with --rename-exports; the symbol map records them in
excluded with reason "quoted-reference". This conservative rule preserves quoted
function designators such as (map 'list 'twice values).

Calls to load-string, load-bytes, load-file, eval, macroexpand, macroexpand-1,
gensym, type, or qualified-symbol preserve EVERY package-level binding name
across all input files, even with --rename-exports. The same rule covers symbol
and intern when supplied by a host, and lisp:-qualified spellings. References
passed as function values or appearing in quoted templates also trigger it
conservatively. Purely lexical locals can still be renamed.

The symbol map records these globals in excluded with reason
"dynamic-evaluation" (taking precedence over "quoted-reference"). One warning
names the first detected site. This prevents runtime-generated names or code
from referring to renamed globals, at the cost of larger output and potentially
little identifier compression for programs using dynamic evaluation.

Package-level bindings are renamed only when package flow and exported names can
be proven statically: every export argument must be a literal quoted symbol,
a literal string, or a literal list (possibly nested) of those, and every
in-package / use-package form must be at the top level of a file with literal
package names. A variable or expression passed to export, a computed package
name, or a package switch/import nested inside any form (including progn,
let, when, functions, and macros) triggers the same fallback as dynamic
evaluation: preserve every package-level binding name across all input files,
even with --rename-exports, while lexical locals can still shorten. The symbol
map records unproven-package-flow for this fallback (dynamic-evaluation for
dynamic evaluation), taking precedence over quoted-reference; one warning
names the first offending form, whose reason is used for all preserved globals.
Literal export names remain preserved even with --rename-exports.

defun, defmacro, set with a quoted symbol, and export affect package bindings at
any nesting depth. A nested function still captures its enclosing lexical values.`,
	Run: func(_ *cobra.Command, args []string) {
		if err := runMinify(args, os.Stdin, os.Stdout); err != nil {
			fmt.Fprintf(os.Stderr, "elps minify: %v\n", err)
			os.Exit(2)
		}
	},
}

func buildMinifyConfig() (*minifier.Config, error) {
	exclusions := make(map[string]bool, len(minifyExcludes))
	for _, name := range minifyExcludes {
		exclusions[name] = true
	}
	for _, path := range minifyExcludeFiles {
		names, err := minifier.ReadExcludeFile(path)
		if err != nil {
			return nil, fmt.Errorf("reading exclude file %s: %w", path, err)
		}
		for _, name := range names {
			exclusions[name] = true
		}
	}

	cfg := &minifier.Config{
		Exclusions:     exclusions,
		RenameExports:  minifyRenameExports,
		PreserveParams: minifyPreserveParams,
		Formatter:      formatter.DefaultConfig(),
		Warn: func(message string) {
			fmt.Fprintf(os.Stderr, "elps minify: warning: %s\n", message)
		},
	}
	cfg.Formatter.Compact = true
	cfg.Formatter.StripComments = true

	if minifyWorkspace != "" {
		acfg, err := lint.BuildAnalysisConfig(&lint.LintConfig{Workspace: minifyWorkspace})
		if err != nil {
			return nil, err
		}
		cfg.Analysis = acfg
	}

	return cfg, nil
}

func runMinify(args []string, stdin io.Reader, stdout io.Writer) error {
	if len(args) > 1 && !minifyWrite {
		return errors.New("multiple files require --write")
	}

	cfg, err := buildMinifyConfig()
	if err != nil {
		return err
	}

	if len(args) == 0 {
		return minifyStdin(cfg, stdin, stdout)
	}

	expanded, err := expandArgs(args, nil)
	if err != nil {
		return err
	}
	if len(expanded) == 0 {
		return errors.New("no .lisp files found")
	}

	inputs := make([]minifier.InputFile, 0, len(expanded))
	for _, path := range expanded {
		src, readErr := os.ReadFile(path) //nolint:gosec // CLI tool reads user-specified files
		if readErr != nil {
			return fmt.Errorf("%s: %w", path, readErr)
		}
		inputs = append(inputs, minifier.InputFile{Path: path, Source: src})
	}

	result, err := minifier.Minify(inputs, cfg)
	if err != nil {
		return err
	}

	if minifyWrite {
		for _, file := range result.Files {
			info, statErr := os.Stat(file.Path)
			if statErr != nil {
				return fmt.Errorf("%s: %w", file.Path, statErr)
			}
			if writeErr := os.WriteFile(file.Path, file.Output, info.Mode().Perm()); writeErr != nil {
				return fmt.Errorf("%s: %w", file.Path, writeErr)
			}
		}
	} else {
		if _, err := stdout.Write(result.Files[0].Output); err != nil {
			return err
		}
	}

	return writeMapIfNeeded(result.SymbolMap)
}

func minifyStdin(cfg *minifier.Config, stdin io.Reader, stdout io.Writer) error {
	src, err := io.ReadAll(stdin)
	if err != nil {
		return err
	}
	out, symMap, err := minifier.MinifySource(src, "<stdin>", cfg)
	if err != nil {
		return err
	}
	if _, err := stdout.Write(out); err != nil {
		return err
	}
	return writeMapIfNeeded(symMap)
}

func writeMapIfNeeded(symMap minifier.SymbolMap) error {
	if minifyMapPath == "" {
		return nil
	}
	data, err := symMap.JSON()
	if err != nil {
		return err
	}
	return os.WriteFile(minifyMapPath, data, 0o600)
}

func init() {
	rootCmd.AddCommand(minifyCmd)

	minifyCmd.Flags().BoolVarP(&minifyWrite, "write", "w", false,
		"Write result back to source files.")
	minifyCmd.Flags().StringVar(&minifyMapPath, "map", "",
		"Write the JSON symbol map to the given path.")
	minifyCmd.Flags().StringArrayVar(&minifyExcludes, "exclude", nil,
		"Symbol name to exclude from renaming (may be repeated).")
	minifyCmd.Flags().StringArrayVar(&minifyExcludeFiles, "exclude-file", nil,
		"File containing symbols to exclude from renaming (may be repeated).")
	minifyCmd.Flags().StringVar(&minifyWorkspace, "workspace", "",
		"Workspace root for cross-file semantic resolution.")
	minifyCmd.Flags().BoolVar(&minifyRenameExports, "rename-exports", false,
		"Rename exported symbols unless excluded (literal export names and dynamic-evaluation globals are always preserved).")
	minifyCmd.Flags().BoolVar(&minifyPreserveParams, "preserve-params", true,
		"Preserve function and macro parameter names (default: true). Use --preserve-params=false to rename them.")
}
