// Copyright © 2026 The ELPS authors

package cmd

import (
	"fmt"
	"io"
	"os"

	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/minifier"
	"github.com/spf13/cobra"
)

var (
	minifyWrite         bool
	minifyMapPath       string
	minifyExcludeFiles  []string
	minifyExcludes      []string
	minifyWorkspace     string
	minifyRenameExports bool
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
removes redundant whitespace and strips comments.`,
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
		Exclusions:    exclusions,
		RenameExports: minifyRenameExports,
		Formatter:     formatter.DefaultConfig(),
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
		return fmt.Errorf("multiple files require --write")
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
		return fmt.Errorf("no .lisp files found")
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
		"Rename exported top-level symbols and rewrite matching export forms.")
}
