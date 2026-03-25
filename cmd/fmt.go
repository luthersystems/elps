// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"io"
	"os"

	"github.com/luthersystems/elps/formatter"
	"github.com/spf13/cobra"
)

// FmtCommand creates the "fmt" cobra command. Embedders can add this
// to their own CLI (e.g., shirotester fmt) without duplicating the
// implementation.
func FmtCommand(opts ...Option) *cobra.Command {
	var cfg cmdConfig
	for _, o := range opts {
		o(&cfg)
	}

	var (
		fmtWrite      bool
		fmtDiff       bool
		fmtList       bool
		fmtIndentSize int
		fmtExcludes   []string
	)

	cmd := &cobra.Command{
		Use:   "fmt [flags] [files...]",
		Short: "Format ELPS source files",
		Long: `Format ELPS Lisp source files, similar to gofmt for Go.

Normalizes whitespace and indentation, aligns forms according to Lisp
conventions, and preserves comments. The formatter is idempotent.

With no files, reads from stdin and writes to stdout.
With files, prints formatted output to stdout unless -w is given.

Modes:
  (default)   Print formatted code to stdout
  -w          Write result back to source file
  -d          Display a diff of changes
  -l          List files that would be changed

Examples:
  elps fmt file.lisp               Print formatted output
  elps fmt -w file.lisp            Format in place
  elps fmt -w *.lisp               Format all lisp files in place
  elps fmt -d file.lisp            Show what would change
  elps fmt -l *.lisp               List files needing formatting
  cat file.lisp | elps fmt         Format from stdin
  elps fmt --indent-size 4 f.lisp  Use 4-space indentation`,
		Run: func(cmd *cobra.Command, args []string) {
			fmtCfg := formatter.DefaultConfig()
			fmtCfg.IndentSize = fmtIndentSize

			if len(args) == 0 {
				if err := fmtStdin(fmtCfg); err != nil {
					fmt.Fprintln(os.Stderr, err)
					os.Exit(1)
				}
				return
			}

			expanded, err := expandArgs(args, fmtExcludes)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}

			exitCode := 0
			for _, path := range expanded {
				changed, err := fmtFile(path, fmtCfg, fmtWrite, fmtDiff, fmtList)
				if err != nil {
					fmt.Fprintln(os.Stderr, err)
					exitCode = 1
				} else if fmtList && changed {
					exitCode = 1
				}
			}
			os.Exit(exitCode)
		},
	}

	cmd.Flags().BoolVarP(&fmtWrite, "write", "w", false,
		"Write result to (source) file instead of stdout.")
	cmd.Flags().BoolVarP(&fmtDiff, "diff", "d", false,
		"Display diffs instead of rewriting files.")
	cmd.Flags().BoolVarP(&fmtList, "list", "l", false,
		"List files whose formatting differs from elps fmt's.")
	cmd.Flags().IntVar(&fmtIndentSize, "indent-size", 2,
		"Number of spaces per indentation level.")
	cmd.Flags().StringArrayVar(&fmtExcludes, "exclude", nil,
		"Glob pattern for files to exclude (may be repeated).")

	return cmd
}

func fmtStdin(cfg *formatter.Config) error {
	src, err := io.ReadAll(os.Stdin)
	if err != nil {
		return fmt.Errorf("reading stdin: %w", err)
	}
	out, err := formatter.Format(src, cfg)
	if err != nil {
		return fmt.Errorf("<stdin>: %w", err)
	}
	_, err = os.Stdout.Write(out)
	return err
}

func fmtFile(path string, cfg *formatter.Config, write, diff, list bool) (bool, error) {
	src, err := os.ReadFile(path) //nolint:gosec // CLI tool reads user-specified files
	if err != nil {
		return false, fmt.Errorf("%s: %w", path, err)
	}
	out, err := formatter.FormatFile(src, path, cfg)
	if err != nil {
		return false, err
	}

	changed := string(src) != string(out)

	if list {
		if changed {
			fmt.Println(path)
		}
		return changed, nil
	}

	if diff {
		if changed {
			printUnifiedDiff(path, src, out)
		}
		return changed, nil
	}

	if write {
		if !changed {
			return false, nil
		}
		info, err := os.Stat(path)
		if err != nil {
			return false, fmt.Errorf("%s: %w", path, err)
		}
		return true, os.WriteFile(path, out, info.Mode().Perm())
	}

	// Default: print to stdout
	_, err = os.Stdout.Write(out)
	return changed, err
}

func printUnifiedDiff(path string, original, formatted []byte) {
	// Simple line-by-line diff output
	fmt.Printf("--- %s\n", path)
	fmt.Printf("+++ %s\n", path)

	origLines := splitLines(original)
	fmtLines := splitLines(formatted)

	i, j := 0, 0
	for i < len(origLines) || j < len(fmtLines) {
		if i < len(origLines) && j < len(fmtLines) && origLines[i] == fmtLines[j] {
			fmt.Printf(" %s\n", origLines[i])
			i++
			j++
		} else if i < len(origLines) {
			fmt.Printf("-%s\n", origLines[i])
			i++
		} else {
			fmt.Printf("+%s\n", fmtLines[j])
			j++
		}
	}
}

func splitLines(data []byte) []string {
	var lines []string
	start := 0
	for i, b := range data {
		if b == '\n' {
			lines = append(lines, string(data[start:i]))
			start = i + 1
		}
	}
	if start < len(data) {
		lines = append(lines, string(data[start:]))
	}
	return lines
}

func init() {
	rootCmd.AddCommand(FmtCommand())
}
