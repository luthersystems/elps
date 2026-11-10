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

The output uses LF line endings. A file with CRLF line endings is rewritten
with LF. The bytes inside a raw string stay as written.

With no files, reads from stdin and writes to stdout.
With files, prints formatted output to stdout unless -w is given.

Modes:
  (default)   Print formatted code to stdout
  -w          Write result back to source file
  -d          Display a unified diff of changes (the format of diff -u)
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
				changed, err := fmtFile(path, fmtCfg, fmtOptions{write: fmtWrite, diff: fmtDiff, list: fmtList})
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

// fmtOptions holds formatter output flags.
type fmtOptions struct {
	// write writes formatted files.
	write bool
	// diff prints formatting differences.
	diff bool
	// list lists changed files.
	list bool
}

func fmtFile(path string, cfg *formatter.Config, opts fmtOptions) (bool, error) {
	write, diff, list := opts.write, opts.diff, opts.list

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
			if err = writeUnifiedDiff(os.Stdout, path, src, out); err != nil {
				return changed, fmt.Errorf("%s: %w", path, err)
			}
		}
		return changed, nil
	}

	if write {
		if !changed {
			return false, nil
		}
		info, statErr := os.Stat(path)
		if statErr != nil {
			return false, fmt.Errorf("%s: %w", path, statErr)
		}
		return true, os.WriteFile(path, out, info.Mode().Perm()) //nolint:gosec // G703: `elps fmt -w` writes back the file it was asked to format
	}

	// Default: print to stdout
	_, err = os.Stdout.Write(out)
	return changed, err
}

func init() {
	rootCmd.AddCommand(FmtCommand())
}
