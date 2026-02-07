// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"io"
	"os"

	"github.com/luthersystems/elps/formatter"
	"github.com/spf13/cobra"
)

var (
	fmtWrite      bool
	fmtDiff       bool
	fmtList       bool
	fmtIndentSize int
)

var fmtCmd = &cobra.Command{
	Use:   "fmt [flags] [files...]",
	Short: "Format elps source files",
	Long: `Format elps source files.

With no files and no flags, reads from stdin and writes to stdout.
With files, prints formatted output to stdout (use -w to overwrite).`,
	Run: func(cmd *cobra.Command, args []string) {
		cfg := formatter.DefaultConfig()
		cfg.IndentSize = fmtIndentSize

		if len(args) == 0 {
			if err := fmtStdin(cfg); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
			return
		}
		exitCode := 0
		for _, path := range args {
			if err := fmtFile(path, cfg); err != nil {
				fmt.Fprintln(os.Stderr, err)
				exitCode = 1
			}
		}
		os.Exit(exitCode)
	},
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

func fmtFile(path string, cfg *formatter.Config) error {
	src, err := os.ReadFile(path) //nolint:gosec // CLI tool reads user-specified files
	if err != nil {
		return fmt.Errorf("%s: %w", path, err)
	}
	out, err := formatter.FormatFile(src, path, cfg)
	if err != nil {
		return err
	}

	if fmtList {
		if string(src) != string(out) {
			fmt.Println(path)
		}
		return nil
	}

	if fmtDiff {
		if string(src) != string(out) {
			printUnifiedDiff(path, src, out)
		}
		return nil
	}

	if fmtWrite {
		if string(src) == string(out) {
			return nil // no change
		}
		info, err := os.Stat(path)
		if err != nil {
			return fmt.Errorf("%s: %w", path, err)
		}
		return os.WriteFile(path, out, info.Mode().Perm())
	}

	// Default: print to stdout
	_, err = os.Stdout.Write(out)
	return err
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
	rootCmd.AddCommand(fmtCmd)

	fmtCmd.Flags().BoolVarP(&fmtWrite, "write", "w", false,
		"Write result to (source) file instead of stdout.")
	fmtCmd.Flags().BoolVarP(&fmtDiff, "diff", "d", false,
		"Display diffs instead of rewriting files.")
	fmtCmd.Flags().BoolVarP(&fmtList, "list", "l", false,
		"List files whose formatting differs from elps fmt's.")
	fmtCmd.Flags().IntVar(&fmtIndentSize, "indent-size", 2,
		"Number of spaces per indentation level.")
}
