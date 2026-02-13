// Copyright © 2018 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var (
	runExpression bool
	runPrint      bool
	runRootDir    string
)

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run [flags] [files...]",
	Short: "Run elps source files or expressions",
	Long: `Run ELPS Lisp code from files or command-line expressions.

With file arguments, each file is loaded and executed in order. With -e,
arguments are interpreted as Lisp expressions and evaluated directly.

The runtime loads all standard library packages automatically. User code
starts in the "user" package and can import other packages with use-package.

File access is confined to the root directory (--root-dir, default: working
directory). The load-file function can only read files within this tree.

Examples:
  elps run hello.lisp              Run a source file
  elps run lib.lisp app.lisp       Load files in order (lib first)
  elps run -e '(+ 1 2)'            Evaluate an expression
  elps run -e -p '(* 6 7)'         Evaluate and print the result
  elps run --root-dir /app scripts/main.lisp

Exit codes:
  0  Success
  1  Runtime error (use elps lint to catch common mistakes before running)`,
	Run: func(cmd *cobra.Command, args []string) {
		rootDir := runRootDir
		if rootDir == "" {
			wd, err := os.Getwd()
			if err != nil {
				fmt.Fprintf(os.Stderr, "cannot determine working directory: %v\n", err)
				os.Exit(1)
			}
			rootDir = wd
		}
		rootDir, err := filepath.Abs(rootDir)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cannot resolve root directory: %v\n", err)
			os.Exit(1)
		}

		env := lisp.NewEnv(nil)
		reader := parser.NewReader()
		env.Runtime.Reader = reader
		env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS(rootDir)}
		rc := lisp.InitializeUserEnv(env)
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		rc = lisplib.LoadLibrary(env)
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
		if !rc.IsNil() {
			fmt.Fprintln(os.Stderr, rc)
			os.Exit(1)
		}
		for i := range args {
			arg, ferr := toRelativePath(rootDir, args[i])
			if ferr != nil {
				fmt.Fprintf(os.Stderr, "%v\n", ferr)
				os.Exit(1)
			}
			res := env.LoadFile(arg)
			if res.Type == lisp.LError {
				renderLispError(res, args[i])
				os.Exit(1)
			}
		}
	},
}

// toRelativePath converts a file path to be relative to rootDir.
// Relative paths are returned as-is. Absolute paths within rootDir
// are converted; absolute paths outside rootDir produce an error.
func toRelativePath(rootDir, path string) (string, error) {
	if !filepath.IsAbs(path) {
		return path, nil
	}
	rel, err := filepath.Rel(rootDir, path)
	if err != nil {
		return "", fmt.Errorf("%s: cannot make relative to root directory %s: %w", path, rootDir, err)
	}
	// filepath.Rel can produce ".." components for paths outside rootDir.
	if len(rel) >= 2 && rel[:2] == ".." {
		return "", fmt.Errorf("%s: outside root directory %s", path, rootDir)
	}
	return rel, nil
}

func init() {
	rootCmd.AddCommand(runCmd)

	// Here flags for the run command are defined
	runCmd.Flags().BoolVarP(&runExpression, "expression", "e", false,
		"Interpret arguments as lisp expressions")
	runCmd.Flags().BoolVarP(&runPrint, "print", "p", false,
		"Print expression values to stdout")
	runCmd.Flags().StringVar(&runRootDir, "root-dir", "",
		"Root directory for file access confinement (default: working directory)")
}
