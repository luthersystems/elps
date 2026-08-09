// Copyright © 2018 The ELPS authors

package cmd

import (
	"errors"
	"fmt"
	"io"
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
arguments are interpreted as Lisp expressions and evaluated directly. With
-p, the value of each file or expression is printed to stdout.

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
		if err := runElps(args, os.Stdout); err != nil {
			if !errors.Is(err, errRendered) {
				fmt.Fprintf(os.Stderr, "%v\n", err)
			}
			os.Exit(1)
		}
	},
}

// errRendered signals that the failure has already been written to stderr in
// diagnostic form, so the caller must not print it again.
var errRendered = errors.New("elps: error already rendered")

// runElps loads each argument — a source file, or with -e a Lisp expression —
// into a fresh environment, writing values to stdout when -p is set.
func runElps(args []string, stdout io.Writer) error {
	rootDir := runRootDir
	if rootDir == "" {
		wd, err := os.Getwd()
		if err != nil {
			return fmt.Errorf("cannot determine working directory: %w", err)
		}
		rootDir = wd
	}
	rootDir, err := filepath.Abs(rootDir)
	if err != nil {
		return fmt.Errorf("cannot resolve root directory: %w", err)
	}

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS(rootDir)}
	for _, rc := range []*lisp.LVal{
		lisp.InitializeUserEnv(env),
		lisplib.LoadLibrary(env),
		env.InPackage(lisp.String(lisp.DefaultUserPackage)),
	} {
		if !rc.IsNil() {
			return fmt.Errorf("%v", rc)
		}
	}

	for i := range args {
		var res *lisp.LVal
		// name selects the source shown in the "try: elps lint" hint. An
		// expression has no file to lint, so it is left empty.
		name := ""
		if runExpression {
			res = env.LoadString(fmt.Sprintf("expression %d", i+1), args[i])
		} else {
			arg, ferr := toRelativePath(rootDir, args[i])
			if ferr != nil {
				return ferr
			}
			res = env.LoadFile(arg)
			name = args[i]
		}
		if res.Type == lisp.LError {
			renderLispError(res, name)
			return errRendered
		}
		if runPrint {
			//nolint:errcheck // best-effort output to stdout
			fmt.Fprintln(stdout, res.String())
		}
	}
	return nil
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
