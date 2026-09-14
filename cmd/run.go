// Copyright © 2018 The ELPS authors

package cmd

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/luthersystems/elps/internal/rootlibrary"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/spf13/cobra"
)

var (
	runExpression bool
	runPrint      bool
	runRootDir    string
	runTimeout    time.Duration
	runMaxSteps   int64
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

Source loads are confined to the root directory (--root-dir, default: working
directory) at open time. Relative symlinks whose targets stay inside the root
are allowed; escaping paths and absolute symlinks produce ordinary errors.

--timeout bounds elapsed evaluation time across all arguments. --max-steps
bounds evaluation steps per file or expression argument. Both default to
unlimited (0). SIGINT or SIGTERM cancels evaluation, reports a one-line error,
and exits with status 1 after cleanup; a second signal force-exits.

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
		if err := runElpsContext(cmd.Context(), args, os.Stdout); err != nil {
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
// into one fresh environment, writing values to stdout when -p is set.
// Each argument gets its own step budget; the timeout spans the entire run.
func runElps(args []string, stdout io.Writer) error {
	return runElpsContext(context.Background(), args, stdout)
}

func runElpsContext(parent context.Context, args []string, stdout io.Writer) error {
	if runTimeout < 0 {
		return errors.New("timeout must be non-negative")
	}
	if runMaxSteps < 0 {
		return errors.New("max-steps must be non-negative")
	}
	ctx, stop := evaluationContext(parent, runTimeout)
	defer stop()
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

	// File-root audit for commands that do not execute through this loader:
	// lint: reads named files/stdin and scans --workspace; workspace is not a security boundary.
	// fmt: reads named files/stdin and expanded directory trees; no root security boundary.
	// lsp: reads client documents/URIs and scans the workspace; workspace is not a security boundary.
	// mcp: reads tool-selected paths/content and scans workspaces; absolute paths and symlinks bypass its relative-path check, so no root security boundary.
	lib, err := rootlibrary.Open(rootDir)
	if err != nil {
		return err
	}
	defer lib.Close() //nolint:errcheck // release the root handle after evaluation
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = lib
	for _, rc := range []*lisp.LVal{
		lisp.InitializeUserEnv(env),
		lisplib.LoadLibrary(env),
		env.InPackage(lisp.String(lisp.DefaultUserPackage)),
	} {
		if !rc.IsNil() {
			return fmt.Errorf("%v", rc)
		}
	}

	// Apply user limits after library initialization, so even a small step
	// budget is available in full to each source argument.
	lisp.WithMaxSteps(runMaxSteps)(env)
	lisp.WithContext(ctx)(env)
	for i := range args {
		var res *lisp.LVal
		// name selects the source shown in the "try: elps lint" hint. An
		// expression has no file to lint, so it is left empty.
		name := ""
		if runExpression {
			res = env.LoadStringContext(ctx, fmt.Sprintf("expression %d", i+1), args[i])
		} else {
			arg, ferr := toRelativePath(rootDir, args[i])
			if ferr != nil {
				return ferr
			}
			res = env.LoadFileContext(ctx, arg)
			name = args[i]
		}
		// Do not traverse Lisp values or stack traces after cancellation.
		// The context supplies a bounded, one-line diagnostic.
		if err := ctx.Err(); err != nil {
			fmt.Fprintf(os.Stderr, "context-cancelled: %v\n", err)
			return errRendered
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
	// Output can block while the signal handler cancels the context.
	if err := ctx.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "context-cancelled: %v\n", err)
		return errRendered
	}
	return nil
}

// toRelativePath converts a file path to be relative to rootDir.
// Relative paths are returned as-is. Absolute paths within rootDir
// are converted; absolute paths outside rootDir produce an error.
func toRelativePath(rootDir, path string) (string, error) {
	return rootlibrary.RelativePath(rootDir, path)
}

func init() {
	rootCmd.AddCommand(runCmd)

	// Here flags for the run command are defined
	runCmd.Flags().BoolVarP(&runExpression, "expression", "e", false,
		"Interpret arguments as lisp expressions")
	runCmd.Flags().BoolVarP(&runPrint, "print", "p", false,
		"Print expression values to stdout")
	runCmd.Flags().StringVar(&runRootDir, "root-dir", "",
		"Root directory for source load confinement (default: working directory)")
	runCmd.Flags().DurationVar(&runTimeout, "timeout", 0,
		"Evaluation timeout across all arguments (0 = unlimited)")
	runCmd.Flags().Int64Var(&runMaxSteps, "max-steps", 0,
		"Maximum evaluation steps per file or expression argument (0 = unlimited)")
}
