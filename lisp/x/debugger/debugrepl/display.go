// Copyright © 2018 The ELPS authors

package debugrepl

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
)

const sourceContextLines = 5

// showSourceContext prints a window of source lines around the given line,
// with a --> marker on the current line.
func showSourceContext(w io.Writer, file string, line int, sourceRoot string) {
	path := resolveSourceFile(file, sourceRoot)
	f, err := os.Open(path) //#nosec G304
	if err != nil {
		fmt.Fprintf(w, "  at %s:%d (source not available)\n", file, line) //nolint:errcheck
		return
	}
	defer f.Close() //nolint:errcheck

	scanner := bufio.NewScanner(f)
	lineNum := 0
	start := line - sourceContextLines
	if start < 1 {
		start = 1
	}
	end := line + sourceContextLines

	for scanner.Scan() {
		lineNum++
		if lineNum < start {
			continue
		}
		if lineNum > end {
			break
		}
		marker := "   "
		if lineNum == line {
			marker = "-->"
		}
		fmt.Fprintf(w, "%s %4d  %s\n", marker, lineNum, scanner.Text()) //nolint:errcheck
	}
}

// showBacktrace prints the call stack in a human-readable format.
func showBacktrace(w io.Writer, stack *lisp.CallStack, pausedExpr *lisp.LVal, sourceRoot string) {
	if stack == nil || len(stack.Frames) == 0 {
		fmt.Fprintln(w, "  (empty stack)") //nolint:errcheck
		return
	}

	// Print frames in reverse order (most recent first).
	for i := len(stack.Frames) - 1; i >= 0; i-- {
		frame := &stack.Frames[i]
		name := frame.QualifiedFunName()
		if name == "" {
			name = "<anonymous>"
		}
		loc := "unknown"
		// For the top frame, use the paused expression's location.
		if i == len(stack.Frames)-1 && pausedExpr != nil && pausedExpr.Source != nil {
			loc = fmt.Sprintf("%s:%d:%d", pausedExpr.Source.File, pausedExpr.Source.Line, pausedExpr.Source.Col)
		} else if frame.Source != nil {
			loc = frame.Source.String()
		}
		depth := len(stack.Frames) - i
		fmt.Fprintf(w, "  #%d  %s  at %s\n", depth, name, loc) //nolint:errcheck
	}
}

// showLocals prints the local variable bindings in a tabular format.
func showLocals(w io.Writer, env *lisp.LEnv, engine *debugger.Engine) {
	locals := debugger.InspectFunctionLocals(env)
	if len(locals) == 0 {
		fmt.Fprintln(w, "  (no locals)") //nolint:errcheck
		return
	}
	for _, b := range locals {
		fmt.Fprintf(w, "  %-20s = %s\n", b.Name, debugger.FormatValueWith(b.Value, engine)) //nolint:errcheck
	}
}

// showBreakpoints prints all breakpoints in a tabular format.
func showBreakpoints(w io.Writer, store *debugger.BreakpointStore) {
	bps := store.All()
	if len(bps) == 0 {
		fmt.Fprintln(w, "  (no breakpoints)") //nolint:errcheck
		return
	}
	sort.Slice(bps, func(i, j int) bool { return bps[i].ID < bps[j].ID })
	for _, bp := range bps {
		status := "enabled"
		if !bp.Enabled {
			status = "disabled"
		}
		line := fmt.Sprintf("  #%d  %s:%d  %s", bp.ID, bp.File, bp.Line, status)
		if bp.Condition != "" {
			line += fmt.Sprintf("  if %s", bp.Condition)
		}
		fmt.Fprintln(w, line) //nolint:errcheck
	}
}

// resolveSourceFile attempts to find the source file by trying the file
// path directly and then under the source root.
func resolveSourceFile(file, sourceRoot string) string {
	// Try as-is first.
	if _, err := os.Stat(file); err == nil {
		return file
	}
	// Try under sourceRoot.
	if sourceRoot != "" {
		// Try joining with sourceRoot.
		joined := filepath.Join(sourceRoot, file)
		if _, err := os.Stat(joined); err == nil {
			return joined
		}
		// Try just the basename under sourceRoot.
		base := filepath.Base(file)
		if base != file {
			joined = filepath.Join(sourceRoot, base)
			if _, err := os.Stat(joined); err == nil {
				return joined
			}
		}
		// Walk sourceRoot looking for the basename.
		var found string
		_ = filepath.Walk(sourceRoot, func(path string, info os.FileInfo, err error) error { //nolint:errcheck
			if err != nil || info.IsDir() || found != "" {
				return err
			}
			if filepath.Base(path) == base {
				found = path
				return filepath.SkipAll
			}
			return nil
		})
		if found != "" {
			return found
		}
	}
	return file
}

