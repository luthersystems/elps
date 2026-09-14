// Copyright © 2018 The ELPS authors

package debugrepl

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/x/debugger"
	"github.com/luthersystems/elps/parser/token"
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
func showBacktrace(w io.Writer, stack *lisp.CallStack, pausedExpr *lisp.LVal, sourceRoot string, envs ...*lisp.LEnv) {
	var env *lisp.LEnv
	if len(envs) > 0 {
		env = envs[0]
	}
	formatter := debugger.NewValueFormatter(env, nil)
	if stack == nil || len(stack.Frames) == 0 {
		io.WriteString(w, formatter.Text("  (empty stack)\n")) //nolint:errcheck
		return
	}

	// Print frames in reverse order (most recent first).
	for i := len(stack.Frames) - 1; i >= 0; i-- {
		if formatter.Exhausted() {
			break
		}
		frame := &stack.Frames[i]
		name := frame.Name
		if name == "" {
			name = frame.FID
		}
		if name == "" {
			name = "<anonymous>"
		}
		io.WriteString(w, formatter.Text(fmt.Sprintf("  #%d  ", len(stack.Frames)-i))) //nolint:errcheck
		if frame.Package != "" {
			io.WriteString(w, formatter.Text(frame.Package, ":")) //nolint:errcheck
		}
		io.WriteString(w, formatter.Text(name, "  at ")) //nolint:errcheck
		var loc *token.Location
		// For the top frame, use the paused expression's location.
		var pausedLoc token.Location
		pausedOK := false
		if pausedExpr != nil {
			pausedLoc, pausedOK = pausedExpr.Source()
		}
		if i == len(stack.Frames)-1 && pausedOK {
			loc = &pausedLoc
		} else if frame.Source != nil {
			loc = frame.Source
		}
		if loc == nil {
			io.WriteString(w, formatter.Text("unknown\n")) //nolint:errcheck
		} else {
			suffix := fmt.Sprintf(":%d:%d", loc.Line, loc.Col)
			if !(i == len(stack.Frames)-1 && pausedOK) {
				// Preserve Location.String's numeric forms without copying its file.
				numeric := *loc
				numeric.File = ""
				suffix = numeric.String()
			}
			io.WriteString(w, formatter.Text(loc.File, suffix, "\n")) //nolint:errcheck
		}
	}
}

// showLocals prints the local variable bindings in a tabular format.
func showLocals(w io.Writer, env *lisp.LEnv, engine *debugger.Engine) {
	formatter := debugger.NewValueFormatter(env, engine)
	locals := debugger.InspectFunctionLocals(env)
	if len(locals) == 0 {
		io.WriteString(w, formatter.Text("  (no locals)\n")) //nolint:errcheck
		return
	}
	for _, b := range locals {
		if formatter.Exhausted() {
			break
		}
		io.WriteString(w, formatter.Text("  "))                                               //nolint:errcheck
		io.WriteString(w, formatter.Text(b.Name))                                             //nolint:errcheck
		io.WriteString(w, formatter.Text(strings.Repeat(" ", max(0, 20-len(b.Name))), " = ")) //nolint:errcheck
		io.WriteString(w, formatter.Format(b.Value))                                          //nolint:errcheck
		io.WriteString(w, formatter.Text("\n"))                                               //nolint:errcheck
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
			line += "  if " + bp.Condition
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
		_ = filepath.Walk(sourceRoot, func(path string, info os.FileInfo, err error) error {
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
