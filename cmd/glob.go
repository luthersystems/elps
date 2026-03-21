// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/luthersystems/elps/analysis"
)

// expandArgs expands arguments, resolving patterns ending with "/..." to all
// .lisp files found recursively under the given directory. Non-pattern
// arguments pass through unchanged. Files matching any exclude pattern are
// omitted from the result.
func expandArgs(args []string, excludes []string) ([]string, error) {
	var out []string
	for _, arg := range args {
		if dir, ok := strings.CutSuffix(arg, "/..."); ok {
			if dir == "" {
				dir = "."
			}
			files, err := findLispFiles(dir)
			if err != nil {
				return nil, fmt.Errorf("expanding %s: %w", arg, err)
			}
			out = append(out, files...)
		} else {
			out = append(out, arg)
		}
	}
	if len(excludes) > 0 {
		out = filterExcludes(out, excludes)
	}
	return out, nil
}

// filterExcludes removes paths matching any of the given glob patterns.
// Each pattern is matched against both the full path and the base name,
// so --exclude='shirocore.lisp' matches any file with that name and
// --exclude='**/build/**' matches paths containing a build directory.
func filterExcludes(paths []string, excludes []string) []string {
	var filtered []string
	for _, p := range paths {
		if matchesAny(p, excludes) {
			continue
		}
		filtered = append(filtered, p)
	}
	return filtered
}

func matchesAny(path string, patterns []string) bool {
	return analysis.MatchesExclude(path, patterns)
}

func findLispFiles(root string) ([]string, error) {
	var files []string
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		if filepath.Ext(path) == ".lisp" {
			files = append(files, path)
		}
		return nil
	})
	return files, err
}
