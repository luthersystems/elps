// Copyright © 2024 The ELPS authors

package cmd

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
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
	for _, pattern := range patterns {
		// Match against full path
		if matched, _ := filepath.Match(pattern, path); matched {
			return true
		}
		// Match against base name (e.g. --exclude='shirocore.lisp')
		if matched, _ := filepath.Match(pattern, filepath.Base(path)); matched {
			return true
		}
		// Match against each path component for directory patterns
		// (e.g. --exclude='build' matches any/build/file.lisp)
		for _, component := range splitPath(path) {
			if matched, _ := filepath.Match(pattern, component); matched {
				return true
			}
		}
	}
	return false
}

// splitPath returns all directory components and the filename of a path.
func splitPath(path string) []string {
	var components []string
	dir, file := filepath.Split(path)
	if file != "" {
		components = append(components, file)
	}
	for dir != "" && dir != "/" && dir != "." {
		dir = strings.TrimRight(dir, string(filepath.Separator))
		parent, base := filepath.Split(dir)
		if base != "" {
			components = append(components, base)
		}
		if parent == dir {
			break
		}
		dir = parent
	}
	return components
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
