// Copyright © 2024 The ELPS authors

package analysis

import (
	"path/filepath"
	"strings"
)

// MatchesExclude returns true if the given path matches any of the exclude
// patterns. Each pattern is matched against the full path and each path
// component (directories + filename), using filepath.Match semantics.
func MatchesExclude(path string, patterns []string) bool {
	for _, pattern := range patterns {
		// Match against full path
		if matched, _ := filepath.Match(pattern, path); matched {
			return true
		}
		// Match against each path component (directories + filename).
		// splitPath includes the base name, so patterns like
		// "shirocore.lisp" or "build" match anywhere in the path.
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
