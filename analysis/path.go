package analysis

import (
	"os"
	"path/filepath"
)

// NormalizePath returns a cleaned absolute path and resolves symlinks when
// possible. If the path cannot be resolved, it falls back to a cleaned
// absolute-or-original path for stable equality checks.
func NormalizePath(path string) string {
	if path == "" {
		return ""
	}

	cleaned := filepath.Clean(path)
	if abs, err := filepath.Abs(cleaned); err == nil {
		cleaned = abs
	}
	if resolved, err := filepath.EvalSymlinks(cleaned); err == nil {
		return filepath.Clean(resolved)
	}
	if resolved, ok := normalizeViaExistingParent(cleaned); ok {
		return resolved
	}
	return cleaned
}

func normalizeViaExistingParent(path string) (string, bool) {
	dir := path
	var suffix []string

	for {
		if _, err := os.Stat(dir); err == nil {
			resolvedDir, err := filepath.EvalSymlinks(dir)
			if err != nil {
				return "", false
			}
			if len(suffix) == 0 {
				return filepath.Clean(resolvedDir), true
			}
			parts := append([]string{resolvedDir}, suffix...)
			return filepath.Clean(filepath.Join(parts...)), true
		}
		if dir == "" || dir == "." || dir == string(filepath.Separator) {
			break
		}
		suffix = append([]string{filepath.Base(dir)}, suffix...)
		next := filepath.Dir(dir)
		if next == dir {
			break
		}
		dir = next
	}

	return "", false
}
