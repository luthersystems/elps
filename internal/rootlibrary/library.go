// Copyright © 2026 The ELPS authors

// Package rootlibrary provides the confined source loader shared by the CLI
// execution commands and the REPL.
package rootlibrary

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// Library reads sources through an open directory handle. Close it after all
// evaluations using the library have finished.
type Library struct {
	root *os.Root
	path string
}

// Open anchors a library to dir, or the working directory when dir is empty.
func Open(dir string) (*Library, error) {
	if dir == "" {
		dir = "."
	}
	abs, err := filepath.Abs(dir)
	if err != nil {
		return nil, fmt.Errorf("cannot resolve root directory: %w", err)
	}
	root, err := os.OpenRoot(abs)
	if err != nil {
		return nil, fmt.Errorf("cannot open root directory: %w", err)
	}
	return &Library{root: root, path: abs}, nil
}

// Close releases the root directory handle.
func (lib *Library) Close() error { return lib.root.Close() }

// LoadSource anchors initial loads at the root and nested loads at the calling
// source's directory. The root handle enforces confinement at read time.
func (lib *Library) LoadSource(ctx lisp.SourceContext, loc string) (string, string, []byte, error) {
	requested := loc
	if !filepath.IsAbs(loc) && ctx.Location() != "" {
		// Join without cleaning the unresolved suffix: link/.. must follow
		// link before moving to its target's parent directory.
		loc = filepath.Dir(ctx.Location()) + string(filepath.Separator) + loc
	}
	loc, err := RelativePath(lib.path, loc)
	if err != nil {
		return "", "", nil, lib.loadError(requested)
	}
	// Never resolve a host pathname and then open it: directory components
	// can change between those operations. Root.ReadFile confines the open
	// even when a component is concurrently replaced by an escaping symlink.
	data, err := lib.root.ReadFile(loc)
	if err != nil {
		return "", "", nil, lib.loadError(requested)
	}
	loc = lib.sourceLocation(loc)
	return filepath.Base(loc), filepath.Join(lib.path, loc), data, nil
}

func (lib *Library) loadError(requested string) error {
	// Filesystem errors and expanded contexts can contain paths the caller
	// did not supply. Expose only the original request and configured root.
	return fmt.Errorf("cannot load %q within root directory %q", requested, lib.path)
}

// RelativePath strips an absolute root prefix without cleaning the unresolved
// request. Relative requests are passed through for os.Root to check at open time.
func RelativePath(root, loc string) (string, error) {
	if !filepath.IsAbs(loc) {
		return loc, nil
	}
	prefix := strings.TrimSuffix(filepath.Clean(root), string(filepath.Separator)) + string(filepath.Separator)
	if strings.HasPrefix(loc, prefix) {
		return strings.TrimPrefix(loc, prefix), nil
	}
	if loc == filepath.Clean(root) {
		return ".", nil
	}
	return "", fmt.Errorf("cannot load %q within root directory %q", loc, root)
}

// sourceLocation preserves target-relative nested loads through internal
// symlinks. This is source metadata only, after the confined read; it is never
// used to authorize an open. Concurrent changes may make the metadata stale,
// but every subsequent load still goes through the same root handle.
func (lib *Library) sourceLocation(loc string) string {
	original := loc
	parts := strings.Split(filepath.FromSlash(loc), string(filepath.Separator))
	var resolved []string
	links := 0
	for len(parts) > 0 {
		part := parts[0]
		parts = parts[1:]
		switch part {
		case "", ".":
			continue
		case "..":
			if len(resolved) == 0 {
				return original // The path changed after the confined read.
			}
			resolved = resolved[:len(resolved)-1]
			continue
		}
		prefix := filepath.Join(strings.Join(resolved, string(filepath.Separator)), part)
		target, err := lib.root.Readlink(prefix)
		if err != nil {
			resolved = append(resolved, part) // Ordinary files are not symlinks.
			continue
		}
		links++
		if filepath.IsAbs(target) || links > 40 {
			return original // Bound work if links change after the read.
		}
		// Expand the target before consuming any remaining components,
		// including .. in either the target or the original suffix.
		parts = append(strings.Split(filepath.FromSlash(target), string(filepath.Separator)), parts...)
	}
	return filepath.Join(resolved...)
}
