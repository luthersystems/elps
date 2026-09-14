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
	if !filepath.IsAbs(loc) && ctx.Location() != "" {
		loc = filepath.Join(filepath.Dir(ctx.Location()), loc)
	}
	if filepath.IsAbs(loc) {
		rel, err := filepath.Rel(lib.path, loc)
		if err != nil {
			return "", "", nil, err
		}
		loc = rel
	}
	// Never resolve a host pathname and then open it: directory components
	// can change between those operations. Root.ReadFile confines the open
	// even when a component is concurrently replaced by an escaping symlink.
	data, err := lib.root.ReadFile(loc)
	if err != nil {
		return "", "", nil, fmt.Errorf("cannot load %s within root directory %s: %w", loc, lib.path, err)
	}
	loc = lib.sourceLocation(loc)
	return filepath.Base(loc), filepath.Join(lib.path, loc), data, nil
}

// sourceLocation preserves target-relative nested loads through internal
// symlinks. This is source metadata only, after the confined read; it is never
// used to authorize an open. Concurrent changes may make the metadata stale,
// but every subsequent load still goes through the same root handle.
func (lib *Library) sourceLocation(loc string) string {
	original := loc
	for links := 0; links < 40; links++ {
		parts := strings.Split(filepath.Clean(loc), string(filepath.Separator))
		changed := false
		for i := range parts {
			prefix := filepath.Join(parts[:i+1]...)
			target, err := lib.root.Readlink(prefix)
			if err != nil {
				continue // Ordinary files are not symlinks; stale metadata is harmless.
			}
			if filepath.IsAbs(target) {
				return original // The link changed after the read.
			}
			loc = filepath.Join(filepath.Dir(prefix), target, filepath.Join(parts[i+1:]...))
			if !filepath.IsLocal(loc) {
				return original
			}
			changed = true
			break
		}
		if !changed {
			return loc
		}
	}
	return original // Bound work if links change or form a cycle after the read.
}
