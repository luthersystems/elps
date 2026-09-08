// Copyright © 2026 The ELPS authors

// Package hook breaks the import cycle between lisp and internal/jsonraw.
// Only lisp (writer) and jsonraw (reader) should use this package.
package hook

// Wrap holds a func(map[string]any) *lisp.LVal, injected during lisp's init.
var Wrap any
