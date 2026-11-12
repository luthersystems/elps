// Copyright © 2026 The ELPS authors

package mcpserver

import (
	"path/filepath"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestWorkspaceBuildBoundsPreamble checks that the index build of a root
// stops a preamble form that does not end, and still loads the macros
// after it. The build holds buildMu and the expander lock while it
// evaluates the preamble, so an unbounded form would stop every request
// for the root and every macro expansion of the server.
func TestWorkspaceBuildBoundsPreamble(t *testing.T) {
	root := t.TempDir()
	writeTestFile(t, filepath.Join(root, "macros.lisp"), "(in-package 'demo)\n"+
		"(set 'x (dotimes (i 4611686018427387904) ()))\n"+
		"(defmacro after () 'a)\n")

	srv := New(WithWorkspaceRoot(root))
	require.NotNil(t, srv.service.expander, "the server has an env")
	type result struct {
		state *workspaceState
		err   error
	}
	done := make(chan result, 1)
	go func() {
		state, err := srv.service.buildWorkspaceState(root, "fingerprint", time.Now())
		done <- result{state, err}
	}()
	var got result
	select {
	case got = <-done:
	case <-time.After(30 * time.Second):
		t.Fatal("the workspace build did not end")
	}
	require.NoError(t, got.err)
	expanded := srv.service.expander.ExpandMacro(lisp.SExpr([]*lisp.LVal{lisp.Symbol("after")}), "demo")
	assert.NotNil(t, expanded, "the macro after the stopped form is loaded")
}
