// Copyright © 2026 The ELPS authors

package cmd

import (
	"errors"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestAnalysisEnv checks the env and registry each choice of options
// gives the lint, lsp and mcp commands (luthersystems/elps#952): an env
// whenever one can expand workspace macros, and the embedder's own env or
// registry when it passed one.
func TestAnalysisEnv(t *testing.T) {
	built := 0
	docEnv, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	newDocEnv := func() (*lisp.LEnv, error) { built++; return docEnv, nil }

	embedderEnv, err := lisplib.NewDocEnv()
	require.NoError(t, err)
	cfg := cmdConfig{env: embedderEnv, newDocEnv: newDocEnv}
	setup, err := cfg.analysisEnv()
	require.NoError(t, err)
	assert.Same(t, embedderEnv, setup.env, "WithEnv: the embedder's env")
	assert.Same(t, embedderEnv.Runtime.Registry, setup.registry)

	cfg = cmdConfig{registry: embedderEnv.Runtime.Registry, newDocEnv: newDocEnv}
	setup, err = cfg.analysisEnv()
	require.NoError(t, err)
	assert.Nil(t, setup.env, "WithRegistry alone: no env, a registry cannot expand a macro")
	assert.Same(t, embedderEnv.Runtime.Registry, setup.registry)
	assert.Zero(t, built, "no stdlib env is built when the embedder passed one of its own")

	cfg = cmdConfig{newDocEnv: newDocEnv}
	setup, err = cfg.analysisEnv()
	require.NoError(t, err)
	assert.Same(t, docEnv, setup.env, "no options: a new stdlib env, so plain elps expands macros")
	assert.Same(t, docEnv.Runtime.Registry, setup.registry)

	boom := errors.New("boom")
	cfg = cmdConfig{newDocEnv: func() (*lisp.LEnv, error) { return nil, boom }}
	_, err = cfg.analysisEnv()
	require.ErrorIs(t, err, boom)
}
