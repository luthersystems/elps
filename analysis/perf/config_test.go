// Copyright © 2024 The ELPS authors

package perf

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDefaultConfig(t *testing.T) {
	cfg := DefaultConfig()
	assert.Equal(t, 50, cfg.ExpensiveCost)
	assert.Equal(t, 20, cfg.LoopMultiplier)
	assert.Equal(t, 100000, cfg.MaxScore)
	assert.Equal(t, 2, cfg.MaxAcceptableOrder)
	assert.Equal(t, 3, cfg.ScalingErrorThreshold)
	assert.Equal(t, 5, cfg.MaxRecursionOrder)
	assert.Contains(t, cfg.LoopKeywords, "dolist")
	assert.Contains(t, cfg.LoopKeywords, "dotimes")
	assert.Contains(t, cfg.ExpensiveFunctions, "db-*")
}

func TestLoadConfigFile_NotExist(t *testing.T) {
	cfg, err := LoadConfigFile("/nonexistent/path.yaml")
	require.NoError(t, err)
	assert.Equal(t, DefaultConfig(), cfg)
}

func TestLoadConfigFile_CustomValues(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, ".elps-analyze.yaml")
	content := `
expensive_functions:
  - "statedb-*"
  - "http-*"
expensive_cost: 100
loop_multiplier: 10
max_score: 50000
`
	require.NoError(t, os.WriteFile(path, []byte(content), 0600))

	cfg, err := LoadConfigFile(path)
	require.NoError(t, err)
	assert.Equal(t, 100, cfg.ExpensiveCost)
	assert.Equal(t, 10, cfg.LoopMultiplier)
	assert.Equal(t, 50000, cfg.MaxScore)
	assert.Equal(t, []string{"statedb-*", "http-*"}, cfg.ExpensiveFunctions)
	// Defaults preserved for unset fields
	assert.Equal(t, 2, cfg.MaxAcceptableOrder)
}

func TestFindConfigFile(t *testing.T) {
	dir := t.TempDir()
	sub := filepath.Join(dir, "a", "b", "c")
	require.NoError(t, os.MkdirAll(sub, 0750))

	// No config anywhere
	assert.Empty(t, FindConfigFile(sub))

	// Place config at root
	cfgPath := filepath.Join(dir, ".elps-analyze.yaml")
	require.NoError(t, os.WriteFile(cfgPath, []byte("max_score: 1"), 0600))

	assert.Equal(t, cfgPath, FindConfigFile(sub))
}
