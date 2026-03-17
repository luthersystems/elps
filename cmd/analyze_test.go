// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"encoding/json"
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/analysis/perf"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestAnalyzeExcludePatterns_DefaultExcludesTests(t *testing.T) {
	excludes := analyzeExcludePatterns(perf.DefaultConfig(), nil, false)
	assert.Contains(t, excludes, "*_test.lisp")
}

func TestAnalyzeExcludePatterns_IncludeTestsFlagOverridesDefault(t *testing.T) {
	excludes := analyzeExcludePatterns(perf.DefaultConfig(), nil, true)
	assert.NotContains(t, excludes, "*_test.lisp")
}

func TestAnalyzeExcludePatterns_ConfigIncludeTestsOverridesDefault(t *testing.T) {
	cfg := perf.DefaultConfig()
	cfg.IncludeTests = true
	excludes := analyzeExcludePatterns(cfg, nil, false)
	assert.NotContains(t, excludes, "*_test.lisp")
}

func TestAnalyzeExcludePatterns_IncludeTestsPreservesExplicitConfigTestExclude(t *testing.T) {
	cfg := perf.DefaultConfig()
	cfg.ExcludeFiles = []string{"*_test.lisp", "build/**"}
	excludes := analyzeExcludePatterns(cfg, nil, true)
	assert.Contains(t, excludes, "*_test.lisp")
	assert.Contains(t, excludes, "build/**")
}

func TestAnalyzeExcludePatterns_MergesConfigAndCLIExcludes(t *testing.T) {
	cfg := perf.DefaultConfig()
	cfg.ExcludeFiles = []string{"build/**", "*_generated.lisp"}
	excludes := analyzeExcludePatterns(cfg, []string{"vendor/**"}, false)
	assert.Contains(t, excludes, "build/**")
	assert.Contains(t, excludes, "*_generated.lisp")
	assert.Contains(t, excludes, "vendor/**")
	assert.Contains(t, excludes, "*_test.lisp")
}

func TestAnalyzeCommand_DefaultFlags(t *testing.T) {
	cmd := AnalyzeCommand()
	for _, name := range []string{"json", "config", "exclude", "include-tests", "rules", "fail-on", "top"} {
		assert.NotNil(t, cmd.Flags().Lookup(name), "missing flag: %s", name)
	}
}

func TestRunAnalyze_DefaultExcludesTestFiles(t *testing.T) {
	dir := t.TempDir()
	writeAnalyzeFixture(t, filepath.Join(dir, "main.lisp"), `(defun greet (name) (concat 'string "Hello, " name))`)
	writeAnalyzeFixture(t, filepath.Join(dir, "feature_test.lisp"), `
(defun process-batch (items)
  (map 'list (lambda (item) (db-put item)) items))
`)
	configPath := writeAnalyzeFixture(t, filepath.Join(dir, ".elps-analyze.yaml"), "{}\n")

	var stdout bytes.Buffer
	var stderr bytes.Buffer
	code, err := runAnalyze([]string{filepath.Join(dir, "...")}, &stdout, &stderr, analyzeRunConfig{
		jsonOutput: true,
		configFile: configPath,
		failOn:     "error",
	})
	require.NoError(t, err)
	assert.Equal(t, 0, code)
	assert.Equal(t, "", stdout.String())
	assert.Equal(t, "", stderr.String())
}

func TestRunAnalyze_IncludeTestsFlagIncludesTestFiles(t *testing.T) {
	dir := t.TempDir()
	writeAnalyzeFixture(t, filepath.Join(dir, "main.lisp"), `(defun greet (name) (concat 'string "Hello, " name))`)
	writeAnalyzeFixture(t, filepath.Join(dir, "feature_test.lisp"), `
(defun process-batch (items)
  (map 'list (lambda (item) (db-put item)) items))
`)
	configPath := writeAnalyzeFixture(t, filepath.Join(dir, ".elps-analyze.yaml"), "{}\n")

	var stdout bytes.Buffer
	var stderr bytes.Buffer
	code, err := runAnalyze([]string{filepath.Join(dir, "...")}, &stdout, &stderr, analyzeRunConfig{
		jsonOutput:   true,
		configFile:   configPath,
		includeTests: true,
		failOn:       "error",
	})
	require.NoError(t, err)
	assert.Equal(t, 0, code)
	assert.Empty(t, stderr.String())

	var issues []map[string]any
	require.NoError(t, json.Unmarshal(stdout.Bytes(), &issues))
	require.NotEmpty(t, issues)
	assert.Equal(t, "feature_test.lisp", filepath.Base(issues[0]["file"].(string)))
}

func TestRunAnalyze_ConfigCanIncludeTests(t *testing.T) {
	dir := t.TempDir()
	writeAnalyzeFixture(t, filepath.Join(dir, "feature_test.lisp"), `
(defun process-batch (items)
  (map 'list (lambda (item) (db-put item)) items))
`)
	configPath := writeAnalyzeFixture(t, filepath.Join(dir, ".elps-analyze.yaml"), "include_tests: true\n")

	var stdout bytes.Buffer
	var stderr bytes.Buffer
	code, err := runAnalyze([]string{filepath.Join(dir, "...")}, &stdout, &stderr, analyzeRunConfig{
		jsonOutput: true,
		configFile: configPath,
		failOn:     "error",
	})
	require.NoError(t, err)
	assert.Equal(t, 0, code)
	assert.Empty(t, stderr.String())

	var issues []map[string]any
	require.NoError(t, json.Unmarshal(stdout.Bytes(), &issues))
	require.NotEmpty(t, issues)
	assert.Equal(t, "feature_test.lisp", filepath.Base(issues[0]["file"].(string)))
}

func TestRunAnalyze_IncludeTestsPreservesExplicitConfigTestExclude(t *testing.T) {
	dir := t.TempDir()
	writeAnalyzeFixture(t, filepath.Join(dir, "feature_test.lisp"), `
(defun process-batch (items)
  (map 'list (lambda (item) (db-put item)) items))
`)
	configPath := writeAnalyzeFixture(t, filepath.Join(dir, ".elps-analyze.yaml"), "exclude_files:\n  - \"*_test.lisp\"\n")

	var stdout bytes.Buffer
	var stderr bytes.Buffer
	code, err := runAnalyze([]string{filepath.Join(dir, "...")}, &stdout, &stderr, analyzeRunConfig{
		jsonOutput:   true,
		configFile:   configPath,
		includeTests: true,
		failOn:       "error",
	})
	require.Error(t, err)
	assert.Equal(t, 2, code)
	assert.Contains(t, err.Error(), "no .lisp files found")
	assert.Empty(t, stdout.String())
	assert.Empty(t, stderr.String())
}

func TestRunAnalyze_IncludeTestsStillHonorsSpecificTestExcludes(t *testing.T) {
	dir := t.TempDir()
	writeAnalyzeFixture(t, filepath.Join(dir, "feature_test.lisp"), `
(defun process-batch (items)
  (map 'list (lambda (item) (db-put item)) items))
`)
	writeAnalyzeFixture(t, filepath.Join(dir, "generated_case_test.lisp"), `
(defun generated-batch (items)
  (map 'list (lambda (item) (db-put item)) items))
`)
	configPath := writeAnalyzeFixture(t, filepath.Join(dir, ".elps-analyze.yaml"), "include_tests: true\nexclude_files:\n  - \"generated_*_test.lisp\"\n")

	var stdout bytes.Buffer
	var stderr bytes.Buffer
	code, err := runAnalyze([]string{filepath.Join(dir, "...")}, &stdout, &stderr, analyzeRunConfig{
		jsonOutput: true,
		configFile: configPath,
		failOn:     "error",
	})
	require.NoError(t, err)
	assert.Equal(t, 0, code)
	assert.Empty(t, stderr.String())

	var issues []map[string]any
	require.NoError(t, json.Unmarshal(stdout.Bytes(), &issues))
	require.NotEmpty(t, issues)
	assert.Equal(t, "feature_test.lisp", filepath.Base(issues[0]["file"].(string)))
}

func TestRunAnalyze_NoFilesRemainAfterDefaultTestExclusion(t *testing.T) {
	dir := t.TempDir()
	writeAnalyzeFixture(t, filepath.Join(dir, "feature_test.lisp"), `
(defun process-batch (items)
  (map 'list (lambda (item) (db-put item)) items))
`)
	configPath := writeAnalyzeFixture(t, filepath.Join(dir, ".elps-analyze.yaml"), "{}\n")

	var stdout bytes.Buffer
	var stderr bytes.Buffer
	code, err := runAnalyze([]string{filepath.Join(dir, "...")}, &stdout, &stderr, analyzeRunConfig{
		configFile: configPath,
	})
	require.Error(t, err)
	assert.Equal(t, 2, code)
	assert.Contains(t, err.Error(), "no .lisp files found")
	assert.Empty(t, stdout.String())
	assert.Empty(t, stderr.String())
}

func writeAnalyzeFixture(t *testing.T, path, content string) string {
	t.Helper()
	require.NoError(t, os.WriteFile(path, []byte(content), 0o600))
	return path
}
