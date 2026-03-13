// Copyright © 2024 The ELPS authors

package perf

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSuppressed(t *testing.T) {
	src := `
;; elps-analyze-disable
(defun noisy (items)
  (map 'list (lambda (item) (db-put item)) items))

(defun normal (items)
  (map 'list (lambda (item) (db-put item)) items))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 2)

	var noisy, normal *FunctionSummary
	for _, s := range summaries {
		switch s.Name {
		case "noisy":
			noisy = s
		case "normal":
			normal = s
		}
	}
	require.NotNil(t, noisy)
	require.NotNil(t, normal)

	assert.True(t, noisy.SuppressAll)
	assert.Empty(t, noisy.SuppressedRules)
	assert.False(t, normal.SuppressAll)
	assert.Empty(t, normal.SuppressedRules)
}

func TestSuppressed_SpecificRules(t *testing.T) {
	src := `
;; elps-analyze-disable: PERF002, PERF004
(defun noisy (items)
  (map 'list (lambda (item) (db-put item)) items))

;; elps-analyze-disable: PERF999, PERF003
(defmacro noisy-macro (items)
  (map 'list (lambda (item) (db-put item)) items))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 2)

	assert.False(t, summaries[0].SuppressAll)
	assert.True(t, summaries[0].SuppressedRules[PERF002])
	assert.True(t, summaries[0].SuppressedRules[PERF004])
	assert.False(t, summaries[0].SuppressedRules[PERF003])

	assert.False(t, summaries[1].SuppressAll)
	assert.True(t, summaries[1].SuppressedRules[PERF003])
	assert.False(t, summaries[1].SuppressedRules[PERF004])
}
