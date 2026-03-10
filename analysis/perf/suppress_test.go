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

	assert.True(t, noisy.Suppressed)
	assert.False(t, normal.Suppressed)
}
