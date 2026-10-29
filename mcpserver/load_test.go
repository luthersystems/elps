// Copyright © 2026 The ELPS authors

package mcpserver

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// mcpMethodSource defines a record type and adds a method to it at the top
// level, as a program's file would.
const mcpMethodSource = `(set 'pt (s:make-record "pt" (s:has-key "x" s:int)))
(s:defmethod pt get-x (o) (o 'x))
`

// TestEvalTool_FormsRunAsTheLoad pins that the eval tool runs the forms as
// the program's load, where s:defmethod works (luthersystems/elps#908).
func TestEvalTool_FormsRunAsTheLoad(t *testing.T) {
	srv := New()
	_, resp, err := srv.service.evalTool(t.Context(), nil, EvalInput{
		Expression: mcpMethodSource + "((pt :x 3) 'get-x)",
	})
	require.NoError(t, err)
	assert.Empty(t, resp.Error)
	assert.Equal(t, "3", resp.Value)
}

// TestTestTool_TopLevelRunsAsTheLoad pins that the test tool loads the
// file's top-level forms as its load and runs each test body after it, as
// elpstest.Runner does: s:defmethod works at the top level and raises
// record-error in a test body (luthersystems/elps#908).
func TestTestTool_TopLevelRunsAsTheLoad(t *testing.T) {
	srv := New()
	content := mcpMethodSource + `(test "top-level method" (assert-equal 3 ((pt :x 3) 'get-x)))
(test "late method" (assert-equal 'record-error
  (handler-bind ((record-error (lambda (c &rest _) c))) (s:defmethod pt late (o) 1))))
`
	_, resp, err := srv.service.testTool(t.Context(), nil, TestInput{Content: &content})
	require.NoError(t, err)
	require.Equal(t, 2, resp.Total, "%+v", resp.Tests)
	assert.Equal(t, 2, resp.Passed, "%+v", resp.Tests)
}
