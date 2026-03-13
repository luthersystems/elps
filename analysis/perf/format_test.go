// Copyright © 2024 The ELPS authors

package perf

import (
	"bytes"
	"encoding/json"
	"testing"

	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFormatText(t *testing.T) {
	issues := []Issue{
		{
			Rule:     PERF003,
			Severity: SeverityWarning,
			Message:  `expensive call "db-put" inside loop (depth 1)`,
			Function: "process",
			Source:   &token.Location{File: "test.lisp", Line: 5, Col: 3},
			File:     "test.lisp",
		},
	}

	var buf bytes.Buffer
	FormatText(&buf, issues)
	output := buf.String()
	// Verify format: "location: severity: message [RULE]"
	assert.Contains(t, output, `test.lisp:5:3: warning: expensive call "db-put" inside loop (depth 1) [PERF003]`)
}

func TestFormatJSON(t *testing.T) {
	issues := []Issue{
		{
			Rule:     PERF004,
			Severity: SeverityWarning,
			Message:  "recursive cycle: ping -> pong",
			Function: "ping",
			Source:   &token.Location{File: "test.lisp", Line: 1, Col: 1},
			File:     "test.lisp",
			Details:  []string{"ping", "pong"},
		},
	}

	var buf bytes.Buffer
	err := FormatJSON(&buf, issues)
	require.NoError(t, err)

	var parsed []jsonIssue
	require.NoError(t, json.Unmarshal(buf.Bytes(), &parsed))
	require.Len(t, parsed, 1)
	assert.Equal(t, string(PERF004), string(parsed[0].Rule))
	assert.Equal(t, "warning", parsed[0].Severity)
	assert.Equal(t, "recursive cycle: ping -> pong", parsed[0].Message)
	assert.Equal(t, "ping", parsed[0].Function)
	assert.Equal(t, "test.lisp", parsed[0].File)
	assert.Equal(t, 1, parsed[0].Line)
	assert.Equal(t, 1, parsed[0].Col)
	assert.Equal(t, []string{"ping", "pong"}, parsed[0].Details)
}

func TestFormatJSON_EmptyCollections(t *testing.T) {
	t.Parallel()
	outputs := make([]string, 0, 2)
	for _, issues := range [][]Issue{nil, {}} {
		var buf bytes.Buffer
		err := FormatJSON(&buf, issues)
		require.NoError(t, err)
		outputs = append(outputs, buf.String())

		var parsed any
		require.NoError(t, json.Unmarshal(buf.Bytes(), &parsed))
		slice, ok := parsed.([]any)
		require.True(t, ok, "empty JSON output should decode as an array")
		assert.Len(t, slice, 0)
	}
	require.Len(t, outputs, 2)
	assert.Equal(t, outputs[0], outputs[1])
}
