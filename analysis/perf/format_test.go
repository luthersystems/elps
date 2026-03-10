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
			File:    "test.lisp",
		},
	}

	var buf bytes.Buffer
	FormatText(&buf, issues)
	output := buf.String()
	assert.Contains(t, output, "PERF003")
	assert.Contains(t, output, "db-put")
	assert.Contains(t, output, "test.lisp:5:3")
}

func TestFormatJSON(t *testing.T) {
	issues := []Issue{
		{
			Rule:     PERF004,
			Severity: SeverityWarning,
			Message:  "recursive cycle: ping -> pong",
			Function: "ping",
			Source:   &token.Location{File: "test.lisp", Line: 1, Col: 1},
			File:    "test.lisp",
			Details: []string{"ping", "pong"},
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
}
