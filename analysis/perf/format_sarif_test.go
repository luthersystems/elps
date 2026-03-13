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

func TestFormatSARIF_ValidSchema(t *testing.T) {
	t.Parallel()
	issues := []Issue{
		{
			Rule:        PERF001,
			Severity:    SeverityWarning,
			Message:     "hot path: score 5000 exceeds threshold 1000",
			Function:    "process-batch",
			Source:      &token.Location{File: "app.lisp", Line: 10, Col: 1},
			File:        "app.lisp",
			Fingerprint: "abc123",
		},
	}

	var buf bytes.Buffer
	err := FormatSARIF(&buf, issues, "elps-perf", "0.1.0")
	require.NoError(t, err)

	var log sarifLog
	require.NoError(t, json.Unmarshal(buf.Bytes(), &log))
	assert.Equal(t, "2.1.0", log.Version)
	assert.Equal(t, "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json", log.Schema)
	require.Len(t, log.Runs, 1)
	assert.Equal(t, "elps-perf", log.Runs[0].Tool.Driver.Name)
	assert.Equal(t, "0.1.0", log.Runs[0].Tool.Driver.Version)
}

func TestFormatSARIF_CodeFlows(t *testing.T) {
	t.Parallel()
	issues := []Issue{
		{
			Rule:        PERF001,
			Severity:    SeverityWarning,
			Message:     "hot path",
			Function:    "process-batch",
			Source:      &token.Location{File: "app.lisp", Line: 10, Col: 1},
			File:        "app.lisp",
			Fingerprint: "abc123",
			Trace: []TraceEntry{
				{Function: "process-batch", Source: &token.Location{File: "app.lisp", Line: 10, Col: 1}, Note: "entry point"},
				{Function: "save-item", Source: &token.Location{File: "app.lisp", Line: 20, Col: 5}, Note: "calls save-item in loop (depth 1)"},
				{Function: "db-put", Source: &token.Location{File: "db.lisp", Line: 3, Col: 1}, Note: "expensive call"},
			},
		},
	}

	var buf bytes.Buffer
	err := FormatSARIF(&buf, issues, "elps-perf", "0.1.0")
	require.NoError(t, err)

	var log sarifLog
	require.NoError(t, json.Unmarshal(buf.Bytes(), &log))
	require.Len(t, log.Runs[0].Results, 1)

	result := log.Runs[0].Results[0]
	require.Len(t, result.CodeFlows, 1)
	require.Len(t, result.CodeFlows[0].ThreadFlows, 1)

	locs := result.CodeFlows[0].ThreadFlows[0].Locations
	require.Len(t, locs, 3)
	assert.Equal(t, "app.lisp", locs[0].Location.PhysicalLocation.ArtifactLocation.URI)
	assert.Equal(t, 10, locs[0].Location.PhysicalLocation.Region.StartLine)
	assert.Equal(t, "entry point", locs[0].Location.Message.Text)
	// Verify middle trace entry is correct too.
	assert.Equal(t, "app.lisp", locs[1].Location.PhysicalLocation.ArtifactLocation.URI)
	assert.Equal(t, 20, locs[1].Location.PhysicalLocation.Region.StartLine)
	assert.Equal(t, "calls save-item in loop (depth 1)", locs[1].Location.Message.Text)
	assert.Equal(t, "db.lisp", locs[2].Location.PhysicalLocation.ArtifactLocation.URI)
	assert.Equal(t, "expensive call", locs[2].Location.Message.Text)
}

func TestFormatSARIF_Fingerprints(t *testing.T) {
	t.Parallel()
	issues := []Issue{
		{
			Rule:        PERF003,
			Severity:    SeverityWarning,
			Message:     "expensive in loop",
			Function:    "do-stuff",
			File:        "test.lisp",
			Fingerprint: "deadbeef12345678",
		},
	}

	var buf bytes.Buffer
	err := FormatSARIF(&buf, issues, "elps-perf", "0.1.0")
	require.NoError(t, err)

	var log sarifLog
	require.NoError(t, json.Unmarshal(buf.Bytes(), &log))

	result := log.Runs[0].Results[0]
	assert.Equal(t, "deadbeef12345678", result.PartialFingerprints["primaryLocationLineHash"])

	// Source is nil but File is set — location URI should be populated, region nil.
	require.Len(t, result.Locations, 1)
	assert.Equal(t, "test.lisp", result.Locations[0].PhysicalLocation.ArtifactLocation.URI)
	assert.Nil(t, result.Locations[0].PhysicalLocation.Region)
}

func TestFormatSARIF_AllRules(t *testing.T) {
	t.Parallel()
	outputs := make([]string, 0, 2)
	for _, issues := range [][]Issue{nil, {}} {
		var buf bytes.Buffer
		err := FormatSARIF(&buf, issues, "elps-perf", "0.1.0")
		require.NoError(t, err)
		outputs = append(outputs, buf.String())

		var log sarifLog
		require.NoError(t, json.Unmarshal(buf.Bytes(), &log))

		rules := log.Runs[0].Tool.Driver.Rules
		require.Len(t, rules, 5)

		ruleIDs := make([]string, len(rules))
		for i, r := range rules {
			ruleIDs[i] = r.ID
		}
		assert.Equal(t, []string{"PERF001", "PERF002", "PERF003", "PERF004", "UNKNOWN001"}, ruleIDs)
		assert.Equal(t, "HotPath", rules[0].Name)
		assert.Equal(t, "DynamicDispatch", rules[4].Name)

		var raw map[string]any
		require.NoError(t, json.Unmarshal(buf.Bytes(), &raw))
		runs, ok := raw["runs"].([]any)
		require.True(t, ok)
		require.Len(t, runs, 1)
		run, ok := runs[0].(map[string]any)
		require.True(t, ok)
		results, ok := run["results"].([]any)
		require.True(t, ok, "empty SARIF results should decode as an array")
		assert.Len(t, results, 0)
	}
	require.Len(t, outputs, 2)
	assert.Equal(t, outputs[0], outputs[1])
}

func TestFormatSARIF_SeverityMapping(t *testing.T) {
	t.Parallel()
	issues := []Issue{
		{Rule: PERF001, Severity: SeverityError, Message: "err", Function: "f1", File: "a.lisp", Fingerprint: "a"},
		{Rule: PERF002, Severity: SeverityWarning, Message: "warn", Function: "f2", File: "b.lisp", Fingerprint: "b"},
		{Rule: UNKNOWN001, Severity: SeverityInfo, Message: "info", Function: "f3", File: "c.lisp", Fingerprint: "c"},
		{Rule: PERF003, Severity: Severity(99), Message: "unknown", Function: "f4", File: "d.lisp", Fingerprint: "d"},
	}

	var buf bytes.Buffer
	err := FormatSARIF(&buf, issues, "elps-perf", "0.1.0")
	require.NoError(t, err)

	var log sarifLog
	require.NoError(t, json.Unmarshal(buf.Bytes(), &log))
	require.Len(t, log.Runs[0].Results, 4)
	assert.Equal(t, "error", log.Runs[0].Results[0].Level)
	assert.Equal(t, "warning", log.Runs[0].Results[1].Level)
	assert.Equal(t, "note", log.Runs[0].Results[2].Level)
	assert.Equal(t, "none", log.Runs[0].Results[3].Level)

	// Verify ruleIndex maps to correct rule in the rules array.
	assert.Equal(t, 0, log.Runs[0].Results[0].RuleIndex) // PERF001 -> index 0
	assert.Equal(t, 1, log.Runs[0].Results[1].RuleIndex) // PERF002 -> index 1
	assert.Equal(t, 4, log.Runs[0].Results[2].RuleIndex) // UNKNOWN001 -> index 4
	assert.Equal(t, 2, log.Runs[0].Results[3].RuleIndex) // PERF003 -> index 2
}
