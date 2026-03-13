// Copyright © 2024 The ELPS authors

package perf

import (
	"encoding/json"
	"fmt"
	"io"
)

// FormatText writes issues in a human-readable text format similar to
// go vet output.
func FormatText(w io.Writer, issues []Issue) {
	for _, issue := range issues {
		loc := "unknown"
		if issue.Source != nil && issue.Source.Line > 0 {
			loc = issue.Source.String()
		} else if issue.File != "" {
			loc = issue.File
		}
		fmt.Fprintf(w, "%s: %s: %s [%s]\n", loc, issue.Severity, issue.Message, issue.Rule) //nolint:errcheck
		for _, detail := range issue.Details {
			fmt.Fprintf(w, "  = detail: %s\n", detail) //nolint:errcheck
		}
		for _, entry := range issue.Trace {
			traceLoc := "unknown"
			if entry.Source != nil && entry.Source.Line > 0 {
				traceLoc = entry.Source.String()
			}
			fmt.Fprintf(w, "  = trace: %s at %s — %s\n", entry.Function, traceLoc, entry.Note) //nolint:errcheck
		}
	}
}

// FormatJSON writes issues as a JSON array.
func FormatJSON(w io.Writer, issues []Issue) error {
	if issues == nil {
		issues = []Issue{}
	}
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(issues)
}

// jsonTraceEntry is the JSON-serializable form of TraceEntry.
type jsonTraceEntry struct {
	Function string `json:"function"`
	File     string `json:"file,omitempty"`
	Line     int    `json:"line,omitempty"`
	Col      int    `json:"col,omitempty"`
	Note     string `json:"note"`
}

// jsonIssue is the JSON-serializable form of Issue.
type jsonIssue struct {
	Rule        RuleID           `json:"rule"`
	Severity    string           `json:"severity"`
	Message     string           `json:"message"`
	Function    string           `json:"function"`
	File        string           `json:"file"`
	Line        int              `json:"line,omitempty"`
	Col         int              `json:"col,omitempty"`
	Details     []string         `json:"details,omitempty"`
	Fingerprint string           `json:"fingerprint"`
	Trace       []jsonTraceEntry `json:"trace,omitempty"`
}

// MarshalJSON implements json.Marshaler for Issue.
func (issue Issue) MarshalJSON() ([]byte, error) {
	ji := jsonIssue{
		Rule:        issue.Rule,
		Severity:    issue.Severity.String(),
		Message:     issue.Message,
		Function:    issue.Function,
		File:        issue.File,
		Details:     issue.Details,
		Fingerprint: issue.Fingerprint,
	}
	if issue.Source != nil {
		ji.Line = issue.Source.Line
		ji.Col = issue.Source.Col
	}
	for _, entry := range issue.Trace {
		jt := jsonTraceEntry{
			Function: entry.Function,
			Note:     entry.Note,
		}
		if entry.Source != nil {
			jt.File = entry.Source.File
			jt.Line = entry.Source.Line
			jt.Col = entry.Source.Col
		}
		ji.Trace = append(ji.Trace, jt)
	}
	return json.Marshal(ji)
}
