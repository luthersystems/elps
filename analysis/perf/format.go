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
	}
}

// FormatJSON writes issues as a JSON array.
func FormatJSON(w io.Writer, issues []Issue) error {
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(issues)
}

// jsonIssue is the JSON-serializable form of Issue.
type jsonIssue struct {
	Rule     RuleID   `json:"rule"`
	Severity string   `json:"severity"`
	Message  string   `json:"message"`
	Function string   `json:"function"`
	File     string   `json:"file"`
	Line     int      `json:"line,omitempty"`
	Col      int      `json:"col,omitempty"`
	Details  []string `json:"details,omitempty"`
}

// MarshalJSON implements json.Marshaler for Issue.
func (issue Issue) MarshalJSON() ([]byte, error) {
	ji := jsonIssue{
		Rule:     issue.Rule,
		Severity: issue.Severity.String(),
		Message:  issue.Message,
		Function: issue.Function,
		File:     issue.File,
		Details:  issue.Details,
	}
	if issue.Source != nil {
		ji.Line = issue.Source.Line
		ji.Col = issue.Source.Col
	}
	return json.Marshal(ji)
}
