// Copyright © 2024 The ELPS authors

package perf

import (
	"encoding/json"
	"io"

	"github.com/luthersystems/elps/parser/token"
)

// FormatSARIF writes issues in SARIF v2.1.0 JSON format.
// The toolName and toolVersion parameters identify the analysis tool in the
// SARIF output (e.g., "elps-perf", "0.1.0").
func FormatSARIF(w io.Writer, issues []Issue, toolName, toolVersion string) error {
	rules := buildRuleDescriptors()
	ruleIndex := make(map[RuleID]int, len(rules))
	for i, r := range rules {
		ruleIndex[RuleID(r.ID)] = i
	}

	results := make([]sarifResult, 0, len(issues))
	for _, issue := range issues {
		r := sarifResult{
			RuleID:    string(issue.Rule),
			RuleIndex: ruleIndex[issue.Rule],
			Level:     severityToSARIFLevel(issue.Severity),
			Message:   sarifMessage{Text: issue.Message},
			PartialFingerprints: map[string]string{
				"primaryLocationLineHash": issue.Fingerprint,
			},
		}

		if issue.Source != nil || issue.File != "" {
			loc := buildSARIFLocation(issue.File, issue.Source, nil)
			r.Locations = []sarifLocation{loc}
		}

		if len(issue.Trace) > 0 {
			var tfLocs []sarifThreadFlowLocation
			for _, entry := range issue.Trace {
				file := ""
				if entry.Source != nil {
					file = entry.Source.File
				}
				msg := entry.Note
				tfl := sarifThreadFlowLocation{
					Location: buildSARIFLocation(file, entry.Source, &msg),
				}
				tfLocs = append(tfLocs, tfl)
			}
			r.CodeFlows = []sarifCodeFlow{
				{
					ThreadFlows: []sarifThreadFlow{
						{Locations: tfLocs},
					},
				},
			}
		}

		results = append(results, r)
	}

	log := sarifLog{
		Schema:  "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json",
		Version: "2.1.0",
		Runs: []sarifRun{
			{
				Tool: sarifTool{
					Driver: sarifDriver{
						Name:    toolName,
						Version: toolVersion,
						Rules:   rules,
					},
				},
				Results: results,
			},
		},
	}

	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(log)
}

func severityToSARIFLevel(s Severity) string {
	switch s {
	case SeverityError:
		return "error"
	case SeverityWarning:
		return "warning"
	case SeverityInfo:
		return "note"
	default:
		return "none"
	}
}

func buildSARIFLocation(file string, src *token.Location, message *string) sarifLocation {
	loc := sarifLocation{
		PhysicalLocation: sarifPhysicalLocation{
			ArtifactLocation: sarifArtifactLocation{URI: file},
		},
	}
	if src != nil && src.Line > 0 {
		loc.PhysicalLocation.Region = &sarifRegion{
			StartLine:   src.Line,
			StartColumn: src.Col,
		}
	}
	if message != nil {
		loc.Message = &sarifMessage{Text: *message}
	}
	return loc
}

func buildRuleDescriptors() []sarifRule {
	return []sarifRule{
		{
			ID:               string(PERF001),
			Name:             "HotPath",
			ShortDescription: sarifMessage{Text: "Cumulative cost score exceeds threshold"},
			DefaultConfig:    sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(PERF002),
			Name:             "ScalingRisk",
			ShortDescription: sarifMessage{Text: "O(N^k) complexity at or above threshold"},
			DefaultConfig:    sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(PERF003),
			Name:             "ExpensiveInLoop",
			ShortDescription: sarifMessage{Text: "Known-expensive function called inside a loop"},
			DefaultConfig:    sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(PERF004),
			Name:             "RecursiveCycle",
			ShortDescription: sarifMessage{Text: "Mutual or self-recursion detected"},
			DefaultConfig:    sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(UNKNOWN001),
			Name:             "DynamicDispatch",
			ShortDescription: sarifMessage{Text: "Callee cannot be statically resolved"},
			DefaultConfig:    sarifRuleConfig{Level: "note"},
		},
	}
}

// SARIF v2.1.0 JSON types — hand-rolled to avoid external dependencies.

type sarifLog struct {
	Schema  string     `json:"$schema"`
	Version string     `json:"version"`
	Runs    []sarifRun `json:"runs"`
}

type sarifRun struct {
	Tool    sarifTool     `json:"tool"`
	Results []sarifResult `json:"results"`
}

type sarifTool struct {
	Driver sarifDriver `json:"driver"`
}

type sarifDriver struct {
	Name    string      `json:"name"`
	Version string      `json:"version"`
	Rules   []sarifRule `json:"rules"`
}

type sarifRule struct {
	ID               string          `json:"id"`
	Name             string          `json:"name"`
	ShortDescription sarifMessage    `json:"shortDescription"`
	DefaultConfig    sarifRuleConfig `json:"defaultConfiguration"`
}

type sarifRuleConfig struct {
	Level string `json:"level"`
}

type sarifMessage struct {
	Text string `json:"text"`
}

type sarifResult struct {
	RuleID              string            `json:"ruleId"`
	RuleIndex           int               `json:"ruleIndex"`
	Level               string            `json:"level"`
	Message             sarifMessage      `json:"message"`
	Locations           []sarifLocation   `json:"locations,omitempty"`
	PartialFingerprints map[string]string `json:"partialFingerprints"`
	CodeFlows           []sarifCodeFlow   `json:"codeFlows,omitempty"`
}

type sarifLocation struct {
	PhysicalLocation sarifPhysicalLocation `json:"physicalLocation"`
	Message          *sarifMessage         `json:"message,omitempty"`
}

type sarifPhysicalLocation struct {
	ArtifactLocation sarifArtifactLocation `json:"artifactLocation"`
	Region           *sarifRegion          `json:"region,omitempty"`
}

type sarifArtifactLocation struct {
	URI string `json:"uri"`
}

type sarifRegion struct {
	StartLine   int `json:"startLine"`
	StartColumn int `json:"startColumn,omitempty"`
}

type sarifCodeFlow struct {
	ThreadFlows []sarifThreadFlow `json:"threadFlows"`
}

type sarifThreadFlow struct {
	Locations []sarifThreadFlowLocation `json:"locations"`
}

type sarifThreadFlowLocation struct {
	Location sarifLocation `json:"location"`
}
