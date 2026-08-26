// Copyright © 2024 The ELPS authors

package perf

import (
	"encoding/json"
	"io"
	"strings"

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

// buildRuleDescriptors returns the rule metadata GitHub code scanning renders
// on an alert page. shortDescription is the one-line summary shown in listings;
// fullDescription and help fill the alert's Description panel, which GitHub
// leaves blank ("No rule help available for this alert") when they are absent.
// GitHub prefers help.markdown and falls back to help.text.
func buildRuleDescriptors() []sarifRule {
	return []sarifRule{
		{
			ID:               string(PERF001),
			Name:             "HotPath",
			ShortDescription: sarifMultiformatMessage{Text: "Cumulative cost score exceeds threshold"},
			FullDescription: sarifMultiformatMessage{
				Text: "A function's propagated cost score exceeds max_score. The score charges every " +
					"call site a cost, multiplies it by loop_multiplier once per enclosing loop form, " +
					"and adds in the already-propagated score of every callee the analyzer could resolve.",
			},
			Help: sarifMultiformatMessage{
				Text: helpTextPERF001,
				Markdown: markdown(
					"### PERF001 — hot path",
					"",
					"**What triggers it.** Every call site is charged a cost: 1 by default, whatever",
					"`function_costs` says for that name, plus `expensive_cost` (50 by default) when the",
					"callee matches an `expensive_functions` glob. That cost is multiplied by",
					"`loop_multiplier` (20 by default) once for each enclosing loop form, and each",
					"callee's own total is propagated back to its callers the same way. PERF001 fires",
					"when a function's total exceeds `max_score` (100000 by default).",
					"",
					"**When it is worth acting on.** Read the `codeFlows` trace attached to the alert: it",
					"follows the highest-cost callee at each step, so its last entries are where the score",
					"actually comes from. The findings worth fixing are usually an expensive call sitting",
					"several loop levels deep, or an already-hot helper called from inside a loop.",
					"",
					"**When it is by design.** The score is a static heuristic, not a measurement. Every",
					"loop is assumed to run `loop_multiplier` iterations whether it walks three elements or",
					"three million, and no callee is weighted by what it really does unless you say so via",
					"`function_costs`. A function that legitimately does a lot of cheap work can cross the",
					"threshold. Tune `.elps-analyze.yaml` (`max_score`, `loop_multiplier`,",
					"`function_costs`, `expensive_functions`) before treating the number as a verdict.",
					"",
					"**Suppressing it.** Put a leading comment directly above the definition:",
					"",
					"```lisp",
					";; elps-analyze-disable:PERF001",
					"(defun render-report (rows) ...)",
					"```",
					"",
					"A bare `;; elps-analyze-disable` suppresses every rule for that function, and a",
					"comma-separated list (`;; elps-analyze-disable:PERF001,PERF002`) suppresses several.",
					"Embedders may rename the prefix through `suppression_prefix`.",
				),
			},
			DefaultConfig: sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(PERF002),
			Name:             "ScalingRisk",
			ShortDescription: sarifMultiformatMessage{Text: "O(N^k) complexity at or above threshold"},
			FullDescription: sarifMultiformatMessage{
				Text: "A function's propagated scaling order — loop nesting accumulated through the call " +
					"graph — reached max_acceptable_order (warning) or scaling_error_threshold (error). " +
					"Order 0 is O(1), 1 is O(N), 2 is O(N^2), and so on.",
			},
			Help: sarifMultiformatMessage{
				Text: helpTextPERF002,
				Markdown: markdown(
					"### PERF002 — scaling risk",
					"",
					"**What triggers it.** Each call site contributes its callee's scaling order plus the",
					"loop depth at the call site, and a function takes the largest such value. With",
					"`amplification_causes_scaling` enabled, an expensive call inside a loop adds a further",
					"+1, which is how the N+1 shape is surfaced. The result is reported as a warning at",
					"`max_acceptable_order` (2 by default) and as an error at `scaling_error_threshold`",
					"(3 by default). Functions inside a recursive cycle are capped at `max_recursion_order`.",
					"",
					"**When it is worth acting on.** When two collections that both grow with the input are",
					"iterated one inside the other — most often a helper that already loops being called",
					"from inside another loop, the accidental quadratic. The `codeFlows` trace follows the",
					"callee contributing the most order, so it points at the inner loop.",
					"",
					"**When it is by design.** The exponent counts *syntactic* loop nesting, not the size of",
					"what is iterated. A loop over a fixed three-element list counts exactly as much as a",
					"loop over unbounded user input, so nested iteration over small, bounded collections is",
					"reported and is usually fine. Raise `max_acceptable_order` or narrow `loop_keywords` if",
					"the shape is normal for your codebase.",
					"",
					"**Suppressing it.** Put a leading comment directly above the definition:",
					"",
					"```lisp",
					";; elps-analyze-disable:PERF002",
					"(defun cross-join (xs ys) ...)",
					"```",
					"",
					"A bare `;; elps-analyze-disable` suppresses every rule for that function.",
				),
			},
			DefaultConfig: sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(PERF003),
			Name:             "ExpensiveInLoop",
			ShortDescription: sarifMultiformatMessage{Text: "Known-expensive function called inside a loop"},
			FullDescription: sarifMultiformatMessage{
				Text: "A call to a function matching an expensive_functions glob occurs inside a loop " +
					"form (dotimes, map, foldl, foldr, select, reject by default, plus any loop_keywords " +
					"an embedder adds). Reported once per call site, at the call site.",
			},
			Help: sarifMultiformatMessage{
				Text: helpTextPERF003,
				Markdown: markdown(
					"### PERF003 — expensive call inside a loop",
					"",
					"**What triggers it.** The head symbol of the call matches one of the",
					"`expensive_functions` globs (`db-*`, `put-state`, `get-state`, `http-*` by default;",
					"embedders add their own) and the call sits at loop depth 1 or deeper. Lambda bodies",
					"are scanned at the depth of the form they are passed to, so",
					"`(map 'list (lambda (x) (db-get x)) xs)` counts. The alert is placed on the call site,",
					"not on the enclosing function.",
					"",
					"**When it is worth acting on.** This is the N+1 shape: one round trip per element.",
					"The fix is usually to hoist the call out of the loop, batch the work into a single",
					"call, or replace per-element lookups with one range query.",
					"",
					"**When it is by design.** When the loop runs over a small fixed collection, or when the",
					"glob has matched a name that is not actually expensive here — the patterns are",
					"configuration, not knowledge about your code. Prefer narrowing `expensive_functions`",
					"over suppressing the rule at every site it touches.",
					"",
					"**Suppressing it.** The comment attaches to the *enclosing definition*, not to the",
					"call site:",
					"",
					"```lisp",
					";; elps-analyze-disable:PERF003",
					"(defun sync-all (ids)",
					"  (map 'list (lambda (id) (db-get id)) ids))",
					"```",
					"",
					"A bare `;; elps-analyze-disable` suppresses every rule for that function.",
				),
			},
			DefaultConfig: sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(PERF004),
			Name:             "RecursiveCycle",
			ShortDescription: sarifMultiformatMessage{Text: "Mutual or self-recursion detected"},
			FullDescription: sarifMultiformatMessage{
				Text: "The call graph contains a cycle: a strongly connected component of two or more " +
					"functions, or a single function that calls itself. Each cycle is reported once, on " +
					"the member whose name sorts first alphabetically, and the message lists every member.",
			},
			Help: sarifMultiformatMessage{
				Text: helpTextPERF004,
				Markdown: markdown(
					"### PERF004 — recursive cycle",
					"",
					"**What triggers it.** Once the call graph is built, cycles are found with Tarjan's",
					"strongly-connected-components algorithm. Any component with more than one member is a",
					"cycle, and so is a single function that calls itself — plain self-recursion is",
					"reported exactly like mutual recursion. Only calls to functions defined in the",
					"analyzed file set form edges. Each cycle is reported once, on the alphabetically first",
					"member, with every member listed in the message.",
					"",
					"**When it is worth acting on.** When the recursion is not the shape you intended: a",
					"helper that unexpectedly calls back into its caller, a mutual recursion introduced by",
					"a refactor, or a recursion whose depth follows the size of an input, which is a stack",
					"risk when the calls are not in tail position.",
					"",
					"**When it is by design — which is common.** A deliberate structural recursion over a",
					"tree-shaped value (walking a nested sorted-map, validating a JSON document, folding an",
					"expression tree) is a normal, correct implementation, and this rule cannot distinguish",
					"it from an unintended cycle. It reports the *shape* of the call graph and nothing",
					"more: there is no base-case, termination, or depth analysis behind it. If the",
					"recursion is the design, suppress it — the alert carries no information beyond \"these",
					"functions call each other\".",
					"",
					"**Suppressing it.** Put a leading comment directly above the definition:",
					"",
					"```lisp",
					";; elps-analyze-disable:PERF004",
					"(defun assert-json-safe (value) ...)",
					"```",
					"",
					"For a cycle with several members, the comment on any one member suppresses the report",
					"for the whole cycle. A bare `;; elps-analyze-disable` suppresses every rule for that",
					"function.",
				),
			},
			DefaultConfig: sarifRuleConfig{Level: "warning"},
		},
		{
			ID:               string(UNKNOWN001),
			Name:             "DynamicDispatch",
			ShortDescription: sarifMultiformatMessage{Text: "Callee cannot be statically resolved"},
			FullDescription: sarifMultiformatMessage{
				Text: "A call whose callee is a runtime value — funcall, apply, or a form whose head is " +
					"itself an expression. Informational: the call graph stops at such a call, so cost " +
					"and scaling beyond it are not counted by the other rules.",
			},
			Help: sarifMultiformatMessage{
				Text: helpTextUNKNOWN001,
				Markdown: markdown(
					"### UNKNOWN001 — dynamic dispatch",
					"",
					"**What triggers it.** `funcall`, `apply`, or any form whose head position holds an",
					"expression rather than a symbol. The callee is a runtime value, so the analyzer",
					"records an edge to `<dynamic>` and cannot follow it.",
					"",
					"**When it is worth acting on.** Not as a defect — as a coverage gap. Everything",
					"reached through that call is invisible to PERF001–PERF004, so a hot path or an N+1",
					"hiding behind it will not be reported. Where the target is in fact fixed, calling it by",
					"name restores the analysis.",
					"",
					"**When it is by design.** Higher-order code — dispatch tables, callbacks, handler",
					"registries — is idiomatic ELPS and will always produce these, which is why the rule",
					"defaults to note severity. Drop it entirely by listing only the rules you want, with",
					"`--rules=PERF001,PERF002,PERF003,PERF004` or the `rules:` key in `.elps-analyze.yaml`.",
					"",
					"**Suppressing it.** Put a leading comment directly above the enclosing definition:",
					"",
					"```lisp",
					";; elps-analyze-disable:UNKNOWN001",
					"(defun dispatch (handler args)",
					"  (apply handler args))",
					"```",
					"",
					"A bare `;; elps-analyze-disable` suppresses every rule for that function.",
				),
			},
			DefaultConfig: sarifRuleConfig{Level: "note"},
		},
	}
}

// markdown joins help lines into a single GitHub-flavored markdown string.
// Go has no raw string literal that can contain a backtick, and rule help is
// full of them, so the lines are written as ordinary literals and joined here.
func markdown(lines ...string) string {
	return strings.Join(lines, "\n")
}

// Plain-text fallbacks for help.markdown. GitHub prefers the markdown, but the
// SARIF spec makes text the required field of a multiformatMessageString and
// other consumers render it instead.
const (
	helpTextPERF001 = "PERF001 fires when a function's propagated cost score exceeds max_score. " +
		"Every call site costs 1 (or its function_costs override) plus expensive_cost when the callee " +
		"matches an expensive_functions glob, multiplied by loop_multiplier once per enclosing loop, " +
		"with each callee's own total propagated back to its callers. The codeFlows trace follows the " +
		"highest-cost callee, so it shows where the score comes from. The score is a static heuristic: " +
		"every loop is assumed to run loop_multiplier iterations regardless of what it iterates, so a " +
		"function doing a lot of cheap work can cross the threshold legitimately — tune " +
		".elps-analyze.yaml before treating it as a verdict. Suppress it with a leading comment on the " +
		"definition: ;; elps-analyze-disable:PERF001"

	helpTextPERF002 = "PERF002 fires when a function's propagated scaling order (loop nesting " +
		"accumulated through the call graph) reaches max_acceptable_order, as a warning, or " +
		"scaling_error_threshold, as an error. Each call site contributes its callee's order plus the " +
		"loop depth at the call site. It is worth acting on when two collections that both grow with " +
		"the input are iterated one inside the other. The exponent counts syntactic loop nesting, not " +
		"the size of what is iterated, so nested loops over small bounded collections are reported and " +
		"are often fine. Suppress it with a leading comment on the definition: " +
		";; elps-analyze-disable:PERF002"

	helpTextPERF003 = "PERF003 fires when a call whose name matches an expensive_functions glob " +
		"occurs inside a loop form (dotimes, map, foldl, foldr, select, reject by default). Lambda " +
		"bodies are scanned at the depth of the form they are passed to. This is the N+1 shape — one " +
		"round trip per element — and the fix is usually to hoist, batch, or use a single range query. " +
		"It is by design when the loop runs over a small fixed collection, or when the glob matched a " +
		"name that is not actually expensive; narrowing expensive_functions beats suppressing it " +
		"everywhere. Suppression attaches to the enclosing definition, not the call site: " +
		";; elps-analyze-disable:PERF003"

	helpTextPERF004 = "PERF004 fires when the call graph contains a cycle — two or more functions " +
		"that call each other, or a single function that calls itself. Each cycle is reported once, on " +
		"the alphabetically first member, and the message lists every member. Act on it when the " +
		"recursion is not the shape you intended, or when its depth follows the size of an input and " +
		"the calls are not in tail position. A deliberate structural recursion over a tree-shaped value " +
		"is a normal, correct implementation, and this rule cannot distinguish it from an unintended " +
		"cycle: it reports the shape of the call graph with no base-case or termination analysis behind " +
		"it. Suppress it with a leading comment on the definition: ;; elps-analyze-disable:PERF004 — " +
		"for a multi-member cycle, a comment on any one member suppresses the whole report."

	helpTextUNKNOWN001 = "UNKNOWN001 reports a call whose callee is a runtime value: funcall, apply, " +
		"or a form whose head is itself an expression. It is not a defect but a coverage gap — the call " +
		"graph stops there, so anything reached through the call is invisible to PERF001-PERF004. " +
		"Higher-order code is idiomatic ELPS, which is why the rule defaults to note severity; list " +
		"only the rules you want via --rules or the rules: config key to drop it. Suppress it with a " +
		"leading comment on the enclosing definition: ;; elps-analyze-disable:UNKNOWN001"
)

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
	ID   string `json:"id"`
	Name string `json:"name"`
	// ShortDescription is the one-line summary shown in alert listings.
	ShortDescription sarifMultiformatMessage `json:"shortDescription"`
	// FullDescription and Help populate the Description panel on a GitHub
	// code-scanning alert page; without them the page reads "No rule help
	// available for this alert".
	FullDescription sarifMultiformatMessage `json:"fullDescription"`
	Help            sarifMultiformatMessage `json:"help"`
	DefaultConfig   sarifRuleConfig         `json:"defaultConfiguration"`
}

type sarifRuleConfig struct {
	Level string `json:"level"`
}

type sarifMessage struct {
	Text string `json:"text"`
}

// sarifMultiformatMessage is a SARIF multiformatMessageString: a plain-text
// rendering plus an optional markdown one. GitHub renders the markdown when
// present and falls back to the text.
type sarifMultiformatMessage struct {
	Text     string `json:"text"`
	Markdown string `json:"markdown,omitempty"`
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
