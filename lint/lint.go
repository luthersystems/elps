// Copyright © 2024 The ELPS authors

// Package lint provides static analysis for ELPS lisp source files.
//
// The linter is modeled after go vet: each check is an independent Analyzer
// that receives a parsed AST and reports diagnostics. The framework handles
// parsing, running analyzers, collecting results, and formatting output.
//
// Analyzers are composable and extensible — embedders can define custom
// checks alongside the built-in set.
package lint

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Severity indicates the severity level of a lint diagnostic.
type Severity int

const (
	severityUnset Severity = iota // unexported zero sentinel for default detection
	SeverityError
	SeverityWarning
	SeverityInfo
)

func (s Severity) String() string {
	switch s {
	case SeverityError:
		return "error"
	case SeverityWarning:
		return "warning"
	case SeverityInfo:
		return "info"
	default:
		return "unknown"
	}
}

// MarshalJSON serializes the severity as a JSON string.
// An unset severity (zero value) is marshaled as "warning".
func (s Severity) MarshalJSON() ([]byte, error) {
	if s == severityUnset {
		return json.Marshal("warning")
	}
	return json.Marshal(s.String())
}

// UnmarshalJSON deserializes a severity from a JSON string.
func (s *Severity) UnmarshalJSON(data []byte) error {
	var str string
	if err := json.Unmarshal(data, &str); err != nil {
		return err
	}
	switch str {
	case "error":
		*s = SeverityError
	case "warning":
		*s = SeverityWarning
	case "info":
		*s = SeverityInfo
	default:
		return fmt.Errorf("unknown severity: %q", str)
	}
	return nil
}

// ParseSeverity converts a string to a Severity value.
// Valid inputs: "error", "warning", "info".
func ParseSeverity(s string) (Severity, error) {
	switch s {
	case "error":
		return SeverityError, nil
	case "warning":
		return SeverityWarning, nil
	case "info":
		return SeverityInfo, nil
	default:
		return 0, fmt.Errorf("unknown severity: %q (valid: error, warning, info)", s)
	}
}

// MaxSeverity returns the most severe level found in the given diagnostics.
// Returns SeverityInfo if diags is empty (i.e., least severe / no problems).
// Severity ordering: SeverityError > SeverityWarning > SeverityInfo.
func MaxSeverity(diags []Diagnostic) Severity {
	if len(diags) == 0 {
		return SeverityInfo
	}
	max := SeverityInfo // start at least severe
	for _, d := range diags {
		sev := d.Severity
		if sev == severityUnset {
			sev = SeverityWarning // matches MarshalJSON behavior
		}
		if sev < max {
			max = sev // lower numeric value = more severe
		}
	}
	return max
}

// ShouldFail returns true if the diagnostics contain at least one finding
// at or above the given severity threshold.
func ShouldFail(diags []Diagnostic, threshold Severity) bool {
	if len(diags) == 0 {
		return false
	}
	return MaxSeverity(diags) <= threshold // lower numeric = more severe
}

// Analyzer defines a single lint check.
type Analyzer struct {
	// Name is a short identifier for this check (e.g. "set-usage").
	Name string

	// Doc is a human-readable description. The first line is a short summary.
	Doc string

	// Severity is the default severity for diagnostics from this analyzer.
	Severity Severity

	// Semantic indicates that this analyzer requires semantic analysis
	// (pass.Semantics != nil) to produce diagnostics. When true and
	// semantic analysis is not available, nolint directives targeting
	// this analyzer are treated as conditionally valid rather than unused.
	Semantic bool

	// Run executes the check. It should call pass.Report() for each finding.
	Run func(pass *Pass) error
}

// Pass provides context to a running analyzer.
type Pass struct {
	// Analyzer is the currently running check.
	Analyzer *Analyzer

	// Filename is the source file being analyzed.
	Filename string

	// Exprs are the top-level parsed expressions.
	Exprs []*lisp.LVal

	// Semantics holds the result of semantic analysis, if available.
	// Nil when semantic analysis has not been run (e.g. no --workspace flag).
	// Semantic analyzers should check for nil and return early.
	Semantics *analysis.Result

	// diagnostics collects reported findings.
	diagnostics []Diagnostic
}

// Report records a diagnostic finding.
func (p *Pass) Report(d Diagnostic) {
	d.Analyzer = p.Analyzer.Name
	if d.Severity == severityUnset {
		d.Severity = p.Analyzer.Severity
	}
	p.diagnostics = append(p.diagnostics, d)
}

// ReportWithNotes records a diagnostic with additional hint text.
func (p *Pass) ReportWithNotes(d Diagnostic, notes ...string) {
	d.Analyzer = p.Analyzer.Name
	if d.Severity == severityUnset {
		d.Severity = p.Analyzer.Severity
	}
	d.Notes = append(d.Notes, notes...)
	p.diagnostics = append(p.diagnostics, d)
}

// ReportNode records a diagnostic spanning a node's source range. It
// extracts both start and end positions from the node. If the node
// lacks end position information, it falls back to using the symbol
// name length as a heuristic.
func (p *Pass) ReportNode(node *lisp.LVal, format string, args ...interface{}) {
	d := Diagnostic{
		Message: fmt.Sprintf(format, args...),
	}
	if node != nil && node.Source != nil {
		d.Pos = Position{File: node.Source.File, Line: node.Source.Line, Col: node.Source.Col}
		if node.Source.EndLine > 0 && node.Source.EndCol > 0 {
			d.EndPos = Position{File: node.Source.File, Line: node.Source.EndLine, Col: node.Source.EndCol}
		} else if node.Type == lisp.LSymbol && len(node.Str) > 0 && node.Source.Col > 0 {
			d.EndPos = Position{File: node.Source.File, Line: node.Source.Line, Col: node.Source.Col + len(node.Str)}
		}
	}
	p.Report(d)
}

// Reportf is a convenience for reporting a diagnostic at a position.
func (p *Pass) Reportf(source *token.Location, format string, args ...interface{}) {
	d := Diagnostic{
		Message: fmt.Sprintf(format, args...),
	}
	if source != nil {
		d.Pos = Position{File: source.File, Line: source.Line, Col: source.Col}
	}
	p.Report(d)
}

// Diagnostic is a single reported problem.
type Diagnostic struct {
	// Pos is the source location of the problem.
	Pos Position `json:"pos"`

	// EndPos is the end of the diagnostic range. When zero, editors
	// treat the diagnostic as zero-width (a single point). When set,
	// the range [Pos, EndPos) is highlighted.
	EndPos Position `json:"end_pos,omitempty"`

	// Message is a human-readable description of the problem.
	Message string `json:"message"`

	// Analyzer is the name of the check that found this problem.
	Analyzer string `json:"analyzer"`

	// Severity is the severity level of the diagnostic.
	Severity Severity `json:"severity"`

	// Notes are optional hint text lines for the user.
	Notes []string `json:"notes,omitempty"`
}

// Position identifies a location in source code.
type Position struct {
	File string `json:"file"`
	Line int    `json:"line"`
	Col  int    `json:"col,omitempty"`
}

// String returns the position in file:line format.
func (p Position) String() string {
	if p.Line == 0 {
		return p.File
	}
	if p.Col > 0 {
		return fmt.Sprintf("%s:%d:%d", p.File, p.Line, p.Col)
	}
	return fmt.Sprintf("%s:%d", p.File, p.Line)
}

// String returns the diagnostic in go vet style: file:line: message (analyzer)
// with optional note lines appended.
func (d Diagnostic) String() string {
	s := fmt.Sprintf("%s: %s (%s)", d.Pos, d.Message, d.Analyzer)
	for _, n := range d.Notes {
		s += "\n  = note: " + n
	}
	return s
}

// Linter runs a set of analyzers over source files.
type Linter struct {
	Analyzers []*Analyzer
}

// LintConfig configures the linter for a single run.
type LintConfig struct {
	// Workspace is the root directory for cross-file scanning.
	// Empty string disables workspace scanning.
	Workspace string

	// Registry provides Go-registered symbols (builtins, special ops, macros)
	// from an embedder's environment. These are merged with stdlib and
	// workspace symbols for semantic analysis. When nil, only stdlib symbols
	// are used.
	Registry *lisp.PackageRegistry

	// Excludes are glob patterns for files to skip during workspace scanning.
	Excludes []string

	// StdlibExports provides pre-extracted stdlib package exports. When nil,
	// LintFiles uses the default stdlib (loaded via lisplib.LoadLibrary).
	// Embedders that already have a configured env can pass
	// analysis.ExtractPackageExports(env.Runtime.Registry) here to avoid
	// the overhead of creating a temporary environment.
	StdlibExports map[string][]analysis.ExternalSymbol
}

// LintFiles analyzes source files with full workspace + embedder context.
// It handles workspace scanning, stdlib/registry symbol extraction, and
// semantic analysis configuration. The files slice contains resolved file
// paths (no glob expansion — callers handle that).
//
// When cfg is nil, all files are linted with syntactic checks only.
func (l *Linter) LintFiles(cfg *LintConfig, files []string) ([]Diagnostic, error) {
	var analysisCfg *analysis.Config
	if cfg != nil && cfg.Workspace != "" {
		acfg, err := BuildAnalysisConfig(cfg)
		if err != nil {
			return nil, err
		}
		analysisCfg = acfg
	}

	var allDiags []Diagnostic
	for _, path := range files {
		src, err := os.ReadFile(path) //nolint:gosec // linter reads user-specified files
		if err != nil {
			return nil, fmt.Errorf("%s: %w", path, err)
		}
		var diags []Diagnostic
		if analysisCfg != nil {
			diags, err = l.LintFileWithAnalysis(src, path, analysisCfg)
		} else {
			diags, err = l.LintFile(src, path)
		}
		if err != nil {
			return nil, err
		}
		allDiags = append(allDiags, diags...)
	}
	return allDiags, nil
}

// BuildAnalysisConfig constructs an analysis.Config from a LintConfig.
// It scans the workspace, extracts stdlib exports, and merges embedder
// registry symbols. This is exported for callers (like the CLI stdin path)
// that need to build the config separately from file linting.
func BuildAnalysisConfig(cfg *LintConfig) (*analysis.Config, error) {
	syms, err := analysis.ScanWorkspace(cfg.Workspace)
	if err != nil {
		return nil, fmt.Errorf("scanning workspace %s: %w", cfg.Workspace, err)
	}

	// Start with stdlib exports (either provided or extracted fresh)
	pkgExports := cfg.StdlibExports
	if pkgExports == nil {
		pkgExports = defaultStdlibExports()
	}

	// Merge embedder registry exports
	if cfg.Registry != nil {
		regExports := analysis.ExtractPackageExports(cfg.Registry)
		for pkg, regSyms := range regExports {
			pkgExports[pkg] = append(pkgExports[pkg], regSyms...)
		}
	}

	// Merge workspace package exports
	wsPkgs, err := analysis.ScanWorkspacePackages(cfg.Workspace)
	if err != nil {
		return nil, fmt.Errorf("scanning workspace packages %s: %w", cfg.Workspace, err)
	}
	for pkg, wsSyms := range wsPkgs {
		pkgExports[pkg] = append(pkgExports[pkg], wsSyms...)
	}

	return &analysis.Config{
		ExtraGlobals:   syms,
		PackageExports: pkgExports,
	}, nil
}

// defaultStdlibExports creates a temporary ELPS env, loads the standard
// library, and extracts package exports.
func defaultStdlibExports() map[string][]analysis.ExternalSymbol {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	lisp.InitializeUserEnv(env)
	lisplib.LoadLibrary(env)
	return analysis.ExtractPackageExports(env.Runtime.Registry)
}

// LintFile analyzes a single source file and returns all diagnostics.
// Semantic analyzers are no-ops because no analysis.Result is provided.
func (l *Linter) LintFile(source []byte, filename string) ([]Diagnostic, error) {
	return l.LintFileWithContext(source, filename, nil)
}

// LintFileWithContext analyzes a source file with optional semantic analysis results.
// When semantics is nil, semantic analyzers (undefined-symbol, unused-variable, etc.)
// are no-ops. When non-nil, they use the scope and reference data for deeper checks.
func (l *Linter) LintFileWithContext(source []byte, filename string, semantics *analysis.Result) ([]Diagnostic, error) {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.NewFormatting(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil, fmt.Errorf("%s: %w", filename, err)
	}

	var all []Diagnostic

	for _, analyzer := range l.Analyzers {
		pass := &Pass{
			Analyzer:  analyzer,
			Filename:  filename,
			Exprs:     exprs,
			Semantics: semantics,
		}
		if err := analyzer.Run(pass); err != nil {
			return nil, fmt.Errorf("%s: analyzer %s: %w", filename, analyzer.Name, err)
		}
		// Set file on diagnostics that don't have one
		for i := range pass.diagnostics {
			if pass.diagnostics[i].Pos.File == "" {
				pass.diagnostics[i].Pos.File = filename
			}
		}
		all = append(all, pass.diagnostics...)
	}

	// Filter suppressed diagnostics (;nolint comments)
	all, nolintMap := filterSuppressed(all, exprs)

	// Build set of all known analyzer names for unknown-analyzer detection.
	// Use DefaultAnalyzers() plus l.Analyzers so that --checks filtering
	// does not cause valid analyzer names to be flagged as unknown.
	knownAnalyzers := make(map[string]bool)
	for _, a := range DefaultAnalyzers() {
		knownAnalyzers[a.Name] = true
	}
	for _, a := range l.Analyzers {
		knownAnalyzers[a.Name] = true
	}
	// unused-nolint is synthetic (not a registered Analyzer) but valid in directives.
	knownAnalyzers["unused-nolint"] = true

	// Build set of semantic-only analyzer names for conditional nolint handling.
	semanticAnalyzers := make(map[string]bool)
	for _, a := range DefaultAnalyzers() {
		if a.Semantic {
			semanticAnalyzers[a.Name] = true
		}
	}
	for _, a := range l.Analyzers {
		if a.Semantic {
			semanticAnalyzers[a.Name] = true
		}
	}

	// Emit unused-nolint diagnostics for stale directives.
	var unusedNolints []Diagnostic
	for _, info := range nolintMap {
		if info.used {
			continue
		}

		// In syntactic mode, skip nolint directives that exclusively target
		// semantic-only analyzers. These are valid when running with --workspace
		// but the underlying analyzers produce no diagnostics without it.
		if semantics == nil && info.directive != "" {
			allSemantic := true
			for _, name := range strings.Split(info.directive, ",") {
				name = strings.TrimSpace(name)
				if name != "" && !semanticAnalyzers[name] {
					allSemantic = false
					break
				}
			}
			if allSemantic {
				continue
			}
		}

		msg := "nolint directive does not suppress any diagnostic"
		// Check for unknown analyzer names in specific directives.
		if info.directive != "" {
			var unknown []string
			for _, name := range strings.Split(info.directive, ",") {
				name = strings.TrimSpace(name)
				if name != "" && !knownAnalyzers[name] {
					unknown = append(unknown, name)
				}
			}
			if len(unknown) > 0 {
				msg = fmt.Sprintf("nolint directive references unknown analyzer(s): %s",
					strings.Join(unknown, ", "))
			}
		}
		unusedNolints = append(unusedNolints, Diagnostic{
			Pos:      Position{File: filename, Line: info.source.Line, Col: info.source.Col},
			Message:  msg,
			Analyzer: "unused-nolint",
			Severity: SeverityWarning,
			Notes:    []string{"remove the nolint directive or fix the analyzer name", "; nolint:unused-nolint"},
		})
	}

	// Allow ; nolint:unused-nolint to self-suppress. Only explicit mentions
	// of "unused-nolint" suppress the diagnostic — a bare ; nolint that didn't
	// suppress anything is still reported as unused.
	for i := len(unusedNolints) - 1; i >= 0; i-- {
		d := unusedNolints[i]
		info, ok := nolintMap[d.Pos.Line]
		if !ok || info.directive == "" {
			continue
		}
		for _, name := range strings.Split(info.directive, ",") {
			if strings.TrimSpace(name) == "unused-nolint" {
				unusedNolints = append(unusedNolints[:i], unusedNolints[i+1:]...)
				break
			}
		}
	}

	all = append(all, unusedNolints...)

	// Sort by file, then line
	sort.Slice(all, func(i, j int) bool {
		if all[i].Pos.File != all[j].Pos.File {
			return all[i].Pos.File < all[j].Pos.File
		}
		return all[i].Pos.Line < all[j].Pos.Line
	})

	return all, nil
}

// LintFileWithAnalysis parses, analyzes, and lints a source file in one call.
// This is a convenience that runs semantic analysis and passes the result to
// all analyzers.
func (l *Linter) LintFileWithAnalysis(source []byte, filename string, cfg *analysis.Config) ([]Diagnostic, error) {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil, fmt.Errorf("%s: %w", filename, err)
	}

	if cfg == nil {
		cfg = &analysis.Config{}
	}
	cfg.Filename = filename
	result := analysis.Analyze(exprs, cfg)

	return l.LintFileWithContext(source, filename, result)
}

// nolintInfo tracks a nolint directive and its source location.
type nolintInfo struct {
	directive string         // "" for suppress-all, or "analyzer1,analyzer2"
	source    token.Location // location of the nolint comment itself
	used      bool           // whether this directive suppressed at least one diagnostic
}

// filterSuppressed removes diagnostics on lines with ;nolint comments and
// returns the filtered diagnostics plus a map of all nolint entries (with
// usage tracking). Callers can inspect entries where used==false to emit
// unused-nolint warnings.
func filterSuppressed(diags []Diagnostic, exprs []*lisp.LVal) ([]Diagnostic, map[int]*nolintInfo) {
	// Build a map of line -> nolint directives from trailing comments
	nolintLines := make(map[int]*nolintInfo)
	walkForNolint(exprs, nolintLines)

	var filtered []Diagnostic
	for _, d := range diags {
		info, ok := nolintLines[d.Pos.Line]
		if !ok {
			filtered = append(filtered, d)
			continue
		}
		// Empty directive = suppress all
		if info.directive == "" {
			info.used = true
			continue
		}
		// Check if this specific analyzer is suppressed
		suppressed := false
		for _, name := range strings.Split(info.directive, ",") {
			if strings.TrimSpace(name) == d.Analyzer {
				suppressed = true
				break
			}
		}
		if suppressed {
			info.used = true
		} else {
			filtered = append(filtered, d)
		}
	}
	return filtered, nolintLines
}

// walkForNolint finds ;nolint comments and maps them to line numbers.
func walkForNolint(exprs []*lisp.LVal, lines map[int]*nolintInfo) {
	for _, expr := range exprs {
		walkNodeForNolint(expr, lines)
	}
}

func walkNodeForNolint(v *lisp.LVal, lines map[int]*nolintInfo) {
	if v == nil {
		return
	}
	if v.Meta != nil {
		checkNolintToken(v.Meta.TrailingComment, lines)
		for _, c := range v.Meta.LeadingComments {
			checkNolintToken(c, lines)
		}
		for _, c := range v.Meta.InnerTrailingComments {
			checkNolintToken(c, lines)
		}
	}
	for _, child := range v.Cells {
		walkNodeForNolint(child, lines)
	}
}

func checkNolintToken(tok *token.Token, lines map[int]*nolintInfo) {
	if tok == nil || tok.Source == nil {
		return
	}
	text := strings.TrimSpace(tok.Text)
	// Strip comment prefix
	text = strings.TrimLeft(text, ";")
	text = strings.TrimSpace(text)

	if !strings.HasPrefix(text, "nolint") {
		return
	}
	rest := strings.TrimPrefix(text, "nolint")
	if rest == "" {
		lines[tok.Source.Line] = &nolintInfo{
			source: *tok.Source,
		}
		return
	}
	if strings.HasPrefix(rest, ":") {
		lines[tok.Source.Line] = &nolintInfo{
			directive: strings.TrimPrefix(rest, ":"),
			source:    *tok.Source,
		}
	}
}

// FormatText writes diagnostics in go vet text format.
func FormatText(w io.Writer, diags []Diagnostic) {
	for _, d := range diags {
		fmt.Fprintln(w, d.String()) //nolint:errcheck // best-effort output to writer
	}
}

// FormatJSON writes diagnostics as JSON.
func FormatJSON(w io.Writer, diags []Diagnostic) error {
	enc := json.NewEncoder(w)
	enc.SetIndent("", "  ")
	return enc.Encode(diags)
}

// DefaultAnalyzers returns the built-in set of lint checks.
func DefaultAnalyzers() []*Analyzer {
	return []*Analyzer{
		AnalyzerSetUsage,
		AnalyzerInPackageToplevel,
		AnalyzerIfArity,
		AnalyzerLetBindings,
		AnalyzerDefunStructure,
		AnalyzerCondStructure,
		AnalyzerBuiltinArity,
		AnalyzerQuoteCall,
		AnalyzerCondMissingElse,
		AnalyzerRethrowContext,
		AnalyzerUnnecessaryProgn,
		AnalyzerUndefinedSymbol,
		AnalyzerUnusedVariable,
		AnalyzerUnusedFunction,
		AnalyzerShadowing,
		AnalyzerUserArity,
	}
}
