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
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Analyzer defines a single lint check.
type Analyzer struct {
	// Name is a short identifier for this check (e.g. "set-usage").
	Name string

	// Doc is a human-readable description. The first line is a short summary.
	Doc string

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

	// diagnostics collects reported findings.
	diagnostics []Diagnostic
}

// Report records a diagnostic finding.
func (p *Pass) Report(d Diagnostic) {
	d.Analyzer = p.Analyzer.Name
	p.diagnostics = append(p.diagnostics, d)
}

// ReportWithNotes records a diagnostic with additional hint text.
func (p *Pass) ReportWithNotes(d Diagnostic, notes ...string) {
	d.Analyzer = p.Analyzer.Name
	d.Notes = append(d.Notes, notes...)
	p.diagnostics = append(p.diagnostics, d)
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

	// Message is a human-readable description of the problem.
	Message string `json:"message"`

	// Analyzer is the name of the check that found this problem.
	Analyzer string `json:"analyzer"`

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

// LintFile analyzes a single source file and returns all diagnostics.
func (l *Linter) LintFile(source []byte, filename string) ([]Diagnostic, error) {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.NewFormatting(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil, fmt.Errorf("%s: %w", filename, err)
	}

	var all []Diagnostic

	for _, analyzer := range l.Analyzers {
		pass := &Pass{
			Analyzer: analyzer,
			Filename: filename,
			Exprs:    exprs,
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
	all = filterSuppressed(all, exprs)

	// Sort by file, then line
	sort.Slice(all, func(i, j int) bool {
		if all[i].Pos.File != all[j].Pos.File {
			return all[i].Pos.File < all[j].Pos.File
		}
		return all[i].Pos.Line < all[j].Pos.Line
	})

	return all, nil
}

// filterSuppressed removes diagnostics on lines with ;nolint comments.
func filterSuppressed(diags []Diagnostic, exprs []*lisp.LVal) []Diagnostic {
	// Build a map of line -> nolint directives from trailing comments
	nolintLines := make(map[int]string) // line -> "" (all) or "analyzer1,analyzer2"
	walkForNolint(exprs, nolintLines)

	var filtered []Diagnostic
	for _, d := range diags {
		directive, ok := nolintLines[d.Pos.Line]
		if !ok {
			filtered = append(filtered, d)
			continue
		}
		// Empty directive = suppress all
		if directive == "" {
			continue
		}
		// Check if this specific analyzer is suppressed
		suppressed := false
		for _, name := range strings.Split(directive, ",") {
			if strings.TrimSpace(name) == d.Analyzer {
				suppressed = true
				break
			}
		}
		if !suppressed {
			filtered = append(filtered, d)
		}
	}
	return filtered
}

// walkForNolint finds ;nolint comments and maps them to line numbers.
func walkForNolint(exprs []*lisp.LVal, lines map[int]string) {
	for _, expr := range exprs {
		walkNodeForNolint(expr, lines)
	}
}

func walkNodeForNolint(v *lisp.LVal, lines map[int]string) {
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

func checkNolintToken(tok *token.Token, lines map[int]string) {
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
		lines[tok.Source.Line] = ""
		return
	}
	if strings.HasPrefix(rest, ":") {
		lines[tok.Source.Line] = strings.TrimPrefix(rest, ":")
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
	}
}
