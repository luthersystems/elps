// Copyright © 2024 The ELPS authors

// Package diagnostic provides Rust-style annotated error rendering for
// ELPS CLI output. It is intentionally independent of the lisp/ package
// so that it can be used by any CLI command without creating import cycles.
package diagnostic

// Severity indicates the severity level of a diagnostic.
type Severity int

const (
	SeverityError Severity = iota
	SeverityWarning
	SeverityNote
)

func (s Severity) String() string {
	switch s {
	case SeverityError:
		return "error"
	case SeverityWarning:
		return "warning"
	case SeverityNote:
		return "note"
	default:
		return "unknown"
	}
}

// Span identifies a region of source code to highlight in the diagnostic.
type Span struct {
	File   string // path for reading source; display name if unreadable
	Line   int    // 1-based line number
	Col    int    // 1-based start column
	EndCol int    // 1-based end column (0 = auto-detect from source)
	Label  string // text shown under the underline
}

// Diagnostic represents a single error, warning, or note with optional
// source annotations and trailing notes.
type Diagnostic struct {
	Severity Severity
	Message  string
	Spans    []Span
	Notes    []string // "= note:" lines (stack trace frames, etc.)
}
