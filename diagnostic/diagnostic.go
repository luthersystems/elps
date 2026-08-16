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
//
// UNITS AND CONVENTION.  Col and EndCol are 1-based BYTE columns, and EndCol
// is INCLUSIVE -- it names the column of the span's last byte, not the one
// after it.  A span covering "false" at columns 7 through 11 is Col: 7,
// EndCol: 11.
//
// That is the OPPOSITE of parser/token.Location.EndCol, which is documented
// (as of the #463 fix) as an EXCLUSIVE byte column.  The two types have
// same-named fields with opposite conventions, and nothing in the tree bridges
// one into the other today: cmd/diagnostic.go and repl/diagnostic.go both set
// Col and leave EndCol zero for auto-detection.  The first caller to wire an
// analyser's end position straight into a Span would get an underline one
// caret too long, which is why the convention is written down here rather than
// left to be inferred (issue #469).  Converting is `EndCol: loc.EndCol - 1`.
//
// The RENDERER measures the underline in terminal CELLS rather than in bytes,
// so the carets line up with what a terminal draws even when the span holds
// multi-byte, East Asian wide, or combining characters -- see displayWidth in
// renderer.go.  The byte convention here is about how a caller ADDRESSES the
// source; it is not the unit the carets are counted in.
type Span struct {
	File   string // path for reading source; display name if unreadable
	Line   int    // 1-based line number
	Col    int    // 1-based start byte column
	EndCol int    // 1-based end byte column, INCLUSIVE (0 = auto-detect from source)
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
