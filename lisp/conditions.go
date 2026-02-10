// Copyright © 2024 The ELPS authors

package lisp

// Parse error condition names. These are stable API for programmatic
// error classification in LSP and tooling integrations.
const (
	CondParseError          = "parse-error"
	CondScanError           = "scan-error"
	CondUnmatchedSyntax     = "unmatched-syntax"
	CondMismatchedSyntax    = "mismatched-syntax"
	CondInvalidSymbol       = "invalid-symbol"
	CondInvalidOctalLiteral = "invalid-octal-literal"
	CondInvalidHexLiteral   = "invalid-hex-literal"
	CondOverflow            = "integer-overflow-error"
)
