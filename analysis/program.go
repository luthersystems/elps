// Copyright © 2026 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/internal/astraw"
	"github.com/luthersystems/elps/lisp"
)

// AnalyzeProgram performs semantic analysis on a sealed lisp.Program.  It is
// Analyze for callers that hold a Program instead of raw parser output, and
// it reads the sealed expressions in place — no Detach, no copying — via the
// internal/astraw accessor (available here because analysis lives in the
// elps module; embedders outside the module have no such bypass).  Analysis
// never mutates the expressions it walks, so the read-only contract on the
// sealed AST is preserved.
func AnalyzeProgram(p lisp.Program, cfg *Config) *Result {
	return Analyze(astraw.Exprs(p), cfg)
}
