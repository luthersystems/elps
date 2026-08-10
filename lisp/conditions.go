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
	CondInvalidFloat        = "invalid-float"
	CondInvalidString       = "invalid-string"
	CondOverflow            = "integer-overflow-error"
)

// Evaluation limit condition names.
const (
	CondContextCancelled  = "context-cancelled"
	CondStepLimitExceeded = "step-limit-exceeded"

	// CondEvalNestingExceeded reports that the evaluator recursed into
	// itself more deeply than Runtime.MaxEvalNesting allows.  It is the
	// recoverable substitute for a Go stack overflow, which is a
	// runtime.throw that neither recover() nor handler-bind can intercept.
	CondEvalNestingExceeded = "eval-nesting-exceeded"

	// CondSleepLimitExceeded reports that a requested sleep was longer than
	// the caller is allowed to sleep for, and was refused WITHOUT sleeping.
	// It is distinct from context-cancelled: nothing was cancelled and no
	// time passed, the request was rejected on entry.  See
	// DefaultMaxSleep and Runtime.MaxSleep.
	CondSleepLimitExceeded = "sleep-limit-exceeded"
)

// CondInternalPanic is the condition type of an error produced by recovering
// a Go panic that escaped a builtin, special operator, or any other host
// code called during evaluation.
//
// It is deliberately NOT an ordinary error condition.  A panic means host Go
// code hit a bug — a nil dereference, an out-of-range index, a failed
// invariant — and left its data structures in an unknown state.  Treating
// that as a routine, catchable lisp error lets `ignore-errors` and a
// catch-all `handler-bind` swallow it silently, so a genuine host defect
// looks exactly like `(error 'my-condition "...")` and the program keeps
// running on top of it.
//
// Accordingly, `ignore-errors` and the catch-all `condition` handler
// specifier do not intercept this condition; it propagates to the caller.  A
// handler that genuinely wants to intercept host panics must name the
// condition explicitly:
//
//	(handler-bind ((internal-panic (lambda (c &rest args) ...)))
//	    (risky))
//
// CondMissingArgument reports that a builtin was invoked with fewer argument
// cells than it reads. The evaluator supplies one cell per declared formal, so
// this cannot arise from lisp; it means an embedder bound the builtin to
// formals declaring fewer arguments than the Go function requires. Raised as
// an ordinary condition so a caller can handler-bind it, rather than panicking
// in the embedder's process.
const CondMissingArgument = "missing-argument"

const CondInternalPanic = "internal-panic"
