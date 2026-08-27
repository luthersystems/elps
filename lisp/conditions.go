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
// specifier do not intercept this condition; it propagates to the caller.
// For the same reason an error raised by an `with-cleanup` cleanup form
// does not mask this condition, though it does replace an ordinary one.  A
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

// CondModifyLiteral reports an attempt to modify a sealed program literal in
// place.  Quoted data, macro arguments and defun bodies are part of the
// program text, shared by every environment evaluating the same parse
// (lisp/seal.go), so the in-place mutators that could write them —
// stable-sort on a sealed list, and the (slice 'vector ...) / (append
// 'vector ...) forms that would wrap or write a sealed list's backing
// array — raise this condition instead.  It is an ordinary catchable
// condition: handler-bind can name it and ignore-errors swallows it.  The
// remedy the message names is `(copy x)`, which returns a fresh, fully
// mutable deep copy.
//
// The empty list is the deliberate carve-out: builtins such as cdr, rest and
// keys return the shared (sealed) empty-list value, so erroring on it would
// make `(stable-sort < (rest xs))` fail only when xs happens to have fewer
// than two elements — a data-dependent error on correct runtime code.  An
// empty list has no storage to write or alias, so the guarded sites accept
// it and return fresh storage.
const CondModifyLiteral = "modify-literal-error"
