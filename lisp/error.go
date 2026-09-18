// Copyright © 2018 The ELPS authors

package lisp

import (
	"context"
	"io"
	"log"

	"github.com/luthersystems/elps/parser/token"
)

// ErrorVal implements the error interface so that errors can be first class lisp
// objects. Rendering honours the originating runtime output limit and context,
// using #<truncated> on exhaustion. The condition name is stored in Str,
// message/data in Cells, and the captured call stack in Native.
type ErrorVal LVal

// nilErrorMessage is the sentinel returned by the rendering chain when a nil
// *ErrorVal receiver is observed. It exists so that diagnostic paths that
// reach the rendering chain with a corrupted value never crash the caller —
// the caller still gets a non-empty string describing what went wrong.
const nilErrorMessage = "<nil error>"

// corruptedNativeMessage is the sentinel returned by ErrorMessage when the
// type switch over Cells[0].Native panics (e.g. due to a stale itab from a
// concurrent mutation or an unsafe pointer cast). The intent is to surface
// the corruption to the operator without taking down the process.
const corruptedNativeMessage = "<corrupted error: cell native deref panicked>"

// Error implements the error interface.  When the error condition is not
// “error” it will be printed preceding the error message.  Otherwise, the
// name of the function that generated the error will be printed preceding the
// error, if the function can be determined.
//
// Defensive: a nil receiver returns the nilErrorMessage sentinel rather than
// dereferencing. This matters because diagnostic code that renders errors may
// be invoked from a deferred recover handler where the LVal pointer can be
// stale or zeroed.
func (e *ErrorVal) Error() string {
	return e.render(false)
}

func (e *ErrorVal) render(message bool) string {
	return e.renderContext(nil, message) //nolint:staticcheck // nil selects the error's captured cancellation context
}

func (e *ErrorVal) renderContext(ctx context.Context, message bool) string {
	if e == nil {
		log.Printf("elps: ErrorVal rendering called on nil receiver; returning sentinel")
		return nilErrorMessage
	}
	limit, ctx := e.renderPolicy(ctx)
	budget := newRenderBudget(limit, ctx)
	s, ok := (*LVal)(e).boundedRender(limit, &budget, message)
	if !ok {
		return truncatedRender(s, limit)
	}
	return s
}

// Unwrap returns the original Go error carried by this condition, if any.
// GoError still returns the ErrorVal so its condition and source remain available.
func (e *ErrorVal) Unwrap() error {
	if e == nil || len(e.Cells) == 0 || e.Cells[0] == nil {
		return nil
	}
	err, _ := e.Cells[0].Native.(error)
	return err
}

// Source returns a copy of the error's originating source location.  It has
// the same semantics as (*LVal).Source: the boolean reports whether the
// error carries a location, the returned value is a private copy, and a nil
// receiver reports no location.
func (e *ErrorVal) Source() (token.Location, bool) {
	return (*LVal)(e).Source()
}

// Condition returns the error condition name (e.g., "parse-error",
// "unmatched-syntax"). This is the programmatic error classification
// stored in the LVal.Str field for LError values.
func (e *ErrorVal) Condition() string {
	if e == nil {
		return ""
	}
	return e.Str
}

// FunName returns the qualified name of function on the top of the call stack
// when the error occurred.
func (e *ErrorVal) FunName() string {
	if e == nil {
		return ""
	}
	stack := (*LVal)(e).CallStack()
	if stack == nil {
		return ""
	}
	top := stack.Top()
	if top == nil {
		return ""
	}
	return top.QualifiedFunName(DefaultUserPackage)
}

// ErrorMessage returns the underlying message in the error.
//
// Defensive: a downstream consumer reported a SIGSEGV inside the type switch
// over Cells[0].Native when the interface header was corrupted (stale itab
// → invalid pointer deref during the type assertion). The deferred recover
// ensures the diagnostic pipeline always produces a renderable string even
// when the underlying LVal is malformed; well-formed errors are unaffected.
// The recovered panic is logged so the operator sees that something is
// corrupting the error's Cells[0].Native — silently swallowing would hide a
// real bug.
func (e *ErrorVal) ErrorMessage() string {
	return e.render(true)
}

// ErrorMessageContext returns the underlying error message bounded by the
// originating output limit and ctx. A nil context uses the captured context.
// Cancellation or exhaustion substitutes a fitting #<truncated> marker.
func (e *ErrorVal) ErrorMessageContext(ctx context.Context) string {
	return e.renderContext(ctx, true)
}

func (e *ErrorVal) renderPolicy(ctx context.Context) (int, context.Context) {
	limit := DefaultMaxAlloc
	if e == nil {
		return limit, ctx
	}
	if stack := (*LVal)(e).CallStack(); stack != nil {
		if stack.renderLimit > 0 {
			limit = stack.renderLimit
		}
		if ctx == nil {
			ctx = liveRenderContext(stack.renderContext)
		}
	}
	return limit, ctx
}

// WriteTrace writes the error and stack trace under the captured output limit
// and context. The entire trace shares one budget, including any Go stack.
// A nil receiver writes the nilErrorMessage sentinel rather than panicking.
func (e *ErrorVal) WriteTrace(w io.Writer) (int, error) {
	return e.WriteTraceContext(nil, w) //nolint:staticcheck // nil selects the error's captured cancellation context
}

// WriteTraceContext writes the error, frames, and Go stack under one output
// budget and ctx. Cancellation or exhaustion emits a fitting #<truncated>
// marker. A nil context uses the error's captured context.
func (e *ErrorVal) WriteTraceContext(ctx context.Context, w io.Writer) (int, error) {
	limit, ctx := e.renderPolicy(ctx)
	r := valueRenderer{limit: limit, budget: newRenderBudget(limit, ctx)}
	if e == nil {
		r.text(nilErrorMessage)
		r.text("\n")
		return writeDiagnostic(ctx, w, r.diagnosticText(), limit)
	} else {
		s, ok := (*LVal)(e).boundedRender(limit, &r.budget, false)
		r.text(s)
		r.full = r.full || !ok
	}
	r.text("\n")
	if stack := (*LVal)(e).CallStack(); stack != nil && !r.full {
		r.stack(stack)
		if len(stack.GoStack) > 0 && !r.full {
			r.text("\nGo stack trace (panic origin):\n")
			for data := stack.GoStack; len(data) > 0 && !r.full; {
				n := min(len(data), 4096)
				r.text(string(data[:n]))
				data = data[n:]
			}
		}
	}
	return writeDiagnostic(ctx, w, r.diagnosticText(), limit)
}
