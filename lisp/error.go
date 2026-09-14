// Copyright © 2018 The ELPS authors

package lisp

import (
	"bufio"
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
	if e == nil {
		log.Printf("elps: ErrorVal rendering called on nil receiver; returning sentinel")
		return nilErrorMessage
	}
	limit := DefaultMaxAlloc
	var budget renderBudget
	if stack := (*LVal)(e).CallStack(); stack != nil {
		if stack.renderLimit > 0 {
			limit = stack.renderLimit
		}
		budget = newRenderBudget(limit, stack.renderContext)
	} else {
		budget = newRenderBudget(limit, nil)
	}
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

// WriteTrace writes the error and a stack trace to w.
//
// Defensive: a nil receiver writes the nilErrorMessage sentinel rather than
// panicking. This keeps callers safe even when fed a corrupted LError pointer.
func (e *ErrorVal) WriteTrace(w io.Writer) (int, error) {
	if e == nil {
		log.Printf("elps: ErrorVal.WriteTrace called on nil receiver; emitting sentinel")
		bw := bufio.NewWriter(w)
		n, err := bw.WriteString(nilErrorMessage + "\n")
		if err != nil {
			return n, err
		}
		return n, bw.Flush()
	}
	bw := bufio.NewWriter(w)
	var n int
	var err error
	wrote := func(_n int, _err error) bool {
		n += _n
		err = _err
		return err == nil
	}
	if !wrote(bw.WriteString(e.Error())) {
		return n, err
	}
	if !wrote(bw.WriteString("\n")) {
		return n, err
	}
	stack := (*LVal)(e).CallStack()
	if stack != nil {
		if !wrote(stack.DebugPrint(bw)) {
			return n, err
		}
		if len(stack.GoStack) > 0 {
			if !wrote(bw.WriteString("\nGo stack trace (panic origin):\n")) {
				return n, err
			}
			if !wrote(bw.Write(stack.GoStack)) {
				return n, err
			}
		}
	}
	return n, bw.Flush()
}
