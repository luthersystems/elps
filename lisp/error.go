// Copyright © 2018 The ELPS authors

package lisp

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"log"

	"github.com/luthersystems/elps/parser/token"
)

// ErrorVal implements the error interface so that errors can be first class lisp
// objects.  The error message is stored in the Str field while contextual
// information (e.g. call stack) can be stored in the Cells slice.
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
// “error” it wil be printed preceding the error message.  Otherwise, the
// name of the function that generated the error will be printed preceding the
// error, if the function can be determined.
//
// Defensive: a nil receiver returns the nilErrorMessage sentinel rather than
// dereferencing. This matters because diagnostic code that renders errors may
// be invoked from a deferred recover handler where the LVal pointer can be
// stale or zeroed.
func (e *ErrorVal) Error() string {
	if e == nil {
		log.Printf("elps: ErrorVal.Error called on nil receiver; returning sentinel")
		return nilErrorMessage
	}
	loc, _ := (*LVal)(e).Source()
	return fmt.Sprintf("%s: %s", &loc, e.baseMessage())
}

// Source returns a copy of the error's originating source location.  It has
// the same semantics as (*LVal).Source: the boolean reports whether the
// error carries a location, the returned value is a private copy, and a nil
// receiver reports no location.
func (e *ErrorVal) Source() (token.Location, bool) {
	return (*LVal)(e).Source()
}

func (e *ErrorVal) baseMessage() string {
	if e == nil {
		log.Printf("elps: ErrorVal.baseMessage called on nil receiver; returning sentinel")
		return nilErrorMessage
	}
	msg := e.ErrorMessage()
	if e.Str != "error" {
		return fmt.Sprintf("%s: %s", e.Str, msg)
	}
	fname := e.FunName()
	if fname == "" {
		return msg
	}
	return fmt.Sprintf("%s: %s", fname, msg)
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
func (e *ErrorVal) ErrorMessage() (msg string) {
	if e == nil {
		log.Printf("elps: ErrorVal.ErrorMessage called on nil receiver; returning sentinel")
		return nilErrorMessage
	}
	defer func() {
		if r := recover(); r != nil {
			log.Printf("elps: ErrorVal.ErrorMessage recovered panic during Cells[0].Native type switch: %v; returning sentinel %q", r, corruptedNativeMessage)
			msg = corruptedNativeMessage
		}
	}()
	if len(e.Cells) > 0 && e.Cells[0] != nil {
		switch v := e.Cells[0].Native.(type) {
		case error:
			if v != nil {
				return v.Error()
			}
		}
	}

	return errorCellMessage(e.Cells)
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

// errorCellMessage renders the cells of an LError as a human-readable
// message. Nil cells are rendered as "<nil>" rather than dereferenced.
func errorCellMessage(ecells []*LVal) string {
	var buf bytes.Buffer
	for i, cell := range ecells {
		if i > 0 {
			buf.WriteString(" ")
		}
		if cell == nil {
			log.Printf("elps: errorCellMessage skipping nil cell at index %d (LError has malformed Cells slice)", i)
			buf.WriteString("<nil>")
			continue
		}
		if cell.Type == LString {
			buf.WriteString(cell.Str)
		} else {
			buf.WriteString(cell.String())
		}
	}
	return buf.String()
}
