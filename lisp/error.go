// Copyright © 2018 The ELPS authors

package lisp

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
)

// ErrorVal implements the error interface so that errors can be first class lisp
// objects.  The error message is stored in the Str field while contextual
// information (e.g. call stack) can be stored in the Cells slice.
type ErrorVal LVal

// Error implements the error interface.  When the error condition is not
// “error” it wil be printed preceding the error message.  Otherwise, the
// name of the function that generated the error will be printed preceding the
// error, if the function can be determined.
func (e *ErrorVal) Error() string {
	if e.Source != nil {
		return fmt.Sprintf("%s: %s", e.Source, e.baseMessage())
	}
	return e.baseMessage()
}

func (e *ErrorVal) baseMessage() string {
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
	return e.Str
}

// FunName returns the qualified name of function on the top of the call stack
// when the error occurred.
func (e *ErrorVal) FunName() string {
	stack := (*LVal)(e).CallStack()
	if stack == nil {
		return ""
	}
	if stack.Top() == nil {
		return ""
	}
	return stack.Top().QualifiedFunName(DefaultUserPackage)
}

// ErrorMessage returns the underlying message in the error.
func (e *ErrorVal) ErrorMessage() string {
	if len(e.Cells) > 0 {
		switch v := e.Cells[0].Native.(type) {
		case error:
			return v.Error()
		}
	}

	return errorCellMessage(e.Cells)
}

// WriteTrace writes the error and a stack trace to w
func (e *ErrorVal) WriteTrace(w io.Writer) (int, error) {
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
	}
	return n, bw.Flush()
}

func errorCellMessage(ecells []*LVal) string {
	var buf bytes.Buffer
	for i, cell := range ecells {
		if i > 0 {
			buf.WriteString(" ")
		}
		if cell.Type == LString {
			buf.WriteString(cell.Str)
		} else {
			buf.WriteString(cell.String())
		}
	}
	return buf.String()
}
