// Copyright © 2026 The ELPS authors

package elpsutil

import (
	"bytes"
	"errors"
	"io"

	"github.com/luthersystems/elps/lisp"
)

// DumpErrorStack writes the trace of the first *lisp.ErrorVal in err's
// chain to w, followed by a newline.  It writes the trace with
// lisp.ErrorVal.WriteTrace, so the trace has the error's output limit.  For
// an err with no *lisp.ErrorVal in its chain, a nil err included, it writes
// nothing.  It ignores write errors.
//
// To show a trace beside the output of the code that failed, as a test
// does, write it to env.Runtime.Stderr.
func DumpErrorStack(w io.Writer, err error) {
	var lerr *lisp.ErrorVal
	if !errors.As(err, &lerr) {
		return
	}
	var buf bytes.Buffer
	_, _ = lerr.WriteTrace(&buf)
	buf.WriteByte('\n')
	_, _ = w.Write(buf.Bytes())
}
