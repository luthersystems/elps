// Copyright © 2018 The ELPS authors

package elpstest

import (
	"bytes"
	"io"
	"testing"
)

type Logger struct {
	t   testing.TB
	buf []byte
}

var _ io.Writer = (*Logger)(nil)

func NewLogger(t testing.TB) *Logger {
	return &Logger{
		t: t,
	}
}

func (log *Logger) Write(b []byte) (int, error) {
	log.buf = append(log.buf, b...)
	i := bytes.Index(log.buf, []byte("\n"))
	if i < 0 {
		return len(b), nil
	}
	log.t.Log(string(log.buf[:i])) // slice does not include \n
	log.buf = log.buf[i+1:]        // slice dos not include \n
	return len(b), nil
}

func (log *Logger) Flush() {
	if len(log.buf) == 0 {
		return
	}
	log.t.Log(string(log.buf))
	log.buf = nil
}
