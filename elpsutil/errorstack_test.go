// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestDumpErrorStack(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("trace.lisp", `(defun boom () (error 'boom "it failed"))
(boom)`)
	require.True(t, rc.IsError(), "%v", rc)
	lerr := lisp.GoError(rc)

	var want bytes.Buffer
	_, err := (*lisp.ErrorVal)(rc).WriteTrace(&want)
	require.NoError(t, err)
	want.WriteByte('\n')
	require.Contains(t, want.String(), "trace.lisp:1:")

	t.Run("error value", func(t *testing.T) {
		var got bytes.Buffer
		elpsutil.DumpErrorStack(&got, lerr)
		assert.Equal(t, want.String(), got.String())
	})
	t.Run("wrapped error value", func(t *testing.T) {
		var got bytes.Buffer
		elpsutil.DumpErrorStack(&got, fmt.Errorf("loading: %w", lerr))
		assert.Equal(t, want.String(), got.String())
	})
	t.Run("other error", func(t *testing.T) {
		var got bytes.Buffer
		elpsutil.DumpErrorStack(&got, errors.New("plain"))
		assert.Empty(t, got.String())
	})
	t.Run("nil error", func(t *testing.T) {
		var got bytes.Buffer
		elpsutil.DumpErrorStack(&got, nil)
		assert.Empty(t, got.String())
	})
}
