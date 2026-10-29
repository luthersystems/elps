// Copyright © 2026 The ELPS authors

package repl

import (
	"bytes"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// replMethodSource defines a record type and adds a method to it at the top
// level, as a program's file would.
const replMethodSource = `(set 'pt (s:make-record "pt" (s:has-key "x" s:int)))
(s:defmethod pt get-x (o) (o 'x))
((pt :x 3) 'get-x)`

// TestREPLFormsRunAsTheLoad pins that the forms a person types run as the
// program's load, where s:defmethod works (luthersystems/elps#908), and that
// the load ends with the session.
func TestREPLFormsRunAsTheLoad(t *testing.T) {
	t.Run("eval", func(t *testing.T) {
		env := newTestEnv(t)
		var stdout, stderr bytes.Buffer
		require.Equal(t, 0, runEval(env, newConfig(WithEval(replMethodSource)), &stdout, &stderr), stderr.String())
		assert.Equal(t, "3\n", stdout.String())
		assert.False(t, env.Runtime.Loading())
	})
	t.Run("batch", func(t *testing.T) {
		env := newTestEnv(t)
		var stdout, stderr bytes.Buffer
		runBatch(env, newConfig(WithStdin(io.NopCloser(strings.NewReader(replMethodSource+"\n")))), &stdout, &stderr)
		assert.Empty(t, stderr.String())
		assert.True(t, strings.HasSuffix(stdout.String(), "3\n"), stdout.String())
		assert.False(t, env.Runtime.Loading())
	})
}
