// Copyright © 2026 The ELPS authors

package debugrepl

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestLocalsResponseRenderBudget(t *testing.T) {
	env := lisp.NewEnv(lisp.NewEnv(nil))
	env.Runtime.MaxAlloc = 64
	for _, name := range []string{"a", "b", "c"} {
		env.Put(lisp.Symbol(name), lisp.String(strings.Repeat("x", 40)))
	}
	var out bytes.Buffer
	showLocals(&out, env, nil)
	require.Contains(t, out.String(), "#<truncated>")
	require.Equal(t, 2, strings.Count(out.String(), "\n"))
}
