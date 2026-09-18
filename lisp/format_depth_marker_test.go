// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestFormatStringPrintsDepthMarkerRatherThanErroring pins that the two
// printing builtins answer the same way for the same value.
//
// Rendering has always had its own fixed 1024-level limit, which substitutes
// `#<depth-limit>` for the subtree below it; format-string then re-walked the
// value it had just printed against the much larger value-depth limit and
// raised if that one was exceeded.  So the same value printed fine through
// debug-print and errored through format-string, and the marker -- the whole
// point of which is that printing a deep value is not a failure -- could only
// ever be seen for values between the two limits.  The marker is the rule for
// both now.
//
// The runtime's value-depth limit is lowered rather than building a value
// past the 1,000,000 default: the check that was removed read exactly this
// setting, so a value above it is the input that used to error.
func TestFormatStringPrintsDepthMarkerRatherThanErroring(t *testing.T) {
	const limit = 1024
	env := newLimitTestEnv(t)
	env.Runtime.MaxValueDepth = limit
	var stderr bytes.Buffer
	env.Runtime.Stderr = &stderr

	deep := lisp.Int(7)
	for range 2 * limit {
		deep = lisp.SExpr([]*lisp.LVal{deep})
	}
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("deep"), deep)))

	want := strings.Repeat("(", 1024) + "#<depth-limit>" + strings.Repeat(")", 1024)

	got := env.LoadString("format-depth.lisp", `(format-string "{}" deep)`)
	require.NoError(t, lisp.GoError(got), "%v", got)
	require.Equal(t, lisp.LString, got.Type)
	assert.Contains(t, got.Str, "#<depth-limit>")
	assert.Equal(t, want, got.Str)

	printed := env.LoadString("format-depth.lisp", `(debug-print deep)`)
	require.NoError(t, lisp.GoError(printed), "%v", printed)
	assert.Equal(t, want+"\n", stderr.String(), "debug-print and format-string must render the same value the same way")

	// The copier still enforces the value-depth limit: this is a rendering
	// rule, not a removal of the limit itself.
	copied := env.LoadString("format-depth.lisp", `(copy deep)`)
	require.Equal(t, lisp.LError, copied.Type, "%v", copied)
	assert.Contains(t, copied.String(), "value nesting depth exceeds maximum: 1024")
}
