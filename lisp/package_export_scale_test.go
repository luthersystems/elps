// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

func newExportEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	return env
}

// Exporting n names in one call cost O(n^2): each name was spliced into the
// sorted list, moving every name after it (321 ms for 10,000 names).  The
// test times 20 times the names, because a wide gap keeps a quadratic cost
// (about 150 times here, with fast memmoves) far from a linear one (about
// 20 to 35 times, with parse and GC noise), so the bound of 80 holds on a busy
// runner.  Each size takes the best of three runs.
func TestExportManyNamesScalesLinearly(t *testing.T) {
	if testing.Short() {
		t.Skip("timing test")
	}
	run := func(n int) time.Duration {
		var b strings.Builder
		b.WriteString("(export")
		// Descending order puts every name at the front of the list, the
		// worst case for an insertion into a sorted slice.
		for i := n - 1; i >= 0; i-- {
			fmt.Fprintf(&b, " 'sym%07d", i)
		}
		b.WriteString(")")
		best := time.Duration(1<<63 - 1)
		for range 3 {
			env := newExportEnv(t)
			start := time.Now()
			v := env.LoadString("export.lisp", b.String())
			best = min(best, time.Since(start))
			require.NotEqual(t, lisp.LError, v.Type, "%v", v)
			require.Equal(t, n, env.Runtime.Package.NumExternals())
		}
		return best
	}
	small, large := run(2000), run(40000)
	ratio := float64(large) / float64(small)
	t.Logf("n=2000 %v, n=40000 %v, ratio %.1f", small, large, ratio)
	require.Less(t, ratio, 80.0, "export of many names scales superlinearly")
}
