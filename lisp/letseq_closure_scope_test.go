// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"runtime"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// A let* splits its scope after each initializer that creates a closure.
// The new scope used to be sized for every binding still to come, so n
// closure bindings in a row allocated scopes for n, n-1, n-2, ... names:
// O(n^2) memory (572 MB at n=5000), although each scope gets one binding
// before the next closure splits it again. This pins the memory to grow
// linearly: ten times the bindings must cost well under 100 times the bytes.
func TestLetSeqClosureBindingsAllocateLinearly(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	src := func(n int) string {
		var b strings.Builder
		b.WriteString("(let* (")
		for i := range n {
			fmt.Fprintf(&b, "(f%d (lambda () %d)) ", i, i)
		}
		fmt.Fprintf(&b, ") (f%d))", n-1)
		return b.String()
	}
	measure := func(n int) uint64 {
		code := src(n)
		var before, after runtime.MemStats
		runtime.GC()
		runtime.ReadMemStats(&before)
		v := env.LoadString("letseq.lisp", code)
		runtime.ReadMemStats(&after)
		require.NotEqual(t, lisp.LError, v.Type, "%v", v)
		require.Equal(t, n-1, v.Int)
		return after.TotalAlloc - before.TotalAlloc
	}
	small, large := measure(200), measure(2000)
	ratio := float64(large) / float64(small)
	t.Logf("bytes: n=200 %d, n=2000 %d, ratio %.1f", small, large, ratio)
	require.Less(t, ratio, 25.0, "let* closure bindings allocate superlinearly")
}
