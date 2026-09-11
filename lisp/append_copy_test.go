// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestAppendWithoutValuesCopiesSequenceStorage(t *testing.T) {
	for _, source := range []string{"list", "vector"} {
		for _, result := range []string{"list", "vector"} {
			t.Run(source+" to "+result, func(t *testing.T) {
				src := fmt.Sprintf("(%s 20 10)", source)
				out := fmt.Sprintf("(append '%s src)", result)
				elpstest.RunTestSuite(t, elpstest.TestSuite{
					{"result mutation preserves source", elpstest.TestSequence{
						{fmt.Sprintf("(let* ((src %s) (out %s)) (stable-sort < out) (list (= (first src) 20) (= (first out) 10)))", src, out), `'(true true)`, ``},
					}},
					{"source mutation preserves result", elpstest.TestSequence{
						{fmt.Sprintf("(let* ((src %s) (out %s)) (stable-sort < src) (list (= (first src) 10) (= (first out) 20)))", src, out), `'(true true)`, ``},
					}},
				})
			})
		}
	}
	// This is a shallow copy: nested values remain references, as documented.
	elpstest.RunTestSuite(t, elpstest.TestSuite{
		{"nested element remains shared", elpstest.TestSequence{
			{`(let* ((m (sorted-map 'n 1)) (src (vector m)) (out (append 'vector src))) (assoc! (first out) 'n 2) (get (first src) 'n))`, `2`, ``},
		}},
	})
}

func TestAppendEmptyBytesCopiesStorage(t *testing.T) {
	// The host can mutate Bytes(), so a new bytes result must own its buffer
	// even when no bytes were appended. Exercise each accepted empty shape.
	for _, expr := range []string{
		`(append 'bytes src)`,
		`(append-bytes src "")`,
		`(append-bytes src (to-bytes ""))`,
		`(append-bytes src ())`,
	} {
		t.Run(expr, func(t *testing.T) {
			env, err := (&elpstest.Runner{}).NewEnv(t)
			require.NoError(t, err)
			src := env.LoadString("bytes", `(set 'src (to-bytes "AB"))`)
			require.Equal(t, lisp.LBytes, src.Type, "%v", src)
			out := env.LoadString("bytes", expr)
			require.Equal(t, lisp.LBytes, out.Type, "%v", out)
			require.Equal(t, []byte("AB"), out.Bytes())
			out.Bytes()[0] = 'X'
			require.Equal(t, []byte("AB"), src.Bytes())
			src.Bytes()[1] = 'Y'
			require.Equal(t, []byte("XB"), out.Bytes())
		})
	}
}

func TestAppendBytesAllocationLimit(t *testing.T) {
	for _, tc := range []struct {
		addition string
		want     string
	}{
		{`""`, "AB"},
		{`(to-bytes "")`, "AB"},
		{`()`, "AB"},
		{`(vector)`, "AB"},
		{`"C"`, "ABC"},
		{`(to-bytes "C")`, "ABC"},
		{`'(67)`, "ABC"},
		{`(vector 67)`, "ABC"},
	} {
		for _, limit := range []int{len(tc.want) - 1, len(tc.want)} {
			t.Run(fmt.Sprintf("%s limit %d", tc.addition, limit), func(t *testing.T) {
				env, err := (&elpstest.Runner{}).NewEnv(t)
				require.NoError(t, err)
				src := env.LoadString("bytes", `(set 'src (to-bytes "AB"))`)
				require.Equal(t, lisp.LBytes, src.Type)
				env.Runtime.MaxAlloc = limit
				out := env.LoadString("bytes", fmt.Sprintf(`(append-bytes src %s)`, tc.addition))
				require.False(t, lisp.IsInternalPanic(out), "%v", out)
				require.Equal(t, []byte("AB"), src.Bytes())
				if limit < len(tc.want) {
					require.Equal(t, lisp.LError, out.Type, "%v", out)
					require.Contains(t, out.String(), fmt.Sprintf("allocation size %d exceeds maximum (%d)", len(tc.want), limit))
					return
				}
				require.Equal(t, lisp.LBytes, out.Type, "%v", out)
				require.Equal(t, []byte(tc.want), out.Bytes())
			})
		}
	}
}
