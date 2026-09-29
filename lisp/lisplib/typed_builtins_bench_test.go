// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// BenchmarkTypedBuiltins calls the stdlib builtins whose arguments are
// decoded by lisp.Func1/Func2, ArgReader or a custom ArgDecoder, 100 times
// each per iteration, on small values so decoding is a visible share.
func BenchmarkTypedBuiltins(b *testing.B) {
	for name, body := range map[string]string{
		"string": `(string:lowercase "AbC") (string:trim "xax" "x") (string:has-prefix? "ab" "a") (string:contains? "ab" "b") (string:join '("a" "b") ",") (string:trim-space " a ")`,
		"time":   `(time:time< t0 t1) (time:time-add t0 d) (time:duration-ms d) (time:format-rfc3339 t0)`,
		"base64": `(base64:encode "hello") (base64:decode "aGVsbG8=")`,
		"math":   `(math:floor 1.5) (math:sqrt 4) (math:log 2 8) (math:atan 1 1)`,
	} {
		env := lisp.NewEnv(nil)
		require.NoError(b, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()), lisp.WithMaxSteps(1<<62))))
		require.NoError(b, lisp.GoError(lisplib.LoadLibrary(env)))
		require.NoError(b, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
		require.NoError(b, lisp.GoError(env.LoadString("setup", `(set 't0 (time:parse-rfc3339 "2023-01-15T10:30:00Z")) (set 't1 (time:parse-rfc3339 "2024-01-15T10:30:00Z")) (set 'd (time:parse-duration "1h"))`)))
		prog, err := env.Runtime.Reader.Read("bench", bytes.NewReader([]byte(`(dotimes (i 100) `+body+`)`)))
		require.NoError(b, err)
		b.Run(name, func(b *testing.B) {
			b.ReportAllocs()
			for range b.N {
				for _, e := range prog {
					if rc := env.Eval(e); rc.Type == lisp.LError {
						b.Fatal(rc)
					}
				}
			}
		})
	}
}
