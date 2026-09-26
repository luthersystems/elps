package lisplib_test

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// chargeBySizeCases pairs each size-proportional stdlib builtin with an
// expression over a bound value.  The same expression is measured with the
// value bound to a small input and to a 64 KiB one, so the step difference is
// exactly what the builtin charges for size (luthersystems/substrate#543).
var chargeBySizeCases = []struct {
	name string
	expr string
}{
	{"json:load-string", `(json:load-string v)`},
	{"json:load-bytes", `(json:load-bytes (to-bytes v))`},
	{"json:load-message", `(json:load-message m)`},
	{"json:message-bytes", `(json:message-bytes m)`},
	{"json:dump-string", `(json:dump-string s)`},
	{"json:dump-bytes", `(json:dump-bytes s)`},
	{"json:dump-message", `(json:dump-message s)`},
	{"base64:encode", `(base64:encode s)`},
	{"base64:decode", `(base64:decode b64)`},
	{"string:split", `(string:split s ",")`},
	{"string:join", `(string:join (list s s) "")`},
	{"string:repeat", `(string:repeat s 2)`},
	{"string:lowercase", `(string:lowercase s)`},
	{"string:uppercase", `(string:uppercase s)`},
	{"string:trim-space", `(string:trim-space s)`},
	{"string:trim", `(string:trim s "y")`},
	{"string:trim-left", `(string:trim-left s "y")`},
	{"string:trim-right", `(string:trim-right s "y")`},
	{"string:contains?", `(string:contains? s "y")`},
	{"regexp:regexp-match?", `(regexp:regexp-match? "y" s)`},
	{"regexp:regexp-compile", `(regexp:regexp-compile s)`},
}

// newChargeEnv returns a library env counting steps under a budget far larger
// than any case needs, with s bound to n bytes of "x", v to a JSON string of
// that s, m to its JSON message, and b64 to its base64 encoding.
func newChargeEnv(t testing.TB, n int) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	rc := lisp.InitializeUserEnv(env, lisp.WithStderr(&bytes.Buffer{}), lisp.WithMaxSteps(1<<40))
	require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
	rc = lisplib.LoadLibrary(env)
	require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
	rc = env.InPackage(lisp.String(lisp.DefaultUserPackage))
	require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
	s := string(bytes.Repeat([]byte("x"), n))
	env.PutGlobal(lisp.Symbol("s"), lisp.String(s))
	rc = env.LoadString("setup", `
		(set 'v (json:dump-string s))
		(set 'm (json:dump-message s))
		(set 'b64 (to-string (base64:encode s)))`)
	require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
	return env
}

func stepsFor(t *testing.T, env *lisp.LEnv, expr string) int64 {
	t.Helper()
	rc := env.LoadString("case", expr)
	require.NotEqual(t, lisp.LError, rc.Type, "%s: %v", expr, rc)
	return env.Runtime.Steps()
}

func TestStdlibChargesBySize(t *testing.T) {
	const big = 64 << 10
	small, large := newChargeEnv(t, 16), newChargeEnv(t, big)
	for _, tc := range chargeBySizeCases {
		t.Run(tc.name, func(t *testing.T) {
			s0 := stepsFor(t, small, tc.expr)
			s1 := stepsFor(t, large, tc.expr)
			// Every case handles at least the 64 KiB input once.
			require.GreaterOrEqual(t, s1-s0, int64(big>>10),
				"%s: small=%d large=%d steps; size is not charged", tc.expr, s0, s1)
			// Deterministic: the same evaluation charges the same again.
			require.Equal(t, s1, stepsFor(t, large, tc.expr), "%s: charge not deterministic", tc.expr)
		})
	}
}

// BenchmarkStdlibSizeCharge measures the hot path the charge adds to: small
// values (no step added) and a 4 KiB value (a ChargeSteps call), under a step
// limit so the counter is live.
func BenchmarkStdlibSizeCharge(b *testing.B) {
	for _, n := range []int{64, 4 << 10} {
		env := newChargeEnv(b, n)
		prog, err := env.Runtime.Reader.Read("bench", bytes.NewReader([]byte(
			`(dotimes (i 100) (json:load-string v) (string:split s ",") (string:contains? s "y") (base64:encode s))`)))
		require.NoError(b, err)
		b.Run(fmt.Sprintf("bytes=%d", n), func(b *testing.B) {
			b.ReportAllocs()
			for i := 0; i < b.N; i++ {
				for _, e := range prog {
					if rc := env.Eval(e); rc.Type == lisp.LError {
						b.Fatal(rc)
					}
				}
			}
		})
	}
}

// TestStdlibSizeChargeEnforced shows the charge is what an enforced budget
// sees: a budget that admits the small input refuses the large one.
func TestStdlibSizeChargeEnforced(t *testing.T) {
	small, large := newChargeEnv(t, 16), newChargeEnv(t, 1<<20)
	expr := `(json:load-string v)`
	budget := stepsFor(t, small, expr) + 100
	for _, env := range []*lisp.LEnv{small, large} {
		rc := lisp.WithMaxSteps(budget)(env)
		require.NotEqual(t, lisp.LError, rc.Type, "%v", rc)
	}
	require.NotEqual(t, lisp.LError, small.LoadString("case", expr).Type)
	rc := large.LoadString("case", expr)
	require.Equal(t, lisp.LError, rc.Type, "1 MiB json load fit a %d-step budget", budget)
	require.Contains(t, rc.String(), "step")
}

// TestStdlibSizeChargeSmallInputsFree pins the floor: inputs under 1 KiB add
// no steps, so programs over ordinary small values keep their step counts.
func TestStdlibSizeChargeSmallInputsFree(t *testing.T) {
	a, b := newChargeEnv(t, 1), newChargeEnv(t, 1000)
	for _, tc := range chargeBySizeCases {
		if tc.name == "string:join" || tc.name == "string:repeat" || tc.name == "base64:decode" ||
			tc.name == "json:load-string" || tc.name == "json:load-bytes" || tc.name == "json:load-message" ||
			tc.name == "json:message-bytes" || tc.name == "json:dump-string" || tc.name == "json:dump-bytes" ||
			tc.name == "json:dump-message" {
			continue // these double or pad the 1000 bytes past 1 KiB
		}
		require.Equal(t, stepsFor(t, a, tc.expr), stepsFor(t, b, tc.expr), tc.name)
	}
}
