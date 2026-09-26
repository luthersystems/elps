// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func timeTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(lisplib.LoadLibrary(env)))
	require.NoError(t, lisp.GoError(env.InPackage(lisp.String(lisp.DefaultUserPackage))))
	return env
}

// time-add reported a native second argument that is not a duration as
// "argument is not a time: #<native value: libtime.ownedTime>", blaming the
// wrong type and printing a time while claiming it was not one.  The
// comparisons printed a whole non-native second argument where they print
// only the type of a bad first one.
func TestTimeArgumentErrorsNameTheRightType(t *testing.T) {
	env := timeTestEnv(t)
	for _, tc := range []struct{ src, want string }{
		{`(time:time-add (time:utc-now) (time:utc-now))`, "argument is not a duration: #<native value: libtime.ownedTime>"},
		{`(time:time-add (time:utc-now) 5)`, "argument is not a duration: int"},
		{`(time:time-add 5 (time:time-from (time:utc-now) (time:utc-now)))`, "argument is not a time: int"},
		{`(time:time= (time:utc-now) 5)`, "argument is not a time: int"},
		{`(time:time< (time:utc-now) "x")`, "argument is not a time: string"},
		{`(time:time> (time:utc-now) '(1))`, "argument is not a time: list"},
		{`(time:time= 5 (time:utc-now))`, "argument is not a time: int"},
		// A native that is not a time is named by value, as for the first
		// argument.
		{`(time:time< (time:utc-now) (time:time-from (time:utc-now) (time:utc-now)))`, "argument is not a time: #<native value: time.Duration>"},
	} {
		t.Run(tc.src, func(t *testing.T) {
			res := env.LoadString("test", tc.src)
			require.Equal(t, lisp.LError, res.Type, "%v", res)
			assert.Contains(t, lisp.GoError(res).Error(), tc.want)
		})
	}
}
