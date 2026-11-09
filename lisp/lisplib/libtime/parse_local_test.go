// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"testing"
	"time"
	_ "time/tzdata" // the test needs a zone with DST on every host

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// A parsed time must not carry the host's time zone.
//
// time.Parse gives the result the host's time.Local when the offset in the
// text equals the host zone's offset at that instant.  Arithmetic on that
// value then follows the host's DST rules, so one program gave different
// text on hosts in different zones (luthersystems/elps#933).  The parse
// builtins keep the offset the text states and nothing else.
func TestParseRFC3339DoesNotDependOnTheHostZone(t *testing.T) {
	// 4320h after a January instant is in July.  New York is at -05:00 in
	// January and -04:00 in July, so a result that kept New York's rules
	// reads 11:30:00-04:00.
	const want = "2023-07-14T10:30:00-05:00"

	ny, err := time.LoadLocation("America/New_York")
	require.NoError(t, err)
	tokyo, err := time.LoadLocation("Asia/Tokyo")
	require.NoError(t, err)

	saved := time.Local
	t.Cleanup(func() { time.Local = saved })

	for _, parse := range []string{"parse-rfc3339", "parse-rfc3339-nano"} {
		for _, host := range []*time.Location{time.UTC, ny, tokyo} {
			t.Run(parse+"/"+host.String(), func(t *testing.T) {
				time.Local = host
				env := timeTestEnv(t)
				src := "(time:format-rfc3339 (time:time-add (time:" + parse +
					` "2023-01-15T10:30:00-05:00") (time:parse-duration "4320h")))`
				got := env.LoadString("test", src)
				require.NotEqual(t, lisp.LError, got.Type, "%v", got)
				assert.Equal(t, want, got.Str)
			})
		}
	}
}
