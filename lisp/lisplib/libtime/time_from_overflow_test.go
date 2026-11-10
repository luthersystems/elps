// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"math"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestTimeFromRefusesADifferenceADurationCannotHold pins that time-from
// returns an error when end - start does not fit a duration
// (luthersystems/elps#943).
//
// A duration is an int64 count of nanoseconds, about 292 years.  Go's
// Time.Sub returns the largest or smallest duration for a difference past
// that, with no error.  A phylum that computed the age of a 300-year-old date
// got 292 years and could not tell.
func TestTimeFromRefusesADifferenceADurationCannotHold(t *testing.T) {
	env := timeTestEnv(t)
	for _, src := range []string{
		`(time:time-from (time:parse-rfc3339 "1700-01-01T00:00:00Z") (time:parse-rfc3339 "2000-01-01T00:00:00Z"))`,
		`(time:time-from (time:parse-rfc3339 "2000-01-01T00:00:00Z") (time:parse-rfc3339 "1700-01-01T00:00:00Z"))`,
		`(time:time-from (time:parse-rfc3339 "0001-01-01T00:00:00Z") (time:parse-rfc3339 "9999-12-31T23:59:59Z"))`,
	} {
		t.Run(src, func(t *testing.T) {
			res := env.LoadString("test", src)
			require.Equal(t, lisp.LError, res.Type, "%v", res)
			assert.Contains(t, lisp.GoError(res).Error(), "the difference does not fit a duration")
		})
	}
}

// TestTimeFromKeepsTheExtremeDurations pins that a difference of exactly
// the largest or the smallest duration is a result and not an error.  Only a
// difference past the range fails.
func TestTimeFromKeepsTheExtremeDurations(t *testing.T) {
	env := timeTestEnv(t)
	start := time.Date(2000, 1, 1, 0, 0, 0, 0, time.UTC)
	for _, tc := range []struct {
		name string
		d    time.Duration
		ok   bool
	}{
		{"max", math.MaxInt64, true},
		{"min", math.MinInt64, true},
		{"max-1", math.MaxInt64 - 1, true},
		{"min+1", math.MinInt64 + 1, true},
		{"290 years", 290 * 365 * 24 * time.Hour, true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env.PutGlobal(lisp.Symbol("start"), libtime.Time(start))
			env.PutGlobal(lisp.Symbol("end"), libtime.Time(start.Add(tc.d)))
			res := env.LoadString("test", `(time:duration-ns (time:time-from start end))`)
			require.Equal(t, lisp.LInt, res.Type, "%v", res)
			assert.Equal(t, int(tc.d), res.Int)
		})
	}
	// One nanosecond past each extreme.
	for _, tc := range []struct {
		name       string
		start, end time.Time
	}{
		{"max+1", start, start.Add(math.MaxInt64).Add(1)},
		{"min-1", start, start.Add(math.MinInt64).Add(-1)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			env.PutGlobal(lisp.Symbol("start"), libtime.Time(tc.start))
			env.PutGlobal(lisp.Symbol("end"), libtime.Time(tc.end))
			res := env.LoadString("test", `(time:time-from start end)`)
			require.Equal(t, lisp.LError, res.Type, "%v", res)
		})
	}
}
