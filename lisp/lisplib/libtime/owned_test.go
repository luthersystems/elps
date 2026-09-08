// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"encoding/json"
	"reflect"
	"testing"
	"time"
	_ "time/tzdata"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/stretchr/testify/require"
)

func TestTimeOwnsInputAndExportedLocations(t *testing.T) {
	zone := time.FixedZone("input", 3600)
	input := time.Date(2026, 1, 2, 3, 4, 5, 123, zone)
	value := libtime.Time(input)
	_, raw := value.Native.(time.Time)
	require.False(t, raw, "library values must not expose raw Go time storage")
	*zone = *time.FixedZone("changed-input", -3600)
	first, ok := libtime.Get(value)
	require.True(t, ok)
	require.Equal(t, "2026-01-02T03:04:05.000000123+01:00", first.Format(time.RFC3339Nano))
	require.Equal(t, "input", first.Location().String())
	*first.Location() = *time.FixedZone("changed-output", 7200)
	second, ok := libtime.Get(value)
	require.True(t, ok)
	require.Equal(t, "2026-01-02T03:04:05.000000123+01:00", second.Format(time.RFC3339Nano))
	require.Equal(t, "input", second.Location().String())
	require.NotSame(t, first.Location(), second.Location())
}

func TestOwnedTimePreservesNamedZoneCalendarRules(t *testing.T) {
	zone, err := time.LoadLocation("America/New_York")
	require.NoError(t, err)
	value := libtime.Time(time.Date(2026, 3, 7, 12, 0, 0, 0, zone))
	*zone = *time.FixedZone("broken", 0)
	stamp, ok := libtime.Get(value)
	require.True(t, ok)
	require.Equal(t, "America/New_York", stamp.Location().String())
	require.Equal(t, "2026-03-07T12:00:00-05:00", stamp.Format(time.RFC3339))
	require.Equal(t, "2026-03-08T12:00:00-04:00", stamp.AddDate(0, 0, 1).Format(time.RFC3339))
	require.Equal(t, 23*time.Hour, stamp.AddDate(0, 0, 1).Sub(stamp))
	require.Equal(t, "2026-03-08T13:00:00-04:00", stamp.Add(24*time.Hour).Format(time.RFC3339))
	// Exercise the tzdata extension beyond the usual explicit transition table.
	require.Equal(t, "2100-07-01T12:00:00-04:00", time.Date(2100, 7, 1, 12, 0, 0, 0, stamp.Location()).Format(time.RFC3339))
	require.Equal(t, "2100-01-01T12:00:00-05:00", time.Date(2100, 1, 1, 12, 0, 0, 0, stamp.Location()).Format(time.RFC3339))
}

func TestOwnedTimePreservesSerializationAndZero(t *testing.T) {
	for _, stamp := range []time.Time{
		{},
		time.Date(2026, 1, 2, 3, 4, 5, 123, time.FixedZone("half-hour", 19800)),
		time.Date(10000, 1, 1, 0, 0, 0, 0, time.UTC), // JSON rejects out-of-range years.
	} {
		value := libtime.Time(stamp)
		want, wantErr := json.Marshal(stamp)
		got, gotErr := json.Marshal(value.Native)
		if wantErr != nil {
			require.Error(t, gotErr)
			require.Contains(t, gotErr.Error(), "year outside of range")
		} else {
			require.NoError(t, gotErr)
			require.Equal(t, string(want), string(got))
		}
		exported, ok := libtime.Get(value)
		require.True(t, ok)
		require.True(t, stamp.Equal(exported))
		require.Equal(t, stamp.IsZero(), exported.IsZero())
	}
}

func TestTimeGetChecksNativeHeaderAndDetachesRawInput(t *testing.T) {
	stamp := time.Date(2026, 1, 2, 3, 4, 5, 0, time.FixedZone("source", 3600))
	for _, value := range []*lisp.LVal{nil, lisp.Int(1), lisp.Native(nil), lisp.Native("wrong"), {Type: lisp.LError, Native: stamp}} {
		got, ok := libtime.Get(value)
		require.False(t, ok)
		require.True(t, got.IsZero())
	}
	got, ok := libtime.Get(lisp.Native(stamp))
	require.True(t, ok, "raw Go times remain usable outside template publication")
	*got.Location() = *time.FixedZone("changed", 0)
	require.Equal(t, "2026-01-02T03:04:05+01:00", stamp.Format(time.RFC3339))
}

func TestOwnedTimeUsesWallClockSemantics(t *testing.T) {
	stamp := time.Now()
	got, ok := libtime.Get(libtime.Time(stamp))
	require.True(t, ok)
	require.True(t, stamp.Equal(got))
	require.NotContains(t, got.String(), "m=", "native times intentionally discard Go monotonic metadata")
	require.Equal(t, reflect.Struct, reflect.TypeOf(libtime.Time(stamp).Native).Kind())
}
