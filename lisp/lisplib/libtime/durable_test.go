// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"reflect"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/stretchr/testify/require"
)

// TestDurableTimeCodecMatchesForeign pins that an embedder that registered
// the time native as a ForeignCodec under its own name keeps its
// Fingerprint when it moves to DurableTimeCodec.WithName.
func TestDurableTimeCodecMatchesForeign(t *testing.T) {
	t.Parallel()
	foreign := libjson.ForeignCodec{
		Type:    reflect.TypeOf(libtime.Time(time.Time{}).Native),
		Name:    "embedder:time",
		Version: 1,
		Save:    libtime.DurableTimeCodec.Save,
		Load:    libtime.DurableTimeCodec.Load,
	}
	a, err := libjson.NewFrozenDurableRegistry(foreign)
	require.NoError(t, err)
	b, err := libjson.NewFrozenDurableRegistry(libtime.DurableTimeCodec.WithName("embedder:time"))
	require.NoError(t, err)
	require.Equal(t, a.Fingerprint(), b.Fingerprint())
	require.Contains(t, b.Fingerprint(), `"type":"github.com/luthersystems/elps/lisp/lisplib/libtime.ownedTime"`)
}

// TestDurableTimeCodecs dumps and loads a time and a duration.  A time
// loads as the same instant in UTC.
func TestDurableTimeCodecs(t *testing.T) {
	t.Parallel()
	env := timeTestEnv(t)
	reg, err := libjson.NewFrozenDurableRegistry(libtime.DurableTimeCodec, libtime.DurableDurationCodec)
	require.NoError(t, err)
	at := time.Date(2026, 10, 8, 12, 30, 0, 123456789, time.FixedZone("X", 3600))
	v := lisp.QExpr([]*lisp.LVal{libtime.Time(at), libtime.Duration(90 * time.Second)})
	b, err := libjson.DumpDurable(env, v, reg)
	require.NoError(t, err)
	require.Contains(t, string(b), `"2026-10-08T11:30:00.123456789Z"`)
	got, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err)
	require.Len(t, got.Cells, 2)
	gotTime, ok := libtime.Get(got.Cells[0])
	require.True(t, ok)
	require.True(t, gotTime.Equal(at))
	require.Equal(t, time.UTC, gotTime.Location())
	gotDur, ok := libtime.GetDuration(got.Cells[1])
	require.True(t, ok)
	require.Equal(t, 90*time.Second, gotDur)
}
