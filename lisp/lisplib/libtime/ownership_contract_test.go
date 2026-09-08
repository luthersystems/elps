// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"encoding"
	"encoding/json"
	"os"
	"os/exec"
	"reflect"
	"runtime"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/stretchr/testify/require"
)

func TestOwnedTimePublicShapeAndDetachedEncoding(t *testing.T) {
	value := libtime.Time(time.Date(2026, 1, 2, 3, 4, 5, 0, time.FixedZone("fixed", 3600)))
	typ := reflect.TypeOf(value.Native)
	require.Equal(t, reflect.Struct, typ.Kind())
	require.Equal(t, 2, typ.NumField(), "new owned state requires an ownership audit")
	marker, field := typ.Field(0), typ.Field(1)
	require.True(t, marker.Anonymous)
	require.Equal(t, "github.com/luthersystems/elps/internal/templatepolicy", marker.Type.PkgPath())
	require.Equal(t, "Marker", marker.Type.Name())
	require.Zero(t, marker.Type.Size())
	require.Equal(t, "t", field.Name)
	require.Equal(t, reflect.TypeFor[time.Time](), field.Type)
	require.False(t, field.IsExported())
	require.False(t, field.Anonymous, "do not promote Go time mutators or Location")
	require.Equal(t, 2, typ.NumMethod())
	require.Equal(t, "MarshalJSON", typ.Method(0).Name)
	require.Equal(t, "MarshalText", typ.Method(1).Name)
	text := value.Native.(encoding.TextMarshaler)
	encoded := value.Native.(json.Marshaler)
	for _, marshaler := range []func() ([]byte, error){text.MarshalText, encoded.MarshalJSON} {
		before, err := marshaler()
		require.NoError(t, err)
		want := string(before)
		before[0] = '!'
		after, err := marshaler()
		require.NoError(t, err)
		require.Equal(t, want, string(after))
	}
}

func TestGoTimeLocationOwnershipContract(t *testing.T) {
	// The snapshot relies on Go's initialized rule tables remaining read-only.
	// Pin all reachable layouts and the Location method set so toolchain changes
	// require a fresh audit. Shape checks supplement, not replace, behavioral
	// alias/DST/race tests and inspection of future Go implementations.
	check := func(typ reflect.Type, fields map[string]string) {
		t.Helper()
		require.Equal(t, len(fields), typ.NumField(), "%s layout changed", typ)
		for i := range typ.NumField() {
			field := typ.Field(i)
			require.False(t, field.IsExported())
			require.False(t, field.Anonymous)
			require.Equal(t, fields[field.Name], field.Type.String(), "%s.%s changed", typ, field.Name)
		}
	}
	check(reflect.TypeFor[time.Time](), map[string]string{"wall": "uint64", "ext": "int64", "loc": "*time.Location"})
	loc := reflect.TypeFor[time.Location]()
	check(loc, map[string]string{
		"name": "string", "zone": "[]time.zone", "tx": "[]time.zoneTrans", "extend": "string",
		"cacheStart": "int64", "cacheEnd": "int64", "cacheZone": "*time.zone",
	})
	zones, _ := loc.FieldByName("zone")
	check(zones.Type.Elem(), map[string]string{"name": "string", "offset": "int", "isDST": "bool"})
	transitions, _ := loc.FieldByName("tx")
	check(transitions.Type.Elem(), map[string]string{"when": "int64", "index": "uint8", "isstd": "bool", "isutc": "bool"})
	require.Zero(t, loc.NumMethod())
	ptr := reflect.PointerTo(loc)
	require.Equal(t, 1, ptr.NumMethod(), "new timezone APIs require a mutation/escape audit")
	require.Equal(t, "String", ptr.Method(0).Name)
	require.Equal(t, "func(*time.Location) string", ptr.Method(0).Type.String())
}

func TestOwnedTimeInitializesLocalBeforeCapture(t *testing.T) {
	if os.Getenv("ELPS_OWNED_TIME_CHILD") == "1" {
		// This child starts with a fresh time.Local. Unix attaches that pointer
		// without loading its timezone; copying first would freeze an empty UTC
		// table rather than the requested New York rules.
		require.Zero(t, reflect.ValueOf(time.Local).Elem().FieldByName("zone").Len())
		input := time.Unix(time.Date(2026, 3, 7, 17, 0, 0, 0, time.UTC).Unix(), 0)
		value := libtime.Time(input)
		got, ok := libtime.Get(value)
		require.True(t, ok)
		// Resolve the original only after capture. A premature snapshot of the
		// lazy object must differ, even where TZ is not supported (Windows).
		require.Equal(t, input.Location().String(), got.Location().String())
		require.Equal(t, input.Format(time.RFC3339), got.Format(time.RFC3339))
		require.Equal(t, input.AddDate(0, 0, 1).Format(time.RFC3339), got.AddDate(0, 0, 1).Format(time.RFC3339))
		if runtime.GOOS == "linux" || runtime.GOOS == "darwin" {
			require.Equal(t, "America/New_York", got.Location().String())
			require.Equal(t, "2026-03-07T12:00:00-05:00", got.Format(time.RFC3339))
			require.Equal(t, "2026-03-08T12:00:00-04:00", got.AddDate(0, 0, 1).Format(time.RFC3339))
		}
		return
	}
	executable, err := os.Executable()
	require.NoError(t, err)
	//nolint:gosec // Re-exec this test binary with fixed arguments; no caller-supplied command or script.
	cmd := exec.CommandContext(t.Context(), executable, "-test.run=^TestOwnedTimeInitializesLocalBeforeCapture$", "-test.count=1")
	for _, variable := range os.Environ() {
		if !strings.HasPrefix(variable, "TZ=") && !strings.HasPrefix(variable, "ELPS_OWNED_TIME_CHILD=") {
			cmd.Env = append(cmd.Env, variable)
		}
	}
	cmd.Env = append(cmd.Env, "TZ=America/New_York", "ELPS_OWNED_TIME_CHILD=1")
	output, err := cmd.CombinedOutput()
	require.NoError(t, err, "%s", output)
}
