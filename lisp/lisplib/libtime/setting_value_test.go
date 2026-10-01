// Copyright © 2026 The ELPS authors

package libtime_test

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libtime"
	"github.com/stretchr/testify/require"
)

// An embedder cannot mark its own structs, but it can store a marked payload
// that an ELPS API returns. docs/embed.md ("Per-VM settings") states this.
func TestTimePayloadIsAValueSetting(t *testing.T) {
	want := time.Date(2026, 10, 1, 12, 30, 0, 0, time.UTC)
	payload := libtime.Time(want).Native
	source := lisp.NewEnv(nil)
	require.NoError(t, source.Runtime.SetSettingValue("loaded-at", payload))

	tmpl, err := lisp.NewTemplate(source)
	require.NoError(t, err)
	vm, err := tmpl.NewVM()
	require.NoError(t, err)
	got, ok := vm.Runtime.SettingValue("loaded-at")
	require.True(t, ok)
	require.IsType(t, payload, got)
	readBack, ok := libtime.Get(&lisp.LVal{Type: lisp.LNative, Native: got})
	require.True(t, ok)
	require.True(t, readBack.Equal(want), "got %v, want %v", readBack, want)

	// A pointer to the same payload is not shareable and is rejected.
	require.Error(t, source.Runtime.SetSettingValue("loaded-at-pointer", &payload))
}
