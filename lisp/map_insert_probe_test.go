// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestMapInsertRejectsBadKeysIdenticallyUnderTheAllocationProbe pins that the
// allocation probe in checkMapInsertAlloc does not change how a bad key is
// reported.
//
// The probe runs only once a map has reached the allocation cap: it asks
// MapData.Get whether the key is already present, so that a replacement is not
// charged as growth.  Get rejects an unhashable key, and the probe used to
// return that rejection as-is, while the ordinary insert path reports the same
// rejection through the calling builtin.  The same expression therefore
// rendered two different messages depending only on how close the map was to
// MaxAlloc -- a diagnostic that changes under memory pressure, which is when it
// is least welcome.  The probe is about allocation; the key belongs to the
// insert.
func TestMapInsertRejectsBadKeysIdenticallyUnderTheAllocationProbe(t *testing.T) {
	// The map is deliberately not tiny: MaxAlloc also caps how much of a
	// value gets rendered, and a cap of a handful of bytes would truncate
	// the message under test rather than change it.
	const size = 64
	for _, tc := range []struct {
		name string
		expr string
	}{
		{"assoc", `(assoc m 5 1)`},
		{"assoc!", `(assoc! m 5 1)`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// The first limit is small enough that the probe runs; the
			// second is large enough that it does not.
			var messages []string
			for _, maxAlloc := range []int{size, 1 << 20} {
				env, err := (&elpstest.Runner{}).NewEnv(t)
				require.NoError(t, err)
				m := lisp.SortedMap()
				for i := range size {
					require.NoError(t, lisp.GoError(m.Map().Set(
						lisp.String(fmt.Sprintf("key%d", i)), lisp.Int(i))))
				}
				env.PutGlobal(lisp.Symbol("m"), m)
				env.Runtime.MaxAlloc = maxAlloc

				got := env.LoadString("map-insert-probe.lisp", tc.expr)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				require.Equal(t, lisp.LError, got.Type, "an int is not a hashable key: %v", got)
				assert.Contains(t, diagnosticText(got), "unhashable type: int")
				messages = append(messages, diagnosticText(got))
			}
			assert.Equal(t, messages[1], messages[0],
				"the same bad key must be reported the same way whether or not the allocation probe ran")
		})
	}
}
