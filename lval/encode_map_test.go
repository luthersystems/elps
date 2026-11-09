// Copyright © 2026 The ELPS authors

package lval_test

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lval"
)

// Encode walks a Go map in key order, so its result and its error do not
// depend on Go's random map order (luthersystems/elps#934).  Two Go keys
// that become one ELPS key are an error, as they are in DecodeInto: before,
// the value that stayed was the last one the random walk met.
func TestEncodeMapIsDeterministic(t *testing.T) {
	// Go picks a new map order on each range, so 200 runs meet both orders.
	const runs = 200
	for range runs {
		_, err := lval.Encode(map[any]int{"true": 1, true: 2})
		require.ErrorContains(t, err, "two keys convert to one map key")

		_, err = lval.Encode(map[any]string{int8(1): "from-int8", int16(1): "from-int16"})
		require.ErrorContains(t, err, "two keys convert to one map key")

		// Every value fails to encode.  The error names the first key in
		// key order.
		_, err = lval.Encode(map[string]any{"a": make(chan int), "b": func() {}, "c": complex(1, 2)})
		require.ErrorContains(t, err, `lval: "a":`)
	}

	v, err := lval.Encode(map[any]int{"b": 2, 1: 10, "a": 1, int8(0): 0})
	require.NoError(t, err)
	b, err := lval.Marshal(v)
	require.NoError(t, err)
	require.Equal(t, `{"a":1,"b":2,"~i0":0,"~i1":10}`, string(b))
}
