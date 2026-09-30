// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"errors"
	"runtime"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/require"
)

// allocated returns the bytes f allocates.
func allocated(f func()) uint64 {
	runtime.GC()
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	f()
	runtime.ReadMemStats(&after)
	return after.TotalAlloc - before.TotalAlloc
}

func TestTypedUnicodeKeyAllocationIsLinear(t *testing.T) {
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.String(strings.Repeat("é", 4096)), lisp.Int(1))
	n := allocated(func() {
		_, err := libjson.DumpTyped(m)
		require.NoError(t, err)
	})
	require.Less(t, n, uint64(256<<10), "a 8 KiB key allocated %d bytes", n)
}

func TestTypedLimitsBeforeAllocation(t *testing.T) {
	cells := make([]*lisp.LVal, 1<<20)
	for i := range cells {
		cells[i] = lisp.Int(0)
	}
	wide := lisp.Vector(cells)
	long := lisp.Symbol(strings.Repeat("x", 1<<20))
	for name, f := range map[string]func() error{
		"tag wide vector": func() error {
			_, err := libjson.Tag(wide, libjson.WithTypedMaxBytes(100), libjson.WithTypedMaxValues(100))
			return err
		},
		"dump long symbol": func() error {
			_, err := libjson.DumpTyped(long, libjson.WithTypedMaxBytes(100))
			return err
		},
		"tag long symbol": func() error {
			_, err := libjson.Tag(long, libjson.WithTypedMaxBytes(100))
			return err
		},
	} {
		var err error
		n := allocated(func() { err = f() })
		require.ErrorIs(t, err, libjson.ErrTypedLimit, name)
		require.Less(t, n, uint64(256<<10), "%s allocated %d bytes", name, n)
	}
}

func TestTypedBuiltinKeepsLimitError(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	require.NoError(t, lisp.GoError(libjson.LoadPackage(env)))
	require.NoError(t, lisp.GoError(lisp.WithMaxAlloc(100)(env)))
	v := lisp.String(strings.Repeat("x", 101))
	for name, b := range map[string]lisp.LBuiltin{"dump": libjson.DumpTypedBuiltin, "tag": libjson.TagBuiltin} {
		out := b(env, lisp.SExpr([]*lisp.LVal{v}))
		require.Equal(t, lisp.LError, out.Type, name)
		require.True(t, errors.Is(lisp.GoError(out), libjson.ErrTypedLimit), "%s: %v", name, out)
	}
}

func TestTypedHostMapKeysNamingOneEntry(t *testing.T) {
	for _, keys := range [][]*lisp.LVal{
		{lisp.String("a"), lisp.Symbol("a")},
		{lisp.String(":a"), lisp.Symbol(":a")},
	} {
		m := canonHostMap(keys...)
		_, err := libjson.DumpTyped(m)
		require.Error(t, err)
		_, err = libjson.Tag(m)
		require.Error(t, err)
	}
}
