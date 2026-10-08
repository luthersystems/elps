// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

// The codec shapes of an embedder registry: a pointer to a struct (a
// handle), a struct value (a date), a struct value of a type the
// registering package cannot name (a ForeignCodec), and a codec with
// options.
type (
	codecHandle struct {
		Open map[string]int
		Name string
	}
	codecDate struct{ Year, Month, Day int }
	codecOpt  struct{ N int }
)

func codecSave(_ *lisp.LEnv, _ *lisp.LVal) (*lisp.LVal, error)        { return lisp.Nil(), nil }
func codecLoad(_ *lisp.LEnv, _ int, _ *lisp.LVal) (*lisp.LVal, error) { return lisp.Nil(), nil }

var (
	handleCodec  = libjson.DurableCodec[*codecHandle]{Name: "test:handle", Version: 1, Save: codecSave, Load: codecLoad}
	dateCodec    = libjson.DurableCodec[codecDate]{Name: "test:date", Version: 2, Save: codecSave, Load: codecLoad}
	foreignCodec = libjson.ForeignCodec{Type: reflect.TypeFor[codecForeign](), Name: "test:foreign", Version: 1, Save: codecSave, Load: codecLoad}
	optCodec     = libjson.DurableCodec[*codecOpt]{Name: "test:opt", Version: 3, Charge: 7, SharedPayload: true, Save: codecSave, Load: codecLoad}
)

type codecForeign struct{ T int64 }

// TestFrozenDurableRegistryMatchesRegister pins that the codec values
// register exactly what RegisterNative and Register do: an embedder that
// moves its registrations to NewFrozenDurableRegistry keeps its
// Fingerprint, which its saved documents depend on.
func TestFrozenDurableRegistryMatchesRegister(t *testing.T) {
	t.Parallel()
	funcs := libjson.NativeFuncs{Save: codecSave, Load: codecLoad}
	old := libjson.NewDurableRegistry()
	require.NoError(t, libjson.RegisterNative[*codecHandle](old, "test:handle", 1, funcs))
	require.NoError(t, libjson.RegisterNative[codecDate](old, "test:date", 2, funcs))
	require.NoError(t, old.Register(reflect.TypeFor[codecForeign](), "test:foreign", 1, funcs))
	require.NoError(t, libjson.RegisterNative[*codecOpt](old, "test:opt", 3, funcs,
		libjson.WithNativeCharge(7), libjson.WithSharedPayload()))
	old.Freeze()

	a, err := libjson.NewFrozenDurableRegistry(handleCodec, dateCodec, foreignCodec, optCodec)
	require.NoError(t, err)
	b, err := libjson.NewFrozenDurableRegistry(optCodec, foreignCodec, dateCodec, handleCodec)
	require.NoError(t, err)
	require.True(t, a.Frozen())
	require.Equal(t, old.Fingerprint(), a.Fingerprint())
	require.Equal(t, a.Fingerprint(), b.Fingerprint())
	require.Contains(t, a.Fingerprint(), `"name":"test:opt","type":"*github.com/luthersystems/elps/lisp/lisplib/libjson_test.codecOpt"`)
	require.Contains(t, a.Fingerprint(), `"version":3,"charge":7,"shared":true`)
}

// TestFrozenDurableRegistryRefuses pins the errors: a codec with no Save or
// no Load, a nil entry, and every refusal of Register.
func TestFrozenDurableRegistryRefuses(t *testing.T) {
	t.Parallel()
	_, err := libjson.NewFrozenDurableRegistry(handleCodec, handleCodec)
	require.ErrorContains(t, err, "already registered")
	_, err = libjson.NewFrozenDurableRegistry(libjson.DurableCodec[codecDate]{Name: "test:date", Save: codecSave, Load: codecLoad})
	require.ErrorContains(t, err, "below 1")
	_, err = libjson.NewFrozenDurableRegistry(libjson.DurableCodec[codecDate]{Name: "test:date", Version: 1, Load: codecLoad})
	require.ErrorContains(t, err, `codec "test:date" needs both a Save and a Load function`)
	_, err = libjson.NewFrozenDurableRegistry(libjson.ForeignCodec{Type: reflect.TypeFor[codecForeign](), Name: "test:foreign", Version: 1, Save: codecSave})
	require.ErrorContains(t, err, "needs both a Save and a Load function")
	_, err = libjson.NewFrozenDurableRegistry(libjson.ForeignCodec{Name: "test:foreign", Version: 1, Save: codecSave, Load: codecLoad})
	require.ErrorContains(t, err, "nil type")
	_, err = libjson.NewFrozenDurableRegistry(nil)
	require.ErrorContains(t, err, "nil codec entry")
}

// TestDurableCodecWithName pins that WithName changes only the name.
func TestDurableCodecWithName(t *testing.T) {
	t.Parallel()
	renamed := optCodec.WithName("embedder:opt")
	require.Equal(t, "embedder:opt", renamed.Name)
	require.Equal(t, "test:opt", optCodec.Name)
	a, err := libjson.NewFrozenDurableRegistry(renamed)
	require.NoError(t, err)
	require.Contains(t, a.Fingerprint(), `"name":"embedder:opt"`)
	require.Contains(t, a.Fingerprint(), `"version":3,"charge":7,"shared":true`)
}
