// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"encoding"
	"encoding/json"
	"fmt"
	"math/big"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

type nilJSONHolder struct{ F json.Marshaler }

type nilTextHolder struct{ F encoding.TextMarshaler }

// marshalResult is what f returns or the text of what it panics with.
func marshalResult(f func() ([]byte, error)) (out string, panicked string) { //nolint:nonamedreturns // the deferred recover sets panicked
	defer func() {
		if p := recover(); p != nil {
			panicked = fmt.Sprint(p)
		}
	}()
	b, err := f()
	if err != nil {
		return "error: " + err.Error(), ""
	}
	return string(b), ""
}

// An interface whose type has a marshal method can hold a nil pointer of a
// type whose size the native walk reads from the value: time.Time,
// json.RawMessage and math/big's numbers.  The v1 engine calls the method on
// the nil pointer, so the dump does the same: it writes what the method
// writes, or panics as the method panics.  The walk used to read the value
// through the nil pointer itself and panic in libjson, on a native that
// encoding/json writes (luthersystems/elps#950).
func TestNativeNilPointerBehindMarshaler(t *testing.T) {
	for _, tc := range []struct {
		name   string
		native any
		want   string // the dump, when wantPanic is empty
		// wantPanic is in the panic of the method.  The v2 engine's
		// json.RawMessage is jsontext.Value, which its text names.
		wantPanic string
		// v2Panics is set when encoding/json on the v2 engine panics in
		// its own code, so it is no reference there.
		v2Panics bool
	}{
		{name: "big.Int in a Marshaler", native: nilJSONHolder{(*big.Int)(nil)}, want: `{"F":null}`},
		{name: "big.Int in a TextMarshaler", native: nilTextHolder{(*big.Int)(nil)}, want: `{"F":"\u003cnil\u003e"}`, v2Panics: true},
		{name: "big.Float in a TextMarshaler", native: nilTextHolder{(*big.Float)(nil)}, want: `{"F":"\u003cnil\u003e"}`, v2Panics: true},
		{name: "big.Rat in a TextMarshaler", native: nilTextHolder{(*big.Rat)(nil)}, wantPanic: "runtime error: invalid memory address or nil pointer dereference", v2Panics: true},
		{name: "time.Time in a Marshaler", native: nilJSONHolder{(*time.Time)(nil)}, wantPanic: "value method time.Time.MarshalJSON called using nil *Time pointer"},
		{name: "time.Time in a TextMarshaler", native: nilTextHolder{(*time.Time)(nil)}, wantPanic: "value method time.Time.MarshalText called using nil *Time pointer", v2Panics: true},
		{name: "RawMessage in a Marshaler", native: nilJSONHolder{(*json.RawMessage)(nil)}, wantPanic: "MarshalJSON called using nil *"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got, gotPanic := marshalResult(func() ([]byte, error) {
				return libjson.Dump(lisp.Native(tc.native), false) //elpsvet:allow-native test owns each native
			})
			if tc.wantPanic != "" {
				require.Contains(t, gotPanic, tc.wantPanic)
			} else {
				require.Empty(t, gotPanic)
				require.Equal(t, tc.want, got)
			}
			if tc.v2Panics && libjson.JSONV2EngineForTest() {
				return
			}
			ref, refPanic := marshalResult(func() ([]byte, error) { return json.Marshal(tc.native) })
			require.Equal(t, ref, got, "encoding/json")
			require.Equal(t, refPanic, gotPanic, "encoding/json")
		})
	}
}

// A marshal method that panics in json:dump-string raises an internal-panic
// error in the program.  It does not stop the process.
func TestNativeNilPointerBehindMarshalerInLisp(t *testing.T) {
	env := allocLimitEnv(t)
	for _, native := range []any{
		nilJSONHolder{(*time.Time)(nil)},
		map[encoding.TextMarshaler]int{nil: 1},
	} {
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("n"), lisp.Native(native)))) //elpsvet:allow-native test owns each native
		res := env.LoadString("test", `(json:dump-string n)`)
		require.Equal(t, lisp.LError, res.Type, "%T", native)
		require.True(t, lisp.IsInternalPanic(res), "%v", res)
	}
}
