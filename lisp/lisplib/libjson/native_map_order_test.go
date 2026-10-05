// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"encoding/json"
	"errors"
	"fmt"
	"reflect"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/require"
)

type failingText struct{}

func (failingText) MarshalText() ([]byte, error) { return nil, errors.New("boom") }

// orderedTextKey reverses numeric order when encoding/json names a key.
type orderedTextKey int

func (k orderedTextKey) MarshalText() ([]byte, error) {
	if k == 2 {
		return []byte("a"), nil
	}
	return []byte("b"), nil
}

// TestDumpNativeMapErrorOrder pins luthersystems/elps#807 with fresh runtimes.
func TestDumpNativeMapErrorOrder(t *testing.T) {
	large := make([]byte, 2048)
	for _, tc := range []struct {
		name   string
		native any
	}{
		{"any-map", map[string]any{"a": failingText{}, "b": large}},
		{"reflected-string-map", map[string]anyAlias{"a": failingText{}, "b": large}},
		{"int-map", map[int]any{10: failingText{}, 2: large}},
		{"text-map", map[orderedTextKey]any{2: failingText{}, 1: large}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			_, want := json.Marshal(tc.native)
			require.ErrorContains(t, want, "boom")
			ctx := lisp.SortedMap()
			require.NoError(t, lisp.GoError(ctx.MapSetString("z", lisp.Native(tc.native)))) //elpsvet:allow-native test owns the native map
			results := make(map[string]bool)
			for i := range 256 {
				env := lisp.NewEnv(nil)
				env.Runtime.MaxAlloc = 1024
				got := libjson.DefaultSerializer().DumpBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{ctx, lisp.Bool(false)}))
				require.Equal(t, lisp.LError, got.Type, "iteration %d", i)
				results[got.String()] = true
			}
			require.Len(t, results, 1)
			for result := range results {
				require.Contains(t, result, want.Error())
			}
		})
	}
}

type anyAlias any

type failingTextKey int

func (k failingTextKey) MarshalText() ([]byte, error) {
	return nil, fmt.Errorf("key %d", k)
}

type stringTextKey string

func (stringTextKey) MarshalText() ([]byte, error) {
	return nil, errors.New("string keys bypass MarshalText")
}

func TestDumpNativeTextKeyErrorOrder(t *testing.T) {
	native := map[failingTextKey]any{2: make([]byte, 2048), 1: nil}
	keyError := fmt.Sprintf("json: encoding error for type %q: %q", reflect.TypeOf(native).String(), "key 1")
	for _, tc := range []struct {
		name   string
		native any
		want   string
	}{
		{"keys-before-values", native, keyError},
		{"earlier-value-error", []any{failingText{}, native}, "boom"},
		{"earlier-allocation-error", []any{make([]byte, 2048), native}, "allocation size exceeds maximum (1024)"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			for i := range 256 {
				env := lisp.NewEnv(nil)
				env.Runtime.MaxAlloc = 1024
				v := lisp.Native(tc.native) //elpsvet:allow-native test owns the native input
				got := libjson.DefaultSerializer().DumpBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{v, lisp.Bool(false)}))
				require.Equal(t, lisp.LError, got.Type, "iteration %d", i)
				require.Contains(t, got.String(), tc.want, "iteration %d", i)
			}
		})
	}
}

func TestDumpNativeMapBytesAndCharges(t *testing.T) {
	text := strings.Repeat("<>&", 300)
	for _, native := range []any{
		map[string]any{"a": text, "b": []byte{1, 2, 3}},
		map[string]anyAlias{"a": text, "b": []byte{1, 2, 3}},
		map[int]any{10: text, 2: []byte{1, 2, 3}},
		map[orderedTextKey]any{2: text, 1: []byte{1, 2, 3}},
		map[stringTextKey]any{"a": text, "b": []byte{1, 2, 3}},
	} {
		want, err := json.Marshal(native)
		require.NoError(t, err)
		env := lisp.NewEnv(nil)
		env.Runtime.SetStepBudget(100)
		v := lisp.Native(native) //elpsvet:allow-native test owns the native map
		got := libjson.DefaultSerializer().DumpBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{v, lisp.Bool(false)}))
		require.Equal(t, lisp.LBytes, got.Type, "%v", got)
		require.Equal(t, want, got.Bytes())
		require.Equal(t, int64(len(want)/1024), env.Runtime.TotalSteps())
	}
}

func TestDumpNativeMapBudgetErrorOrder(t *testing.T) {
	for _, native := range []any{
		map[string]any{"a": strings.Repeat("x", 3000), "b": make([]byte, 8192)},
		map[int]any{10: strings.Repeat("x", 3000), 2: make([]byte, 8192)},
		map[orderedTextKey]any{2: strings.Repeat("x", 3000), 1: make([]byte, 8192)},
	} {
		for i := range 256 {
			env := lisp.NewEnv(nil)
			env.Runtime.MaxAlloc = 4096
			env.Runtime.SetStepBudget(1)
			v := lisp.Native(native) //elpsvet:allow-native test owns the native map
			got := libjson.DefaultSerializer().DumpBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{v, lisp.Bool(false)}))
			require.Equal(t, lisp.LError, got.Type, "iteration %d", i)
			require.Equal(t, lisp.CondStepBudgetExceeded, got.Str, "iteration %d: %v", i, got)
		}
	}
}

func TestLoadIndirectMapErrorOrder(t *testing.T) {
	for i := range 256 {
		got := libjson.LoadIndirectForTest([]byte(`{"b":[0,0,0,0,0],"a":[0,0,0,0]}`), libjson.LoadOpts{MaxAlloc: 3})
		require.Equal(t, lisp.LError, got.Type, "iteration %d", i)
		require.Contains(t, got.String(), "allocation size 4 exceeds maximum (3)")
	}
}

func TestCanonizeNativeMapKeyErrorOrder(t *testing.T) {
	for _, tc := range []struct {
		native any
		want   string
	}{
		{map[int]string{2: "two", 10: "ten"}, "int map key 10 at $[key 10]"},
		{map[uint]string{2: "two", 10: "ten"}, "int map key 10 at $[key 10]"},
		{map[bool]string{false: "no", true: "yes"}, "unsupported native map key bool at $"},
	} {
		for i := range 256 {
			_, err := libjson.Canonize(lisp.Native(tc.native)) //elpsvet:allow-native test owns the native map
			require.EqualError(t, err, "json:canonize: "+tc.want, "iteration %d", i)
		}
	}
}
