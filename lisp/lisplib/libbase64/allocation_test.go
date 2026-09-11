// Copyright © 2026 The ELPS authors

package libbase64_test

import (
	"encoding/base64"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libbase64"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func newBase64AllocationEnv(t *testing.T, limit int) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()))))
	require.NoError(t, lisp.GoError(libbase64.LoadPackage(env)))
	env.Runtime.MaxAlloc = limit
	return env
}

func assertBase64Allocation(t *testing.T, got *lisp.LVal, want []byte, limit int) {
	t.Helper()
	require.False(t, lisp.IsInternalPanic(got), "%v", got)
	if len(want) > limit {
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		assert.Contains(t, got.String(), "allocation")
		return
	}
	require.Equal(t, lisp.LBytes, got.Type, "%v", got)
	assert.Equal(t, want, got.Bytes())
}

func TestBase64AllocationEncode(t *testing.T) {
	for _, bytesInput := range []bool{false, true} {
		for _, limit := range []int{7, 8, 9} {
			t.Run(fmt.Sprintf("bytes=%t limit=%d", bytesInput, limit), func(t *testing.T) {
				env := newBase64AllocationEnv(t, limit)
				source := lisp.String("éé")
				if bytesInput {
					source = lisp.Bytes([]byte("éé"))
				}
				env.PutGlobal(lisp.Symbol("source"), source)
				before := source.String()
				got := env.LoadString("base64-allocation.lisp", `(base64:encode source)`)
				assert.Equal(t, before, source.String())
				assertBase64Allocation(t, got, []byte("w6nDqQ=="), limit)
			})
		}
	}
}

func TestBase64AllocationDecode(t *testing.T) {
	const limit = 8
	for _, bytesInput := range []bool{false, true} {
		for _, newlines := range []bool{false, true} {
			for _, size := range []int{limit - 1, limit, limit + 1} {
				t.Run(fmt.Sprintf("bytes=%t newlines=%t size=%d", bytesInput, newlines, size), func(t *testing.T) {
					want := []byte(strings.Repeat("a", size))
					encoded := base64.StdEncoding.EncodeToString(want)
					if newlines {
						encoded = "\r\n" + strings.Join(strings.Split(encoded, ""), "\r\n") + "\n"
					}
					env := newBase64AllocationEnv(t, limit)
					source := lisp.String(encoded)
					if bytesInput {
						source = lisp.Bytes([]byte(encoded))
					}
					env.PutGlobal(lisp.Symbol("source"), source)
					before := source.String()
					got := env.LoadString("base64-allocation.lisp", `(base64:decode source)`)
					assert.Equal(t, before, source.String())
					assertBase64Allocation(t, got, want, limit)
				})
			}
		}
	}
}

func TestBase64AllocationEmptyAndMalformed(t *testing.T) {
	for _, bytesInput := range []bool{false, true} {
		for _, input := range []string{"", "\r\n", "=", "A", "AA", "AAA", "====", "A===", "AA=A", "AAAAAA==A", "!!!!", "AA==\nA"} {
			t.Run(fmt.Sprintf("bytes=%t %q", bytesInput, input), func(t *testing.T) {
				env := newBase64AllocationEnv(t, 8)
				source := lisp.String(input)
				if bytesInput {
					source = lisp.Bytes([]byte(input))
				}
				env.PutGlobal(lisp.Symbol("source"), source)
				got := env.LoadString("base64-allocation.lisp", `(base64:decode source)`)
				require.False(t, lisp.IsInternalPanic(got), "%v", got)
				if input != "" && input != "\r\n" {
					require.Equal(t, lisp.LError, got.Type, "%v", got)
					assert.Contains(t, got.String(), "base64")
					return
				}
				require.Equal(t, lisp.LBytes, got.Type, "%v", got)
				assert.Empty(t, got.Bytes())
			})
		}
	}
	got := newBase64AllocationEnv(t, 1).LoadString("base64-allocation.lisp", `(base64:encode "")`)
	require.Equal(t, lisp.LBytes, got.Type, "%v", got)
	assert.Empty(t, got.Bytes())
}
