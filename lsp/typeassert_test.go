// Copyright © 2026 The ELPS authors

package lsp

import (
	"testing"

	"github.com/stretchr/testify/require"
)

// requireType returns v as a T. It fails the test when v holds another
// type.
func requireType[T any](t testing.TB, v any) T {
	t.Helper()
	got, ok := v.(T)
	require.Truef(t, ok, "got %T, want %T", v, got)
	return got
}
