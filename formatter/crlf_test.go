// Copyright © 2024 The ELPS authors

package formatter

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestCRLFSourceFormatsToLF pins luthersystems/elps#943. A CRLF file used to
// keep CRLF on comment lines and get LF on the other lines, because the
// comment token keeps the carriage return and the code lines do not. The
// formatter writes LF line endings everywhere. The bytes of a raw string are
// program data and stay as written.
func TestCRLFSourceFormatsToLF(t *testing.T) {
	lf := strings.Join([]string{
		"#!/usr/bin/env elps",
		"; leading comment",
		"(defun f (x) ; trailing comment",
		"  ; inner comment",
		"  (+ x 1)",
		"  ; inner trailing comment",
		"  )",
		"",
		"; detached comment",
		"",
		"(set 's \"a\\r\\nb\")",
		"; last comment",
		"",
	}, "\n")
	crlf := strings.ReplaceAll(lf, "\n", "\r\n")

	for _, cfg := range []*Config{nil, {IndentSize: 2, MaxBlankLines: 1, Compact: true}} {
		want, err := Format([]byte(lf), cfg)
		require.NoError(t, err)
		require.NotContains(t, string(want), "\r")

		got, err := Format([]byte(crlf), cfg)
		require.NoError(t, err)
		assert.Equal(t, string(want), string(got), "a CRLF file formats to the bytes of its LF twin")

		// Stray carriage returns before the line feed go too.
		got, err = Format([]byte(strings.ReplaceAll(lf, "\n", "\r\r\n")), cfg)
		require.NoError(t, err)
		assert.Equal(t, string(want), string(got))
	}
}

// TestCRLFInsideLiteralsIsKept checks that the LF rule covers line endings
// only. A raw string that holds a real CR LF pair is a different value from
// one that holds LF, and so is a carriage return inside a comment line.
func TestCRLFInsideLiteralsIsKept(t *testing.T) {
	src := "(set 's \"\"\"a\r\nb\"\"\") ; c\r\n(set 't \"x\\r\\n\")\r\n; a\rb\r\n"
	want := "(set 's \"\"\"a\r\nb\"\"\") ; c\n(set 't \"x\\r\\n\")\n; a\rb\n"
	got, err := Format([]byte(src), nil)
	require.NoError(t, err)
	assert.Equal(t, want, string(got))
	roundTripEqual(t, src, string(got))

	again, err := Format(got, nil)
	require.NoError(t, err)
	assert.Equal(t, string(got), string(again), "not idempotent")
}
