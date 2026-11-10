// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"io"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// lenLiar is a host reader whose Len method does not count its bytes.
type lenLiar struct{ io.Reader }

func (lenLiar) Len() int { return 3 }

// TestReadSourceBytesMatchesReadAll pins that readSourceBytes returns the
// bytes io.ReadAll returns, for the in-memory readers it sizes exactly and
// for every other reader.  The load cache keys on these bytes, so a wrong
// read would load the wrong program.
func TestReadSourceBytesMatchesReadAll(t *testing.T) {
	env := NewEnv(nil)
	for _, src := range []string{"", "a", "(+ 1 2)", strings.Repeat("(defun f (x) (+ x 1))\n", 300)} {
		for _, skip := range []int{0, 1, len(src)} {
			if skip > len(src) {
				continue
			}
			makers := map[string]func() io.Reader{
				"strings.Reader": func() io.Reader { return strings.NewReader(src) },
				"bytes.Reader":   func() io.Reader { return bytes.NewReader([]byte(src)) },
				"bytes.Buffer":   func() io.Reader { return bytes.NewBufferString(src) },
				"host reader":    func() io.Reader { return io.MultiReader(strings.NewReader(src)) },
				"host Len":       func() io.Reader { return lenLiar{strings.NewReader(src)} },
			}
			for name, mk := range makers {
				// The reader has already given up its first skip bytes.
				want, got := mk(), mk()
				_, err := io.CopyN(io.Discard, want, int64(skip))
				require.NoError(t, err)
				_, err = io.CopyN(io.Discard, got, int64(skip))
				require.NoError(t, err)

				wantBytes, wantErr := io.ReadAll(want)
				gotBytes, gotErr := env.readSourceBytes(got)
				assert.Equal(t, wantErr, gotErr, "%s len=%d skip=%d", name, len(src), skip)
				assert.Equal(t, string(wantBytes), string(gotBytes), "%s len=%d skip=%d", name, len(src), skip)
				assert.NotNil(t, gotBytes, "%s len=%d skip=%d", name, len(src), skip)

				// The reader is at its end, as after io.ReadAll.
				n, err := got.Read(make([]byte, 1))
				assert.Zero(t, n, name)
				assert.Equal(t, io.EOF, err, name)
			}
		}
	}
}

// TestReadSourceBytesSizesInMemorySourceExactly pins the gain: a source read
// from memory takes one buffer of its own size.
func TestReadSourceBytesSizesInMemorySourceExactly(t *testing.T) {
	env := NewEnv(nil)
	src := strings.Repeat("x", 5000)
	got, err := env.readSourceBytes(strings.NewReader(src))
	require.NoError(t, err)
	assert.Equal(t, len(src), cap(got))
}
