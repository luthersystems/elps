// Copyright © 2026 The ELPS authors

package diagnosticsource

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/diagnostic"
	"github.com/luthersystems/elps/internal/rootlibrary"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestSourceReaderPhysicalSource(t *testing.T) {
	root := t.TempDir()
	file := filepath.Join(root, "main.lisp")
	require.NoError(t, os.WriteFile(file, []byte("missing-symbol ; physical source"), 0o600))
	lib, err := rootlibrary.Open(root)
	require.NoError(t, err)
	t.Cleanup(func() { require.NoError(t, lib.Close()) })
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = NewReader(parser.NewReader())
	env.Runtime.Library = lib
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	// An inline label colliding with a physical path must not replace file text.
	require.Equal(t, lisp.LError, env.LoadString(file, "other-missing-symbol").Type)
	result := env.LoadFile("main.lisp")
	require.Equal(t, lisp.LError, result.Type)
	read := SourceReader(env.Runtime, result)
	source, err := read(file)
	require.NoError(t, err)
	assert.Equal(t, "missing-symbol ; physical source", string(source))
	// Even an existing file is unavailable without the active library.
	env.Runtime.Library = nil
	_, err = read(file)
	require.ErrorIs(t, err, os.ErrNotExist)
}

func TestInlineSourceBudget(t *testing.T) {
	const budget = 4 << 20
	for _, builtin := range []string{"load-string", "load-bytes"} {
		t.Run(builtin, func(t *testing.T) {
			env := lisp.NewEnv(nil)
			reader := NewReader(parser.NewReader())
			env.Runtime.Reader = reader
			require.True(t, lisp.InitializeUserEnv(env).IsNil())
			source := "42 ;" + strings.Repeat("x", (1<<20)-4)
			value := lisp.String(source)
			if builtin == "load-bytes" {
				value = lisp.Bytes([]byte(source))
			}
			env.Put(lisp.Symbol("source"), value)
			for i := 0; i < 12; i++ {
				result := env.LoadString("driver", fmt.Sprintf("(%s source :name \"source-%d\")", builtin, i))
				require.Equal(t, lisp.LInt, result.Type, result.String())
				retained := 0
				for _, text := range reader.inline {
					retained += len(text)
				}
				require.LessOrEqual(t, retained, budget, "retained inline source bytes after load %d", i)
			}
			assert.NotContains(t, reader.inline, "source-0")
			assert.Contains(t, reader.inline, "source-11")
		})
	}
}

func TestInlineSourceEvictionRendering(t *testing.T) {
	env := lisp.NewEnv(nil)
	reader := NewReader(parser.NewReader())
	env.Runtime.Reader = reader
	require.True(t, lisp.InitializeUserEnv(env).IsNil())
	old := env.LoadString("old", "missing-symbol ; old snippet")
	require.Equal(t, lisp.LError, old.Type)
	// A colliding library file must not supply a snippet for evicted inline text.
	root := t.TempDir()
	require.NoError(t, os.WriteFile(filepath.Join(root, "old"), []byte("wrong snippet"), 0o600))
	lib, err := rootlibrary.Open(root)
	require.NoError(t, err)
	t.Cleanup(func() { require.NoError(t, lib.Close()) })
	env.Runtime.Library = lib
	for i := 0; i < 5; i++ {
		require.Equal(t, lisp.LInt, env.LoadString(fmt.Sprint(i), "42 ;"+strings.Repeat("x", 1<<20)).Type)
	}
	recent := env.LoadString("recent", "missing-symbol ; recent snippet")
	require.Equal(t, lisp.LError, recent.Type)
	for _, tc := range []struct {
		result  *lisp.LVal
		snippet string
	}{
		{old, ""}, {recent, "recent snippet"},
	} {
		loc, ok := tc.result.Source()
		require.True(t, ok)
		message := (*lisp.ErrorVal)(tc.result).ErrorMessage()
		renderer := diagnostic.Renderer{Color: diagnostic.ColorNever, SourceReader: SourceReader(env.Runtime, tc.result)}
		var out bytes.Buffer
		require.NoError(t, renderer.Render(&out, diagnostic.Diagnostic{
			Message: message, Spans: []diagnostic.Span{{File: loc.File, Line: loc.Line, Col: loc.Col}},
		}))
		assert.Contains(t, out.String(), message)
		assert.Contains(t, out.String(), loc.File+":1:1")
		if tc.snippet == "" {
			assert.NotContains(t, out.String(), "snippet")
			_, err := SourceReader(env.Runtime, tc.result)(loc.File)
			assert.ErrorIs(t, err, os.ErrNotExist)
		} else {
			assert.Contains(t, out.String(), tc.snippet)
		}
	}
}

func TestInlineSourceCacheReplacement(t *testing.T) {
	reader := NewReader(parser.NewReader())
	load := func(name, source string) {
		_, err := reader.Read(name, strings.NewReader(source))
		require.NoError(t, err)
		retained := 0
		for _, text := range reader.inline {
			retained += cap(text)
		}
		assert.Equal(t, retained, reader.inlineBytes)
		assert.LessOrEqual(t, retained, inlineByteBudget)
		assert.Len(t, reader.order, len(reader.inline))
	}
	large := ";" + strings.Repeat("x", inlineByteBudget/2-1)
	load("first", large)
	load("second", large)
	load("first", large) // Replacement becomes the newest entry.
	load("third", "42")
	assert.NotContains(t, reader.inline, "second")
	assert.Contains(t, reader.inline, "first")
	assert.Contains(t, reader.inline, "third")
	load("first", ";"+strings.Repeat("x", inlineByteBudget))
	assert.NotContains(t, reader.inline, "first") // No stale text for an oversized replacement.
	assert.Contains(t, reader.inline, "third")
	load("third", "")
	assert.Empty(t, reader.inline)
	assert.Empty(t, reader.order)
	load("exact", ";"+strings.Repeat("x", inlineByteBudget-1))
	assert.Equal(t, inlineByteBudget, reader.inlineBytes)
}
