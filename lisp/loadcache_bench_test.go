// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"fmt"
	"io"
	"strings"
	"sync"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The refund this hook exists for, measured on the elps side.
//
// Issue #368 was filed as a correctness argument and has since become a cost
// argument: substrate closed the aliasing hazard on its own side by copying
// the cached parse on every hit, which is safe and which it measured at
// ~22 ms and 26.5 MiB per environment load.  This benchmark reproduces that
// trade in this repository, where all three arms can be measured against
// each other on one machine in one run:
//
//	no-cache            parse the file in every environment (today's
//	                    behaviour with no hook installed)
//	reader-cache-copy   the shape an EMBEDDER can write: cache the parsed
//	                    []*LVal behind a lisp.Reader and hand out a deep
//	                    Copy on every hit, because from outside the module
//	                    a copy is the only way to be sure
//	load-cache-alias    the hook: elps admits the parse once, seals it, and
//	                    hands the same sealed nodes to every environment
//
// env-only is the control: environment construction with no load at all.
// Every arm pays it, so it is what the reader subtracts to see the load
// cost by itself.  Without it the arms are three numbers whose differences
// are the only meaningful part, and it is easy to misread the ratio.
//
// The source is generated rather than read from testdata so the size is
// stated in the code and one arm cannot quietly measure a different file
// from another.

// benchSource generates a source file of roughly the shape a real library
// has: many small function definitions over quoted literals, which is what
// makes a parse expensive (nodes, not bytes).
func benchSource(defuns int) string {
	var sb strings.Builder
	sb.WriteString("(in-package 'user)\n")
	for i := range defuns {
		fmt.Fprintf(&sb, `
(defun bench-fn-%d (x y)
  (let ([table '((:a 1) (:b 2) (:c 3) (:d 4))]
        [names '("alpha" "beta" "gamma" "delta")])
    (if (< x y)
      (list x y table names)
      (list y x names table))))
`, i)
	}
	return sb.String()
}

// benchCacheSizes are the two points worth reporting: a library-sized file
// and a small one.  The small case is not decoration — the hook's cost is a
// SHA-256 of the source plus a map probe, and a file small enough for that
// to matter is where a cache could plausibly lose.
var benchCacheSizes = []struct {
	name   string
	defuns int
}{
	{"small", 8},
	{"library", 400},
}

// readerCacheCopy is the embedder-side cache, reproduced: a lisp.Reader in
// front of the real parser, holding the parsed slice and handing out a deep
// copy on every hit.  It is substrate's cacheReader with the buffering
// simplified away — the copy is the part being measured.
type readerCacheCopy struct {
	entries map[string][]*lisp.LVal
	inner   readLocationReader
	mu      sync.Mutex
}

func newReaderCacheCopy() *readerCacheCopy {
	return &readerCacheCopy{
		entries: make(map[string][]*lisp.LVal),
		inner:   parser.NewReader().(readLocationReader),
	}
}

func (c *readerCacheCopy) get(key string) []*lisp.LVal {
	c.mu.Lock()
	defer c.mu.Unlock()
	got, ok := c.entries[key]
	if !ok {
		return nil
	}
	out := make([]*lisp.LVal, len(got))
	for i, v := range got {
		out[i] = v.Copy()
	}
	return out
}

func (c *readerCacheCopy) put(key string, exprs []*lisp.LVal) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.entries[key] = exprs
}

func (c *readerCacheCopy) Read(name string, r io.Reader) ([]*lisp.LVal, error) {
	return c.ReadLocation(name, name, r)
}

func (c *readerCacheCopy) ReadLocation(name, loc string, r io.Reader) ([]*lisp.LVal, error) {
	src, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	key := loc + "\x00" + string(src)
	if got := c.get(key); got != nil {
		return got, nil
	}
	exprs, err := c.inner.ReadLocation(name, loc, bytes.NewReader(src))
	if err != nil {
		return nil, err
	}
	c.put(key, exprs)
	// The pristine parse stays in the cache and the caller gets a copy on
	// the miss path too, exactly as substrate's does: handing the stored
	// tree to the first caller would leave the cache aliased to that one
	// environment.
	return c.get(key), nil
}

// benchLoadCache is a plain map cache, which is all the hook asks an
// embedder for.
type benchLoadCache struct {
	entries map[string]*lisp.CachedSource
	mu      sync.Mutex
}

func newBenchLoadCache() *benchLoadCache {
	return &benchLoadCache{entries: make(map[string]*lisp.CachedSource)}
}

func (c *benchLoadCache) Load(key string) (*lisp.CachedSource, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	src, ok := c.entries[key]
	return src, ok
}

func (c *benchLoadCache) Store(key string, src *lisp.CachedSource) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.entries[key] = src
}

// newBenchEnv builds one environment, which is what a deployment does per
// warm VM.  The stdlib is deliberately NOT loaded: LoadLibrary dominates
// everything else and would bury the quantity under measurement.
func newBenchEnv(b *testing.B, reader lisp.Reader, cache lisp.LoadCache) *lisp.LEnv {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = reader
	env.Runtime.LoadCache = cache
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		b.Fatalf("could not initialize the environment: %v", rc)
	}
	return env
}

// BenchmarkLoadIntoEnv measures one file loaded into one fresh environment,
// which is the unit a preheated VM pool multiplies.  Arms differ only in
// what stands between the bytes and the expressions.
func BenchmarkLoadIntoEnv(b *testing.B) {
	for _, size := range benchCacheSizes {
		src := benchSource(size.defuns)
		name := fmt.Sprintf("size=%s-%dKiB", size.name, len(src)/1024)

		b.Run(name+"/arm=env-only", func(b *testing.B) {
			reader := parser.NewReader()
			b.ReportAllocs()
			for range b.N {
				newBenchEnv(b, reader, nil)
			}
		})

		b.Run(name+"/arm=no-cache", func(b *testing.B) {
			reader := parser.NewReader()
			b.ReportAllocs()
			for range b.N {
				env := newBenchEnv(b, reader, nil)
				loadBenchSource(b, env, src)
			}
		})

		b.Run(name+"/arm=reader-cache-copy", func(b *testing.B) {
			reader := newReaderCacheCopy()
			// Warm: the miss path is not what a preheated pool pays.
			loadBenchSource(b, newBenchEnv(b, reader, nil), src)
			b.ResetTimer()
			b.ReportAllocs()
			for range b.N {
				env := newBenchEnv(b, reader, nil)
				loadBenchSource(b, env, src)
			}
		})

		b.Run(name+"/arm=load-cache-alias", func(b *testing.B) {
			reader := parser.NewReader()
			cache := newBenchLoadCache()
			loadBenchSource(b, newBenchEnv(b, reader, cache), src)
			b.ResetTimer()
			b.ReportAllocs()
			for range b.N {
				env := newBenchEnv(b, reader, cache)
				loadBenchSource(b, env, src)
			}
		})
	}
}

func loadBenchSource(b *testing.B, env *lisp.LEnv, src string) {
	v := env.LoadLocation("bench.lisp", "bench.lisp", strings.NewReader(src))
	if v.Type == lisp.LError {
		b.Fatalf("load failed: %v", v)
	}
}

// TestBenchmarkArmsAgree is the benchmark's anti-vacuity gate.
//
// A benchmark that measures the wrong thing is worse than no benchmark, and
// the specific way this one could go wrong is silent: an arm whose load
// failed, or whose cache never hit, would post a beautiful number for doing
// nothing.  This asserts every arm ends in the same environment state, and
// that the two cached arms really do parse once.
func TestBenchmarkArmsAgree(t *testing.T) {
	t.Parallel()
	src := benchSource(4)

	states := make(map[string]string)

	plain := newCountingReader()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = plain
	require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
	require.NotEqual(t, lisp.LError, env.LoadLocation("bench.lisp", "bench.lisp", strings.NewReader(src)).Type)
	states["no-cache"] = envStateDump(t, env)

	copyCache := newReaderCacheCopy()
	for i := range 2 {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = copyCache
		require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
		require.NotEqual(t, lisp.LError, env.LoadLocation("bench.lisp", "bench.lisp", strings.NewReader(src)).Type)
		if i == 1 {
			states["reader-cache-copy"] = envStateDump(t, env)
		}
	}

	aliasReader := newCountingReader()
	aliasCache := newBenchLoadCache()
	for i := range 2 {
		env := lisp.NewEnv(nil)
		env.Runtime.Reader = aliasReader
		env.Runtime.LoadCache = aliasCache
		require.NotEqual(t, lisp.LError, lisp.InitializeUserEnv(env).Type)
		require.NotEqual(t, lisp.LError, env.LoadLocation("bench.lisp", "bench.lisp", strings.NewReader(src)).Type)
		if i == 1 {
			states["load-cache-alias"] = envStateDump(t, env)
		}
	}
	assert.Equal(t, 1, aliasReader.reads, "the aliasing arm must parse the source exactly once")
	assert.Len(t, aliasCache.entries, 1)

	assert.Equal(t, states["no-cache"], states["reader-cache-copy"],
		"the copying arm must reach the same environment state as an uncached load")
	assert.Equal(t, states["no-cache"], states["load-cache-alias"],
		"the aliasing arm must reach the same environment state as an uncached load")
}
