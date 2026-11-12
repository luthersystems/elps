// Copyright © 2026 The ELPS authors

package libregexp

import "github.com/luthersystems/elps/lisp"

// ProgramSize is programSize, for the tests of the match charge.
var ProgramSize = programSize

// SizeCacheBytes and SizeCacheEntries are the patternCache bounds, for the
// tests of the cache.
const (
	SizeCacheBytes    = patternCacheBytes
	SizeCacheEntries  = patternCacheEntries
	MaxCacheProgBytes = maxCacheProgBytes
	MaxEntryProgBytes = maxEntryProgBytes
)

// ProgramBytes is programBytes of the program of pattern.
func ProgramBytes(pattern string) int {
	prog := compileProgram(pattern)
	if prog == nil {
		return 0
	}
	return programBytes(prog)
}

// CachedProgBytes returns the program bytes rt's patternCache counts.
func CachedProgBytes(rt *lisp.Runtime) int {
	c, _ := rt.VMCache(patternCacheKey{}).(*patternCache)
	if c == nil {
		return 0
	}
	return c.progBytes
}

// CachedSizes returns the program sizes rt's patternCache holds.
func CachedSizes(rt *lisp.Runtime) map[string]int {
	out := map[string]int{}
	c, _ := rt.VMCache(patternCacheKey{}).(*patternCache)
	if c == nil {
		return out
	}
	for k, e := range c.entries {
		if e.sized {
			out[k] = e.insts
		}
	}
	return out
}

// CachedPatterns returns the patterns rt's patternCache holds, each with
// whether it holds the compiled matcher, and the bytes it counts for them.
func CachedPatterns(rt *lisp.Runtime) (map[string]bool, int) {
	out := map[string]bool{}
	c, _ := rt.VMCache(patternCacheKey{}).(*patternCache)
	if c == nil {
		return out, 0
	}
	for k, e := range c.entries {
		out[k] = e.re != nil
	}
	return out, c.bytes
}
