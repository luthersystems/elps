// Copyright © 2026 The ELPS authors

package lisp

import "sync"

// MapLoadCache is a LoadCache that keeps every entry until Clear.  It is the
// cache an embedder installs when it wants no eviction policy of its own.
// It is safe for concurrent use: every Runtime of a process may share one.
// The zero value is an empty cache ready to use.
//
// It retains every distinct source a Runtime loads, including source that
// guest code passes to load-string or load-bytes.  A host that evaluates
// many distinct one-off programs calls Clear, or installs a bounded cache.
//
// Clear drops the cache's references only.  A Runtime that already loaded an
// entry keeps its program until the Runtime is collected, and the next load
// of that source parses it again.
type MapLoadCache struct {
	m  map[string]*CachedSource
	mu sync.RWMutex
}

var _ LoadCache = (*MapLoadCache)(nil)

// NewMapLoadCache returns an empty MapLoadCache.  It is the same as
// &MapLoadCache{}.
func NewMapLoadCache() *MapLoadCache {
	return &MapLoadCache{m: make(map[string]*CachedSource)}
}

// Load implements LoadCache.
func (c *MapLoadCache) Load(key string) (*CachedSource, bool) {
	c.mu.RLock()
	defer c.mu.RUnlock()
	src, ok := c.m[key]
	return src, ok
}

// Store implements LoadCache.  It only writes the map: a Store that loaded
// source would re-enter the load path that called it.
func (c *MapLoadCache) Store(key string, src *CachedSource) {
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.m == nil {
		c.m = make(map[string]*CachedSource)
	}
	c.m[key] = src
}

// Clear drops every entry.
func (c *MapLoadCache) Clear() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.m = make(map[string]*CachedSource)
}

// Len returns the number of entries.
func (c *MapLoadCache) Len() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return len(c.m)
}
