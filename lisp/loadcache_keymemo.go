// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"sync"
	"sync/atomic"
)

// Digest memo for loadCacheKey (luthersystems/substrate#548).
//
// Every cached load drains its stream into a fresh []byte and, before this
// memo, SHA-256'd the whole thing to derive the cache key.  On runners
// without SHA extensions that is ~250 MB/s — ~1.7 ms per load of a 430 KB
// phylum, ~7% of a cold substrate load — paid again for bytes the process
// has already hashed.
//
// The memo remembers, per load identity (name, loc, reader identity, reader
// method), a PRIVATE COPY of the last source bytes seen and their digest.  A
// later load of the same identity reuses the digest only if its bytes are
// byte-for-byte equal to that copy (bytes.Equal: memcmp speed, an order of
// magnitude or more faster than SHA-256 without hardware support).  Nothing
// is keyed on slice identity or on a sampled/cheap check, so:
//
//   - different content can never be handed another content's key — a
//     reused digest is by construction the digest of identical input, so the
//     key is exactly what loadCacheKey would compute;
//   - a caller that mutates its slice after (or during a later) load cannot
//     poison the memo, because the memo compares against its own copy.
//
// The memo changes only how the key string is obtained, never its value, so
// evaluation results, errors and step counts are unaffected.
//
// It is process-wide (a cold substrate load builds a fresh Runtime, so a
// per-Runtime memo would never hit) and bounded: sources larger than
// loadCacheKeyMemoMaxSource are not memoised, and when the retained bytes
// would exceed loadCacheKeyMemoMaxBytes the memo is emptied.  A miss merely
// costs the hash it always cost.

const (
	loadCacheKeyMemoMaxSource = 8 << 20
	loadCacheKeyMemoMaxBytes  = 16 << 20
	// loadCacheKeyMemoEntryOverhead is charged per entry on top of its
	// strings and source copy, so many tiny (or empty) sources under
	// distinct names cannot grow the map without bound.
	loadCacheKeyMemoEntryOverhead = 128
)

type loadCacheKeyMemoID struct {
	name, loc, readerID string
	byLoc               bool
}

type loadCacheKeyMemoEntry struct {
	key  string
	src  []byte // private copy; never aliased to a caller's slice
	cost int    // bytes charged against loadCacheKeyMemoMaxBytes
}

var (
	loadCacheKeyMemoMu    sync.Mutex
	loadCacheKeyMemoMap   map[loadCacheKeyMemoID]loadCacheKeyMemoEntry
	loadCacheKeyMemoBytes int

	// loadCacheKeyDigests counts SHA-256 key derivations (tests only read it).
	loadCacheKeyDigests atomic.Int64
)

// memoLoadCacheKey returns loadCacheKey(name, loc, opts), skipping the digest
// when the source bytes and identity match the last cached entry.
func memoLoadCacheKey(name string, loc string, opts loadKeySource) string {
	readerID, byLoc, src := opts.readerID, opts.byLoc, opts.src

	if len(src) > loadCacheKeyMemoMaxSource {
		return loadCacheKey(name, loc, loadKeySource{readerID: readerID, byLoc: byLoc, src: src})
	}
	id := loadCacheKeyMemoID{name: name, loc: loc, readerID: readerID, byLoc: byLoc}
	loadCacheKeyMemoMu.Lock()
	e, ok := loadCacheKeyMemoMap[id]
	loadCacheKeyMemoMu.Unlock()
	if ok && bytes.Equal(e.src, src) {
		return e.key
	}
	key := loadCacheKey(name, loc, loadKeySource{readerID: readerID, byLoc: byLoc, src: src})
	cp := bytes.Clone(src)
	if cp == nil {
		cp = []byte{}
	}
	loadCacheKeyMemoMu.Lock()
	defer loadCacheKeyMemoMu.Unlock()
	if old, ok := loadCacheKeyMemoMap[id]; ok {
		loadCacheKeyMemoBytes -= old.cost
		delete(loadCacheKeyMemoMap, id)
	}
	cost := len(cp) + len(key) + len(name) + len(loc) + len(readerID) + loadCacheKeyMemoEntryOverhead
	if loadCacheKeyMemoMap == nil || loadCacheKeyMemoBytes+cost > loadCacheKeyMemoMaxBytes {
		loadCacheKeyMemoMap = make(map[loadCacheKeyMemoID]loadCacheKeyMemoEntry)
		loadCacheKeyMemoBytes = 0
	}
	loadCacheKeyMemoMap[id] = loadCacheKeyMemoEntry{key: key, src: cp, cost: cost}
	loadCacheKeyMemoBytes += cost
	return key
}

// loadCacheKeyMemoUsage reports the retained entry count and charged bytes
// (tests).
func loadCacheKeyMemoUsage() (int, int) {
	loadCacheKeyMemoMu.Lock()
	defer loadCacheKeyMemoMu.Unlock()
	return len(loadCacheKeyMemoMap), loadCacheKeyMemoBytes
}

// resetLoadCacheKeyMemo empties the memo (tests).
func resetLoadCacheKeyMemo() {
	loadCacheKeyMemoMu.Lock()
	defer loadCacheKeyMemoMu.Unlock()
	loadCacheKeyMemoMap = nil
	loadCacheKeyMemoBytes = 0
}
