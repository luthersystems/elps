// Copyright © 2026 The ELPS authors

package elpstest

import (
	"crypto/sha256"
	"encoding/binary"
	"io"
	"time"

	"github.com/luthersystems/elps/lisp"
)

// DefaultTestTime is the first time a Determinism clock returns when its
// Start is zero: 2000-01-01T00:00:00Z.
var DefaultTestTime = time.Date(2000, time.January, 1, 0, 0, 0, 0, time.UTC)

// Determinism makes the time- and randomness-dependent parts of a test
// environment reproducible. Set it on a Runner and every environment the
// runner builds gets a fresh StepClock and a fresh SeededEntropy reader, so
// each test sees the same times and the same random bytes on every run, in
// any order, on any machine.
//
// The clock is installed as Runtime.Clock (read by time:utc-now and anything
// else calling Runtime.Now) and the reader as Runtime.Entropy (read by
// builtins calling Runtime.Random, such as an embedder's UUID generator).
// Both are in place before LoaderFn runs, so values read while loading are
// reproducible too.
type Determinism struct {
	// Start is the clock's first reading. Zero means DefaultTestTime.
	Start time.Time
	// Step is how far the clock advances after each reading. Zero freezes
	// the clock at Start.
	Step time.Duration
	// Seed seeds the entropy reader. Different seeds give different streams.
	Seed uint64
}

// Apply installs a fresh clock and entropy reader on env's runtime.
func (d *Determinism) Apply(env *lisp.LEnv) {
	if d == nil || env == nil || env.Runtime == nil {
		return
	}
	start := d.Start
	if start.IsZero() {
		start = DefaultTestTime
	}
	env.Runtime.Clock = StepClock(start, d.Step)
	env.Runtime.Entropy = SeededEntropy(d.Seed)
}

// StepClock returns a clock whose first reading is start and which advances
// by step after every reading. A zero step gives a frozen clock. The clock is
// not safe for concurrent use, like the Runtime it is installed on.
func StepClock(start time.Time, step time.Duration) func() time.Time {
	next := start
	return func() time.Time {
		now := next
		next = next.Add(step)
		return now
	}
}

// SeededEntropy returns a reader producing a deterministic stream of bytes
// derived from seed. It never returns an error. The stream is SHA-256 in
// counter mode -- block i is SHA-256(seed || i), both big-endian uint64 -- so
// it is fully specified and the same on every platform and Go release. It is
// for tests only: the stream is predictable by design and must never back
// real identifiers or keys. Like the Runtime it is installed on, the reader
// is not safe for concurrent use.
func SeededEntropy(seed uint64) io.Reader {
	return &seededReader{seed: seed, used: sha256.Size}
}

type seededReader struct {
	seed    uint64
	counter uint64
	block   [sha256.Size]byte
	used    int // bytes of block already returned; len(block) means empty
}

func (r *seededReader) Read(p []byte) (int, error) {
	n := 0
	for n < len(p) {
		if r.used == len(r.block) {
			var in [16]byte
			binary.BigEndian.PutUint64(in[:8], r.seed)
			binary.BigEndian.PutUint64(in[8:], r.counter)
			r.counter++
			r.block = sha256.Sum256(in[:])
			r.used = 0
		}
		c := copy(p[n:], r.block[r.used:])
		r.used += c
		n += c
	}
	return n, nil
}
