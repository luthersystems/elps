// Copyright © 2026 The ELPS authors

package lval

import "testing"

// FuzzOnePassChecks: str's one-pass check and parseInt's fast path agree
// with the slow forms they replaced, on any bytes.  One target for both
// keeps the nightly fuzz sweep inside its shard budget
// (scripts/fuzz-budget-check.sh).
func FuzzOnePassChecks(f *testing.F) {
	for _, s := range strSeeds {
		f.Add([]byte(s))
	}
	for _, s := range intSeeds {
		f.Add([]byte(s))
	}
	f.Fuzz(func(t *testing.T, b []byte) {
		checkParseIntMatchesStrconv(t, string(b))
		if len(b) == 0 || b[0] != '"' {
			b = append([]byte{'"'}, b...)
		}
		checkStrMatchesReencode(t, b)
	})
}
