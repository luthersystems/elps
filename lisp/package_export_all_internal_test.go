// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"math/rand/v2"
	"testing"

	"github.com/stretchr/testify/require"
)

// exportAll must leave the export list exactly as calling exportSorted for
// each name in turn does, which is how `(export ...)` added names before it
// batched them.  The cases mix duplicates in one call, names already
// exported, an unsorted list from Export (with its own duplicates) and
// empty and one-name calls.
func TestExportAllMatchesExportSortedOneByOne(t *testing.T) {
	r := rand.New(rand.NewPCG(1, 2)) //nolint:gosec // G404: a fixed seed makes the random cases repeatable; nothing here is secret
	name := func() string { return fmt.Sprintf("s%d", r.IntN(40)) }
	for trial := range 500 {
		want, got := NewPackage("p"), NewPackage("p")
		for range r.IntN(6) {
			switch r.IntN(3) {
			case 0: // verbatim append, unsorted, may duplicate
				names := []string{name(), name()}
				want.Export(names...)
				got.Export(names...)
			default:
				names := make([]string, r.IntN(8))
				for i := range names {
					names[i] = name()
				}
				for _, n := range names {
					want.exportSorted(n)
				}
				got.exportAll(names)
			}
			require.Equal(t, want.Externals(), got.Externals(), "trial %d", trial)
		}
	}
}
