//go:build race

package lisp_test

// raceEnabled: under -race, sync.Pool drops a share of Puts at random, so
// allocation counts of pooled paths are not deterministic.
const raceEnabled = true
