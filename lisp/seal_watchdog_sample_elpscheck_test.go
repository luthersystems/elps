// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

// sealWatchSampleRoots feeds the seal write watchdog from the
// checked-mode inspector's registry: every sealed parse the process has
// recorded is eligible, and each call returns a fresh bounded sample (Go
// map iteration order rotates it).  This is what makes `go test -race
// -tags elpscheck ./lisp/` the fully armed configuration — the whole
// suite's parses take turns under unsynchronized read.
func sealWatchSampleRoots(limit int) []*LVal {
	return sealCheckSampleRoots(limit)
}
