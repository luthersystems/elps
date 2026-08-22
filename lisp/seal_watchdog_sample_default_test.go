// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// sealWatchSampleRoots returns nothing in untagged builds: the
// checked-mode inspector records no parses without the elpscheck tag, so
// the watchdog watches only explicitly registered trees.  See
// seal_watchdog_sample_elpscheck_test.go for the armed version.
func sealWatchSampleRoots(_ int) []*LVal {
	return nil
}
