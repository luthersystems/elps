// Copyright © 2026 The ELPS authors

package lisp

// The names of the copy-on-write-on-sealed sites, shared by the checked-
// mode census (lisp/cow_check_elpscheck.go) and the sites themselves so
// that a rename cannot silently split a site's counts in two.
//
// These are untyped constants: the production build's recordCoWOnSealed
// is an empty inlined function (lisp/cow_check_default.go), so the
// arguments die with the call and no site string reaches a release
// binary.
const (
	// cowSiteSortStable is stable-sort finding a sealed list: it sorts a
	// fresh header over a fresh backing array and returns that instead of
	// sorting in place.
	cowSiteSortStable = "stable-sort"

	// cowSiteSliceVector is (slice 'vector ...) over a sealed sequence:
	// vectors are mutable through append!, so the sealed backing array
	// must not be wrapped in one.
	cowSiteSliceVector = "slice-vector"

	// cowSiteAppendVector is (append 'vector ...) over a sealed sequence:
	// appending into spare backing capacity would write the shared
	// program's storage.
	cowSiteAppendVector = "append-vector"
)
