// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// callListsReused is true in a build that reuses a returned call list.
const callListsReused = true

// poisonCallList does nothing outside elpscheck builds, where a returned
// call list is reused and put never calls it.  See calllist_elpscheck.go.
func poisonCallList(_ *LVal, _ []*LVal) {}
