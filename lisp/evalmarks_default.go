// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// evalMarksReused is true in a build that reuses a consumed trampoline mark.
const evalMarksReused = true

// poisonEvalMark does nothing outside elpscheck builds, where a consumed mark
// is reused and no release calls it.  See evalmarks_elpscheck.go.
func poisonEvalMark(_ *LVal) {}
