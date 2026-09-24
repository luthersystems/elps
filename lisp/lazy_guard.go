// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// lazyGuard detects concurrent lazy materialization in checked builds only;
// production builds compile it out.
type lazyGuard struct{}

func (lazyGuard) enter() {}
func (lazyGuard) leave() {}
