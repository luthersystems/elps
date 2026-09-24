// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import "sync/atomic"

// lazyGuard detects two goroutines materializing template values in one VM
// at the same time, which breaks the Template.NewVM contract (a VM is used
// by one goroutine at a time). Materialization is reentrant only through the
// work queue, never through a nested ref, so any overlap is a second goroutine.
type lazyGuard struct {
	busy atomic.Int32
}

const lazyConcurrentMessage = "lisp: concurrent lazy template instantiation: a VM from Template.NewVM " +
	"must be used by one goroutine at a time (use TemplateWithEagerInstantiation to share read-only access)"

func (g *lazyGuard) enter() {
	if !g.busy.CompareAndSwap(0, 1) {
		panic(lazyConcurrentMessage)
	}
}

func (g *lazyGuard) leave() {
	g.busy.Store(0)
}
