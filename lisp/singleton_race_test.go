// Copyright © 2025 The ELPS authors

// Singleton mutation regression test for issue #274.
//
// Pre-fix: stampMacroExpansion (macro.go) mutated LVal.source and
// .MacroExpansion on every recursive node, and its singleton guard
// checked only for empty LSExpr (Nil), missing singletonTrue /
// singletonFalse (LSymbol values with Source.Pos == -1). Every macro
// expansion that recursed into a Bool() result therefore overwrote the
// shared global singleton, racing with every concurrent
// macro-expanding goroutine.
//
// With the fix in place, stampMacroExpansion uses an identity-based
// guard (isSingleton) and never mutates any of the three singletons.
// This test hammers it concurrently and asserts the singletons are
// untouched. Run with `go test -race` to confirm no data race.

package lisp

import (
	"sync"
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

func TestSingletonRaceRegression(t *testing.T) {
	const (
		goroutines = 8
		iterations = 2000
	)

	origTrueSource := Bool(true).source
	origFalseSource := Bool(false).source

	rt := &Runtime{}
	var wg sync.WaitGroup
	wg.Add(goroutines)

	for g := range goroutines {
		go func(g int) {
			defer wg.Done()
			callSite := &token.Location{
				File: "racer.lisp",
				Pos:  100 + g,
				Line: g + 1,
				Col:  g + 1,
			}
			ctx := &MacroExpansionContext{CallSite: callSite, Name: "lisp:test"}
			for range iterations {
				// Each goroutine repeatedly attempts to stamp the global
				// singletonTrue and singletonFalse. With the fix the calls
				// are no-ops on the singletons.
				stampMacroExpansion(Bool(true), callSite, ctx, rt)
				stampMacroExpansion(Bool(false), callSite, ctx, rt)
			}
		}(g)
	}

	wg.Wait()

	if Bool(true).source != origTrueSource {
		t.Errorf("Bool(true).source mutated by concurrent macro stamping: got %+v want %+v",
			Bool(true).source, origTrueSource)
	}
	if Bool(true).MacroExpansion != nil {
		t.Errorf("Bool(true).MacroExpansion mutated by concurrent macro stamping: got %+v",
			Bool(true).MacroExpansion)
	}
	if Bool(false).source != origFalseSource {
		t.Errorf("Bool(false).source mutated by concurrent macro stamping: got %+v want %+v",
			Bool(false).source, origFalseSource)
	}
	if Bool(false).MacroExpansion != nil {
		t.Errorf("Bool(false).MacroExpansion mutated by concurrent macro stamping: got %+v",
			Bool(false).MacroExpansion)
	}
}
