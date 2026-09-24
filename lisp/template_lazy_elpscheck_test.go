// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import (
	"strings"
	"testing"
)

// TestTemplateLazyConcurrentFillPanics simulates a second goroutine arriving
// while a fill is in progress (the guard is held) and requires the checked
// build's clear panic rather than a silent race.
func TestTemplateLazyConcurrentFillPanics(t *testing.T) {
	vm, err := lazyFixture(t).NewVM()
	if err != nil {
		t.Fatal(err)
	}
	lib := vm.Runtime.Registry.Package("lib")
	inst := lib.lazy.inst
	inst.guard.enter() // another goroutine is mid-fill
	defer func() {
		r := recover()
		msg, _ := r.(string)
		if !strings.Contains(msg, "must be used by one goroutine at a time") {
			t.Fatalf("recovered %v, want the concurrent-instantiation panic", r)
		}
		inst.guard.leave()
		if v, _ := lib.Symbol("v1"); v.Cells[0].Int != 1 {
			t.Fatalf("guard left the VM unusable: %v", v)
		}
	}()
	lib.Symbol("v0")
	t.Fatal("concurrent fill was not detected")
}
