// Copyright © 2025 The ELPS authors

//go:build elpscheck

package lisp

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// TestCheckSingleton_DetectsCorruption verifies that the elpscheck
// build tag actually catches singleton mutation. Run with:
//
//	go test -tags elpscheck -run TestCheckSingleton_DetectsCorruption ./lisp/
//
// We mutate Bool(true).Source, then call checkSingleton — it should
// panic. The test restores the singleton before returning so it does
// not poison the rest of the suite (TestMain would otherwise fail).
func TestCheckSingleton_DetectsCorruption(t *testing.T) {
	orig := singletonTrue.Source
	defer func() {
		singletonTrue.Source = orig
	}()

	singletonTrue.Source = nil

	defer func() {
		r := recover()
		if r == nil {
			t.Fatal("expected checkSingleton to panic, got nil")
		}
		msg, ok := r.(string)
		if !ok {
			t.Fatalf("expected panic to be a string, got %T: %v", r, r)
		}
		assert.Contains(t, msg, "Bool(true)",
			"panic message should name the offending singleton; got: %s", msg)
	}()

	checkSingleton(singletonTrue)
}
