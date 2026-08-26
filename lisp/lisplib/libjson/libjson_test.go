// Copyright © 2018 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libjson_test.lisp")
}

// TestPackageCyclicValue runs the lisp-level tests for values that contain
// themselves (issue #390).  They live in a file of their own because
// BenchmarkPackage's $load sub-benchmark parses and evaluates
// libjson_test.lisp on every iteration: source added to that file is charged
// to a benchmark that exists to measure the loader, and the CI benchmark gate
// reads the jump as a regression in the loader itself.
func TestPackageCyclicValue(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libjson_cycle_test.lisp")
}

// TestPackageExactIntegers runs the lisp-level tests for issue #350.  Same
// reasoning as TestPackageCyclicValue for why they get their own file.
func TestPackageExactIntegers(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libjson_integer_test.lisp")
}

func BenchmarkPackage(b *testing.B) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunBenchmarkFile(b, "libjson_test.lisp")
}

// TestGoValueBytes covers the LBytes arm of the deprecated
// Serializer.GoValue, which carried the same defect lisp.GoValue did until
// issue #548: `return v.Bytes` returns a bound METHOD VALUE, a func()
// []byte, where every other arm returns data.
//
// It survived the fix to the other one because that fix's own comment cited
// this method -- as "a different GoValue" -- without checking it for the
// same bug. An adversarial review caught that. Hence a test rather than
// only a fix: the two implementations are independent and can drift again.
//
// Asserts the concrete dynamic type for the reason its counterpart in
// lisp/embed_test.go does: the arm's result is interface{}, so the wrong
// type compiles and only fails at use, far from the mistake.
func TestGoValueBytes(t *testing.T) {
	src := []byte("here I stand")
	// Captured before anything runs: lisp.Bytes stores a slice header over
	// src's OWN backing array, so an assertion phrased against src after a
	// mutation compares two values that both changed.
	want := string(src)

	v := lisp.Bytes(src)
	got := libjson.DefaultSerializer().GoValue(v, false)
	b, ok := got.([]byte)
	if !ok {
		t.Fatalf("Serializer.GoValue of an LBytes returned %T, want []byte", got)
	}
	if string(b) != want {
		t.Errorf("Serializer.GoValue returned %q, want %q", b, want)
	}

	// And it is a copy, so a write through the result cannot reach the lisp
	// value -- this method is exported and documented as kept for outside
	// callers, which is precisely who would mutate it.
	b[0] = 'H'
	if after := string(v.Bytes()); after != want {
		t.Errorf("writing through Serializer.GoValue's result changed the lisp value to %q, want %q",
			after, want)
	}
}
