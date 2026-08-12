// Copyright © 2018 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
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

func BenchmarkPackage(b *testing.B) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunBenchmarkFile(b, "libjson_test.lisp")
}
