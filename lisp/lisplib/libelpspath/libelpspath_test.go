// Copyright © 2026 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libelpspath_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// TestPackage runs elpspath lisp tests.
func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libelpspath_test.lisp")
}
