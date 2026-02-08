// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestAllocLimits(t *testing.T) {
	tests := elpstest.TestSuite{
		{"make-sequence allocation limit", elpstest.TestSequence{
			// Normal usage within limits.
			{`(length (make-sequence 0 100))`, `100`, ""},
			// Exceeding the default limit (10Mi elements) must produce an error.
			{`(make-sequence 0 20000000)`, `test:1:1: lisp:make-sequence: make-sequence would exceed maximum allocation size (10485760 elements)`, ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
