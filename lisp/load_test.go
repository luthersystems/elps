// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestLoad(t *testing.T) {
	tests := elpstest.TestSuite{
		{"load simple strings", elpstest.TestSequence{
			{`(load-string "(+ 2 3)")`, "5", ""},
			{`((load-string "(lambda (x) (* x 2))") 4)`, "8", ""},
			{`(load-string "(defun double (x) (* x 2))")`, "()", ""},
			{`(double 3)`, "6", ""},
			{`(load-string "(defun double (x) (- (* x 2)))")`, "()", ""},
			{`(double 3)`, "-6", ""},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
