// Copyright © 2018 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

func TestBytes(t *testing.T) {
	// NOTE:  These tests mirror tests for vectors (array_test.go)
	tests := elpstest.TestSuite{
		{"append-bytes!", elpstest.TestSequence{
			{`(set 'v (to-bytes ""))`, `#<bytes>`, ""},
			{`(to-string (append-bytes! v "a"))`, `"a"`, ""},
			{`(to-string (append-bytes! v (to-bytes "b")))`, `"ab"`, ""},
			{`(to-string (append-bytes! v '(99)))`, `"abc"`, ""},
			{`(to-string v)`, `"abc"`, ""},
		}},
		{"append!", elpstest.TestSequence{
			{`(set 'v (to-bytes ""))`, `#<bytes>`, ""},
			{`(to-string (append! v 97))`, `"a"`, ""},
			{`(to-string (append! v 98))`, `"ab"`, ""},
			{`(to-string (append! v 99))`, `"abc"`, ""},
			{`(to-string v)`, `"abc"`, ""},
		}},
		{"append 'bytes", elpstest.TestSequence{
			{`(set 'v (to-bytes ""))`, `#<bytes>`, ``},
			{`(set 'v1 (append 'bytes v 97))`, `#<bytes 97>`, ``},
			{`(set 'v12 (append 'bytes v1 98))`, `#<bytes 97 98>`, ``},
			{`(set 'v123 (append 'bytes v12 99))`, `#<bytes 97 98 99>`, ``},
			{`(set 'v1234 (append 'bytes v123 100))`, `#<bytes 97 98 99 100>`, ``},
			{`(to-string v)`, `""`, ``},
			{`(to-string v1)`, `"a"`, ``},
			{`(to-string v12)`, `"ab"`, ``},
			{`(to-string v123)`, `"abc"`, ``},
			{`(to-string v1234)`, `"abcd"`, ``},
			{`(set 'v1235 (append 'bytes v123 101))`, `#<bytes 97 98 99 101>`, ``},
			// The above append reuses excess vector capacity allocated in the
			// previous append to v12, creating v123.  Analogous to go slices,
			// this causes side effects in the value of v1234.  The assumed
			// performance benefit seems valuable but in general (append ...)
			// should be used sparingly and with care.  The append!  function
			// will be easier to reason about.
			{`(to-string v1234)`, `"abce"`, ``},
		}},
		{"append-bytes", elpstest.TestSequence{
			{`(set 'v (to-bytes ""))`, `#<bytes>`, ``},
			{`(set 'v1 (append-bytes v "a"))`, `#<bytes 97>`, ``},
			{`(set 'v12 (append-bytes v1 (to-bytes "b")))`, `#<bytes 97 98>`, ``},
			{`(set 'v123 (append-bytes v12 (vector 99)))`, `#<bytes 97 98 99>`, ``},
			{`(set 'v1234 (append-bytes v123 "d"))`, `#<bytes 97 98 99 100>`, ``},
			{`(to-string v)`, `""`, ``},
			{`(to-string v1)`, `"a"`, ``},
			{`(to-string v12)`, `"ab"`, ``},
			{`(to-string v123)`, `"abc"`, ``},
			{`(to-string v1234)`, `"abcd"`, ``},
			{`(set 'v1235 (append-bytes v123 "e"))`, `#<bytes 97 98 99 101>`, ``},
			// The above append reuses excess vector capacity allocated in the
			// previous append to v12, creating v123.  Analogous to go slices,
			// this causes side effects in the value of v1234.  The assumed
			// performance benefit seems valuable but in general (append-bytes
			// ...) should be used sparingly and with care.  The append-bytes!
			// function will be easier to reason about.
			{`(to-string v1234)`, `"abce"`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
