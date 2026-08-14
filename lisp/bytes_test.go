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
			// Two appends off the same source are independent (issue #373).
			// This row used to assert "abce": the append above grew into
			// spare capacity left in v123 and rewrote v1234, a value
			// `append` had already returned and promised not to touch.
			// `append 'bytes` now clamps its input so it reallocates
			// instead; `append-bytes!` is the in-place variant.
			{`(to-string v1234)`, `"abcd"`, ``},
		}},
		// `append` validates the type SPECIFIER and then dispatched to the
		// bytes path without validating the sequence, so a non-bytes second
		// argument reached LVal.Bytes() and panicked ("not bytes: int").  In
		// an embedded interpreter that surfaces as an internal-panic
		// condition, which ignore-errors and a catch-all handler-bind both
		// refuse to contain by design; in a phylum it is a chaincode crash.
		// append-bytes has always had this guard -- these rows pin that
		// `append 'bytes` now mirrors it.  Found by FuzzEval (issue #320).
		{"append 'bytes rejects a non-bytes sequence", elpstest.TestSequence{
			{`(append 'bytes 0)`, `test:1:1: lisp:append: second argument is not bytes: int`, ``},
			{`(append 'bytes "s")`, `test:1:1: lisp:append: second argument is not bytes: string`, ``},
			{`(append 'bytes ())`, `test:1:1: lisp:append: second argument is not bytes: list`, ``},
			{`(append 'bytes 'x)`, `test:1:1: lisp:append: second argument is not bytes: symbol`, ``},
			{`(append 'bytes (vector 1))`, `test:1:1: lisp:append: second argument is not bytes: array`, ``},
			// ...and the valid form still works.
			{`(to-string (append 'bytes (to-bytes "ab") 99))`, `"abc"`, ``},
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
			// Two appends off the same source are independent (issue #373).
			// This row used to assert "abce" -- see the note in the
			// `append 'bytes` sequence above.  `append-bytes!` is the
			// in-place variant and keeps its amortised growth.
			{`(to-string v1234)`, `"abcd"`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
