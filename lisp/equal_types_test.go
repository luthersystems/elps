// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// equal? is documented as a deep comparison across all value types, but bytes
// values and reader quotes (a value quoted more than once) used to fall
// through both equality walkers to false -- a bytes value was not even
// equal? to itself.
func TestEqualBytesAndQuote(t *testing.T) {
	tests := elpstest.TestSuite{
		{"equal? on bytes", elpstest.TestSequence{
			{`(let ((b (to-bytes "ab"))) (equal? b b))`, `true`, ``},
			{`(equal? (to-bytes "ab") (to-bytes "ab"))`, `true`, ``},
			{`(equal? (to-bytes "") (to-bytes ""))`, `true`, ``},
			{`(equal? (slice 'bytes (to-bytes "abc") 0 1) (to-bytes "a"))`, `true`, ``},
			{`(equal? (to-bytes "a") (to-bytes "b"))`, `false`, ``},
			{`(equal? (to-bytes "a") (to-bytes "ab"))`, `false`, ``},
			// Bytes never equal a string or a vector of the same octets.
			{`(equal? (to-bytes "a") "a")`, `false`, ``},
			{`(equal? (to-bytes "a") (vector 97))`, `false`, ``},
		}},
		{"equal? on bytes inside containers", elpstest.TestSequence{
			{`(equal? (list (to-bytes "a")) (list (to-bytes "a")))`, `true`, ``},
			{`(equal? (vector (to-bytes "a")) (vector (to-bytes "b")))`, `false`, ``},
			{`(equal? (sorted-map "k" (to-bytes "a")) (sorted-map "k" (to-bytes "a")))`, `true`, ``},
			{`(equal? (sorted-map "k" (to-bytes "a")) (sorted-map "k" (to-bytes "b")))`, `false`, ``},
		}},
		{"equal? on reader quotes", elpstest.TestSequence{
			{`(equal? ''a ''a)`, `true`, ``},
			{`(equal? '''(1 "x") '''(1 "x"))`, `true`, ``},
			{`(equal? ''a ''b)`, `false`, ``},
			// The quote depth is part of the value.
			{`(equal? ''a '''a)`, `false`, ``},
			// The documented distinction between the two spellings stands.
			{`(equal? ''a '(quote a))`, `false`, ``},
			{`(equal? (list ''a) (list ''a))`, `true`, ``},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
