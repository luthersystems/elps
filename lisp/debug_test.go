// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
)

// TestDebugPrint pins that debug-print writes to Runtime.Stderr in BOTH of its
// branches.
//
// The zero-argument form used to take a separate path that called
// fmt.Println -- writing its newline to os.Stdout and ignoring Runtime.Stderr
// entirely, contradicting the builtin's own documentation and its non-empty
// branch.  For a host whose stdout is a protocol stream (the LSP server, a
// JSON-RPC chaincode process) a stray newline there is a corrupted stream, not
// cosmetic noise, and an embedder who had redirected debug output had no way
// to stop it.
//
// The third field of each row is the debug output RunTestSuite captures from
// Runtime.Stderr, which is the only place this is observable: reverting the
// fix leaves the whole rest of `go test ./...` green, because nothing else
// asserts on where a zero-argument debug-print writes.
func TestDebugPrint(t *testing.T) {
	tests := elpstest.TestSuite{
		{"debug-print", elpstest.TestSequence{
			{`(debug-print)`, `()`, "\n"},
			{`(debug-print 1)`, `()`, "1\n"},
			{`(debug-print 1 2 3)`, `()`, "1 2 3\n"},
			{`(debug-print "a")`, `()`, "\"a\"\n"},
			{`(debug-print ())`, `()`, "()\n"},
		}},
	}
	elpstest.RunTestSuite(t, tests)
}
