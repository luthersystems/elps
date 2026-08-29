// Copyright © 2026 The ELPS authors

// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libelpspath_test

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath"
)

// TestPackage runs elpspath lisp tests.
func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libelpspath_test.lisp")
}

// TestPackageCyclicValue runs the tests for values that contain themselves
// (issue #393). They live in their own file so that source added for them is
// not charged to a benchmark that loads libelpspath_test.lisp.
func TestPackageCyclicValue(t *testing.T) {
	r := &elpstest.Runner{}
	defer r.Close()
	r.RunTestFile(t, "libelpspath_cycle_test.lisp")
}

// TestOKSimpleTypeIsReachableFromAnEmbedder is a compile-time assertion as
// much as a behavioural one: this file is package libelpspath_test, so it
// can only reach the gate through the package's exported surface, which is
// the entire point of exporting it (issue #564).
//
// Before the export, a Go embedder that wanted the gate had to call
// BuiltinQueryGet with no path steps -- ArgsToPath of an empty step list is
// the identity, so the call's only observable effect is the verdict. That
// works, and substrate shipped it with a paragraph of comment explaining
// why, but it needs an *LEnv the embedder may not have and it reads as a
// path query rather than as a type check.
//
// The behavioural half covers the two verdicts that matter to a caller: an
// unsupported type, and -- the reason the gate is not optional -- a value
// that contains itself, which every unguarded recursive walk answers by
// growing the goroutine stack until the runtime kills the process, an abort
// recover() cannot intercept (issue #393).
func TestOKSimpleTypeIsReachableFromAnEmbedder(t *testing.T) {
	t.Parallel()

	ok := lisp.SortedMap()
	ok.MapSet("a", lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.String("two")}))
	require.NoError(t, libelpspath.OKSimpleType(ok), "an ordinary nested document must be accepted")

	require.NoError(t, libelpspath.OKSimpleType(lisp.Bool(true)),
		"true and false are the two symbols the gate admits")
	require.NoError(t, libelpspath.OKSimpleType(lisp.Symbol(lisp.FalseSymbol)),
		"and false, spelled as the bare symbol")
	require.Error(t, libelpspath.OKSimpleType(lisp.Symbol("not-a-boolean")),
		"an arbitrary symbol is not a value the engine can hold")
	require.Error(t, libelpspath.OKSimpleType(lisp.Native(struct{ X int }{1})),
		"a native Go value is not a value the engine can hold")

	// The cycle. Built here rather than borrowed from the in-package
	// helpers so that this test stands on the exported API alone.
	cyclic := lisp.Vector([]*lisp.LVal{lisp.Int(1)})
	cyclic.Cells[1].Cells = append(cyclic.Cells[1].Cells, cyclic)
	require.Error(t, libelpspath.OKSimpleType(cyclic),
		"a value that contains itself must be REFUSED, not walked (issue #393)")
}
