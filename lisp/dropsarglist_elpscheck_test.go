// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

// A host builtin marked with DropsArgList that keeps its list breaks the
// mark's promise.  elpsvet cannot read a host's code, so the elpscheck
// build is what shows it: the list the builtin kept reads as invalid values
// once its call returns.
func TestDropsArgListKeeperReadsPoison(t *testing.T) {
	var kept *lisp.LVal
	env := newCallSemanticsEnv(t)
	env.AddBuiltins(false, lisp.DropsArgList(elpsutil.Function("keeper", lisp.Formals("a", "b"),
		func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			kept = args
			return args.Cells[0]
		})))
	require.Equal(t, "1", env.LoadString("t.lisp", "(keeper 1 2)").String())
	require.NotNil(t, kept)
	require.Equal(t, lisp.LInvalid, kept.Type, "the kept list was not poisoned")
}
