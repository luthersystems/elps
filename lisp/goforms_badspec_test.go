// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// badTypespecGolden holds what concat, zip, slice and map return for the
// type specifiers no SeqKind stands for.  It was written by this test run on
// v1.82.0, before the builtins parsed their type specifier into a SeqKind.
// Rewrite it with UPDATE_GOLDEN=1 only for an intended change of result.
var badTypespecGolden = filepath.Join("testdata", "badtypespec_golden.txt")

// badTypespecs are type specifiers that no SeqKind stands for: an unknown
// symbol and values that are not symbols.  Only the builtins take them.
func badTypespecs() []*lisp.LVal {
	return []*lisp.LVal{lisp.Symbol("bogus"), lisp.Int(1), lisp.String("list")}
}

// renderResult is one line of the golden file: an error's condition and
// message, or a value's type and printed form.
func renderResult(v *lisp.LVal) string {
	if v.IsError() {
		ev := (*lisp.ErrorVal)(v)
		return "error " + ev.Condition() + ": " + ev.ErrorMessage()
	}
	return v.Type.String() + " " + v.String()
}

// TestSequenceBuiltinsRefuseBadTypespecs sweeps the type specifiers no
// SeqKind stands for across every oracle value, every slice bound and every
// map fn, on the builtins' side.  Each result must match the golden file, so
// each builtin keeps its checks, their order and their messages.
func TestSequenceBuiltinsRefuseBadTypespecs(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", mapSetup)
	require.False(t, rc.IsError(), "%v", rc)
	var b strings.Builder
	line := func(label string, v *lisp.LVal) {
		fmt.Fprintf(&b, "%s\t%s\n", label, renderResult(v))
	}
	for si, ts := range badTypespecs() {
		spec := func() *lisp.LVal { return badTypespecs()[si] }
		tsLabel := ts.Type.String() + ":" + ts.String()
		line("concat/"+tsLabel+"/none", env.CallBuiltin(refConcat, spec()))
		for i, a := range oracle.Values() {
			label := tsLabel + "/" + a.Label
			line("concat/"+label, env.CallBuiltin(refConcat, spec(), value(i)))
			line("zip/"+label, env.CallBuiltin(refZip, spec(), value(i)))
			for j, bv := range oracle.Values() {
				pairLabel := label + "/" + bv.Label
				line("concat/"+pairLabel, env.CallBuiltin(refConcat, spec(), value(i), value(j)))
				line("zip/"+pairLabel, env.CallBuiltin(refZip, spec(), value(i), value(j), value(i)))
			}
			for _, start := range []*lisp.LVal{lisp.Int(-1), lisp.Int(0), lisp.Int(1), lisp.Int(5), lisp.String("0")} {
				for _, end := range []*lisp.LVal{lisp.Int(0), lisp.Int(1), lisp.Int(2), lisp.Int(9), lisp.Float(1)} {
					line(fmt.Sprintf("slice/%s/%v/%v", label, start, end),
						env.CallBuiltin(refSlice, spec(), value(i), start, end))
				}
			}
			for fi, fn := range mapFns(env) {
				line("map/"+label+"/"+fn.Label, inEval(t, env, func(env *lisp.LEnv) *lisp.LVal {
					return env.CallBuiltin(refMap, spec(), mapFns(env)[fi].V, value(i))
				}))
			}
		}
	}
	got := b.String()
	if os.Getenv("UPDATE_GOLDEN") == "1" {
		require.NoError(t, os.WriteFile(badTypespecGolden, []byte(got), 0o600))
		return
	}
	want, err := os.ReadFile(badTypespecGolden)
	require.NoError(t, err)
	assert.Equal(t, string(want), got)
}
