// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

// #625: generated diagnostics carry no sealed message child whose location
// could accidentally stand in for the error's own origin.
const errorProvenanceProgram = `(error "same")`

func requireGeneratedProvenanceError(t *testing.T, value *lisp.LVal, file string) {
	t.Helper()
	require.Equal(t, lisp.LError, value.Type, value)
	require.False(t, value.IsSealed())
	require.Equal(t, "condition type is not a symbol: string", (*lisp.ErrorVal)(value).ErrorMessage())
	require.Len(t, value.Cells, 1)
	require.False(t, value.Cells[0].IsSealed(), "a sealed message would hide the missing error-origin lens")
	loc, ok := value.Source()
	require.True(t, ok)
	require.Equal(t, file, loc.File)
	require.Contains(t, value.String(), file+":")
}

func TestValueFingerprintGeneratedErrorProvenance(t *testing.T) {
	load := func(file string) *lisp.LVal {
		value := copyTestEnv(t).LoadString(file, errorProvenanceProgram)
		requireGeneratedProvenanceError(t, value, file)
		return value
	}
	one, again, other := load("first.lisp"), load("first.lisp"), load("second.lisp")
	require.NotEqual(t, one.String(), other.String(), "the diagnostic difference must be real")
	require.Equal(t, valueFingerprint([]*lisp.LVal{one}), valueFingerprint([]*lisp.LVal{other}),
		"the content-only lens intentionally compares programs loaded under different names")
	require.Equal(t, valueFingerprintProv([]*lisp.LVal{one}), valueFingerprintProv([]*lisp.LVal{again}),
		"independent equal errors must not differ by Go identity")
	require.NotEqual(t, valueFingerprintProv([]*lisp.LVal{one}), valueFingerprintProv([]*lisp.LVal{other}),
		"generated errors from different files must have different provenance")
}

func provenanceFrameError() *lisp.LVal {
	value := lisp.Errorf("message _fun17")
	value.SetSource(&token.Location{File: "origin.lisp", Path: "/source/origin.lisp", Pos: 2, Line: 1, Col: 3})
	value.SetCallStack(&lisp.CallStack{Frames: []lisp.CallFrame{{
		Source: &token.Location{File: "caller.lisp", Path: "/source/caller.lisp", Pos: 4, Line: 2, Col: 3},
		FID:    "_fun17", Package: "user", Name: "named", HeightLogical: 4,
		Terminal: true, TROBlock: true, TailIterations: 7,
	}}})
	return value
}

func TestValueFingerprintErrorFrameProvenance(t *testing.T) {
	for _, tc := range []struct {
		name   string
		change func(*lisp.LVal)
	}{
		{"error-source", func(v *lisp.LVal) { v.SetSource(&token.Location{File: "different.lisp"}) }},
		{"error-source-absent", func(v *lisp.LVal) { v.SetSource(nil) }},
		{"stack-absent", func(v *lisp.LVal) { v.Native = nil }},
		{"frame-count", func(v *lisp.LVal) { v.CallStack().Frames = append(v.CallStack().Frames, lisp.CallFrame{}) }},
		{"frame-source", func(v *lisp.LVal) { v.CallStack().Frames[0].Source.File = "different.lisp" }},
		{"frame-source-path", func(v *lisp.LVal) { v.CallStack().Frames[0].Source.Path = "/other/caller.lisp" }},
		{"frame-source-span", func(v *lisp.LVal) { v.CallStack().Frames[0].Source.EndCol = 9 }},
		{"frame-source-absent", func(v *lisp.LVal) { v.CallStack().Frames[0].Source = nil }},
		{"frame-package", func(v *lisp.LVal) { v.CallStack().Frames[0].Package = "other" }},
		{"frame-name", func(v *lisp.LVal) { v.CallStack().Frames[0].Name = "other" }},
		{"frame-stable-id", func(v *lisp.LVal) { v.CallStack().Frames[0].FID = "explicit-host-id" }},
		{"frame-logical-height", func(v *lisp.LVal) { v.CallStack().Frames[0].HeightLogical++ }},
		{"frame-terminal", func(v *lisp.LVal) { v.CallStack().Frames[0].Terminal = false }},
		{"frame-tro-block", func(v *lisp.LVal) { v.CallStack().Frames[0].TROBlock = false }},
		{"frame-tail-iterations", func(v *lisp.LVal) { v.CallStack().Frames[0].TailIterations++ }},
	} {
		t.Run(tc.name, func(t *testing.T) {
			one, other := provenanceFrameError(), provenanceFrameError()
			tc.change(other)
			require.Equal(t, valueFingerprint([]*lisp.LVal{one}), valueFingerprint([]*lisp.LVal{other}),
				"provenance must not alter the content-only lens")
			require.NotEqual(t, valueFingerprintProv([]*lisp.LVal{one}), valueFingerprintProv([]*lisp.LVal{other}))
		})
	}
}

func TestValueFingerprintErrorNormalizesOnlyGeneratedFrameIDs(t *testing.T) {
	for _, ids := range [][2]string{{"_fun17", "_fun42"}, {"_validation_fun_17", "_validation_fun_42"}} {
		one, other := provenanceFrameError(), provenanceFrameError()
		one.CallStack().Frames[0].FID = ids[0]
		other.CallStack().Frames[0].FID = ids[1]
		require.Equal(t, valueFingerprintProv([]*lisp.LVal{one}), valueFingerprintProv([]*lisp.LVal{other}),
			"allocation-derived frame IDs are not comparable across independent loads")
	}
	for _, tc := range []struct {
		name   string
		change func(*lisp.LVal, string)
	}{
		{"message", func(v *lisp.LVal, s string) { v.Cells[0] = lisp.String(s) }},
		{"condition", func(v *lisp.LVal, s string) { v.Str = s }},
		{"filename", func(v *lisp.LVal, s string) { v.SetSource(&token.Location{File: s}) }},
		{"frame-name", func(v *lisp.LVal, s string) { v.CallStack().Frames[0].Name = s }},
		{"frame-package", func(v *lisp.LVal, s string) { v.CallStack().Frames[0].Package = s }},
		{"non-generated-frame-id", func(v *lisp.LVal, s string) { v.CallStack().Frames[0].FID = "host" + s }},
	} {
		t.Run(tc.name, func(t *testing.T) {
			one, other := provenanceFrameError(), provenanceFrameError()
			tc.change(one, "_fun17")
			tc.change(other, "_fun42")
			require.NotEqual(t, valueFingerprintProv([]*lisp.LVal{one}), valueFingerprintProv([]*lisp.LVal{other}),
				"ID-shaped user data must not be normalized")
		})
	}
}

func TestValueFingerprintColdErrorAllocationIDs(t *testing.T) {
	oneEnv, otherEnv := copyTestEnv(t), copyTestEnv(t)
	// Allocate an otherwise unreachable function only in the second VM. The
	// same subsequent program then receives different generated frame IDs.
	warmup := otherEnv.LoadString("warmup.lisp", `(lambda () 1)`)
	require.Equal(t, lisp.LFun, warmup.Type)
	const source = `((lambda () (error "same")))`
	one := oneEnv.LoadString("same.lisp", source)
	other := otherEnv.LoadString("same.lisp", source)
	for _, value := range []*lisp.LVal{one, other} {
		requireGeneratedProvenanceError(t, value, "same.lisp")
		require.NotNil(t, value.CallStack())
	}
	ids := func(value *lisp.LVal) []string {
		var out []string
		for _, frame := range value.CallStack().Frames {
			out = append(out, frame.FID)
		}
		return out
	}
	require.NotEqual(t, ids(one), ids(other), "the control must actually shift generated frame IDs")
	require.Equal(t, valueFingerprintProv([]*lisp.LVal{one}), valueFingerprintProv([]*lisp.LVal{other}),
		"independent cold allocation counters must not invent a provenance difference")
}

func TestLoadCacheGeneratedErrorProvenance(t *testing.T) {
	cache := newTestLoadCache()
	cached, cachedReader := newCacheEnv(t, nil, cache)
	cold, coldReader := newCacheEnv(t, nil, nil)
	names := []string{"first.lisp", "second.lisp", "first.lisp"}
	results := make([]*lisp.LVal, len(names))
	for i, name := range names {
		results[i] = cached.LoadLocation(name, name, strings.NewReader(errorProvenanceProgram))
		want := cold.LoadLocation(name, name, strings.NewReader(errorProvenanceProgram))
		requireGeneratedProvenanceError(t, results[i], name)
		requireGeneratedProvenanceError(t, want, name)
		require.Equal(t, want.String(), results[i].String())
		require.Equal(t, valueFingerprintProv([]*lisp.LVal{want}), valueFingerprintProv([]*lisp.LVal{results[i]}))
	}
	require.Equal(t, 2, cachedReader.reads, "A and B each parse once; the final A is a real hit")
	require.Equal(t, 3, coldReader.reads, "the reference must parse all three loads")
	require.Equal(t, 2, cache.stores)
	require.Equal(t, 1, cache.hits)
	require.NotEqual(t, valueFingerprintProv(results[:1]), valueFingerprintProv(results[1:2]),
		"the real A/B error-location difference must survive the oracle")
	require.Equal(t, valueFingerprintProv(results[:1]), valueFingerprintProv(results[2:]),
		"a cache hit must reproduce the original file's diagnostic")
	for i, name := range names {
		requireGeneratedProvenanceError(t, results[i], name)
	}
}
