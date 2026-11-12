// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// offPathFaults are the three shapes the whole-document gate refuses. Issue
// #960 scopes reads to their path, so each is planted where a read of "a" "b"
// never goes, and stepped into where a read must still refuse it.
func offPathFaults() map[string]func() *lisp.LVal {
	return map[string]func() *lisp.LVal{
		"cycle": func() *lisp.LVal { return cyclicMap("self") },
		"multi-dim-array": func() *lisp.LVal {
			return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)}),
				[]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5), lisp.Int(6)})
		},
		// The reader cannot build this; only host code can.
		"malformed-quote": func() *lisp.LVal { return &lisp.LVal{Type: lisp.LQuote} },
	}
}

// docWithFault returns {"a": {"b": 1}, "bad": fault}.
func docWithFault(fault *lisp.LVal) *lisp.LVal {
	doc := mapHolding("a", mapHolding("b", lisp.Int(1)))
	doc.MapSetString("bad", fault)
	return doc
}

// TestQueryGetIgnoresOffPathFaults pins issue #960: a read visits only the
// containers on its path, so a fault elsewhere in the document cannot change
// its answer. Before the fix every read validated the whole document first
// and refused all of these.
func TestQueryGetIgnoresOffPathFaults(t *testing.T) {
	for name, fault := range offPathFaults() {
		t.Run(name, func(t *testing.T) {
			env := testEnv(t)
			got := callBuiltin(env, BuiltinQueryGet, docWithFault(fault()), lisp.String("a"), lisp.String("b"))
			require.NotEqual(t, lisp.LError, got.Type, "%v", got)
			assert.Equal(t, "1", got.String())
		})
	}

	t.Run("iterator", func(t *testing.T) {
		env := testEnv(t)
		xs := lisp.Vector([]*lisp.LVal{mapHolding("v", lisp.Int(1)), mapHolding("v", lisp.Int(2))})
		doc := mapHolding("xs", xs)
		doc.MapSetString("bad", cyclicMap("self"))
		got := callBuiltin(env, BuiltinQueryGet, doc, lisp.String("xs"), lisp.Symbol("*"), lisp.String("v"))
		require.NotEqual(t, lisp.LError, got.Type, "%v", got)
		assert.Equal(t, "(vector 1 2)", got.String())
	})
}

// TestQueryGetOnPathFaultsStillError pins the other half of #960: dropping
// the whole-document gate must not let a read step INTO a bad shape. The
// accessors on the path refuse it themselves, with an error and no panic.
func TestQueryGetOnPathFaultsStillError(t *testing.T) {
	faults := offPathFaults()

	t.Run("index-multi-dim-array", func(t *testing.T) {
		env := testEnv(t)
		doc := docWithFault(faults["multi-dim-array"]())
		got := callBuiltin(env, BuiltinQueryGet, doc, lisp.String("bad"), lisp.Int(0))
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		assert.Contains(t, got.String(), "cannot index multi-dimensional array")
		assert.NotContains(t, got.String(), "panic")
	})

	t.Run("step-through-malformed-quote", func(t *testing.T) {
		env := testEnv(t)
		doc := docWithFault(faults["malformed-quote"]())
		got := callBuiltin(env, BuiltinQueryGet, doc, lisp.String("bad"), lisp.String("x"))
		require.Equal(t, lisp.LError, got.Type, "%v", got)
		// Before #960 the whole-document gate answered first, with "invalid
		// quote wrapper: expected one value". The read now refuses the step
		// itself, as it refuses any wrapper, well-formed or not.
		assert.Contains(t, got.String(), "cannot index into quote at path bad")
		assert.NotContains(t, got.String(), "panic")
	})

	t.Run("step-around-cycle", func(t *testing.T) {
		// A cycle is walkable one finite step at a time; what a read must not
		// do is recurse through it. Two steps around the loop land back on it.
		env := testEnv(t)
		cycle := faults["cycle"]()
		doc := docWithFault(cycle)
		got := callBuiltin(env, BuiltinQueryGet, doc, lisp.String("bad"), lisp.String("self"), lisp.String("self"))
		require.NotEqual(t, lisp.LError, got.Type, "%v", got)
		assert.Same(t, cycle, got)
	})

	t.Run("stop-at-multi-dim-array", func(t *testing.T) {
		// Intended new behaviour under #960: a read that STOPS at a
		// multi-dimensional array returns it, as it would any other value it
		// does not step into. Only indexing it is refused.
		env := testEnv(t)
		arr := faults["multi-dim-array"]()
		got := callBuiltin(env, BuiltinQueryGet, docWithFault(arr), lisp.String("bad"))
		require.NotEqual(t, lisp.LError, got.Type, "%v", got)
		assert.Same(t, arr, got)
	})
}

// TestWriteBuiltinsStillValidateWholeDocument pins the scope of #960: only
// reads drop the gate. A write can copy or rebuild any part of the document,
// so an off-path fault still refuses it.
func TestWriteBuiltinsStillValidateWholeDocument(t *testing.T) {
	for name, fn := range map[string]lisp.LBuiltin{"?set!": BuiltinQuerySetMutate, "?set": BuiltinQuerySet} {
		t.Run(name, func(t *testing.T) {
			env := testEnv(t)
			doc := docWithFault(cyclicMap("self"))
			got := callBuiltin(env, fn, doc, lisp.String("a"), lisp.String("b"), lisp.Int(2))
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			assert.Contains(t, got.String(), errCyclicValue.Error())
		})
	}
}

// TestValidateDocumentMatchesGate pins ValidateDocument to the gate the write
// builtins run, so an embedder gating its own writes refuses the same
// documents with the same text.
func TestValidateDocumentMatchesGate(t *testing.T) {
	require.NoError(t, ValidateDocument(docWithFault(lisp.Int(0))))
	for name, fault := range offPathFaults() {
		t.Run(name, func(t *testing.T) {
			doc := docWithFault(fault())
			want := okSimpleType(doc)
			require.Error(t, want)
			got := ValidateDocument(doc)
			require.Error(t, got)
			assert.Equal(t, want.Error(), got.Error())
		})
	}
}

// TestQueryGetIteratorYieldsNilForMultiDimElement pins what #960 changes for
// an iterator: a 2-D array element is now an element the rest of the path
// cannot index, so it yields nil like any other such element. Before, the
// whole-document gate failed the entire read.
func TestQueryGetIteratorYieldsNilForMultiDimElement(t *testing.T) {
	env := testEnv(t)
	xs := lisp.Vector([]*lisp.LVal{
		lisp.Vector([]*lisp.LVal{lisp.Int(1)}),
		offPathFaults()["multi-dim-array"](),
		lisp.Vector([]*lisp.LVal{lisp.Int(3)}),
	})
	got := callBuiltin(env, BuiltinQueryGet, mapHolding("xs", xs), lisp.String("xs"), lisp.Symbol("*"), lisp.Int(0))
	require.NotEqual(t, lisp.LError, got.Type, "%v", got)
	assert.Equal(t, "(vector 1 () 3)", got.String())
}

// TestQueryGetRefusesGoNilCell covers a host-built document holding a Go nil
// cell. The reader cannot make one, and the whole-document gate used to
// stop it before a read began; with that gate gone (#960) the read itself
// must refuse it rather than hand back a Go nil *LVal or panic on it.
func TestQueryGetRefusesGoNilCell(t *testing.T) {
	doc := func() *lisp.LVal { return mapHolding("xs", lisp.Vector([]*lisp.LVal{lisp.Int(1), nil})) }
	for name, steps := range map[string][]*lisp.LVal{
		"index":      {lisp.String("xs"), lisp.Int(1)},
		"index-then": {lisp.String("xs"), lisp.Int(1), lisp.String("k")},
	} {
		t.Run(name, func(t *testing.T) {
			got := callBuiltin(testEnv(t), BuiltinQueryGet, append([]*lisp.LVal{doc()}, steps...)...)
			require.NotNil(t, got)
			require.Equal(t, lisp.LError, got.Type, "%v", got)
			assert.NotContains(t, got.String(), "panic")
			assert.Contains(t, got.String(), errNilCell.Error())
		})
	}
	t.Run("iterator", func(t *testing.T) {
		// Under an iterator an element's failure is that element's nil.
		got := callBuiltin(testEnv(t), BuiltinQueryGet, doc(), lisp.String("xs"), lisp.Symbol("*"))
		require.NotEqual(t, lisp.LError, got.Type, "%v", got)
		for i, c := range got.Cells {
			require.NotNil(t, c, "cell %d is a Go nil", i)
		}
		assert.Equal(t, "(vector 1 ())", got.String())
	})
}

// TestCopyRefusesMultiDimArray is defense in depth for #960. The copy used
// to replace a multi-dimensional array with nil, silently dropping data. The
// gated builtins never reach that, but an embedder that gated writes through
// (elpspath:? v) lost its gate when reads became path-scoped, so the copy
// must refuse the shape itself. It goes through the exported Path API, which
// runs no gate.
func TestCopyRefusesMultiDimArray(t *testing.T) {
	arr := offPathFaults()["multi-dim-array"]()
	doc := mapHolding("a", lisp.Int(1))
	doc.MapSetString("bad", arr)
	before := doc.String()
	path, err := ArgsToPath([]*lisp.LVal{lisp.String("a")})
	require.NoError(t, err)
	for name, run := range map[string]func() (*lisp.LVal, error){
		"set":    func() (*lisp.LVal, error) { return path.Set(doc, lisp.Int(2)) },
		"delete": func() (*lisp.LVal, error) { return path.Delete(doc) },
		"nil":    func() (*lisp.LVal, error) { return path.Nil(doc) },
	} {
		t.Run(name, func(t *testing.T) {
			out, err := run()
			require.Error(t, err, "copy returned %v", out)
			assert.Contains(t, err.Error(), "cannot index multi-dimensional array")
			assert.Equal(t, before, doc.String(), "the input must be unchanged")
			got, ok := doc.Map().Get(lisp.String("bad"))
			require.True(t, ok)
			assert.Same(t, arr, got)
		})
	}
}
