// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"context"
	"fmt"
	"testing"

	"github.com/luthersystems/elps/elpstest/oracle"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The builtins the LEnv Go forms of luthersystems/elps#859 return what they
// return, called as the oracle.
var (
	refToInt      = lisp.BuiltinFunc("to-int")
	refToBytes    = lisp.BuiltinFunc("to-bytes")
	refCopy       = lisp.BuiltinFunc("copy")
	refNth        = lisp.BuiltinFunc("nth")
	refAssoc      = lisp.BuiltinFunc("assoc")
	refKeyP       = lisp.BuiltinFunc("key?")
	refStringEq   = lisp.BuiltinFunc("string=")
	refNumEq      = lisp.BuiltinFunc("=")
	refNumLT      = lisp.BuiltinFunc("<")
	refNumLEq     = lisp.BuiltinFunc("<=")
	refNumGT      = lisp.BuiltinFunc(">")
	refNumGEq     = lisp.BuiltinFunc(">=")
	refSub        = lisp.BuiltinFunc("-")
	refConcat     = lisp.BuiltinFunc("concat")
	refZip        = lisp.BuiltinFunc("zip")
	refSlice      = lisp.BuiltinFunc("slice")
	refMap        = lisp.BuiltinFunc("map")
	refAppendMut  = lisp.BuiltinFunc("append!")
	refDebugStack = lisp.BuiltinFunc("debug-stack")
	refList       = lisp.BuiltinFunc("list")
	refVector     = lisp.BuiltinFunc("vector")
)

// sameResult fails t unless got is what the builtin returned (want): both
// errors with the same condition and message, or two values of the same
// type that are equal.  A float compares bit for bit, as oracle.Same
// compares it.  Each side builds its own inputs, and Equal compares
// functions, natives and stored errors by identity, so two other values are
// also equal when they print the same.
func sameResult(t *testing.T, label string, got, want *lisp.LVal) {
	t.Helper()
	if want.IsError() || got.IsError() || got.Type == lisp.LFloat {
		oracle.Same(t, label, got, want)
		if got.IsError() && want.IsError() {
			gm := (*lisp.ErrorVal)(got).ErrorMessage()
			wm := (*lisp.ErrorVal)(want).ErrorMessage()
			if gm != wm {
				t.Errorf("%s: builtin's message %q, Go's message %q", label, wm, gm)
			}
		}
		return
	}
	if got.Type != want.Type {
		t.Errorf("%s: builtin returned %s, Go returned %s", label, want.Type, got.Type)
		return
	}
	if eq := got.Equal(want); !eq.IsError() && lisp.True(eq) {
		return
	}
	if got.String() != want.String() {
		t.Errorf("%s: builtin returned %v, Go returned %v", label, want, got)
	}
}

// goValue turns a (value, error) Go form result into the value its builtin
// returns: the error as the *lisp.ErrorVal it is, or the Go value as an LVal.
// It panics on an error of another type, which no Go form returns.
func goValue[T string | int | bool | []byte](v T, err error) *lisp.LVal {
	if err != nil {
		e, ok := err.(*lisp.ErrorVal) //nolint:errorlint // a Go form returns the builtin's *ErrorVal unwrapped
		if !ok {
			panic(fmt.Sprintf("a Go form returned an error of type %T", err))
		}
		return (*lisp.LVal)(e)
	}
	switch x := any(v).(type) {
	case string:
		return lisp.String(x)
	case int:
		return lisp.Int(x)
	case bool:
		return lisp.Bool(x)
	case []byte:
		return lisp.Bytes(x)
	}
	panic("unreachable")
}

// boolValue turns a (bool, error) Go form result into the builtin's value.
func boolValue(ok bool, err error) *lisp.LVal { return goValue(ok, err) }

// value returns the i-th input of a fresh oracle.Values.
func value(i int) *lisp.LVal { return oracle.Values()[i].V }

// numValues is the count of oracle.Values.
var numValues = len(oracle.Values())

// unaryForms pairs each one-argument Go form with its builtin.
var unaryForms = []struct {
	name    string
	builtin lisp.BuiltinRef
	fn      func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
}{
	{"to-int", refToInt, func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal { return goValue(env.ToInt(v)) }},
	{"to-bytes", refToBytes, func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal { return goValue(env.ToBytes(v)) }},
	{"copy", refCopy, func(env *lisp.LEnv, v *lisp.LVal) *lisp.LVal { return lisp.ValueOrError(env.DeepCopy(v)) }},
}

// binaryForms pairs each two-argument Go form with its builtin.
var binaryForms = []struct {
	name    string
	builtin lisp.BuiltinRef
	fn      func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal
}{
	{"nth", refNth, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return lisp.ValueOrError(env.Nth(a, b)) }},
	{"key?", refKeyP, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.MapHasKey(a, b)) }},
	{"string=", refStringEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.StringEqual(a, b)) }},
	{"=", refNumEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumEqual(a, b)) }},
	{"<", refNumLT, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumLess(a, b)) }},
	{"<=", refNumLEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumLessEqual(a, b)) }},
	{">", refNumGT, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumGreater(a, b)) }},
	{">=", refNumGEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumGreaterEqual(a, b)) }},
	{"+", refAdd, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return lisp.ValueOrError(env.Add(a, b)) }},
	{"-", refSub, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return lisp.ValueOrError(env.Sub(a, b)) }},
}

// seqKinds are the kinds the sequence forms are tried with: every SeqKind
// constant, the zero kind and a kind past the constants.
var seqKinds = []lisp.SeqKind{
	lisp.SeqList, lisp.SeqVector, lisp.SeqString, lisp.SeqBytes, lisp.SeqNil,
	0, 200,
}

// kindSpec is the type specifier the builtins are given for kind: () for
// SeqNil, and otherwise the symbol kind prints as.  For a kind outside the
// constants the symbol is unknown, so the builtin refuses it as an invalid
// type specifier.
func kindSpec(kind lisp.SeqKind) *lisp.LVal {
	if kind == lisp.SeqNil {
		return lisp.Nil()
	}
	return lisp.Symbol(kind.String())
}

// TestGoFormsMatchBuiltins holds each Go form to its builtin on inputs of
// every type: the same value, or the same error condition and message.
func TestGoFormsMatchBuiltins(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for _, f := range unaryForms {
		for i, in := range oracle.Values() {
			sameResult(t, f.name+"/"+in.Label, f.fn(env, in.V), lisp.ValueOrError(env.CallBuiltin(f.builtin, value(i))))
		}
	}
	for _, f := range binaryForms {
		for i, a := range oracle.Values() {
			for j, b := range oracle.Values() {
				label := f.name + "/" + a.Label + "/" + b.Label
				got := f.fn(env, a.V, b.V)
				want := lisp.ValueOrError(env.CallBuiltin(f.builtin, value(i), value(j)))
				if f.name == "nth" {
					// nth returns the element itself, as the builtin does.
					oracle.SameObject(t, label, f.fn(env, a.V, b.V), lisp.ValueOrError(env.CallBuiltin(f.builtin, a.V, b.V)))
				}
				sameResult(t, label, got, want)
			}
		}
	}
}

// TestMapAssocMatchesAssoc holds MapAssoc to assoc for every map and key
// type, and checks that neither changes the map it is given: after the call,
// each input still equals a fresh copy of it.
func TestMapAssocMatchesAssoc(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for i, m := range oracle.Values() {
		for j, k := range oracle.Values() {
			label := "assoc/" + m.Label + "/" + k.Label
			gm, wm := value(i), value(i)
			got := lisp.ValueOrError(env.MapAssoc(gm, value(j), lisp.Int(42)))
			want := lisp.ValueOrError(env.CallBuiltin(refAssoc, wm, value(j), lisp.Int(42)))
			sameResult(t, label, got, want)
			sameResult(t, label+"/go-input", gm, value(i))
			sameResult(t, label+"/builtin-input", wm, value(i))
		}
	}
}

// TestArithmeticFormsVariadic holds Add and Sub to + and - for zero to three
// arguments, so a float anywhere in the list sums in float from the start.
func TestArithmeticFormsVariadic(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	args := [][]*lisp.LVal{
		nil,
		{lisp.Int(3)},
		{lisp.Float(2.5)},
		{lisp.String("x")},
		{lisp.Int(1), lisp.Int(2), lisp.Int(3)},
		{lisp.Int(1), lisp.Float(2.5), lisp.Int(3)},
		{lisp.Int(1<<62 + (1<<62 - 1)), lisp.Int(1), lisp.Float(0.5)},
		{lisp.Int(1), lisp.Int(2), lisp.Symbol("a")},
		{lisp.Symbol("a"), lisp.Int(1), lisp.String("b")},
	}
	for _, xs := range args {
		label := fmt.Sprint(xs)
		sameResult(t, "+"+label, lisp.ValueOrError(env.Add(xs...)), lisp.ValueOrError(env.CallBuiltin(refAdd, xs...)))
		sameResult(t, "-"+label, lisp.ValueOrError(env.Sub(xs...)), lisp.ValueOrError(env.CallBuiltin(refSub, xs...)))
	}
}

// TestSequenceFormsMatchBuiltins holds Concat, Zip and Slice to concat, zip
// and slice for every type specifier and for inputs of every type.
func TestSequenceFormsMatchBuiltins(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for _, kind := range seqKinds {
		tsLabel := kind.String()
		spec := func() *lisp.LVal { return kindSpec(kind) }
		sameResult(t, "concat/"+tsLabel+"/none", lisp.ValueOrError(env.Concat(kind)), lisp.ValueOrError(env.CallBuiltin(refConcat, spec())))
		for i, a := range oracle.Values() {
			label := tsLabel + "/" + a.Label
			sameResult(t, "concat/"+label, lisp.ValueOrError(env.Concat(kind, value(i))), lisp.ValueOrError(env.CallBuiltin(refConcat, spec(), value(i))))
			sameResult(t, "zip/"+label, lisp.ValueOrError(env.Zip(kind, value(i))), lisp.ValueOrError(env.CallBuiltin(refZip, spec(), value(i))))
			for j, b := range oracle.Values() {
				pairLabel := label + "/" + b.Label
				sameResult(t, "concat/"+pairLabel,
					lisp.ValueOrError(env.Concat(kind, value(i), value(j))),
					lisp.ValueOrError(env.CallBuiltin(refConcat, spec(), value(i), value(j))))
				sameResult(t, "zip/"+pairLabel,
					lisp.ValueOrError(env.Zip(kind, value(i), value(j), value(i))),
					lisp.ValueOrError(env.CallBuiltin(refZip, spec(), value(i), value(j), value(i))))
			}
			for _, start := range []*lisp.LVal{lisp.Int(-1), lisp.Int(0), lisp.Int(1), lisp.Int(5), lisp.String("0")} {
				for _, end := range []*lisp.LVal{lisp.Int(0), lisp.Int(1), lisp.Int(2), lisp.Int(9), lisp.Float(1)} {
					sliceLabel := fmt.Sprintf("slice/%s/%v/%v", label, start, end)
					sameResult(t, sliceLabel,
						lisp.ValueOrError(env.Slice(kind, value(i), start, end)),
						lisp.ValueOrError(env.CallBuiltin(refSlice, spec(), value(i), start, end)))
				}
			}
		}
	}
}

// TestAppendInPlaceMatchesAppend holds AppendInPlace to append! for every
// target and value type, with zero, one and two values, on the result and on
// the target it changes.
func TestAppendInPlaceMatchesAppend(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for i, s := range oracle.Values() {
		gs, ws := value(i), value(i)
		sameResult(t, "append!/"+s.Label+"/none", lisp.ValueOrError(env.AppendInPlace(gs)), lisp.ValueOrError(env.CallBuiltin(refAppendMut, ws)))
		sameResult(t, "append!/"+s.Label+"/none/target", gs, ws)
		for j, x := range oracle.Values() {
			label := "append!/" + s.Label + "/" + x.Label
			gs, ws := value(i), value(i)
			sameResult(t, label, lisp.ValueOrError(env.AppendInPlace(gs, value(j))), lisp.ValueOrError(env.CallBuiltin(refAppendMut, ws, value(j))))
			sameResult(t, label+"/target", gs, ws)
			gs, ws = value(i), value(i)
			sameResult(t, label+"/2",
				lisp.ValueOrError(env.AppendInPlace(gs, value(j), lisp.Int(7))),
				lisp.ValueOrError(env.CallBuiltin(refAppendMut, ws, value(j), lisp.Int(7))))
			sameResult(t, label+"/2/target", gs, ws)
		}
	}
}

// mapFns are the fn arguments MapSeq is tried with: a lambda, a symbol bound
// to it, a lambda that raises, an unbound symbol, a symbol bound to a value,
// a macro, a special operator and a value of every type.
func mapFns(env *lisp.LEnv) []oracle.Value {
	fns := []oracle.Value{
		{V: lisp.Symbol("twice"), Label: "symbol-fun"},
		{V: env.GetGlobal(lisp.Symbol("twice")), Label: "lambda"},
		{V: env.GetGlobal(lisp.Symbol("raise")), Label: "lambda-raises"},
		{V: lisp.Symbol("no-such-fun"), Label: "symbol-unbound"},
		{V: lisp.Symbol("not-a-fun"), Label: "symbol-value"},
		{V: lisp.Symbol("defun"), Label: "macro"},
		{V: lisp.Symbol("if"), Label: "special-op"},
	}
	return append(fns, oracle.Values()...)
}

const mapSetup = `(defun twice (x) (list x x))
(defun raise (x) (error 'map-failed "no map for" x))
(set 'not-a-fun 3)`

// TestMapSeqMatchesMap holds MapSeq to map, called from a Go builtin, for
// every type specifier, fn and seq.
func TestMapSeqMatchesMap(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", mapSetup)
	require.False(t, rc.IsError(), "%v", rc)
	run := func(f func(env *lisp.LEnv) *lisp.LVal) *lisp.LVal { return inEval(t, env, f) }
	for _, kind := range seqKinds {
		spec := func() *lisp.LVal { return kindSpec(kind) }
		for fi, fn := range mapFns(env) {
			for i, seq := range oracle.Values() {
				label := "map/" + kind.String() + "/" + fn.Label + "/" + seq.Label
				got := run(func(env *lisp.LEnv) *lisp.LVal {
					return lisp.ValueOrError(env.MapSeq(kind, mapFns(env)[fi].V, value(i)))
				})
				want := run(func(env *lisp.LEnv) *lisp.LVal {
					return lisp.ValueOrError(env.CallBuiltin(refMap, spec(), mapFns(env)[fi].V, value(i)))
				})
				sameResult(t, label, got, want)
			}
		}
	}
}

// TestMapSeqCellsMatchesMap holds MapSeqCells to map's read of its sequence
// argument, for a value of every type.  A value map refuses must return the
// same error condition and message.  For any other value, map over a list of
// the returned cells must return what map over the value returns.
func TestMapSeqCellsMatchesMap(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", "(defun wrap (x) (list x))")
	require.False(t, rc.IsError(), "%v", rc)
	run := func(f func(env *lisp.LEnv) *lisp.LVal) *lisp.LVal { return inEval(t, env, f) }
	mapWrap := func(env *lisp.LEnv, seq *lisp.LVal) *lisp.LVal {
		return lisp.ValueOrError(env.CallBuiltin(refMap, lisp.Symbol("list"), lisp.Symbol("wrap"), seq))
	}
	for i, seq := range oracle.Values() {
		label := "map-seq-cells/" + seq.Label
		got := run(func(env *lisp.LEnv) *lisp.LVal {
			cells, lerr := env.MapSeqCells(value(i))
			if lerr != nil {
				return lisp.Error(lerr)
			}
			return mapWrap(env, lisp.QExpr(cells))
		})
		want := run(func(env *lisp.LEnv) *lisp.LVal { return mapWrap(env, value(i)) })
		sameResult(t, label, got, want)
	}
}

// TestMapSeqCellsAliases checks that MapSeqCells returns the value's own
// cells, so a Go port reads them without a copy.
func TestMapSeqCellsAliases(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	list := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	cells, lerr := env.MapSeqCells(list)
	require.Nil(t, lerr)
	require.Len(t, cells, 2)
	assert.Same(t, list.Cells[0], cells[0])

	cells, lerr = env.MapSeqCells(lisp.String("abc"))
	assert.Nil(t, cells)
	require.NotNil(t, lerr)
	assert.Equal(t, "third argument is not a proper sequence: string", (*lisp.ErrorVal)(lisp.Error(lerr)).ErrorMessage())
}

// TestDebugStackMatchesDebugStack holds DebugStack to debug-stack: the same
// result and the same printed stack.
func TestDebugStackMatchesDebugStack(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	var gotOut, wantOut bytes.Buffer
	env.Runtime.Stderr = &gotOut
	got := inEval(t, env, debugStack)
	env.Runtime.Stderr = &wantOut
	want := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallBuiltin(refDebugStack)) })
	sameResult(t, "debug-stack", got, want)
	assert.NotEmpty(t, wantOut.String())
	assert.Equal(t, wantOut.String(), gotOut.String())

	// Under a small allocation limit, both return the same result and print
	// the same text.
	gotOut.Reset()
	wantOut.Reset()
	env.Runtime.MaxAlloc = 1
	env.Runtime.Stderr = &gotOut
	got = inEval(t, env, debugStack)
	env.Runtime.Stderr = &wantOut
	want = inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallBuiltin(refDebugStack)) })
	sameResult(t, "debug-stack/alloc", got, want)
	assert.Equal(t, wantOut.String(), gotOut.String(), "debug-stack/alloc output")
}

// TestListVectorNeedNoGoForm shows that Cells.Clone().List and
// Cells.Clone().Vector return what list and vector return, so neither
// builtin needs a Go form.  The clone gives the result storage of its own,
// as list and vector called from Lisp have: a later write to the caller's
// slice does not reach the result.
func TestListVectorNeedNoGoForm(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for n := range 4 {
		xs := make(lisp.Cells, 0, n+4)
		for i := range n {
			xs = append(xs, value(i*7%numValues))
		}
		list, vec := xs.Clone().List(), xs.Clone().Vector()
		sameResult(t, fmt.Sprintf("list/%d", n), list, lisp.ValueOrError(env.CallBuiltin(refList, xs...)))
		sameResult(t, fmt.Sprintf("vector/%d", n), vec, lisp.ValueOrError(env.CallBuiltin(refVector, xs...)))
		if n == 0 {
			continue
		}
		before := list.String()
		xs[0] = lisp.String("changed")
		assert.Equal(t, before, list.String(), "list/%d: a write to the Go slice reached the list", n)
		assert.NotEqual(t, `"changed"`, vec.Cells[1].Cells[0].String(), "vector/%d", n)
		cells, _ := list.SeqCells()
		assert.Equal(t, len(cells), cap(cells), "list/%d: the list has spare capacity", n)
	}
}

// TestGoFormsAllocLimit holds the Go forms that make allocation checks to
// their builtins under a small allocation limit.
func TestGoFormsAllocLimit(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	env.Runtime.MaxAlloc = 1
	big := func() *lisp.LVal {
		return lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	}
	bigVec := func() *lisp.LVal { return lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}) }
	bigMap := func() *lisp.LVal { return lisp.MapOf("a", lisp.Int(1), "b", lisp.Int(2)) }
	list, vec := lisp.Symbol("list"), lisp.Symbol("vector")
	cases := []struct {
		name      string
		got, want *lisp.LVal
	}{
		{"to-bytes", goValue(env.ToBytes(lisp.String("abc"))), lisp.ValueOrError(env.CallBuiltin(refToBytes, lisp.String("abc")))},
		{"copy", lisp.ValueOrError(env.DeepCopy(big())), lisp.ValueOrError(env.CallBuiltin(refCopy, big()))},
		{"assoc", lisp.ValueOrError(env.MapAssoc(bigMap(), lisp.String("c"), lisp.Int(3))), lisp.ValueOrError(env.CallBuiltin(refAssoc, bigMap(), lisp.String("c"), lisp.Int(3)))},
		{"concat", lisp.ValueOrError(env.Concat(lisp.SeqList, big(), big())), lisp.ValueOrError(env.CallBuiltin(refConcat, list, big(), big()))},
		{"concat-string", lisp.ValueOrError(env.Concat(lisp.SeqString, lisp.String("ab"))), lisp.ValueOrError(env.CallBuiltin(refConcat, lisp.Symbol("string"), lisp.String("ab")))},
		{"zip", lisp.ValueOrError(env.Zip(lisp.SeqVector, big(), big())), lisp.ValueOrError(env.CallBuiltin(refZip, vec, big(), big()))},
		{"slice", lisp.ValueOrError(env.Slice(lisp.SeqList, lisp.String("abc"), lisp.Int(0), lisp.Int(3))), lisp.ValueOrError(env.CallBuiltin(refSlice, list, lisp.String("abc"), lisp.Int(0), lisp.Int(3)))},
		{"append!", lisp.ValueOrError(env.AppendInPlace(bigVec(), lisp.Int(3))), lisp.ValueOrError(env.CallBuiltin(refAppendMut, bigVec(), lisp.Int(3)))},
		{"append!-bytes", lisp.ValueOrError(env.AppendInPlace(lisp.Bytes([]byte{1, 2}), lisp.Int(3))), lisp.ValueOrError(env.CallBuiltin(refAppendMut, lisp.Bytes([]byte{1, 2}), lisp.Int(3)))},
	}
	for _, c := range cases {
		require.True(t, c.want.IsError(), "%s: the builtin did not hit the limit: %v", c.name, c.want)
		sameResult(t, c.name, c.got, c.want)
	}
}

// TestDeepCopyValueDepth holds DeepCopy to copy on a value deeper than the
// runtime's value-depth limit: both raise the same error.
func TestDeepCopyValueDepth(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	require.False(t, lisp.WithMaxValueDepth(1024)(env).IsError())
	deep := func() *lisp.LVal {
		v := lisp.Int(0)
		for range 1100 {
			v = lisp.QExpr([]*lisp.LVal{v})
		}
		return v
	}
	got, want := lisp.ValueOrError(env.DeepCopy(deep())), lisp.ValueOrError(env.CallBuiltin(refCopy, deep()))
	require.True(t, want.IsError(), "%v", want)
	sameResult(t, "copy/depth", got, want)
}

// TestGoFormsCancelled holds each Go form to its builtin at every context
// check: each makes the builtin's checks at the same points.
func TestGoFormsCancelled(t *testing.T) {
	// Each run builds its own inputs: a value belongs to one runtime.
	list3 := func() *lisp.LVal { return lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)}) }
	cases := []struct {
		name      string
		run, want func(env *lisp.LEnv) *lisp.LVal
	}{
		{"to-int",
			func(env *lisp.LEnv) *lisp.LVal { return goValue(env.ToInt(lisp.String("12"))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refToInt, lisp.String("12")))
			}},
		{"to-bytes",
			func(env *lisp.LEnv) *lisp.LVal { return goValue(env.ToBytes(lisp.String("ab"))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refToBytes, lisp.String("ab")))
			}},
		{"copy",
			func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.DeepCopy(list3())) },
			func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallBuiltin(refCopy, list3())) }},
		{"nth",
			func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Nth(list3(), lisp.Int(1))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refNth, list3(), lisp.Int(1)))
			}},
		{"assoc",
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.MapAssoc(lisp.Nil(), lisp.String("a"), lisp.Int(1)))
			},
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refAssoc, lisp.Nil(), lisp.String("a"), lisp.Int(1)))
			}},
		{"key?",
			func(env *lisp.LEnv) *lisp.LVal { return boolValue(env.MapHasKey(lisp.SortedMap(), lisp.String("a"))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refKeyP, lisp.SortedMap(), lisp.String("a")))
			}},
		{"string=",
			func(env *lisp.LEnv) *lisp.LVal { return boolValue(env.StringEqual(lisp.String("a"), lisp.String("a"))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refStringEq, lisp.String("a"), lisp.String("a")))
			}},
		{"<",
			func(env *lisp.LEnv) *lisp.LVal { return boolValue(env.NumLess(lisp.Int(1), lisp.Float(2))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refNumLT, lisp.Int(1), lisp.Float(2)))
			}},
		{"+",
			func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Add(lisp.Int(1), lisp.Int(2))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refAdd, lisp.Int(1), lisp.Int(2)))
			}},
		{"-",
			func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Sub(lisp.Int(1), lisp.Int(2))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refSub, lisp.Int(1), lisp.Int(2)))
			}},
		{"concat",
			func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Concat(lisp.SeqList, list3(), list3())) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refConcat, lisp.Symbol("list"), list3(), list3()))
			}},
		{"zip",
			func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Zip(lisp.SeqList, list3(), list3())) },
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refZip, lisp.Symbol("list"), list3(), list3()))
			}},
		{"slice",
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.Slice(lisp.SeqList, list3(), lisp.Int(0), lisp.Int(2)))
			},
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refSlice, lisp.Symbol("list"), list3(), lisp.Int(0), lisp.Int(2)))
			}},
		{"append!",
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.AppendInPlace(lisp.Vector(nil), lisp.Int(1)))
			},
			func(env *lisp.LEnv) *lisp.LVal {
				return lisp.ValueOrError(env.CallBuiltin(refAppendMut, lisp.Vector(nil), lisp.Int(1)))
			}},
	}
	for _, c := range cases {
		oracle.Sweep(t, c.name, c.run, c.want)
	}
	// map and debug-stack run inside a Go builtin called from Lisp, as a
	// caller of them does.
	load := func(f func(env *lisp.LEnv) *lisp.LVal) func(env *lisp.LEnv) *lisp.LVal {
		return func(env *lisp.LEnv) *lisp.LVal {
			if rc := env.LoadString("setup", mapSetup); rc.IsError() {
				return rc
			}
			env.Runtime.Stderr = &bytes.Buffer{}
			return inEval(t, env, f)
		}
	}
	oracle.Sweep(t, "map",
		load(func(env *lisp.LEnv) *lisp.LVal {
			return lisp.ValueOrError(env.MapSeq(lisp.SeqList, lisp.Symbol("twice"), list3()))
		}),
		load(func(env *lisp.LEnv) *lisp.LVal {
			return lisp.ValueOrError(env.CallBuiltin(refMap, lisp.Symbol("list"), lisp.Symbol("twice"), list3()))
		}))
	oracle.Sweep(t, "debug-stack",
		load(debugStack),
		load(func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.CallBuiltin(refDebugStack)) }))
}

// TestMapSeqFromTerminalFrame calls MapSeq from a Go builtin whose frame is
// marked terminal, as funcall and apply mark their own frame.  A callback
// whose last form calls that builtin again still runs as a plain call, and
// the frame's mark is restored after.
func TestMapSeqFromTerminalFrame(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	calls := 0
	env.AddBuiltins(false, &testBuiltinDef{name: "go-map-f", formals: lisp.Formals(),
		fn: func(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
			calls++
			if calls > 3 {
				return lisp.String("done")
			}
			env.Runtime.Stack.Top().Terminal = true
			v := lisp.ValueOrError(env.MapSeq(lisp.SeqList, lisp.Symbol("f"), lisp.QExpr([]*lisp.LVal{lisp.Int(1)})))
			if !env.Runtime.Stack.Top().Terminal {
				return env.Errorf("the frame's terminal mark was not restored")
			}
			env.Runtime.Stack.Top().Terminal = false
			if v.IsError() {
				return v
			}
			return v.Cells[0]
		}})
	v := env.LoadString("test", `(defun f (x) (go-map-f)) (f 0)`)
	require.False(t, v.IsError(), "%v", v)
	assert.Equal(t, `"done"`, v.String())
	assert.Equal(t, 4, calls)
}

// TestGoFormsSteps holds each Go form to its builtin on the steps it
// charges: none for every form, MapSeq over an empty seq included, and for
// MapSeq over a list the same count as map, the callback's own steps
// included.
func TestGoFormsSteps(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	rc := env.LoadString("setup", mapSetup)
	require.False(t, rc.IsError(), "%v", rc)
	list3 := func() *lisp.LVal { return lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)}) }
	steps := func(f func(env *lisp.LEnv) *lisp.LVal) int64 {
		var n int64
		v := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal {
			before := env.Runtime.Steps()
			v := f(env)
			n = env.Runtime.Steps() - before
			return v
		})
		require.False(t, v.IsError(), "%v", v)
		return n
	}
	got := steps(func(env *lisp.LEnv) *lisp.LVal {
		return lisp.ValueOrError(env.MapSeq(lisp.SeqList, lisp.Symbol("twice"), list3()))
	})
	want := steps(func(env *lisp.LEnv) *lisp.LVal {
		return lisp.ValueOrError(env.CallBuiltin(refMap, lisp.Symbol("list"), lisp.Symbol("twice"), list3()))
	})
	assert.Positive(t, want)
	assert.Equal(t, want, got, "map")
	b := boolValue
	forms := map[string]func(env *lisp.LEnv) *lisp.LVal{
		"to-int":   func(env *lisp.LEnv) *lisp.LVal { return goValue(env.ToInt(lisp.String("12"))) },
		"to-bytes": func(env *lisp.LEnv) *lisp.LVal { return goValue(env.ToBytes(lisp.String("ab"))) },
		"copy":     func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.DeepCopy(list3())) },
		"nth":      func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Nth(list3(), lisp.Int(1))) },
		"assoc": func(env *lisp.LEnv) *lisp.LVal {
			return lisp.ValueOrError(env.MapAssoc(lisp.MapOf("a", lisp.Int(1)), lisp.String("b"), lisp.Int(2)))
		},
		"key?": func(env *lisp.LEnv) *lisp.LVal {
			return b(env.MapHasKey(lisp.MapOf("a", lisp.Int(1)), lisp.String("a")))
		},
		"string=": func(env *lisp.LEnv) *lisp.LVal { return b(env.StringEqual(lisp.String("a"), lisp.String("b"))) },
		"=":       func(env *lisp.LEnv) *lisp.LVal { return b(env.NumEqual(lisp.Int(1), lisp.Float(1))) },
		"<":       func(env *lisp.LEnv) *lisp.LVal { return b(env.NumLess(lisp.Int(1), lisp.Float(2))) },
		"<=":      func(env *lisp.LEnv) *lisp.LVal { return b(env.NumLessEqual(lisp.Int(1), lisp.Int(2))) },
		">":       func(env *lisp.LEnv) *lisp.LVal { return b(env.NumGreater(lisp.Float(1), lisp.Int(2))) },
		">=":      func(env *lisp.LEnv) *lisp.LVal { return b(env.NumGreaterEqual(lisp.Int(1), lisp.Int(2))) },
		"+":       func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Add(lisp.Int(1), lisp.Float(2))) },
		"-": func(env *lisp.LEnv) *lisp.LVal {
			return lisp.ValueOrError(env.Sub(lisp.Int(1), lisp.Int(2), lisp.Int(3)))
		},
		"concat": func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Concat(lisp.SeqList, list3(), list3())) },
		"zip":    func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.Zip(lisp.SeqList, list3(), list3())) },
		"slice": func(env *lisp.LEnv) *lisp.LVal {
			return lisp.ValueOrError(env.Slice(lisp.SeqVector, list3(), lisp.Int(0), lisp.Int(2)))
		},
		"append!": func(env *lisp.LEnv) *lisp.LVal {
			return lisp.ValueOrError(env.AppendInPlace(lisp.Vector(nil), lisp.Int(1)))
		},
		"debug-stack": func(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.DebugStack()) },
		"map-empty": func(env *lisp.LEnv) *lisp.LVal {
			return lisp.ValueOrError(env.MapSeq(lisp.SeqList, lisp.Symbol("twice"), lisp.Nil()))
		},
	}
	env.Runtime.Stderr = &bytes.Buffer{}
	for name, f := range forms {
		assert.Zero(t, steps(f), name)
	}
}

// debugStack is LEnv.DebugStack as the value its builtin returns.
func debugStack(env *lisp.LEnv) *lisp.LVal { return lisp.ValueOrError(env.DebugStack()) }
