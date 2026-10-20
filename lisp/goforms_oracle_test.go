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

// boolValue turns a (bool, *LVal) Go form result into the builtin's value.
func boolValue(ok bool, lerr *lisp.LVal) *lisp.LVal {
	if lerr != nil {
		return lerr
	}
	return lisp.Bool(ok)
}

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
	{"to-int", refToInt, (*lisp.LEnv).ToInt},
	{"to-bytes", refToBytes, (*lisp.LEnv).ToBytes},
	{"copy", refCopy, (*lisp.LEnv).DeepCopy},
}

// binaryForms pairs each two-argument Go form with its builtin.
var binaryForms = []struct {
	name    string
	builtin lisp.BuiltinRef
	fn      func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal
}{
	{"nth", refNth, (*lisp.LEnv).Nth},
	{"key?", refKeyP, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.MapHasKey(a, b)) }},
	{"string=", refStringEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.StringEqual(a, b)) }},
	{"=", refNumEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumEqual(a, b)) }},
	{"<", refNumLT, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumLess(a, b)) }},
	{"<=", refNumLEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumLessEqual(a, b)) }},
	{">", refNumGT, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumGreater(a, b)) }},
	{">=", refNumGEq, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return boolValue(env.NumGreaterEqual(a, b)) }},
	{"+", refAdd, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return env.Add(a, b) }},
	{"-", refSub, func(env *lisp.LEnv, a, b *lisp.LVal) *lisp.LVal { return env.Sub(a, b) }},
}

// typespecs are the type specifiers the sequence forms are tried with: the
// valid ones, an unknown symbol, nil and values that are not symbols.
func typespecs() []*lisp.LVal {
	return []*lisp.LVal{
		lisp.Symbol("list"), lisp.Symbol("vector"), lisp.Symbol("string"),
		lisp.Symbol("bytes"), lisp.Symbol("bogus"), lisp.Nil(), lisp.Int(1),
		lisp.String("list"),
	}
}

// TestGoFormsMatchBuiltins holds each Go form to its builtin on inputs of
// every type: the same value, or the same error condition and message.
func TestGoFormsMatchBuiltins(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for _, f := range unaryForms {
		for i, in := range oracle.Values() {
			sameResult(t, f.name+"/"+in.Label, f.fn(env, in.V), env.CallBuiltin(f.builtin, value(i)))
		}
	}
	for _, f := range binaryForms {
		for i, a := range oracle.Values() {
			for j, b := range oracle.Values() {
				label := f.name + "/" + a.Label + "/" + b.Label
				got := f.fn(env, a.V, b.V)
				want := env.CallBuiltin(f.builtin, value(i), value(j))
				if f.name == "nth" {
					// nth returns the element itself, as the builtin does.
					oracle.SameObject(t, label, f.fn(env, a.V, b.V), env.CallBuiltin(f.builtin, a.V, b.V))
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
			got := env.MapAssoc(gm, value(j), lisp.Int(42))
			want := env.CallBuiltin(refAssoc, wm, value(j), lisp.Int(42))
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
		sameResult(t, "+"+label, env.Add(xs...), env.CallBuiltin(refAdd, xs...))
		sameResult(t, "-"+label, env.Sub(xs...), env.CallBuiltin(refSub, xs...))
	}
}

// TestSequenceFormsMatchBuiltins holds Concat, Zip and Slice to concat, zip
// and slice for every type specifier and for inputs of every type.
func TestSequenceFormsMatchBuiltins(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	for si, ts := range typespecs() {
		tsLabel := ts.String()
		spec := func() *lisp.LVal { return typespecs()[si] }
		sameResult(t, "concat/"+tsLabel+"/none", env.Concat(spec()), env.CallBuiltin(refConcat, spec()))
		for i, a := range oracle.Values() {
			label := tsLabel + "/" + a.Label
			sameResult(t, "concat/"+label, env.Concat(spec(), value(i)), env.CallBuiltin(refConcat, spec(), value(i)))
			sameResult(t, "zip/"+label, env.Zip(spec(), value(i)), env.CallBuiltin(refZip, spec(), value(i)))
			for j, b := range oracle.Values() {
				label := label + "/" + b.Label
				sameResult(t, "concat/"+label,
					env.Concat(spec(), value(i), value(j)),
					env.CallBuiltin(refConcat, spec(), value(i), value(j)))
				sameResult(t, "zip/"+label,
					env.Zip(spec(), value(i), value(j), value(i)),
					env.CallBuiltin(refZip, spec(), value(i), value(j), value(i)))
			}
			for _, start := range []*lisp.LVal{lisp.Int(-1), lisp.Int(0), lisp.Int(1), lisp.Int(5), lisp.String("0")} {
				for _, end := range []*lisp.LVal{lisp.Int(0), lisp.Int(1), lisp.Int(2), lisp.Int(9), lisp.Float(1)} {
					label := fmt.Sprintf("slice/%s/%v/%v", label, start, end)
					sameResult(t, label,
						env.Slice(spec(), value(i), start, end),
						env.CallBuiltin(refSlice, spec(), value(i), start, end))
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
		sameResult(t, "append!/"+s.Label+"/none", env.AppendInPlace(gs), env.CallBuiltin(refAppendMut, ws))
		sameResult(t, "append!/"+s.Label+"/none/target", gs, ws)
		for j, x := range oracle.Values() {
			label := "append!/" + s.Label + "/" + x.Label
			gs, ws := value(i), value(i)
			sameResult(t, label, env.AppendInPlace(gs, value(j)), env.CallBuiltin(refAppendMut, ws, value(j)))
			sameResult(t, label+"/target", gs, ws)
			gs, ws = value(i), value(i)
			sameResult(t, label+"/2",
				env.AppendInPlace(gs, value(j), lisp.Int(7)),
				env.CallBuiltin(refAppendMut, ws, value(j), lisp.Int(7)))
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
	for si, ts := range typespecs() {
		spec := func() *lisp.LVal { return typespecs()[si] }
		for fi, fn := range mapFns(env) {
			for i, seq := range oracle.Values() {
				label := "map/" + ts.String() + "/" + fn.Label + "/" + seq.Label
				got := run(func(env *lisp.LEnv) *lisp.LVal { return env.MapSeq(spec(), mapFns(env)[fi].V, value(i)) })
				want := run(func(env *lisp.LEnv) *lisp.LVal {
					return env.CallBuiltin(refMap, spec(), mapFns(env)[fi].V, value(i))
				})
				sameResult(t, label, got, want)
			}
		}
	}
}

// TestDebugStackMatchesDebugStack holds DebugStack to debug-stack: the same
// result and the same printed stack.
func TestDebugStackMatchesDebugStack(t *testing.T) {
	env := oracle.NewEnv(t, context.Background())
	var gotOut, wantOut bytes.Buffer
	env.Runtime.Stderr = &gotOut
	got := inEval(t, env, (*lisp.LEnv).DebugStack)
	env.Runtime.Stderr = &wantOut
	want := inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refDebugStack) })
	sameResult(t, "debug-stack", got, want)
	assert.NotEmpty(t, wantOut.String())
	assert.Equal(t, wantOut.String(), gotOut.String())

	// Under a small allocation limit, both return the same result and print
	// the same text.
	gotOut.Reset()
	wantOut.Reset()
	env.Runtime.MaxAlloc = 1
	env.Runtime.Stderr = &gotOut
	got = inEval(t, env, (*lisp.LEnv).DebugStack)
	env.Runtime.Stderr = &wantOut
	want = inEval(t, env, func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refDebugStack) })
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
	for n := 0; n <= 3; n++ {
		xs := make(lisp.Cells, 0, n+4)
		for i := range n {
			xs = append(xs, value(i*7%numValues))
		}
		list, vec := xs.Clone().List(), xs.Clone().Vector()
		sameResult(t, fmt.Sprintf("list/%d", n), list, env.CallBuiltin(refList, xs...))
		sameResult(t, fmt.Sprintf("vector/%d", n), vec, env.CallBuiltin(refVector, xs...))
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
		{"to-bytes", env.ToBytes(lisp.String("abc")), env.CallBuiltin(refToBytes, lisp.String("abc"))},
		{"copy", env.DeepCopy(big()), env.CallBuiltin(refCopy, big())},
		{"assoc", env.MapAssoc(bigMap(), lisp.String("c"), lisp.Int(3)), env.CallBuiltin(refAssoc, bigMap(), lisp.String("c"), lisp.Int(3))},
		{"concat", env.Concat(list, big(), big()), env.CallBuiltin(refConcat, list, big(), big())},
		{"concat-string", env.Concat(lisp.Symbol("string"), lisp.String("ab")), env.CallBuiltin(refConcat, lisp.Symbol("string"), lisp.String("ab"))},
		{"zip", env.Zip(vec, big(), big()), env.CallBuiltin(refZip, vec, big(), big())},
		{"slice", env.Slice(list, lisp.String("abc"), lisp.Int(0), lisp.Int(3)), env.CallBuiltin(refSlice, list, lisp.String("abc"), lisp.Int(0), lisp.Int(3))},
		{"append!", env.AppendInPlace(bigVec(), lisp.Int(3)), env.CallBuiltin(refAppendMut, bigVec(), lisp.Int(3))},
		{"append!-bytes", env.AppendInPlace(lisp.Bytes([]byte{1, 2}), lisp.Int(3)), env.CallBuiltin(refAppendMut, lisp.Bytes([]byte{1, 2}), lisp.Int(3))},
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
	got, want := env.DeepCopy(deep()), env.CallBuiltin(refCopy, deep())
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
			func(env *lisp.LEnv) *lisp.LVal { return env.ToInt(lisp.String("12")) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refToInt, lisp.String("12")) }},
		{"to-bytes",
			func(env *lisp.LEnv) *lisp.LVal { return env.ToBytes(lisp.String("ab")) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refToBytes, lisp.String("ab")) }},
		{"copy",
			func(env *lisp.LEnv) *lisp.LVal { return env.DeepCopy(list3()) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refCopy, list3()) }},
		{"nth",
			func(env *lisp.LEnv) *lisp.LVal { return env.Nth(list3(), lisp.Int(1)) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refNth, list3(), lisp.Int(1)) }},
		{"assoc",
			func(env *lisp.LEnv) *lisp.LVal { return env.MapAssoc(lisp.Nil(), lisp.String("a"), lisp.Int(1)) },
			func(env *lisp.LEnv) *lisp.LVal {
				return env.CallBuiltin(refAssoc, lisp.Nil(), lisp.String("a"), lisp.Int(1))
			}},
		{"key?",
			func(env *lisp.LEnv) *lisp.LVal { return boolValue(env.MapHasKey(lisp.SortedMap(), lisp.String("a"))) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refKeyP, lisp.SortedMap(), lisp.String("a")) }},
		{"string=",
			func(env *lisp.LEnv) *lisp.LVal { return boolValue(env.StringEqual(lisp.String("a"), lisp.String("a"))) },
			func(env *lisp.LEnv) *lisp.LVal {
				return env.CallBuiltin(refStringEq, lisp.String("a"), lisp.String("a"))
			}},
		{"<",
			func(env *lisp.LEnv) *lisp.LVal { return boolValue(env.NumLess(lisp.Int(1), lisp.Float(2))) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refNumLT, lisp.Int(1), lisp.Float(2)) }},
		{"+",
			func(env *lisp.LEnv) *lisp.LVal { return env.Add(lisp.Int(1), lisp.Int(2)) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refAdd, lisp.Int(1), lisp.Int(2)) }},
		{"-",
			func(env *lisp.LEnv) *lisp.LVal { return env.Sub(lisp.Int(1), lisp.Int(2)) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refSub, lisp.Int(1), lisp.Int(2)) }},
		{"concat",
			func(env *lisp.LEnv) *lisp.LVal { return env.Concat(lisp.Symbol("list"), list3(), list3()) },
			func(env *lisp.LEnv) *lisp.LVal {
				return env.CallBuiltin(refConcat, lisp.Symbol("list"), list3(), list3())
			}},
		{"zip",
			func(env *lisp.LEnv) *lisp.LVal { return env.Zip(lisp.Symbol("list"), list3(), list3()) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refZip, lisp.Symbol("list"), list3(), list3()) }},
		{"slice",
			func(env *lisp.LEnv) *lisp.LVal {
				return env.Slice(lisp.Symbol("list"), list3(), lisp.Int(0), lisp.Int(2))
			},
			func(env *lisp.LEnv) *lisp.LVal {
				return env.CallBuiltin(refSlice, lisp.Symbol("list"), list3(), lisp.Int(0), lisp.Int(2))
			}},
		{"append!",
			func(env *lisp.LEnv) *lisp.LVal { return env.AppendInPlace(lisp.Vector(nil), lisp.Int(1)) },
			func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refAppendMut, lisp.Vector(nil), lisp.Int(1)) }},
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
		load(func(env *lisp.LEnv) *lisp.LVal { return env.MapSeq(lisp.Symbol("list"), lisp.Symbol("twice"), list3()) }),
		load(func(env *lisp.LEnv) *lisp.LVal {
			return env.CallBuiltin(refMap, lisp.Symbol("list"), lisp.Symbol("twice"), list3())
		}))
	oracle.Sweep(t, "debug-stack",
		load((*lisp.LEnv).DebugStack),
		load(func(env *lisp.LEnv) *lisp.LVal { return env.CallBuiltin(refDebugStack) }))
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
			v := env.MapSeq(lisp.Symbol("list"), lisp.Symbol("f"), lisp.QExpr([]*lisp.LVal{lisp.Int(1)}))
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
	got := steps(func(env *lisp.LEnv) *lisp.LVal { return env.MapSeq(lisp.Symbol("list"), lisp.Symbol("twice"), list3()) })
	want := steps(func(env *lisp.LEnv) *lisp.LVal {
		return env.CallBuiltin(refMap, lisp.Symbol("list"), lisp.Symbol("twice"), list3())
	})
	assert.Positive(t, want)
	assert.Equal(t, want, got, "map")
	b := boolValue
	forms := map[string]func(env *lisp.LEnv) *lisp.LVal{
		"to-int":   func(env *lisp.LEnv) *lisp.LVal { return env.ToInt(lisp.String("12")) },
		"to-bytes": func(env *lisp.LEnv) *lisp.LVal { return env.ToBytes(lisp.String("ab")) },
		"copy":     func(env *lisp.LEnv) *lisp.LVal { return env.DeepCopy(list3()) },
		"nth":      func(env *lisp.LEnv) *lisp.LVal { return env.Nth(list3(), lisp.Int(1)) },
		"assoc": func(env *lisp.LEnv) *lisp.LVal {
			return env.MapAssoc(lisp.MapOf("a", lisp.Int(1)), lisp.String("b"), lisp.Int(2))
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
		"+":       func(env *lisp.LEnv) *lisp.LVal { return env.Add(lisp.Int(1), lisp.Float(2)) },
		"-":       func(env *lisp.LEnv) *lisp.LVal { return env.Sub(lisp.Int(1), lisp.Int(2), lisp.Int(3)) },
		"concat":  func(env *lisp.LEnv) *lisp.LVal { return env.Concat(lisp.Symbol("list"), list3(), list3()) },
		"zip":     func(env *lisp.LEnv) *lisp.LVal { return env.Zip(lisp.Symbol("list"), list3(), list3()) },
		"slice": func(env *lisp.LEnv) *lisp.LVal {
			return env.Slice(lisp.Symbol("vector"), list3(), lisp.Int(0), lisp.Int(2))
		},
		"append!":     func(env *lisp.LEnv) *lisp.LVal { return env.AppendInPlace(lisp.Vector(nil), lisp.Int(1)) },
		"debug-stack": func(env *lisp.LEnv) *lisp.LVal { return env.DebugStack() },
		"map-empty": func(env *lisp.LEnv) *lisp.LVal {
			return env.MapSeq(lisp.Symbol("list"), lisp.Symbol("twice"), lisp.Nil())
		},
	}
	env.Runtime.Stderr = &bytes.Buffer{}
	for name, f := range forms {
		assert.Zero(t, steps(f), name)
	}
}
