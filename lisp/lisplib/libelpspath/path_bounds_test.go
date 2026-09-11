// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// This file covers the bounds handling of the path engine that the
// positional-arg (?-family) builtins drive. The cases mirror
// path_bounds_test.go in luthersystems/substrate, which exercises the same
// engine through both its legacy string API and this positional API.

// allMethods is every operation the path engine exposes.
var allMethods = []PathMethod{Get, Set, SetMutate, Del, DelMutate, Nil, NilMutate}

func (m PathMethod) String() string {
	switch m {
	case Get:
		return "Get"
	case Set:
		return "Set"
	case SetMutate:
		return "SetMutate"
	case Del:
		return "Del"
	case DelMutate:
		return "DelMutate"
	case Nil:
		return "Nil"
	case NilMutate:
		return "NilMutate"
	}
	return "unknown"
}

// applyMethod runs one operation and renders the outcome as a comparable
// string. Errors are reported as a bare marker rather than their message;
// what matters to these tests is which inputs fail, not the prose.
func applyMethod(path Path, method PathMethod, in string) (result string, failed bool) {
	lval := libjson.Load([]byte(in), false)
	newVal := lisp.String("REPLACEMENT")

	var data *lisp.LVal
	var err error
	switch method {
	case Get:
		data, err = path.Get(lval)
	case Set:
		data, err = path.Set(lval, newVal)
	case SetMutate:
		data, err = path.SetMutate(lval, newVal)
	case Del:
		data, err = path.Delete(lval)
	case DelMutate:
		data, err = path.DeleteMutate(lval)
	case Nil:
		data, err = path.Nil(lval)
	case NilMutate:
		data, err = path.NilMutate(lval)
	}
	if err != nil {
		return "", true
	}
	// Render both the operation's return value and the (possibly mutated)
	// input, so mutate-vs-copy differences cannot hide.
	var out string
	if data != nil {
		b, derr := libjson.Dump(data, false)
		if derr != nil {
			return "<undumpable>", false
		}
		out = string(b)
	} else {
		out = "<nil>"
	}
	orig, derr := libjson.Dump(lval, false)
	if derr != nil {
		return out + " | <undumpable>", false
	}
	return out + " | " + string(orig), false
}

// TestNegativeIndexOutOfRange pins the fix for an out-of-bounds panic: a
// negative index counts back from the end, so one whose magnitude exceeds
// the sequence length used to stay negative after folding and index out of
// bounds. Every operation now treats it the same way it treats an index
// past the end.
//
// This matters because it is reachable straight from lisp code — (? items
// -1) on an empty array — and a panic cannot be caught by handler-bind.
func TestNegativeIndexOutOfRange(t *testing.T) {
	t.Parallel()
	cases := []struct {
		Name  string
		Steps []*lisp.LVal
		In    string
	}{
		{Name: "last of empty array", Steps: []*lisp.LVal{lisp.Int(-1)}, In: `[]`},
		{Name: "beyond start", Steps: []*lisp.LVal{lisp.Int(-5)}, In: `["a","b"]`},
		{Name: "far beyond start", Steps: []*lisp.LVal{lisp.Int(-100)}, In: `["a"]`},
		{Name: "nested", Steps: []*lisp.LVal{lisp.String("a"), lisp.Int(-3)}, In: `{"a":[1]}`},
		{Name: "boundary in range", Steps: []*lisp.LVal{lisp.Int(-2)}, In: `["a","b"]`},
		{Name: "boundary out of range", Steps: []*lisp.LVal{lisp.Int(-3)}, In: `["a","b"]`},
	}
	for _, tc := range cases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			path, err := ArgsToPath(tc.Steps)
			if err != nil {
				t.Fatalf("ArgsToPath: %v", err)
			}
			for _, method := range allMethods {
				// The assertion is simply that no operation panics;
				// applyMethod would not return at all otherwise.
				res, failed := applyMethod(path, method, tc.In)
				t.Logf("%s: %s (failed=%v)", method, res, failed)
			}
		})
	}
}

// TestNegativeIndexInRangeStillWorks guards the fix above against
// over-correction: negative indexes that land inside the sequence must
// keep working.
func TestNegativeIndexInRangeStillWorks(t *testing.T) {
	t.Parallel()
	path, err := ArgsToPath([]*lisp.LVal{lisp.Int(-1)})
	if err != nil {
		t.Fatalf("ArgsToPath: %v", err)
	}
	res, failed := applyMethod(path, Get, `["a","b","c"]`)
	if failed {
		t.Fatal("get -1 failed")
	}
	if res != `"c" | ["a","b","c"]` {
		t.Fatalf("get -1 = %s", res)
	}
}

// TestRootAndIterDeleteAreLispNil pins the second crash: deleting an empty
// chain — which is what the root path (?del v), and every element of a bare
// iterator (?del v '*), reduce to — used to hand back an untyped Go nil
// with no error. That nil was stored straight into the result array, so the
// value looked fine until something dereferenced it: json:dump-bytes,
// printing, or a further path operation would panic the interpreter instead
// of raising a catchable condition.
//
// The contract asserted here is that every value an operation returns is a
// real LVal.
func TestRootAndIterDeleteAreLispNil(t *testing.T) {
	t.Parallel()

	assertNoGoNils := func(t *testing.T, v *lisp.LVal) {
		t.Helper()
		if v == nil {
			t.Fatal("operation returned an untyped Go nil")
		}
		var walk func(*lisp.LVal)
		walk = func(n *lisp.LVal) {
			if n == nil {
				t.Fatal("result contains an untyped Go nil cell")
			}
			for _, c := range n.Cells {
				walk(c)
			}
		}
		walk(v)
	}

	iter := lisp.Symbol("*") // the "iterate all elements" step
	cases := []struct {
		Name     string
		Steps    []*lisp.LVal
		In       string
		Expected string
	}{
		{Name: "root", Steps: nil, In: `{"hello":"world"}`, Expected: `null`},
		{Name: "root of array", Steps: nil, In: `["a","b"]`, Expected: `null`},
		{Name: "bare iter over scalars", Steps: []*lisp.LVal{iter}, In: `["a","b","c"]`, Expected: `[null,null,null]`},
		{Name: "bare iter over maps", Steps: []*lisp.LVal{iter}, In: `[{"a":1},{"b":2}]`, Expected: `[null,null]`},
		{Name: "bare iter empty", Steps: []*lisp.LVal{iter}, In: `[]`, Expected: `[]`},
		{Name: "nested bare iter", Steps: []*lisp.LVal{lisp.String("a"), iter}, In: `{"a":["x","y"]}`, Expected: `{"a":[null,null]}`},
	}
	for _, tc := range cases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			path, err := ArgsToPath(tc.Steps)
			if err != nil {
				t.Fatalf("ArgsToPath: %v", err)
			}
			out, err := path.Delete(libjson.Load([]byte(tc.In), false))
			if err != nil {
				t.Fatalf("delete: %v", err)
			}
			assertNoGoNils(t, out)
			// Dumping is what calling code actually does with these
			// values, and is where the old nils detonated.
			b, err := libjson.Dump(out, false)
			if err != nil {
				t.Fatalf("dump: %v", err)
			}
			if string(b) != tc.Expected && !jsonEqual(string(b), tc.Expected) {
				t.Fatalf("delete %v on %s = %s, want %s",
					tc.Steps, tc.In, b, tc.Expected)
			}
		})
	}
}

// TestQueryDeleteBuiltinNeverReturnsGoNil drives the same two shapes
// through the exported builtins, the way the evaluator calls them.
func TestQueryDeleteBuiltinNeverReturnsGoNil(t *testing.T) {
	t.Parallel()
	env := lisp.NewEnv(nil)
	v := lisp.Array(nil, []*lisp.LVal{lisp.Int(1), lisp.Int(2)})

	res := BuiltinQueryDelete(env, lisp.QExpr([]*lisp.LVal{v}))
	if res == nil {
		t.Fatal("(?del v) returned an untyped Go nil")
	}
	if !res.IsNil() {
		t.Fatalf("(?del v) = %v, want ()", res)
	}

	res = BuiltinQueryDelete(env, lisp.QExpr([]*lisp.LVal{v, lisp.Symbol("*")}))
	if res == nil {
		t.Fatal("(?del v '*) returned an untyped Go nil")
	}
	if res.Type == lisp.LArray {
		for i, c := range res.Cells[1].Cells {
			if c == nil {
				t.Fatalf("(?del v '*) cell %d is an untyped Go nil", i)
			}
		}
	}
}
