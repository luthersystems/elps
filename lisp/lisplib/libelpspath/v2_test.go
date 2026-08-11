package libelpspath

import (
	"encoding/json"
	"reflect"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// PathMethod names the Path interface operation a table-driven case
// exercises.
type PathMethod int

const (
	Get PathMethod = iota
	Set
	SetMutate
	Del
	DelMutate
	Nil
	NilMutate
)

// jsonEqual compares two JSON documents structurally.
func jsonEqual(a, b string) bool {
	var aRaw interface{}
	err := json.Unmarshal([]byte(a), &aRaw)
	if err != nil {
		return false
	}
	var bRaw interface{}
	err = json.Unmarshal([]byte(b), &bRaw)
	if err != nil {
		return false
	}
	return reflect.DeepEqual(aRaw, bRaw)
}

func TestArgsToPath(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name         string
		Args         []*lisp.LVal
		ExpectedPath string // Path.String() output; empty = don't check
		HasError     bool
	}{
		{
			Name:         "empty args (root)",
			Args:         nil,
			ExpectedPath: ".",
		},
		{
			Name:         "string key",
			Args:         []*lisp.LVal{lisp.String("foo")},
			ExpectedPath: `.["foo"]`,
		},
		{
			Name:         "int index",
			Args:         []*lisp.LVal{lisp.Int(0)},
			ExpectedPath: ".[0]",
		},
		{
			Name:         "negative int index",
			Args:         []*lisp.LVal{lisp.Int(-1)},
			ExpectedPath: ".[-1]",
		},
		{
			Name:         "star symbol (iter)",
			Args:         []*lisp.LVal{lisp.Symbol("*")},
			ExpectedPath: ".[]",
		},
		{
			Name:         "range expression",
			Args:         []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(1), lisp.Int(3)})},
			ExpectedPath: ".[1:3]",
		},
		{
			Name:         "mixed path",
			Args:         []*lisp.LVal{lisp.String("foo"), lisp.Int(0), lisp.String("bar")},
			ExpectedPath: `.["foo"][0]["bar"]`,
		},
		{
			Name:     "unsupported symbol",
			Args:     []*lisp.LVal{lisp.Symbol("bad")},
			HasError: true,
		},
		{
			Name:     "unsupported type (float)",
			Args:     []*lisp.LVal{lisp.Float(1.5)},
			HasError: true,
		},
		{
			Name:     "range wrong arg count",
			Args:     []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(1)})},
			HasError: true,
		},
		{
			Name:     "range non-int from",
			Args:     []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.String("a"), lisp.Int(3)})},
			HasError: true,
		},
		{
			Name:     "range non-int to",
			Args:     []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(1), lisp.String("b")})},
			HasError: true,
		},
		{
			Name:     "empty s-expression",
			Args:     []*lisp.LVal{lisp.SExpr(nil)},
			HasError: true,
		},
		{
			Name:     "s-expression with non-symbol head",
			Args:     []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})},
			HasError: true,
		},
		{
			Name:     "s-expression with unknown symbol",
			Args:     []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("unknown"), lisp.Int(1), lisp.Int(2)})},
			HasError: true,
		},
	}
	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			path, err := ArgsToPath(tc.Args)
			if tc.HasError {
				if err == nil {
					t.Fatal("expected error")
				}
				return
			}
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
			if tc.ExpectedPath != "" {
				got := path.String()
				if got != tc.ExpectedPath {
					t.Fatalf("path.String() = %q, want %q", got, tc.ExpectedPath)
				}
			}
		})
	}
}

func TestV2Operations(t *testing.T) {
	t.Parallel()
	testCases := []struct {
		Name     string
		In       string
		Args     []*lisp.LVal // path step args (for set ops, last arg is value)
		Method   PathMethod
		Expected string
		HasError bool
	}{
		// --- Get ---
		{
			Name:     "get simple key",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello")},
			Expected: `"world"`,
		},
		{
			Name:     "get nested key",
			Method:   Get,
			In:       `{"a":{"b":"world"}}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.String("b")},
			Expected: `"world"`,
		},
		{
			Name:     "get array index",
			Method:   Get,
			In:       `["a","b","c"]`,
			Args:     []*lisp.LVal{lisp.Int(1)},
			Expected: `"b"`,
		},
		{
			Name:     "get negative index",
			Method:   Get,
			In:       `["a","b","c"]`,
			Args:     []*lisp.LVal{lisp.Int(-1)},
			Expected: `"c"`,
		},
		{
			Name:     "get nested index",
			Method:   Get,
			In:       `{"items":["a","b","c"]}`,
			Args:     []*lisp.LVal{lisp.String("items"), lisp.Int(0)},
			Expected: `"a"`,
		},
		{
			Name:     "get missing key returns nil",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("wut")},
			Expected: `null`,
		},
		{
			Name:     "get iterator",
			Method:   Get,
			In:       `[{"a":1},{"a":2},{"a":3}]`,
			Args:     []*lisp.LVal{lisp.Symbol("*"), lisp.String("a")},
			Expected: `[1,2,3]`,
		},
		{
			Name:     "get range",
			Method:   Get,
			In:       `["a","b","c","d"]`,
			Args:     []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(1), lisp.Int(3)})},
			Expected: `["b","c"]`,
		},
		{
			Name:     "get empty range (from==to)",
			Method:   Get,
			In:       `["a","b","c"]`,
			Args:     []*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(1), lisp.Int(1)})},
			Expected: `[]`,
		},
		{
			Name:     "get root (no path steps)",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Args:     nil,
			Expected: `{"hello":"world"}`,
		},
		{
			Name:     "get nested iterator collapses",
			Method:   Get,
			In:       `[{"a":[{"b":2},{"b":3}]},{"a":[{"b":4}]}]`,
			Args:     []*lisp.LVal{lisp.Symbol("*"), lisp.String("a"), lisp.Symbol("*"), lisp.String("b")},
			Expected: `[2,3,4]`,
		},
		// --- Get errors ---
		{
			Name:     "get key on non-map errors",
			Method:   Get,
			In:       `"hello"`,
			Args:     []*lisp.LVal{lisp.String("foo")},
			HasError: true,
		},
		{
			Name:     "get index on map errors",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.Int(0)},
			HasError: true,
		},
		{
			Name:     "get nested key on non-map errors",
			Method:   Get,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello"), lisp.String("nested")},
			HasError: true,
		},
		// --- SetMutate ---
		{
			Name:     "set! simple key",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello"), lisp.Int(42)},
			Expected: `{"hello":42}`,
		},
		{
			Name:     "set! nested key",
			Method:   SetMutate,
			In:       `{"a":{"b":"world"}}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.String("b"), lisp.Int(23)},
			Expected: `{"a":{"b":23}}`,
		},
		{
			Name:     "set! array index",
			Method:   SetMutate,
			In:       `["a","b","c"]`,
			Args:     []*lisp.LVal{lisp.Int(1), lisp.String("d")},
			Expected: `["a","d","c"]`,
		},
		// --- SetMutate errors ---
		{
			Name:     "set! root errors (cannot mutate root)",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("replaced")},
			HasError: true,
		},
		{
			Name:     "set! index on map errors",
			Method:   SetMutate,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.Int(0), lisp.String("x")},
			HasError: true,
		},
		// --- Set (copy) ---
		{
			Name:     "set simple key",
			Method:   Set,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello"), lisp.Int(42)},
			Expected: `{"hello":42}`,
		},
		{
			Name:     "set nested key",
			Method:   Set,
			In:       `{"a":{"b":"world"}}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.String("b"), lisp.Int(23)},
			Expected: `{"a":{"b":23}}`,
		},
		{
			Name:     "set root (no path steps)",
			Method:   Set,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("replaced")},
			Expected: `"replaced"`,
		},
		// --- DeleteMutate ---
		{
			Name:     "del! simple key",
			Method:   DelMutate,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello")},
			Expected: `{}`,
		},
		{
			Name:     "del! nested key",
			Method:   DelMutate,
			In:       `{"a":{"b":"world"}}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.String("b")},
			Expected: `{"a":{}}`,
		},
		{
			Name:     "del! array index",
			Method:   DelMutate,
			In:       `["a","b","c"]`,
			Args:     []*lisp.LVal{lisp.Int(1)},
			Expected: `["a","c"]`,
		},
		// --- Delete (copy) ---
		{
			Name:     "del simple key",
			Method:   Del,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello")},
			Expected: `{}`,
		},
		{
			Name:     "del nested key",
			Method:   Del,
			In:       `{"a":{"b":"world"}}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.String("b")},
			Expected: `{"a":{}}`,
		},
		// --- NilMutate ---
		{
			Name:     "nil! simple key",
			Method:   NilMutate,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello")},
			Expected: `{"hello":null}`,
		},
		{
			Name:     "nil! array index",
			Method:   NilMutate,
			In:       `["a","b","c"]`,
			Args:     []*lisp.LVal{lisp.Int(1)},
			Expected: `["a",null,"c"]`,
		},
		// --- NilMutate errors ---
		{
			Name:     "nil! root errors (cannot mutate root)",
			Method:   NilMutate,
			In:       `{"hello":"world"}`,
			Args:     nil,
			HasError: true,
		},
		// --- Nil (copy) ---
		{
			Name:     "nil simple key",
			Method:   Nil,
			In:       `{"hello":"world"}`,
			Args:     []*lisp.LVal{lisp.String("hello")},
			Expected: `{"hello":null}`,
		},
		{
			Name:     "nil root (no steps)",
			Method:   Nil,
			In:       `{"hello":"world"}`,
			Args:     nil,
			Expected: `null`,
		},
		// --- Iterator set/del/nil ---
		{
			Name:     "set! iterator",
			Method:   SetMutate,
			In:       `{"a":[{"b":2},{"c":3},{"b":4}]}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.Symbol("*"), lisp.String("b"), lisp.Int(42)},
			Expected: `{"a":[{"b":42},{"c":3,"b":42},{"b":42}]}`,
		},
		{
			Name:     "del! iterator",
			Method:   DelMutate,
			In:       `{"a":[{"b":2},{"c":3},{"b":4}]}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.Symbol("*"), lisp.String("b")},
			Expected: `{"a":[{},{"c":3},{}]}`,
		},
		{
			Name:     "nil! iterator",
			Method:   NilMutate,
			In:       `{"a":[{"b":2},{"c":3},{"b":4}]}`,
			Args:     []*lisp.LVal{lisp.String("a"), lisp.Symbol("*"), lisp.String("b")},
			Expected: `{"a":[{"b":null},{"b":null,"c":3},{"b":null}]}`,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.Name, func(t *testing.T) {
			t.Parallel()
			lval := libjson.Load([]byte(tc.In), false)

			var data *lisp.LVal
			var err error

			switch tc.Method {
			case Get:
				path, perr := ArgsToPath(tc.Args)
				if perr != nil {
					t.Fatalf("unexpected ArgsToPath error: %v", perr)
				}
				data, err = path.Get(lval)
			case SetMutate:
				if len(tc.Args) < 1 {
					t.Fatal("set! requires at least a value arg")
				}
				steps, newVal := tc.Args[:len(tc.Args)-1], tc.Args[len(tc.Args)-1]
				path, perr := ArgsToPath(steps)
				if perr != nil {
					t.Fatalf("unexpected ArgsToPath error: %v", perr)
				}
				data, err = path.SetMutate(lval, newVal)
			case Set:
				if len(tc.Args) < 1 {
					t.Fatal("set requires at least a value arg")
				}
				steps, newVal := tc.Args[:len(tc.Args)-1], tc.Args[len(tc.Args)-1]
				path, perr := ArgsToPath(steps)
				if perr != nil {
					t.Fatalf("unexpected ArgsToPath error: %v", perr)
				}
				data, err = path.Set(lval, newVal)
			case Del:
				path, perr := ArgsToPath(tc.Args)
				if perr != nil {
					t.Fatalf("unexpected ArgsToPath error: %v", perr)
				}
				data, err = path.Delete(lval)
			case DelMutate:
				path, perr := ArgsToPath(tc.Args)
				if perr != nil {
					t.Fatalf("unexpected ArgsToPath error: %v", perr)
				}
				data, err = path.DeleteMutate(lval)
			case Nil:
				path, perr := ArgsToPath(tc.Args)
				if perr != nil {
					t.Fatalf("unexpected ArgsToPath error: %v", perr)
				}
				data, err = path.Nil(lval)
			case NilMutate:
				path, perr := ArgsToPath(tc.Args)
				if perr != nil {
					t.Fatalf("unexpected ArgsToPath error: %v", perr)
				}
				data, err = path.NilMutate(lval)
			default:
				t.Fatalf("unknown method: %v", tc.Method)
			}

			if tc.HasError {
				if err == nil {
					t.Fatal("expected error")
				}
			} else {
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				var ejson []byte
				if data != nil {
					ejson, err = libjson.Dump(data, false)
				}
				if err != nil {
					t.FailNow()
				}
				if string(ejson) != tc.Expected && !jsonEqual(string(ejson), tc.Expected) {
					t.Fatalf("\ngot:\n%s\n----\nexpected:\n%s\n", string(ejson), tc.Expected)
				}
			}
		})
	}
}

// TestV2MutateCopySemantics verifies that mutate operations modify the
// original and copy operations leave the original unchanged.
func TestV2MutateCopySemantics(t *testing.T) {
	t.Parallel()
	mustJSON := func(v *lisp.LVal) string {
		b, err := libjson.Dump(v, false)
		if err != nil {
			t.Fatalf("dump error: %v", err)
		}
		return string(b)
	}

	t.Run("SetMutate modifies original", func(t *testing.T) {
		t.Parallel()
		lval := libjson.Load([]byte(`{"a":"old"}`), false)
		path, _ := ArgsToPath([]*lisp.LVal{lisp.String("a")})
		_, err := path.SetMutate(lval, lisp.String("new"))
		if err != nil {
			t.Fatal(err)
		}
		got := mustJSON(lval)
		if !jsonEqual(got, `{"a":"new"}`) {
			t.Fatalf("original should be mutated, got %s", got)
		}
	})

	t.Run("Set does not modify original", func(t *testing.T) {
		t.Parallel()
		lval := libjson.Load([]byte(`{"a":"old"}`), false)
		path, _ := ArgsToPath([]*lisp.LVal{lisp.String("a")})
		result, err := path.Set(lval, lisp.String("new"))
		if err != nil {
			t.Fatal(err)
		}
		origJSON := mustJSON(lval)
		resultJSON := mustJSON(result)
		if !jsonEqual(origJSON, `{"a":"old"}`) {
			t.Fatalf("original should be unchanged, got %s", origJSON)
		}
		if !jsonEqual(resultJSON, `{"a":"new"}`) {
			t.Fatalf("result should have new value, got %s", resultJSON)
		}
	})

	t.Run("DeleteMutate modifies original", func(t *testing.T) {
		t.Parallel()
		lval := libjson.Load([]byte(`{"a":"val","b":"keep"}`), false)
		path, _ := ArgsToPath([]*lisp.LVal{lisp.String("a")})
		_, err := path.DeleteMutate(lval)
		if err != nil {
			t.Fatal(err)
		}
		got := mustJSON(lval)
		if !jsonEqual(got, `{"b":"keep"}`) {
			t.Fatalf("original should be mutated, got %s", got)
		}
	})

	t.Run("Delete does not modify original", func(t *testing.T) {
		t.Parallel()
		lval := libjson.Load([]byte(`{"a":"val","b":"keep"}`), false)
		path, _ := ArgsToPath([]*lisp.LVal{lisp.String("a")})
		result, err := path.Delete(lval)
		if err != nil {
			t.Fatal(err)
		}
		origJSON := mustJSON(lval)
		resultJSON := mustJSON(result)
		if !jsonEqual(origJSON, `{"a":"val","b":"keep"}`) {
			t.Fatalf("original should be unchanged, got %s", origJSON)
		}
		if !jsonEqual(resultJSON, `{"b":"keep"}`) {
			t.Fatalf("result should have deletion, got %s", resultJSON)
		}
	})

	t.Run("NilMutate modifies original", func(t *testing.T) {
		t.Parallel()
		lval := libjson.Load([]byte(`{"a":"val"}`), false)
		path, _ := ArgsToPath([]*lisp.LVal{lisp.String("a")})
		_, err := path.NilMutate(lval)
		if err != nil {
			t.Fatal(err)
		}
		got := mustJSON(lval)
		if !jsonEqual(got, `{"a":null}`) {
			t.Fatalf("original should be mutated, got %s", got)
		}
	})

	t.Run("Nil does not modify original", func(t *testing.T) {
		t.Parallel()
		lval := libjson.Load([]byte(`{"a":"val"}`), false)
		path, _ := ArgsToPath([]*lisp.LVal{lisp.String("a")})
		result, err := path.Nil(lval)
		if err != nil {
			t.Fatal(err)
		}
		origJSON := mustJSON(lval)
		resultJSON := mustJSON(result)
		if !jsonEqual(origJSON, `{"a":"val"}`) {
			t.Fatalf("original should be unchanged, got %s", origJSON)
		}
		if !jsonEqual(resultJSON, `{"a":null}`) {
			t.Fatalf("result should have nil, got %s", resultJSON)
		}
	})
}
