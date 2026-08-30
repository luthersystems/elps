// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"context"
	"fmt"
	"runtime/debug"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// THE NO-PANIC CONTRACT.
//
// elpspath may RAISE on anything -- a bad step, an out-of-range index, a
// container it cannot index -- and must PANIC on nothing. The three sweeps
// below assert that from the three surfaces that reach it, because each one
// hides a panic differently and none of them is covered by the others:
//
//   - From ELPS, `env.eval` recovers every Go panic into an ordinary-looking
//     *LVal, so "the expression returned an error" proves nothing at all.
//     The assertion with teeth is lisp.IsInternalPanic(result) == false --
//     the non-forgeable marker keyed off the recovered Go-stack snapshot.
//   - From the BUILTINS called directly, there is no evaluator, so a panic
//     is a plain Go panic. Reaching them this way still exercises argToStep
//     and okSimpleType, which the Go API skips.
//   - From the Go PATH API, there is nothing recovering at all, and a panic
//     terminates the embedding host. This is the surface the one crash these
//     sweeps found lived on (see path_degenerate_shape_test.go).
//
// Each case recovers individually so one bad input reports itself instead of
// aborting the run -- a panic escaping t.Run takes down the whole test
// binary, which is not hypothetical: it happened while these were written
// and two sibling sweeps silently never ran.

func noPanicEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", lerr)
	}
	if lerr := LoadPackage(env); lerr.Type == lisp.LError {
		t.Fatalf("LoadPackage: %v", lerr)
	}
	if lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage)); lerr.Type == lisp.LError {
		t.Fatalf("InPackage: %v", lerr)
	}
	return env
}

// evalNoPanic evaluates src and fails if the engine panicked, whether the
// panic escaped as Go's or was recovered by the evaluator into an
// internal-panic condition.
func evalNoPanic(t *testing.T, env *lisp.LEnv, label, src string) *lisp.LVal {
	t.Helper()
	var res *lisp.LVal
	func() {
		defer func() {
			if r := recover(); r != nil {
				t.Errorf("GO PANIC %s\n  %s\n  %v\n%s", label, src, r, debug.Stack())
			}
		}()
		res = env.LoadStringContext(context.Background(), "no_panic.lisp", src)
	}()
	if lisp.IsInternalPanic(res) {
		t.Errorf("INTERNAL PANIC %s\n  %s\n  => %v", label, src, res)
	}
	return res
}

// TestElpsSurfaceNeverPanics crosses documents, steps and replacement values
// as ELPS SOURCE and evaluates every combination.
//
// Source rather than constructed LVals on purpose: it is the only way to
// exercise the reader, the evaluator's argument handling and the builtins
// together, and it is the surface a phylum author actually types. The step
// column deliberately mixes well-formed steps with malformed ones -- an
// empty list, a bad head symbol, a float, a nested list, arity extremes --
// since a step that argToStep must reject is where a crash would hide.
func TestElpsSurfaceNeverPanics(t *testing.T) {
	t.Parallel()

	docs := []string{
		`(vector)`, `(vector 1 2 3 4 5)`, `'(1 2 3 4 5)`, `'()`,
		`(vector (vector 1 2) (vector 3) (vector))`,
		`(sorted-map "a" (vector 1 2) "b" 3)`, `(sorted-map)`,
		`"hello"`, `""`, `5`, `3.14`, `true`, `'sym`,
		`(to-bytes "abc")`,
		`(vector "a" () 3 (sorted-map "k" "v"))`,
		`(vector (sorted-map "a" (vector (sorted-map "b" (vector 1)))))`,
		`(lambda (x) x)`,
	}
	steps := []string{
		``, // no steps at all: the identity path
		`0`, `-1`, `1`, `5`, `-6`, `999999999`,
		// The int extremes, where validateRange's negative-index rewrite
		// (n+bound) is nearest to wrapping.
		`9223372036854775807`, `-9223372036854775808`,
		`"a"`, `"missing"`, `""`,
		`'*`,
		`'(range 0)`, `'(range 1)`, `'(range -1)`, `'(range 9)`,
		`'(range 9223372036854775807)`, `'(range -9223372036854775808)`,
		`'(range 0 2)`, `'(range 3 1)`, `'(range -3 -1)`,
		`'(range 9223372036854775807 -9223372036854775808)`,
		// Malformed steps: argToStep must refuse each of these.
		`'(range)`, `'(range 0 1 2)`, `'(range "x")`, `'(range 1.5)`,
		`'()`, `'(bogus 1)`, `'(1 2)`, `3.14`, `true`, `(vector 1)`,
		// Multi-step paths, including ones that mix every form.
		`0 0`, `0 "a"`, `'* '*`, `'(range 0) '(range 0)`,
		`"a" 0 '* '(range 0) "b"`,
		`'* 0 '* 0 '* 0 '* 0 '* 0`,
	}
	values := []string{`9`, `(vector 1 2)`, `()`, `"s"`, `(sorted-map "a" 1)`, `'(1 2)`}

	ops := []struct {
		name  string
		value bool
	}{
		{"elpspath:?", false},
		{"elpspath:?set", true},
		{"elpspath:?set!", true},
		{"elpspath:?del", false},
		{"elpspath:?del!", false},
		{"elpspath:?nil", false},
		{"elpspath:?nil!", false},
	}

	// One environment for the whole sweep, which is stronger than one per
	// expression: the parse cache aliases a quoted literal into every
	// evaluation, so an in-place operation that corrupted one would show up
	// in a later expression rather than being isolated away.
	env := noPanicEnv(t)
	n := 0
	for _, op := range ops {
		for _, doc := range docs {
			for _, step := range steps {
				vs := []string{`9`}
				if op.value {
					vs = values
				}
				for _, v := range vs {
					src := fmt.Sprintf("(%s %s %s)", op.name, doc, step)
					if op.value {
						src = fmt.Sprintf("(%s %s %s %s)", op.name, doc, step, v)
					}
					n++
					evalNoPanic(t, env, "elps", src)
				}
			}
		}
	}
	t.Logf("evaluated %d elps expressions", n)
}

// TestEveryValueTypeNeverPanics crosses every LVal TYPE as the document, as
// a step and as the replacement value.
//
// The type axis is what the ELPS sweep above cannot reach on its own: source
// can express most types but not all of them, and not the degenerate array
// layouts (zero- and multi-dimensional) that only lisp.Array's explicit-dims
// branch produces -- which is exactly where the one crash these sweeps found
// was hiding.
//
// A Go nil *LVal is deliberately NOT in the corpus. It is not a value the
// reader or the evaluator can produce; it would be a Go programming error in
// the caller, and every LVal-consuming function in the repository dereferences
// its argument. Including it would report a panic that says nothing about
// this package.
func TestEveryValueTypeNeverPanics(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil
	tagged := lisp.NewEnv(nil)

	kinds := map[string]func() *lisp.LVal{
		"invalid": func() *lisp.LVal { return &lisp.LVal{} },
		"int":     func() *lisp.LVal { return lisp.Int(3) },
		// Int(0) as well as Int(3): as a STEP these are different paths --
		// 3 is out of range on most of the documents here and errors before
		// any write, so a crash in the write itself needs an index that
		// lands. The same reasoning puts the two range spellings and the
		// iterator in the corpus: without them the Go-API arm below builds
		// no rangePath at all, and this test is a TYPE sweep rather than a
		// value sweep unless the values that reach code are present.
		"int-zero": func() *lisp.LVal { return lisp.Int(0) },
		"iter":     func() *lisp.LVal { return lisp.Symbol("*") },
		"range-open": func() *lisp.LVal {
			return lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0)})
		},
		"range-closed": func() *lisp.LVal {
			return lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0), lisp.Int(1)})
		},
		"float":     func() *lisp.LVal { return lisp.Float(1.5) },
		"error":     func() *lisp.LVal { return lisp.Errorf("boom") },
		"symbol":    func() *lisp.LVal { return lisp.Symbol("s") },
		"qsymbol":   func() *lisp.LVal { v := lisp.Symbol("s"); v.Type = lisp.LQSymbol; return v },
		"sexpr":     func() *lisp.LVal { return lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}) },
		"sexpr-nil": func() *lisp.LVal { return lisp.Nil() },
		"quote":     func() *lisp.LVal { return lisp.Quote(lisp.Int(1)) },
		"string":    func() *lisp.LVal { return lisp.String("ab") },
		"string-mt": func() *lisp.LVal { return lisp.String("") },
		"bytes":     func() *lisp.LVal { return lisp.Bytes([]byte("ab")) },
		"bytes-nil": func() *lisp.LVal { return lisp.Bytes(nil) },
		"sortmap":   func() *lisp.LVal { m := lisp.SortedMap(); m.MapSet(lisp.String("a"), lisp.Int(1)); return m },
		"array":     func() *lisp.LVal { return lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}) },
		"array-0d":  func() *lisp.LVal { return lisp.Array(lisp.QExpr(nil), nil) },
		"array-2d": func() *lisp.LVal {
			return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}),
				[]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
		},
		"native":    func() *lisp.LVal { return lisp.Native(struct{ A int }{1}) },
		"native-nl": func() *lisp.LVal { return lisp.Native(nil) },
		"tagged": func() *lisp.LVal {
			return tagged.TaggedValue(lisp.Symbol("mytype"), lisp.Vector([]*lisp.LVal{lisp.Int(1)}))
		},
		"tagged-map": func() *lisp.LVal {
			m := lisp.SortedMap()
			m.MapSet(lisp.String("a"), lisp.Int(1))
			return tagged.TaggedValue(lisp.Symbol("mytype"), m)
		},
		// A tagged value stripped of its payload, which the accessors reach
		// through Cells[0].
		"tagged-empty": func() *lisp.LVal {
			v := tagged.TaggedValue(lisp.Symbol("mytype"), lisp.Int(1))
			v.Cells = nil
			return v
		},
		"typemax": func() *lisp.LVal { v := lisp.Int(1); v.Type = lisp.LTypeMax; return v },
	}

	builtins := []struct {
		name     string
		fn       func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
		wantsVal bool
	}{
		{"?", BuiltinQueryGet, false},
		{"?set", BuiltinQuerySet, true},
		{"?set!", BuiltinQuerySetMutate, true},
		{"?del", BuiltinQueryDelete, false},
		{"?del!", BuiltinQueryDeleteMutate, false},
		{"?nil", BuiltinQueryNil, false},
		{"?nil!", BuiltinQueryNilMutate, false},
		{"parse-path", BuiltinParsePath, false},
	}

	n := 0
	for _, b := range builtins {
		for docName, mkDoc := range kinds {
			for stepName, mkStep := range kinds {
				valNames := []string{"int"}
				if b.wantsVal {
					valNames = []string{"int", "array", "sexpr-nil", "native", "tagged"}
				}
				for _, vn := range valNames {
					label := fmt.Sprintf("%s doc=%s step=%s val=%s", b.name, docName, stepName, vn)
					args := []*lisp.LVal{mkDoc(), mkStep()}
					if b.wantsVal {
						args = append(args, kinds[vn]())
					}
					n++
					mustNotPanic(t, label, func() {
						if got := b.fn(env, lisp.QExpr(args)); got == nil {
							t.Errorf("%s: builtin returned a nil LVal", label)
						}
					})
				}
			}
		}
	}

	// The same crossing again through the Go API, which is NOT redundant:
	// the builtins run okSimpleType before they build a path, and that gate
	// duplicates several of the accessor's own checks. Measured -- reverting
	// toCells' dimensionality guard on its own left this test green through
	// the builtin arm alone, because okSimpleContainerContents caught the
	// shape first. A defect in one of the two therefore needs the surface
	// that skips the other.
	goOps := goPathOps()
	for docName, mkDoc := range kinds {
		for stepName, mkStep := range kinds {
			path, err := ArgsToPath([]*lisp.LVal{mkStep()})
			if err != nil {
				// Not a step this package can express; the builtin arm
				// above already covered the rejection.
				continue
			}
			for _, op := range goOps {
				label := fmt.Sprintf("goapi/%s doc=%s step=%s", op.name, docName, stepName)
				n++
				mustNotPanic(t, label, func() { _, _ = op.run(path, mkDoc()) })
			}
		}
	}
	t.Logf("crossed %d type combinations", n)
}

// TestPathologicalValuesNeverPanic covers the shapes that are awkward to
// reach any other way: values that contain themselves, program literals the
// parse cache shares between evaluations, and documents deeper, wider or
// longer than a source literal can express.
//
// Depth and breadth are here as PANIC cases, not cost cases -- every walk in
// this package (okSimpleType, copyGuarded, expandPaths, String) is recursive,
// and a stack overflow is the one panic Go cannot recover. Measured on the
// sizes below the engine stays comfortable: a 50,000-deep document answers a
// Get in 65ms.
func TestPathologicalValuesNeverPanic(t *testing.T) {
	t.Parallel()

	t.Run("cycles", func(t *testing.T) {
		// A value that contains itself must be REFUSED, not walked forever
		// and not crashed on (issue #393). Every operation, since each runs
		// its own walk.
		for _, src := range []string{
			`(let ([v (vector 1 2)]) (append! v v) (elpspath:? v 2 2 2 2 0))`,
			`(let ([v (vector 1 2)]) (append! v v) (elpspath:? v '*))`,
			`(let ([v (vector 1 2)]) (append! v v) (elpspath:?set v 0 9))`,
			`(let ([v (vector 1 2)]) (append! v v) (elpspath:?set! v 0 9))`,
			`(let ([v (vector 1 2)]) (append! v v) (elpspath:?del v 0))`,
			`(let ([v (vector 1 2)]) (append! v v) (elpspath:?del! v '(range 0)))`,
			`(let ([v (vector 1 2)]) (append! v v) (elpspath:?nil v '(range 0)))`,
			`(let ([m (sorted-map "a" 1)]) (assoc! m "self" m) (elpspath:?set m "a" 9))`,
			// A cycle created THROUGH the operation under test.
			`(let ([v (vector 1 2)]) (elpspath:?set! v 0 v) (elpspath:? v 0 0 0 0))`,
		} {
			env := noPanicEnv(t)
			res := evalNoPanic(t, env, "cycle", src)
			require1Error(t, res, src)
		}
	})

	t.Run("program-literals", func(t *testing.T) {
		// The parse cache aliases a quoted literal into every evaluation, so
		// an in-place operation on one would corrupt the shared AST
		// (substrate#378). errMutateList refuses lists outright; the copying
		// form is allowed and must leave the literal alone.
		env := noPanicEnv(t)
		for _, src := range []string{
			`(elpspath:?set! '(1 2 3) 0 9)`,
			`(elpspath:?del! '(1 2 3) 0)`,
			`(elpspath:?nil! '(1 2 3) '(range 0))`,
			`(elpspath:?set! '((1 2) (3)) 0 0 9)`,
		} {
			require1Error(t, evalNoPanic(t, env, "literal", src), src)
		}
		// Twice, because the second evaluation reads the literal the first
		// one might have corrupted.
		for range 2 {
			res := evalNoPanic(t, env, "literal-copy", `(elpspath:?set '(1 2 3) 0 9)`)
			if res.Type == lisp.LError {
				t.Errorf("the copying form should succeed on a literal: %v", res)
			}
		}
	})

	t.Run("depth", func(t *testing.T) {
		// Past the reader's 10,000-level nesting cap, so these documents are
		// built here and bound into the environment: source could not
		// express them.
		for _, depth := range []int{1000, 20000, 50000} {
			for _, tc := range []struct{ name, src string }{
				{"get", "(elpspath:? deep " + strings.Repeat("0 ", depth) + ")"},
				{"set-copy", "(elpspath:?set deep 0 9)"},
				{"nil-copy", "(elpspath:?nil deep 0)"},
				{"del-mutate", "(elpspath:?del! deep 0)"},
				{"deep-value", "(elpspath:?set (vector 1 2) 0 deep)"},
			} {
				env := noPanicEnv(t)
				env.PutGlobal(lisp.Symbol("deep"), deepNested(depth, "vector"))
				evalNoPanic(t, env, fmt.Sprintf("deep-vector/%s/%d", tc.name, depth), tc.src)
			}
			for _, shape := range []string{"list", "map"} {
				env := noPanicEnv(t)
				env.PutGlobal(lisp.Symbol("deep"), deepNested(depth, shape))
				step := "0"
				if shape == "map" {
					step = `"k"`
				}
				evalNoPanic(t, env, fmt.Sprintf("deep-%s/%d", shape, depth),
					"(elpspath:? deep "+step+")")
			}
		}
	})

	t.Run("breadth", func(t *testing.T) {
		// A step list longer than any source literal, spliced in with apply
		// exactly as parse-path's documented idiom does.
		for _, n := range []int{10000, 200000} {
			for _, mk := range []struct {
				name string
				step func() *lisp.LVal
			}{
				{"iters", func() *lisp.LVal { return lisp.Symbol("*") }},
				{"open-ranges", func() *lisp.LVal {
					return lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0)})
				}},
			} {
				steps := make([]*lisp.LVal, n)
				for i := range steps {
					steps[i] = mk.step()
				}
				env := noPanicEnv(t)
				env.PutGlobal(lisp.Symbol("steps"), lisp.QExpr(steps))
				evalNoPanic(t, env, fmt.Sprintf("apply/%s/%d", mk.name, n),
					"(apply elpspath:? (cons (vector 1) steps))")
			}
		}
		// A wide document, where the RESULT is as wide as the input.
		cells := make([]*lisp.LVal, 100000)
		for i := range cells {
			cells[i] = lisp.Vector([]*lisp.LVal{lisp.Int(i)})
		}
		env := noPanicEnv(t)
		env.PutGlobal(lisp.Symbol("wide"), lisp.Vector(cells))
		evalNoPanic(t, env, "wide/iter", "(elpspath:? wide '* 0)")
		evalNoPanic(t, env, "wide/nil-copy", "(elpspath:?nil wide '(range 0))")
	})

	t.Run("parse-path", func(t *testing.T) {
		env := noPanicEnv(t)
		for _, tc := range []struct{ name, src string }{
			{"apply-get", `(apply elpspath:? (cons (vector (sorted-map "id" 1)) (elpspath:parse-path ".[0].id")))`},
			{"apply-set", `(apply elpspath:?set (concat 'list (list (vector 1 2 3)) (elpspath:parse-path ".[1:]") (list (vector 9))))`},
			{"long-selector", fmt.Sprintf(`(length (elpspath:parse-path "%s"))`, "."+strings.Repeat("[]", 40000))},
			{"long-key", fmt.Sprintf(`(length (elpspath:parse-path ".%s"))`, strings.Repeat("k", 100000))},
			{"apply-onto-cycle", `(let ([v (vector 1 2)]) (append! v v)
                                    (apply elpspath:? (cons v (elpspath:parse-path ".[2]"))))`},
		} {
			evalNoPanic(t, env, "parse-path/"+tc.name, tc.src)
		}
	})
}

// require1Error fails unless res is an ordinary error -- the outcome these
// pathological inputs are supposed to get. A test that only checked "did not
// panic" would pass on an operation that silently returned a wrong answer.
func require1Error(t *testing.T, res *lisp.LVal, src string) {
	t.Helper()
	if res.Type != lisp.LError {
		t.Errorf("expected a raised error, got %v\n  %s", res, src)
	}
}

// deepNested builds a value nested `depth` levels in the requested shape.
func deepNested(depth int, shape string) *lisp.LVal {
	v := lisp.Int(1)
	for range depth {
		switch shape {
		case "vector":
			v = lisp.Vector([]*lisp.LVal{v})
		case "list":
			v = lisp.QExpr([]*lisp.LVal{v})
		case "map":
			m := lisp.SortedMap()
			m.MapSet(lisp.String("k"), v)
			v = m
		}
	}
	return v
}
