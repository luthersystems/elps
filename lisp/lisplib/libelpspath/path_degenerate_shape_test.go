// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"math"
	"runtime/debug"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// DEGENERATE SHAPES AND EXTREME BOUNDS: the panic battery.
//
// Every operation in this package answers with an (*LVal, error) pair, and a
// caller reaching it through the builtins is behind the evaluator's recover,
// which turns a Go panic into an ordinary-looking error value. An EMBEDDER
// calling Path.Set directly is behind nothing at all: a panic there
// terminates the host. So "it returned an error" is the contract, and the
// tests below assert it by calling the Go API with no recover between them
// and the code -- a panic is a failure, recovered per case only so that one
// bad shape reports itself instead of aborting the run.
//
// WHAT THIS FOUND. toCells asked whether an array had MORE than one
// dimension. A ZERO-dimensional array -- dims '(), which lisp.Array builds
// for an embedder passing an empty dims list -- answered no and was accepted
// as indexable. It has no cardinality slot, and storeCells writes one
// unconditionally, so every in-place write through it panicked on
// `dims.Cells[0].Int = len(vals)`: ?set!, ?del! and ?nil! over an index or
// either range spelling. Ten combinations, all
// `index out of range [0] with length 0`, all reproducing on the released
// code. The requirement is now exactly one dimension, checked in toCells and
// in okSimpleContainerContents so the gate and the accessor agree.

// degenerateDocs are the array shapes whose LAYOUT is unusual, plus the
// ordinary ones as a control -- a guard that only ever sees broken input
// stops proving that the good cases still work.
func degenerateDocs() map[string]func() *lisp.LVal {
	return map[string]func() *lisp.LVal{
		// The shape that panicked. dims is an EMPTY list, so there is no
		// slot for the element count.
		"zero-dim-array": func() *lisp.LVal { return lisp.Array(lisp.QExpr(nil), nil) },
		// Two dimensions: refused, and always was.
		"multi-dim-array": func() *lisp.LVal {
			return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)}),
				[]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5), lisp.Int(6)})
		},
		// One dimension of zero: a legitimate empty vector spelled the long
		// way, and the near-miss the fix must NOT reject.
		"one-dim-empty": func() *lisp.LVal { return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(0)}), nil) },
		"one-dim-three": func() *lisp.LVal {
			return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(3)}), nil)
		},
		// The ordinary controls.
		"vector-empty": func() *lisp.LVal { return lisp.Vector(nil) },
		"vector-five": func() *lisp.LVal {
			return lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5)})
		},
		"list-five": func() *lisp.LVal {
			return lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5)})
		},
		"nil":        func() *lisp.LVal { return lisp.Nil() },
		"string":     func() *lisp.LVal { return lisp.String("hello") },
		"int":        func() *lisp.LVal { return lisp.Int(7) },
		"sorted-map": func() *lisp.LVal { return lisp.SortedMap() },
	}
}

// pathOpCall names one operation on the Go API, which is the surface with no
// recover in front of it.
type pathOpCall struct {
	name string
	run  func(Path, *lisp.LVal) (*lisp.LVal, error)
}

func goPathOps() []pathOpCall {
	val := func() *lisp.LVal {
		return lisp.Vector([]*lisp.LVal{lisp.Int(91), lisp.Int(92)})
	}
	return []pathOpCall{
		{"Get", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.Get(d) }},
		{"Set", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.Set(d, val()) }},
		{"SetMutate", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.SetMutate(d, val()) }},
		{"Delete", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.Delete(d) }},
		{"DeleteMutate", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.DeleteMutate(d) }},
		{"Nil", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.Nil(d) }},
		{"NilMutate", func(p Path, d *lisp.LVal) (*lisp.LVal, error) { return p.NilMutate(d) }},
	}
}

// mustNotPanic runs fn and turns a panic into a test failure carrying the
// stack, so the battery reports every offending combination in one run
// rather than aborting on the first.
func mustNotPanic(t *testing.T, label string, fn func()) {
	t.Helper()
	defer func() {
		if r := recover(); r != nil {
			t.Errorf("PANIC %s: %v\n%s", label, r, debug.Stack())
		}
	}()
	fn()
}

// TestDegenerateArrayShapesDoNotPanic is the regression for the
// zero-dimensional array, and the general guard for the shape axis.
func TestDegenerateArrayShapesDoNotPanic(t *testing.T) {
	t.Parallel()

	paths := map[string]Path{
		"index-0":      Index(0),
		"index-neg":    Index(-1),
		"range-open-0": Range(0, 0, true),
		"range-open-2": Range(2, 0, true),
		"range-closed": Range(0, 1, false),
		"iter":         Iter(),
		"dot":          Dot("a"),
		"chain":        Chain(Range(0, 0, true), Index(0)),
		"root":         Root(Chain(Range(1, 0, true))),
	}

	for docName, mkDoc := range degenerateDocs() {
		for pathName, p := range paths {
			for _, op := range goPathOps() {
				label := fmt.Sprintf("doc=%s path=%s op=%s", docName, pathName, op.name)
				mustNotPanic(t, label, func() {
					// The pair is the contract: whatever happens, it comes
					// back as a value or an error, never as a panic.
					v, err := op.run(p, mkDoc())
					if err == nil && v == nil {
						t.Errorf("%s: nil value and nil error", label)
					}
				})
			}
		}
	}
}

// TestZeroDimensionalArrayIsRefused pins the FIX rather than the absence of
// the crash, which is the half a no-panic sweep cannot check: a guard that
// silently accepted the shape and wrote inconsistent dims would pass the
// sweep above and corrupt the array.
//
// The message is asserted too, because the multi-dimensional case has to
// keep its existing wording -- anything downstream matching that text still
// has to match -- and the zero case has to be distinguishable from it.
func TestZeroDimensionalArrayIsRefused(t *testing.T) {
	t.Parallel()

	zeroDim := func() *lisp.LVal { return lisp.Array(lisp.QExpr(nil), nil) }
	require.Equalf(t, lisp.LArray, zeroDim().Type,
		"the fixture stopped being an array; the test below proves nothing")

	for _, op := range goPathOps() {
		for pathName, p := range map[string]Path{
			"index":        Index(0),
			"range-open":   Range(0, 0, true),
			"range-closed": Range(0, 1, false),
		} {
			t.Run(op.name+"/"+pathName, func(t *testing.T) {
				// Through mustNotPanic even though the assertions below
				// would fail anyway: a panic inside t.Run with nothing
				// recovering it takes down the whole test BINARY, so a
				// regression here would abort every other test in the
				// package before they reported. Measured -- reverting the
				// guard did exactly that, and two sibling sweeps never ran.
				var err error
				mustNotPanic(t, op.name+"/"+pathName, func() {
					_, err = op.run(p, zeroDim())
				})
				require.Errorf(t, err, "a zero-dimensional array was accepted")
				require.Equalf(t, "cannot index zero-dimensional array", err.Error(),
					"the refusal should name the shape")
			})
		}
	}

	// The neighbouring shapes, so the guard is not simply "reject arrays".
	oneDim := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(3)}), nil)
	_, err := Index(0).Get(oneDim)
	require.NoErrorf(t, err, "a one-dimensional array must still be indexable")

	multiDim := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)}),
		[]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5), lisp.Int(6)})
	_, err = Index(0).Get(multiDim)
	require.EqualErrorf(t, err, "cannot index multi-dimensional array",
		"the multi-dimensional wording must not change")
}

// TestExtremeRangeBoundsDoNotPanic sweeps the arithmetic in validateRange,
// which rewrites a negative bound as n+bound and then compares. The extremes
// are where that rewrite could wrap: math.MinInt+n is still negative, but
// only because |MinInt| dwarfs any real document, and nothing in the code
// says so out loud.
//
// Both spellings, because the open form skips the `to` rewrite entirely and
// so reaches the comparisons with a value the closed form can never produce.
func TestExtremeRangeBoundsDoNotPanic(t *testing.T) {
	t.Parallel()

	bounds := []int{
		math.MinInt, math.MinInt + 1, math.MinInt / 2,
		-9, -6, -5, -1, 0, 1, 4, 5, 6, 9,
		math.MaxInt / 2, math.MaxInt - 1, math.MaxInt,
	}
	docs := degenerateDocs()

	cases := 0
	for _, from := range bounds {
		open := Range(from, 0, true)
		for docName, mkDoc := range docs {
			for _, op := range goPathOps() {
				label := fmt.Sprintf("open from=%d doc=%s op=%s", from, docName, op.name)
				cases++
				mustNotPanic(t, label, func() { _, _ = op.run(open, mkDoc()) })
			}
		}
		for _, to := range bounds {
			closed := Range(from, to, false)
			// String() is swept too: it is the only operation that takes no
			// document, so a bound it cannot render would go unseen above.
			mustNotPanic(t, fmt.Sprintf("String from=%d to=%d", from, to),
				func() { _ = closed.String() })
			for docName, mkDoc := range docs {
				for _, op := range goPathOps() {
					label := fmt.Sprintf("closed from=%d to=%d doc=%s op=%s", from, to, docName, op.name)
					cases++
					mustNotPanic(t, label, func() { _, _ = op.run(closed, mkDoc()) })
				}
			}
		}
	}
	t.Logf("swept %d operations over extreme range bounds", cases)
}

// TestBuiltinSurfaceDoesNotPanic is the same sweep one layer up.
//
// It is not redundant with the Go-API sweep: the builtins parse their steps
// from lisp values first (argToStep), run okSimpleType over the document,
// and only then build a path -- three stages the Go API skips, each able to
// reach the engine with a shape the API could not construct.
//
// callBuiltin is deliberately NOT used here. It recovers panics into an
// error value, which is exactly the confusion this test exists to avoid:
// a panic must fail, not be reported as an ordinary rejection.
func TestBuiltinSurfaceDoesNotPanic(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	steps := map[string]*lisp.LVal{
		"index":          lisp.Int(0),
		"index-neg":      lisp.Int(-1),
		"index-min":      lisp.Int(math.MinInt),
		"index-max":      lisp.Int(math.MaxInt),
		"range-open":     lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(1)}),
		"range-open-min": lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(math.MinInt)}),
		"range-closed":   lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0), lisp.Int(2)}),
		"range-inverted": lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(4), lisp.Int(1)}),
		"iter":           lisp.Symbol("*"),
		"key":            lisp.String("a"),
		// Malformed steps, which argToStep must reject rather than crash on.
		"range-no-args":  lisp.QExpr([]*lisp.LVal{lisp.Symbol("range")}),
		"range-too-many": lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(0), lisp.Int(1), lisp.Int(2)}),
		"range-non-int":  lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.String("x")}),
		"empty-sexpr":    lisp.QExpr(nil),
	}
	values := map[string]func() *lisp.LVal{
		"vec2":   func() *lisp.LVal { return lisp.Vector([]*lisp.LVal{lisp.Int(91), lisp.Int(92)}) },
		"vec0":   func() *lisp.LVal { return lisp.Vector(nil) },
		"nil":    func() *lisp.LVal { return lisp.Nil() },
		"int":    func() *lisp.LVal { return lisp.Int(7) },
		"string": func() *lisp.LVal { return lisp.String("x") },
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
	}

	cases := 0
	for _, b := range builtins {
		for stepName, step := range steps {
			for docName, mkDoc := range degenerateDocs() {
				valNames := []string{"vec2"}
				if b.wantsVal {
					valNames = []string{"vec2", "vec0", "nil", "int", "string"}
				}
				for _, vn := range valNames {
					label := fmt.Sprintf("%s doc=%s step=%s val=%s", b.name, docName, stepName, vn)
					args := []*lisp.LVal{mkDoc(), step}
					if b.wantsVal {
						args = append(args, values[vn]())
					}
					cases++
					mustNotPanic(t, label, func() {
						got := b.fn(env, lisp.QExpr(args))
						if got == nil {
							t.Errorf("%s: builtin returned a nil LVal", label)
						}
					})
				}
			}
		}
	}
	t.Logf("swept %d builtin invocations", cases)
}
