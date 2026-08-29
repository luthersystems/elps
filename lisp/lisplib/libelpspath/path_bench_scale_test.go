// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Cost benchmarks for path CONSTRUCTION, RENDERING and PARSING -- the three
// stages that run before a document is touched.
//
// They exist for the PR benchmark gate, which compares a branch against its
// base, and they are aimed at a different failure than the cost unit tests
// beside them. TestNormalizePathsIsNotExponential, IsNotQuadratic and
// TestStringIsLinearInNesting assert allocation counts and byte totals, so
// they fail hard and immediately on a COMPLEXITY regression and say which
// one. Nothing there watches the constant factor, which is what an extra
// allocation per step or a lost fast path costs, and what these catch.
//
// Two sizes for each stage, deliberately:
//
//   - "practical" is the shape real callers use -- two or three steps over a
//     small document. A regression here is one that everybody pays.
//   - the larger sizes are a second signal on the same axis: they are chosen
//     to stay cheap while a superlinear regression makes them diverge from
//     the practical arm rather than track it, so a gate comparing the two
//     arms sees the shape change even when the unit tests are the thing that
//     names it.
//
// The larger arms stay modest on purpose. `go test -bench` runs at least one
// iteration whatever -benchtime says, so a size at which a reintroduced
// exponential costs seconds would put those seconds into every CI benchmark
// round on both arms.

func benchSteps(n int, step func(int) *lisp.LVal) []*lisp.LVal {
	out := make([]*lisp.LVal, n)
	for i := range out {
		out[i] = step(i)
	}
	return out
}

func dotStep(int) *lisp.LVal  { return lisp.String("k") }
func iterStep(int) *lisp.LVal { return lisp.Symbol("*") }

// BenchmarkPathConstruct covers ArgsToPath, which is what the ? family calls
// on every invocation: normalizePaths runs per call, so its cost is paid per
// operation and not amortised.
func BenchmarkPathConstruct(b *testing.B) {
	cases := []struct {
		name  string
		steps []*lisp.LVal
	}{
		{"practical/key.key.key", benchSteps(3, dotStep)},
		{"practical/key.iter.key", []*lisp.LVal{
			lisp.String("items"), lisp.Symbol("*"), lisp.String("id"),
		}},
		{"practical/key.index.key", []*lisp.LVal{
			lisp.String("items"), lisp.Int(0), lisp.String("id"),
		}},
		{"keys/100", benchSteps(100, dotStep)},
		{"keys/400", benchSteps(400, dotStep)},
		{"iterators/8", benchSteps(8, iterStep)},
		{"iterators/16", benchSteps(16, iterStep)},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if _, err := ArgsToPath(tc.steps); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

// BenchmarkPathString covers rendering. Composition is the axis that matters:
// the leaves are O(1) and only nesting compounds.
func BenchmarkPathString(b *testing.B) {
	build := func(n int, f func(int) *lisp.LVal) Path {
		p, err := ArgsToPath(benchSteps(n, f))
		if err != nil {
			b.Fatal(err)
		}
		return p
	}
	cases := []struct {
		name string
		path Path
	}{
		{"practical/3keys", build(3, dotStep)},
		{"practical/key.iter.key", func() Path {
			p, err := ArgsToPath([]*lisp.LVal{
				lisp.String("items"), lisp.Symbol("*"), lisp.String("id"),
			})
			if err != nil {
				b.Fatal(err)
			}
			return p
		}()},
		{"keys/200", build(200, dotStep)},
		{"nested-iterators/50", build(50, iterStep)},
		{"nested-iterators/200", build(200, iterStep)},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				_ = tc.path.String()
			}
		})
	}
}

// BenchmarkParseSelector covers the jq-string front end, whose cost is paid
// per call by every v1 operation downstream -- nothing caches a parsed path.
func BenchmarkParseSelector(b *testing.B) {
	cases := []struct{ name, sel string }{
		{"practical/dot", ".a.b.c"},
		{"practical/index", ".items[0].id"},
		{"practical/quoted", `.["first name"].address.city`},
		{"practical/range", ".items[1:3]"},
		{"practical/open-range", ".items[1:]"},
		{"keys/100", "." + strings.TrimSuffix(strings.Repeat("k.", 100), ".")},
		{"iterators/50", "." + strings.Repeat("[]", 50)},
	}
	for _, tc := range cases {
		b.Run(tc.name, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				if _, err := ParseSelector(tc.sel); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

// BenchmarkPathEndToEnd is the whole per-operation cost a v1 caller pays:
// parse a selector, then run it. It is the arm that would catch a
// construction regression being masked by a cheap Get, or the reverse.
func BenchmarkPathEndToEnd(b *testing.B) {
	doc := []byte(`{"items":[{"id":1,"v":"a"},{"id":2,"v":"b"},{"id":3,"v":"c"}],"n":3}`)
	for _, sel := range []string{".items[0].id", ".items[].id", ".items[1:]"} {
		b.Run("get/"+sel, func(b *testing.B) {
			b.ReportAllocs()
			for b.Loop() {
				p, err := ParseSelector(sel)
				if err != nil {
					b.Fatal(err)
				}
				if _, err := p.Get(libjson.Load(doc, false)); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}
