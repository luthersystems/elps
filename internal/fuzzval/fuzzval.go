// Copyright © 2026 The ELPS authors

// Package fuzzval turns a fuzzer-supplied byte string into typed lisp.LVal
// arguments for the repository's builtin/stdlib fuzz targets.
//
// WHY A VALUE GENERATOR AND NOT MORE SOURCE-LEVEL FUZZING.  The parser,
// lexer, formatter and minifier targets (internal/fuzzseed) mutate ELPS
// SOURCE.  Everything they can reach has to be spellable in the grammar, and
// several LVal shapes are not: an LNative wrapping an arbitrary Go value, a
// multi-dimensional LArray, an LError value passed as an ordinary argument, a
// tagged-value whose user data is itself a tagged-value, a sorted-map keyed by
// a symbol rather than a string.  Those shapes DO reach builtins in practice
// -- host code hands natives in, json:load-bytes builds nested maps, deftype
// builds tagged values -- so the only way to fuzz a builtin against them is to
// construct the values directly and apply the function to them.
//
// SINGLETONS ARE DELIBERATELY IN THE CORPUS.  Value can return the shared
// Nil() / Bool(true) / Bool(false) singletons.  Any builtin that writes
// through one of those pointers corrupts every other holder of it; that is
// exactly what the `elpscheck` build tag detects (see lisp/singleton.go and
// issue #274).  Running these targets under `go test -tags elpscheck` is
// therefore a much stronger check than running them without it, and the
// harnesses additionally verify a lisp.SingletonSnapshot on every iteration so
// the default build catches it too.
//
// DETERMINISM.  A Gen is a pure function of its input bytes: the same []byte
// always yields the same values.  That is what makes a saved crasher
// reproducible.  Nothing here reads a clock, a map iteration order, or a
// random source.
package fuzzval

import (
	"math"

	"github.com/luthersystems/elps/lisp"
)

// Budget caps how many LVals one Gen will construct across all calls.  The
// generator is recursive -- a list can hold arrays that hold maps -- so
// without a global cap a few bytes can ask for an exponentially large value
// and the target spends its whole budget in the allocator rather than in the
// code under test.  Depth alone is not enough: breadth multiplies too.
const Budget = 512

// maxDepth bounds nesting independently of Budget so a pathological input
// cannot build a 500-deep spine, which would test the recursion limits of
// LVal.String() rather than the builtin under test.
const maxDepth = 6

// maxSeqLen bounds the length of any single generated sequence.
const maxSeqLen = 8

// Gen is a deterministic LVal generator driven by a byte string.
//
// It never runs out of input: reads past the end of the byte string return 0.
// A short input therefore produces a small, boring value rather than an error,
// which is what lets the fuzzer start from a one-byte seed and grow.
type Gen struct {
	env    *lisp.LEnv
	b      []byte
	i      int
	budget int
}

// New returns a Gen driven by data.
//
// env is used only to construct tagged-values (LEnv.TaggedValue is the only
// supported constructor, and it stamps a source location; hand-rolling the
// struct literal would leave Source nil, which no real tagged-value ever has
// and which would make a nil-deref in the harness look like a builtin bug).
// A nil env is allowed and simply removes tagged-values from the corpus.
func New(data []byte, env *lisp.LEnv) *Gen {
	return &Gen{b: data, env: env, budget: Budget}
}

// Byte consumes and returns one byte, or 0 once the input is exhausted.
func (g *Gen) Byte() byte {
	if g.i >= len(g.b) {
		return 0
	}
	c := g.b[g.i]
	g.i++
	return c
}

// Intn consumes one byte and returns a value in [0,n).  Returns 0 for n <= 0.
func (g *Gen) Intn(n int) int {
	if n <= 0 {
		return 0
	}
	return int(g.Byte()) % n
}

// Bytes consumes up to n bytes and returns them.  The returned slice is a
// copy: builtins are allowed to retain what they are given, and the fuzzing
// engine reuses the input buffer between iterations.
func (g *Gen) Bytes(n int) []byte {
	if n <= 0 {
		return nil
	}
	if g.i >= len(g.b) {
		return []byte{}
	}
	end := min(g.i+n, len(g.b))
	out := make([]byte, end-g.i)
	copy(out, g.b[g.i:end])
	g.i = end
	return out
}

// interestingInts are the integer values that break index and size
// arithmetic: the signed boundaries powInt's doubling loop overflows through,
// the 32-bit boundaries, the float64 exact-integer boundary, and the small
// values around zero that off-by-one bugs live on.
var interestingInts = []int{
	0, 1, -1, 2, -2, 3, 8,
	math.MaxInt, math.MinInt,
	math.MaxInt - 1, math.MinInt + 1,
	math.MaxInt32, math.MinInt32,
	1 << 53, -(1 << 53),
	1 << 62, -(1 << 62),
	1 << 31, 1 << 16,
	-9223372036854775807,
}

// interestingFloats are the float64 values with no total order and no
// round-trip: NaN (which is not equal to itself, so reflexivity must NOT be
// asserted anywhere downstream), both infinities, both zeros, and the
// subnormal/limit boundaries.
var interestingFloats = []float64{
	0, math.Copysign(0, -1), 1, -1, 0.5, -0.5,
	math.NaN(), math.Inf(1), math.Inf(-1),
	math.MaxFloat64, -math.MaxFloat64,
	math.SmallestNonzeroFloat64,
	1e308, 1e-308, 1 << 53, 9007199254740993,
}

// interestingStrings are the string values that break byte-vs-rune
// arithmetic, UTF-8 decoding, and anything that treats a string as a symbol
// or a package-qualified name.
var interestingStrings = []string{
	"", " ", "\x00", "a", "ab", "abc",
	"\xff", "a\xffb", "\xed\xa0\x80", // invalid / surrogate UTF-8
	"é", "😀", "é", // multibyte and combining
	"a:b", "a:b:c", ":", "::", "lisp:car",
	"true", "false", "nil", "()",
	"-1", "0", "9223372036854775808",
	"\n", "\t", `"`, `\`,
}

// interestingSymbols are symbol names with special meaning to the evaluator
// or to the special operators that walk caller-supplied fragments by hand.
var interestingSymbols = []string{
	"a", "b", "x", "else", "true", "false",
	"&rest", "&optional", "&key",
	"lisp:car", "lisp", ":", "", "\xff",
}

// The Value kind tags.  Ordered so the low tags are the cheap scalar shapes:
// a mutator that flips a byte down is more likely to shrink a value than to
// grow it, which keeps minimised crashers small.
const (
	kindNil = iota
	kindBoolTrue
	kindBoolFalse
	kindInt
	kindFloat
	kindString
	kindSymbol
	kindQSymbol
	kindBytes
	kindError
	kindQuote
	kindSExpr
	kindQExpr
	kindVector
	kindArrayND
	kindSortMap
	kindTagged
	kindNative
	kindFun
	kindNumKinds
)

// Value returns one generated LVal.
//
// The returned value may be a shared singleton (see the package doc); callers
// must treat every generated value as potentially shared and must not mutate
// it themselves.
func (g *Gen) Value() *lisp.LVal { return g.value(0) }

func (g *Gen) value(depth int) *lisp.LVal {
	if g.budget <= 0 {
		return lisp.Nil()
	}
	g.budget--

	kind := g.Intn(kindNumKinds)
	// Past the depth limit, collapse every compound shape onto a scalar
	// rather than truncating mid-structure: a half-built array with a
	// dimension header that disagrees with its cell count is not a value any
	// real caller can produce, and a crash on one would be a harness bug
	// reported as a builtin bug.
	if depth >= maxDepth && kind > kindQuote {
		kind = g.Intn(kindQuote)
	}

	switch kind {
	case kindNil:
		return lisp.Nil()
	case kindBoolTrue:
		return lisp.Bool(true)
	case kindBoolFalse:
		return lisp.Bool(false)
	case kindInt:
		return lisp.Int(g.pickInt())
	case kindFloat:
		return lisp.Float(g.pickFloat())
	case kindString:
		return lisp.String(g.pickString())
	case kindSymbol:
		return lisp.Symbol(g.pickSymbol())
	case kindQSymbol:
		return lisp.QSymbol(g.pickSymbol())
	case kindBytes:
		return lisp.Bytes(g.Bytes(g.Intn(maxSeqLen + 1)))
	case kindError:
		// An LError is an ordinary first-class value in ELPS; handler-bind
		// hands one to a user function, which can then pass it anywhere.
		return lisp.ErrorConditionf("fuzz-condition", "%s", g.pickString())
	case kindQuote:
		// lisp.Quote only produces an LQuote node when its operand is ALREADY
		// quoted -- one level of quoting is just the Quoted flag on the value
		// itself (''3, not '3). Quoting twice is the only way to reach the
		// LQuote type at all, so half the corpus does.
		v := lisp.Quote(g.value(depth + 1))
		if g.Byte()&1 == 0 {
			v = lisp.Quote(v)
		}
		return v
	case kindSExpr:
		return lisp.SExpr(g.cells(depth))
	case kindQExpr:
		return lisp.QExpr(g.cells(depth))
	case kindVector:
		return lisp.Vector(g.cells(depth))
	case kindArrayND:
		return g.arrayND(depth)
	case kindSortMap:
		return g.sortMap(depth)
	case kindTagged:
		return g.tagged(depth)
	case kindNative:
		return g.native()
	case kindFun:
		return g.fun(depth)
	}
	return lisp.Nil()
}

// funNames are the stdlib callables handed in as first-class function
// arguments.  Higher-order builtins (map, foldl, apply, funcall, sort,
// compose, flip, curry) are the ones that call back into an argument, so
// giving the generator real callables reaches their argument-mismatch paths
// rather than only their "not a function" rejection.
var funNames = []string{
	"lisp:car", "lisp:cdr", "lisp:identity", "lisp:not", "lisp:+",
	"lisp:length", "lisp:to-string", "lisp:type", "lisp:error",
	"lisp:list", "lisp:apply", "lisp:map",
}

// fun returns a first-class function value.
//
// THREE DISTINCT SHAPES, because callers discriminate between them and get it
// wrong:
//
//   - a Go builtin (Builtin != nil, Cells = [formals, docstring]);
//   - a user lambda (Builtin == nil, Cells = [formals, body...]);
//   - an existing stdlib callable, including special operators and macros,
//     which several builtins must reject rather than invoke.
//
// libschema's constraint calling convention reaches into the raw Builtin closure
// directly, so shape 1 with mismatched arity indexes past the end of an empty
// args list and shape 2 dereferences a nil Builtin.  Both are only reachable
// with an LFun in the corpus.
func (g *Gen) fun(depth int) *lisp.LVal {
	switch g.Intn(4) {
	case 0:
		// A Go builtin. Deliberately declares one formal and ignores its
		// arguments: a caller that invokes Builtin directly, bypassing bind,
		// gets whatever list it passed, including an empty one.
		return lisp.FunInPackage("user", "fuzz-builtin", lisp.Formals("x"),
			func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
				if len(args.Cells) == 0 {
					return lisp.Nil()
				}
				return args.Cells[0]
			})
	case 1:
		if g.env == nil {
			return lisp.Nil()
		}
		// A user lambda: Builtin is nil.
		fn := g.env.Lambda(lisp.Formals("x"), []*lisp.LVal{g.value(depth + 1)})
		if fn.Type == lisp.LError {
			return lisp.Nil()
		}
		return fn
	case 2:
		if g.env == nil {
			return lisp.Nil()
		}
		fn := g.env.GetGlobal(lisp.Symbol(funNames[g.Intn(len(funNames))]))
		if fn.Type != lisp.LFun {
			return lisp.Nil()
		}
		return fn
	default:
		if g.env == nil {
			return lisp.Nil()
		}
		// A special operator or macro in value position. `let` and `defmacro`
		// are LFun values that FunCall must refuse rather than invoke.
		fn := g.env.GetGlobal(lisp.Symbol([]string{"lisp:let", "lisp:quote", "lisp:defmacro", "lisp:cond"}[g.Intn(4)]))
		if fn.Type != lisp.LFun {
			return lisp.Nil()
		}
		return fn
	}
}

func (g *Gen) cells(depth int) []*lisp.LVal {
	n := g.Intn(maxSeqLen)
	cells := make([]*lisp.LVal, 0, n)
	for range n {
		if g.budget <= 0 {
			break
		}
		cells = append(cells, g.value(depth+1))
	}
	return cells
}

func (g *Gen) pickInt() int {
	if g.Byte()&1 == 0 {
		return interestingInts[g.Intn(len(interestingInts))]
	}
	// A small arbitrary value, so the corpus is not only the boundary
	// constants.  Sign taken from the same byte to keep the read count fixed.
	v := int(g.Byte())
	if g.Byte()&1 == 0 {
		return -v
	}
	return v
}

func (g *Gen) pickFloat() float64 {
	if g.Byte()&1 == 0 {
		return interestingFloats[g.Intn(len(interestingFloats))]
	}
	return float64(int8(g.Byte())) / 8
}

func (g *Gen) pickString() string {
	if g.Byte()&1 == 0 {
		return interestingStrings[g.Intn(len(interestingStrings))]
	}
	return string(g.Bytes(g.Intn(24)))
}

func (g *Gen) pickSymbol() string {
	if g.Byte()&1 == 0 {
		return interestingSymbols[g.Intn(len(interestingSymbols))]
	}
	return string(g.Bytes(g.Intn(8)))
}

// arrayND builds a multi-dimensional array.  lisp.Array validates that the
// dimension header multiplies out to the cell count, so the cell slice is
// sized from the dims rather than generated independently; an array whose
// header lies is not constructible through any lisp-visible path and would
// only test lisp.Array's own guard.
func (g *Gen) arrayND(depth int) *lisp.LVal {
	ndim := 1 + g.Intn(3)
	dims := make([]*lisp.LVal, 0, ndim)
	total := 1
	for range ndim {
		d := g.Intn(4)
		dims = append(dims, lisp.Int(d))
		total *= d
	}
	if total > maxSeqLen {
		total = 0
		dims = []*lisp.LVal{lisp.Int(0)}
	}
	cells := make([]*lisp.LVal, 0, total)
	for range total {
		cells = append(cells, g.value(depth+1))
	}
	arr := lisp.Array(lisp.QExpr(dims), cells)
	if arr.Type == lisp.LError {
		// Fall back rather than smuggling an LError in under an array tag:
		// the caller asked for an array-shaped value.
		return lisp.Vector(nil)
	}
	return arr
}

// sortMap builds a sorted-map.  Keys are strings and symbols in a mix,
// because sortedmap.Set accepts both and stores them under a keytype
// discriminator -- code that reads back a key and assumes LString is wrong,
// and only a symbol-keyed map exposes it.
func (g *Gen) sortMap(depth int) *lisp.LVal {
	m := lisp.SortedMap()
	n := g.Intn(maxSeqLen)
	for range n {
		if g.budget <= 0 {
			break
		}
		var key *lisp.LVal
		if g.Byte()&1 == 0 {
			key = lisp.String(g.pickString())
		} else {
			key = lisp.Symbol(g.pickSymbol())
		}
		if lerr := m.Map().Set(key, g.value(depth+1)); lerr.Type == lisp.LError {
			continue
		}
	}
	return m
}

// tagged builds a tagged-value.  Tagged values are what deftype/new produce,
// and their user data is itself an arbitrary LVal, so a builtin that unwraps
// one and assumes a shape is reachable from ordinary lisp.
func (g *Gen) tagged(depth int) *lisp.LVal {
	if g.env == nil {
		return g.value(depth + 1)
	}
	typ := g.pickSymbol()
	if typ == "" {
		typ = "user:fuzz-type"
	}
	v := g.env.TaggedValue(lisp.Symbol(typ), g.value(depth+1))
	if v.Type == lisp.LError {
		return lisp.Nil()
	}
	return v
}

// nativeValues are the Go values an LNative can wrap.  Host applications
// embed ELPS and hand natives across the boundary, so a builtin that type
// switches on Native without a default is reachable from real code even
// though no lisp source can spell these.
func (g *Gen) native() *lisp.LVal {
	switch g.Intn(6) {
	case 0:
		return lisp.Native(nil)
	case 1:
		return lisp.Native(struct{}{})
	case 2:
		return lisp.Native(g.pickString())
	case 3:
		return lisp.Native(g.pickInt())
	case 4:
		return lisp.Native([]byte(g.pickString()))
	default:
		return lisp.Native(map[string]int{"a": 1})
	}
}

// Seeds returns the shared seed corpus for the value-driven targets.
//
// A coverage-guided fuzzer descends from the seeds it is given, so the seeds
// are chosen to land on the interesting kind tags immediately rather than to
// be pretty: each entry is a short byte string whose leading bytes select a
// kind and whose remainder feeds that kind's own reads.
func Seeds() [][]byte {
	seeds := [][]byte{
		{},
		{0},
		{1}, {2},
	}
	// One seed per kind tag, so the very first generation already covers
	// every value shape rather than discovering them by mutation.
	for k := range byte(kindNumKinds) {
		seeds = append(seeds,
			[]byte{k, 0, 0, 0, 0, 0, 0, 0},
			[]byte{k, 1, 2, 3, 4, 5, 6, 7},
			[]byte{k, 0xff, 0xfe, 0xfd, 0xfc, 0xfb, 0xfa, 0xf9},
		)
	}
	// Deep and wide shapes: repeated compound tags nest, repeated scalar
	// tags widen.
	seeds = append(seeds,
		[]byte{kindSExpr, 8, kindSExpr, 8, kindSExpr, 8, kindSExpr, 8, kindSExpr, 8},
		[]byte{kindArrayND, 3, 2, 2, 2, kindInt, 0, 1},
		[]byte{kindSortMap, 8, 0, kindString, 0, 1, 0, kindSymbol, 0, 1},
		[]byte{kindTagged, 0, 0, kindTagged, 0, 0, kindTagged, 0, 0},
		[]byte{kindNative, 0}, []byte{kindNative, 5},
		[]byte{kindFun, 0}, []byte{kindFun, 1}, []byte{kindFun, 2}, []byte{kindFun, 3},
		[]byte{kindInt, 0, 7},   // math.MaxInt
		[]byte{kindInt, 0, 8},   // math.MinInt
		[]byte{kindFloat, 0, 6}, // NaN
	)
	return seeds
}
