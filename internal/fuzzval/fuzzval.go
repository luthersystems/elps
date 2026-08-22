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
	"encoding/json"
	"math"
	"regexp"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
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
	// nloc counts the real source locations handed out, so each is distinct
	// and so the whole assignment stays a pure function of the input bytes.
	nloc int
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

	// One byte carries two independent decisions. The kind comes from it
	// modulo the number of kinds, and the source-location decision from its
	// top bit -- which that modulo leaves essentially unused, and every kind
	// is reachable with the bit both set and clear.
	//
	// Two decisions from one byte rather than two bytes so that adding the
	// second decision does not shift the byte stream: a saved crasher keeps
	// selecting the values it selected when it was saved. A regression corpus
	// that silently starts testing something else is worse than no regression
	// corpus.
	sel := g.Byte()
	kind := int(sel) % kindNumKinds
	// Past the depth limit, collapse every compound shape onto a scalar
	// rather than truncating mid-structure: a half-built array with a
	// dimension header that disagrees with its cell count is not a value any
	// real caller can produce, and a crash on one would be a harness bug
	// reported as a builtin bug.
	if depth >= maxDepth && kind > kindQuote {
		kind = g.Intn(kindQuote)
	}

	return g.locate(g.construct(kind, depth), sel)
}

// locate gives a generated value a REAL source location about half the time.
//
// WHY.  Before this, every generated value carried lisp.nativeSource's shared
// synthetic Location (Pos < 0), and the only write the interpreter makes to
// Source -- stampMacroExpansion -- is guarded by `Source == nil || Source.Pos
// < 0`.  A corpus of exclusively synthetic values therefore never presents the
// case that a corrupting builtin would have to be caught on, and
// internal/fuzzfp's Source rule has nothing to bite on.
//
// It is also the more faithful model, not merely the more testable one.  Every
// callable that receives an UNEVALUATED fragment -- every special operator,
// every macro -- receives it straight from the reader, so in production those
// nodes all carry real, distinct, per-node locations.  Synthetic locations are
// what a computed value has.  Generating a mix covers both.
//
// Locations come from a fixed process-wide pool rather than being allocated
// per node, for two reasons.
//
//   - Reproducibility.  LVal.String() renders an LQSymbol with %#v, which
//     prints the Source POINTER.  A freshly allocated Location per node would
//     therefore make the rendering of a generated value depend on the
//     allocator, and TestGeneratorIsDeterministic -- the property every saved
//     crasher rests on -- would fail.
//   - Fidelity.  LVal.Source documents itself as shared: "the reference may be
//     shared by multiple LVals".  A pool models that, and it is the sharing
//     that gives an in-place edit of a Location its real blast radius.
//
// NOT applied to the singletons (mutating one corrupts every holder -- see
// issue #274) or to LFun values (kinds 2 and 3 of fun() return the
// environment's own global function objects; writing to one would corrupt the
// environment rather than test a builtin).
func (g *Gen) locate(v *lisp.LVal, sel byte) *lisp.LVal {
	if sel&0x80 == 0 || v == nil {
		return v
	}
	if v.Type == lisp.LFun || v == lisp.Nil() || v == lisp.Bool(true) || v == lisp.Bool(false) {
		return v
	}
	// SetSource, not a field write: #362 unexported the field behind an
	// audited setter.
	v.SetSource(realLocations[g.nloc%len(realLocations)])
	g.nloc++
	return v
}

// realLocations is the pool locate draws from: distinct, REAL (Pos >= 0)
// locations of the shape the reader produces.
//
// Shared across iterations on purpose, in the same spirit as feeding the
// singletons in: if a builtin writes through a Source pointer, the value it
// wrote is visible to the guard that recorded it.
var realLocations = func() []*token.Location {
	const n = 64
	locs := make([]*token.Location, n)
	for i := range locs {
		locs[i] = &token.Location{
			File: "fuzz", Path: "fuzz",
			Pos: i, Line: 1 + i/16, Col: 1 + i%16,
			EndPos: i + 1, EndLine: 1 + i/16, EndCol: 2 + i%16,
		}
	}
	return locs
}()

func (g *Gen) construct(kind, depth int) *lisp.LVal {
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

// fuzzDurations are the durations handed to time:sleep and the rest of
// libtime.
//
// Bounded deliberately.  BuiltinSleep refuses a duration over an hour and one
// that would outlast the evaluation deadline, both instantly -- but anything
// in between it actually sleeps for, and the fuzz environment's deadline is 5
// seconds.  A generated 4-second duration would therefore cost four seconds of
// the target's budget for one input.  The values here cover the refusal paths
// (negative, over-the-hour, saturated) at zero cost and the SLEEPING path at a
// worst case of two milliseconds, which is what the tracker recorded as
// missing: "fuzzval has no time.Duration among its nativeValues, so generated
// arguments reach the builtin's type check and stop there".
var fuzzDurations = []time.Duration{
	0,
	1,
	-1,
	time.Millisecond,
	2 * time.Millisecond,
	-time.Hour,
	2 * time.Hour, // over BuiltinSleep's ceiling: refused, no wait
	math.MaxInt64,
	math.MinInt64,
}

// fuzzPatterns are compiled to *regexp.Regexp, which is what libregexp hands
// back to lisp and therefore what a later builtin can receive.
var fuzzPatterns = []string{
	``, `a`, `(a|b)+`, `^$`, `\p{L}{2,3}`, `(?i)x(y)?z`, `.*`, `[^\x00-\x7f]`,
}

// native returns an LNative.
//
// TWO POPULATIONS, for two different reasons.
//
// The first is the embedder's: nil, an empty struct, a string, an int, a byte
// slice, a map.  Host applications embed ELPS and hand natives across the
// boundary, so a builtin that type switches on Native without a default is
// reachable from real code even though no lisp source can spell these.
//
// The second is the standard library's own: time.Time, time.Duration,
// *regexp.Regexp and json.RawMessage are all produced by libtime, libregexp
// and libjson and handed straight back to lisp, so a phylum can obtain one
// from one builtin and pass it to any other.  Before these were here, the only
// natives in the corpus were shapes no stdlib function had a branch for, so
// every one of them stopped at a type check.  They are also what gives
// internal/fuzzfp's LNative rule something to guard: a *regexp.Regexp is a
// compiled program a few hundred nodes deep and a time.Time carries a pointer
// to a shared *time.Location, so "a builtin mutated a native's internals" is a
// reachable defect rather than a hypothetical one.
// nativeNumKinds is the number of shapes native() can build. Named so the
// seed corpus and the switch cannot drift apart silently.
const nativeNumKinds = 10

func (g *Gen) native() *lisp.LVal {
	switch g.Intn(nativeNumKinds) {
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
	case 5:
		return lisp.Native(map[string]int{"a": 1})
	case 6:
		// Deterministic instants only: time.Now() would make a saved crasher
		// irreproducible, which is the one property a regression corpus has to
		// have.  UTC for the same reason -- the process's local zone is not an
		// input to the fuzzer.
		return lisp.Native(time.Unix(int64(g.pickInt()), int64(g.Intn(1000))).UTC())
	case 7:
		return lisp.Native(fuzzDurations[g.Intn(len(fuzzDurations))])
	case 8:
		// Compiled fresh rather than shared from a package-level table: a
		// builtin that mutated a shared *regexp.Regexp would corrupt every
		// LATER iteration instead of failing the one that did it, and a
		// crasher that only reproduces after its predecessors is not a
		// crasher.
		re, err := regexp.Compile(fuzzPatterns[g.Intn(len(fuzzPatterns))])
		if err != nil {
			return lisp.Native(nil)
		}
		return lisp.Native(re)
	default:
		return lisp.Native(json.RawMessage(g.pickString()))
	}
}

// KindSeeds returns exactly one seed per value kind.
//
// Seeds() is a grab-bag sized for the mutator to descend from; this is the
// smaller, structural list a caller uses when it needs the GUARANTEE that
// every value shape is represented -- for instance a target crossing seeds
// against callables, which must not leave a builtin having seen only six of
// the nineteen shapes because six was the sample size.
//
// It is derived from kindNumKinds rather than written out, so a kind added to
// value() is covered by every such caller without anyone remembering to
// update a list.
func KindSeeds() [][]byte {
	seeds := make([][]byte, 0, kindNumKinds)
	for k := range byte(kindNumKinds) {
		seeds = append(seeds, []byte{k, 1, 2, 3, 4, 5, 6, 7})
	}
	return seeds
}

// LocatedKindSeeds is KindSeeds with the real-source-location bit set: one
// seed per kind, each producing a value carrying a REAL token.Location rather
// than the shared synthetic one.
//
// Kept separate from KindSeeds because the two populations cost the same and
// buy different things. Measured, crossing the located population against
// every callable moved statement coverage of lisp/lisplib not at all -- a
// location does not steer control flow -- while roughly doubling what the seed
// corpus costs on every `go test`. What it buys instead is the ONLY population
// internal/fuzzfp's "a real location is never replaced" rule can bite on, so a
// caller should spend it where that matters rather than everywhere.
func LocatedKindSeeds() [][]byte {
	seeds := make([][]byte, 0, kindNumKinds)
	for k := range byte(kindNumKinds) {
		seeds = append(seeds, []byte{stamped(k), 1, 2, 3, 4, 5, 6, 7})
	}
	return seeds
}

// stamped returns a selector byte that chooses kind k AND sets the
// real-source-location bit -- the smallest byte >= 0x80 congruent to k modulo
// kindNumKinds. Setting the top bit of k directly would not do: the bit is
// part of the value the modulus is taken of, so `k | 0x80` selects a different
// kind entirely.
func stamped(k byte) byte {
	b := int(k)
	for b < 0x80 {
		b += kindNumKinds
	}
	return byte(b)
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
			// The same kind with a REAL source location (see Gen.locate: the
			// selector's top bit). Reaching the located population by mutation
			// means flipping one specific bit of one specific byte, which for
			// a 19-way modulus is a needle the mutator has no reason to look
			// for.
			[]byte{stamped(k), 0, 0, 0, 0, 0, 0, 0},
			[]byte{stamped(k), 1, 2, 3, 4, 5, 6, 7},
		)
	}
	// Deep and wide shapes: repeated compound tags nest, repeated scalar
	// tags widen.
	seeds = append(seeds,
		[]byte{kindSExpr, 8, kindSExpr, 8, kindSExpr, 8, kindSExpr, 8, kindSExpr, 8},
		[]byte{kindArrayND, 3, 2, 2, 2, kindInt, 0, 1},
		[]byte{kindSortMap, 8, 0, kindString, 0, 1, 0, kindSymbol, 0, 1},
		[]byte{kindTagged, 0, 0, kindTagged, 0, 0, kindTagged, 0, 0},
		[]byte{kindFun, 0}, []byte{kindFun, 1}, []byte{kindFun, 2}, []byte{kindFun, 3},
		[]byte{kindInt, 0, 7},   // math.MaxInt
		[]byte{kindInt, 0, 8},   // math.MinInt
		[]byte{kindFloat, 0, 6}, // NaN
	)
	// One seed per NATIVE shape. The kind tag alone is not enough: native()
	// switches again on its own byte, so a corpus carrying only
	// {kindNative, 0} and {kindNative, 5} reached two of ten shapes and left
	// the rest -- including every type the standard library itself produces --
	// to be found by mutation. TestGeneratorProducesStdlibNativeTypes fails if
	// this drifts out of step with native().
	for k := range byte(nativeNumKinds) {
		seeds = append(seeds,
			[]byte{kindNative, k},
			[]byte{kindNative, k, 1, 2, 3, 4},
			[]byte{stamped(kindNative), k, 1, 2, 3, 4},
		)
	}
	return seeds
}
