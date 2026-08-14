// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"fmt"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// The aliasing battery for issue #395.
//
// The non-mutating operations (?set, ?del, ?nil) are documented to "return a
// copy and leave the original unchanged".  #395 is that the copy shared its
// nested containers with the source, so a write through the copy reached the
// original.  Equality cannot see that: a sharing copy and an independent one
// print the same and compare equal, which is why the bug was silent in every
// layer of the suite.  Only a write distinguishes them.
//
// So this battery writes.  For every copy operation at every path into a
// nested document, it then mutates the returned copy at every path into the
// same document -- on the copy path, off it at each level, above it and below
// it -- and requires the source to be byte-identical afterwards.  It also
// re-queries the copy first and mutates what the query hands back, which is
// the reachability a spine-only copy has to answer for: an off-path subtree
// shared with the source is one (elpspath:? copy "address") away from a
// caller holding the source's own container.
//
// Red-proof: TestAliasBatteryDetectsSharing below rebuilds the pre-#395
// shallow map copy and runs the same battery over it, requiring it to fail.
// A battery that cannot fail is not evidence.
//
// A battery is also not evidence about code its paths cannot reach.  Until
// enumeratePaths grew spans, the only steps it emitted were map keys and
// integer indices, so rangePath -- every operation's splice, and the getter
// whose result slices the source's own backing array -- was outside the
// sweep entirely.  Adding spans took the battery from 8,007 written-through
// combinations to 117,633, and the red-proof from 1,044 failures to 2,184.
// TestEnumeratePathsCoversEveryStepForm keeps that from silently regressing.

// aliasDoc is the document the battery walks: sorted-maps, vectors and a
// quoted list, nested three container levels deep, with several siblings at
// every level so that a copy has somewhere off-path to get wrong.
func aliasDoc() *lisp.LVal {
	lines := lisp.Vector([]*lisp.LVal{lisp.String("10 Downing"), lisp.String("SW1")})
	addr := lisp.SortedMap()
	addr.MapSet("city", lisp.String("London"))
	addr.MapSet("lines", lines)

	item := func(id int, tags ...string) *lisp.LVal {
		cells := make([]*lisp.LVal, len(tags))
		for i, t := range tags {
			cells[i] = lisp.String(t)
		}
		m := lisp.SortedMap()
		m.MapSet("id", lisp.Int(id))
		m.MapSet("tags", lisp.Vector(cells))
		return m
	}

	doc := lisp.SortedMap()
	doc.MapSet("ssn", lisp.String("123"))
	doc.MapSet("count", lisp.Int(7))
	doc.MapSet("address", addr)
	doc.MapSet("items", lisp.Vector([]*lisp.LVal{item(1, "a", "b"), item(2, "c")}))
	doc.MapSet("notes", lisp.QExpr([]*lisp.LVal{lisp.String("n1"), lisp.String("n2")}))
	return doc
}

// aliasListDoc is a list-rooted source: the same shapes hung off a quoted
// list rather than a map, so the battery covers the sequence helpers as
// roots and not only as nested values.
func aliasListDoc() *lisp.LVal {
	inner := lisp.SortedMap()
	inner.MapSet("k", lisp.String("v"))
	inner.MapSet("deep", lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}))
	return lisp.QExpr([]*lisp.LVal{
		lisp.String("head"),
		inner,
		lisp.Vector([]*lisp.LVal{lisp.String("x"), lisp.String("y")}),
	})
}

// aliasVectorDoc is the same again rooted at a vector.
func aliasVectorDoc() *lisp.LVal {
	inner := lisp.SortedMap()
	inner.MapSet("k", lisp.String("v"))
	inner.MapSet("deep", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}))
	return lisp.Vector([]*lisp.LVal{
		lisp.String("head"),
		inner,
		lisp.Vector([]*lisp.LVal{lisp.String("x"), lisp.String("y")}),
	})
}

var aliasSources = map[string]func() *lisp.LVal{
	"map":    aliasDoc,
	"list":   aliasListDoc,
	"vector": aliasVectorDoc,
}

// enumeratePaths lists every step sequence of length 1..maxDepth that names
// a value inside v: map keys as strings, sequence positions as ints, and
// spans of a sequence as (range a b).  The battery copies at each of these
// and mutates at each of these, so on-path, off-path and cross-depth
// combinations are covered by construction rather than by a hand-picked list
// that can miss the interesting one.
//
// RANGE STEPS were missing until they were added here, and their absence was
// a hole rather than an omission: this enumerator is the ONLY source of paths
// the battery uses, so with only keys and indices in it, rangePath was
// unreachable from the battery entirely.  Every aliasing question a range
// raises -- rangePath.Get hands back a slice over the source's OWN backing
// array (issues #369/#373), and the splice rebuilds a sequence around a span
// -- was therefore outside the one test in the package whose job is to catch
// a write through a copy reaching the source.  The range-splice corruption
// lived in exactly that unreachable code.
//
// The spans are a fixed small set per sequence rather than all O(n^2) of
// them, chosen so that each one is a splice shape the others are not:
//
//	(0,n)  the whole sequence -- no prefix and no tail
//	(0,0)  an insertion point -- replaces nothing, tail is everything
//	(0,1)  anchored at the front WITH a tail, which is the shape a
//	       replacement longer than its span corrupts: the tail is the part
//	       the splice must not overwrite before it reads it
//	(1,2)  a span with elements on BOTH sides, the only one that exercises a
//	       non-zero `from` and a non-empty tail together
//
// Enumerating every span instead multiplies the battery's already-quadratic
// sweep for shapes that repeat.
func enumeratePaths(v *lisp.LVal, maxDepth int) [][]*lisp.LVal {
	if maxDepth == 0 {
		return nil
	}
	var out [][]*lisp.LVal
	eachBelow := func(step *lisp.LVal, child *lisp.LVal, below int) {
		out = append(out, []*lisp.LVal{step})
		for _, sub := range enumeratePaths(child, below) {
			out = append(out, append([]*lisp.LVal{step}, sub...))
		}
	}
	each := func(step *lisp.LVal, child *lisp.LVal) {
		eachBelow(step, child, maxDepth-1)
	}
	// eachSeq enumerates a sequence's positions and its spans.  cells is the
	// sequence's own cell slice; wrap rebuilds a span into a value of the
	// same layout, which is what a range step's result is and so what the
	// enumeration below the step has to walk.
	eachSeq := func(cells []*lisp.LVal, wrap func([]*lisp.LVal) *lisp.LVal) {
		for i, c := range cells {
			each(lisp.Int(i), c)
		}
		for _, span := range enumerateSpans(len(cells)) {
			sub := make([]*lisp.LVal, span[1]-span[0])
			copy(sub, cells[span[0]:span[1]])
			eachBelow(rangeStep(span[0], span[1]), wrap(sub), min(maxDepth-1, spanSubDepth))
		}
	}
	switch v.Type { //nolint:exhaustive // the container types; everything else is a leaf with nothing below it
	case lisp.LSortMap:
		m := v.Map()
		entries := make([]*lisp.LVal, m.Len())
		if lerr := m.Entries(entries); lerr.Type == lisp.LError {
			panic(fmt.Sprintf("map entries: %v", lerr))
		}
		for _, ent := range entries {
			each(lisp.String(ent.Cells[0].Str), ent.Cells[1])
		}
	case lisp.LArray:
		eachSeq(v.Cells[1].Cells, toVector)
	case lisp.LSExpr:
		eachSeq(v.Cells, toList)
	}
	return out
}

// spanSubDepth bounds how far the enumeration goes BELOW a range step.
//
// One step is what the aliasing question needs and the whole battery can
// afford.  A range's result is a sequence the operation synthesised, so the
// route that matters is "query the copy at a span, then write through an
// element of what comes back" -- which is one step below the span, and is
// precisely the reachability rangePath.Get's two-index slice over the
// source's own backing array (#369/#373) would open.  Letting the sweep
// recurse the full maxDepth below every span instead multiplies both path
// sets in an already-quadratic battery: measured, it takes the run from 15s
// to a minute under -race for combinations that repeat the same shapes
// deeper down.
const spanSubDepth = 1

// enumerateSpans is the fixed span set described above, deduplicated and
// kept in a stable order so the battery's combination count is reproducible.
func enumerateSpans(n int) [][2]int {
	candidates := [][2]int{{0, n}, {0, 0}, {0, 1}, {1, 2}}
	seen := map[[2]int]bool{}
	var out [][2]int
	for _, c := range candidates {
		if c[0] < 0 || c[1] > n || c[0] > c[1] || seen[c] {
			continue
		}
		seen[c] = true
		out = append(out, c)
	}
	return out
}

func pathString(steps []*lisp.LVal) string {
	parts := make([]string, len(steps))
	for i, s := range steps {
		parts[i] = s.String()
	}
	return "[" + strings.Join(parts, " ") + "]"
}

// callBuiltin runs one of the package builtins the way lisp does, returning
// the result.  A panic is turned into an error value: the battery is looking
// for aliasing, and a panic partway through an operation would otherwise take
// the whole run down before it could report which case found it.
func callBuiltin(env *lisp.LEnv, fn func(*lisp.LEnv, *lisp.LVal) *lisp.LVal, args ...*lisp.LVal) (res *lisp.LVal) {
	defer func() {
		if r := recover(); r != nil {
			res = env.Errorf("panic: %v", r)
		}
	}()
	return fn(env, lisp.QExpr(args))
}

func withSteps(head *lisp.LVal, steps []*lisp.LVal, tail ...*lisp.LVal) []*lisp.LVal {
	args := make([]*lisp.LVal, 0, 1+len(steps)+len(tail))
	args = append(args, head)
	args = append(args, steps...)
	args = append(args, tail...)
	return args
}

// copyOps are the three non-mutating builtins under test, each with the
// extra trailing arguments its signature needs.
var copyOps = []struct {
	name string
	fn   func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
	tail []*lisp.LVal
}{
	{"?set", BuiltinQuerySet, []*lisp.LVal{lisp.String("COPYSET")}},
	{"?del", BuiltinQueryDelete, nil},
	{"?nil", BuiltinQueryNil, nil},
}

// sharingCopyOps are the same three operations built out of a copy that
// rebuilds only the node the path names and stores every child by reference.
//
// That is two things at once.  It is bit for bit what copyMap did before
// issue #395 -- (?nil patient "ssn") rebuilt the patient map and shared the
// address map hanging off it.  And it is what a fully path-aware copy
// produces for a one-step path: the spine is the root alone, so everything
// else is off-path and shared.  Running the battery over it is therefore
// both the red-proof that the battery can fail, and the measurement of what
// sharing off-path subtrees actually costs in reachability.
var sharingCopyOps = []struct {
	name string
	fn   func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
	tail []*lisp.LVal
}{
	{"?set/sharing", sharingCopyBuiltin(BuiltinQuerySetMutate), []*lisp.LVal{lisp.String("COPYSET")}},
	{"?del/sharing", sharingCopyBuiltin(BuiltinQueryDeleteMutate), nil},
	{"?nil/sharing", sharingCopyBuiltin(BuiltinQueryNilMutate), nil},
}

// sharingCopyBuiltin turns an in-place builtin into the copying builtin it
// would be if the copy shared everything below the node it rebuilds.
func sharingCopyBuiltin(mutate func(*lisp.LEnv, *lisp.LVal) *lisp.LVal) func(*lisp.LEnv, *lisp.LVal) *lisp.LVal {
	return func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		cp := sharingCopy(args.Cells[0])
		if cp == nil {
			return env.Errorf("not a container")
		}
		return mutate(env, lisp.QExpr(append([]*lisp.LVal{cp}, args.Cells[1:]...)))
	}
}

// sharingCopy rebuilds v's own node and stores v's children by reference.
func sharingCopy(v *lisp.LVal) *lisp.LVal {
	switch v.Type {
	case lisp.LSortMap:
		m0 := v.Map()
		entries := sortedMapEntries(m0)
		if entries.Type == lisp.LError {
			return nil
		}
		sm := lisp.SortedMap()
		m := sm.Map()
		for _, pair := range entries.Cells {
			m.Set(pair.Cells[0], pair.Cells[1])
		}
		return sameQuoting(v, sm)
	case lisp.LArray:
		if v.Cells[0].Len() > 1 {
			return nil
		}
		cells := make([]*lisp.LVal, len(v.Cells[1].Cells))
		copy(cells, v.Cells[1].Cells)
		return sameQuoting(v, toVector(cells))
	case lisp.LSExpr:
		cells := make([]*lisp.LVal, len(v.Cells))
		copy(cells, v.Cells)
		return sameQuoting(v, toList(cells))
	default:
		return nil
	}
}

// mutOps are the in-place builtins the battery writes through the copy with.
var mutOps = []struct {
	name string
	fn   func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
	tail []*lisp.LVal
}{
	{"?set!", BuiltinQuerySetMutate, []*lisp.LVal{lisp.String("MUTATED")}},
	{"?del!", BuiltinQueryDeleteMutate, nil},
	{"?nil!", BuiltinQueryNilMutate, nil},
}

// aliasFailure is one (source, copy op, copy path, mutate op, mutate path,
// re-query split) case in which writing through the copy changed the source.
type aliasFailure struct {
	source   string
	copyOp   string
	copyPath string
	mutOp    string
	mutPath  string
	split    int
	before   string
	after    string
}

func (f aliasFailure) String() string {
	via := ""
	if f.split > 0 {
		via = fmt.Sprintf(" re-queried at split %d;", f.split)
	}
	return fmt.Sprintf("(%s %s) copy of a %s, then (%s copy %s);%s source changed\n  before: %s\n  after:  %s",
		f.copyOp, f.copyPath, f.source, f.mutOp, f.mutPath, via, f.before, f.after)
}

// aliasStrategy is a set of copying operations to sweep.  The battery runs
// the package's real ones; the red-proof runs sharing ones.
type aliasStrategy = []struct {
	name string
	fn   func(*lisp.LEnv, *lisp.LVal) *lisp.LVal
	tail []*lisp.LVal
}

// runAliasBattery is the battery itself, factored out so the red-proof can
// run the identical sweep against a deliberately sharing copy.
//
// For every source shape, every copy operation at every path of at most
// copyDepth steps, and every mutating operation at every path, it writes
// through the copy and reports every case in which the source moved.  Each
// case additionally re-queries the copy at each prefix of the mutation path
// and writes through what the query returns, which is how a caller reaches a
// subtree the copy shares.
func runAliasBattery(t *testing.T, ops aliasStrategy, copyDepth, maxDepth int) (map[string]int, []aliasFailure) {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil
	var failures []aliasFailure
	// A battery whose copies all error is a battery that writes nowhere and
	// passes for the wrong reason, which is how the first draft of this file
	// passed: a value the type gate refuses is skipped, and the map source
	// was being skipped whole.  Every case that got as far as writing through
	// a copy is counted, and the callers floor the count.
	ran := map[string]int{}

	for srcName, mk := range aliasSources {
		paths := enumeratePaths(mk(), maxDepth)
		copyPaths := enumeratePaths(mk(), copyDepth)
		for _, cop := range ops {
			for _, cpath := range copyPaths {
				for _, mop := range mutOps {
					for _, mpath := range paths {
						// split 0 writes straight through the copy;
						// split k>0 re-queries the copy at the first k
						// steps and writes through the result.
						for split := range mpath {
							src := mk()
							before := src.String()

							cp := callBuiltin(env, cop.fn, withSteps(src, cpath, cop.tail...)...)
							if cp.Type == lisp.LError {
								// A path the operation refuses is not a
								// copy to write through.
								continue
							}
							if got := src.String(); got != before {
								failures = append(failures, aliasFailure{
									source: srcName, copyOp: cop.name, copyPath: pathString(cpath),
									mutOp: "(none)", mutPath: "", before: before, after: got,
								})
								continue
							}

							target := cp
							if split > 0 {
								target = callBuiltin(env, BuiltinQueryGet, withSteps(cp, mpath[:split])...)
								if target.Type == lisp.LError {
									continue
								}
							}
							ran[srcName]++
							callBuiltin(env, mop.fn, withSteps(target, mpath[split:], mop.tail...)...)

							if got := src.String(); got != before {
								failures = append(failures, aliasFailure{
									source: srcName, copyOp: cop.name, copyPath: pathString(cpath),
									mutOp: mop.name, mutPath: pathString(mpath), split: split,
									before: before, after: got,
								})
							}
						}
					}
				}
			}
		}
	}
	return ran, failures
}

// requireBatteryWrote fails when a source shape was skipped rather than
// exercised.  A value the type gate refuses is skipped silently, which is how
// the first draft of this file passed: one malformed quoted list in the map
// document had the whole map source erroring out at the gate.
func requireBatteryWrote(t *testing.T, ran map[string]int, floor int, sources ...string) {
	t.Helper()
	for _, name := range sources {
		require.Greater(t, ran[name], floor,
			"the battery wrote through only %d copies of the %s source: "+
				"it is passing because it is not writing", ran[name], name)
	}
}

// TestEnumeratePathsCoversEveryStepForm is the battery's own gate, in the
// spirit of TestPathGenCoverage: enumeratePaths is the ONLY source of paths
// the battery has, so a step form missing from it is a whole family of the
// engine the battery silently cannot reach.  That is not hypothetical --
// range steps were missing for the entire life of this file, which is why
// the range-splice corruption was never a candidate for it to find.
//
// It also pins that a span step is reachable with a step BELOW it, since
// that is the route (query the copy at a span, write through what comes
// back) the spans were added for.
func TestEnumeratePathsCoversEveryStepForm(t *testing.T) {
	t.Parallel()

	for name, mk := range aliasSources {
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			forms := map[string]int{}
			belowRange := 0
			for _, p := range enumeratePaths(mk(), 3) {
				for i, s := range p {
					switch s.Type {
					case lisp.LString:
						forms["key"]++
					case lisp.LInt:
						forms["index"]++
					case lisp.LSExpr:
						forms["range"]++
						if i < len(p)-1 {
							belowRange++
						}
					default:
						forms["other"]++
					}
				}
			}
			for _, want := range []string{"key", "index", "range"} {
				assert.Greater(t, forms[want], 0,
					"the battery enumerates no %s step for the %s source, so every "+
						"path operation reached only through one is untested: %v",
					want, name, forms)
			}
			assert.Greater(t, belowRange, 0,
				"no enumerated path continues below a span step, so the "+
					"query-a-span-then-write-through-it route is untested")
		})
	}
}

// TestAliasBattery is the requirement: no write through a returned copy, by
// any route, may reach the source.
func TestAliasBattery(t *testing.T) {
	t.Parallel()

	ran, failures := runAliasBattery(t, copyOps, 3, 3)
	requireBatteryWrote(t, ran, 500, "map", "list", "vector")
	if len(failures) == 0 {
		t.Logf("alias battery: %v copy/mutate combinations written through, none reached the source", ran)
		return
	}
	// Report a bounded sample: a broken copy fails in the hundreds and the
	// first few name the shape.
	const sample = 12
	msgs := make([]string, 0, sample+1)
	for i, f := range failures {
		if i == sample {
			msgs = append(msgs, fmt.Sprintf("... and %d more", len(failures)-sample))
			break
		}
		msgs = append(msgs, f.String())
	}
	t.Errorf("%d of the copy/mutate combinations reached the source (#395):\n%s",
		len(failures), strings.Join(msgs, "\n"))
}

// TestAliasBatteryRedProof runs the identical battery against a copy that
// rebuilds only the node its path names and shares everything below it, and
// requires it to fail.  Without this a clean TestAliasBattery would be
// consistent with a battery that never writes anywhere interesting.
//
// The copy paths are held to one step so the sharing copy is exactly the
// pre-#395 shallow copy and exactly a fully path-aware copy, rather than
// something that also leaks while it is being built.
func TestAliasBatteryRedProof(t *testing.T) {
	t.Parallel()

	ran, failures := runAliasBattery(t, sharingCopyOps, 1, 3)
	// The list source is absent by construction: the sharing copy is built
	// out of the in-place builtins and errMutateList refuses a list, so the
	// red-proof covers the map and vector shapes.
	requireBatteryWrote(t, ran, 100, "map", "vector")
	require.NotEmpty(t, failures,
		"the battery must fail against a copy that shares its off-path subtrees, "+
			"or a clean run proves nothing")
	t.Logf("red-proof: a copy that shares everything off its path fails %d of the "+
		"battery's combinations, e.g.\n%s", len(failures), failures[0].String())
}

// TestIssue395Reproduction is the issue's own example, at the lisp-visible
// surface: a redaction copy, then a write into the copy's address.
func TestIssue395Reproduction(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	inner := lisp.SortedMap()
	inner.MapSet("city", lisp.String("London"))
	patient := lisp.SortedMap()
	patient.MapSet("ssn", lisp.String("123"))
	patient.MapSet("address", inner)

	redacted := callBuiltin(env, BuiltinQueryNil, patient, lisp.String("ssn"))
	require.NotEqual(t, lisp.LError, redacted.Type, "%v", redacted)

	got := callBuiltin(env, BuiltinQuerySetMutate,
		redacted, lisp.String("address"), lisp.String("city"), lisp.String("REDACTED"))
	require.NotEqual(t, lisp.LError, got.Type, "%v", got)

	city, _ := inner.Map().Get(lisp.String("city"))
	assert.Equal(t, `"London"`, city.String(),
		"the write through the redaction copy reached the patient record (#395)")

	// And the same reach one query further out, which is the route a
	// spine-only copy leaves open even when the direct write is blocked.
	sub := callBuiltin(env, BuiltinQueryGet, redacted, lisp.String("address"))
	require.NotEqual(t, lisp.LError, sub.Type, "%v", sub)
	callBuiltin(env, BuiltinQuerySetMutate, sub, lisp.String("city"), lisp.String("REDACTED2"))
	city, _ = inner.Map().Get(lisp.String("city"))
	assert.Equal(t, `"London"`, city.String(),
		"(? copy \"address\") handed back the source's own map")
}

// TestIssue395StructuralReproduction is the issue's second example: a ?del!
// through a ?set copy restructuring the source's array in place and
// rewriting its dims.
func TestIssue395StructuralReproduction(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	vec := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	src := lisp.SortedMap()
	src.MapSet("v", vec)
	before := src.String()

	cp := callBuiltin(env, BuiltinQuerySet, src, lisp.String("other"), lisp.String("x"))
	require.NotEqual(t, lisp.LError, cp.Type, "%v", cp)
	callBuiltin(env, BuiltinQueryDeleteMutate, cp, lisp.String("v"), lisp.Int(0))

	assert.Equal(t, before, src.String(),
		"the delete through the copy restructured the source's array (#395)")
}

// TestCopyPreservesQuoting pins the regression the copy rework has to keep
// clear of: toList and toVector build unquoted, so a copy that rebuilds a
// quoted list without restoring the quote demotes a list to an s-expression
// the evaluator would try to apply.
func TestCopyPreservesQuoting(t *testing.T) {
	t.Parallel()

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil

	for name, mk := range map[string]func() *lisp.LVal{
		"nested in a map": func() *lisp.LVal {
			m := lisp.SortedMap()
			m.MapSet("notes", lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}))
			m.MapSet("other", lisp.String("x"))
			return m
		},
		"nested in a vector": func() *lisp.LVal {
			return lisp.Vector([]*lisp.LVal{
				lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}),
				lisp.String("x"),
			})
		},
	} {
		t.Run(name, func(t *testing.T) {
			src := mk()
			cp, err := copyLVal(src)
			require.NoError(t, err)
			require.Equal(t, src.String(), cp.String(), "the copy must print the same")

			for _, steps := range [][]*lisp.LVal{
				{lisp.String("notes")}, {lisp.Int(0)},
			} {
				got := callBuiltin(env, BuiltinQueryGet, withSteps(cp, steps)...)
				want := callBuiltin(env, BuiltinQueryGet, withSteps(src, steps)...)
				if want.Type == lisp.LError {
					continue
				}
				require.NotEqual(t, lisp.LError, got.Type, "%v", got)
				assert.Equal(t, want.Quoted, got.Quoted,
					"the copy changed the quoting of %s", pathString(steps))
				assert.Equal(t, want.Type, got.Type,
					"the copy changed the type at %s", pathString(steps))
			}

			// The same through a copying path operation, which is where a
			// caller meets the copy.
			cp2 := callBuiltin(env, BuiltinQuerySet, withSteps(src, []*lisp.LVal{lisp.String("added")}, lisp.String("y"))...)
			if cp2.Type != lisp.LError {
				notes := callBuiltin(env, BuiltinQueryGet, cp2, lisp.String("notes"))
				if notes.Type != lisp.LError && !notes.IsNil() {
					assert.True(t, notes.Quoted, "?set demoted a quoted list to an s-expression")
				}
			}
		})
	}
}
