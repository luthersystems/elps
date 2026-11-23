// Copyright © 2026 The ELPS authors

package libelpspath_test

import (
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libelpspath"
)

// visitCounts returns, for each segment of sel, the number of values Visit
// says the segment applies to.
func visitCounts(t testing.TB, doc *lisp.LVal, sel string) []int {
	t.Helper()
	p, err := libelpspath.ParseSelector(sel)
	if err != nil {
		t.Fatalf("ParseSelector(%q): %v", sel, err)
	}
	steps, err := libelpspath.SelectorSteps(sel)
	if err != nil {
		t.Fatalf("SelectorSteps(%q): %v", sel, err)
	}
	counts := make([]int, len(steps))
	err = libelpspath.Visit(p, doc, func(segment int) error {
		if segment < 0 || segment >= len(counts) {
			return fmt.Errorf("segment %d out of [0, %d)", segment, len(counts))
		}
		counts[segment]++
		return nil
	})
	if err != nil {
		t.Fatalf("Visit(%q): %v", sel, err)
	}
	return counts
}

// frontierCounts is an independent count of the same thing: it builds the
// whole frontier of each segment from the exported, one-step paths that
// ArgsToPath builds, and tells a found value from Get's () for a missing
// key or index by looking in the container itself.
func frontierCounts(t testing.TB, doc *lisp.LVal, sel string) []int {
	t.Helper()
	steps, err := libelpspath.SelectorSteps(sel)
	if err != nil {
		t.Fatalf("SelectorSteps(%q): %v", sel, err)
	}
	return frontierStepCounts(t, doc, steps)
}

// frontierStepCounts is frontierCounts over the steps ArgsToPath takes.
func frontierStepCounts(t testing.TB, doc *lisp.LVal, steps []*lisp.LVal) []int {
	t.Helper()
	counts := make([]int, len(steps))
	frontier := []*lisp.LVal{doc}
	for i, step := range steps {
		counts[i] = len(frontier)
		one, err := libelpspath.ArgsToPath([]*lisp.LVal{step})
		if err != nil {
			t.Fatalf("ArgsToPath(%v): %v", step, err)
		}
		var next []*lisp.LVal
		for _, v := range frontier {
			got, err := one.Get(v)
			if err != nil {
				continue // Get refuses the step: nothing passes on.
			}
			switch step.Type {
			case lisp.LString:
				if _, ok := v.Map().Get(lisp.String(step.Str)); ok {
					next = append(next, got)
				}
			case lisp.LInt:
				cells, _ := v.SeqCells()
				j := step.Int
				if j < 0 {
					j += len(cells)
				}
				if j >= 0 && j < len(cells) {
					next = append(next, got)
				}
			case lisp.LSymbol: // '*
				cells, _ := v.SeqCells()
				for _, c := range cells {
					if c != nil {
						next = append(next, c)
					}
				}
			default: // a range: its window, unscanned
				next = append(next, got)
			}
		}
		frontier = next
	}
	return counts
}

// appendStepCounts is the count of luthersystems/substrate's appendStep, the
// copy of the step rules that Visit replaces (luthersystems/substrate#1144). It is copied
// here so that every input on which Visit counts differently is listed in
// TestVisitDiffersFromSubstrateCopy.
func appendStepCounts(doc *lisp.LVal, steps []*lisp.LVal) []int {
	counts := make([]int, len(steps))
	frontier := []*lisp.LVal{doc}
	for i, step := range steps {
		counts[i] = len(frontier)
		var next []*lisp.LVal
		for _, v := range frontier {
			next = substrateAppendStep(next, v, step)
		}
		frontier = next
	}
	return counts
}

func substrateAppendStep(out []*lisp.LVal, v *lisp.LVal, step *lisp.LVal) []*lisp.LVal {
	if v == nil {
		return out
	}
	listCells := func(v *lisp.LVal) []*lisp.LVal {
		cells, _ := v.SeqCells()
		return cells
	}
	switch step.Type {
	case lisp.LString:
		vm, _ := lisp.AsMap(v)
		if x, ok := lisp.Lookup[*lisp.LVal](vm, step.Str); ok {
			out = append(out, x)
		}
	case lisp.LInt:
		cells := listCells(v)
		i := step.Int
		if i < 0 {
			i += len(cells)
		}
		if i >= 0 && i < len(cells) {
			out = append(out, cells[i])
		}
	case lisp.LSymbol:
		if v.Type == lisp.LSortMap {
			for _, x := range v.All() {
				out = append(out, x)
			}
		} else {
			out = append(out, listCells(v)...)
		}
	case lisp.LSExpr:
		cells := listCells(v)
		if len(step.Cells) < 2 {
			break
		}
		from, to := step.Cells[1].Int, len(cells)
		if len(step.Cells) > 2 {
			to = step.Cells[2].Int
		}
		from, to = max(0, min(from, len(cells))), max(0, min(to, len(cells)))
		if from < to {
			out = append(out, lisp.SExpr(cells[from:to]))
		}
	default:
	}
	return out
}

// cyclicPair returns a list of two cells that both hold the list itself.
// Each '* over it doubles the number of values reached.
func cyclicPair() *lisp.LVal {
	v := lisp.Cells{lisp.Nil(), lisp.Nil()}.SExpr()
	v.Cells[0] = v
	v.Cells[1] = v
	return v
}

// sharedDAG returns a list of two references to a list of two references
// to ..., depth levels deep, over the map {"x": 1}: depth allocations and
// 2^depth paths.
func sharedDAG(depth int) *lisp.LVal {
	v := lisp.MapOf("x", 1)
	for range depth {
		v = lisp.Cells{v, v}.SExpr()
	}
	return v
}

// visitDocs are the documents of the count tests: the shapes of
// substrate's TestChargeVisitsSameSteps, and the shapes on which the step
// rules of Get and of substrate's copy differ.
func visitDocs() map[string]*lisp.LVal {
	items := func(n int) *lisp.LVal {
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			inner := make([]*lisp.LVal, i%4)
			for j := range inner {
				inner[j] = lisp.Int(j)
			}
			cells[i] = lisp.MapOf("x", i, "ys", lisp.SExpr(inner))
		}
		d := lisp.MapOf("items", lisp.SExpr(cells), "vec", lisp.Vector(cells))
		byName := lisp.SortedMap()
		for i, c := range cells {
			byName.MapSetString(string(rune('a'+i%26))+strings.Repeat("z", i/26), c)
		}
		d.MapSetString("byname", byName)
		return d
	}
	odd := lisp.MapOf(
		"nilval", lisp.Nil(), // a key that holds ()
		"err", lisp.Error(errors.New("boom")), // a key that holds an error value
		"nilcell", lisp.Cells{lisp.Int(1), nil, lisp.Int(2)}.SExpr(), // a Go nil cell
		"grid", lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(2)}),
			[]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}),
		"lists", lisp.Cells{
			lisp.Cells{lisp.Int(1), lisp.Int(2), lisp.Int(3)}.SExpr(),
			lisp.Cells{lisp.Int(4), lisp.Int(5)}.SExpr(),
		}.SExpr(),
	)
	odd.MapSet(lisp.Symbol("sym"), lisp.Cells{lisp.Int(1), lisp.Int(2)}.SExpr())
	return map[string]*lisp.LVal{
		"items":      items(60),
		"empty":      items(0),
		"cyclic":     cyclicPair(),
		"shared-dag": sharedDAG(8),
		"leaf":       lisp.Int(3),
		"nil":        lisp.Nil(),
		"odd":        odd,
	}
}

// visitSelectors are the selectors of the count tests: substrate's list
// first, then the shapes on which the two step rules differ.
var visitSelectors = []string{
	".", ".items", ".items[]", ".items[].x", ".items[].ys[]", ".items[].ys[][]",
	".items[1:4][].x", ".items[3:][].ys[]", ".items[-1].ys[]", ".vec[].ys[0]",
	".byname[].ys[]", ".nope[].x", ".items[].nope.x.y", ".[]", ".[][]", ".[][][][][]",
	".[][0][][1][]", ".[1:][][]", ".items[][]",
	// ranges
	".items[-2:][]", ".items[-2:][].x", ".items[:-1][]", ".items[0:-58][]", ".items[2:2][]", ".items[5:2][]",
	".items[0:100][]", ".items[100:][]", ".items[-100:][]", ".[0:1][]", ".[1:1][]",
	".[][1:1][]", ".[][0:5][]", ".[][-1:][]", ".lists[][1:][]", ".lists[][:-1][]",
	".lists[][2:2][]", ".lists[][2:9][]", ".byname[1:3][]", ".items[1:3][1:2][].x",
	// indexes
	".items[-1].x", ".items[-60].x", ".items[-61].x", ".items[60].x", ".items[59].ys[][]",
	".grid[0].x", ".grid[].x", ".grid[0:1][]", ".lists[][-1].x", ".lists[][5].x",
	// keys and iterators over odd shapes
	".nilval[]", ".nilval.x", ".nilval[0]", ".err.x", ".err[]", ".nilcell[].x",
	".nilcell[1].x", ".nilcell[0:3][]", ".nilcell[0:3][0]", ".nilcell[0:3][].x", ".nilcell[2:3][].x", ".sym[]", ".byname[].x", ".byname[][]",
	".[].x", ".[][].x.y", ".items[].ys[].x",
}

// TestVisitMatchesFrontier pins that Visit counts, for each segment, what an
// independent frontier count over Get's one-step paths counts.
func TestVisitMatchesFrontier(t *testing.T) {
	for dname, doc := range visitDocs() {
		for _, sel := range visitSelectors {
			got := visitCounts(t, doc, sel)
			want := frontierCounts(t, doc, sel)
			if !slices.Equal(got, want) {
				t.Errorf("%s %s: Visit counts %v, frontier counts %v", dname, sel, got, want)
			}
		}
	}
}

// charge is substrate's price of a selector: max(1, count) per segment.
func charge(counts []int) int {
	total := 0
	for _, n := range counts {
		total += max(1, n)
	}
	return total
}

// The classes of input on which Visit, which follows Get, counts
// differently from substrate's copy of the step rules.
const (
	// '* over a map: the copy fans out over the map's values; Get refuses
	// the step, so nothing passes on.
	diffMapIter = "'* over a map"
	// A range past an end of the sequence: the copy clamps it to the
	// sequence; Get refuses it.
	diffRangeClamp = "range out of bounds"
	// A negative range start: Get counts it back from the end; the copy
	// takes it as 0, so its window is wider.
	diffRangeNegStart = "negative range start"
	// A negative range end: Get counts it back from the end; the copy takes
	// it as 0 and selects nothing.
	diffRangeNegEnd = "negative range end"
	// An empty window (from = to): Get passes it on; the copy passes
	// nothing.
	diffRangeEmpty = "empty range window"
	// A key that holds an error value: Get passes the value on; the copy's
	// Lookup skips error values.
	diffErrValue = "key holds an error value"
	// A Go nil cell under '*: Get fails that element before the next step
	// reads it; the copy passes the cell on and counts it.
	diffNilCellIter = "nil cell under '*"
	// An index that lands on a Go nil cell: Get refuses it; the copy passes
	// the cell on and counts it.
	diffNilCellIndex = "index on a nil cell"
)

// TestVisitDiffersFromSubstrateCopy lists every document and selector of
// the count tests on which Visit and substrate's copy of the step rules
// count differently, with the class of the difference, and pins the list.
// A difference changes substrate's step count when it changes the charge,
// the sum of max(1, count): those are marked charged.
func TestVisitDiffersFromSubstrateCopy(t *testing.T) {
	type diff struct {
		class   string
		charged bool
	}
	want := map[string]diff{
		"empty .[].x":           {diffMapIter, true},
		"empty .[][-1:][]":      {diffMapIter, true},
		"empty .[][0:5][]":      {diffMapIter, true},
		"empty .[][0][][1][]":   {diffMapIter, true},
		"empty .[][1:1][]":      {diffMapIter, true},
		"empty .[][].x.y":       {diffMapIter, true},
		"empty .[][]":           {diffMapIter, true},
		"empty .[][][][][]":     {diffMapIter, true},
		"items .[].x":           {diffMapIter, true},
		"items .[][-1:][]":      {diffMapIter, true},
		"items .[][0:5][]":      {diffMapIter, true},
		"items .[][0][][1][]":   {diffMapIter, true},
		"items .[][1:1][]":      {diffMapIter, true},
		"items .[][].x.y":       {diffMapIter, true},
		"items .[][]":           {diffMapIter, true},
		"items .[][][][][]":     {diffMapIter, true},
		"items .byname[].x":     {diffMapIter, true},
		"items .byname[].ys[]":  {diffMapIter, true},
		"items .byname[][]":     {diffMapIter, true},
		"odd .[].x":             {diffMapIter, true},
		"odd .[][-1:][]":        {diffMapIter, true},
		"odd .[][0:5][]":        {diffMapIter, true},
		"odd .[][0][][1][]":     {diffMapIter, true},
		"odd .[][1:1][]":        {diffMapIter, true},
		"odd .[][].x.y":         {diffMapIter, true},
		"odd .[][]":             {diffMapIter, true},
		"odd .[][][][][]":       {diffMapIter, true},
		"cyclic .[][0:5][]":     {diffRangeClamp, true},
		"shared-dag .[][0:5][]": {diffRangeClamp, true},
		"items .items[-100:][]": {diffRangeClamp, false},
		"items .items[0:100][]": {diffRangeClamp, false},
		"odd .lists[][2:9][]":   {diffRangeClamp, false},
		"items .items[-2:][].x": {diffRangeNegStart, true},
		"items .items[0:-58][]": {diffRangeNegEnd, false},
		"items .items[:-1][]":   {diffRangeNegEnd, false},
		"odd .lists[][:-1][]":   {diffRangeNegEnd, true},
		"cyclic .[1:1][]":       {diffRangeEmpty, false},
		"cyclic .[][1:1][]":     {diffRangeEmpty, true},
		"shared-dag .[1:1][]":   {diffRangeEmpty, false},
		"shared-dag .[][1:1][]": {diffRangeEmpty, true},
		"items .items[2:2][]":   {diffRangeEmpty, false},
		"odd .lists[][2:2][]":   {diffRangeEmpty, true},
		"odd .err.x":            {diffErrValue, false},
		"odd .err[]":            {diffErrValue, false},
		"odd .nilcell[].x":      {diffNilCellIter, true},
		"odd .nilcell[1].x":     {diffNilCellIndex, false},
		"odd .nilcell[0:3][].x": {diffNilCellIter, true},
	}
	seen := map[string]bool{}
	for dname, doc := range visitDocs() {
		for _, sel := range visitSelectors {
			steps, err := libelpspath.SelectorSteps(sel)
			if err != nil {
				t.Fatal(err)
			}
			v := visitCounts(t, doc, sel)
			s := appendStepCounts(doc, steps)
			key := dname + " " + sel
			w, listed := want[key]
			if slices.Equal(v, s) {
				continue
			}
			seen[key] = true
			if !listed {
				t.Errorf("%s: new difference: Visit %v, substrate copy %v", key, v, s)
				continue
			}
			if charged := charge(v) != charge(s); charged != w.charged {
				t.Errorf("%s (%s): charge Visit %d, substrate copy %d; listed charged=%v",
					key, w.class, charge(v), charge(s), w.charged)
			}
		}
	}
	for key, w := range want {
		if !seen[key] {
			t.Errorf("%s: listed as %q, but the counts agree or the pair is not tested", key, w.class)
		}
	}
}

// TestVisitSegmentsAreSelectorSteps pins that Visit numbers its segments as
// SelectorSteps renders the selector: an embedder prices segment i with
// step i.
func TestVisitSegmentsAreSelectorSteps(t *testing.T) {
	doc := cyclicPair()
	for _, sel := range visitSelectors {
		steps, err := libelpspath.SelectorSteps(sel)
		if err != nil {
			t.Fatal(err)
		}
		p, err := libelpspath.ParseSelector(sel)
		if err != nil {
			t.Fatal(err)
		}
		last := -1
		err = libelpspath.Visit(p, doc, func(segment int) error {
			if segment >= len(steps) {
				return fmt.Errorf("segment %d, selector has %d steps", segment, len(steps))
			}
			last = max(last, segment)
			return nil
		})
		if err != nil {
			t.Errorf("%s: %v", sel, err)
		}
		// Over the cyclic pair every '* and every in-range index reaches a
		// value; only the selectors with a key stop early.
		if !strings.ContainsAny(sel, "abcdefghijklmnopqrstuvwxyz") && !strings.Contains(sel, ":") && last != len(steps)-1 {
			t.Errorf("%s: last segment visited %d, want %d", sel, last, len(steps)-1)
		}
	}
}

// TestVisitStopsOnError pins that the first error visit returns ends the
// walk at once: 40 iterators over a cyclic pair reach 2^40 values.
func TestVisitStopsOnError(t *testing.T) {
	p, err := libelpspath.ParseSelector("." + strings.Repeat("[]", 40))
	if err != nil {
		t.Fatal(err)
	}
	stop := errors.New("stop")
	calls := 0
	err = libelpspath.Visit(p, cyclicPair(), func(int) error {
		calls++
		if calls == 1000 {
			return stop
		}
		return nil
	})
	if !errors.Is(err, stop) {
		t.Fatalf("Visit returned %v, want the callback's error", err)
	}
	if calls != 1000 {
		t.Errorf("visit called %d times, want 1000", calls)
	}
}

// otherPath is a Path this package did not build.
type otherPath struct{ libelpspath.Path }

// TestVisitRefusesForeignPath pins that Visit refuses a Path it cannot
// split into steps, before it calls visit.
func TestVisitRefusesForeignPath(t *testing.T) {
	p := libelpspath.Chain(libelpspath.Dot("a"), otherPath{libelpspath.Dot("b")})
	called := false
	err := libelpspath.Visit(p, lisp.MapOf("a", 1), func(int) error {
		called = true
		return nil
	})
	if err == nil {
		t.Fatal("Visit returned nil, want an error")
	}
	if called {
		t.Error("Visit called visit before it refused the path")
	}
}

// TestVisitMemoryBounded pins that Visit does not hold the values a segment
// reaches. Over a cyclic pair or a shared DAG that number grows as 2^k in
// the number k of iterators; Visit's memory follows the path length. It
// measures with testing.Benchmark, whose bytes per operation average over
// many calls.
func TestVisitMemoryBounded(t *testing.T) {
	const k = 19 // 2^18 values on the last segment
	p, err := libelpspath.ParseSelector("." + strings.Repeat("[]", k))
	if err != nil {
		t.Fatal(err)
	}
	for name, doc := range map[string]*lisp.LVal{
		"cyclic":     cyclicPair(),
		"shared-dag": sharedDAG(20),
	} {
		t.Run(name, func(t *testing.T) {
			var visits int64
			if err := libelpspath.Visit(p, doc, func(int) error { visits++; return nil }); err != nil {
				t.Fatal(err)
			}
			if want := int64(1)<<k - 1; visits != want {
				t.Errorf("visited %d values, want %d", visits, want)
			}
			res := testing.Benchmark(func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if err := libelpspath.Visit(p, doc, func(int) error { return nil }); err != nil {
						b.Fatal(err)
					}
				}
			})
			n := res.AllocedBytesPerOp()
			// The frontier of the last segment alone is 2^18 pointers, 2 MiB.
			const limit = 16 << 10
			t.Logf("%s: %d iterators, %d visits, %d bytes per call over %d calls", name, k, visits, n, res.N)
			if n > limit {
				t.Errorf("Visit allocated %d bytes per call, want at most %d", n, limit)
			}
		})
	}
}

// composedCase is a Path built with Chain, Iter and Root, and the flat
// steps Get treats it as, or nil when Visit must refuse it.
type composedCase struct {
	name  string
	path  libelpspath.Path
	steps []*lisp.LVal
}

func composedCases(t *testing.T) []composedCase {
	t.Helper()
	parsed := func(sel string) libelpspath.Path {
		p, err := libelpspath.ParseSelector(sel)
		if err != nil {
			t.Fatal(err)
		}
		return p
	}
	key := lisp.String
	idx := func(i int) *lisp.LVal { return lisp.Int(i) }
	iter := lisp.Symbol("*")
	rng := func(from, to int) *lisp.LVal {
		return lisp.QExpr([]*lisp.LVal{lisp.Symbol("range"), lisp.Int(from), lisp.Int(to)})
	}
	return []composedCase{
		{"chain", libelpspath.Chain(libelpspath.Dot("items"), libelpspath.Iter(), libelpspath.Dot("x")),
			[]*lisp.LVal{key("items"), iter, key("x")}},
		{"iter-holds-steps", libelpspath.Iter(libelpspath.Dot("ys"), libelpspath.Iter()),
			[]*lisp.LVal{iter, key("ys"), iter}},
		{"nested-chains", libelpspath.Chain(
			libelpspath.Chain(libelpspath.Dot("items"), libelpspath.Iter()),
			libelpspath.Chain(libelpspath.Dot("ys"), libelpspath.Iter())),
			[]*lisp.LVal{key("items"), iter, key("ys"), iter}},
		{"iter-in-iter", libelpspath.Chain(libelpspath.Dot("items"),
			libelpspath.Iter(libelpspath.Dot("ys"), libelpspath.Iter(libelpspath.Index(0)))),
			[]*lisp.LVal{key("items"), iter, key("ys"), iter, idx(0)}},
		{"iter-then-chain", libelpspath.Chain(libelpspath.Iter(libelpspath.Dot("x")), libelpspath.Dot("y")),
			[]*lisp.LVal{iter, key("x"), key("y")}},
		{"range-iter-iter", libelpspath.Chain(libelpspath.Dot("lists"), libelpspath.Range(0, 2, false),
			libelpspath.Iter(), libelpspath.Iter()),
			[]*lisp.LVal{key("lists"), rng(0, 2), iter, iter}},
		{"triple-iter", libelpspath.Iter(libelpspath.Iter(libelpspath.Iter())),
			[]*lisp.LVal{iter, iter, iter}},
		{"root-outside", libelpspath.Root(libelpspath.Chain(libelpspath.Dot("items"), libelpspath.Iter())),
			[]*lisp.LVal{key("items"), iter}},
		{"root-root", libelpspath.Root(libelpspath.Root(libelpspath.Dot("items"))),
			[]*lisp.LVal{key("items")}},
		{"root-last", libelpspath.Chain(libelpspath.Dot("items"),
			libelpspath.Root(libelpspath.Chain(libelpspath.Iter(), libelpspath.Dot("x")))),
			[]*lisp.LVal{key("items"), iter, key("x")}},
		{"parsed-last", libelpspath.Chain(libelpspath.Dot("lists"), parsed(".[][0]")),
			[]*lisp.LVal{key("lists"), iter, idx(0)}},
		// Get runs a Root with steps after it as one unit.
		{"root-then-key", libelpspath.Chain(parsed(".a[]"), libelpspath.Dot("b")), nil},
		{"root-then-iter", libelpspath.Chain(libelpspath.Root(libelpspath.Dot("items")), libelpspath.Iter()), nil},
		{"root-in-iter", libelpspath.Iter(libelpspath.Root(libelpspath.Iter(libelpspath.Dot("x"))), libelpspath.Dot("y")), nil},
		{"root-mid", libelpspath.Chain(libelpspath.Dot("items"), libelpspath.Root(libelpspath.Dot("a")), libelpspath.Index(0)), nil},
	}
}

// TestVisitComposedPathsFollowGet pins that Visit groups the steps of a
// path built with Chain, Iter and Root as Get does. For each path it
// accepts, Get answers what the flat path of its steps answers, so the
// grouping is the flat one, and Visit counts what the frontier counts over
// those steps. A path whose Root has steps after it is refused before any
// visit.
func TestVisitComposedPathsFollowGet(t *testing.T) {
	docs := visitDocs()
	docs["ab"] = lisp.MapOf("a", lisp.Cells{
		lisp.MapOf("b", lisp.MapOf("c", 1)),
		lisp.MapOf("b", lisp.MapOf("c", 1)),
	}.SExpr())
	// Get's answers are compared as text, so skip the documents whose
	// answers contain themselves.
	delete(docs, "cyclic")
	delete(docs, "shared-dag")
	answer := func(p libelpspath.Path, doc *lisp.LVal) string {
		got, err := p.Get(doc)
		if err != nil {
			return "error: " + err.Error()
		}
		return got.String()
	}
	for _, c := range composedCases(t) {
		for dname, doc := range docs {
			calls := 0
			counts := map[int]int{}
			err := libelpspath.Visit(c.path, doc, func(segment int) error {
				calls++
				counts[segment]++
				return nil
			})
			if c.steps == nil {
				if err == nil {
					t.Errorf("%s %s: Visit accepted a Root with steps after it", c.name, dname)
				}
				if calls != 0 {
					t.Errorf("%s %s: Visit called visit %d times before it refused", c.name, dname, calls)
				}
				continue
			}
			if err != nil {
				t.Errorf("%s %s: %v", c.name, dname, err)
				continue
			}
			flat, err := libelpspath.ArgsToPath(c.steps)
			if err != nil {
				t.Fatal(err)
			}
			if g, f := answer(c.path, doc), answer(flat, doc); g != f {
				t.Errorf("%s %s: Get answers %s, flat steps answer %s", c.name, dname, g, f)
			}
			want := frontierStepCounts(t, doc, c.steps)
			got := make([]int, len(want))
			for seg, n := range counts {
				if seg >= len(got) {
					t.Fatalf("%s %s: segment %d past %d steps", c.name, dname, seg, len(got))
				}
				got[seg] = n
			}
			if !slices.Equal(got, want) {
				t.Errorf("%s %s: Visit counts %v, frontier counts %v", c.name, dname, got, want)
			}
		}
	}
	// Why a Root with steps after it is refused: Get runs the key on the
	// Root's whole answer, a list, and refuses it, where a flat walk would
	// reach both "b" values.
	for _, c := range composedCases(t) {
		if c.name != "root-then-key" {
			continue
		}
		if _, err := c.path.Get(docs["ab"]); err == nil {
			t.Errorf("%s: Get succeeded; want it to refuse the key on the Root's answer", c.name)
		}
	}
}
