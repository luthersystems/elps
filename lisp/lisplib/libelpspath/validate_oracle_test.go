// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"
	"math/rand/v2"
	"slices"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// The validator every builtin runs first (okSimpleType) walks a document in
// two passes: an unordered pass that answers "is it valid", and, only for a
// document that is not, the walk in sorted key order whose first error is
// the result (luthersystems/elps#944).  Its contract is that nothing a
// caller can see changed: every document gets the result, the error or the
// panic of the single ordered walk, and a host map's Entries method is
// called as often as before.
//
// oracleOkSimpleType below is that single ordered walk, copied from the
// validator as it was before the unordered pass existed.  It is a copy and
// not a call into the shipped ordered pass so that a later edit to the
// shipped walk cannot change both sides of the comparison at once.

func oracleOkSimpleType(in *lisp.LVal) error {
	var st cycleState
	return oracleOkSimpleTypeGuarded(in, newCycleGuard(&st))
}

func oracleOkSimpleTypeGuarded(in *lisp.LVal, g cycleGuard) error {
	// The validator reports a Go nil cell instead of panicking on it, so
	// the reference walk does too.
	if in == nil {
		return errNilCell
	}
	if in.IsNil() {
		return nil
	}
	switch in.Type {
	case lisp.LSortMap, lisp.LArray, lisp.LSExpr, lisp.LTaggedVal, lisp.LQuote:
		return oracleOkSimpleContainerTypeGuarded(in, g)
	default:
		return nil
	}
}

func oracleOkSimpleContainerTypeGuarded(in *lisp.LVal, g cycleGuard) error {
	if in.IsNil() {
		return errors.New("nil container type invalid")
	}
	switch in.Type {
	case lisp.LSortMap, lisp.LArray, lisp.LSExpr, lisp.LTaggedVal, lisp.LQuote:
	default:
		return fmt.Errorf("invalid container type: %v", in.Type)
	}
	if _, ok := g.state.valid[in]; ok {
		return nil
	}
	g, cyclic := g.descend(in)
	if cyclic {
		return errCyclicValue
	}
	err := oracleOkSimpleContainerContents(in, g)
	if g.tracking() {
		g.ascend(in)
	}
	if err == nil {
		g.state.noteValid(in, containerWidth(in))
	}
	return err
}

func oracleOkSimpleContainerContents(in *lisp.LVal, g cycleGuard) error {
	switch in.Type {
	case lisp.LTaggedVal, lisp.LQuote:
		wrapped, err := wrapperValue(in)
		if err != nil {
			return err
		}
		return oracleOkSimpleTypeGuarded(wrapped, g)
	case lisp.LSortMap:
		// A map with no map data is refused, as the validator refuses it
		// (luthersystems/elps#963).
		m0, ok := in.Native.(*lisp.MapData)
		if !ok {
			return errMalformedMap
		}
		entries := sortedMapEntries(m0)
		if entries.IsError() {
			return lisp.GoError(entries)
		}
		for _, ent := range entries.Cells {
			v := ent.Cells[1]
			err := oracleOkSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LArray:
		// An array without its dims and data lists is refused, as the
		// validator refuses it (luthersystems/elps#963).
		dims, data := in.ArrayParts()
		if dims == nil || data == nil || dims.Type != lisp.LSExpr || data.Type != lisp.LSExpr {
			return errMalformedArray
		}
		if n := dims.Len(); n != 1 {
			if n > 1 {
				return errors.New("cannot index multi-dimensional array")
			}
			return errors.New("cannot index zero-dimensional array")
		}
		cells := data.Cells
		for _, v := range cells {
			err := oracleOkSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LSExpr:
		cells := in.Cells
		for _, v := range cells {
			err := oracleOkSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	default:
		return fmt.Errorf("invalid container type: %v", in.Type)
	}
}

// validateOutcome is everything a caller can see of one validation: the
// error or the panic, and the host map calls it made.
type validateOutcome struct {
	err    string
	panic  string
	cyclic bool
	host   bool
	trace  []string
}

func (o validateOutcome) String() string {
	return fmt.Sprintf("error=%q panic=%q cyclic=%t host=%t trace=%v", o.err, o.panic, o.cyclic, o.host, o.trace)
}

func (o validateOutcome) equal(p validateOutcome) bool {
	return o.err == p.err && o.panic == p.panic && o.cyclic == p.cyclic && o.host == p.host && slices.Equal(o.trace, p.trace)
}

// observeValidate runs validate on v and records its outcome.  in holds the
// trace of the document's host maps.
func observeValidate(in *goldenInput, v *lisp.LVal, validate func(*lisp.LVal) error) (o validateOutcome) { //nolint:nonamedreturns // the deferred recover sets the result in this frame
	in.trace = nil
	defer func() {
		if p := recover(); p != nil {
			o.panic = fmt.Sprintf("%T: %v", p, p)
		}
		o.trace = slices.Clone(in.trace)
	}()
	if err := validate(v); err != nil {
		o.err = fmt.Sprintf("%T: %v", err, err)
		o.cyclic = errors.Is(err, errCyclicValue)
		o.host = errors.Is(err, goldenHostError)
	}
	return o
}

// Every fixture of the value walker corpus validates as it did under the
// single ordered walk.  The corpus holds the malformed values host code can
// build, which panic the walk, and host maps whose Entries method fails or
// rewrites the document while the walk runs.
func TestValidatorMatchesOrderedWalkOnFixtures(t *testing.T) {
	for _, f := range goldenFixtures() {
		t.Run(f.name, func(t *testing.T) {
			// One document for each side: a host map can change its own.
			wantIn, wantDoc := newGoldenInput(f)
			gotIn, gotDoc := newGoldenInput(f)
			want := observeValidate(wantIn, wantDoc, oracleOkSimpleType)
			got := observeValidate(gotIn, gotDoc, okSimpleType)
			if !got.equal(want) {
				t.Fatalf("validator: %v\nordered walk: %v", got, want)
			}
			wantAfter, _ := wantIn.render(wantDoc)
			gotAfter, _ := gotIn.render(gotDoc)
			if gotAfter != wantAfter {
				t.Fatalf("the document differs after validation:\nvalidator: %s\nordered walk: %s", gotAfter, wantAfter)
			}
		})
	}
}

// docGen builds random documents for the differential test below.
type docGen struct {
	rng *rand.Rand
	in  *goldenInput
	// made holds containers already built, which a later container can
	// hold again: a document that shares a subtree, or (through
	// cycleInto) holds itself.
	made []*lisp.LVal
	// faults is the per-node chance, in 1/1000, of a value the validator
	// refuses or panics on.
	faults int
}

func (g *docGen) leaf() *lisp.LVal {
	switch g.rng.IntN(7) {
	case 0:
		return lisp.Int(g.rng.IntN(100))
	case 1:
		return lisp.String("s" + strconv.Itoa(g.rng.IntN(100)))
	case 2:
		return lisp.Nil()
	case 3:
		return lisp.Symbol(":kw")
	case 4:
		return lisp.Bytes([]byte{1, 2})
	case 5:
		return lisp.Float(0.5)
	default:
		return lisp.Bool(true)
	}
}

// fault returns a value the validator refuses.
func (g *docGen) fault() *lisp.LVal {
	switch g.rng.IntN(9) {
	case 0: // two dimensions
		return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(1)}), []*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	case 1: // no dimension
		return lisp.Array(lisp.QExpr(nil), []*lisp.LVal{lisp.Int(1)})
	case 2: // a list that holds itself
		v := lisp.SExpr([]*lisp.LVal{lisp.Int(1), nil})
		v.Cells[1] = v
		return v
	case 3: // a map that holds itself through a vector
		m := lisp.SortedMap()
		m.MapSetString("self", lisp.Vector([]*lisp.LVal{m}))
		return m
	case 4: // a tagged value with two payloads
		v := goldenTagged(lisp.Int(1))
		v.Cells = append(v.Cells, lisp.Int(2))
		return v
	case 5: // a quote with no payload
		return &lisp.LVal{Type: lisp.LQuote}
	case 6: // a host map whose Entries method fails
		v := g.in.hostMap("failing-"+strconv.Itoa(len(g.in.maps)), nil, nil)
		g.in.maps[v].err = goldenHostError
		return v
	case 7: // a Go nil cell: the walk refuses it with errNilCell
		return lisp.SExpr([]*lisp.LVal{lisp.Int(1), nil})
	default: // an array with no cells: refused as malformed (#963)
		return &lisp.LVal{Type: lisp.LArray}
	}
}

func (g *docGen) key() *lisp.LVal {
	switch g.rng.IntN(8) {
	case 0:
		return lisp.Int(g.rng.IntN(20) - 5)
	case 1:
		return lisp.Symbol("k" + strconv.Itoa(g.rng.IntN(20)))
	default:
		return lisp.String("k" + strconv.Itoa(g.rng.IntN(20)))
	}
}

func (g *docGen) value(depth int) *lisp.LVal {
	if g.rng.IntN(1000) < g.faults {
		return g.fault()
	}
	if depth <= 0 || g.rng.IntN(4) == 0 {
		return g.leaf()
	}
	if len(g.made) > 0 && g.rng.IntN(8) == 0 {
		return g.made[g.rng.IntN(len(g.made))]
	}
	n := g.rng.IntN(5)
	var v *lisp.LVal
	switch g.rng.IntN(11) {
	case 0, 1, 2: // the builtin sorted-map: string, symbol and int keys
		v = lisp.SortedMap()
		for range n {
			v.MapSetLVal(g.key(), g.value(depth-1))
		}
	case 3: // a map decoded from JSON, which has its own backing
		v = libjson.Load([]byte(`{"j0":1,"j1":[1,2],"j2":{"x":1}}`), false)
		for range n {
			v.MapSetString("k"+strconv.Itoa(g.rng.IntN(20)), g.value(depth-1))
		}
	case 4: // a host map that works
		pairs := make([]*lisp.LVal, n)
		for i := range pairs {
			pairs[i] = goldenPair(lisp.String("h"+strconv.Itoa(i)), g.value(depth-1))
		}
		v = g.in.hostMap("host-"+strconv.Itoa(len(g.in.maps)), pairs, nil)
	case 5: // a map with no backing
		v = lisp.SortedMapFromData(lisp.NewMapData(nil))
	case 6, 7:
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = g.value(depth - 1)
		}
		v = lisp.Vector(cells)
	case 8:
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = g.value(depth - 1)
		}
		if g.rng.IntN(2) == 0 {
			v = lisp.QExpr(cells)
		} else {
			v = lisp.SExpr(cells)
		}
	case 9:
		v = goldenTagged(g.value(depth - 1))
	default:
		v = &lisp.LVal{Type: lisp.LQuote, Cells: []*lisp.LVal{g.value(depth - 1)}}
	}
	g.made = append(g.made, v)
	return v
}

// cycleInto stores root in one of the builtin maps or sequences under it,
// which makes the document hold itself.
func (g *docGen) cycleInto(root *lisp.LVal) {
	for _, i := range g.rng.Perm(len(g.made)) {
		switch v := g.made[i]; {
		case v.Type == lisp.LSExpr && len(v.Cells) > 0:
			v.Cells[g.rng.IntN(len(v.Cells))] = root
			return
		case v.Type == lisp.LSortMap && g.in.maps[v] == nil && v.Map() != nil && v.Len() > 0:
			if rc := v.MapSetString("cycle", root); rc.Type != lisp.LError {
				return
			}
		}
	}
}

// document builds one random document.  Some are wrapped past the depth at
// which the walk records its path (cycleGuardDepth), and some are wide
// enough to switch the validity memo on (sharedWalkBudget).
func (g *docGen) document() *lisp.LVal {
	root := g.value(2 + g.rng.IntN(5))
	if g.rng.IntN(6) == 0 {
		g.cycleInto(root)
	}
	if g.rng.IntN(5) == 0 {
		// Under a chain of maps and vectors deeper than the guard depth.
		for i := range cycleGuardDepth + g.rng.IntN(8) {
			if i%2 == 0 {
				m := lisp.SortedMap()
				m.MapSetString("b", root)
				m.MapSetString("a", g.value(0))
				root = m
			} else {
				root = lisp.Vector([]*lisp.LVal{g.value(0), root})
			}
		}
	}
	if g.rng.IntN(5) == 0 {
		// After a filler that spends the validity memo's budget, and
		// twice, so that the second visit is a memo hit.
		filler := make([]*lisp.LVal, sharedWalkBudget+1)
		for i := range filler {
			filler[i] = lisp.Int(i)
		}
		m := lisp.SortedMap()
		m.MapSetString("a-filler", lisp.SExpr(filler))
		m.MapSetString("b", root)
		m.MapSetString("c", lisp.Vector([]*lisp.LVal{root, g.value(1)}))
		root = m
	}
	return root
}

// A random document validates as it did under the single ordered walk: the
// same error, the same panic, and the same calls to its host maps.
//
// The documents put more than one fault in a map, under keys of every type,
// so that the ordered walk and an unordered one meet different faults
// first.  About one document in twenty has such a pair of faults, so a
// validator that reported the fault its unordered pass met fails this test.
func TestValidatorMatchesOrderedWalk(t *testing.T) {
	const docs = 6000
	outcomes := map[string]int{}
	for seed := range uint64(docs) {
		rng := rand.New(rand.NewPCG(seed, 0x944)) //nolint:gosec // G404: a fixed seed makes the test repeatable
		g := &docGen{rng: rng, in: &goldenInput{maps: make(map[*lisp.LVal]*goldenMap)}}
		// A third of the documents have no fault, a third a few, and a
		// third many.
		g.faults = []int{0, 30, 250}[seed%3]
		doc := g.document()
		want := observeValidate(g.in, doc, oracleOkSimpleType)
		// Each side is run twice: the unordered pass reads a builtin
		// map in Go's random map order, which differs between runs.
		for range 2 {
			got := observeValidate(g.in, doc, okSimpleType)
			if !got.equal(want) {
				t.Fatalf("seed %d:\nvalidator: %v\nordered walk: %v", seed, got, want)
			}
		}
		switch {
		case want.panic != "":
			outcomes["panic"]++
		case want.cyclic:
			outcomes["cycle"]++
		case want.host:
			outcomes["host error"]++
		case want.err != "":
			outcomes["other error"]++
		default:
			outcomes["valid"]++
		}
	}
	// The comparison is only worth its name if the documents reach every
	// kind of outcome.  No generated document panics the walk since
	// luthersystems/elps#963 made a malformed array an error; a panic
	// still fails the comparison above.
	for _, kind := range []string{"valid", "cycle", "host error", "other error"} {
		if outcomes[kind] < docs/200 {
			t.Errorf("only %d of %d documents had the outcome %q", outcomes[kind], docs, kind)
		}
	}
	t.Logf("outcomes: %v", outcomes)
}

// A valid document of builtin maps is validated by the unordered pass
// alone, and a host map sends the walk to the ordered pass before any of
// its methods run.  The first is what the optimization rests on, and the
// second is what keeps a host map's calls as they were.
func TestValidatorPasses(t *testing.T) {
	doc := lisp.SortedMap()
	doc.MapSetString("a", lisp.Vector([]*lisp.LVal{lisp.SortedMap(), lisp.Int(1)}))
	doc.MapSet(7, lisp.SortedMap())
	if !okSimpleTypeUnordered(doc) {
		t.Fatal("the unordered pass refused a valid document")
	}

	in := &goldenInput{maps: make(map[*lisp.LVal]*goldenMap)}
	host := in.hostMap("host", []*lisp.LVal{goldenPair(lisp.String("a"), lisp.Int(1))}, nil)
	doc.MapSetString("host", host)
	if okSimpleTypeUnordered(doc) {
		t.Fatal("the unordered pass read a host map")
	}
	if len(in.trace) != 0 {
		t.Fatalf("the unordered pass called the host map: %v", in.trace)
	}
	if err := okSimpleType(doc); err != nil {
		t.Fatal(err)
	}
	if want := []string{"entries:host"}; !slices.Equal(in.trace, want) {
		t.Fatalf("host map calls: got %v, want %v", in.trace, want)
	}
}

// The unordered pass allocates one slice for the entries of the maps on its
// path, whatever the number of maps, and nothing for a document without a
// map.  The ordered walk built and sorted an entry list for each map.
func TestValidatorAllocationsDoNotScaleWithMaps(t *testing.T) {
	records := func(n int) *lisp.LVal {
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			inner := lisp.SortedMap()
			inner.MapSetString("x", lisp.Int(i))
			rec := lisp.SortedMap()
			rec.MapSetString("id", lisp.Int(i))
			rec.MapSetString("meta", inner)
			cells[i] = rec
		}
		return lisp.Vector(cells)
	}
	allocs := func(doc *lisp.LVal) float64 {
		return testing.AllocsPerRun(100, func() {
			if err := okSimpleType(doc); err != nil {
				t.Fatal(err)
			}
		})
	}
	// Both sizes stay below sharedWalkBudget, where the validity memo
	// starts to record containers.
	if small, large := allocs(records(4)), allocs(records(400)); small != 1 || large != 1 {
		t.Fatalf("validation allocated %v times for 4 records and %v for 400, want 1 and 1", small, large)
	}
	noMaps := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.SExpr([]*lisp.LVal{lisp.Int(2)})})
	if got := allocs(noMaps); got != 0 {
		t.Fatalf("validation of a document without a map allocated %v times, want 0", got)
	}
}
