// Copyright © 2026 The ELPS authors

package libjson

import (
	"errors"
	"math"
	"math/big"
	"reflect"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// TestDurableFunctionReserveWritesNothing pins the ~#fn reserve: one byte
// short of ["~#fn","NAME"], the encoder refuses before it writes a byte.
func TestDurableFunctionReserveWritesNothing(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatal(err)
	}
	f := env.LoadString("t", `(defun my-fn () 1) my-fn`)
	want := len(`["~#fn","user:my-fn"]`)
	e := newDurableEncoder(env, nil, newTypedConfig([]TypedOption{WithTypedMaxBytes(want - 1)}))
	if err := e.scan(f, 0); err != nil {
		t.Fatal(err)
	}
	if err := e.value(f, 0); err == nil {
		t.Fatal("no error one byte short")
	}
	if len(e.buf) != 0 {
		t.Fatalf("wrote %q before refusing", e.buf)
	}
	e = newDurableEncoder(env, nil, newTypedConfig([]TypedOption{WithTypedMaxBytes(want)}))
	if err := e.scan(f, 0); err != nil {
		t.Fatal(err)
	}
	if err := e.value(f, 0); err != nil || len(e.buf) != want {
		t.Fatalf("exact size: %q, %v", e.buf, err)
	}
}

// TestDurableIntegerKeyText pins the first pass's key accounting: integer
// keys count their "~i" text, so the key scratch stays under the byte limit.
func TestDurableIntegerKeyText(t *testing.T) {
	if strconv.IntSize < 64 {
		t.Skip("int is 32 bits")
	}
	env := lisp.NewEnv(nil)
	m := lisp.SortedMap()
	base := int64(1_000_000_000_000_000_000)
	for i := range 10000 {
		m.MapSetLVal(lisp.Int(int(base)+i), lisp.Int(0))
	}
	const limit = 65536
	e := newDurableEncoder(env, nil, newTypedConfig([]TypedOption{WithTypedMaxBytes(limit)}))
	e.scanning = true
	if err := e.scan(m, 0); !errors.Is(err, ErrTypedLimit) {
		t.Fatalf("scan: %v", err)
	}
	if len(e.keys) > limit {
		t.Fatalf("key scratch holds %d bytes, past the %d-byte limit", len(e.keys), limit)
	}
}

// nestedPairs returns struct{ L, R *A } nested n times over struct{ X int }:
// a type with 2^n paths to its innermost struct but only n+1 unnamed types.
func nestedPairs(n int) reflect.Type {
	a := reflect.TypeFor[struct{ X int }]()
	for range n {
		p := reflect.PointerTo(a)
		a = reflect.StructOf([]reflect.StructField{{Name: "L", Type: p}, {Name: "R", Type: p}})
	}
	return a
}

// TestTypeShapeLinear pins that a repeated unnamed subtype is written once
// and referenced after, so the shape is linear in the distinct types.
func TestTypeShapeLinear(t *testing.T) {
	shape := typeShape(nestedPairs(20), false)
	if len(shape) > 4096 {
		t.Fatalf("shape of a 20-level repeated subtype is %d bytes", len(shape))
	}
	if strings.Contains(shape, "...") {
		t.Fatalf("shape was cut short: %s", shape)
	}
	// Back-references keep different structures apart.
	same := reflect.TypeFor[struct{ A, B *struct{ X int } }]()
	diff := reflect.TypeFor[struct {
		A *struct{ X int }
		B *struct{ Y int }
	}]()
	if a, b := typeShape(same, false), typeShape(diff, false); a == b {
		t.Fatalf("one shape for two structures: %s", a)
	}
	if typeShape(nestedPairs(3), false) == typeShape(nestedPairs(4), false) {
		t.Fatal("one shape for two nesting depths")
	}
}

// A list and all of its tails is linear to save and to load: each view's
// claimed cells are found through skip links and a minimum tree, not cell
// by cell, and discovery walks each cell once.  Cell by cell, n tails cost
// n*n/2 steps (200 million here) in each direction.
func TestDurableAllTailsLinear(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatal(err)
	}
	const n = 20000
	cells := make([]*lisp.LVal, n)
	for i := range cells {
		cells[i] = lisp.Int(0)
	}
	roots := []*lisp.LVal{lisp.QExpr(cells)}
	for i := 1; i < n; i++ {
		roots = append(roots, lisp.QExpr(cells[i:n:n]))
	}
	const bound = 64 * n // about 2 log2(n) steps per view and per cell
	e := newDurableEncoder(env, nil, durableConfig(env, nil))
	b, err := e.dump(lisp.QExpr(roots))
	if err != nil {
		t.Fatal(err)
	}
	// The root list's n cells and the storage's n cells.
	if len(e.walked) != 2*n {
		t.Fatalf("discovery walked %d cell addresses, want %d", len(e.walked), 2*n)
	}
	ops := 0
	for _, st := range e.storages {
		ops += st.scan.ops + st.emit.ops
	}
	if ops > bound {
		t.Fatalf("dump took %d claim steps, want at most %d", ops, bound)
	}
	d := newDurableDecoder(env, b, nil, nil)
	if _, err = d.load(); err != nil {
		t.Fatal(err)
	}
	ops = 0
	for _, st := range d.storages {
		ops += st.claims.ops
	}
	if ops > bound {
		t.Fatalf("load took %d claim steps, want at most %d", ops, bound)
	}
	// The liveness check unions the views' lengths: a step per view and
	// per dead cell, not per covered cell.
	// At least a step per view: a check that counts nothing is not one.
	if d.liveOps < n || d.liveOps > 2*n {
		t.Fatalf("the liveness check took %d steps, want %d to %d", d.liveOps, n, 2*n)
	}

	// Sealed tails: each is a literal, and the load seals the union of
	// their ranges once.
	sealed := make([]*lisp.LVal, n)
	for i := range sealed {
		sealed[i] = lisp.Int(0)
	}
	head := lisp.QExpr(sealed)
	head.InheritSeal(lisp.Nil())
	roots = []*lisp.LVal{head}
	for i := 1; i < n; i++ {
		tail := lisp.QExpr(sealed[i:n:n])
		tail.InheritSeal(lisp.Nil())
		roots = append(roots, tail)
	}
	b, err = DumpDurable(env, lisp.QExpr(roots), nil)
	if err != nil {
		t.Fatal(err)
	}
	d = newDurableDecoder(env, b, nil, nil)
	if _, err := d.load(); err != nil {
		t.Fatal(err)
	}
	if len(d.literals) != n || d.sealOps < n || d.sealOps > 2*n {
		t.Fatalf("sealing %d literals took %d steps, want %d to %d", len(d.literals), d.sealOps, n, 2*n)
	}
}

// A long chain of empty frames is walked once per dump, not once per
// closure and pass: 20,000 closures at the end of 100,000 empty frames.
// Closures over one code object scan it once per pass.
func TestDurableClosureWorkBounded(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatal(err)
	}
	root := env
	for root.Parent() != nil {
		root = root.Parent()
	}
	const depth, closures, body = 100000, 20000, 2000
	leaf := root
	for range depth {
		leaf = lisp.NewEnv(leaf)
	}
	forms := make([]*lisp.LVal, body)
	for i := range forms {
		forms[i] = lisp.Int(i)
	}
	code, lerr := env.NewLambdaCode(lisp.SExpr(nil), forms)
	if lerr != nil {
		t.Fatal(lerr)
	}
	fs := make([]*lisp.LVal, closures)
	for i := range fs {
		fs[i] = leaf.RestoreLambda(lisp.DefaultUserPackage, code)
	}
	e := newDurableEncoder(env, nil, durableConfig(env, nil))
	if _, err := e.dump(lisp.QExpr(fs)); err != nil {
		t.Fatal(err)
	}
	if e.ancestorVisits < depth || e.ancestorVisits > depth+10 {
		t.Fatalf("walked %d environments, want %d", e.ancestorVisits, depth)
	}
	// Discovery and the counting pass each scan the code once: the
	// formals and the body.
	if want := 2 * (body + 1); e.codeVisits != want {
		t.Fatalf("scanned %d code nodes, want %d", e.codeVisits, want)
	}
}

// emptyChain returns a closure at the end of depth empty frames under the
// root, with an empty body.
func emptyChain(t *testing.T, depth int) (*lisp.LEnv, *lisp.LVal) {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatal(err)
	}
	root := env
	for root.Parent() != nil {
		root = root.Parent()
	}
	leaf := root
	for range depth {
		leaf = lisp.NewEnv(leaf)
	}
	code, lerr := env.NewLambdaCode(lisp.SExpr(nil), []*lisp.LVal{lisp.Int(1)})
	if lerr != nil {
		t.Fatal(lerr)
	}
	return env, leaf.RestoreLambda(lisp.DefaultUserPackage, code)
}

// A frame chain longer than the cap is refused when the walk crosses the
// cap, not after it walks the whole chain.
func TestDurableClosureFrameWalkCapped(t *testing.T) {
	const depth = 100000
	env, f := emptyChain(t, depth)
	cfg := durableConfig(env, []TypedOption{WithTypedMaxValues(10)})
	limit := frameWalkCap(cfg.maxValues)
	e := newDurableEncoder(env, nil, cfg)
	_, err := e.dump(f)
	if !errors.Is(err, ErrTypedLimit) {
		t.Fatalf("dump: %v, want ErrTypedLimit", err)
	}
	if e.ancestorVisits > limit+1 {
		t.Fatalf("walked %d environments, cap %d", e.ancestorVisits, limit)
	}
	if len(e.nearestFrame) > limit+1 {
		t.Fatalf("memoized %d environments, cap %d", len(e.nearestFrame), limit)
	}
}

// The frame walk is charged as it goes: a budget the walk crosses stops it
// there, with the budget's error.
func TestDurableClosureFrameWalkCharged(t *testing.T) {
	const depth, budget = 100000, 100
	env, f := emptyChain(t, depth)
	errBudget := errors.New("budget")
	used := 0
	charge := WithTypedCharge(func(n int) error {
		used += n
		if used > budget {
			return errBudget
		}
		return nil
	})
	e := newDurableEncoder(env, nil, durableConfig(env, []TypedOption{charge}))
	_, err := e.dump(f)
	if !errors.Is(err, errBudget) {
		t.Fatalf("dump: %v, want the budget's error", err)
	}
	if most := (budget + 1) * bindingsPerUnit; e.ancestorVisits > most {
		t.Fatalf("walked %d environments on a budget of %d units", e.ancestorVisits, budget)
	}
}

// The cap saturates instead of wrapping: on 386, 16*(1<<27)+1024 does not
// fit an int.
func TestDurableFrameWalkCap(t *testing.T) {
	maxInt := big.NewInt(math.MaxInt)
	for _, in := range []int{
		0, 10, 1 << 20, 1 << 26, 1 << 27, 1 << 30,
		math.MaxInt32 / 16, math.MaxInt32/16 + 1,
		(math.MaxInt - frameWalkSlack) / frameWalkFactor,
		(math.MaxInt-frameWalkSlack)/frameWalkFactor + 1,
		math.MaxInt,
	} {
		want := new(big.Int).Mul(big.NewInt(int64(in)), big.NewInt(frameWalkFactor))
		want.Add(want, big.NewInt(frameWalkSlack))
		if want.Cmp(maxInt) > 0 {
			want = maxInt
		}
		if got := frameWalkCap(in); int64(got) != want.Int64() {
			t.Errorf("frameWalkCap(%d) = %d, want %v", in, got, want)
		}
	}
}

// A large value limit does not wrap the cap and refuse a small closure
// (on 386, before frameWalkCap saturated).
func TestDurableClosureFrameWalkLargeLimit(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.MaxAlloc = 1 << 30
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatal(err)
	}
	f := env.Eval(lisp.SExpr([]*lisp.LVal{lisp.Symbol("let"),
		lisp.SExpr([]*lisp.LVal{lisp.SExpr([]*lisp.LVal{lisp.Symbol("x"), lisp.Int(1)})}),
		lisp.SExpr([]*lisp.LVal{lisp.Symbol("lambda"), lisp.SExpr(nil), lisp.Symbol("x")})}))
	if err := lisp.GoError(f); err != nil {
		t.Fatal(err)
	}
	if _, err := DumpDurable(env, f, nil, WithTypedMaxValues(1<<27)); err != nil {
		t.Fatal(err)
	}
}
