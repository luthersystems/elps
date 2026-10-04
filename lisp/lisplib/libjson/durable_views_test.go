// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// sameAfterRestore builds a value with src twice.  It runs ops with r bound
// to the first build, and with r bound to the second after a durable round
// trip, and requires the same result: sharing through views behaves as it
// did before the save.  It returns the document.
func sameAfterRestore(t *testing.T, src, ops string) string {
	t.Helper()
	env := newTypedTestEnv(t)
	run := func(v *lisp.LVal) string {
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("r"), v)))
		return evalString(t, env, ops)
	}
	orig := env.LoadString("test", src)
	require.NoError(t, lisp.GoError(orig), src)
	want := run(orig)
	v := env.LoadString("test", src)
	require.NoError(t, lisp.GoError(v), src)
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err, src)
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err, string(b))
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	require.Equal(t, string(b), string(again), "not canonical")
	assert.Equal(t, want, run(back), "%s\nafter %s", ops, b)
	return string(b)
}

// roundTrip saves v, restores it, requires the restored value to save to
// the same bytes, and returns the document and the restored value.
func roundTrip(t *testing.T, env *lisp.LEnv, v *lisp.LVal, reg *libjson.DurableRegistry) (string, *lisp.LVal) {
	t.Helper()
	b, err := libjson.DumpDurable(env, v, reg)
	require.NoError(t, err)
	back, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err, string(b))
	again, err := libjson.DumpDurable(env, back, reg)
	require.NoError(t, err)
	require.Equal(t, string(b), string(again), "not canonical")
	return string(b), back
}

func TestDurableViewsKeepSharing(t *testing.T) {
	for _, c := range []struct {
		name, src, ops, doc string
	}{
		{
			name: "a list, its tail and a middle slice",
			src:  `(let* ((xs (list 0 3 2 1)) (tail (rest xs)) (mid (slice 'list xs 1 3))) (list xs tail mid))`,
			ops:  `(stable-sort < (second r)) r`,
			doc: `["~#durable",[1,["~#list",[["~#view",[["~#obj",[0,["~#cells",4]]],0,4,4,[0,3,2,1]]],` +
				`["~#view",[["~#ref",0],1,3,3,[]]],["~#view",[["~#ref",0],1,2,2,[]]]]]]]`,
		},
		{
			name: "a tail reached before its list",
			src:  `(let* ((xs (list 0 3 2 1)) (tail (rest xs))) (list tail xs))`,
			ops:  `(stable-sort < (first r)) r`,
			doc: `["~#durable",[1,["~#list",[["~#view",[["~#obj",[0,["~#cells",4]]],1,3,3,[3,2,1]]],` +
				`["~#view",[["~#ref",0],0,4,4,[0]]]]]]]`,
		},
		{
			name: "cdr of a list",
			src:  `(let* ((xs (list 5 4 3)) (tail (cdr xs))) (vector tail xs))`,
			ops:  `(stable-sort < (aref r 0)) r`,
		},
		{
			name: "two overlapping vector slices of a dead vector",
			src:  `(let ((v (vector 0 9 8 7 6 5 4 3 2 1))) (list (slice 'vector v 0 5) (slice 'vector v 3 8)))`,
			ops:  `(stable-sort < (second r)) r`,
		},
		{
			name: "a vector, its slice and append! in place",
			src:  `(let ((v (vector 3 2 1))) (append! v 0) (list v (slice 'vector v 0 2)))`,
			ops:  `(append! (first r) 7) (stable-sort < (first r)) r`,
		},
		{
			// The first append! gives v spare capacity, so the second
			// writes v's own slice into v's storage, in place.
			name: "a vector that holds its own slice",
			src:  `(let ((v (vector 3 2 1))) (append! v 0) (append! v (slice 'vector v 0 2)) v)`,
			ops:  `(stable-sort (lambda (a b) (and (int? a) (int? b) (< a b))) r) r`,
			doc: `["~#durable",[1,["~#array",[[5],["~#view",[["~#obj",[0,["~#cells",6]]],0,5,6,` +
				`[3,2,1,0,["~#array",[[2],["~#view",[["~#ref",0],0,2,2,[]]]]],null]]]]]]]`,
		},
		{
			// Views made after the restore see the restored capacity.
			name: "views made after the restore",
			src:  `(let ((v (vector 3 2 1))) (append! v 0) v)`,
			ops: `(let ((s (slice 'vector r 0 2)) (w (append! r 9)))` +
				` (stable-sort < r) (list r s (length r)))`,
		},
	} {
		t.Run(c.name, func(t *testing.T) {
			doc := sameAfterRestore(t, c.src, c.ops)
			if c.doc != "" {
				assert.Equal(t, c.doc, doc)
			}
		})
	}
}

// Spare capacity behind a vector is part of its storage: append! fills it
// in place, so a view over the vector keeps seeing the vector's writes.
func TestDurableViewsKeepCapacity(t *testing.T) {
	doc := sameAfterRestore(t,
		`(let ((v (vector 4 3 2 1))) (append! v 0) (list v (slice 'vector v 1 3)))`,
		`(append! (first r) 9) (stable-sort < (first r)) (list r (length (first r)))`)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#array",[[5],["~#view",[["~#obj",[0,["~#cells",8]]],0,5,8,[4,3,2,1,0,null,null,null]]]]],`+
		`["~#array",[[2],["~#view",[["~#ref",0],1,2,2,[]]]]]]]]]`, doc)
}

// A vector's capacity is saved even when nothing else shares its storage,
// so append! after a restore writes in place exactly when it did before.
func TestDurableVectorKeepsCapacity(t *testing.T) {
	env := newTypedTestEnv(t)
	v := env.LoadString("test", `(let ((v (vector 1 2 3))) (append! v 4) v)`)
	require.NoError(t, lisp.GoError(v))
	require.Equal(t, 6, cap(v.Cells[1].Cells))
	doc, back := roundTrip(t, env, v, nil)
	assert.Equal(t, `["~#durable",[1,["~#array",[[4],["~#view",[["~#cells",6],0,4,6,[1,2,3,4,null,null]]]]]]]`, doc)
	assert.Equal(t, 6, cap(back.Cells[1].Cells))

	// The vector and its own data list, reached as a list.
	for _, root := range []*lisp.LVal{
		lisp.QExpr([]*lisp.LVal{v, v.Cells[1]}),
		lisp.QExpr([]*lisp.LVal{v.Cells[1], v}),
	} {
		_, back := roundTrip(t, env, root, nil)
		vec, data := back.Cells[0], back.Cells[1]
		if vec.Type != lisp.LArray {
			vec, data = data, vec
		}
		assert.Same(t, vec.Cells[1], data, "the list is the vector's data list")
		assert.Equal(t, 6, cap(data.Cells))
		cells := &data.Cells[0]
		require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("v"), vec)))
		evalString(t, env, `(append! v 5)`)
		assert.Len(t, data.Cells, 5, "the list sees the vector's append!")
		assert.Same(t, cells, &data.Cells[0], "the append! wrote in place")
	}
}

// Data that no vector uses cannot be appended to, so its capacity is not
// saved: it is written as a list, or as a view with capacity equal to its
// length.
func TestDurableArrayDataCapacityNormalized(t *testing.T) {
	env := newTypedTestEnv(t)
	backing := make([]*lisp.LVal, 6, 10)
	for i := range backing {
		backing[i] = lisp.Int(i)
	}
	grid := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)}), lisp.QExpr(backing)}}
	doc, back := roundTrip(t, env, grid, nil)
	assert.Equal(t, `["~#durable",[1,["~#array",[[2,3],[0,1,2,3,4,5]]]]]`, doc)
	assert.Equal(t, 6, cap(back.Cells[1].Cells))

	doc, _ = roundTrip(t, env, lisp.QExpr([]*lisp.LVal{grid, lisp.QExpr(backing[2:4:4])}), nil)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#array",[[2,3],["~#view",[["~#obj",[0,["~#cells",6]]],0,6,6,[0,1,2,3,4,5]]]]],`+
		`["~#view",[["~#ref",0],2,2,2,[]]]]]]]`, doc)
}

// Two arrays of different dimensions over one data list, and a vector's
// data list held as a list, restore over one header.
func TestDurableViewsArrayDataHolders(t *testing.T) {
	env := newTypedTestEnv(t)
	data := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5), lisp.Int(6)})
	grid := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)}), data}}
	flat := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(6)}), data}}
	doc, back := roundTrip(t, env, lisp.QExpr([]*lisp.LVal{grid, flat}), nil)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#array",[[2,3],["~#obj",[0,["~#list",[1,2,3,4,5,6]]]]]],["~#array",[[6],["~#ref",0]]]]]]]`, doc)
	assert.Same(t, back.Cells[0].Cells[1], back.Cells[1].Cells[1])

	// Five cells: a Go append of five pointers rounds the capacity up to
	// six, which the restored data list must not keep.
	vec := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5)})
	doc, back = roundTrip(t, env, lisp.QExpr([]*lisp.LVal{vec, vec.Cells[1]}), nil)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#array",[[5],["~#obj",[0,["~#list",[1,2,3,4,5]]]]]],["~#ref",0]]]]]`, doc)
	assert.Equal(t, 5, cap(back.Cells[1].Cells))
	_, back = roundTrip(t, env, lisp.QExpr([]*lisp.LVal{vec.Cells[1], vec}), nil)
	assert.Equal(t, 5, cap(back.Cells[0].Cells))
	_, back = roundTrip(t, env, lisp.QExpr([]*lisp.LVal{vec, vec.Cells[1]}), nil)
	assert.Same(t, back.Cells[0].Cells[1], back.Cells[1], "the list is the vector's data list")
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("v"), back.Cells[0])))
	evalString(t, env, `(append! v 6)`)
	assert.Len(t, back.Cells[1].Cells, 6, "the list sees the vector's append!")
}

// A vector's data list, another list header over the same cells, and the
// vector restore the same way whatever order the walk meets them in.
func TestDurableDataHolderOrder(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, spare := range []bool{false, true} {
		v := lisp.Vector([]*lisp.LVal{lisp.Int(3), lisp.Int(2), lisp.Int(1)})
		if spare {
			v.Cells[1].Cells = append(make([]*lisp.LVal, 0, 5), v.Cells[1].Cells...)
		}
		d := v.Cells[1]
		alias := lisp.QExpr(d.Cells[:3:3])
		for _, order := range [][3]int{{0, 1, 2}, {0, 2, 1}, {1, 0, 2}, {1, 2, 0}, {2, 0, 1}, {2, 1, 0}} {
			vals := [3]*lisp.LVal{d, alias, v}
			root := lisp.QExpr([]*lisp.LVal{vals[order[0]], vals[order[1]], vals[order[2]]})
			doc, back := roundTrip(t, env, root, nil)
			var got [3]*lisp.LVal
			for i, k := range order {
				got[k] = back.Cells[i]
			}
			require.Equal(t, lisp.LArray, got[2].Type, doc)
			assert.Same(t, got[2].Cells[1], got[0], "%v: the list is the vector's data list: %s", order, doc)
			assert.NotSame(t, got[0], got[1], "%v: the alias is a header of its own: %s", order, doc)
			assert.Equal(t, cap(d.Cells), cap(got[0].Cells), "%v: %s", order, doc)
			got[1].Cells[0] = lisp.Int(9)
			assert.Equal(t, 9, got[0].Cells[0].Int, "%v: the alias shares the data's cells: %s", order, doc)
		}
	}
}

// A data list held by a sorted map is the array's data list after the
// restore.
func TestDurableMapHeldDataHolder(t *testing.T) {
	env := newTypedTestEnv(t)
	v := env.LoadString("test", `(let ((v (vector 1 2))) (append! v 3) v)`)
	require.NoError(t, lisp.GoError(v))
	m := lisp.SortedMap()
	m.MapSetLVal(lisp.String("a"), v.Cells[1])
	m.MapSetLVal(lisp.String("b"), v)
	_, back := roundTrip(t, env, m, nil)
	data, vec := back.MapGet(lisp.String("a")), back.MapGet(lisp.String("b"))
	assert.Same(t, vec.Cells[1], data)
	assert.Equal(t, cap(v.Cells[1].Cells), cap(data.Cells))
}

// An array inside its own data list restores: its size is checked once
// the data list is read.
func TestDurableArrayInsideItsData(t *testing.T) {
	env := newTypedTestEnv(t)
	for _, src := range []string{`(vector 1 2)`, `(let ((v (vector 1 2))) (append! v 3) v)`} {
		v := env.LoadString("test", src)
		require.NoError(t, lisp.GoError(v))
		v.Cells[1].Cells[1] = v
		_, back := roundTrip(t, env, lisp.QExpr([]*lisp.LVal{v.Cells[1], v}), nil)
		assert.Same(t, back.Cells[1], back.Cells[0].Cells[1], src)
		assert.Same(t, back.Cells[0], back.Cells[1].Cells[1], src)
	}
}

// An empty data list shared with a list value keeps its identity, so the
// document re-encodes to itself.
func TestDurableEmptyDataHolder(t *testing.T) {
	env := newTypedTestEnv(t)
	const doc = `["~#durable",[1,["~#list",[["~#array",[[0],["~#obj",[0,["~#list",[]]]]]],["~#ref",0]]]]]`
	back, err := libjson.LoadDurable(env, []byte(doc), nil)
	require.NoError(t, err)
	assert.Same(t, back.Cells[0].Cells[1], back.Cells[1])
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, doc, string(again))
	v := lisp.Vector(nil)
	got, _ := roundTrip(t, env, lisp.QExpr([]*lisp.LVal{v.Cells[1], v}), nil)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#obj",[0,["~#list",[]]]],["~#array",[[0],["~#ref",0]]]]]]]`, got)
}

// A native's payload may hold a view of the storage that holds the native
// when the view does not cover the native's cell.
func TestDurableNativeInViewStorage(t *testing.T) {
	env := newTypedTestEnv(t)
	reg, _ := countingRegistry(t, true)
	cells := []*lisp.LVal{nil, lisp.Int(1), lisp.Int(2)}
	xs, tail := lisp.QExpr(cells), lisp.QExpr(cells[1:3:3])
	cells[0] = lisp.Native(&boxed{tail})
	_, back := roundTrip(t, env, xs, reg)
	p := nativeOf[*boxed](back.Cells[0]).v
	p.Cells[0] = lisp.Int(9)
	assert.Equal(t, 9, back.Cells[1].Int, "the payload's tail shares the list's cells")

	// A view that covers the native's own cell is a real cycle.
	cells = []*lisp.LVal{nil, lisp.Int(1), lisp.Int(2)}
	xs = lisp.QExpr(cells)
	cells[0] = lisp.Native(&boxed{lisp.QExpr(cells[0:2:2])})
	_, err := libjson.DumpDurable(env, xs, reg)
	require.EqualError(t, err, `durable json: native "test:box" payload refers to a value that encloses the native`)
	_, err = libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#view",[["~#obj",[0,["~#cells",3]]],0,3,3,`+
		`[["~#native",["test:box",1,["~#view",[["~#ref",0],0,2,2,[1]]]]],2]]]]]`), reg)
	require.ErrorContains(t, err, `native "test:box" payload refers to a view cell that encloses the native`)
}

// A native payload can hold views when its codec keeps sharing.
func TestDurableViewsInNativePayload(t *testing.T) {
	env := newTypedTestEnv(t)
	reg, _ := countingRegistry(t, true)
	xs := env.LoadString("test", `(let* ((xs (list 3 2 1)) (tail (rest xs))) (list xs tail))`)
	require.NoError(t, lisp.GoError(xs))
	b, err := libjson.DumpDurable(env, lisp.Native(&boxed{xs}), reg)
	require.NoError(t, err)
	back, err := libjson.LoadDurable(env, b, reg)
	require.NoError(t, err)
	p := nativeOf[*boxed](back).v
	p.Cells[1].Cells[0] = lisp.Int(9)
	assert.Equal(t, 9, p.Cells[0].Cells[1].Int, "the tail shares the list's cells")
	// A codec that rebuilds its payload cannot keep a view.
	tree, _ := countingRegistry(t, false)
	_, err = libjson.DumpDurable(env, lisp.Native(&boxed{xs}), tree)
	require.EqualError(t, err, `durable json: native "test:box" payload shares a value, and its codec does not keep sharing`)
	// A vector with spare capacity is not shared, so it can.
	v := env.LoadString("test", `(let ((v (vector 1))) (append! v 2) v)`)
	require.NoError(t, lisp.GoError(v))
	_, back = roundTrip(t, env, lisp.Native(&boxed{v}), tree)
	assert.Equal(t, 4, cap(nativeOf[*boxed](back).v.Cells[1].Cells))
}

// Every cell of a storage counts once against the value limit, dead cells
// included, and DumpDurable and LoadDurable agree at every limit: the
// smallest value limit each accepts is the document's count.
func TestDurableViewsAtExactLimit(t *testing.T) {
	env := newTypedTestEnv(t)
	chain := func(n int) *lisp.LVal {
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = lisp.Int(i)
		}
		roots := []*lisp.LVal{lisp.QExpr(cells)}
		for i := 1; i < n; i++ {
			roots = append(roots, lisp.QExpr(cells[i:n:n]))
		}
		return lisp.QExpr(roots)
	}
	for _, c := range []struct {
		name  string
		src   string
		v     *lisp.LVal
		count int
	}{
		// list, view, storage object and cells, 3 cells, view, storage ref.
		{name: "three cells and a tail", src: `(let* ((xs (list 1 2 3)) (tail (rest xs))) (list xs tail))`, count: 9},
		{name: "five cells and a tail", src: `(let* ((xs (list 1 2 3 4 5)) (tail (rest xs))) (list xs tail))`, count: 11},
		{name: "a vector and its slice", src: `(let ((v (vector 4 3 2 1))) (append! v 0) (list v (slice 'vector v 1 3)))`},
		// list, then per view one view and one storage value (the first
		// two), and the 30 cells: 1 + 30*2 + 1 + 30.
		{name: "thirty cells and every tail", v: chain(30), count: 92},
	} {
		t.Run(c.name, func(t *testing.T) {
			v := c.v
			if v == nil {
				v = env.LoadString("test", c.src)
				require.NoError(t, lisp.GoError(v))
			}
			requireExactLimit(t, env, v, c.name)
			b, err := libjson.DumpDurable(env, v, nil)
			require.NoError(t, err)
			_, err = libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxBytes(len(b)-1))
			require.ErrorIs(t, err, libjson.ErrTypedLimit)
			dumpAt, loadAt := 0, 0
			for n := 1; n <= 200 && (dumpAt == 0 || loadAt == 0); n++ {
				if _, derr := libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxValues(n)); derr == nil && dumpAt == 0 {
					dumpAt = n
				}
				if _, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxValues(n)); lerr == nil && loadAt == 0 {
					loadAt = n
				}
			}
			require.NotZero(t, loadAt, string(b))
			assert.Equal(t, loadAt, dumpAt, "smallest accepted value limit, dump and load: %s", b)
			if c.count > 0 {
				assert.Equal(t, c.count, loadAt, string(b))
			}
		})
	}
	_, err := libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#view",[["~#cells",9999],0,1,9999,[1]]]]]`), nil)
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	_, err = libjson.LoadDurable(env, []byte(`["~#durable",[1,["~#view",[["~#cells",3],0,3,3,[1,2,3]]]]]`), nil, libjson.WithTypedMaxValues(4))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
}

// A shared empty data list is held to the nesting limit like any other
// container, so a document LoadDurable accepts at a depth limit dumps at
// that limit too.
func TestDurableEmptyDataHolderDepth(t *testing.T) {
	env := newTypedTestEnv(t)
	doc := []byte(`["~#durable",[1,["~#list",[["~#list",[["~#obj",[0,["~#list",[]]]]]],["~#array",[[0],["~#ref",0]]]]]]]`)
	back, err := libjson.LoadDurable(env, doc, nil)
	require.NoError(t, err)
	for depth := 1; depth <= 4; depth++ {
		_, lerr := libjson.LoadDurable(env, doc, nil, libjson.WithTypedMaxDepth(depth))
		_, derr := libjson.DumpDurable(env, back, nil, libjson.WithTypedMaxDepth(depth))
		require.Equal(t, derr == nil, lerr == nil, "depth %d: dump %v, load %v", depth, derr, lerr)
	}
	_, err = libjson.LoadDurable(env, doc, nil, libjson.WithTypedMaxDepth(2))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
}

func TestLoadDurableRejectsViews(t *testing.T) {
	env := newTypedTestEnv(t)
	const pre = `["~#durable",[1,`
	const post = `]]`
	two := func(a, b string) string { return pre + `["~#list",[` + a + `,` + b + `]]` + post }
	vec := func(n int, view string) string { return fmt.Sprintf(`["~#array",[[%d],%s]]`, n, view) }
	const base = `["~#obj",[0,["~#cells",3]]]`
	const ref = `["~#ref",0]`
	for _, c := range []struct{ doc, want string }{
		{two(`["~#view",[`+base+`,0,3,3,[1,2,3]]]`, `["~#view",[`+ref+`,4,1,1,[]]]`), "past its storage"},
		{two(`["~#view",[`+base+`,0,3,3,[1,2,3]]]`, `["~#view",[`+ref+`,1,3,2,[]]]`), "past its capacity"},
		{two(`["~#view",[`+base+`,0,3,3,[1,2,3]]]`, `["~#view",[`+ref+`,1,0,0,[]]]`), "empty view"},
		{two(`["~#view",[`+base+`,0,2,3,[1,2,null]]]`, `["~#view",[`+ref+`,1,2,2,[]]]`), "spare capacity is no vector's data"},
		{two(`["~#view",[`+base+`,0,3,3,[1,2,3]]]`, `["~#view",[`+ref+`,0,3,3,[]]]`), "two equal list views"},
		{two(`["~#view",[`+base+`,0,1,1,[1]]]`, `["~#view",[`+ref+`,1,2,2,[2,3]]]`), "not one run of overlapping views"},
		{two(`["~#view",[`+base+`,1,2,2,[2,3]]]`, `["~#view",[`+ref+`,1,1,1,[]]]`), "not one run of overlapping views"},
		{two(`["~#view",[`+base+`,0,2,2,[1,2]]]`, `["~#view",[`+ref+`,1,1,1,[]]]`), "cells no view covers"},
		{two(`["~#view",[`+base+`,0,2,2,[1,2]]]`, `["~#view",[`+ref+`,1,2,2,[null]]]`), ""},
		{two(`["~#view",[`+base+`,0,2,2,[1,2]]]`, `["~#view",[`+ref+`,1,2,2,[3,4]]]`), "expected ']'"},
		{two(`["~#view",[`+base+`,0,2,2,[1,2]]]`, `["~#view",[`+ref+`,1,2,2,[]]]`), "invalid value"},
		{two(`["~#view",[`+base+`,0,1,1,[1]]]`, vec(1, `["~#view",[`+ref+`,0,1,3,[2,3]]]`)), "dead but not null"},
		{pre + `["~#view",[` + base + `,0,3,3,[1,2,3]]]` + post, "object 0 is defined but never referenced"},
		{pre + `["~#view",[[1,2,3],0,3,3,[1,2,3]]]` + post, "a view's storage must be"},
		{pre + `["~#view",[["~#obj",[0,["~#list",[1]]]],0,1,1,[1]]]` + post, "a view's storage must be"},
		{two(`["~#obj",[0,["~#list",[1]]]]`, `["~#view",[`+ref+`,0,1,1,[]]]`), "which is not storage"},
		{two(`["~#view",[`+base+`,0,3,3,[1,2,3]]]`, ref), "reference to storage object 0 outside a view"},
		{pre + `["~#cells",3]` + post, "unknown tag"},
		{pre + `["~#view",[["~#cells",9999],0,1,9999,[1]]]` + post, "larger than the input allows"},
		{pre + `["~#array",[[2],["~#list",[1,2]]]]` + post, "must be a shared object, a reference or a view"},
		{pre + `["~#array",[[1],["~#obj",[0,{}]]]]` + post, "array data must be a list"},
		{pre + `["~#list",[["~#obj",[0,["~#list",[]]]],["~#ref",0]]]` + post, "shared empty list that is no array's data"},
		{pre + `["~#view",[["~#cells",3],0,3,3,[1,2,3]]]` + post, "not one vector's data with spare capacity"},
		{pre + vec(1, `["~#view",[["~#cells",3],1,1,2,[1,null]]]`) + post, "not one vector's data with spare capacity"},
		{pre + vec(1, `["~#view",[["~#cells",4],0,1,3,[1,null,null]]]`) + post, "not one vector's data with spare capacity"},
		{pre + vec(1, `["~#view",[["~#cells",3],0,1,2,[1,null]]]`) + post, "not one vector's data with spare capacity"},
		{pre + `["~#array",[[1,1],["~#view",[["~#cells",2],0,1,2,[1,null]]]]]` + post, "spare capacity is no vector's data"},
		{pre + vec(3, `["~#view",[["~#cells",4],0,2,4,[1,2,null,null]]]`) + post, "array contents do not match its dimensions"},
		{pre + vec(1, `["~#view",[["~#cells",2],0,1,2,[1,5]]]`) + post, "dead but not null"},
		{pre + vec(1, `["~#view",[["~#cells",4],0,1,4,[1,null,null,null]]]`) + post, ""},
		{pre + vec(0, `["~#view",[["~#cells",4],0,0,4,[null,null,null,null]]]`) + post, ""},
	} {
		t.Run(c.doc, func(t *testing.T) {
			_, err := libjson.LoadDurable(env, []byte(c.doc), nil)
			if c.want == "" {
				require.NoError(t, err)
				return
			}
			require.Error(t, err)
			assert.Contains(t, err.Error(), c.want)
		})
	}
}

// Capacity, and so the document, is the same for values built in a cold
// environment, in a VM of an eager or lazy template (prewarmed or not), and
// for values those VMs build themselves.
func TestDurableCapacityParity(t *testing.T) {
	const program = `
(set 'v (vector 1))
(append! v 2)
(append! v 3 4 5)
(set 's (slice 'vector v 1 3))
(set 'xs (list 1 2 3))
(set 'tail (rest xs))`
	dump := func(env *lisp.LEnv) string {
		t.Helper()
		var roots []libjson.DurableRoot
		for _, name := range []string{"s", "tail", "v", "xs"} {
			val := env.LoadString("test", name)
			require.NoError(t, lisp.GoError(val), name)
			roots = append(roots, libjson.DurableRoot{Name: name, Value: val})
		}
		b, err := libjson.DumpDurableRoots(env, roots, nil)
		require.NoError(t, err)
		return string(b)
	}
	// The runtime library: the testing registry cannot be published.
	source := lisp.NewEnv(nil)
	source.Runtime.Reader = parser.NewReader()
	require.NoError(t, lisp.GoError(lisp.InitializeUserEnv(source)))
	require.NoError(t, lisp.GoError(lisplib.LoadRuntimeLibrary(source)))
	require.NoError(t, lisp.GoError(source.InPackage(lisp.String(lisp.DefaultUserPackage))))
	require.NoError(t, lisp.GoError(source.LoadString("program", program)))
	want := dump(source)
	assert.Contains(t, want, `["~#cells",8]`, "append! grew v to capacity 8")
	policy := lisp.TemplateWithBuiltinPolicy(func(*lisp.LVal) bool { return true })
	eager, err := lisp.NewTemplate(source, policy, lisp.TemplateWithEagerInstantiation())
	require.NoError(t, err)
	lazy, err := lisp.NewTemplate(source, policy)
	require.NoError(t, err)
	for _, c := range []struct {
		name string
		tmpl *lisp.Template
		opts []lisp.VMOption
	}{
		{"eager", eager, nil},
		{"lazy", lazy, nil},
		{"lazy prewarmed", lazy, []lisp.VMOption{lisp.VMWithPrewarm()}},
	} {
		vm, err := c.tmpl.NewVM(c.opts...)
		require.NoError(t, err)
		assert.Equal(t, want, dump(vm), "%s: values from the template", c.name)
		require.NoError(t, lisp.GoError(vm.LoadString("program", program)))
		assert.Equal(t, want, dump(vm), "%s: values the VM built", c.name)
	}
}

// Discovery meets a container by another path than the output (a vector's
// spare capacity holds a list another view makes live), so the nesting
// limit is applied to the counting pass, not to discovery: dump and load
// agree at every depth.
func TestDurableViewsDepthAgreement(t *testing.T) {
	env := newTypedTestEnv(t)
	doc := []byte(`["~#durable",[1,["~#list",[["~#array",[[1],["~#view",[["~#obj",[0,["~#cells",2]]],0,1,2,[0,["~#list",[1]]]]]]],["~#list",[["~#view",[["~#ref",0],1,1,1,[]]]]]]]]]`)
	back, err := libjson.LoadDurable(env, doc, nil)
	require.NoError(t, err)
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	require.Equal(t, string(doc), string(again))
	for depth := 1; depth <= 6; depth++ {
		_, lerr := libjson.LoadDurable(env, doc, nil, libjson.WithTypedMaxDepth(depth))
		_, derr := libjson.DumpDurable(env, back, nil, libjson.WithTypedMaxDepth(depth))
		require.Equal(t, derr == nil, lerr == nil, "depth %d: dump %v, load %v", depth, derr, lerr)
	}
	_, err = libjson.DumpDurable(env, back, nil, libjson.WithTypedMaxDepth(3))
	require.NoError(t, err)
}
