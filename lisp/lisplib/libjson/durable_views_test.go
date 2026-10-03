// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
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

func TestDurableViewsKeepSharing(t *testing.T) {
	for _, c := range []struct {
		name, src, ops, doc string
	}{
		{
			name: "a list, its tail and a middle slice",
			src:  `(let* ((xs (list 0 3 2 1)) (tail (rest xs)) (mid (slice 'list xs 1 3))) (list xs tail mid))`,
			ops:  `(stable-sort < (second r)) r`,
			doc: `["~#durable",[1,["~#list",[["~#view",[["~#obj",[0,["~#cells",[4,[0,3,2,1]]]]],0,4,4]],` +
				`["~#view",[["~#ref",0],1,3,3]],["~#view",[["~#ref",0],1,2,2]]]]]]`,
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
			name: "a vector that holds its own slice",
			src:  `(let ((v (vector 3 2 1))) (append! v (slice 'vector v 0 2)) v)`,
			ops:  `(stable-sort (lambda (a b) (and (int? a) (int? b) (< a b))) (aref r 3)) r`,
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
	assert.Contains(t, doc, `"~#cells"`)
	assert.Contains(t, doc, `null`, "dead spare capacity is written as null")
}

// Two arrays of different dimensions over one data list, and a vector's
// data list held as a list, restore over one header.
func TestDurableViewsArrayDataHolders(t *testing.T) {
	env := newTypedTestEnv(t)
	data := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4), lisp.Int(5), lisp.Int(6)})
	grid := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)}), data}}
	flat := &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(6)}), data}}
	b, err := libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{grid, flat}), nil)
	require.NoError(t, err)
	assert.Equal(t, `["~#durable",[1,["~#list",[["~#array",[[2,3],["~#obj",[0,["~#list",[1,2,3,4,5,6]]]]]],["~#array",[[6],["~#ref",0]]]]]]]`, string(b))
	back, err := libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.Same(t, back.Cells[0].Cells[1], back.Cells[1].Cells[1])
	again, err := libjson.DumpDurable(env, back, nil)
	require.NoError(t, err)
	assert.Equal(t, string(b), string(again))

	vec := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	b, err = libjson.DumpDurable(env, lisp.QExpr([]*lisp.LVal{vec, vec.Cells[1]}), nil)
	require.NoError(t, err)
	back, err = libjson.LoadDurable(env, b, nil)
	require.NoError(t, err)
	assert.Same(t, back.Cells[0].Cells[1], back.Cells[1], "the list is the vector's data list")
	require.NoError(t, lisp.GoError(env.PutGlobal(lisp.Symbol("v"), back.Cells[0])))
	evalString(t, env, `(append! v 3)`)
	assert.Len(t, back.Cells[1].Cells, 3, "the list sees the vector's append!")
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
}

// The hidden cells of a storage count against the limits like any other.
func TestDurableViewsAtExactLimit(t *testing.T) {
	env := newTypedTestEnv(t)
	v := env.LoadString("test", `(let ((v (vector 4 3 2 1))) (append! v 0) (list v (slice 'vector v 1 3)))`)
	require.NoError(t, lisp.GoError(v))
	requireExactLimit(t, env, v, "views")
	b, err := libjson.DumpDurable(env, v, nil)
	require.NoError(t, err)
	_, err = libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxBytes(len(b)-1))
	require.ErrorIs(t, err, libjson.ErrTypedLimit)
	for n := 1; n < 40; n++ {
		_, derr := libjson.DumpDurable(env, v, nil, libjson.WithTypedMaxValues(n))
		_, lerr := libjson.LoadDurable(env, b, nil, libjson.WithTypedMaxValues(n))
		require.Equal(t, derr == nil, lerr == nil, "value limit %d: dump %v, load %v", n, derr, lerr)
	}
}

func TestLoadDurableRejectsViews(t *testing.T) {
	env := newTypedTestEnv(t)
	const pre = `["~#durable",[1,`
	const post = `]]`
	two := func(a, b string) string { return pre + `["~#list",[` + a + `,` + b + `]]` + post }
	base := `["~#obj",[0,["~#cells",[3,[1,2,3]]]]]`
	for _, c := range []struct{ doc, want string }{
		{two(`["~#view",[`+base+`,0,3,3]]`, `["~#view",[["~#ref",0],4,1,1]]`), "past its storage"},
		{two(`["~#view",[`+base+`,0,3,3]]`, `["~#view",[["~#ref",0],1,3,2]]`), "past its capacity"},
		{two(`["~#view",[`+base+`,0,3,3]]`, `["~#view",[["~#ref",0],1,0,0]]`), "empty view"},
		{two(`["~#view",[`+base+`,0,2,3]]`, `["~#view",[["~#ref",0],1,2,2]]`), "list view with spare capacity"},
		{two(`["~#view",[`+base+`,0,3,3]]`, `["~#view",[["~#ref",0],0,3,3]]`), "two equal list views"},
		{two(`["~#view",[`+base+`,0,1,1]]`, `["~#view",[["~#ref",0],1,2,2]]`), "not one run of overlapping views"},
		{two(`["~#view",[`+base+`,1,2,2]]`, `["~#view",[["~#ref",0],1,1,1]]`), "not one run of overlapping views"},
		{two(`["~#view",[`+base+`,0,2,2]]`, `["~#view",[["~#ref",0],1,1,1]]`), "cells no view covers"},
		{two(`["~#view",[["~#obj",[0,["~#cells",[3,[1,2,null]]]]],0,2,2]]`, `["~#view",[["~#ref",0],1,2,2]]`), ""},
		{two(`["~#view",[["~#obj",[0,["~#cells",[3,[1,2,3]]]]],0,1,1]]`, `["~#array",[[1],["~#view",[["~#ref",0],0,1,3]]]]`), "dead but not null"},
		{pre + `["~#view",[` + base + `,0,3,3]]` + post, "object 0 is defined but never referenced"},
		{pre + `["~#view",[[1,2,3],0,3,3]]` + post, "storage must be a storage object"},
		{pre + `["~#view",[["~#obj",[0,["~#list",[1]]]],0,1,1]]` + post, "storage must be a storage object"},
		{two(`["~#obj",[0,["~#list",[1]]]]`, `["~#view",[["~#ref",0],0,1,1]]`), "which is not storage"},
		{two(`["~#view",[`+base+`,0,3,3]]`, `["~#ref",0]`), "reference to storage object 0 outside a view"},
		{pre + `["~#cells",[1,[1]]]` + post, "unknown tag"},
		{pre + `["~#view",[["~#obj",[0,["~#cells",[9999,[1]]]]],0,1,1]]` + post, "larger than the input allows"},
		{pre + `["~#array",[[2],["~#list",[1,2]]]]` + post, "must be a shared object, a reference or a view"},
		{pre + `["~#array",[[1],["~#obj",[0,{}]]]]` + post, "array data must be a list"},
		{pre + `["~#list",[["~#obj",[0,["~#list",[]]]],["~#ref",0]]]` + post, "empty list must be null"},
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
