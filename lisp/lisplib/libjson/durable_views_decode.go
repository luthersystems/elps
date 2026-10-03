// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"cmp"
	"slices"

	"github.com/luthersystems/elps/lisp"
)

// decStorage is one "~#cells" object being read or read: its cells, which
// cells were written as null, and the views of it.
type decStorage struct {
	cells []*lisp.LVal
	null  []bool
	views []*decView
}

// decView is one view of a storage object.  data is set once the view's
// header is used as an array's data list.
type decView struct {
	header                *lisp.LVal
	off, length, capacity int
	storage               int
	data, inline          bool
}

// view reads [STORAGE,OFF,LEN,CAP] after "~#view",[ and builds a list
// header over the storage's cells.  The header is defined before the
// storage is read, so a cell can refer to it.
func (d *durableDecoder) view(depth int, dataPos bool) (*lisp.LVal, error) {
	inline := d.pending < 0
	h := lisp.QExpr(nil)
	d.define(h)
	id, err := d.storageRef(depth)
	if err != nil {
		return nil, err
	}
	var nums [3]int
	for i := range nums {
		if err = d.expect(','); err != nil {
			return nil, err
		}
		if nums[i], err = d.index(); err != nil {
			return nil, err
		}
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	off, length, capacity := nums[0], nums[1], nums[2]
	st := d.storage[id]
	switch {
	case length > capacity:
		return nil, d.errorf("view length %d is past its capacity %d", length, capacity)
	case off > len(st.cells) || capacity > len(st.cells)-off:
		return nil, d.errorf("view [%d,%d) is past its storage of %d cells", off, off+capacity, len(st.cells))
	case capacity == 0:
		return nil, d.errorf("empty view")
	case inline && !dataPos && capacity != length:
		// A list holder never has spare capacity; only array data does.
		return nil, d.errorf("list view with spare capacity")
	}
	h.Cells = st.cells[off : off+length : off+capacity]
	dv := &decView{header: h, off: off, length: length, capacity: capacity, storage: id, data: dataPos, inline: inline}
	st.views = append(st.views, dv)
	d.views[h] = dv
	return h, nil
}

// storageRef reads a view's storage: ["~#obj",[ID,["~#cells",[N,[...]]]]]
// at its first use, ["~#ref",ID] after.  It returns the storage's id.
func (d *durableDecoder) storageRef(depth int) (int, error) {
	if d.tree > 0 {
		return 0, d.errorf("a payload of a codec that does not keep sharing holds a shared object")
	}
	if err := d.count(); err != nil {
		return 0, err
	}
	switch {
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagRef+`",`)):
		d.i += len(tagRef) + 4
		id, err := d.index()
		if err != nil {
			return 0, err
		}
		if id >= len(d.objs) || !d.storageObj[id] {
			return 0, d.errorf("view of object %d, which is not storage", id)
		}
		if err := d.expect(']'); err != nil {
			return 0, err
		}
		d.reach(id)
		d.used[id] = true
		return id, nil
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagObj+`",[`)):
		d.i += len(tagObj) + 5
	default:
		return 0, d.errorf("a view's storage must be a storage object or a reference to one")
	}
	id, err := d.index()
	if err != nil {
		return 0, err
	}
	if id != len(d.objs) {
		return 0, d.errorf("object id %d out of sequence", id)
	}
	if !bytes.HasPrefix(d.b[d.i:], []byte(`,["`+tagCells+`",[`)) {
		return 0, d.errorf("a view's storage must be a storage object or a reference to one")
	}
	d.i += len(tagCells) + 6
	if err = d.count(); err != nil {
		return 0, err
	}
	if err = d.depth(depth); err != nil {
		return 0, err
	}
	n, err := d.index()
	if err != nil {
		return 0, err
	}
	// Each cell costs a value and at least two bytes of input ("0,"), so
	// both bound the storage before it is allocated.
	if n > d.cfg.maxValues-d.values || n > (len(d.b)-d.i)/2+1 {
		return 0, d.errorf("storage of %d cells is larger than the input allows", n)
	}
	if err := d.expect(','); err != nil {
		return 0, err
	}
	if err := d.expect('['); err != nil {
		return 0, err
	}
	st := &decStorage{cells: make([]*lisp.LVal, n), null: make([]bool, n)}
	backing := lisp.QExpr(st.cells) // never returned; holds the storage for d.objs
	d.objs = append(d.objs, backing)
	d.open = append(d.open, true)
	d.low = append(d.low, noLow)
	d.used = append(d.used, false)
	d.storageObj = append(d.storageObj, true)
	d.frames = append(d.frames, noLow)
	d.storage[id] = st
	for k := range n {
		if k > 0 {
			if err := d.expect(','); err != nil {
				return 0, err
			}
		}
		st.null[k] = bytes.HasPrefix(d.b[d.i:], []byte("null"))
		c, err := d.value(depth + 1)
		if err != nil {
			return 0, err
		}
		st.cells[k] = c
	}
	// Close the cells, [N,…], "~#cells", [ID,…] and "~#obj".
	for range 5 {
		if err := d.expect(']'); err != nil {
			return 0, err
		}
	}
	low := d.popFrame()
	d.open[id] = false
	if low < id {
		d.low[id] = low
		d.reach(low)
	}
	return id, nil
}

// dataHolder reads an array's data written in its own form: a shared list
// (which may be empty), a reference to a list, or a view.
func (d *durableDecoder) dataHolder(depth int) (*lisp.LVal, error) {
	d.dataPos = true
	h, err := d.value(depth)
	d.dataPos = false
	if err != nil {
		return nil, err
	}
	if h.Type != lisp.LSExpr {
		return nil, d.errorf("array data must be a list")
	}
	if dv, ok := d.views[h]; ok {
		dv.data = true
	}
	return h, nil
}

// checkViews checks, after the whole document is read, that every storage
// object is exactly what DumpDurable writes: its views' ranges (a list's
// length, array data's capacity) overlap in one chain that covers it from
// its first cell to its last, every cell no view's length covers is null,
// a list view has no spare capacity, and no two list views are equal.
func (d *durableDecoder) checkViews() error {
	ids := make([]int, 0, len(d.storage))
	for id := range d.storage {
		ids = append(ids, id)
	}
	slices.Sort(ids)
	for _, id := range ids {
		st := d.storage[id]
		type span struct{ start, end int }
		spans := make([]span, 0, len(st.views))
		live := make([]bool, len(st.cells))
		lists := map[[2]int]bool{}
		for _, v := range st.views {
			end := v.off + v.length
			if v.data {
				end = v.off + v.capacity
			} else {
				if v.capacity != v.length {
					return d.errorf("list view of storage %d with spare capacity", id)
				}
				if lists[[2]int{v.off, v.length}] {
					return d.errorf("two equal list views of storage %d", id)
				}
				lists[[2]int{v.off, v.length}] = true
			}
			if end == v.off {
				return d.errorf("view of storage %d covers no cell", id)
			}
			spans = append(spans, span{v.off, end})
			for k := v.off; k < v.off+v.length; k++ {
				live[k] = true
			}
		}
		slices.SortFunc(spans, func(a, b span) int { return cmp.Compare(a.start, b.start) })
		reach := 0
		for i, s := range spans {
			if (i == 0 && s.start != 0) || (i > 0 && s.start >= reach) {
				return d.errorf("storage %d is not one run of overlapping views", id)
			}
			reach = max(reach, s.end)
		}
		if reach != len(st.cells) {
			return d.errorf("storage %d has cells no view covers", id)
		}
		for k, l := range live {
			if !l && !st.null[k] {
				return d.errorf("storage %d cell %d is dead but not null", id, k)
			}
		}
	}
	return nil
}
