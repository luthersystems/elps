// Copyright © 2026 The ELPS authors

package libjson

import (
	"bytes"
	"cmp"
	"errors"
	"fmt"
	"slices"

	"github.com/luthersystems/elps/lisp"
)

// decStorage is one "~#cells" storage being read: its cells, which cells
// were written as null, the cells views have claimed and their nodes, and
// the views of it.  id is its object id, -1 when inline.
type decStorage struct {
	claims *cellClaims
	cells  []*lisp.LVal
	null   []bool
	views  []*decView
	id     int
}

// decView is one view of a storage.  data is set once an array uses the
// view's header as its data, vector once a vector does.
type decView struct {
	header                *lisp.LVal
	storage               *decStorage
	off, length, capacity int
	data, vector          bool
}

// holderArray is an array whose data is a holder of its own: the holder
// must have size cells once the document is read.
type holderArray struct {
	data     *lisp.LVal
	size, at int
}

// view reads [STORAGE,OFF,LEN,CAP,[CELLS...]] after "~#view",[ and builds
// a list header over the storage's cells.  The header is defined before
// anything else is read, so a cell can refer to it.  CELLS are the cells of
// [OFF,OFF+CAP) that no earlier view claimed, in offset order; this view
// claims them.  The cells earlier views claimed are reached through their
// nodes, so the cycle check sees what this view's cells reach.
func (d *durableDecoder) view(depth int) (*lisp.LVal, error) {
	h := lisp.QExpr(nil)
	d.define(h)
	st, err := d.storageRef(depth)
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
	off, length, capacity := nums[0], nums[1], nums[2]
	switch {
	case length > capacity:
		return nil, d.errorf("view length %d is past its capacity %d", length, capacity)
	case off > len(st.cells) || capacity > len(st.cells)-off:
		return nil, d.errorf("view [%d,%d) is past its storage of %d cells", off, off+capacity, len(st.cells))
	case capacity == 0:
		return nil, d.errorf("empty view")
	}
	h.Cells = st.cells[off : off+length : off+capacity]
	dv := &decView{header: h, storage: st, off: off, length: length, capacity: capacity}
	st.views = append(st.views, dv)
	d.views[h] = dv
	if err = d.expect(','); err != nil {
		return nil, err
	}
	if err = d.expect('['); err != nil {
		return nil, err
	}
	first := true
	end := off + capacity
	for k := st.claims.free(off); k < end; k = st.claims.free(k + 1) {
		if !first {
			if err := d.expect(','); err != nil {
				return nil, err
			}
		}
		first = false
		node := d.openNode(-1)
		st.claims.claim(k, node)
		st.null[k] = bytes.HasPrefix(d.b[d.i:], []byte("null"))
		c, err := d.value(depth + 1)
		if err != nil {
			return nil, err
		}
		st.cells[k] = c
		d.closeNode(node)
	}
	// The cells of the range earlier views claimed (see cellClaims).
	if n := st.claims.reach(off, end, d.open, d.low); n != noLow {
		d.reach(n)
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	return h, nil
}

// storageRef reads a view's storage: ["~#cells",N] inline,
// ["~#obj",[ID,["~#cells",N]]] at a shared storage's first use, and
// ["~#ref",ID] after.
func (d *durableDecoder) storageRef(depth int) (*decStorage, error) {
	if err := d.count(); err != nil {
		return nil, err
	}
	id := -1
	switch {
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagCells+`",`)):
		d.i += len(tagCells) + 4
		return d.newStorage(depth, id)
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagRef+`",`)):
		if d.tree > 0 {
			return nil, d.errorf("a payload of a codec that does not keep sharing holds a reference")
		}
		d.i += len(tagRef) + 4
		ref, err := d.index()
		if err != nil {
			return nil, err
		}
		if ref >= len(d.objs) || !d.storageObj[ref] {
			return nil, d.errorf("view of object %d, which is not storage", ref)
		}
		if err := d.expect(']'); err != nil {
			return nil, err
		}
		d.used[ref] = true
		return d.storage[ref], nil
	case bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagObj+`",[`)):
		if d.tree > 0 {
			return nil, d.errorf("a payload of a codec that does not keep sharing holds a shared object")
		}
		d.i += len(tagObj) + 5
	default:
		return nil, d.errorf("a view's storage must be storage, a storage object or a reference to one")
	}
	id, err := d.index()
	if err != nil {
		return nil, err
	}
	if id != len(d.objs) {
		return nil, d.errorf("object id %d out of sequence", id)
	}
	if !bytes.HasPrefix(d.b[d.i:], []byte(`,["`+tagCells+`",`)) {
		return nil, d.errorf("a view's storage must be storage, a storage object or a reference to one")
	}
	d.i += len(tagCells) + 5
	if err = d.count(); err != nil {
		return nil, err
	}
	st, err := d.newStorage(depth, id)
	if err != nil {
		return nil, err
	}
	// Close "~#obj",[ID,…].
	for range 2 {
		if err := d.expect(']'); err != nil {
			return nil, err
		}
	}
	return st, nil
}

// newStorage reads N] after "~#cells", and allocates the storage's cells.
// Every cell is written later in the input, each as a value and with at
// least a byte and a comma, so both limits bound the storage before it is
// allocated.
func (d *durableDecoder) newStorage(depth, id int) (*decStorage, error) {
	if err := d.depth(depth); err != nil {
		return nil, err
	}
	n, err := d.index()
	if err != nil {
		return nil, err
	}
	if n > d.cfg.maxValues-d.values {
		return nil, fmt.Errorf("%w: more than %d values", ErrTypedLimit, d.cfg.maxValues)
	}
	if n > (len(d.b)-d.i)/2+1 {
		return nil, fmt.Errorf("%w: storage of %d cells is larger than the input allows", ErrTypedLimit, n)
	}
	if err := d.expect(']'); err != nil {
		return nil, err
	}
	st := &decStorage{cells: make([]*lisp.LVal, n), null: make([]bool, n), claims: newCellClaims(n), id: id}
	d.storages = append(d.storages, st)
	if id >= 0 {
		// The backing list is never returned; it holds the storage's slot
		// in d.objs.
		d.objs = append(d.objs, lisp.QExpr(st.cells))
		d.objNode = append(d.objNode, -1)
		d.used = append(d.used, false)
		d.storageObj = append(d.storageObj, true)
		d.storage[id] = st
	}
	return st, nil
}

// dataHolder reads an array's data written in its own form: a shared list
// (which may be empty), a reference to a list, or a view.  vector marks a
// vector's data.
func (d *durableDecoder) dataHolder(depth int, vector bool) (*lisp.LVal, error) {
	d.dataPos = true
	h, err := d.value(depth)
	d.dataPos = false
	if err != nil {
		return nil, err
	}
	if h.Type != lisp.LSExpr {
		return nil, d.errorf("array data must be a list")
	}
	d.dataUsed[h] = d.dataUsed[h] || vector
	if dv, ok := d.views[h]; ok {
		dv.data = true
		dv.vector = dv.vector || vector
	}
	return h, nil
}

// checkHolders checks, after the whole document is read, what can only be
// checked then: each array's data has the size its dimensions give, each
// empty list object is an array's data, and each storage is exactly what
// DumpDurable writes.  A storage's views' ranges [OFF,OFF+CAP) overlap in
// one chain that covers it from its first cell to its last; every cell no
// view's length covers is null; only a vector's data has spare capacity;
// no two list views are equal; and inline storage has one view, of a
// vector's data with spare capacity, that covers it.
func (d *durableDecoder) checkHolders() error {
	for _, a := range d.arrays {
		if len(a.data.Cells) != a.size {
			return fmt.Errorf("typed json: offset %d: array contents do not match its dimensions", a.at)
		}
	}
	for _, h := range d.emptyObjs {
		if _, ok := d.dataUsed[h]; !ok {
			return errors.New("durable json: a shared empty list that is no array's data")
		}
	}
	for _, st := range d.storages {
		if err := d.checkStorage(st); err != nil {
			return err
		}
	}
	return nil
}

func (d *durableDecoder) checkStorage(st *decStorage) error {
	name := "inline storage"
	if st.id >= 0 {
		name = fmt.Sprintf("storage %d", st.id)
	}
	if st.id < 0 {
		v := st.views[0]
		if v.off != 0 || v.capacity != len(st.cells) || v.capacity == v.length {
			return fmt.Errorf("durable json: %s is not one vector's data with spare capacity", name)
		}
	}
	type span struct{ start, end int }
	spans := make([]span, 0, len(st.views))
	lives := make([]span, 0, len(st.views))
	lists := map[[2]int]bool{}
	for _, v := range st.views {
		if v.capacity != v.length && !v.vector {
			return fmt.Errorf("durable json: view of %s with spare capacity is no vector's data", name)
		}
		if !v.data {
			if lists[[2]int{v.off, v.length}] {
				return fmt.Errorf("durable json: two equal list views of %s", name)
			}
			lists[[2]int{v.off, v.length}] = true
		}
		spans = append(spans, span{v.off, v.off + v.capacity})
		lives = append(lives, span{v.off, v.off + v.length})
	}
	slices.SortFunc(spans, func(a, b span) int { return cmp.Compare(a.start, b.start) })
	reach := 0
	for i, s := range spans {
		if (i == 0 && s.start != 0) || (i > 0 && s.start >= reach) {
			return fmt.Errorf("durable json: %s is not one run of overlapping views", name)
		}
		reach = max(reach, s.end)
	}
	if reach != len(st.cells) {
		return fmt.Errorf("durable json: %s has cells no view covers", name)
	}
	// The live cells are the union of the views' lengths; each dead cell,
	// between and after them, is checked once.
	slices.SortFunc(lives, func(a, b span) int { return cmp.Compare(a.start, b.start) })
	dead := 0
	for _, l := range append(lives, span{len(st.cells), len(st.cells)}) {
		for k := dead; k < l.start; k++ {
			if !st.null[k] {
				return fmt.Errorf("durable json: %s cell %d is dead but not null", name, k)
			}
		}
		dead = max(dead, l.end)
	}
	return nil
}

// literal reads X after "~#lit", (a list or a view) and marks its header
// as a program literal, so the mutators that refuse literals refuse it.
func (d *durableDecoder) literal(depth int) (*lisp.LVal, error) {
	if !bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagList+`",`)) && !bytes.HasPrefix(d.b[d.i:], []byte(`["`+tagView+`",`)) {
		return nil, d.errorf("a literal marker must wrap a list or a view")
	}
	h, err := d.value(depth)
	if err != nil {
		return nil, err
	}
	h.InheritSeal(lisp.Nil())
	return h, nil
}
