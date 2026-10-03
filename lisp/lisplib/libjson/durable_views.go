// Copyright © 2026 The ELPS authors

package libjson

// Views: lists and array data that share storage.
//
// A list's cells and a vector's data are Go slices.  rest, cdr and slice
// return new headers over the same backing array, and append! on a vector
// grows its data in place while capacity allows.  So two values can share
// storage without being one value: a write through one (stable-sort,
// append!) is seen through the other.  DumpDurable keeps that sharing.
//
// A holder is a header that owns a slice of cells: a nonempty list, or an
// array's data list.  Holders are found in a first pass over the graph
// (discovery), then grouped by the address ranges their cells cover: a
// list covers its length, an array's data covers its capacity (append!
// writes there).  Holders whose ranges overlap form one group.  A group of
// two or more holders is written as one storage object,
// ["~#cells",[N,[cells...]]], and each holder as a view of it,
// ["~#view",[STORAGE,OFF,LEN,CAP]].  Storage cells no holder's length
// covers are dead: no value can read them before an append writes them, so
// they are written as null.  Addresses decide only which holders overlap
// and their offsets, both properties of the memory layout; every order the
// document shows comes from the graph walk.

import (
	"cmp"
	"fmt"
	"reflect"
	"slices"

	"github.com/luthersystems/elps/lisp"
)

// The view extension tags.
const (
	tagView  = "~#view"
	tagCells = "~#cells"
)

// holderKey identifies an array's data list by its header: append! on the
// vector replaces the header's cells, so two arrays over one header see
// each other's appends, and a list value that is that header sees them too.
type holderKey struct{ p *lisp.LVal }

// storageKey identifies one group's storage.
type storageKey struct{ g int }

// holderRec is one holder found by discovery.
type holderRec struct {
	key    any
	header *lisp.LVal
	cells  []*lisp.LVal // the holder's cells, with their capacity
	start  uintptr
	width  int // cells the holder's range covers: len for a list, cap for array data
	data   bool
}

// viewInfo places a grouped holder in its group's storage.
type viewInfo struct {
	group, off, length, capacity int
}

// storageInfo is one group's storage: the live cells by offset (nil where
// dead) and which offsets are live.
type storageInfo struct {
	cells []*lisp.LVal
	live  []bool
}

var cellSize = reflect.TypeFor[*lisp.LVal]().Size()

// cellAddr is the address of the first cell of s[:cap(s)].
func cellAddr(s []*lisp.LVal) uintptr {
	return reflect.ValueOf(&s[:cap(s)][0]).Pointer()
}

// recordList notes a nonempty list holder during discovery.
func (e *durableEncoder) recordList(key any, v *lisp.LVal) {
	e.holders = append(e.holders, holderRec{key: key, header: v, cells: v.Cells, start: cellAddr(v.Cells), width: len(v.Cells)})
}

// recordData notes an array's data list during discovery.
func (e *durableEncoder) recordData(d *lisp.LVal) {
	e.dataHeaders[d] = true
	if cap(d.Cells) == 0 {
		return
	}
	e.holders = append(e.holders, holderRec{key: holderKey{d}, header: d, cells: d.Cells, start: cellAddr(d.Cells), width: cap(d.Cells), data: true})
}

// groupHolders turns the discovered holders into groups.  A list holder
// whose header is an array's data list is that data holder.  It reserves
// one value per storage cell against the value limit before it builds any
// storage.
func (e *durableEncoder) groupHolders() error {
	recs := e.holders[:0:0]
	seen := map[any]int{}
	for _, h := range e.holders {
		if e.dataHeaders[h.header] {
			// A list value whose header is an array's data list is that
			// data holder, whatever key discovery gave it first.
			if !h.data {
				continue
			}
		}
		if _, dup := seen[h.key]; dup {
			continue
		}
		seen[h.key] = len(recs)
		recs = append(recs, h)
	}
	slices.SortFunc(recs, func(a, b holderRec) int { return cmp.Compare(a.start, b.start) })
	total := 0
	for i := 0; i < len(recs); {
		j := i + 1
		end := recs[i].start + uintptr(recs[i].width)*cellSize
		for j < len(recs) && recs[j].start < end {
			end = max(end, recs[j].start+uintptr(recs[j].width)*cellSize)
			j++
		}
		if j-i >= 2 {
			n := int((end - recs[i].start) / cellSize)
			total += n
			if total > e.cfg.maxValues {
				return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
			}
			g := len(e.storages)
			st := storageInfo{cells: make([]*lisp.LVal, n), live: make([]bool, n)}
			for _, h := range recs[i:j] {
				off := int((h.start - recs[i].start) / cellSize)
				capacity := len(h.cells)
				if h.data {
					capacity = cap(h.cells)
				}
				e.views[h.key] = viewInfo{group: g, off: off, length: len(h.cells), capacity: capacity}
				for k, c := range h.cells {
					st.cells[off+k] = c
					st.live[off+k] = true
				}
			}
			e.storages = append(e.storages, st)
		}
		i = j
	}
	return nil
}

// identity is durableIdentity with the encoder's view of holders: a list
// whose header is an array's data list is that holder, and an array is its
// dims and data headers.
func (e *durableEncoder) identity(v *lisp.LVal) (any, bool) {
	if v.Type == lisp.LSExpr && len(v.Cells) > 0 && e.dataHeaders[v] {
		return holderKey{v}, true
	}
	if v.Type == lisp.LArray && len(v.Cells) == 2 {
		return arrayKey{v.Cells[0], v.Cells[1]}, true
	}
	return durableIdentity(v)
}

// scanStorage visits a group's storage: its cells once, in offset order.
func (e *durableEncoder) scanStorage(g, depth int) error {
	key := storageKey{g}
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	st := e.storages[g]
	if err := e.countScan(len(st.cells)); err != nil {
		return err
	}
	if err := e.cfg.depthError(depth); err != nil {
		return err
	}
	i := e.openObject(key)
	for k, c := range st.cells {
		if !st.live[k] {
			continue
		}
		if err := e.scan(c, depth+1); err != nil {
			return err
		}
	}
	e.closeObject(key, i)
	return nil
}

// scanHolder visits a holder's contents: its group's storage when it is a
// view, else its own cells.
func (e *durableEncoder) scanHolder(key any, cells []*lisp.LVal, depth int) error {
	i := e.openObject(key)
	if vi, ok := e.views[key]; ok {
		if err := e.scanStorage(vi.group, depth); err != nil {
			return err
		}
	} else {
		for _, c := range cells {
			if err := e.scan(c, depth+1); err != nil {
				return err
			}
		}
	}
	e.closeObject(key, i)
	return nil
}

// scanData visits an array's data list as a holder of its own.
func (e *durableEncoder) scanData(d *lisp.LVal, depth int) error {
	key := holderKey{d}
	e.refs[key]++
	if e.refs[key] > 1 {
		return e.revisit(nil, key)
	}
	return e.scanHolder(key, d.Cells, depth)
}

// separateData reports whether array data d is written as a holder of its
// own (shared, or a view) rather than inline.
func (e *durableEncoder) separateData(d *lisp.LVal) bool {
	key := holderKey{d}
	_, view := e.views[key]
	return view || e.refs[key] >= 2
}

// holder writes a holder (with its object wrapper when shared): a view, or
// a tagged list of its cells (empty only as array data).
func (e *durableEncoder) holder(key any, cells []*lisp.LVal, depth int) error {
	if e.refs[key] >= 2 {
		return e.object(key, depth, func() error { return e.holderBody(key, cells, depth) })
	}
	return e.holderBody(key, cells, depth)
}

func (e *durableEncoder) holderBody(key any, cells []*lisp.LVal, depth int) error {
	if err := e.container(depth); err != nil {
		return err
	}
	vi, ok := e.views[key]
	if !ok {
		e.buf = append(e.buf, `["`+tagList+`",`...)
		if err := e.cells(cells, depth); err != nil {
			return err
		}
		e.buf = append(e.buf, ']')
		return e.grow()
	}
	e.buf = append(e.buf, `["`+tagView+`",[`...)
	if err := e.storage(vi.group, depth); err != nil {
		return err
	}
	e.buf = fmt.Appendf(e.buf, ",%d,%d,%d]]", vi.off, vi.length, vi.capacity)
	return e.grow()
}

// storage writes a group's storage.  It is always shared (two or more
// views refer to it).
func (e *durableEncoder) storage(g, depth int) error {
	key := storageKey{g}
	return e.object(key, depth, func() error {
		st := e.storages[g]
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = fmt.Appendf(e.buf, `["`+tagCells+`",[%d,[`, len(st.cells))
		for k, c := range st.cells {
			if k > 0 {
				e.buf = append(e.buf, ',')
			}
			if !st.live[k] {
				if err := e.typedEncoder.value(lisp.Nil(), depth+1); err != nil {
					return err
				}
				continue
			}
			if err := e.value(c, depth+1); err != nil {
				return err
			}
		}
		e.buf = append(e.buf, ']', ']', ']')
		return e.grow()
	})
}

// object writes key's value through body, as ["~#obj",[ID,…]] at its first
// occurrence and ["~#ref",ID] after.
func (e *durableEncoder) object(key any, depth int, body func() error) error {
	if err := e.count(); err != nil {
		return err
	}
	if id, ok := e.ids[key]; ok {
		e.buf = fmt.Appendf(e.buf, `["`+tagRef+`",%d]`, id)
		return e.grow()
	}
	id := e.nextID
	e.nextID++
	e.ids[key] = id
	e.buf = fmt.Appendf(e.buf, `["`+tagObj+`",[%d,`, id)
	if err := body(); err != nil {
		return err
	}
	e.buf = append(e.buf, ']', ']')
	return e.grow()
}
