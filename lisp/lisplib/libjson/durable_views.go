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
// array's data list.  Discovery (the first pass over the graph) records
// every holder header it meets.  Once every array's data header is known,
// each header gets its final identity, and holders are grouped by the
// address ranges their cells cover.  A list covers its length.  A vector's
// data covers its capacity, because append! writes there.  Other array
// data (no vector uses it) covers its length, and is written with capacity
// equal to its length: nothing can append to it.
//
// Holders whose ranges overlap form one group, and a group of two or more
// is one storage object, ["~#obj",[ID,["~#cells",N]]] at its first use and
// ["~#ref",ID] after.  A vector's data alone with spare capacity is a
// storage of its own, ["~#cells",N], written inline.  Each holder is a view
// of its storage, ["~#view",[STORAGE,OFF,LEN,CAP,[CELLS...]]], whose range
// is [OFF,OFF+CAP).  CELLS holds the cells of that range that no earlier
// view wrote, in offset order, so each cell is written once, by the first
// view the walk reaches that covers it.  A cell that no view's length
// covers is dead: no value can read it before an append writes it, so it
// is written as null.  Addresses decide only which holders overlap and
// their offsets, both properties of the memory layout; every order the
// document shows comes from the graph walk.
//
// Each cell is a node of its own in the cycle check (low links): a view
// reaches the cells of its range, not its whole storage.  A native's
// payload can then hold a view of the storage that holds the native, as
// long as the view does not cover the native's own cell.

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

// holderRec is one holder grouped after discovery.
type holderRec struct {
	key      any
	cells    []*lisp.LVal // the holder's cells, with their capacity
	start    uintptr
	capacity int // cells the holder's range covers
}

// viewInfo places a grouped holder in its group's storage.
type viewInfo struct {
	group, off, length, capacity int
}

// storageInfo is one group's storage.
type storageInfo struct {
	// tree is the no-sharing codec whose payload the storage was first
	// used in, if any.
	tree string
	// cells holds the live cells by offset (nil where dead), and live
	// which offsets are live.
	cells []*lisp.LVal
	live  []bool
	// node holds, during the counting pass, the node index of each cell a
	// view has claimed, or -1.  claimed marks, while writing, each cell a
	// view has written.
	node    []int
	claimed []bool
	// inline marks the storage of a single vector data holder.
	inline, started bool
}

var cellSize = reflect.TypeFor[*lisp.LVal]().Size()

// cellAddr is the address of the first cell of s[:cap(s)].
func cellAddr(s []*lisp.LVal) uintptr {
	return reflect.ValueOf(&s[:cap(s)][0]).Pointer()
}

// recordHeader notes a holder header during discovery, once per header.
// Discovery records every header it meets, before it skips an object it
// has walked, because its keys are not final: a list header can turn out
// to be an array's data list after the walk has met it as a list.
func (e *durableEncoder) recordHeader(h *lisp.LVal) {
	if !e.recorded[h] {
		e.recorded[h] = true
		e.headers = append(e.headers, h)
	}
}

// recordData notes an array's data list during discovery.  vector marks
// data that a vector uses, which append! can grow.
func (e *durableEncoder) recordData(d *lisp.LVal, vector bool) {
	e.dataHeaders[d] = true
	if vector {
		e.appendable[d] = true
	}
	e.recordHeader(d)
}

// holderCap is the capacity DumpDurable writes for a holder: its Go
// capacity for a vector's data, its length for any other holder.
func (e *durableEncoder) holderCap(h *lisp.LVal) int {
	if e.appendable[h] {
		return cap(h.Cells)
	}
	return len(h.Cells)
}

// groupHolders turns the discovered holders into groups, once every
// header's identity is final.  It reserves one value per storage cell
// against the value limit before it builds any storage.
func (e *durableEncoder) groupHolders() error {
	recs := make([]holderRec, 0, len(e.headers))
	seen := map[any]bool{}
	for _, h := range e.headers {
		capacity := e.holderCap(h)
		if capacity == 0 {
			continue
		}
		key, _ := e.identity(h)
		if seen[key] {
			continue
		}
		seen[key] = true
		recs = append(recs, holderRec{key: key, cells: h.Cells, start: cellAddr(h.Cells), capacity: capacity})
	}
	slices.SortFunc(recs, func(a, b holderRec) int { return cmp.Compare(a.start, b.start) })
	total := 0
	for i := 0; i < len(recs); {
		j := i + 1
		end := recs[i].start + uintptr(recs[i].capacity)*cellSize
		for j < len(recs) && recs[j].start < end {
			end = max(end, recs[j].start+uintptr(recs[j].capacity)*cellSize)
			j++
		}
		inline := j-i == 1 && recs[i].capacity > len(recs[i].cells)
		if j-i >= 2 || inline {
			n := int((end - recs[i].start) / cellSize)
			total += n
			if total > e.cfg.maxValues {
				return fmt.Errorf("%w: more than %d values", ErrTypedLimit, e.cfg.maxValues)
			}
			g := len(e.storages)
			st := storageInfo{cells: make([]*lisp.LVal, n), live: make([]bool, n), node: make([]int, n), claimed: make([]bool, n), inline: inline}
			for k := range st.node {
				st.node[k] = -1
			}
			for _, h := range recs[i:j] {
				off := int((h.start - recs[i].start) / cellSize)
				e.views[h.key] = viewInfo{group: g, off: off, length: len(h.cells), capacity: h.capacity}
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
// whose header is an array's data list is that holder, even when empty,
// and an array is its dims and data headers.
func (e *durableEncoder) identity(v *lisp.LVal) (any, bool) {
	if v.Type == lisp.LSExpr && e.dataHeaders[v] {
		return holderKey{v}, true
	}
	if v.Type == lisp.LArray && len(v.Cells) == 2 {
		return arrayKey{v.Cells[0], v.Cells[1]}, true
	}
	return durableIdentity(v)
}

// scanView visits a view's range: each cell no earlier view claimed, as a
// node of its own, and a reach to each cell an earlier view claimed.
func (e *durableEncoder) scanView(vi viewInfo, depth int) error {
	st := &e.storages[vi.group]
	if !st.started {
		st.started = true
		if err := e.countScan(1); err != nil {
			return err
		}
		if len(e.treeNames) > 0 {
			st.tree = e.treeNames[len(e.treeNames)-1]
		}
	} else if !st.inline {
		// A second view of shared storage.
		if len(e.treeNames) > 0 {
			return treeSharingError(e.treeNames[len(e.treeNames)-1])
		}
		if st.tree != "" {
			return treeSharingError(st.tree)
		}
	}
	for k := vi.off; k < vi.off+vi.capacity; k++ {
		if st.node[k] >= 0 {
			e.reach(st.node[k])
			continue
		}
		i := e.openNode()
		st.node[k] = i
		if st.live[k] {
			if err := e.scan(st.cells[k], depth+1); err != nil {
				return err
			}
		} else if err := e.countScan(1); err != nil {
			return err
		}
		e.closeNode(i)
	}
	return nil
}

// scanHolder visits a holder's contents: its view's range when it is a
// view, else its own cells.
func (e *durableEncoder) scanHolder(key any, cells []*lisp.LVal, depth int) error {
	i := e.openObject(key)
	if vi, ok := e.views[key]; ok {
		if err := e.scanView(vi, depth); err != nil {
			return err
		}
	} else {
		for _, c := range cells {
			if err := e.scan(c, depth+1); err != nil {
				return err
			}
		}
	}
	e.closeNode(i)
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
	e.buf = fmt.Appendf(e.buf, ",%d,%d,%d,[", vi.off, vi.length, vi.capacity)
	st := &e.storages[vi.group]
	first := true
	for k := vi.off; k < vi.off+vi.capacity; k++ {
		if st.claimed[k] {
			continue
		}
		st.claimed[k] = true
		if !first {
			e.buf = append(e.buf, ',')
		}
		first = false
		c := st.cells[k]
		if !st.live[k] {
			c = lisp.Nil()
		}
		if err := e.value(c, depth+1); err != nil {
			return err
		}
	}
	e.buf = append(e.buf, ']', ']', ']')
	return e.grow()
}

// storage writes a group's storage: inline for a single vector's data,
// else as a shared object.
func (e *durableEncoder) storage(g, depth int) error {
	body := func() error {
		if err := e.container(depth); err != nil {
			return err
		}
		e.buf = fmt.Appendf(e.buf, `["`+tagCells+`",%d]`, len(e.storages[g].cells))
		return e.grow()
	}
	if e.storages[g].inline {
		return body()
	}
	return e.object(storageKey{g}, depth, body)
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
