// Copyright © 2026 The ELPS authors

package lisp

import (
	"cmp"
	"reflect"
	"slices"
)

// templateStorage is a construction-time index, private to one immutable
// snapshot. Runtime values retain their existing representation; no ownership
// flags or overloaded LVal fields are introduced. Overlap is discovered once,
// then every view names one storage object and its original slice bounds.
// Numeric address ordering relies on the current Go runtime's non-moving heap;
// span.value retains each allocation. No numeric address is used to access memory.
type templateStorage struct {
	cellViews map[*LVal]templateView
	byteViews map[*[]byte]templateView
	cells     [][]*LVal
	bytes     [][]byte
}

type templateView struct {
	storage  int
	offset   int
	length   int
	capacity int
}

type templateCellSpan struct {
	value *LVal
	start uintptr
	end   uintptr
}

type templateByteSpan struct {
	value *[]byte
	start uintptr
	end   uintptr
}

func compareTemplateCellSpans(a, b templateCellSpan) int {
	return cmp.Compare(a.start, b.start)
}

func newTemplateCellSpan(v *LVal) templateCellSpan {
	// Callers admit only nonzero-capacity storage. Point at its first slot,
	// including for an empty view, without boxing a temporary slice header.
	start := reflect.ValueOf(&v.Cells[:cap(v.Cells)][0]).Pointer()
	return templateCellSpan{value: v, start: start,
		end: start + uintptr(cap(v.Cells))*reflect.TypeFor[*LVal]().Size()}
}

func newTemplateByteSpan(v *[]byte) templateByteSpan {
	start := reflect.ValueOf(&(*v)[:cap(*v)][0]).Pointer()
	return templateByteSpan{value: v, start: start, end: start + uintptr(cap(*v))}
}

// storage consumes a successfully scanned inventory. The shared-storage check
// already sorted mutable cell spans; no graph discovery occurs after that check.
func (s *templateInventory) storage() *templateStorage {
	checkTemplateStorageOrder(s.cells)
	p := &templateStorage{
		cellViews: make(map[*LVal]templateView, len(s.cells)),
		byteViews: make(map[*[]byte]templateView, len(s.bytes)),
	}
	width := reflect.TypeFor[*LVal]().Size()
	for first := 0; first < len(s.cells); {
		start, end := s.cells[first].start, s.cells[first].end
		last := first + 1
		for last < len(s.cells) && s.cells[last].start < end {
			if s.cells[last].end > end {
				end = s.cells[last].end
			}
			last++
		}
		// The compiler immediately converts these slots into owned indexed
		// references. A disjoint span can be borrowed read-only for that step;
		// it never becomes VM storage or escapes into the published plan.
		var data []*LVal
		if last == first+1 {
			v := s.cells[first].value
			data = v.Cells[:cap(v.Cells)]
		} else {
			data = make([]*LVal, int((end-start)/width))
		}
		index := len(p.cells)
		for _, span := range s.cells[first:last] {
			offset := int((span.start - start) / width)
			if last != first+1 {
				copy(data[offset:], span.value.Cells[:cap(span.value.Cells)])
			}
			p.cellViews[span.value] = templateView{storage: index, offset: offset,
				length: len(span.value.Cells), capacity: cap(span.value.Cells)}
		}
		p.cells = append(p.cells, data)
		first = last
	}
	slices.SortFunc(s.bytes, func(a, b templateByteSpan) int { return cmp.Compare(a.start, b.start) })
	for first := 0; first < len(s.bytes); {
		start, end := s.bytes[first].start, s.bytes[first].end
		last := first + 1
		for last < len(s.bytes) && s.bytes[last].start < end {
			if s.bytes[last].end > end {
				end = s.bytes[last].end
			}
			last++
		}
		data := make([]byte, int(end-start))
		index := len(p.bytes)
		for _, span := range s.bytes[first:last] {
			offset := int(span.start - start)
			copy(data[offset:], (*span.value)[:cap(*span.value)])
			p.byteViews[span.value] = templateView{storage: index, offset: offset,
				length: len(*span.value), capacity: cap(*span.value)}
		}
		p.bytes = append(p.bytes, data)
		first = last
	}
	return p
}
