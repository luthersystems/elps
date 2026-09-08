// Copyright © 2026 The ELPS authors

package lisp

import (
	"reflect"
	"testing"
)

var templateSpanCellSink templateCellSpan
var templateSpanByteSink templateByteSpan

// Address extraction must preserve zero-length views with reachable capacity.
// The returned span retains its owning header, not only a uintptr address.
func TestTemplateSpanAddressPreservesCapacity(t *testing.T) {
	cells := []*LVal{Int(1), Int(2), Int(3), Int(4)}
	bytes := []byte("abcd")
	for _, bounds := range [][2]int{{0, 4}, {1, 3}, {2, 2}} {
		v := QExpr(cells[bounds[0]:bounds[1]])
		span := newTemplateCellSpan(v)
		start := reflect.ValueOf(v.Cells).Pointer()
		if span.value != v || span.start != start || span.end != start+uintptr(cap(v.Cells))*reflect.TypeFor[*LVal]().Size() {
			t.Fatalf("cell bounds %v lost ownership or capacity: %+v", bounds, span)
		}
		b := bytes[bounds[0]:bounds[1]]
		byteSpan := newTemplateByteSpan(&b)
		byteStart := reflect.ValueOf(b).Pointer()
		if byteSpan.value != &b || byteSpan.start != byteStart || byteSpan.end != byteStart+uintptr(cap(b)) {
			t.Fatalf("byte bounds %v lost ownership or capacity: %+v", bounds, byteSpan)
		}
	}
}

func TestTemplateSpanAddressDoesNotAllocate(t *testing.T) {
	v := QExpr([]*LVal{Int(7)}[:0])
	b := []byte("a")[:0]
	if got := testing.AllocsPerRun(100, func() { templateSpanCellSink = newTemplateCellSpan(v) }); got != 0 {
		t.Errorf("recording existing cell storage allocated %g times, want 0", got)
	}
	if got := testing.AllocsPerRun(100, func() { templateSpanByteSink = newTemplateByteSpan(&b) }); got != 0 {
		t.Errorf("recording existing byte storage allocated %g times, want 0", got)
	}
}
