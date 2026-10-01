// Copyright © 2026 The ELPS authors

package valwalk

import (
	"slices"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

type countVisitor struct{}

func (countVisitor) Visit(_ *Walker[int], v *lisp.LVal) (Step, int, error) {
	return Step{Done: lisp.ShapeOf(v.Type) != lisp.ShapeList, Children: v.Cells}, 1, nil
}
func (countVisitor) Child(*Walker[int], *lisp.LVal, int) error { return nil }
func (countVisitor) Leave(_ *Walker[int], _ *lisp.LVal, children []int) (int, error) {
	n := 1
	for _, child := range children {
		n += child
	}
	return n, nil
}
func recursiveCount(v *lisp.LVal) int {
	n := 1
	if lisp.ShapeOf(v.Type) == lisp.ShapeList {
		for _, child := range v.Cells {
			n += recursiveCount(child)
		}
	}
	return n
}

type copyVisitor struct{}

func (copyVisitor) Visit(_ *Walker[*lisp.LVal], v *lisp.LVal) (Step, *lisp.LVal, error) {
	if lisp.ShapeOf(v.Type) == lisp.ShapeList {
		return Step{Children: v.Cells}, nil, nil
	}
	cp := *v
	return Step{Done: true}, &cp, nil
}
func (copyVisitor) Child(*Walker[*lisp.LVal], *lisp.LVal, int) error { return nil }
func (copyVisitor) Leave(_ *Walker[*lisp.LVal], v *lisp.LVal, children []*lisp.LVal) (*lisp.LVal, error) {
	cp := *v
	cp.Cells = slices.Clone(children)
	return &cp, nil
}
func recursiveCopy(v *lisp.LVal) *lisp.LVal {
	cp := *v
	if lisp.ShapeOf(v.Type) == lisp.ShapeList {
		cp.Cells = make([]*lisp.LVal, len(v.Cells))
		for i, child := range v.Cells {
			cp.Cells[i] = recursiveCopy(child)
		}
	}
	return &cp
}
func benchmarkTree(depth, width int) *lisp.LVal {
	if depth == 0 {
		return lisp.Int(1)
	}
	children := make([]*lisp.LVal, width)
	for i := range children {
		children[i] = benchmarkTree(depth-1, width)
	}
	return lisp.SExpr(children)
}
func TestPrototypeVisitorsMatch(t *testing.T) {
	root := benchmarkTree(3, 4)
	count, err := Walk(root, countVisitor{})
	if err != nil || count != recursiveCount(root) {
		t.Fatalf("count %d, error %v", count, err)
	}
	cp, err := Walk(root, copyVisitor{})
	if err != nil || cp.Equal(recursiveCopy(root)) != lisp.Bool(true) {
		t.Fatalf("copy differs: %v", err)
	}
}
func BenchmarkPrototype(b *testing.B) {
	root := benchmarkTree(4, 8)
	b.Run("count/recursive", func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			if recursiveCount(root) != 4681 {
				b.Fatal("wrong count")
			}
		}
	})
	b.Run("count/valwalk", func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			n, err := Walk(root, countVisitor{})
			if err != nil || n != 4681 {
				b.Fatal("wrong count")
			}
		}
	})
	b.Run("copy/recursive", func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			if recursiveCopy(root) == nil {
				b.Fatal("nil copy")
			}
		}
	})
	b.Run("copy/valwalk", func(b *testing.B) {
		b.ReportAllocs()
		for b.Loop() {
			if cp, err := Walk(root, copyVisitor{}); err != nil || cp == nil {
				b.Fatal("copy failed")
			}
		}
	})
}
