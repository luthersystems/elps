// Copyright © 2026 The ELPS authors

//go:build elpscheck

package lisp

import "slices"

func checkTemplateStorageOrder(cells []templateCellSpan) {
	if !slices.IsSortedFunc(cells, compareTemplateCellSpans) {
		panic("template: storage requires sorted mutable cell spans")
	}
}
