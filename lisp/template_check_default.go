// Copyright © 2026 The ELPS authors

//go:build !elpscheck

package lisp

// checkTemplateStorageOrder compiles out in production: admission already
// sorts mutable spans. Checked builds assert this internal precondition.
func checkTemplateStorageOrder(_ []templateCellSpan) {}
