// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// packageFormHead recognizes qualified package control calls when expansion
// is enabled. Without an expander, preserve the historical head spelling so
// both analyzer passes retain their existing package and reference policy.
func (a *analyzer) packageFormHead(form *lisp.LVal) string {
	head := astutil.HeadSymbol(form)
	if a.cfg != nil && a.cfg.MacroExpander != nil &&
		(head == "lisp:in-package" || head == "lisp:use-package") {
		return strings.TrimPrefix(head, "lisp:")
	}
	return head
}
