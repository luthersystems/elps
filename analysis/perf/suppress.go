// Copyright © 2024 The ELPS authors

package perf

import (
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// isSuppressed checks whether a function definition has a suppression
// comment in its leading comments. The default directive is:
//
//	;; elps-analyze-disable
//
// The prefix can be customized via Config.SuppressionPrefix so that
// embedders can use their own convention (e.g., "substrate-analyze-disable").
//
// When present on a defun/defmacro, the function is excluded from
// performance analysis entirely.
func isSuppressed(node *lisp.LVal, prefix string) bool {
	if node == nil || node.Meta == nil {
		return false
	}
	if prefix == "" {
		prefix = "elps-analyze-disable"
	}
	for _, tok := range node.Meta.LeadingComments {
		if tok == nil {
			continue
		}
		text := strings.TrimSpace(tok.Text)
		text = strings.TrimLeft(text, ";")
		text = strings.TrimSpace(text)
		if text == prefix {
			return true
		}
	}
	return false
}
