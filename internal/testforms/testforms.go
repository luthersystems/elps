// Copyright © 2026 The ELPS authors

// Package testforms names the core forms that register a test or benchmark
// (elps#736): test, test-let, test-let*, benchmark and benchmark-simple.
// The test runner runs their bodies after the load.  Package lint (the
// check test-outside-test-file) and package analysis (the check
// record-defmethod-not-toplevel) read the names here, so the two agree.
package testforms

import "strings"

// forms are the test definition forms, unqualified.
var forms = map[string]bool{
	"test": true, "test-let": true, "test-let*": true, "benchmark": true, "benchmark-simple": true,
}

// Name returns the unqualified form name when head calls a test definition
// form, unqualified, lisp:-qualified or testing:-qualified, or "" otherwise.
func Name(head string) string {
	name := head
	if ns, local, qualified := strings.Cut(head, ":"); qualified {
		if ns != "lisp" && ns != "testing" {
			return ""
		}
		name = local
	}
	if forms[name] {
		return name
	}
	return ""
}
