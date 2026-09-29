// Copyright © 2026 The ELPS authors

package lisp

import (
	"sort"
	"testing"

	"github.com/stretchr/testify/assert"
)

// TestEverySpecialOpHasAShape fails when a special operator is added to
// DefaultSpecialOps without telling the code walker its syntax.  Add the
// operator to specialFormShapes in lisp/codewalk.go (and a macroexpand-all
// case below) to fix it.
func TestEverySpecialOpHasAShape(t *testing.T) {
	for _, op := range DefaultSpecialOps() {
		assert.NotEqual(t, shapeUnknown, specialFormShape(op.Name()),
			"special operator %q has no code-walker shape (lisp/codewalk.go specialFormShapes)", op.Name())
	}
}

// TestEveryCoreMacroReviewedForWalking fails when a builtin macro is added
// without deciding how the code walker treats it.  Most macros expand to
// ordinary source and need nothing.  A macro whose expansion embeds a value
// that is not source (defun and defmacro embed a compiled function) must be
// given a shape instead, so walkers keep it as written.
func TestEveryCoreMacroReviewedForWalking(t *testing.T) {
	reviewed := map[string]formShape{
		"defmacro":         shapeDefun,
		"defun":            shapeDefun,
		"deftype":          shapeUnknown,
		"curry-function":   shapeUnknown,
		"get-default":      shapeUnknown,
		"trace":            shapeUnknown,
		"defconst":         shapeUnknown,
		"test-let":         shapeUnknown,
		"test-let*":        shapeUnknown,
		"benchmark-simple": shapeUnknown,
	}
	var names []string
	for _, m := range DefaultMacros() {
		names = append(names, m.Name())
		want, ok := reviewed[m.Name()]
		if assert.True(t, ok, "builtin macro %q has not been reviewed for the code walker", m.Name()) {
			assert.Equal(t, want, specialFormShape(m.Name()), m.Name())
		}
	}
	sort.Strings(names)
	assert.Len(t, names, len(reviewed))
}
