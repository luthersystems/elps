// Copyright © 2026 The ELPS authors

package testforms

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

func TestName(t *testing.T) {
	for head, want := range map[string]string{
		"test": "test", "lisp:test": "test", "testing:test-let*": "test-let*",
		"benchmark": "benchmark", "benchmark-simple": "benchmark-simple",
		"other:test": "", "testing": "", "tests": "", "": "",
	} {
		assert.Equal(t, want, Name(head), head)
	}
	// The lisp: prefix is the language package.
	assert.Equal(t, "lisp", lisp.DefaultLangPackage)
}
