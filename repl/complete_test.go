// Copyright © 2018 The ELPS authors

package repl

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func TestSymbolCompleter(t *testing.T) {
	env := lisp.NewEnv(nil)
	lisp.InitializeUserEnv(env,
		lisp.WithReader(parser.NewReader()),
	)
	lisplib.LoadLibrary(env)
	env.InPackage(lisp.String(lisp.DefaultUserPackage))

	c := &symbolCompleter{env: env}

	// "de" should match defun, defmacro, debug-print, etc.
	candidates, offset := c.Do([]rune("(de"), 3)
	if offset != 2 {
		t.Errorf("offset = %d, want 2", offset)
	}
	if len(candidates) == 0 {
		t.Error("expected completions for 'de', got none")
	}

	// "string:" should complete with string package symbols.
	candidates, offset = c.Do([]rune("(string:"), 8)
	if offset != 7 {
		t.Errorf("offset = %d, want 7", offset)
	}
	if len(candidates) == 0 {
		t.Error("expected completions for 'string:', got none")
	}

	// "zzz-nonexistent" should have no completions.
	candidates, _ = c.Do([]rune("(zzz-nonexistent"), 16)
	if len(candidates) != 0 {
		t.Errorf("expected no completions for 'zzz-nonexistent', got %d", len(candidates))
	}
}
