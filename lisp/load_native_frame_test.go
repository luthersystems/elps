// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// TestLoadFileFromNativeFrame is a regression test for the nil-source frame
// crash found integrating issue #362: constructors no longer stamp the
// shared "<native code>" location, so an expression built by Go code (the
// way embedding applications drive load-file — e.g. substrate's
// cc:load-chaincode) produces a call-stack frame whose Source is nil.
// Runtime.sourceContext used to dereference that Source unconditionally and
// panicked.  It must instead report the synthetic native location, and the
// load must succeed.
func TestLoadFileFromNativeFrame(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "lib.lisp"), []byte("(set 'loaded-marker 7)\n"), 0o600); err != nil {
		t.Fatal(err)
	}

	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("init: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}

	// The call must be constructed in Go, NOT parsed from source: parsed
	// expressions carry real locations and would not reproduce the nil
	// frame Source.
	call := lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("lisp:load-file"),
		lisp.String(filepath.Join(dir, "lib.lisp")),
	})
	if rc := env.Eval(call); rc.Type == lisp.LError {
		t.Fatalf("load-file from a native frame failed: %v", rc)
	}
	got := env.GetGlobal(lisp.Symbol("loaded-marker"))
	if got.Type != lisp.LInt || got.Int != 7 {
		t.Fatalf("loaded file did not evaluate: %v", got)
	}
}
