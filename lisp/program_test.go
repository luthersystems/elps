// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

func programTestEnv(tb testing.TB) *lisp.LEnv {
	tb.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		tb.Fatalf("initialize-user-env: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		tb.Fatalf("load-library: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		tb.Fatalf("in-package: %v", rc)
	}
	return env
}

// TestLoadProgram proves the Program path evaluates exactly like the Reader
// path: parse once into a Program, load it, and get the same result
// LoadString produces for the same source.
func TestLoadProgram(t *testing.T) {
	const src = `
(defun double (x) (* x 2))
(set 'xs '(1 2 3))
(map 'list double xs)
`
	env := programTestEnv(t)
	p, err := env.ParseProgram("prog.lisp", "prog.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if p.Len() != 3 {
		t.Errorf("Len() = %d, want 3", p.Len())
	}
	got := env.LoadProgram(p)
	if got.Type == lisp.LError {
		t.Fatalf("load: %v", got)
	}
	ref := programTestEnv(t)
	want := ref.LoadString("prog.lisp", src)
	if want.Type == lisp.LError {
		t.Fatalf("reference load: %v", want)
	}
	if got.String() != want.String() {
		t.Errorf("LoadProgram = %v, LoadString = %v", got, want)
	}

	// Reload in the same env: definitions re-evaluate cleanly and the final
	// expression yields the same value.
	again := env.LoadProgram(p)
	if again.Type == lisp.LError {
		t.Fatalf("reload: %v", again)
	}
	if again.String() != got.String() {
		t.Errorf("reload = %v, first load = %v", again, got)
	}
}

// TestLoadProgramRestoresPackage mirrors the in-package guarantee documented
// on Load: after LoadProgram the current package is what it was before.
func TestLoadProgramRestoresPackage(t *testing.T) {
	env := programTestEnv(t)
	before := env.Runtime.Package.Name
	p, err := env.ParseProgram("pkg.lisp", "pkg.lisp",
		strings.NewReader(`(in-package 'lisp)`))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	if rc := env.LoadProgram(p); rc.Type == lisp.LError {
		t.Fatalf("load: %v", rc)
	}
	if env.Runtime.Package.Name != before {
		t.Errorf("package after load = %q, want %q", env.Runtime.Package.Name, before)
	}
}

func TestReadProgram(t *testing.T) {
	p, err := lisp.ReadProgram(parser.NewReader(), "read.lisp", strings.NewReader(`(+ 1 2) (+ 3 4)`))
	if err != nil {
		t.Fatalf("ReadProgram: %v", err)
	}
	if p.Len() != 2 {
		t.Errorf("Len() = %d, want 2", p.Len())
	}
	env := programTestEnv(t)
	if got := env.LoadProgram(p); got.String() != "7" {
		t.Errorf("LoadProgram = %v, want 7", got)
	}
	if _, err := lisp.ReadProgram(nil, "x", strings.NewReader("()")); err == nil {
		t.Error("ReadProgram(nil reader) did not error")
	}
}

func TestReadLocationProgram(t *testing.T) {
	reader, ok := parser.NewReader().(lisp.LocationReader)
	if !ok {
		t.Fatal("parser.NewReader does not implement LocationReader")
	}
	p, err := lisp.ReadLocationProgram(reader, "loc.lisp", "phylum/loc.lisp", strings.NewReader(`(- 10 1)`))
	if err != nil {
		t.Fatalf("ReadLocationProgram: %v", err)
	}
	env := programTestEnv(t)
	if got := env.LoadProgram(p); got.String() != "9" {
		t.Errorf("LoadProgram = %v, want 9", got)
	}
	if _, err := lisp.ReadLocationProgram(nil, "x", "x", strings.NewReader("()")); err == nil {
		t.Error("ReadLocationProgram(nil reader) did not error")
	}
}

func TestParseProgramErrors(t *testing.T) {
	env := programTestEnv(t)
	if _, err := env.ParseProgram("bad.lisp", "bad.lisp", strings.NewReader("(unbalanced")); err == nil {
		t.Error("parse of unbalanced source did not error")
	}
	noReader := lisp.NewEnv(nil)
	if _, err := noReader.ParseProgram("x.lisp", "x.lisp", strings.NewReader("()")); err == nil {
		t.Error("ParseProgram without a runtime reader did not error")
	}
}

// TestProgramZero pins the zero value's behavior: empty, loadable, nil
// result.
func TestProgramZero(t *testing.T) {
	var p lisp.Program
	if p.Len() != 0 {
		t.Errorf("zero Len() = %d", p.Len())
	}
	env := programTestEnv(t)
	if got := env.LoadProgram(p); !got.IsNil() {
		t.Errorf("zero LoadProgram = %v, want nil", got)
	}
}

func TestProgramString(t *testing.T) {
	p, err := lisp.ReadProgram(parser.NewReader(), "s.lisp", strings.NewReader(`(+ 1 1) 'a "b"`))
	if err != nil {
		t.Fatalf("ReadProgram: %v", err)
	}
	// Exact match doubles as a no-AST-rendering check: the description names
	// only the expression count, never the expressions.
	if got := p.String(); got != "<program 3 exprs>" {
		t.Errorf("String() = %q, want \"<program 3 exprs>\"", got)
	}
}
