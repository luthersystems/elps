// Copyright © 2026 The ELPS authors

package analysis

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/parser/token"
)

// enclosingFinderSources are sources that exercise each kind of function
// scope: defun, defmacro, lambda, flet, labels, nested and same-line
// forms.
var enclosingFinderSources = map[string]string{
	"defun": `(in-package 'p)
(defun helper (x) (+ x 1))
(defun caller (y)
  (helper (helper y)))
(defmacro twice (form) (list 'progn form (helper 1)))
(helper 2)
`,
	"nested": `(defun helper (x) x)
(defun outer (a)
  (let ((f (lambda (b) (helper b))))
    (flet ((inner (c) (helper c)) (other (d) (helper d)))
      (labels ((rec (e) (if e (rec (helper e)) (inner e))))
        (rec (funcall f a))))))
`,
	"same-line": `(defun helper (x) x)
(defun one (a) (helper a)) (defun two (b) (helper (one b)))
(defun three (c) (lambda (d) (helper d))) (set 'v (lambda (e) (helper e)))
(flet ((p (x) (helper x)) (q (y) (helper y))) (p (q 1)))
`,
	"top-level-lambda": `(defun helper (x) x)
(set 'f (lambda (a)
  (helper a)))
(map 'list (lambda (b) (helper b)) '(1 2))
`,
}

// assertFinderMatchesFindEnclosingFunction compares enclosingFinder with
// FindEnclosingFunction at every reference of result and at the start of
// every line of the source.
func assertFinderMatchesFindEnclosingFunction(t *testing.T, name string, result *Result, lines int) {
	t.Helper()
	type pos struct{ line, col int }
	var positions []pos
	for _, ref := range result.References {
		if ref.Source != nil {
			positions = append(positions, pos{ref.Source.Line, ref.Source.Col})
		}
	}
	for line := 1; line <= lines; line++ {
		positions = append(positions, pos{line, 1}, pos{line, 12})
	}
	finder := enclosingFinder{root: result.RootScope}
	for _, p := range positions {
		got := finder.find(p.line, p.col)
		// FindEnclosingFunction visits a map, so it may return any of
		// several symbols when more than one function of a scope starts
		// on the line of a lambda. Ask again before a mismatch counts.
		matched := false
		var want *Symbol
		for range 50 {
			want = FindEnclosingFunction(result.RootScope, p.line, p.col)
			if want == got {
				matched = true
				break
			}
			if (want == nil) != (got == nil) {
				break
			}
		}
		if !matched {
			t.Errorf("%s:%d:%d: finder returned %s, FindEnclosingFunction returned %s",
				name, p.line, p.col, describeSymbol(got), describeSymbol(want))
		}
	}
}

func describeSymbol(sym *Symbol) string {
	if sym == nil {
		return "no function"
	}
	return sym.Name
}

// TestEnclosingFinderMatchesFindEnclosingFunction pins the finder that
// ExtractFileRefs uses to FindEnclosingFunction, its definition.
func TestEnclosingFinderMatchesFindEnclosingFunction(t *testing.T) {
	for name, src := range enclosingFinderSources {
		file := name + ".lisp"
		// A global of another file that starts on a line of this
		// source, under this file's name: an external symbol is never
		// the enclosing function.
		cfg := &Config{ExtraGlobals: []ExternalSymbol{{
			Name:   "elsewhere",
			Kind:   SymFunction,
			Source: &token.Location{File: file, Line: 3, Col: 1},
		}}}
		result := AnalyzeFile([]byte(src), file, cfg)
		if result == nil {
			t.Fatalf("%s does not parse", name)
		}
		assertFinderMatchesFindEnclosingFunction(t, name, result, strings.Count(src, "\n")+1)
	}
}

// TestEnclosingFinderMatchesOnRepositoryFiles runs the comparison over
// every lisp file of the repository.
func TestEnclosingFinderMatchesOnRepositoryFiles(t *testing.T) {
	files := 0
	err := filepath.WalkDir("..", func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if strings.HasPrefix(d.Name(), ".") && d.Name() != ".." {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".lisp" {
			return nil
		}
		src, err := os.ReadFile(path) //nolint:gosec // a file of this repository
		if err != nil {
			return err
		}
		result := AnalyzeFile(src, path, nil)
		if result == nil {
			// Not every fixture parses.
			return nil
		}
		files++
		assertFinderMatchesFindEnclosingFunction(t, path, result, strings.Count(string(src), "\n")+1)
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if files < 20 {
		t.Fatalf("compared %d files, want at least 20", files)
	}
}
