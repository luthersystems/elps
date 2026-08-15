package mcpserver

import (
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// BenchmarkBuildWorkspaceState measures a full workspace index build over a
// macro-heavy workspace.
//
// It exists because issue #403's fix moved macro expansion into the index
// build: ScanWorkspaceRefs now expands every macro call in every workspace
// file, where before it walked them opaquely. That is the intended behaviour
// — the index has to agree with per-document analysis — but it made indexing
// cost proportional to how many macro call sites a workspace has, which it
// was not before, so the cost belongs under measurement.
//
// The workspace is generated rather than read from testdata so the shape being
// measured (call sites per file, files, distinct callees) is visible here and
// stays stable if testdata changes.
func BenchmarkBuildWorkspaceState(b *testing.B) {
	root := macroHeavyWorkspace(b)
	srv := New(WithWorkspaceRoot(root))

	b.ReportAllocs()
	for b.Loop() {
		if _, err := srv.service.buildWorkspaceState(root, "benchmark", time.Now()); err != nil {
			b.Fatal(err)
		}
	}
}

// macroHeavyWorkspace writes a workspace of 10 files with 20 macro call sites
// each. Every call site expands to a call of a function defined in lib.lisp,
// so expansion is what makes those references visible to the index.
func macroHeavyWorkspace(tb testing.TB) string {
	tb.Helper()
	root := tb.TempDir()

	const (
		files       = 10
		callsPer    = 20
		helperCount = 5
	)

	var lib strings.Builder
	lib.WriteString("(defmacro run-all (specs)\n  (quasiquote (progn (unquote-splicing specs))))\n")
	for i := range helperCount {
		lib.WriteString("(defun helper" + strconv.Itoa(i) + " () " + strconv.Itoa(i) + ")\n")
	}
	writeBenchFile(tb, filepath.Join(root, "lib.lisp"), lib.String())

	for f := range files {
		var src strings.Builder
		for c := range callsPer {
			src.WriteString("(defun caller" + strconv.Itoa(f) + "-" + strconv.Itoa(c) +
				" () (run-all '((helper" + strconv.Itoa(c%helperCount) + "))))\n")
		}
		writeBenchFile(tb, filepath.Join(root, "file"+strconv.Itoa(f)+".lisp"), src.String())
	}
	return root
}

func writeBenchFile(tb testing.TB, path, content string) {
	tb.Helper()
	if err := os.WriteFile(path, []byte(content), 0o600); err != nil {
		tb.Fatal(err)
	}
}
