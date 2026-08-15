package mcpserver

import (
	"context"
	"path/filepath"
	"strconv"
	"testing"
	"time"
)

// BenchmarkLintTool measures a warm lint request — the workspace state is
// already built and validated, so the benchmark isolates what the tool does
// per request.
//
// It exists because issue #424's fix changes exactly that: the tool used to
// call lint.BuildAnalysisConfig on every request, which runs a full
// PrescanWorkspace, a full ScanWorkspaceRefs and (through
// defaultStdlibExports) boots a fresh env with the whole standard library
// loaded. It now analyses against the cached workspace state. #422 made both
// of those scans more expensive by putting macro expansion inside them, so the
// per-request cost of the old shape was still growing.
//
// This benchmark is NEW, so the regression gate has no paired baseline for it
// and cannot fire on it. Its before/after numbers are reported in the pull
// request by running it against both revisions by hand.
func BenchmarkLintTool(b *testing.B) {
	root := macroHeavyWorkspace(b)
	srv := New(WithWorkspaceRoot(root))
	srv.service.workspaceValidationInterval = time.Hour
	if _, err := srv.service.workspace(root); err != nil {
		b.Fatal(err)
	}
	path := filepath.Join(root, "file"+strconv.Itoa(0)+".lisp")
	ctx := context.Background()

	b.ReportAllocs()
	for b.Loop() {
		if _, _, err := srv.service.lintTool(ctx, nil, LintInput{Path: path}); err != nil {
			b.Fatal(err)
		}
	}
}
