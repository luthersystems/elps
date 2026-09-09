// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// elps#611: textDocument/didSave refreshes the workspace index from the saved
// file on disk -- updateFileDefinitions and updateFileRefs -- and did so with a
// bare os.ReadFile and a full parse, no size check. The workspace scan that
// builds the same tables skips any file over ScanConfig.MaxFileBytes by its
// Stat size before opening it, so a file the scan would never index was
// being read and parsed in full on every save. This pins the incremental path
// to the scan's bound: an over-limit save leaves both tables exactly as they
// were and allocates next to nothing (the read alone is the file size, the
// parse many times that), while an under-limit save still updates them.
//
// Bounded by ALLOCATION, not wall clock, as TestDidChange_OverLimitDocumentIsCheap
// is: a Stat costs bytes, a read-and-parse costs megabytes, and no scheduler
// stall can blur that.

// overLimitProgram is a well-formed program just over the scan's per-file
// limit: if it were parsed, it would add "target" and "caller" definitions
// and a target<-caller reference, so "tables untouched" is a real assertion.
func overLimitProgram(t *testing.T) []byte {
	t.Helper()
	const unit = "(defun target (x) x)\n(defun caller () (target 1))\n"
	n := int(analysis.DefaultMaxFileBytes)/len(unit) + 1
	src := []byte(strings.Repeat(unit, n))
	require.Greater(t, int64(len(src)), int64(analysis.DefaultMaxFileBytes))
	return src
}

// allocatedBy returns the bytes allocated while fn ran.
func allocatedBy(fn func()) uint64 {
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	fn()
	runtime.ReadMemStats(&after)
	return after.TotalAlloc - before.TotalAlloc
}

// A generous ceiling: the fixed path does a Stat and a handful of small
// allocations; the defect reads 5 MiB and parses it (hundreds of MiB).
const didSaveAllocCeiling = 1 << 20

func TestDidSave_OverLimitFileLeavesIndexUntouched(t *testing.T) {
	s := testServer()
	dir := t.TempDir()

	// Sentinel table contents from another file, which must survive intact.
	other := &token.Location{File: filepath.Join(dir, "other.lisp"), Line: 1, Col: 1}
	sentinelGlobals := []analysis.ExternalSymbol{{
		Name: "existing", Kind: analysis.SymFunction, Package: "user", Source: other,
	}}
	sentinelKey := analysis.SymbolKey{Package: "user", Name: "existing", Kind: analysis.SymFunction}.String()
	sentinelRefs := map[string][]analysis.FileReference{
		sentinelKey: {{SymbolKey: analysis.SymbolKey{Package: "user", Name: "existing", Kind: analysis.SymFunction},
			Source: other, File: other.File, Enclosing: "user-of-existing"}},
	}
	setTestAnalysisCfg(s, &analysis.Config{ExtraGlobals: append([]analysis.ExternalSymbol(nil), sentinelGlobals...)})
	s.setTestWorkspaceRefs(cloneRefs(sentinelRefs))

	globals := func() []analysis.ExternalSymbol {
		s.analysisCfgMu.RLock()
		defer s.analysisCfgMu.RUnlock()
		return append([]analysis.ExternalSymbol(nil), s.analysisCfg.ExtraGlobals...)
	}
	refs := func() map[string][]analysis.FileReference {
		s.workspaceRefsMu.RLock()
		defer s.workspaceRefsMu.RUnlock()
		return cloneRefs(s.workspaceRefs)
	}

	// Positive control: an under-limit save of the same program updates
	// both tables, so the gate is the only thing separating the two cases.
	smallPath := filepath.Join(dir, "small.lisp")
	require.NoError(t, os.WriteFile(smallPath, []byte("(defun target (x) x)\n(defun caller () (target 1))\n"), 0o600))
	s.updateFileDefinitions(pathToURI(smallPath))
	s.updateFileRefs(pathToURI(smallPath))
	require.Len(t, globals(), 3, "an under-limit save adds target and caller to the sentinel")
	targetKey := analysis.SymbolKey{Package: "user", Name: "target", Kind: analysis.SymFunction}.String()
	require.NotEmpty(t, refs()[targetKey], "an under-limit save records the target<-caller reference")

	// Reset to the sentinels and save a file the workspace scan would skip.
	setTestAnalysisCfg(s, &analysis.Config{ExtraGlobals: append([]analysis.ExternalSymbol(nil), sentinelGlobals...)})
	s.setTestWorkspaceRefs(cloneRefs(sentinelRefs))
	bigPath := filepath.Join(dir, "big.lisp")
	require.NoError(t, os.WriteFile(bigPath, overLimitProgram(t), 0o600))
	bigURI := pathToURI(bigPath)

	defsAlloc := allocatedBy(func() { s.updateFileDefinitions(bigURI) })
	assert.Equal(t, sentinelGlobals, globals(), "an over-limit save must leave ExtraGlobals untouched")
	assert.Less(t, defsAlloc, uint64(didSaveAllocCeiling),
		"updateFileDefinitions on an over-limit file must not read or parse it (allocated %d bytes)", defsAlloc)

	refsAlloc := allocatedBy(func() { s.updateFileRefs(bigURI) })
	assert.Equal(t, sentinelRefs, refs(), "an over-limit save must leave workspaceRefs untouched")
	assert.Less(t, refsAlloc, uint64(didSaveAllocCeiling),
		"updateFileRefs on an over-limit file must not read or parse it (allocated %d bytes)", refsAlloc)
}

// TestDidSave_OverLimitSavePurgesTheFilesStaleIndexEntries pins the half of
// readWorkspaceFile's contract that "leave the tables as they were" gets
// wrong. The godoc promises an over-limit file leaves the index exactly as if
// the scan had skipped the file -- but the scan skipping a file means the
// index holds NOTHING for it, and a file that was indexed while it was small
// and is then saved over the limit keeps every definition and reference the
// small version had. Go-to-definition then points at a symbol the file no
// longer defines. So an over-limit read purges that file's entries, while a
// missing or unreadable one still leaves them (a transient read error must
// not empty the index).
func TestDidSave_OverLimitSavePurgesTheFilesStaleIndexEntries(t *testing.T) {
	s := testServer()
	dir := t.TempDir()

	// A sentinel from a DIFFERENT file: the purge is scoped to the file that
	// was saved, so this must survive.
	other := &token.Location{File: filepath.Join(dir, "other.lisp"), Line: 1, Col: 1}
	sentinel := analysis.ExternalSymbol{
		Name: "existing", Kind: analysis.SymFunction, Package: "user", Source: other,
	}
	setTestAnalysisCfg(s, &analysis.Config{ExtraGlobals: []analysis.ExternalSymbol{sentinel}})
	s.setTestWorkspaceRefs(map[string][]analysis.FileReference{})

	path := filepath.Join(dir, "grows.lisp")
	uri := pathToURI(path)
	normalized := analysis.NormalizePath(path)

	globalNames := func() []string {
		s.analysisCfgMu.RLock()
		defer s.analysisCfgMu.RUnlock()
		names := make([]string, 0, len(s.analysisCfg.ExtraGlobals))
		for _, g := range s.analysisCfg.ExtraGlobals {
			names = append(names, g.Name)
		}
		return names
	}
	refsFromFile := func() []analysis.FileReference {
		s.workspaceRefsMu.RLock()
		defer s.workspaceRefsMu.RUnlock()
		var out []analysis.FileReference
		for _, refs := range s.workspaceRefs {
			for _, r := range refs {
				if analysis.NormalizePath(r.File) == normalized {
					out = append(out, r)
				}
			}
		}
		return out
	}

	// Saved small: the file is indexed, exactly as the positive control in
	// TestDidSave_OverLimitFileLeavesIndexUntouched.
	require.NoError(t, os.WriteFile(path,
		[]byte("(defun target (x) x)\n(defun caller () (target 1))\n"), 0o600))
	s.updateFileDefinitions(uri)
	s.updateFileRefs(uri)
	require.Contains(t, globalNames(), "target", "the small version is indexed")
	require.NotEmpty(t, refsFromFile(), "the small version contributes references")

	// Saved over the limit, and no longer defining target. The scan would
	// skip this file entirely, so the index must hold nothing for it.
	const unit = "(defun other-thing (x) x)\n"
	big := strings.Repeat(unit, int(analysis.DefaultMaxFileBytes)/len(unit)+1)
	require.Greater(t, int64(len(big)), int64(analysis.DefaultMaxFileBytes))
	require.NoError(t, os.WriteFile(path, []byte(big), 0o600))
	s.updateFileDefinitions(uri)
	s.updateFileRefs(uri)

	assert.NotContains(t, globalNames(), "target",
		"an over-limit save must not leave a definition the file no longer has")
	assert.NotContains(t, globalNames(), "caller",
		"an over-limit save must not leave a definition the file no longer has")
	assert.Empty(t, refsFromFile(),
		"an over-limit save must not leave references from the file")
	assert.Contains(t, globalNames(), "existing",
		"the purge is scoped to the saved file")

	// A MISSING file is the other case, and it keeps main's behaviour: index
	// a small version again, delete the file, and the entries stay.
	require.NoError(t, os.WriteFile(path,
		[]byte("(defun target (x) x)\n(defun caller () (target 1))\n"), 0o600))
	s.updateFileDefinitions(uri)
	s.updateFileRefs(uri)
	require.Contains(t, globalNames(), "target")
	require.NoError(t, os.Remove(path))
	s.updateFileDefinitions(uri)
	s.updateFileRefs(uri)
	assert.Contains(t, globalNames(), "target",
		"an unreadable file leaves the tables alone; only over-limit purges")
	assert.NotEmpty(t, refsFromFile(),
		"an unreadable file leaves the tables alone; only over-limit purges")
}

// The rename fallback in position_encoding.go reads a closed file from disk to
// convert edit columns; it is the third bare read the issue names. Over the
// limit it must answer "no text" without reading, and the caller then leaves
// the range in byte columns as it already does for an unreadable file.
func TestDocumentTexts_OverLimitFileIsNotRead(t *testing.T) {
	s := testServer()
	dir := t.TempDir()

	smallPath := filepath.Join(dir, "small.lisp")
	require.NoError(t, os.WriteFile(smallPath, []byte("(defun f () 1)\n"), 0o600))
	bigPath := filepath.Join(dir, "big.lisp")
	require.NoError(t, os.WriteFile(bigPath, overLimitProgram(t), 0o600))

	d := &documentTexts{srv: s, texts: map[string]string{}}

	text, ok := d.get(pathToURI(smallPath))
	require.True(t, ok, "an under-limit closed file is read from disk")
	assert.Equal(t, "(defun f () 1)\n", text)

	var text2 string
	var ok2 bool
	alloc := allocatedBy(func() { text2, ok2 = d.get(pathToURI(bigPath)) })
	assert.False(t, ok2, "an over-limit closed file yields no text")
	assert.Empty(t, text2)
	assert.Less(t, alloc, uint64(didSaveAllocCeiling),
		"documentTexts.get on an over-limit file must not read it (allocated %d bytes)", alloc)
}

func cloneRefs(m map[string][]analysis.FileReference) map[string][]analysis.FileReference {
	out := make(map[string][]analysis.FileReference, len(m))
	for k, v := range m {
		out[k] = append([]analysis.FileReference(nil), v...)
	}
	return out
}
