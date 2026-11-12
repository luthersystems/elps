// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// macroSaveWorkspace is a workspace whose file app.lisp calls the macro
// make-fn of macros.lisp. The expansion of the call defines a function,
// so the undefined symbols of app.lisp show which definition of make-fn
// the analysis expanded. The name make-fn has no "def" prefix: a def*
// macro would also give a DefFormSpec, which the scan reads without the
// expander.
type macroSaveWorkspace struct {
	s          *Server
	macrosPath string
	app        *Document
}

const macroSaveApp = "(in-package 'demo)\n(make-fn ignored)\n(defun main () (+ (alpha) (beta)))\n"

func newMacroSaveWorkspace(t *testing.T, macrosSrc string) *macroSaveWorkspace {
	t.Helper()
	dir := t.TempDir()
	w := &macroSaveWorkspace{macrosPath: filepath.Join(dir, "macros.lisp")}
	appPath := filepath.Join(dir, "app.lisp")
	require.NoError(t, os.WriteFile(w.macrosPath, []byte(macrosSrc), 0o600))
	require.NoError(t, os.WriteFile(appPath, []byte(macroSaveApp), 0o600))
	w.s = testServer()
	w.s.rootPath = dir
	w.s.ensureWorkspaceIndex()
	w.app = openDoc(w.s, pathToURI(appPath), macroSaveApp)
	return w
}

// save writes src to macros.lisp and sends didSave for it.
func (w *macroSaveWorkspace) save(t *testing.T, src string) {
	t.Helper()
	require.NoError(t, os.WriteFile(w.macrosPath, []byte(src), 0o600))
	ctx := &glsp.Context{Notify: func(string, any) {}}
	require.NoError(t, w.s.textDocumentDidSave(ctx, &protocol.DidSaveTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: pathToURI(w.macrosPath)},
	}))
}

const (
	makeAlpha = "(in-package 'demo)\n(defmacro make-fn (name) (list 'defun 'alpha '() 1))\n"
	makeBeta  = "(in-package 'demo)\n(defmacro make-fn (name) (list 'defun 'beta '() 2))\n"
)

// TestSaveReloadsChangedMacro checks that a save that changes a macro
// makes the analysis expand the new definition (luthersystems/elps#949).
// Before the fix the env of the expander kept the definition of the
// index build until the server started again.
func TestSaveReloadsChangedMacro(t *testing.T) {
	w := newMacroSaveWorkspace(t, makeAlpha)
	assert.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app),
		"make-fn defines alpha before the save")

	w.save(t, makeBeta)
	assert.Equal(t, []string{"alpha"}, undefinedSymbols(t, w.s, w.app),
		"make-fn defines beta after the save")

	w.save(t, makeAlpha)
	assert.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app),
		"a second save brings the first definition back")
}

// TestSaveRetiresRemovedMacro checks that a macro that a save removes no
// longer expands. An env cannot unbind the macro, so the expander must
// refuse the old definition.
func TestSaveRetiresRemovedMacro(t *testing.T) {
	w := newMacroSaveWorkspace(t, makeAlpha)
	require.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app))

	w.save(t, "(in-package 'demo)\n(defun helper () 1)\n")
	assert.ElementsMatch(t, []string{"make-fn", "ignored", "alpha", "beta"}, undefinedSymbols(t, w.s, w.app),
		"the removed macro does not expand: its call is a function call, and nothing defines alpha")

	w.save(t, makeAlpha)
	assert.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app),
		"a macro defined again expands again")
}

// TestSaveRetiresRenamedMacro checks a rename: the call of the old name
// no longer expands, and a call of the new name would.
func TestSaveRetiresRenamedMacro(t *testing.T) {
	w := newMacroSaveWorkspace(t, makeAlpha)
	require.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app))

	w.save(t, "(in-package 'demo)\n(defmacro make-fn-2 (name) (list 'defun 'alpha '() 1))\n")
	assert.ElementsMatch(t, []string{"make-fn", "ignored", "alpha", "beta"}, undefinedSymbols(t, w.s, w.app))
}

// TestSaveRemovedFileRetiresMacros checks that the macros of a file
// removed from the workspace (an over-limit save, or a deleted file) do
// not expand any more, as after a new index build.
func TestSaveRemovedFileRetiresMacros(t *testing.T) {
	w := newMacroSaveWorkspace(t, makeAlpha)
	require.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app))

	require.NoError(t, os.Remove(w.macrosPath))
	up := w.s.removeFileIndex(w.macrosPath)
	assert.True(t, up.macros)
	assert.ElementsMatch(t, []string{"make-fn", "ignored", "alpha", "beta"}, undefinedSymbols(t, w.s, w.app))
}

// TestSaveKeepsMacrosOfSamePreamble checks that a save that leaves the
// preamble of the file as it was evaluates nothing. Most saves change a
// function body or a comment, and a reload costs an evaluation of the
// file.
func TestSaveKeepsMacrosOfSamePreamble(t *testing.T) {
	w := newMacroSaveWorkspace(t, makeAlpha)
	require.NoError(t, os.WriteFile(w.macrosPath, []byte("; moved down\n\n"+makeAlpha), 0o600))
	up := w.s.updateFileIndex(pathToURI(w.macrosPath))
	assert.False(t, up.macros, "same forms at other positions")
	assert.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app))
}

// loopingPreambleSet is a preamble form that never ends: an empty
// dotimes of about 2^62 turns.
const loopingPreambleSet = "(set 'x (dotimes (i 4611686018427387904) ()))\n"

// TestSaveBoundsMacroReload checks that a save whose preamble has a form
// that does not end returns, logs a warning, and leaves the macros of
// the file as they were. The reload holds the lock of every expansion,
// so an unbounded evaluation would stop all analysis.
func TestSaveBoundsMacroReload(t *testing.T) {
	w := newMacroSaveWorkspace(t, makeAlpha)
	require.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app))

	var mu sync.Mutex
	var logs []string
	ctx := &glsp.Context{Notify: func(method string, params any) {
		if p, ok := params.(*protocol.LogMessageParams); ok && method == "window/logMessage" {
			mu.Lock()
			logs = append(logs, p.Message)
			mu.Unlock()
		}
	}}
	require.NoError(t, os.WriteFile(w.macrosPath, []byte(makeBeta+loopingPreambleSet), 0o600))
	done := make(chan error, 1)
	go func() {
		done <- w.s.textDocumentDidSave(ctx, &protocol.DidSaveTextDocumentParams{
			TextDocument: protocol.TextDocumentIdentifier{URI: pathToURI(w.macrosPath)},
		})
	}()
	select {
	case err := <-done:
		require.NoError(t, err)
	case <-time.After(30 * time.Second):
		t.Fatal("the save did not return")
	}
	mu.Lock()
	joined := strings.Join(logs, "\n")
	mu.Unlock()
	assert.Contains(t, joined, analysis.ErrPreambleLimit.Error(), "the save logs the form that stopped")
	assert.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app),
		"make-fn keeps the definition it had before the save")
}

// TestIndexBuildBoundsPreamble checks that the index build stops a
// preamble form that does not end and still loads the macros after it.
// Every request waits for the build, so an unbounded form would stop
// the server before its first answer.
func TestIndexBuildBoundsPreamble(t *testing.T) {
	dir := t.TempDir()
	macrosSrc := "(in-package 'demo)\n" + loopingPreambleSet +
		"(defmacro make-fn (name) (list 'defun 'alpha '() 1))\n"
	appPath := filepath.Join(dir, "app.lisp")
	require.NoError(t, os.WriteFile(filepath.Join(dir, "macros.lisp"), []byte(macrosSrc), 0o600))
	require.NoError(t, os.WriteFile(appPath, []byte(macroSaveApp), 0o600))
	s := testServer()
	s.rootPath = dir
	done := make(chan struct{})
	go func() {
		defer close(done)
		s.ensureWorkspaceIndex()
	}()
	select {
	case <-done:
	case <-time.After(30 * time.Second):
		t.Fatal("the index build did not end")
	}
	app := openDoc(s, pathToURI(appPath), macroSaveApp)
	assert.Equal(t, []string{"beta"}, undefinedSymbols(t, s, app),
		"the macro after the stopped form is loaded")
}

// logCapture is a glsp context that keeps the window/logMessage texts.
type logCapture struct {
	mu   sync.Mutex
	logs []string
}

func (c *logCapture) ctx() *glsp.Context {
	return &glsp.Context{Notify: func(method string, params any) {
		if p, ok := params.(*protocol.LogMessageParams); ok && method == "window/logMessage" {
			c.mu.Lock()
			c.logs = append(c.logs, p.Message)
			c.mu.Unlock()
		}
	}}
}

func (c *logCapture) text() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	return strings.Join(c.logs, "\n")
}

// TestSaveRetriesStoppedReload checks that a reload that stopped at a
// limit is tried again by the next save, even when the file did not
// change between the saves. The scan of the file follows the first save;
// a comparison of it with the second would find nothing to do, and the
// file would keep its old macros until its preamble changes.
func TestSaveRetriesStoppedReload(t *testing.T) {
	w := newMacroSaveWorkspace(t, makeAlpha)
	require.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app))

	w.s.macroReloadTimeout = 0 // the context of the reload has ended
	w.save(t, makeBeta)
	require.Equal(t, []string{"beta"}, undefinedSymbols(t, w.s, w.app),
		"the stopped reload left the old macro")

	w.s.macroReloadTimeout = defaultMacroReloadTimeout
	w.save(t, makeBeta)
	assert.Equal(t, []string{"alpha"}, undefinedSymbols(t, w.s, w.app),
		"the second save of the same text reloads the file")

	up := w.s.updateFileIndex(pathToURI(w.macrosPath))
	assert.False(t, up.macros, "after a reload that ended, the same text evaluates nothing")
}

// TestSaveLoadsFilesTheBuildSkipped checks that a file whose preamble the
// index build did not reach before its deadline is named in a warning,
// and loads at its next save even with the same text.
func TestSaveLoadsFilesTheBuildSkipped(t *testing.T) {
	dir := t.TempDir()
	macrosPath := filepath.Join(dir, "macros.lisp")
	appPath := filepath.Join(dir, "app.lisp")
	require.NoError(t, os.WriteFile(macrosPath, []byte(makeAlpha), 0o600))
	require.NoError(t, os.WriteFile(appPath, []byte(macroSaveApp), 0o600))
	s := testServer()
	s.rootPath = dir
	s.macroBuildTimeout = 0 // the build deadline has passed
	logs := &logCapture{}
	s.captureNotify(logs.ctx())
	s.ensureWorkspaceIndex()
	assert.Contains(t, logs.text(), "macros.lisp", "the warning names the skipped file")
	w := &macroSaveWorkspace{s: s, macrosPath: macrosPath, app: openDoc(s, pathToURI(appPath), macroSaveApp)}
	require.ElementsMatch(t, []string{"ignored", "alpha", "beta"}, undefinedSymbols(t, s, w.app),
		"the build loaded no macro; the scan still knows the name make-fn")

	w.save(t, makeAlpha)
	assert.Equal(t, []string{"beta"}, undefinedSymbols(t, s, w.app),
		"the save loads the skipped file")
}
