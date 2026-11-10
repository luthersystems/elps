// Copyright © 2026 The ELPS authors

package lsp

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// undefinedSymbols returns the names that the diagnostics published for
// doc report as undefined, after a new analysis of the document with the
// current workspace config.
func undefinedSymbols(t *testing.T, s *Server, doc *Document) []string {
	t.Helper()
	var names []string
	s.notifyMu.Lock()
	s.notify = func(method string, params any) {
		if method != protocol.ServerTextDocumentPublishDiagnostics {
			return
		}
		for _, d := range publishedDiagnostics(params).Diagnostics {
			if rest, ok := strings.CutPrefix(d.Message, "undefined symbol: "); ok {
				names = append(names, rest)
			}
		}
	}
	s.notifyMu.Unlock()
	doc.mu.Lock()
	doc.analysis = nil
	doc.publishedVersion = 0
	doc.mu.Unlock()
	s.analyzeAndPublish(doc)
	return names
}

// TestSaveRefreshesPackageExports checks that a save of a file replaces
// what the file adds to the package tables of the workspace config, not
// only its entry in ExtraGlobals (luthersystems/elps#943).
//
// A file that imports a package with use-package resolves the names
// through Config.PackageExports. With stale tables, an export added in
// one file stays an undefined symbol in the other files, and a removed
// export stays defined, until the server starts again.
func TestSaveRefreshesPackageExports(t *testing.T) {
	dir := t.TempDir()
	libPath := filepath.Join(dir, "lib.lisp")
	appPath := filepath.Join(dir, "app.lisp")
	write := func(path, src string) {
		t.Helper()
		require.NoError(t, os.WriteFile(path, []byte(src), 0o600))
	}
	write(libPath, "(in-package 'lib)\n(export 'one)\n(defun one () 1)\n")
	appSrc := "(in-package 'app)\n(use-package 'lib)\n" +
		"(defun main () (+ (one) (two)))\n"
	write(appPath, appSrc)

	s := testServer()
	s.rootPath = dir
	s.ensureWorkspaceIndex()
	app := openDoc(s, pathToURI(appPath), appSrc)
	// libSymbols are the names in Config.PackageSymbols of lib. The
	// analysis reads that table for a qualified name (lib:hidden), and
	// completion lists it after "lib:".
	libSymbols := func() []string {
		s.analysisCfgMu.RLock()
		defer s.analysisCfgMu.RUnlock()
		var names []string
		for _, sym := range s.analysisCfg.PackageSymbols["lib"] {
			names = append(names, sym.Name)
		}
		return names
	}
	assert.Equal(t, []string{"two"}, undefinedSymbols(t, s, app), "before the save, lib does not export two")
	assert.ElementsMatch(t, []string{"one"}, libSymbols())

	// The save adds an export and a symbol that is not exported.
	write(libPath, "(in-package 'lib)\n(export 'one 'two)\n(defun one () 1)\n"+
		"(defun two () 2)\n(defun hidden () 3)\n")
	s.updateFileDefinitions(pathToURI(libPath))
	assert.Empty(t, undefinedSymbols(t, s, app),
		"the export that the save added must resolve")
	assert.ElementsMatch(t, []string{"one", "two", "hidden"}, libSymbols())

	// The save removes the export of one and the definition of hidden.
	write(libPath, "(in-package 'lib)\n(export 'two)\n(defun one () 1)\n(defun two () 2)\n")
	s.updateFileDefinitions(pathToURI(libPath))
	assert.Equal(t, []string{"one"}, undefinedSymbols(t, s, app),
		"a name the save no longer exports must not resolve")
	assert.ElementsMatch(t, []string{"one", "two"}, libSymbols())

	// A deleted file adds nothing.
	s.removeFileDefinitions(libPath)
	assert.ElementsMatch(t, []string{"one", "two"}, undefinedSymbols(t, s, app))
	assert.Empty(t, libSymbols())
}

// TestSaveKeepsRegistryExports checks that a save does not remove the
// exports that come from the package registry (the standard library),
// and that a registry entry still wins over a workspace entry of the same
// name in the same package.
func TestSaveKeepsRegistryExports(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "more_string.lisp")
	src := "(in-package 'string)\n(export 'shout 'join)\n(defun shout (s) s)\n(defun join (a b) a)\n"
	require.NoError(t, os.WriteFile(path, []byte(src), 0o600))

	s := testServer()
	s.rootPath = dir
	s.ensureWorkspaceIndex()
	exports := func() map[string]bool {
		s.analysisCfgMu.RLock()
		defer s.analysisCfgMu.RUnlock()
		byName := make(map[string]bool)
		for _, sym := range s.analysisCfg.PackageExports["string"] {
			// A registry entry has no file.
			fromRegistry := sym.Source == nil || sym.Source.File != path
			if _, dup := byName[sym.Name]; dup {
				t.Errorf("string:%s is in the exports two times", sym.Name)
			}
			byName[sym.Name] = fromRegistry
		}
		return byName
	}
	before := exports()
	require.Contains(t, before, "shout")
	require.True(t, before["join"], "the registry entry of string:join wins at the index build")
	require.True(t, before["split"])

	s.updateFileDefinitions(pathToURI(path))
	assert.Equal(t, before, exports(), "a save with no change leaves the exports as the index build made them")

	require.NoError(t, os.WriteFile(path, []byte("(in-package 'string)\n"), 0o600))
	s.updateFileDefinitions(pathToURI(path))
	after := exports()
	assert.NotContains(t, after, "shout")
	assert.True(t, after["join"])
	assert.True(t, after["split"])
}

// TestSaveKeepsLoadTreePackage checks that a save of a file with no
// in-package form keeps its definitions in the package that loads the
// file. The index build puts them there (analysis.PrescanWorkspace). A
// save that moved them to the "user" package would make each one an
// undefined symbol in the other files of the real package.
func TestSaveKeepsLoadTreePackage(t *testing.T) {
	dir := t.TempDir()
	mainPath := filepath.Join(dir, "main.lisp")
	utilPath := filepath.Join(dir, "util.lisp")
	mainSrc := "(in-package 'app)\n(load-file \"util.lisp\")\n(defun main () (helper))\n"
	require.NoError(t, os.WriteFile(mainPath, []byte(mainSrc), 0o600))
	require.NoError(t, os.WriteFile(utilPath, []byte("(defun helper () 1)\n"), 0o600))

	s := testServer()
	s.rootPath = dir
	s.ensureWorkspaceIndex()
	helperPackages := func() []string {
		s.analysisCfgMu.RLock()
		defer s.analysisCfgMu.RUnlock()
		var pkgs []string
		for _, sym := range s.analysisCfg.ExtraGlobals {
			if sym.Name == "helper" {
				pkgs = append(pkgs, sym.Package)
			}
		}
		return pkgs
	}
	require.Equal(t, []string{"app"}, helperPackages())
	doc := openDoc(s, pathToURI(mainPath), mainSrc)
	require.Empty(t, undefinedSymbols(t, s, doc))

	s.updateFileDefinitions(pathToURI(utilPath))
	assert.Equal(t, []string{"app"}, helperPackages())
	assert.Empty(t, undefinedSymbols(t, s, doc))
}

// TestSaveDuringRequests runs request handlers while saves replace the
// workspace tables. A handler reads the fields of the analysis config
// after it releases analysisCfgMu, so a save must store a new config and
// must not write the one a handler holds. Under -race the detector
// reports a write in place.
func TestSaveDuringRequests(t *testing.T) {
	dir := t.TempDir()
	libPath := filepath.Join(dir, "lib.lisp")
	appPath := filepath.Join(dir, "app.lisp")
	libSrc := "(in-package 'lib)\n(export 'one)\n(defun one () 1)\n"
	appSrc := "(in-package 'app)\n(use-package 'lib)\n(defun main () (one))\n"
	require.NoError(t, os.WriteFile(libPath, []byte(libSrc), 0o600))
	require.NoError(t, os.WriteFile(appPath, []byte(appSrc), 0o600))

	s := testServer()
	s.rootPath = dir
	s.ensureWorkspaceIndex()
	appURI := pathToURI(appPath)
	openDoc(s, appURI, appSrc)

	stop := make(chan struct{})
	done := make(chan struct{})
	go func() {
		defer close(done)
		for {
			select {
			case <-stop:
				return
			default:
			}
			s.updateFileDefinitions(pathToURI(libPath))
		}
	}()
	ctx := mockContext()
	for range 200 {
		// didChange drops the analysis, so each round analyzes with the
		// config of that moment.
		s.docs.Change(appURI, 1, appSrc)
		_, err := s.textDocumentCompletion(ctx, &protocol.CompletionParams{
			TextDocumentPositionParams: protocol.TextDocumentPositionParams{
				TextDocument: protocol.TextDocumentIdentifier{URI: appURI},
				Position:     protocol.Position{Line: 2, Character: 19},
			},
		})
		require.NoError(t, err)
		_, err = s.workspaceSymbol(ctx, &protocol.WorkspaceSymbolParams{Query: "one"})
		require.NoError(t, err)
	}
	close(stop)
	<-done
}
