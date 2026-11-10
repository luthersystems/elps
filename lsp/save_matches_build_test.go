// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"math/rand/v2"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// saveStep is one change of a workspace file followed by the
// notification an editor sends for it. remove deletes the file.
type saveStep struct {
	file   string
	src    string
	remove bool
}

// workspaceTables are the tables of the analysis config that the index
// build merges from the workspace files.
type workspaceTables struct {
	ExtraGlobals   []analysis.ExternalSymbol
	PackageExports map[string][]analysis.ExternalSymbol
	PackageSymbols map[string][]analysis.ExternalSymbol
	PackageImports map[string][]string
	DefForms       []analysis.DefFormSpec
	DefaultPackage string
	FilePackages   map[string]string
	RecordMethods  map[string]analysis.RecordMethods
}

// tablesOf returns the workspace tables of the config of s. files are
// the names of the workspace files, for the record methods.
func tablesOf(s *Server, dir string, files []string) workspaceTables {
	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()
	tables := workspaceTables{
		ExtraGlobals:   cfg.ExtraGlobals,
		PackageExports: cfg.PackageExports,
		PackageSymbols: cfg.PackageSymbols,
		PackageImports: cfg.PackageImports,
		DefForms:       cfg.DefForms,
		DefaultPackage: cfg.DefaultPackage,
		FilePackages:   cfg.FilePackages,
		RecordMethods:  make(map[string]analysis.RecordMethods),
	}
	for _, name := range files {
		if m := cfg.RecordMethodsByFile.File(filepath.Join(dir, name)); len(m) > 0 {
			tables.RecordMethods[name] = m
		}
	}
	return tables
}

// describeTables prints the names in the tables, in order, for a failure
// message that a person can read.
func describeTables(tables workspaceTables) string {
	var out strings.Builder
	list := func(title string, syms []analysis.ExternalSymbol) {
		fmt.Fprintf(&out, "%s:", title)
		for _, sym := range syms {
			file := "-"
			if sym.Source != nil {
				file = fmt.Sprintf("%s:%d", filepath.Base(sym.Source.File), sym.Source.Line)
			}
			fmt.Fprintf(&out, " %s:%s@%s", sym.Package, sym.Name, file)
		}
		out.WriteString("\n")
	}
	table := func(title string, t map[string][]analysis.ExternalSymbol) {
		pkgs := make([]string, 0, len(t))
		for pkg := range t {
			// The registry packages are the same on both sides.
			if len(t[pkg]) > 0 && t[pkg][0].Source != nil {
				pkgs = append(pkgs, pkg)
			}
		}
		slices.Sort(pkgs)
		for _, pkg := range pkgs {
			list(title+"["+pkg+"]", t[pkg])
		}
	}
	list("globals", tables.ExtraGlobals)
	table("exports", tables.PackageExports)
	table("symbols", tables.PackageSymbols)
	fmt.Fprintf(&out, "imports: %v\ndef forms: %v\ndefault package: %q\nfile packages: %v\nrecord methods: %v\n",
		tables.PackageImports, tables.DefForms, tables.DefaultPackage, tables.FilePackages, tables.RecordMethods)
	return out.String()
}

// requireSavesMatchBuild writes the files, builds the index, and applies
// the steps. After each step the tables of the server must equal those
// of a new server that builds its index over the files as they are then.
func requireSavesMatchBuild(t *testing.T, files map[string]string, steps []saveStep) {
	t.Helper()
	dir := t.TempDir()
	names := make(map[string]bool)
	write := func(name, src string) {
		path := filepath.Join(dir, name)
		require.NoError(t, os.MkdirAll(filepath.Dir(path), 0o700))
		require.NoError(t, os.WriteFile(path, []byte(src), 0o600))
		names[name] = true
	}
	for name, src := range files {
		write(name, src)
	}
	s := testServer()
	s.rootPath = dir
	s.buildWorkspaceIndex()

	for i, step := range steps {
		path := filepath.Join(dir, step.file)
		if step.remove {
			require.NoError(t, os.Remove(path))
			s.removeFileDefinitions(path)
		} else {
			write(step.file, step.src)
			s.updateFileDefinitions(pathToURI(path))
		}

		fresh := testServer()
		fresh.rootPath = dir
		fresh.buildWorkspaceIndex()
		all := make([]string, 0, len(names))
		for name := range names {
			all = append(all, name)
		}
		slices.Sort(all)
		want, got := tablesOf(fresh, dir, all), tablesOf(s, dir, all)
		if !assert.ObjectsAreEqual(want, got) {
			require.Equal(t, describeTables(want), describeTables(got),
				"step %d (%s): the tables after the save differ from a new index build", i, step.file)
			require.Equal(t, want, got, "step %d (%s)", i, step.file)
		}
	}
}

// TestSaveGivesTheTablesOfANewBuild checks that the workspace tables
// after a sequence of saves are those of a new index build over the same
// files (luthersystems/elps#943). Tables that are updated entry by entry
// drift from the build: the order of the entries follows the saves, a
// removed import stays, and an export that lost a tie does not come
// back.
func TestSaveGivesTheTablesOfANewBuild(t *testing.T) {
	for _, tc := range []struct {
		name  string
		files map[string]string
		steps []saveStep
	}{
		{
			name: "move a defun to another package",
			files: map[string]string{
				"a.lisp": "(in-package 'a)\n(export 'one)\n(defun one () 1)\n(defun two () 2)\n",
				"b.lisp": "(in-package 'b)\n(defun three () 3)\n",
				"c.lisp": "(in-package 'a)\n(defun four () 4)\n",
			},
			steps: []saveStep{
				{file: "a.lisp", src: "(in-package 'a)\n(export 'one)\n(defun one () 1)\n(in-package 'b)\n(defun two () 2)\n"},
				{file: "a.lisp", src: "(in-package 'b)\n(export 'one)\n(defun one () 1)\n(defun two () 2)\n"},
				{file: "a.lisp", src: "(in-package 'a)\n(export 'one)\n(defun one () 1)\n(defun two () 2)\n"},
			},
		},
		{
			name: "drop a use-package",
			files: map[string]string{
				"lib.lisp":  "(in-package 'lib)\n(export 'one 'two)\n(defun one () 1)\n(defun two () 2)\n",
				"app.lisp":  "(in-package 'app)\n(use-package 'lib)\n(use-package 'other)\n(defun main () (+ (one) (two)))\n",
				"app2.lisp": "(in-package 'app)\n(use-package 'other)\n(defun more () 2)\n",
			},
			steps: []saveStep{
				{file: "app.lisp", src: "(in-package 'app)\n(use-package 'other)\n(defun main () 1)\n"},
				{file: "app2.lisp", src: "(in-package 'app)\n(defun more () 2)\n"},
				{file: "app.lisp", src: "(in-package 'app)\n(defun main () 1)\n"},
				{file: "app.lisp", src: "(in-package 'app)\n(use-package 'lib)\n(defun main () (+ (one) (two)))\n"},
			},
		},
		{
			name: "drop an export that two files had",
			files: map[string]string{
				"a1.lisp": "(in-package 'a)\n(export 'f)\n(defun f () 1)\n",
				"a2.lisp": "(in-package 'a)\n(export 'f)\n(defun f (x) x)\n",
			},
			steps: []saveStep{
				{file: "a2.lisp", src: "(in-package 'a)\n(defun g () 2)\n"},
				{file: "a2.lisp", src: "(in-package 'a)\n(export 'f)\n(defun f (x) x)\n"},
				{file: "a1.lisp", src: "(in-package 'a)\n(defun h () 1)\n"},
				{file: "a2.lisp", remove: true},
			},
		},
		{
			name: "add and remove a macro",
			files: map[string]string{
				"m.lisp":   "(in-package 'm)\n(defun plain () 1)\n",
				"use.lisp": "(in-package 'm)\n(defun user () 2)\n",
			},
			steps: []saveStep{
				{file: "m.lisp", src: "(in-package 'm)\n(export 'defthing)\n(defmacro defthing (name &rest body) (list 'defun name '() 1))\n(defun plain () 1)\n"},
				{file: "m.lisp", src: "(in-package 'm)\n(defmacro defthing (name) (list 'defun name '() 1))\n(defmacro defother (name) (list 'defun name '() 2))\n"},
				{file: "m.lisp", src: "(in-package 'm)\n(defun plain () 1)\n"},
			},
		},
		{
			name: "create and delete files",
			files: map[string]string{
				"a.lisp":     "(in-package 'p)\n(defun a () 1)\n",
				"a/b.lisp":   "(in-package 'p)\n(defun b () 1)\n",
				"sub/z.lisp": "(in-package 'p)\n(export 'dup)\n(defun dup () 1)\n",
			},
			steps: []saveStep{
				{file: "a/a.lisp", src: "(in-package 'p)\n(export 'dup)\n(defun dup () 2)\n"},
				{file: "a-b.lisp", src: "(in-package 'p)\n(defun ab () 1)\n"},
				{file: "sub/m.lisp", src: "(in-package 'p)\n(s:defmethod order size (o) 0)\n"},
				{file: "zz.lisp", src: "(in-package 'p)\n(export 'dup)\n(defun dup () 3)\n"},
				{file: "sub/z.lisp", remove: true},
				{file: "a.lisp", remove: true},
				{file: "sub/m.lisp", src: "(in-package 'p)\n(s:defmethod order weight (o) 0)\n"},
				{file: "zz.lisp", remove: true},
			},
		},
		{
			name: "the load tree of main.lisp",
			files: map[string]string{
				"main.lisp":  "(in-package 'app)\n(load-file \"bare.lisp\")\n(defun main () 1)\n",
				"bare.lisp":  "(use-package 'extra)\n(export 'helper)\n(defun helper () 1)\n",
				"loose.lisp": "(defun loose () 2)\n",
				"late.lisp":  "(defun early () 3)\n(in-package 'lib)\n(defun one () 1)\n",
			},
			steps: []saveStep{
				{file: "bare.lisp", src: "(export 'helper)\n(defun helper () 1)\n(defun helper2 () 2)\n"},
				{file: "main.lisp", src: "(in-package 'app)\n(load-file \"bare.lisp\")\n(in-package 'tail)\n(load-file \"late.lisp\")\n(defun main () 1)\n"},
				{file: "main.lisp", src: "(in-package 'other)\n(load-file \"late.lisp\")\n(defun main () 1)\n"},
				{file: "loose.lisp", src: "(defun loose () 2)\n(defun looser () 3)\n"},
				{file: "main.lisp", remove: true},
			},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			requireSavesMatchBuild(t, tc.files, tc.steps)
		})
	}
}

// TestRandomSavesGiveTheTablesOfANewBuild applies random sequences of
// saves and deletions over a pool of file texts.
func TestRandomSavesGiveTheTablesOfANewBuild(t *testing.T) {
	texts := []string{
		"(in-package 'a)\n(export 'f)\n(defun f () 1)\n(defun g () 2)\n",
		"(in-package 'a)\n(use-package 'b)\n(export 'f 'h)\n(defun f (x) x)\n(defun h () 2)\n",
		"(in-package 'b)\n(use-package 'a)\n(export 'g)\n(defun g () 1)\n(defmacro defx (n) (list 'defun n '() 1))\n",
		"(in-package 'b)\n(defun f () 1)\n(in-package 'a)\n(export 'g)\n(defun g () 3)\n",
		"(defun bare () 1)\n(export 'bare)\n(use-package 'a)\n",
		"(in-package 'c)\n(s:defmethod order size (o) 0)\n(set 'v 1)\n",
		"(in-package 'a\n",
		"",
	}
	names := []string{"one.lisp", "two.lisp", "dir/one.lisp", "dir/three.lisp", "four.lisp"}
	for seed := range uint64(12) {
		t.Run(fmt.Sprintf("seed=%d", seed), func(t *testing.T) {
			rng := rand.New(rand.NewPCG(seed, 7)) //nolint:gosec // a repeatable test sequence
			files := map[string]string{}
			present := map[string]bool{}
			for _, name := range names[:3] {
				files[name] = texts[rng.IntN(len(texts))]
				present[name] = true
			}
			var steps []saveStep
			for range 14 {
				name := names[rng.IntN(len(names))]
				if present[name] && rng.IntN(5) == 0 {
					steps = append(steps, saveStep{file: name, remove: true})
					present[name] = false
					continue
				}
				steps = append(steps, saveStep{file: name, src: texts[rng.IntN(len(texts))]})
				present[name] = true
			}
			requireSavesMatchBuild(t, files, steps)
		})
	}
}

// TestSaveReanalyzesOtherOpenDocuments checks that a save that removes
// an export publishes new diagnostics for the other open documents. Their
// analysis read the tables of before the save.
func TestSaveReanalyzesOtherOpenDocuments(t *testing.T) {
	old := recordReanalyzeDelay
	recordReanalyzeDelay = time.Millisecond
	t.Cleanup(func() { recordReanalyzeDelay = old })

	dir := t.TempDir()
	libPath := filepath.Join(dir, "lib.lisp")
	appPath := filepath.Join(dir, "app.lisp")
	appSrc := "(in-package 'app)\n(use-package 'lib)\n(defun main () (+ (one) (two)))\n"
	require.NoError(t, os.WriteFile(libPath, []byte("(in-package 'lib)\n(export 'one 'two)\n(defun one () 1)\n(defun two () 2)\n"), 0o600))
	require.NoError(t, os.WriteFile(appPath, []byte(appSrc), 0o600))

	s := testServer()
	s.rootPath = dir
	// ensureWorkspaceIndex, so the first request does not build a second
	// index over an environment that holds the first one's packages.
	s.ensureWorkspaceIndex()
	ctx, captured := capturingContext()
	var mu sync.Mutex
	notify := ctx.Notify
	ctx.Notify = func(method string, params any) {
		mu.Lock()
		defer mu.Unlock()
		notify(method, params)
	}
	s.captureNotify(ctx)
	appURI := pathToURI(appPath)
	undefinedOne := func() bool {
		mu.Lock()
		defer mu.Unlock()
		for i := len(*captured) - 1; i >= 0; i-- {
			p := (*captured)[i]
			if p.URI != appURI {
				continue
			}
			for _, d := range p.Diagnostics {
				if strings.Contains(d.Message, "undefined symbol: one") {
					return true
				}
			}
			return false
		}
		return false
	}
	require.NoError(t, s.textDocumentDidOpen(ctx, &protocol.DidOpenTextDocumentParams{
		TextDocument: protocol.TextDocumentItem{URI: appURI, Version: 1, Text: appSrc},
	}))
	require.False(t, undefinedOne(), "lib exports one before the save")

	require.NoError(t, os.WriteFile(libPath, []byte("(in-package 'lib)\n(export 'two)\n(defun one () 1)\n(defun two () 2)\n"), 0o600))
	require.NoError(t, s.textDocumentDidSave(ctx, &protocol.DidSaveTextDocumentParams{
		TextDocument: protocol.TextDocumentIdentifier{URI: pathToURI(libPath)},
	}))
	require.Eventually(t, undefinedOne, 5*time.Second, 5*time.Millisecond,
		"the open document is analyzed again without the export")
}
