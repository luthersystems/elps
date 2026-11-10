// Copyright © 2026 The ELPS authors

package analysis

import (
	"os"
	"path/filepath"
	"slices"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestIndexFileMatchesPrescan checks that IndexFile gives, for each file
// of a workspace, the entries PrescanWorkspace collects for that file.
// An editor replaces a file's entries with the result of IndexFile on a
// save, so a difference would change the analysis of the other files
// after a save that changed nothing.
func TestIndexFileMatchesPrescan(t *testing.T) {
	dir := t.TempDir()
	files := map[string]string{
		// The package of main.lisp is the default package.
		"main.lisp": "(in-package 'app)\n(use-package 'lib)\n(load-file \"bare.lisp\")\n" +
			"(load-file \"late.lisp\")\n(defun main () (helper))\n",
		// No in-package and in the load tree: rule 1.
		"bare.lisp": "(use-package 'extra)\n(export 'helper)\n(defun helper () 1)\n",
		// No in-package and not in the load tree: rule 2.
		"loose.lisp": "(defun loose () 2)\n",
		// A definition before the first in-package: rule 3.
		"late.lisp": "(defun early () 3)\n(in-package 'lib)\n(export 'one)\n(defun one () 1)\n(defun hidden () 0)\n",
		// Its own package, with a record method.
		"lib2.lisp": "(in-package 'lib)\n(export 'two)\n(defun two () 2)\n(s:defmethod order size (o) 0)\n",
	}
	for name, src := range files {
		require.NoError(t, os.WriteFile(filepath.Join(dir, name), []byte(src), 0o600))
	}

	prescan, err := PrescanWorkspace(dir, nil)
	require.NoError(t, err)
	require.Equal(t, "app", prescan.DefaultPackage)
	cfg := &Config{DefaultPackage: prescan.DefaultPackage, FilePackages: prescan.FilePackages}

	fromFile := func(syms []ExternalSymbol, path string) []ExternalSymbol {
		var out []ExternalSymbol
		for _, sym := range syms {
			if sym.Source != nil && sym.Source.File == path {
				out = append(out, sym)
			}
		}
		return out
	}
	byPackage := func(table map[string][]ExternalSymbol, path string) map[string][]ExternalSymbol {
		out := make(map[string][]ExternalSymbol)
		for pkg, syms := range table {
			if own := fromFile(syms, path); len(own) > 0 {
				out[pkg] = own
			}
		}
		return out
	}
	// sorted drops the empty packages and puts the symbols of each package
	// in source order. A scan gives the definitions of one file in no fixed
	// order, so two scans of a file are compared as sets.
	sorted := func(table map[string][]ExternalSymbol) map[string][]ExternalSymbol {
		out := make(map[string][]ExternalSymbol)
		for pkg, syms := range table {
			if len(syms) > 0 {
				syms = slices.Clone(syms)
				SortDefinitions(syms)
				out[pkg] = syms
			}
		}
		return out
	}

	imports := make(map[string][]string)
	for name, src := range files {
		path := filepath.Join(dir, name)
		idx := IndexFile([]byte(src), path, cfg)
		assert.ElementsMatch(t, fromFile(prescan.AllDefs, path), idx.Defs, "%s: Defs", name)
		assert.Equal(t, sorted(byPackage(prescan.PkgExports, path)), sorted(idx.PkgExports), "%s: PkgExports", name)
		assert.Equal(t, sorted(byPackage(prescan.PkgAllSymbols, path)), sorted(idx.PkgAllSymbols), "%s: PkgAllSymbols", name)
		assert.Equal(t, prescan.RecordMethodsByFile.File(path), idx.RecordMethods, "%s: RecordMethods", name)
		for pkg, names := range idx.PackageImports {
			imports[pkg] = appendUnique(imports[pkg], names...)
		}
	}
	for pkg, names := range prescan.PackageImports {
		assert.ElementsMatch(t, names, imports[pkg], "PackageImports of %s", pkg)
	}
	assert.Len(t, imports, len(prescan.PackageImports))

	// The rules put each definition in the package it runs in.
	pkgOf := make(map[string]string)
	for _, sym := range prescan.AllDefs {
		pkgOf[sym.Name] = sym.Package
	}
	assert.Equal(t, map[string]string{
		"main": "app", "helper": "app", "loose": "app", "early": "app",
		"one": "lib", "hidden": "lib", "two": "lib",
	}, pkgOf)
}
