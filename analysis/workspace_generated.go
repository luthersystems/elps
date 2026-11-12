// Copyright © 2026 The ELPS authors

package analysis

import (
	"bytes"
	"os"
	"reflect"
	"runtime"
	"slices"
	"strings"
	"sync"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// generatedDefs holds the package-level definitions that the top-level
// macro calls of one file make (luthersystems/elps#953). The scan of a
// file reads its text only, so it cannot see them: a call such as
// (defhelper foo) defines foo only when the macro expands. Without them
// in the workspace tables, foo resolves in the file of the call, whose
// own analysis expands it, and is an undefined symbol in every other
// file.
//
// The fields mirror those of scannedFile, so Prescan merges them with the
// same code. Their packages are the ones the analysis of the file gives
// them (Config.FilePackages, DefaultPackage), so the file's own analysis
// and the other files agree on where a generated name lives.
type generatedDefs struct {
	allDefs []ExternalSymbol
	globals []ExternalSymbol
	pkgs    map[string][]ExternalSymbol
	pkgAll  map[string][]ExternalSymbol
}

// empty reports whether g adds nothing to the tables.
func (g *generatedDefs) empty() bool {
	return len(g.allDefs) == 0
}

// hasMacroCandidates reports whether a form that the prescan of an
// analysis offers for expansion (a top-level form, also inside a top-level
// progn: expansionPackageForms) is a call that is not a package form.
// Only such a call can be a macro call that defines a name. The scan
// records it, so WithGeneratedDefinitions reads and expands only the files
// that have one: most files of a phylum hold only defun and set forms, and
// an editor computes the definitions again on each save that loads a
// macro.
//
// It runs on each scan of a file, so a file with no progn costs no
// allocation: expansionPackageForms is called only for a progn body (the
// declarations that it adds for a form nested in a top-level form are
// package forms, so they never count).
func hasMacroCandidates(exprs []*lisp.LVal) bool {
	for _, form := range exprs {
		head := astutil.HeadSymbol(form)
		if head == "" || form.IsQuoted() {
			continue
		}
		head = strings.TrimPrefix(head, "lisp:")
		if head == "progn" {
			for _, inner := range expansionPackageForms(form.Cells[1:]) {
				h := astutil.HeadSymbol(inner)
				if h != "" && !inner.IsQuoted() && !isPackageFormHead(strings.TrimPrefix(h, "lisp:")) {
					return true
				}
			}
			continue
		}
		if !isPackageFormHead(head) {
			return true
		}
	}
	return false
}

// fileGeneratedDefinitions runs the prescan of the analysis of one file
// with cfg and returns the package-level definitions its macro calls make.
// The prescan is the part of the analysis that expands top-level macro
// calls (expandPackageForms); the deep walk adds no package-level
// definition, so it is not run.
//
// A definition whose name has no location in the file (a macro that
// builds the name, or writes it in its own body) takes the location of
// the call. An entry of the tables must be in the file that makes it:
// the go-to-definition of a use reaches the call, and the tables drop the
// entry when the file changes.
//
// A definition the scan of the text already records is left out. A defun
// written in the arguments of a macro call, (with-logging (defun h ...)),
// is found by both, at the same position; recording it twice would list
// it twice in completion and in the tables.
func fileGeneratedDefinitions(source []byte, filename string, cfg *Config, textDefs []ExternalSymbol) generatedDefs {
	s := token.NewScanner(filename, bytes.NewReader(source))
	exprs, err := rdparser.New(s).ParseProgram()
	if err != nil {
		return generatedDefs{}
	}
	a := newAnalyzer(ConfigForFile(cfg, filename))
	a.prescan(exprs, a.root)

	type textKey struct {
		name      string
		line, col int
	}
	inText := make(map[textKey]bool, len(textDefs))
	for _, d := range textDefs {
		if d.Source != nil {
			inText[textKey{d.Name, d.Source.Line, d.Source.Col}] = true
		}
	}

	var g generatedDefs
	for _, sym := range a.result.Symbols {
		if sym.GeneratedBy == nil || sym.External || sym.Scope != a.root {
			continue
		}
		if sym.Source != nil && sym.Source.File == filename &&
			inText[textKey{sym.Name, sym.Source.Line, sym.Source.Col}] {
			continue
		}
		src := sym.Source
		if src == nil || src.File != filename {
			src = sym.GeneratedBy.CallSite
		}
		if src == nil {
			continue
		}
		ext := ExternalSymbol{
			Name:        sym.Name,
			Kind:        sym.Kind,
			Package:     sym.Package,
			Signature:   sym.Signature,
			Source:      src,
			DocString:   sym.DocString,
			GeneratedBy: sym.GeneratedBy,
		}
		g.allDefs = append(g.allDefs, ext)
		if g.pkgAll == nil {
			g.pkgAll = make(map[string][]ExternalSymbol)
		}
		g.pkgAll[ext.Package] = append(g.pkgAll[ext.Package], ext)
		if sym.Exported {
			if g.pkgs == nil {
				g.pkgs = make(map[string][]ExternalSymbol)
			}
			g.globals = append(g.globals, ext)
			g.pkgs[ext.Package] = append(g.pkgs[ext.Package], ext)
		}
	}
	return g
}

// WithGeneratedDefinitions returns a scan in which each file named in
// paths (every file when paths is nil) also gives the workspace tables
// the definitions that its top-level macro calls make. It reports
// whether that changed the tables. Prescan merges them with the
// definitions of the text.
//
// cfg must have a MacroExpander whose env holds the workspace macros
// (LoadWorkspaceMacros), and the tables of a Prescan of w: the package
// imports and exports decide which heads are macros. Without an
// expander it returns w. WithFile keeps what the calls of a file defined
// before the change, so the tables do not lose the names meanwhile and
// the result here reports a change against them; the caller computes
// them again after a change: for the changed file, or for every file
// when the macros changed.
//
// The definitions are found by an expansion with the tables of cfg,
// which do not hold the definitions that macro calls make. So a macro
// call is expanded only when a definition of the text, or of the
// workspace macros, makes its head a macro.
//
// read gives the text of a file, by its path in the scan, and whether it
// could be read; a file that cannot be read gives no definitions. A nil
// read reads the file from disk. An editor passes the text it scanned, so
// the definitions and the scan of a file come from the same text, and its
// own size limit.
func (w *WorkspaceScan) WithGeneratedDefinitions(cfg *Config, paths []string, read func(path string) ([]byte, bool)) (*WorkspaceScan, bool) {
	if cfg == nil || cfg.MacroExpander == nil {
		return w, false
	}
	var todo []int
	consider := func(i int) {
		if w.mayGenerate(i) {
			todo = append(todo, i)
		}
	}
	if paths == nil {
		for i := range w.paths {
			consider(i)
		}
	} else {
		for _, p := range paths {
			if i := w.find(p); i >= 0 {
				consider(i)
			}
		}
	}
	if len(todo) == 0 {
		// Before the config is indexed below: most saves land here.
		return w, false
	}
	if read == nil {
		read = func(path string) ([]byte, bool) {
			source, err := os.ReadFile(path) //nolint:gosec // G304: reads a workspace file the scan listed
			return source, err == nil
		}
	}

	fileCfg := *cfg
	if !fileCfg.GlobalIndex.indexes(fileCfg.ExtraGlobals) {
		// Each file is analyzed with the same globals: index them once
		// (see GlobalIndex).
		fileCfg.GlobalIndex = NewGlobalIndex(fileCfg.ExtraGlobals)
	}

	found := make([]generatedDefs, len(todo))
	numWorkers := min(runtime.NumCPU(), len(todo))
	work := make(chan int, len(todo))
	for k := range todo {
		work <- k
	}
	close(work)
	var wg sync.WaitGroup
	wg.Add(numWorkers)
	for range numWorkers {
		go func() {
			defer wg.Done()
			for k := range work {
				i := todo[k]
				if !w.results[i].macroCandidates {
					// It lost its calls: found[k] stays empty.
					continue
				}
				source, ok := read(w.paths[i])
				if !ok {
					continue
				}
				found[k] = fileGeneratedDefinitions(source, w.paths[i], &fileCfg, w.results[i].allDefs)
			}
		}()
	}
	wg.Wait()

	next := *w
	next.results = slices.Clone(w.results)
	changed := false
	for k, i := range todo {
		if !sameGenerated(&w.results[i].generated, &found[k]) {
			changed = true
		}
		next.results[i].generated = found[k]
	}
	return &next, changed
}

// mayGenerate reports whether file i can have generated definitions, or
// has some to drop.
func (w *WorkspaceScan) mayGenerate(i int) bool {
	return w.results[i].macroCandidates || !w.results[i].generated.empty()
}

// MayGenerateDefinitions reports whether WithGeneratedDefinitions can
// change what the file at path gives to the tables: the file has a call
// that may be a macro call, or definitions that earlier calls made. A
// caller that must build a config for WithGeneratedDefinitions asks it
// first, so a save of a file of defun forms costs no more than before.
func (w *WorkspaceScan) MayGenerateDefinitions(path string) bool {
	i := w.find(path)
	return i >= 0 && w.mayGenerate(i)
}

// sameGenerated reports whether two sets of generated definitions give
// the same entries to the tables, the source positions aside.
func sameGenerated(a, b *generatedDefs) bool {
	if a.empty() && b.empty() {
		return true
	}
	strip := func(syms []ExternalSymbol) []ExternalSymbol {
		out := make([]ExternalSymbol, len(syms))
		for i, sym := range syms {
			sym.Source, sym.GeneratedBy = nil, nil
			out[i] = sym
		}
		return out
	}
	return reflect.DeepEqual(strip(a.allDefs), strip(b.allDefs)) &&
		reflect.DeepEqual(strip(a.globals), strip(b.globals))
}
