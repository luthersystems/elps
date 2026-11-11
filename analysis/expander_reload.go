// Copyright © 2026 The ELPS authors

package analysis

import (
	"context"
	"slices"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// ReloadFileMacros brings the env of the expander from the file at path
// as old describes it to the file as next describes it, after an editor
// saved, created or deleted the file. A nil scan, or one that does not
// hold the file, means the file has no forms. It reports whether it
// changed the macros of the env; only a change of the preamble of the
// file makes it evaluate anything.
//
// The index build loads the macros of the workspace into the env one
// time. Without a reload, a save that changes a macro leaves the old
// definition in the env, and the analysis of every file expands the old
// macro until the server starts again (luthersystems/elps#949).
//
// The reload evaluates the new preamble of the file (FilePreamble) in
// the package the index build evaluated it in: the build runs the
// preambles of all files in a row, so a file with no in-package form
// runs in the package the file before it left active. A changed
// definition replaces the old one. A macro that the old preamble defined
// and the new one does not is retired: an env cannot unbind a symbol, so
// the expander refuses to expand the value bound now. The reload holds
// the lock of ExpandMacro, so a concurrent analysis sees the env before
// or after it, never in between.
//
// The evaluation has the limits of LoadWorkspaceMacrosContext, because
// every expansion waits for it. When a form stops at a limit, the macros
// of the file keep the definitions they had before the reload, nothing
// is retired, and ReloadFileMacros returns false with the errors. A form
// that fails for another reason is reported and the reload goes on, as
// at the index build.
//
// Limits:
//
//   - A macro that another file of next also defines is not retired; the
//     env keeps the definition it has, which may be that of the saved file.
//   - A defun or a set that the file no longer has stays bound. Only a
//     macro body reads them, at expansion. A defun or set evaluated before
//     a form that stopped at a limit keeps its new value.
//   - The other preamble forms of the file are evaluated again: a set of
//     a counter at load time counts one more time, as a second (load) of
//     the file would.
//   - A save that changes the package the file leaves active does not
//     move the macros of a bare file after it; they move when that file
//     is saved, or at the next index build.
//
// Callers that can reload one file from two goroutines must serialize
// the calls, in the order of the scans.
func (e *EnvMacroExpander) ReloadFileMacros(ctx context.Context, old, next *WorkspaceScan, path string) (bool, []error) {
	if e.Env == nil {
		return false, nil
	}
	before := filePreambleOf(old, path)
	after := filePreambleOf(next, path)
	if samePreamble(before, after) {
		return false, nil
	}

	e.mu.Lock()
	defer e.mu.Unlock()
	start := ""
	if e.Env.Runtime.Package != nil {
		start = e.Env.Runtime.Package.Name
	}
	// One walk of each scan. The walk of old is memoized: old was next
	// at the reload before.
	afterPkg := next.loadPackageOf(path, start)
	kept := preambleMacros(after, afterPkg)
	var removed []macroName
	for _, m := range preambleMacros(before, old.loadPackageOf(path, start)) {
		if !slices.Contains(kept, m) {
			removed = append(removed, m)
		}
	}
	if len(removed) > 0 && next != nil {
		nextPkgs := next.loadPackages(start)
		removed = slices.DeleteFunc(removed, func(m macroName) bool {
			return next.othersDefineMacro(path, m, nextPkgs)
		})
	}

	saved := e.bindings(kept)
	forms := after
	if afterPkg != start && len(after) > 0 {
		forms = lisp.Cells{inPackageForm(afterPkg)}.Append(after...)
	}
	load := LoadWorkspaceMacrosContext(ctx, e.Env, forms)
	e.notMacro = nil
	if load.Stopped {
		e.restore(saved)
		return false, load.Errors
	}
	for _, m := range removed {
		e.retire(m)
	}
	return true, load.Errors
}

// inPackageForm returns the form (in-package 'pkg).
func inPackageForm(pkg string) *lisp.LVal {
	return lisp.Cells{lisp.Symbol("in-package"), lisp.Quote(lisp.Symbol(pkg))}.SExpr()
}

// macroBinding is what the env binds to a macro name before a reload.
type macroBinding struct {
	m     macroName
	value *lisp.LVal
	bound bool
}

// bindings returns the values the env binds to the names of macros now.
// The caller holds e.mu.
func (e *EnvMacroExpander) bindings(macros []macroName) []macroBinding {
	saved := make([]macroBinding, 0, len(macros))
	for _, m := range macros {
		b := macroBinding{m: m}
		if pkg := e.Env.Runtime.Registry.Package(m.pkg); pkg != nil {
			b.value, b.bound = pkg.Symbol(m.name)
		}
		saved = append(saved, b)
	}
	return saved
}

// restore binds the names of saved to their values again, after a reload
// that stopped at a limit. A name that had no binding cannot be unbound,
// so the macro the reload bound to it is retired. The caller holds e.mu.
func (e *EnvMacroExpander) restore(saved []macroBinding) {
	for _, b := range saved {
		if !b.bound {
			e.retire(b.m)
			continue
		}
		if pkg := e.Env.Runtime.Registry.Package(b.m.pkg); pkg != nil {
			pkg.Put(lisp.Symbol(b.m.name), b.value)
		}
	}
}

// filePreambleOf is FilePreamble of a scan that may be nil.
func filePreambleOf(scan *WorkspaceScan, path string) []*lisp.LVal {
	if scan == nil {
		return nil
	}
	forms, _ := scan.FilePreamble(path)
	return forms
}

// samePreamble reports whether two preambles evaluate the same forms.
// The forms are compared without their source positions: a save that
// only moves a form (a line added above it) must not evaluate the file
// again.
func samePreamble(a, b []*lisp.LVal) bool {
	return slices.EqualFunc(a, b, sameForm)
}

// sameForm reports whether two parsed forms are the same code. Each save
// compares the whole preamble of the file, so lists, symbols and strings
// are compared in place; rendering every form as text cost an allocation
// per node. Other atoms (numbers) are compared as text.
func sameForm(x, y *lisp.LVal) bool {
	if x == y {
		return true
	}
	if x == nil || y == nil || x.Type != y.Type || x.Str != y.Str ||
		x.IsQuoted() != y.IsQuoted() || x.IsSpliced() != y.IsSpliced() ||
		len(x.Cells) != len(y.Cells) {
		return false
	}
	switch x.Type {
	case lisp.LSExpr, lisp.LQuote:
		return slices.EqualFunc(x.Cells, y.Cells, sameForm)
	case lisp.LSymbol, lisp.LString:
		return true
	default:
		return x.String() == y.String()
	}
}

// macroName is a macro that a preamble defines, with its package.
type macroName struct {
	pkg, name string
}

// preambleMacros returns the macros that the defmacro forms of preamble
// define, each with the package that the in-package forms before it
// select. start is the package before the first form.
func preambleMacros(preamble []*lisp.LVal, start string) []macroName {
	var macros []macroName
	pkg := start
	for _, form := range preamble {
		pkg = nextPackage(form, pkg)
		if astutil.HeadSymbol(form) == "defmacro" && len(form.Cells) > 1 &&
			form.Cells[1].Type == lisp.LSymbol {
			macros = append(macros, macroName{pkg: pkg, name: form.Cells[1].Str})
		}
	}
	return macros
}

// finalPackage returns the package active after preamble, which starts
// in start.
func finalPackage(preamble []*lisp.LVal, start string) string {
	pkg := start
	for _, form := range preamble {
		pkg = nextPackage(form, pkg)
	}
	return pkg
}

// nextPackage returns the package active after form, when pkg is active
// before it. It reads an in-package of a literal name. The package of a
// computed name is known only after evaluation, so it is taken to stay.
func nextPackage(form *lisp.LVal, pkg string) string {
	if astutil.HeadSymbol(form) == "in-package" && len(form.Cells) > 1 {
		if name := preamblePkgName(form.Cells[1]); name != "" {
			return name
		}
	}
	return pkg
}

// othersDefineMacro reports whether a file of the scan other than the
// one at path defines the macro m. pkgs are the loadPackages of the scan.
func (w *WorkspaceScan) othersDefineMacro(path string, m macroName, pkgs []string) bool {
	self := w.find(path)
	for i := range w.results {
		if i == self || !w.results[i].definesMacroNamed(m.name) {
			continue
		}
		if slices.Contains(preambleMacros(w.filePreamble(i), pkgs[i]), m) {
			return true
		}
	}
	return false
}

// definesMacroNamed reports whether the preamble of the file has a
// defmacro of name, in any package. It is the cheap test that runs
// before the package of each form is worked out.
func (r *scannedFile) definesMacroNamed(name string) bool {
	for _, form := range r.preamble {
		if astutil.HeadSymbol(form) == "defmacro" && len(form.Cells) > 1 &&
			form.Cells[1].IsSymbol(name) {
			return true
		}
	}
	return false
}

// retire marks the macro bound to m now as removed. The caller holds e.mu.
func (e *EnvMacroExpander) retire(m macroName) {
	pkg := e.Env.Runtime.Registry.Package(m.pkg)
	if pkg == nil {
		return
	}
	v, ok := pkg.Symbol(m.name)
	// A builtin macro is never retired: its FID names the builtin, not one
	// definition, so the mark would hide it in every package.
	if !ok || v == nil || v.Type != lisp.LFun || !v.IsMacro() || v.Builtin() != nil {
		return
	}
	if e.retired == nil {
		e.retired = make(map[string]struct{})
	}
	e.retired[v.FID()] = struct{}{}
}

// isRetired reports whether a reload removed the macro mac from the
// workspace. The caller holds e.mu.
func (e *EnvMacroExpander) isRetired(mac *lisp.LVal) bool {
	if len(e.retired) == 0 {
		return false
	}
	_, gone := e.retired[mac.FID()]
	return gone
}
