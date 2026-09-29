// Copyright © 2026 The ELPS authors

package astutil

import "github.com/luthersystems/elps/lisp"

// MacroExpander expands one macro call.  It has the method set of
// analysis.MacroExpander, so an *analysis.EnvMacroExpander can be passed
// directly.  ExpandMacro returns nil when form is not a macro call or cannot
// be expanded.
type MacroExpander interface {
	ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal
}

// WalkCode walks form as ELPS code without expanding anything, calling
// visit for every event (see lisp.WalkEvent).  Unlike Walk, it knows which
// parts of each special form are code, bindings or data: it reports the
// names each binding form introduces, opens and closes their scopes, and
// never descends into quoted data.  Heads are classified statically by
// lisp.DefaultSpecialOpName.
func WalkCode(form *lisp.LVal, visit lisp.CodeVisitor) {
	w := &lisp.CodeWalker{Visit: visit}
	w.Walk(form)
}

// ExpandAll returns form with every macro call exp can expand replaced by
// its full expansion, in code position only, calling visit (which may be
// nil) for every event of the expanded code.  pkg is the package form is
// read in, passed to exp.
//
// A call whose head is lexically bound in form (a local function, a let
// variable, a macrolet name) is not expanded.  Calls to macrolet macros
// are reported as opaque forms: without an environment they cannot be
// expanded.  A macro exp cannot expand is left as a call.
//
// form is never modified.  Nodes the expansion did not touch are returned
// as the same values, and every rebuilt list keeps the source location of
// the list it replaces, so positions still point into the original file;
// nodes a macro synthesized carry whatever location the expander gave
// them.  lisp.LEnv.MacroExpandAll is the variant that resolves heads in a
// live environment and expands local macros too.
func ExpandAll(form *lisp.LVal, exp MacroExpander, pkg string, visit lisp.CodeVisitor) *lisp.LVal {
	w := &lisp.CodeWalker{Visit: visit}
	if exp != nil {
		w.Expand1 = func(f *lisp.LVal) (*lisp.LVal, bool) {
			r := exp.ExpandMacro(f, pkg)
			if r == nil {
				return nil, false
			}
			return r, true
		}
	}
	out := w.Walk(form)
	if out != nil && out.Type == lisp.LError && (form == nil || form.Type != lisp.LError) {
		// Only depth limits fail a walk with no expansion errors; leave
		// the form as written.
		return form
	}
	return out
}
