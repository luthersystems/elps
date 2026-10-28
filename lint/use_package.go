// Copyright © 2026 The ELPS authors

package lint

import (
	"sort"
	"strings"
	"sync"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// AnalyzerUsePackageHidesLisp warns when a use-package of a standard
// library package hides lisp names, and at each unqualified call that then
// reaches the package's export instead of the core form.  use-package copies
// a package's exports over the caller's bindings, so after (use-package 's)
// a plain (not x) calls the constraint s:not (elps#903).
var AnalyzerUsePackageHidesLisp = &Analyzer{
	Name:     "use-package-hides-lisp",
	Severity: SeverityWarning,
	Doc: "Warn when a use-package hides lisp names, and at each call that then misses the core form.\n\n" +
		"use-package copies the package's exports over the caller's bindings. " +
		"After (use-package 's), not, when, list, error and sorted-map are the s exports, " +
		"so (not x) calls the constraint s:not. Use the package qualified (s:not), " +
		"or call the core form as lisp:not. Only standard library packages are known. " +
		"Suppress with ; nolint:use-package-hides-lisp.",
	Run: func(pass *Pass) error {
		hidden := stdlibHiddenLispNames()
		userDefs := UserDefined(pass.Exprs)
		pkg := lisp.DefaultUserPackage
		// imports maps a package to the names its use-packages hide there,
		// each to the package that hides it.
		imports := map[string]map[string]string{}
		for _, top := range topLevelForms(pass.Exprs) {
			head := unqualifiedLispName(HeadSymbol(top))
			if top.Type == lisp.LSExpr && len(top.Cells) >= 2 && !userDefs[HeadSymbol(top)] {
				switch head {
				case "in-package":
					// Trailing arguments are docstrings.  "" is a dynamic
					// package: no imports match it.
					pkg = literalPackageName(top.Cells[1])
					continue
				case "use-package":
					for _, arg := range top.Cells[1:] {
						name := literalPackageName(arg)
						names := hidden[name]
						if len(names) == 0 || pkg == "" {
							continue
						}
						if imports[pkg] == nil {
							imports[pkg] = map[string]string{}
						}
						for _, n := range names {
							imports[pkg][n] = name
						}
						pass.ReportNode(arg,
							"use-package '%s hides lisp names: %s; use %s qualified, or call the core forms as lisp:NAME",
							name, strings.Join(names, ", "), name)
					}
					continue
				}
			}
			if hides := imports[pkg]; len(hides) > 0 {
				reportHiddenCalls(pass, top, hides, userDefs)
			}
		}
		return nil
	},
}

// topLevelForms returns exprs with each top-level progn replaced by its
// body, so a use-package or in-package in a progn is seen in order.
func topLevelForms(exprs []*lisp.LVal) []*lisp.LVal {
	var out []*lisp.LVal
	for _, e := range exprs {
		if e.Type == lisp.LSExpr && !e.IsQuoted() && unqualifiedLispName(HeadSymbol(e)) == "progn" {
			out = append(out, topLevelForms(e.Cells[1:])...)
			continue
		}
		out = append(out, e)
	}
	return out
}

// literalPackageName returns the package an in-package or use-package
// argument names, or "" when the argument is evaluated at run time.  The
// operators are functions, so an unquoted symbol is a variable.
func literalPackageName(arg *lisp.LVal) string {
	if arg.Type == lisp.LSymbol && !arg.IsQuoted() {
		return ""
	}
	return astutil.PackageNameArg(arg)
}

func reportHiddenCalls(pass *Pass, form *lisp.LVal, hides map[string]string, userDefs map[string]bool) {
	names := make([]string, 0, len(hides))
	for n := range hides {
		names = append(names, n)
	}
	var roles *astutil.Roles
	for _, site := range astutil.FindCalls(form, names...) {
		from, ok := hides[site.Name]
		if !ok || userDefs[site.Name] {
			continue
		}
		if roles == nil {
			roles = astutil.ClassifyNodes(form)
		}
		if roles.Role(site.Form) == astutil.RoleData {
			continue
		}
		pass.ReportNode(site.Form.Cells[0], "%s here is %s:%s, which use-package '%s brings in; call lisp:%s for the core form",
			site.Name, from, site.Name, from, site.Name)
	}
}

// stdlibHiddenLispNames maps each standard library package to the lisp
// names its exports hide.  An export bound to lisp's own value (testing:test
// is lisp:test) hides nothing.
var stdlibHiddenLispNames = sync.OnceValue(func() map[string][]string {
	reg := newStdlibEnv().Runtime.Registry
	core := reg.Package(lisp.DefaultLangPackage)
	hidden := map[string][]string{}
	for _, name := range reg.PackageNames() {
		if name == lisp.DefaultLangPackage || name == lisp.DefaultUserPackage {
			continue
		}
		pkg := reg.Package(name)
		for _, ext := range pkg.Externals() {
			v, ok := pkg.Symbol(ext)
			c, isCore := core.Symbol(ext)
			if ok && isCore && v != c {
				hidden[name] = append(hidden[name], ext)
			}
		}
		sort.Strings(hidden[name])
	}
	return hidden
})
