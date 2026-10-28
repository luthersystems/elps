// Copyright © 2026 The ELPS authors

package lint

import (
	"sort"
	"strings"
	"sync"

	"github.com/luthersystems/elps/analysis"
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
		for _, top := range pass.Exprs {
			head := unqualifiedLispName(HeadSymbol(top))
			if top.Type == lisp.LSExpr && len(top.Cells) == 2 && !userDefs[HeadSymbol(top)] {
				name := astutil.PackageNameArg(top.Cells[1])
				switch head {
				case "in-package":
					pkg = name // "" for a dynamic package: no imports match it.
					continue
				case "use-package":
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
					pass.ReportNode(top.Cells[1],
						"use-package '%s hides lisp names: %s; use %s qualified, or call the core forms as lisp:NAME",
						name, strings.Join(names, ", "), name)
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

// reportHiddenCalls reports each call in form whose bare head is a name
// hides.  FindCalls skips quoted data and lexically bound names; a call in a
// quasiquote template is data, which
// ClassifyNodes marks.
func reportHiddenCalls(pass *Pass, form *lisp.LVal, hides map[string]string, userDefs map[string]bool) {
	names := make([]string, 0, len(hides))
	for n := range hides {
		names = append(names, n)
	}
	roles := astutil.ClassifyNodes(form)
	for _, site := range astutil.FindCalls(form, names...) {
		from, ok := hides[site.Name]
		if !ok || userDefs[site.Name] || roles.Role(site.Form) == astutil.RoleData {
			continue
		}
		pass.ReportNode(site.Form.Cells[0], "%s here is %s:%s, which use-package '%s brings in; call lisp:%s for the core form",
			site.Name, from, site.Name, from, site.Name)
	}
}

var stdlibHiddenLispNames = sync.OnceValue(func() map[string][]string {
	exports := analysis.ExtractPackageExports(newStdlibEnv().Runtime.Registry)
	core := map[string]bool{}
	for _, sym := range exports[lisp.DefaultLangPackage] {
		core[sym.Name] = true
	}
	hidden := map[string][]string{}
	for pkg, syms := range exports {
		if pkg == lisp.DefaultLangPackage || pkg == lisp.DefaultUserPackage {
			continue
		}
		for _, sym := range syms {
			if core[sym.Name] {
				hidden[pkg] = append(hidden[pkg], sym.Name)
			}
		}
		sort.Strings(hidden[pkg])
	}
	return hidden
})
