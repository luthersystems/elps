// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/types"
	"path/filepath"
	"slices"
	"sort"
	"strings"

	"golang.org/x/tools/go/analysis"
)

// lTypeSwitchAnalyzer requires exhaustive LType switches in audited value walkers.
// It checks the tag's Go type, including aliases, locals, and method results.
// A default arm always fails. Cases cover constant values, including named aliases.
// Code outside lTypeSwitchScope remains outside this rule.
var lTypeSwitchAnalyzer = &analysis.Analyzer{
	Name: "elpsltypeswitch",
	Doc:  "require every lisp.LType constant and forbid default arms in audited value walkers",
	Run:  runLTypeSwitch,
}

// lTypeSwitchScope lists the audited files as "package path/file name". A nil
// function list covers the whole file. Otherwise only the named functions
// (methods by name) are checked, because the file has other LType switches
// whose default arm is a deliberate type error.
var lTypeSwitchScope = map[string][]string{
	lispPkgPath + "/shape.go":                     nil,
	lispPkgPath + "/loader.go":                    nil,
	lispPkgPath + "/detach.go":                    nil,
	lispPkgPath + "/copier.go":                    nil,
	lispPkgPath + "/maps.go":                      nil,
	lispPkgPath + "/embed.go":                     nil,
	lispPkgPath + "/seal.go":                      nil,
	lispPkgPath + "/ownership_check_elpscheck.go": nil,
	lispPkgPath + "/lisp.go":                      {"equalShallow", "equalIter"},
	libjsonPkgPath + "/encode.go":                 nil,
	libjsonPkgPath + "/json.go":                   nil,
	libjsonPkgPath + "/canonize.go":               {"value"},
	libjsonPkgPath + "/tag.go":                    {"value"},
	libjsonPkgPath + "/untag.go":                  {"value"},
}

const libjsonPkgPath = "github.com/luthersystems/elps/lisp/lisplib/libjson"

// lTypeSwitchFuncs reports whether a file is in scope and, if so, which
// functions are checked (nil means all).
func lTypeSwitchFuncs(pkg, filename string) (funcs []string, ok bool) {
	funcs, ok = lTypeSwitchScope[pkg+"/"+filepath.Base(filename)]
	return funcs, ok
}

func runLTypeSwitch(pass *analysis.Pass) (any, error) {
	for _, file := range pass.Files {
		funcs, ok := lTypeSwitchFuncs(pass.Pkg.Path(), pass.Fset.Position(file.Pos()).Filename)
		if !ok {
			continue
		}
		for _, decl := range file.Decls {
			if funcs != nil {
				fd, isFunc := decl.(*ast.FuncDecl)
				if !isFunc || !slices.Contains(funcs, fd.Name.Name) {
					continue
				}
			}
			checkLTypeSwitches(pass, decl)
		}
	}
	return nil, nil
}

func checkLTypeSwitches(pass *analysis.Pass, decl ast.Decl) {
	ast.Inspect(decl, func(n ast.Node) bool {
		sw, ok := n.(*ast.SwitchStmt)
		if !ok || sw.Tag == nil || !isLispNamed(pass.TypesInfo.TypeOf(sw.Tag), "LType") {
			return true
		}
		named := types.Unalias(pass.TypesInfo.TypeOf(sw.Tag)).(*types.Named)
		constants := map[string][]string{}
		for _, name := range named.Obj().Pkg().Scope().Names() {
			c, ok := named.Obj().Pkg().Scope().Lookup(name).(*types.Const)
			if ok && types.Identical(c.Type(), named) {
				constants[c.Val().ExactString()] = append(constants[c.Val().ExactString()], name)
			}
		}
		for _, stmt := range sw.Body.List {
			arm := stmt.(*ast.CaseClause)
			if arm.List == nil {
				pass.Reportf(arm.Pos(), "lisp.LType switch has a default arm; name every constant explicitly")
			}
			for _, expr := range arm.List {
				if value := pass.TypesInfo.Types[expr].Value; value != nil {
					delete(constants, value.ExactString())
				}
			}
		}
		var missing []string
		for _, names := range constants {
			missing = append(missing, strings.Join(names, "/"))
		}
		sort.Strings(missing)
		if len(missing) > 0 {
			pass.Reportf(sw.Pos(), "lisp.LType switch misses constants: %s; name every constant explicitly", strings.Join(missing, ", "))
		}
		return true
	})
}
