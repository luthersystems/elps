// Copyright © 2026 The ELPS authors

package main

import (
	"go/ast"
	"go/types"
	"path/filepath"
	"sort"
	"strings"

	"golang.org/x/tools/go/analysis"
)

// lTypeSwitchAnalyzer requires exhaustive LType switches in audited value walkers.
// It checks the tag's Go type, including aliases, locals, and method results.
// A default arm always fails. Cases cover constant values, including named aliases.
// Files outside lTypeSwitchFiles and internal/valwalk remain outside this rule.
var lTypeSwitchAnalyzer = &analysis.Analyzer{
	Name: "elpsltypeswitch",
	Doc:  "require every lisp.LType constant and forbid default arms in audited value walkers",
	Run:  runLTypeSwitch,
}

var lTypeSwitchFiles = map[string]bool{
	"shape.go":  true,
	"loader.go": true,
	"detach.go": true,
	// TODO: Add these scopes after their exhaustive-dispatch migrations:
	// lisp/copier.go
	// lisp/lisp.go (equalShallow, equalIter)
	// lisp/lisplib/libjson/encode.go
	// lisp/maps.go
	// lisp/embed.go
	// lisp/lisplib/libjson/json.go
	// lisp/seal.go
	// lisp/ownership_check_elpscheck.go
}

func inLTypeSwitchScope(pkg, filename string) bool {
	return pkg == lispPkgPath && lTypeSwitchFiles[filepath.Base(filename)] ||
		pkg == "github.com/luthersystems/elps/internal/valwalk" ||
		strings.HasPrefix(pkg, "github.com/luthersystems/elps/internal/valwalk/")
}

func runLTypeSwitch(pass *analysis.Pass) (any, error) {
	for _, file := range pass.Files {
		if !inLTypeSwitchScope(pass.Pkg.Path(), pass.Fset.Position(file.Pos()).Filename) {
			continue
		}
		ast.Inspect(file, func(n ast.Node) bool {
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
	return nil, nil
}
