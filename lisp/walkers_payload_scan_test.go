// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"go/ast"
	"go/types"
	"sort"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"
)

// The PAYLOAD SCAN, rebuilt so that it fails on a CLASS rather than on one
// spelling.
//
// THE TRIGGERING EXAMPLE.  (*LVal).Copy used to decide "this Native is a
// cell-view link, drop it" with a bare `v.Native.(*LVal)` -- keyed on the
// payload type, with no gate on the header type -- so every LNative whose
// payload happened to be a *LVal came back from Copy with a nil payload.
// That is the shape lisp.NativeOf[*lisp.LVal] writes (native.go, issue
// #546), and lisp `(stable-sort < boxes key)` reaches it through
// lvalByFun.Less.  It shipped green, and the reason it shipped green is
// what this file is about: the old scan collected `case *T:` arms from
// `switch ….Native.(type)` in a HARDCODED list of two files, fork.go and
// detach.go.  So it was blind three times over --
//
//   - to a type ASSERTION rather than a type switch (Copy's site);
//   - to payload handling expressed as an `if` with no type named at all
//     (fork.go's `if v.IsCellView()` block, which writes cp.Native twice
//     before the type switch is reached);
//   - to any file outside those two, lisp.go included -- even though
//     (*LVal).Copy is a REGISTERED walker with Rebuilds: true.
//
// Worse, the memoExemptions row for "*LVal" was marked used ONLY because
// detach.go happened to spell its arm as a `case`.  Rewrite that one arm as
// an `if` and the shrink-only checker would have called the row dead and
// demanded its deletion -- while fork.go and Copy both still handled the
// payload.  Row liveness was coupled to an unrelated walker's syntax.
//
// WHAT REPLACES IT.  The scan starts from the property that makes a site
// payload handling at all, not from a spelling: a function that ASSIGNS to
// a header's Native field is rebuilding or re-pointing payload storage, and
// that is where two names for one payload are born.  Package lisp holds
// exactly five such functions today and the scan finds them by type-checking
// the package, so no file list can go stale.  Within each, a payload type is
// collected however it is named:
//
//	case *T:                in a `switch x := ….Native.(type)`   -> "case"
//	….Native.(*T)           a type assertion anywhere            -> "assert"
//	v.IsCellView() / v.CellView() / v.cellsView()                -> "accessor"
//
// The accessor form is what makes the `if`-shaped handling visible: those
// three functions ARE the read of a *LVal off Native (the convention on
// cellsView says readers use exactly these), so calling one names the *LVal
// payload just as surely as asserting it.
//
// A CONSTRUCTOR is deliberately not a payload site.  `&LVal{Native: x}`
// MINTS a payload onto a header that had none; a walker ASSIGNS one onto a
// header it copied from another. Only the second can put two names on one
// payload, which is the whole subject of walkers.go.

// payloadSite is one place a Native-writing function names a payload type.
type payloadSite struct {
	Typ  string // "*MapData", "*LVal", "NativeCloner" -- as memoExemptions spells it
	Func string // enclosing function, as funcLabel renders it
	Form string // "case", "assert" or "accessor"
	File string // "lisp.go"
	Pos  string // "lisp.go:1747"
}

func (s payloadSite) String() string {
	return fmt.Sprintf("%s in %s (%s): %s", s.Pos, s.Func, s.Form, s.Typ)
}

// cellViewAccessors are the three readers of the cell-view link.  Calling
// one is a read of a *LVal off Native (the convention on cellsView).
var cellViewAccessors = map[string]bool{
	"IsCellView": true,
	"CellView":   true,
	"cellsView":  true,
}

// nativePayloadSites type-checks package lisp's production files and
// returns every payload type named inside a function that writes a Native
// field.  It also returns the file each registered walker is DECLARED in,
// so the registry can be checked against what the scan actually visited.
func nativePayloadSites() (sites []payloadSite, declaredIn map[string]string, err error) {
	pkgs, err := packages.Load(&packages.Config{
		Mode: packages.NeedName | packages.NeedSyntax | packages.NeedTypes |
			packages.NeedTypesInfo | packages.NeedFiles,
		Dir: ".",
	}, ".")
	if err != nil {
		return nil, nil, fmt.Errorf("load package lisp: %w", err)
	}
	declaredIn = map[string]string{}
	for _, pkg := range pkgs {
		if len(pkg.Errors) != 0 {
			return nil, nil, fmt.Errorf("%s: %w", pkg.ID, pkg.Errors[0])
		}
		for _, f := range pkg.Syntax {
			file := shortName(pkg.Fset.Position(f.Pos()).Filename)
			for _, decl := range f.Decls {
				switch d := decl.(type) {
				case *ast.GenDecl:
					for _, spec := range d.Specs {
						if ts, ok := spec.(*ast.TypeSpec); ok {
							declaredIn[ts.Name.Name] = file
						}
					}
				case *ast.FuncDecl:
					label := funcLabel(pkg, d)
					declaredIn[label] = file
					if d.Body == nil || !writesNativeField(d.Body) {
						continue
					}
					sites = append(sites, collectPayloadTypes(pkg, d, label, file)...)
				}
			}
		}
	}
	sort.Slice(sites, func(i, j int) bool { return sites[i].Pos < sites[j].Pos })
	return sites, declaredIn, nil
}

// writesNativeField reports whether the body assigns to a `.Native` field.
// A composite literal is deliberately not a write; see the file comment.
func writesNativeField(body *ast.BlockStmt) bool {
	found := false
	ast.Inspect(body, func(n ast.Node) bool {
		as, ok := n.(*ast.AssignStmt)
		if !ok {
			return true
		}
		for _, lhs := range as.Lhs {
			if sel, ok := lhs.(*ast.SelectorExpr); ok && sel.Sel.Name == "Native" {
				found = true
			}
		}
		return true
	})
	return found
}

// collectPayloadTypes returns every payload type the function names, in any
// of the three forms.
func collectPayloadTypes(pkg *packages.Package, fd *ast.FuncDecl, label, file string) []payloadSite {
	var out []payloadSite
	at := func(n ast.Node) string {
		p := pkg.Fset.Position(n.Pos())
		return fmt.Sprintf("%s:%d", file, p.Line)
	}
	add := func(n ast.Node, typ, form string) {
		out = append(out, payloadSite{Typ: typ, Func: label, Form: form, File: file, Pos: at(n)})
	}
	ast.Inspect(fd.Body, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.TypeSwitchStmt:
			if !strings.HasSuffix(types.ExprString(typeSwitchSubject(x)), ".Native") {
				return true
			}
			for _, stmt := range x.Body.List {
				cc, ok := stmt.(*ast.CaseClause)
				if !ok {
					continue
				}
				for _, e := range cc.List {
					if id, ok := e.(*ast.Ident); ok && id.Name == "nil" {
						continue
					}
					add(e, types.ExprString(e), "case")
				}
			}
		case *ast.TypeAssertExpr:
			if x.Type == nil || !strings.HasSuffix(types.ExprString(x.X), ".Native") {
				return true
			}
			add(x, types.ExprString(x.Type), "assert")
		case *ast.CallExpr:
			if sel, ok := x.Fun.(*ast.SelectorExpr); ok && cellViewAccessors[sel.Sel.Name] {
				add(x, "*LVal", "accessor")
			}
		}
		return true
	})
	return out
}

// typeSwitchSubject returns the expression a type switch switches on:
// `x := v.Native.(type)` and `switch v.Native.(type)` alike.
func typeSwitchSubject(sw *ast.TypeSwitchStmt) ast.Expr {
	var assert *ast.TypeAssertExpr
	switch a := sw.Assign.(type) {
	case *ast.AssignStmt:
		if len(a.Rhs) == 1 {
			assert, _ = a.Rhs[0].(*ast.TypeAssertExpr)
		}
	case *ast.ExprStmt:
		assert, _ = a.X.(*ast.TypeAssertExpr)
	}
	if assert == nil {
		return &ast.Ident{Name: ""}
	}
	return assert.X
}

func shortName(path string) string {
	if i := strings.LastIndex(path, "/"); i >= 0 {
		return path[i+1:]
	}
	return path
}

// walkerOwningFunc maps an enclosing function label to the registered
// walker it belongs to, or "" when it belongs to none.  Resolved by
// function, never by file: fork.go holds free functions that are not part
// of the walk, and a file-level rule would sweep them in.
func walkerOwningFunc(label string) string {
	for _, m := range WalkerMemos() {
		if m.Walker == label {
			return m.Walker // "(*LVal).Copy"
		}
		if strings.HasPrefix(label, "(") {
			if i := strings.Index(label, ")"); i > 0 &&
				strings.TrimPrefix(label[1:i], "*") == m.Walker {
				return m.Walker // "(*forker).val" -> "forker"
			}
		}
	}
	return ""
}

// TestThePayloadScanSeesEveryRebuildingWalker ties the scan to the
// REGISTRY rather than to a file list.  The old scan named fork.go and
// detach.go in a Go slice literal, so (*LVal).Copy -- registered, and
// Rebuilds: true -- was never read at all.  A rebuilding walker whose
// declaring file the scan does not reach, or which handles no payload the
// scan can see, fails here.
func TestThePayloadScanSeesEveryRebuildingWalker(t *testing.T) {
	sites, declaredIn, err := nativePayloadSites()
	if err != nil {
		t.Fatal(err)
	}
	if len(sites) == 0 {
		t.Fatal("the payload scan found no site in package lisp; it has stopped looking")
	}
	visited := map[string]bool{}
	byWalker := map[string][]payloadSite{}
	for _, s := range sites {
		visited[s.File] = true
		if w := walkerOwningFunc(s.Func); w != "" {
			byWalker[w] = append(byWalker[w], s)
		}
	}
	for _, m := range WalkerMemos() {
		if !m.Rebuilds {
			continue
		}
		file, ok := declaredIn[m.Walker]
		if !ok {
			t.Errorf("registered walker %s is not declared anywhere in package lisp;\n"+
				"either the registry row in lisp/walkers.go is stale or the walker moved out of the package.",
				m.Walker)
			continue
		}
		if !visited[file] {
			t.Errorf("registered walker %s is declared in %s, which the payload scan never read.\n"+
				"The scan must cover every file holding a rebuilding walker -- that blind spot is\n"+
				"exactly how (*LVal).Copy's cell-view arm shipped unexamined.", m.Walker, file)
		}
		if len(byWalker[m.Walker]) == 0 {
			t.Errorf("registered walker %s handles no payload the scan can see.\n"+
				"A walker with Rebuilds: true writes a header's Native field; if it no longer does,\n"+
				"its registry row is wrong, and if it does, the scan has stopped recognising the form.",
				m.Walker)
		}
	}

	// The named pin: Copy's cell-view decision must be visible to the scan.
	// On the commit that introduced the bug this site was a type assertion
	// in lisp.go and the scan read neither the form nor the file.
	found := false
	for _, s := range byWalker["(*LVal).Copy"] {
		if s.Typ == "*LVal" {
			found = true
		}
	}
	if !found {
		t.Errorf("the scan does not see (*LVal).Copy handling a *LVal payload.\n"+
			"That handling is its cell-view arm, and it is the site whose blindness this test exists\n"+
			"to close. Sites seen for Copy: %v", byWalker["(*LVal).Copy"])
	}

	var summary []string
	for _, s := range sites {
		summary = append(summary, s.String())
	}
	t.Logf("%d payload sites in package lisp:\n%s", len(sites), strings.Join(summary, "\n"))
}

// TestCellViewLinkIsGatedOnTheHeaderType is the poka-yoke for the class the
// bug belongs to, not for its one instance.
//
// A *LVal read off Native is a cell-view LINK only when the header carrying
// it is an LSExpr -- that is the first thing cellsView checks, before it
// looks at the field at all.  On any other header the same payload is
// embedder DATA.  So a function that acts on a *LVal payload must state the
// header-type gate: either by going through one of the three accessors,
// which apply it, or by testing LSExpr itself, which is what
// (*detacher).detach does.  Naming the payload type and acting on it with
// neither gate present is the bug: it is what (*LVal).Copy did, and
// reverting Copy to that shape fails this test.
func TestCellViewLinkIsGatedOnTheHeaderType(t *testing.T) {
	sites, _, err := nativePayloadSites()
	if err != nil {
		t.Fatal(err)
	}
	gated := map[string]bool{} // function -> states the header-type gate
	for _, s := range sites {
		if s.Form == "accessor" {
			gated[s.Func] = true
		}
	}
	names, err := lsExprNamingFuncs()
	if err != nil {
		t.Fatal(err)
	}
	for fn := range names {
		gated[fn] = true
	}

	checked := 0
	for _, s := range sites {
		if s.Typ != "*LVal" || s.Form == "accessor" {
			continue
		}
		checked++
		if gated[s.Func] {
			continue
		}
		t.Errorf("%s reads a *LVal off a Native field and acts on it without gating on the header type.\n"+
			"A *LVal payload is a cell-view LINK only on an LSExpr (the convention on cellsView, which\n"+
			"checks the header type before it reads the field); on an LNative the same payload is\n"+
			"embedder data written by lisp.NativeOf[*lisp.LVal] (native.go, issue #546). Keying the\n"+
			"decision on the payload type alone is how Copy came to destroy every such payload -- see\n"+
			"TestCopyKeepsNativeLValPayloadUnderStableSort.\n"+
			"Gate it: call IsCellView/CellView, or test v.Type == LSExpr in this function.", s)
	}
	if checked == 0 {
		t.Log("no ungated *LVal payload assertion in the package; every site goes through an accessor")
	}
}

// lsExprNamingFuncs returns the functions that mention the LSExpr constant,
// i.e. those that state the header-type gate inline rather than through an
// accessor.
func lsExprNamingFuncs() (map[string]bool, error) {
	pkgs, err := packages.Load(&packages.Config{
		Mode: packages.NeedName | packages.NeedSyntax | packages.NeedTypes |
			packages.NeedTypesInfo | packages.NeedFiles,
		Dir: ".",
	}, ".")
	if err != nil {
		return nil, err
	}
	out := map[string]bool{}
	for _, pkg := range pkgs {
		for _, f := range pkg.Syntax {
			for _, decl := range f.Decls {
				fd, ok := decl.(*ast.FuncDecl)
				if !ok || fd.Body == nil {
					continue
				}
				label := funcLabel(pkg, fd)
				ast.Inspect(fd.Body, func(n ast.Node) bool {
					if id, ok := n.(*ast.Ident); ok && id.Name == "LSExpr" {
						out[label] = true
					}
					return true
				})
			}
		}
	}
	return out, nil
}
