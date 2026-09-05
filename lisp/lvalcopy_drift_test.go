// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"os"
	"sort"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"
)

// The fourth half of the drift guard: STRUCT COPIES of an LVal.
//
// The other three halves start from a walker and check what it declares.
// This one starts from the SOURCE and asks where a payload pointer can
// cross from one header to another at all. `*cp = *v` on an LVal carries
// every pointer field -- Native, and the Cells slice header -- and rebuilds
// nothing; a value copy `cp := *v` does the same and then usually has its
// address taken. Inside a registered walker that is the walker's job, and
// walkers.go's registry governs it. Outside one it is unreviewed sharing,
// and issue #551 plus (*LVal).Copy's *MapData defect are what unreviewed
// sharing looks like after a few years.
//
// So: every such site is either inside a registered walker or on the
// shrink-only allowlist in walkers.go, with a reason that names the class
// it belongs to.
//
// FORMS SCANNED: `*x = *y` (pointer-deref assignment) and `x := *y` /
// `var x = *y` / `x = *y` (value copy of a deref), where the copied type is
// lisp.LVal.
//
// FORMS DELIBERATELY NOT SCANNED, and why this is principled rather than
// convenient: value-typed STORAGE -- []LVal in defformals.go and maps.go,
// the LVal struct fields in singleton.go -- is not itself a copy. Declaring
// a []LVal copies nothing. The copy INTO such storage is an assignment, and
// it is caught by the form-B scan: defformals.go's `syms[i] = *cell` is
// reported and carries a row. That is the falsification -- delete the
// form-B arm below and (*formalsCopier).copy drops from two sites to one,
// which fails its allowlist row's count.
//
// Range copies (`for _, v := range someLValSlice`) are also out of scope
// and this is the weaker exclusion of the two: such a copy is real, but no
// site in package lisp currently ranges a []LVal in a way that publishes
// the copy, and widening the scan to cover the possibility would report
// maps.go's entry enumeration on every run. If a range copy is ever
// published, this scan will not see it.

// lvalCopySite is one struct copy of an LVal in package lisp.
type lvalCopySite struct {
	Pos  string // "lisp.go:1601"
	Func string // enclosing function as the allowlist names it
	Form string // "*x = *y" or "x = *y"
	Src  string // the source line, trimmed
}

func (s lvalCopySite) String() string {
	return fmt.Sprintf("%s in %s: %s", s.Pos, s.Func, s.Src)
}

// TestEveryLValStructCopyIsInAWalkerOrAllowlisted is the fourth half.
func TestEveryLValStructCopyIsInAWalkerOrAllowlisted(t *testing.T) {
	sites, err := lvalStructCopySites()
	if err != nil {
		t.Fatal(err)
	}
	if len(sites) == 0 {
		t.Fatal("the scan found no LVal struct copy in package lisp, which cannot be true while\n" +
			"(*LVal).Copy exists. The scan has stopped looking -- check the form matching below.")
	}

	// "Inside a registered walker" is resolved by ENCLOSING FUNCTION, never
	// by file: fork.go holds both the forker's methods and free functions
	// that are not part of the walk, and a file-level rule would exempt the
	// second along with the first.
	walkers := map[string]bool{}
	for _, m := range WalkerMemos() {
		walkers[m.Walker] = true
	}
	inWalker := func(fn string) bool {
		if walkers[fn] {
			return true // "(*LVal).Copy"
		}
		// A method of a registered walker type: "(*forker).val" -> "forker".
		if strings.HasPrefix(fn, "(") {
			if i := strings.Index(fn, ")"); i > 0 {
				return walkers[strings.TrimPrefix(fn[1:i], "*")]
			}
		}
		return false
	}

	allowed := map[string]LValCopyExemption{}
	for _, e := range LValCopyExemptions() {
		allowed[e.Func] = e
	}

	counts := map[string]int{}
	for _, s := range sites {
		if inWalker(s.Func) {
			continue
		}
		counts[s.Func]++
		if _, ok := allowed[s.Func]; ok {
			continue
		}
		t.Errorf("%s\n"+
			"This copies an LVal struct outside any registered walker, so every pointer field it\n"+
			"holds -- Native, and the Cells slice header -- is now reachable from two headers with\n"+
			"nothing rebuilding it. That is how issue #551 got into (*LVal).Copy.\n\n"+
			"Two remedies, and only these two:\n"+
			"  1. Route the copy through a registered walker (lisp/walkers.go, walkerMemos), so the\n"+
			"     registry governs what it rebuilds; or\n"+
			"  2. Add a row to lvalCopyExemptions in lisp/walkers.go naming the enclosing function,\n"+
			"     its site count, and WHY the sharing is intended -- name the class, not just that\n"+
			"     it is safe.\n\n"+
			"If you are rebasing a branch that adds a value-view or header constructor onto this\n"+
			"one, remedy 2 is almost certainly yours: a view header that records its root is a\n"+
			"deliberate second reference and wants a row saying so.", s)
	}

	// Shrink-only, both directions: a row whose function no longer copies
	// anything is dead, and a function that grew a copy needs re-review.
	for _, e := range LValCopyExemptions() {
		got, ok := counts[e.Func]
		if !ok {
			t.Errorf("lvalCopyExemptions has a row for %q, which no longer contains an LVal struct copy\n"+
				"outside a walker. Delete the row: this allowlist is shrink-only, and a row that outlives\n"+
				"its site is a review that never has to happen again.", e.Func)
			continue
		}
		if got != e.Sites {
			t.Errorf("%s contains %d LVal struct copies; its lvalCopyExemptions row allows %d.\n"+
				"A copy added to an already-allowlisted function is exactly the change the count exists\n"+
				"to catch. Re-read the function, then update the row's Sites and its Reason together.",
				e.Func, got, e.Sites)
		}
	}

	var summary []string
	for _, s := range sites {
		summary = append(summary, s.String())
	}
	sort.Strings(summary)
	t.Logf("%d LVal struct copies in package lisp:\n%s", len(sites), strings.Join(summary, "\n"))
}

// lvalStructCopySites type-checks package lisp -- production files and both
// test variants -- and returns every struct copy of an LVal.
//
// A type checker rather than a syntactic heuristic because the distinction
// that matters is `*cp = *v` on an *LVal versus the same spelling on an
// *LEnv (env.go) or a *token.Location (detach.go), and no amount of name
// matching separates those reliably.
func lvalStructCopySites() ([]lvalCopySite, error) {
	pkgs, err := packages.Load(&packages.Config{
		Mode: packages.NeedName | packages.NeedSyntax | packages.NeedTypes |
			packages.NeedTypesInfo | packages.NeedFiles,
		Dir:   ".",
		Tests: true,
	}, ".")
	if err != nil {
		return nil, fmt.Errorf("load package lisp: %w", err)
	}
	seen := map[string]bool{}
	var out []lvalCopySite
	for _, pkg := range pkgs {
		if len(pkg.Errors) != 0 {
			return nil, fmt.Errorf("%s: %w", pkg.ID, pkg.Errors[0])
		}
		for _, f := range pkg.Syntax {
			collectLValCopies(pkg, f, seen, &out)
		}
	}
	return out, nil
}

func collectLValCopies(pkg *packages.Package, f *ast.File, seen map[string]bool, out *[]lvalCopySite) {
	isLVal := func(e ast.Expr) bool {
		tv, ok := pkg.TypesInfo.Types[e]
		if !ok {
			return false
		}
		named, ok := tv.Type.(*types.Named)
		if !ok {
			return false
		}
		obj := named.Obj()
		return obj != nil && obj.Name() == "LVal" &&
			obj.Pkg() != nil && obj.Pkg().Path() == "github.com/luthersystems/elps/lisp"
	}
	var fn string
	var stack []string
	ast.Inspect(f, func(n ast.Node) bool {
		switch x := n.(type) {
		case *ast.FuncDecl:
			stack = append(stack, fn)
			fn = funcLabel(pkg, x)
		case *ast.AssignStmt:
			for i, rhs := range x.Rhs {
				star, ok := rhs.(*ast.StarExpr)
				if !ok || !isLVal(star) {
					continue
				}
				form := "x = *y"
				if i < len(x.Lhs) {
					if _, deref := x.Lhs[i].(*ast.StarExpr); deref {
						form = "*x = *y"
					}
				}
				record(pkg, x.Pos(), fn, form, seen, out)
			}
		case *ast.ValueSpec:
			for _, v := range x.Values {
				if star, ok := v.(*ast.StarExpr); ok && isLVal(star) {
					record(pkg, x.Pos(), fn, "var x = *y", seen, out)
				}
			}
		}
		return true
	})
	_ = stack
}

func funcLabel(pkg *packages.Package, fd *ast.FuncDecl) string {
	if fd.Recv == nil || len(fd.Recv.List) == 0 {
		return fd.Name.Name
	}
	recv := types.ExprString(fd.Recv.List[0].Type)
	return "(" + recv + ")." + fd.Name.Name
}

func record(pkg *packages.Package, pos token.Pos, fn, form string, seen map[string]bool, out *[]lvalCopySite) {
	p := pkg.Fset.Position(pos)
	short := p.Filename
	if i := strings.LastIndex(short, "/"); i >= 0 {
		short = short[i+1:]
	}
	key := fmt.Sprintf("%s:%d", short, p.Line)
	if seen[key] {
		return
	}
	seen[key] = true
	*out = append(*out, lvalCopySite{Pos: key, Func: fn, Form: form, Src: srcLine(p)})
}

// srcLine returns the trimmed source line at a position, for the report.
func srcLine(p token.Position) string {
	b, err := readFileOnce(p.Filename)
	if err != nil {
		return ""
	}
	lines := strings.Split(string(b), "\n")
	if p.Line-1 >= len(lines) {
		return ""
	}
	return strings.TrimSpace(lines[p.Line-1])
}

var srcCache = map[string][]byte{}

func readFileOnce(name string) ([]byte, error) {
	if b, ok := srcCache[name]; ok {
		return b, nil
	}
	b, err := os.ReadFile(name) //nolint:gosec // a path the go/packages loader produced for this package
	if err != nil {
		return nil, err
	}
	srcCache[name] = b
	return b, nil
}
