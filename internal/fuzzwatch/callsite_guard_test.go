// Copyright © 2026 The ELPS authors

package fuzzwatch

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"
	"time"
)

// GUARD, not a catch. Every call site in the tree already clears the floor by
// 50x or more, so this test passes on main and always has. It is here to stop
// a budget arriving BELOW the floor later, which is the specific mistake #453
// exists to prevent and the one PR #447 had to measure its way out of.
//
// It is a source scan rather than a check inside New because New is called
// from inside f.Fuzz bodies: a panic there would be recorded as a crasher and
// attributed to the input, which is a worse failure than the one being
// prevented. A test failure names the file and the number and says why.
//
// The evaluator below is deliberately small. It understands the shapes the
// repository actually uses -- a duration constant, a sum of them, a literal
// multiple of time.Second -- and REFUSES anything else rather than guessing.
// An unrecognised shape fails the test asking for the evaluator to be taught,
// because silently skipping call sites it cannot read is exactly how a guard
// like this rots into one that cannot fail.

// budgetSite is one evaluated fuzzwatch.New call.
type budgetSite struct {
	Pos    token.Position
	Budget time.Duration
}

// budgetsIn returns every fuzzwatch.New budget in f that could be evaluated,
// plus a description of each one that could not. Shared by the repository scan
// and by the negative control below, so the control exercises the same code
// path the real guard runs on.
func budgetsIn(fset *token.FileSet, f *ast.File, consts map[string]time.Duration) (sites []budgetSite, unreadable []string) {
	ast.Inspect(f, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok || sel.Sel.Name != "New" {
			return true
		}
		if pkg, ok := sel.X.(*ast.Ident); !ok || pkg.Name != "fuzzwatch" {
			return true
		}
		if len(call.Args) != 1 {
			return true
		}
		pos := fset.Position(call.Pos())
		d, ok := evalDuration(call.Args[0], consts)
		if !ok {
			unreadable = append(unreadable,
				fmt.Sprintf("%s:%d: %s", pos.Filename, pos.Line, exprString(call.Args[0])))
			return true
		}
		sites = append(sites, budgetSite{Pos: pos, Budget: d})
		return true
	})
	return sites, unreadable
}

const floorDiagnosis = "fuzzwatch resolves scheduler STALL above %s, not CPU SHARE: measured on 4 cores\n" +
	"    under 200 spinners it reported lost=0s while real work ran 103x slower at 0.9%% share.\n" +
	"    A budget this tight cannot tell 'the machine was busy' from 'the code hung', so it will\n" +
	"    fire on innocent inputs under load. PR #447 measured exactly this for a 2s budget and\n" +
	"    rejected it; see #453 and the fuzzwatch package doc."

func TestEveryBudgetIsAboveTheHonestFloor(t *testing.T) {
	root := repoRoot(t)

	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			switch d.Name() {
			case ".git", "testdata", "node_modules", "_examples":
				return fs.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") {
			return nil
		}
		// The probe under internal/fuzzwatch/probe is a manual reproduction
		// harness for the #453 measurement; it deliberately uses a short
		// budget because measuring the instrument is its whole purpose.
		if strings.Contains(filepath.ToSlash(path), "/internal/fuzzwatch/") {
			return nil
		}
		b, rerr := os.ReadFile(path) //nolint:gosec // path comes from WalkDir over the module root, not from input
		if rerr != nil {
			return rerr
		}
		if strings.Contains(string(b), "fuzzwatch.New(") {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walking %s: %v", root, err)
	}

	// A scan that finds nothing reports "all call sites are fine", which is
	// the same output as a scan that works. The floor makes the two different.
	const wantAtLeast = 8
	if len(files) < wantAtLeast {
		t.Fatalf("found fuzzwatch.New in only %d file(s) (%v), expected at least %d -- "+
			"the scan is broken, not the tree", len(files), files, wantAtLeast)
	}

	checked := 0
	for _, path := range files {
		fset := token.NewFileSet()
		f, perr := parser.ParseFile(fset, path, nil, 0)
		if perr != nil {
			t.Errorf("%s: parse: %v", path, perr)
			continue
		}
		sites, unreadable := budgetsIn(fset, f, parseDirConsts(filepath.Dir(path)))
		for _, u := range unreadable {
			t.Errorf("%s: cannot evaluate the budget expression. Teach evalDuration this shape "+
				"rather than leaving the call site unchecked -- an unreadable call site is an "+
				"unguarded one.", u)
		}
		for _, s := range sites {
			checked++
			if s.Budget < MinHonestBudget {
				t.Errorf("%s:%d: fuzzwatch budget is %s, below MinHonestBudget (%s).\n    "+floorDiagnosis,
					s.Pos.Filename, s.Pos.Line, s.Budget, MinHonestBudget, tolerance*tick)
			}
		}
	}

	if checked == 0 {
		t.Fatalf("scanned %d file(s) containing fuzzwatch.New but evaluated no budgets", len(files))
	}
	t.Logf("checked %d fuzzwatch budget(s) across %d file(s) against a %s floor",
		checked, len(files), MinHonestBudget)
}

// NEGATIVE CONTROL for the guard above. The repository scan passes today and
// is expected to pass forever, which makes it indistinguishable from a scan
// that has quietly stopped looking -- the failure shape this codebase keeps
// finding. So drive the same evaluator over a synthetic file holding exactly
// the budget #435 proposed and #447 rejected, and require it to be caught.
func TestTheFloorGuardCanFail(t *testing.T) {
	const src = `package p

import (
	"time"

	"github.com/luthersystems/elps/internal/fuzzwatch"
)

const proposedWatchdog = 2 * time.Second

func f() {
	_ = fuzzwatch.New(proposedWatchdog)          // the #435 proposal
	_ = fuzzwatch.New(500 * time.Millisecond)    // tighter still
	_ = fuzzwatch.New(30 * time.Second)          // a house-sized budget
}
`
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "synthetic_test.go", src, 0)
	if err != nil {
		t.Fatalf("parsing the fixture: %v", err)
	}

	// The fixture declares its own constant, so its const scope is the one
	// file -- which is also a small check that the package-scope collection
	// did not become a requirement to be handed sibling files.
	sites, unreadable := budgetsIn(fset, file, durationConsts([]*ast.File{file}))
	if len(unreadable) != 0 {
		t.Fatalf("the evaluator could not read the fixture: %v", unreadable)
	}
	if len(sites) != 3 {
		t.Fatalf("found %d budgets in the fixture, want 3: %v", len(sites), sites)
	}

	var caught []time.Duration
	var passed []time.Duration
	for _, s := range sites {
		if s.Budget < MinHonestBudget {
			caught = append(caught, s.Budget)
		} else {
			passed = append(passed, s.Budget)
		}
	}

	if want := []time.Duration{2 * time.Second, 500 * time.Millisecond}; !slices.Equal(caught, want) {
		t.Errorf("guard caught %v, want %v -- it cannot detect a sub-floor budget", caught, want)
	}
	if want := []time.Duration{30 * time.Second}; !slices.Equal(passed, want) {
		t.Errorf("guard passed %v, want %v -- it would red the build on a correctly sized budget", passed, want)
	}
}

// repoRoot walks up from the working directory to the module root.
func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("no go.mod found above %s", dir)
		}
		dir = parent
	}
}

// durationConsts collects file-level `name = <duration expr>` declarations so a
// budget written as an identifier can be resolved. Only same-file declarations
// are understood; every call site in this repository declares its watchdog
// beside itself, and a cross-package constant would be reported as
// unevaluatable rather than assumed safe.
// durationConsts collects the duration-valued constants visible to a call
// site, across EVERY file given -- which is every .go file in the call site's
// directory, not just its own.
//
// Package scope, not file scope, because Go's is: lisp.FuzzSharedProgramMultiEnv
// lives in shared_program_fuzz_test.go and writes `fuzzwatch.New(watchdogTimeout)`
// against a constant declared in eval_fuzz_test.go, which is ordinary and
// correct.  Read one file at a time, the guard could not resolve it and reported
// the call site as UNREADABLE -- and its own message is the argument for fixing
// it here rather than at the call site: "Teach evalDuration this shape rather
// than leaving the call site unchecked -- an unreadable call site is an
// unguarded one."  Duplicating the literal at the second call site would have
// silenced the guard by making the two budgets independently editable, which is
// the drift this whole file exists to catch.
func durationConsts(files []*ast.File) map[string]time.Duration {
	out := map[string]time.Duration{}
	// Two passes, so `callDeadline + watchdogGrace` resolves regardless of the
	// order the two constants are declared in -- now also regardless of which
	// FILE each is declared in.
	for range 2 {
		for _, f := range files {
			for _, decl := range f.Decls {
				gd, ok := decl.(*ast.GenDecl)
				if !ok || (gd.Tok != token.CONST && gd.Tok != token.VAR) {
					continue
				}
				for _, spec := range gd.Specs {
					vs, ok := spec.(*ast.ValueSpec)
					if !ok || len(vs.Names) != len(vs.Values) {
						continue
					}
					for i, name := range vs.Names {
						if d, ok := evalDuration(vs.Values[i], out); ok {
							out[name.Name] = d
						}
					}
				}
			}
		}
	}
	return out
}

// parseDirConsts parses every .go file in dir and returns the duration
// constants they declare between them.  Files that do not parse are skipped
// silently: the caller parses the file it actually checks and reports a parse
// error there, so a broken sibling would otherwise be reported twice.
func parseDirConsts(dir string) map[string]time.Duration {
	entries, err := os.ReadDir(dir)
	if err != nil {
		return map[string]time.Duration{}
	}
	fset := token.NewFileSet()
	var files []*ast.File
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".go") {
			continue
		}
		f, perr := parser.ParseFile(fset, filepath.Join(dir, e.Name()), nil, 0)
		if perr != nil {
			continue
		}
		files = append(files, f)
	}
	return durationConsts(files)
}

// evalDuration understands the shapes this repository writes budgets in, and
// nothing else. Returning false is a request to be taught, not permission to
// skip.
func evalDuration(e ast.Expr, consts map[string]time.Duration) (time.Duration, bool) {
	switch v := e.(type) {
	case *ast.Ident:
		d, ok := consts[v.Name]
		return d, ok

	case *ast.ParenExpr:
		return evalDuration(v.X, consts)

	case *ast.BinaryExpr:
		// `N * time.Second`, `time.Second * N`, and sums/differences of
		// durations. A product of two durations is meaningless, so the
		// multiply arm requires exactly one side to be a plain integer.
		switch v.Op { //nolint:exhaustive // only the operators a duration expression can use; everything else is refused below
		case token.ADD, token.SUB:
			l, lok := evalDuration(v.X, consts)
			r, rok := evalDuration(v.Y, consts)
			if !lok || !rok {
				return 0, false
			}
			if v.Op == token.SUB {
				return l - r, true
			}
			return l + r, true

		case token.MUL:
			if n, ok := evalInt(v.X); ok {
				if d, ok := evalDuration(v.Y, consts); ok {
					return time.Duration(n) * d, true
				}
			}
			if n, ok := evalInt(v.Y); ok {
				if d, ok := evalDuration(v.X, consts); ok {
					return time.Duration(n) * d, true
				}
			}
			return 0, false
		}
		return 0, false

	case *ast.SelectorExpr:
		pkg, ok := v.X.(*ast.Ident)
		if !ok || pkg.Name != "time" {
			return 0, false
		}
		switch v.Sel.Name {
		case "Nanosecond":
			return time.Nanosecond, true
		case "Microsecond":
			return time.Microsecond, true
		case "Millisecond":
			return time.Millisecond, true
		case "Second":
			return time.Second, true
		case "Minute":
			return time.Minute, true
		case "Hour":
			return time.Hour, true
		}
		return 0, false
	}
	return 0, false
}

func evalInt(e ast.Expr) (int64, bool) {
	lit, ok := e.(*ast.BasicLit)
	if !ok || lit.Kind != token.INT {
		return 0, false
	}
	n, err := strconv.ParseInt(strings.ReplaceAll(lit.Value, "_", ""), 0, 64)
	if err != nil {
		return 0, false
	}
	return n, true
}

func exprString(e ast.Expr) string {
	var sb strings.Builder
	if err := printExpr(&sb, e); err != nil {
		return "<unprintable>"
	}
	return sb.String()
}

func printExpr(sb *strings.Builder, e ast.Expr) error {
	switch v := e.(type) {
	case *ast.Ident:
		sb.WriteString(v.Name)
	case *ast.BasicLit:
		sb.WriteString(v.Value)
	case *ast.ParenExpr:
		sb.WriteString("(")
		if err := printExpr(sb, v.X); err != nil {
			return err
		}
		sb.WriteString(")")
	case *ast.SelectorExpr:
		if err := printExpr(sb, v.X); err != nil {
			return err
		}
		sb.WriteString("." + v.Sel.Name)
	case *ast.BinaryExpr:
		if err := printExpr(sb, v.X); err != nil {
			return err
		}
		sb.WriteString(" " + v.Op.String() + " ")
		if err := printExpr(sb, v.Y); err != nil {
			return err
		}
	case *ast.CallExpr:
		if err := printExpr(sb, v.Fun); err != nil {
			return err
		}
		sb.WriteString("(...)")
	default:
		sb.WriteString("<expr>")
	}
	return nil
}
