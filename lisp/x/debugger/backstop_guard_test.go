// Copyright © 2026 The ELPS authors

package debugger

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// GUARD, not a catch. This passes on the branch that introduces it and is
// meant to keep passing. It exists because the fix for #489 is a *shape* -- a
// wait blocks on the event the debugger emits, and the clock is only a
// backstop -- and a shape that nothing checks is a shape that comes back.
//
// The 200-odd waits in this tree were, until #489, denominated in 2s and 5s
// wall-clock budgets. Under load they stopped measuring the debugger and
// started measuring the machine: 21 of 48 concurrent runs of the dapserver
// package failed on those numbers with the code under test untouched. The
// repair was not a bigger number. It was to wait on the stopped event, the DAP
// message, or the evaluation result, and to leave one named constant behind as
// a hang detector.
//
// Nothing stops the next person writing `2*time.Second` into a new wait, so
// this scans for it. A source scan rather than a runtime check because the
// mistake is textual and the failure it causes is a flake on someone else's
// machine three weeks later.
//
// The scanner refuses what it cannot read. An exemption must be listed by name
// with a reason, and an exemption that no longer matches anything fails too --
// a guard whose allow-list has quietly outgrown the tree is a guard that has
// stopped guarding.

// backstopConst is the name every debug-event wait in this tree must use.
// Both packages declare their own (test helpers do not cross package
// boundaries), with identical documentation.
const backstopConst = "debugEventBackstop"

// backstopExemptions maps an enclosing function name to the reason a wait
// inside it is allowed to use something other than backstopConst.
//
// Every entry is a wait whose *timeout is the assertion*: the test is showing
// that nothing arrives, so a fired timeout is the expected answer and machine
// load can only push it further towards passing. PR #447 calls this class
// "robust by direction". Denominating these in the backstop would make each
// negative check sit for two minutes proving a negative.
var backstopExemptions = map[string]string{
	"tryRead": "the timeout is the assertion -- callers use it to show the DAP server sends " +
		"nothing further, so load can only make it pass. Late messages are not dropped: the " +
		"read stays parked on the session and the next read consumes it.",
	"debugEvalBudgeted": "not a debug-event wait at all -- this is the fuzzwatch scheduled-time " +
		"watchdog around fuzz evaluation, a different instrument with its own floor guard " +
		"(internal/fuzzwatch, MinHonestBudget) and its own open issue (#488).",
}

// waitSite is one wall-clock wait found in the debugger tree's tests.
type waitSite struct {
	Pos  token.Position
	Func string // enclosing function, "" at file scope
	Kind string // "time.After" or "Eventually"
	Expr string // the duration expression as written
}

// waitSitesIn returns every wall-clock wait in f. Shared by the tree scan and
// by the negative control, so the control exercises the code the guard runs.
func waitSitesIn(fset *token.FileSet, f *ast.File) []waitSite {
	var sites []waitSite

	// Enclosing function for a position, resolved by containment rather than
	// by tracking state through Inspect, which loses it inside closures.
	enclosing := func(pos token.Pos) string {
		for _, d := range f.Decls {
			fd, ok := d.(*ast.FuncDecl)
			if !ok {
				continue
			}
			if fd.Pos() <= pos && pos <= fd.End() {
				return fd.Name.Name
			}
		}
		return ""
	}

	ast.Inspect(f, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		sel, ok := call.Fun.(*ast.SelectorExpr)
		if !ok {
			return true
		}
		pkg, ok := sel.X.(*ast.Ident)
		if !ok {
			return true
		}

		var kind string
		var arg ast.Expr
		switch {
		case pkg.Name == "time" && sel.Sel.Name == "After" && len(call.Args) == 1:
			kind, arg = "time.After", call.Args[0]
		case (pkg.Name == "require" || pkg.Name == "assert") &&
			strings.HasPrefix(sel.Sel.Name, "Eventually") && len(call.Args) >= 4:
			// require.Eventually(t, cond, waitFor, tick, ...)
			kind, arg = pkg.Name+"."+sel.Sel.Name, call.Args[2]
		default:
			return true
		}

		pos := fset.Position(call.Pos())
		sites = append(sites, waitSite{
			Pos:  pos,
			Func: enclosing(call.Pos()),
			Kind: kind,
			Expr: exprText(arg),
		})
		return true
	})
	return sites
}

// exprText renders a duration expression the way it was written, for a
// diagnostic a reader can match against the source.
func exprText(e ast.Expr) string {
	switch v := e.(type) {
	case *ast.Ident:
		return v.Name
	case *ast.SelectorExpr:
		return exprText(v.X) + "." + v.Sel.Name
	case *ast.BasicLit:
		return v.Value
	case *ast.ParenExpr:
		return "(" + exprText(v.X) + ")"
	case *ast.BinaryExpr:
		return exprText(v.X) + " " + v.Op.String() + " " + exprText(v.Y)
	case *ast.CallExpr:
		return exprText(v.Fun) + "(...)"
	default:
		return fmt.Sprintf("%T", e)
	}
}

const backstopDiagnosis = "a debug-event wait must be denominated in %s, not in a bare duration.\n" +
	"    The engine and the DAP server both publish the event this wait is for -- a stopped\n" +
	"    event on the callback, a message on the connection, a result on the eval channel.\n" +
	"    Block on that and the wait finishes when the debugger acts, so a loaded machine\n" +
	"    reaches the same verdict as an idle one. A duration here does the opposite: it\n" +
	"    measures how much CPU the OS handed over, which is #489 (21 of 48 concurrent runs\n" +
	"    failed on the 2s and 5s numbers this replaced). Raising the number is not the fix --\n" +
	"    see #443/#452 and #435/#447. If the timeout genuinely IS the assertion (you are\n" +
	"    showing that nothing arrives), add the enclosing function to backstopExemptions\n" +
	"    with the reason."

func TestEveryDebugEventWaitUsesTheBackstop(t *testing.T) {
	root := debuggerRoot(t)

	var files []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "testdata" {
				return fs.SkipDir
			}
			return nil
		}
		if strings.HasSuffix(path, "_test.go") {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walking %s: %v", root, err)
	}

	// A scan that finds nothing reports "everything is fine", which reads
	// exactly like a scan that works. Require it to find the tree.
	const wantAtLeastFiles = 6
	if len(files) < wantAtLeastFiles {
		t.Fatalf("found only %d test file(s) under %s (%v), expected at least %d -- "+
			"the scan is broken, not the tree", len(files), root, files, wantAtLeastFiles)
	}

	usedExemptions := map[string]bool{}
	checked := 0
	for _, path := range files {
		fset := token.NewFileSet()
		f, perr := parser.ParseFile(fset, path, nil, 0)
		if perr != nil {
			t.Errorf("%s: parse: %v", path, perr)
			continue
		}
		for _, s := range waitSitesIn(fset, f) {
			checked++
			if s.Expr == backstopConst {
				continue
			}
			if _, ok := backstopExemptions[s.Func]; ok {
				usedExemptions[s.Func] = true
				continue
			}
			t.Errorf("%s:%d: %s(%s) in %s: "+backstopDiagnosis,
				s.Pos.Filename, s.Pos.Line, s.Kind, s.Expr, s.Func, backstopConst)
		}
	}

	const wantAtLeastWaits = 40
	if checked < wantAtLeastWaits {
		t.Fatalf("evaluated only %d wait(s) across %d file(s), expected at least %d -- "+
			"the scanner has stopped recognising the shapes this tree uses",
			checked, len(files), wantAtLeastWaits)
	}

	// A stale exemption is an allow-list that has outgrown the tree, which is
	// how a guard rots into one that cannot fire.
	for _, name := range slices.Sorted(maps.Keys(backstopExemptions)) {
		if !usedExemptions[name] {
			t.Errorf("backstopExemptions has an entry for %q but no wait in the tree "+
				"is inside a function of that name -- delete the exemption rather than "+
				"leaving the allow-list wider than the code", name)
		}
	}

	t.Logf("checked %d wall-clock wait(s) across %d file(s); %d exemption(s) in force",
		checked, len(files), len(usedExemptions))
}

// NEGATIVE CONTROL for the guard above. The tree scan passes today and is
// meant to pass forever, which makes it indistinguishable from a scan that has
// quietly stopped looking. Drive the same scanner over a synthetic file that
// contains exactly the budgets #489 is about and require them to be caught.
func TestTheBackstopGuardCanFail(t *testing.T) {
	const src = `package p

import (
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

func TestRegression(t *testing.T) {
	require.Eventually(t, func() bool { return paused() }, 2*time.Second, 10*time.Millisecond)
	select {
	case <-ch:
	case <-time.After(5 * time.Second):
		t.Fatal("timeout")
	}
	select {
	case <-ch:
	case <-time.After(debugEventBackstop):
		t.Fatal("hang")
	}
}

func tryRead(timeout time.Duration) {
	select {
	case <-ch:
	case <-time.After(timeout):
	}
}
`
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "synthetic_test.go", src, 0)
	if err != nil {
		t.Fatalf("parsing the fixture: %v", err)
	}

	sites := waitSitesIn(fset, file)
	if len(sites) != 4 {
		t.Fatalf("found %d waits in the fixture, want 4: %v", len(sites), sites)
	}

	var flagged, allowed []string
	for _, s := range sites {
		switch {
		case s.Expr == backstopConst:
			allowed = append(allowed, s.Expr)
		case backstopExemptions[s.Func] != "":
			allowed = append(allowed, s.Func+":"+s.Expr)
		default:
			flagged = append(flagged, s.Expr)
		}
	}

	wantFlagged := []string{"2 * time.Second", "5 * time.Second"}
	if !slices.Equal(flagged, wantFlagged) {
		t.Errorf("guard flagged %v, want %v -- it cannot detect a bare wall-clock budget",
			flagged, wantFlagged)
	}
	wantAllowed := []string{backstopConst, "tryRead:timeout"}
	if !slices.Equal(allowed, wantAllowed) {
		t.Errorf("guard allowed %v, want %v -- it would red the build on a correct wait, or "+
			"wave through an exempted one for the wrong reason", allowed, wantAllowed)
	}
}

// debuggerRoot returns the lisp/x/debugger directory, whichever package inside
// it the test binary was built for.
func debuggerRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for {
		if filepath.Base(dir) == "debugger" {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("no debugger directory above %s", dir)
		}
		dir = parent
	}
}
