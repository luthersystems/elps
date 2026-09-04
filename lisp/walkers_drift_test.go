// Copyright © 2026 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// The memo drift guard, both halves.
//
// Issue #585 is the whole argument for this file.  The fork walker's
// per-payload memos were added for issue #576 and guarded by a harness that
// only ever drove Fork.  The detach/copy walker had the identical defect,
// nothing compared the two, and it stayed live for the whole time the guard
// was green.  A comparison between the walkers is what would have caught it,
// and it has to be a comparison a machine makes.
//
//   - The REGISTRY half asserts that every walker which rebuilds payload
//     storage memoises the same set of payload kinds.  A kind added to one
//     walker and not the other fails here.
//   - The SOURCE SCAN half reads this package's own source and asserts that
//     every memo-shaped struct field — a map keyed by a pointer type —
//     belongs to a registered walker, and that every field the registry
//     names still exists.  The registry alone cannot catch a BRAND NEW
//     walker nobody told it about, and cannot catch a memo DELETED from a
//     registered one; the scan catches both, deterministically, without any
//     test having to generate the value shape the memo protects.
//   - The PAYLOAD SCAN half reads the two walkers' `v.Native` type switches
//     and asserts that every payload type they copy is either a registered
//     memo kind or an exempted one.  That is issue #585 stated exactly: a
//     payload the walker rebuilds but does not memoise is a payload two
//     headers come apart over.

// mustRebuild names the walkers that MUST declare Rebuilds.  Without this
// pin the registry half is disableable in one edit: set detacher's
// Rebuilds to false and the cross-walker comparison below has nothing left
// to compare, so it passes vacuously while the exact drift it exists to
// catch — issue #585, a payload kind memoised in the forker and not in the
// detacher — goes unreported.  All three halves of the guard stayed green
// under that mutation until this pin existed.
//
// A walker legitimately leaving this set is a deliberate change to what the
// guard covers, and should be made deliberately, here.
var mustRebuild = []string{"forker", "detacher"}

// checkRebuildingWalkers is the registry half, as a pure function over a
// registry, so a negative control can hand it a weakened one.  It returns
// one line per problem.
func checkRebuildingWalkers(memos []WalkerMemo) []string {
	var problems []string
	rebuilding := map[string]bool{}
	var reference *WalkerMemo
	for i := range memos {
		m := &memos[i]
		if !m.Rebuilds {
			continue
		}
		rebuilding[m.Walker] = true
		if reference == nil {
			reference = m
			continue
		}
		if got, want := kindSet(m.Payloads), kindSet(reference.Payloads); got != want {
			problems = append(problems, fmt.Sprintf(
				"walker %s memoises %s; walker %s memoises %s.\n"+
					"Every walker that rebuilds payload storage must memoise the same payload kinds:\n"+
					"a kind memoised in one and not the other is the issue #585 defect, where two headers\n"+
					"over one payload came apart in the copy while the other walker's guard stayed green.",
				m.Walker, got, reference.Walker, want))
		}
	}
	for _, name := range mustRebuild {
		if !rebuilding[name] {
			problems = append(problems, fmt.Sprintf(
				"walker %q does not declare Rebuilds, but it is one of the walkers this guard exists to\n"+
					"compare (mustRebuild). With fewer than two rebuilding walkers the cross-walker\n"+
					"comparison passes vacuously and the issue #585 class stops being guarded at all.\n"+
					"If the walker genuinely stopped rebuilding payload storage, remove it from mustRebuild\n"+
					"in the same commit, and say why.", name))
		}
	}
	if len(rebuilding) < 2 {
		problems = append(problems, fmt.Sprintf(
			"only %d walker(s) declare Rebuilds; the cross-walker comparison needs at least two to\n"+
				"compare anything. A registry with one rebuilding walker is a guard that cannot fail.",
			len(rebuilding)))
	}
	if reference == nil {
		problems = append(problems, "no walker declares Rebuilds; the registry has lost its subject")
	} else if len(reference.Payloads) == 0 {
		problems = append(problems, "the rebuilding walkers declare no payload memos at all")
	}
	return problems
}

// TestRebuildingWalkersMemoiseTheSamePayloadKinds is the registry half.
func TestRebuildingWalkersMemoiseTheSamePayloadKinds(t *testing.T) {
	for _, p := range checkRebuildingWalkers(WalkerMemos()) {
		t.Error(p)
	}
}

// TestRegistryHalfCannotBeDisabledByDroppingRebuilds is the negative
// control for the check above: the precise mutation the adversarial review
// of #599 found — set detacher's Rebuilds to false and drop a payload kind
// from it — must now be reported.  Before mustRebuild existed this mutation
// left all three halves of the drift guard green.
func TestRegistryHalfCannotBeDisabledByDroppingRebuilds(t *testing.T) {
	weakened := WalkerMemos()
	var found bool
	for i := range weakened {
		if weakened[i].Walker != "detacher" {
			continue
		}
		found = true
		weakened[i].Rebuilds = false
		var kept []PayloadKind
		for _, k := range weakened[i].Payloads {
			if k != PayloadBytes {
				kept = append(kept, k)
			}
		}
		weakened[i].Payloads = kept
	}
	if !found {
		t.Fatal("the registry has no detacher row; this control is no longer modelling anything")
	}
	problems := checkRebuildingWalkers(weakened)
	if len(problems) == 0 {
		t.Fatal("switching off the detacher's Rebuilds flag and dropping a payload kind from it was\n" +
			"NOT reported. The registry half of the drift guard can be silently disabled, which is\n" +
			"exactly what mustRebuild exists to prevent.")
	}
	var mentions bool
	for _, p := range problems {
		if strings.Contains(p, "detacher") {
			mentions = true
		}
	}
	if !mentions {
		t.Errorf("the weakened registry was reported, but no problem named the detacher:\n%s",
			strings.Join(problems, "\n"))
	}
	// And the mutation must be reported ONLY because it was applied: the
	// real registry stays clean, so a failure here is attributable.
	if p := checkRebuildingWalkers(WalkerMemos()); len(p) != 0 {
		t.Errorf("the real registry is not clean, so this control proves nothing: %s",
			strings.Join(p, "\n"))
	}
}

// TestWalkerRegistryMatchesTheSource is the source-scan half.
func TestWalkerRegistryMatchesTheSource(t *testing.T) {
	fields, err := memoShapedFields()
	if err != nil {
		t.Fatal(err)
	}
	if len(fields) == 0 {
		t.Fatal("the scan found no memo-shaped field in package lisp; it has stopped looking")
	}

	registered := map[string]map[string]PayloadKind{} // struct -> field -> kind
	for _, m := range walkerMemos {
		byField := map[string]PayloadKind{}
		for kind, field := range m.Fields {
			byField[field] = kind
		}
		registered[m.Walker] = byField
	}
	exempt := map[string]bool{}
	for _, e := range memoExemptions {
		exempt[e.Subject] = true
	}
	used := map[string]bool{}

	seen := map[string]map[string]bool{} // struct -> fields found in source
	for _, f := range fields {
		if seen[f.structName] == nil {
			seen[f.structName] = map[string]bool{}
		}
		seen[f.structName][f.fieldName] = true

		qualified := "lisp." + f.structName + "." + f.fieldName
		if exempt[qualified] {
			used[qualified] = true
			continue
		}
		byField, ok := registered[f.structName]
		if !ok {
			t.Errorf("%s: %s has a memo-shaped field %q (%s) but is not a registered walker.\n"+
				"A value-rebuilding walker must declare the payload kinds it memoises in walkerMemos\n"+
				"(lisp/walkers.go) so the registry half can compare it against the others.  If it is\n"+
				"not a walker, add a row to memoExemptions saying what it is.",
				f.pos, f.structName, f.fieldName, f.fieldType)
			continue
		}
		if _, ok := byField[f.fieldName]; !ok {
			t.Errorf("%s: walker %s has a memo-shaped field %q (%s) that walkerMemos does not name.\n"+
				"Add it to the walker's Fields map with the payload kind it memoises.",
				f.pos, f.structName, f.fieldName, f.fieldType)
		}
	}

	// Every registered field must still exist.  This is the half that
	// catches a memo DELETED from a walker: the registry still claims it,
	// the source no longer has it, and no test has to generate the aliased
	// value shape for the guard to go red.
	for _, m := range walkerMemos {
		for kind, field := range m.Fields {
			if !seen[m.Walker][field] {
				t.Errorf("walker %s declares a %s memo in field %q, but no such memo-shaped field exists in the source.\n"+
					"Either the memo was deleted — which reopens issues #576/#585 for that payload kind —\n"+
					"or the registry in lisp/walkers.go is stale.",
					m.Walker, kind, field)
			}
		}
	}

	// The exemption list may only shrink.
	for _, e := range memoExemptions {
		if strings.HasPrefix(e.Subject, "lisp.") && !used[e.Subject] {
			t.Errorf("memoExemptions has a row for %s, which the source scan no longer finds; delete it.\n"+
				"The list is shrink-only: a dead row hides the next real one.", e.Subject)
		}
		if strings.TrimSpace(e.Reason) == "" {
			t.Errorf("memoExemptions row %s carries no reason", e.Subject)
		}
	}
}

// TestEveryCopiedPayloadTypeIsMemoisedOrExempt is the payload-scan half.
func TestEveryCopiedPayloadTypeIsMemoisedOrExempt(t *testing.T) {
	// The payload kind each `case *T:` arm in a walker's v.Native type
	// switch corresponds to.  A new arm that is in neither table fails
	// below, which is the point: a payload the walker rebuilds is a payload
	// two headers can come apart over.
	kindOf := map[string]PayloadKind{
		"*[]byte":  PayloadBytes,
		"*MapData": PayloadSortedMap,
	}
	exempt := map[string]bool{}
	for _, e := range memoExemptions {
		exempt[e.Subject] = true
	}
	used := map[string]bool{}

	for _, file := range []string{"fork.go", "detach.go"} {
		types, err := nativeSwitchCaseTypes(file)
		if err != nil {
			t.Fatal(err)
		}
		if len(types) == 0 {
			t.Errorf("%s: no v.Native type switch found; the scan has stopped looking", file)
			continue
		}
		for _, typ := range types {
			if kind, ok := kindOf[typ]; ok {
				if !memoisedByEveryRebuildingWalker(kind) {
					t.Errorf("%s: payload type %s is rebuilt but is not memoised by every rebuilding walker",
						file, typ)
				}
				continue
			}
			if exempt[typ] {
				used[typ] = true
				continue
			}
			t.Errorf("%s: the walker copies payload type %s, which is neither a registered memo kind\n"+
				"nor an exempted one.  A rebuilt payload that is not memoised per payload is rebuilt\n"+
				"once per HEADER, so two names for it come apart in the copy — issues #576 and #585.\n"+
				"Memoise it in both rebuilding walkers, or add a row to memoExemptions in\n"+
				"lisp/walkers.go stating why it cannot be aliased across two headers.",
				file, typ)
		}
	}
	for _, e := range memoExemptions {
		if !strings.HasPrefix(e.Subject, "lisp.") && !used[e.Subject] {
			t.Errorf("memoExemptions has a row for payload type %s, which no walker copies any more; delete it.", e.Subject)
		}
	}
}

func memoisedByEveryRebuildingWalker(kind PayloadKind) bool {
	for _, m := range walkerMemos {
		if !m.Rebuilds {
			continue
		}
		found := false
		for _, k := range m.Payloads {
			if k == kind {
				found = true
			}
		}
		if !found {
			return false
		}
	}
	return true
}

func kindSet(kinds []PayloadKind) string {
	out := make([]string, len(kinds))
	for i, k := range kinds {
		out[i] = string(k)
	}
	sort.Strings(out)
	return "{" + strings.Join(out, ", ") + "}"
}

// memoField is one memo-shaped struct field found in the source.
type memoField struct {
	structName string
	fieldName  string
	fieldType  string
	pos        string
}

// memoShapedFields reads every .go file in this package as source and
// returns the struct fields whose type is a map keyed by a pointer or by an
// empty interface — the shape a copy walker's memo table has.  Test files
// are skipped: a memo in a test copies nothing that ships.
func memoShapedFields() ([]memoField, error) {
	names, err := filepath.Glob("*.go")
	if err != nil {
		return nil, err
	}
	sort.Strings(names)
	fset := token.NewFileSet()
	var out []memoField
	for _, name := range names {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		src, err := os.ReadFile(name) //nolint:gosec // a fixed glob over this package's own directory
		if err != nil {
			return nil, err
		}
		// Parse with no build-constraint filtering, so files behind
		// `-tags elpscheck` are scanned in an ordinary build too.  A memo
		// invisible to the default build is exactly the blind spot the
		// second golangci-lint pass exists for.
		f, err := parser.ParseFile(fset, name, src, parser.SkipObjectResolution)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", name, err)
		}
		ast.Inspect(f, func(n ast.Node) bool {
			ts, ok := n.(*ast.TypeSpec)
			if !ok {
				return true
			}
			st, ok := ts.Type.(*ast.StructType)
			if !ok || st.Fields == nil {
				return true
			}
			for _, field := range st.Fields.List {
				mt, ok := field.Type.(*ast.MapType)
				if !ok || !isMemoKey(mt.Key) {
					continue
				}
				for _, id := range field.Names {
					out = append(out, memoField{
						structName: ts.Name.Name,
						fieldName:  id.Name,
						fieldType:  render(fset, field.Type),
						pos:        fset.Position(id.Pos()).String(),
					})
				}
			}
			return true
		})
	}
	return out, nil
}

// isMemoKey reports whether a map key type is the shape a memo table uses:
// a pointer (payload identity) or an empty interface (an opaque native
// payload, keyed by Go ==).
func isMemoKey(e ast.Expr) bool {
	switch k := e.(type) {
	case *ast.StarExpr:
		return true
	case *ast.InterfaceType:
		return k.Methods == nil || len(k.Methods.List) == 0
	case *ast.Ident:
		return k.Name == "any"
	}
	return false
}

// nativeSwitchCaseTypes returns the concrete case types of every
// `switch x := v.Native.(type)` in the named file: the payload types the
// walker knows how to rebuild.
func nativeSwitchCaseTypes(name string) ([]string, error) {
	fset := token.NewFileSet()
	src, err := os.ReadFile(name) //nolint:gosec // a fixed file name in this package's own directory
	if err != nil {
		return nil, err
	}
	f, err := parser.ParseFile(fset, name, src, parser.SkipObjectResolution)
	if err != nil {
		return nil, err
	}
	var out []string
	ast.Inspect(f, func(n ast.Node) bool {
		sw, ok := n.(*ast.TypeSwitchStmt)
		if !ok || !isNativeTypeSwitch(fset, sw) {
			return true
		}
		for _, stmt := range sw.Body.List {
			cc, ok := stmt.(*ast.CaseClause)
			if !ok {
				continue
			}
			for _, e := range cc.List {
				if id, ok := e.(*ast.Ident); ok && id.Name == "nil" {
					continue
				}
				out = append(out, render(fset, e))
			}
		}
		return true
	})
	sort.Strings(out)
	return out, nil
}

func isNativeTypeSwitch(fset *token.FileSet, sw *ast.TypeSwitchStmt) bool {
	return strings.Contains(render(fset, sw.Assign), ".Native.(type)")
}

func render(fset *token.FileSet, n ast.Node) string {
	var b bytes.Buffer
	if err := printer.Fprint(&b, fset, n); err != nil {
		return "<unrenderable>"
	}
	return b.String()
}
