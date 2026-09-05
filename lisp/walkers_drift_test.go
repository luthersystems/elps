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
func checkRebuildingWalkers(memos []WalkerMemo) (problems, known []string) {
	rebuilding := map[string]bool{}
	clean := 0
	var reference *WalkerMemo
	for i := range memos {
		m := &memos[i]
		if !m.Rebuilds {
			continue
		}
		rebuilding[m.Walker] = true
		// A KNOWN-DEFECTIVE walker never becomes the reference: it is the
		// thing being compared against a correct walker, and letting a
		// walker with no memos define the standard would report every
		// correct walker as wrong.
		if IsKnownDefective(m.Walker) {
			known = append(known, describeDefects(m.Walker))
			continue
		}
		clean++
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
	// A walker with an allowlist row must still BE defective by the
	// registry's own rule, or the row is dead and the allowlist has stopped
	// shrinking. Checked against the reference the same way a clean walker
	// would be.
	for i := range memos {
		m := &memos[i]
		if !m.Rebuilds || !IsKnownDefective(m.Walker) || reference == nil {
			continue
		}
		if kindSet(m.Payloads) == kindSet(reference.Payloads) {
			problems = append(problems, fmt.Sprintf(
				"walker %s has a knownDefectiveWalkers row but now memoises the same payload kinds as\n"+
					"%s, so the registry no longer considers it defective. If the defect is fixed, DELETE\n"+
					"its rows from knownDefectiveWalkers -- the allowlist is shrink-only and a row that\n"+
					"outlives its defect is how a fixed bug gets recorded as still open.",
				m.Walker, reference.Walker))
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
	// Counted over CLEAN walkers only: a known-defective walker is the
	// subject of the comparison, not a participant in it, so admitting one
	// here would let the allowlist restore a vacuous guard.
	if clean < 2 {
		problems = append(problems, fmt.Sprintf(
			"only %d non-defective walker(s) declare Rebuilds; the cross-walker comparison needs at\n"+
				"least two to compare anything. A registry with one rebuilding walker is a guard that\n"+
				"cannot fail.",
			clean))
	}
	if reference == nil {
		problems = append(problems, "no walker declares Rebuilds; the registry has lost its subject")
	} else if len(reference.Payloads) == 0 {
		problems = append(problems, "the rebuilding walkers declare no payload memos at all")
	}
	return problems, known
}

// describeDefects renders a walker's open defect rows for the KNOWN report.
func describeDefects(walker string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "KNOWN-DEFECTIVE walker %s (knownDefectiveWalkers, lisp/walkers.go):", walker)
	for _, d := range WalkerDefects() {
		if d.Walker != walker {
			continue
		}
		fmt.Fprintf(&b, "\n  %s: %s\n    pinned by %s", d.Payload, d.Defect, d.Pin)
	}
	return b.String()
}

// TestRebuildingWalkersMemoiseTheSamePayloadKinds is the registry half.
//
// A walker on the known-defect allowlist is REPORTED, not passed over: the
// log below is the only place a reader learns that a registered walker is
// tolerated rather than clean.
func TestRebuildingWalkersMemoiseTheSamePayloadKinds(t *testing.T) {
	problems, known := checkRebuildingWalkers(WalkerMemos())
	for _, p := range problems {
		t.Error(p)
	}
	for _, k := range known {
		t.Log(k)
	}
	// Every allowlist row must name a REGISTERED walker. A row for a walker
	// nobody registers is a defect record nothing checks.
	registered := map[string]bool{}
	for _, m := range WalkerMemos() {
		registered[m.Walker] = true
	}
	for _, d := range WalkerDefects() {
		if !registered[d.Walker] {
			t.Errorf("knownDefectiveWalkers has a row for %q (%s), which is not a registered walker.\n"+
				"Register it or delete the row: an allowlist entry for an unregistered walker exempts\n"+
				"nothing and records a defect no check can see.", d.Walker, d.Payload)
		}
	}
	if len(known) == 0 && len(WalkerDefects()) != 0 {
		t.Error("knownDefectiveWalkers is non-empty but no walker was reported as defective, so the\n" +
			"allowlist is no longer connected to the check it is supposed to soften.")
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
	problems, _ := checkRebuildingWalkers(weakened)
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
	if p, _ := checkRebuildingWalkers(WalkerMemos()); len(p) != 0 {
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
		// A KNOWN-DEFECTIVE walker is excluded here for the same reason it
		// never becomes the reference above: it is the subject of the
		// comparison. Including it would report fork.go and detach.go --
		// which memoise both payload kinds correctly -- as the defective
		// ones, pointing every reader at the wrong file. Delete the
		// walker's allowlist row and this exclusion stops applying, which
		// is the weakening that proves the exclusion is not a hiding place.
		if IsKnownDefective(m.Walker) {
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

// TestWalkerMemosCannotBeEditedByACaller is the control for WalkerMemos's
// deep copy.  The function's doc promises a caller cannot edit the
// registry; before the copy was made deep that promise was false — the
// returned structs shared Fields, Payloads and Graph with the package
// state, so a caller building a weakened variant (exactly what
// TestRegistryHalfCannotBeDisabledByDroppingRebuilds does) silently
// rewrote what every later caller read.
//
// Reverting the deep copy to a shallow one must fail here.
func TestWalkerMemosCannotBeEditedByACaller(t *testing.T) {
	// Snapshot the expected state as IMMUTABLE STRINGS before scribbling.
	// Holding a []WalkerMemo as the "before" is not good enough: under a
	// shallow copy that snapshot aliases the same backing arrays as the
	// scribbled copy, so it is corrupted too and the comparison passes.
	// That is how the first version of this test stayed green under the
	// very weakening it exists to catch.
	type snap struct{ walker, payloads, graph, fields string }
	take := func() []snap {
		var out []snap
		for _, m := range WalkerMemos() {
			fields := make([]string, 0, len(m.Fields))
			for k, v := range m.Fields {
				fields = append(fields, fmt.Sprintf("%s=%s", k, v))
			}
			sort.Strings(fields)
			out = append(out, snap{
				walker:   m.Walker,
				payloads: kindSet(m.Payloads),
				graph:    kindSet(m.Graph),
				fields:   strings.Join(fields, ","),
			})
		}
		return out
	}
	before := take()
	if len(before) == 0 {
		t.Fatal("the registry is empty")
	}

	// Edit every SHARED part of a returned copy, IN PLACE.  Reassigning a
	// slice field writes only to the caller's own struct and cannot reach
	// package state at any copy depth; writing THROUGH the slice is what a
	// shallow copy shares.
	scribbled := WalkerMemos()
	for i := range scribbled {
		for j := range scribbled[i].Payloads {
			scribbled[i].Payloads[j] = PayloadValue
		}
		for j := range scribbled[i].Graph {
			scribbled[i].Graph[j] = PayloadValue
		}
		for k := range scribbled[i].Fields {
			scribbled[i].Fields[k] = "scribbled"
		}
	}

	after := take()
	if len(after) != len(before) {
		t.Fatalf("the registry changed length: %d then %d", len(before), len(after))
	}
	for i := range after {
		if after[i] != before[i] {
			t.Errorf("walker %s: a caller's in-place edit reached the registry.\n"+
				"  payloads: %s, was %s\n  graph:    %s, was %s\n  fields:   %s, was %s\n"+
				"WalkerMemos returns a SHALLOW copy again, so its doc comment is false and any caller\n"+
				"that edits a returned row corrupts what every later caller reads.",
				after[i].walker,
				after[i].payloads, before[i].payloads,
				after[i].graph, before[i].graph,
				after[i].fields, before[i].fields)
		}
	}
}

// TestCopyAliasesCallStackAcrossHeaders backs the *CallStack exemption's
// stated reason.
//
// That row used to say "no constructor can alias one across two headers:
// SetCallStack is called once". This test is what falsifies it: (*LVal).Copy
// is `*cp = *v`, a shallow copy that carries Native, so two headers end up
// over one *CallStack. The exemption's CONCLUSION is still right -- a captured
// stack's identity carries no observable state, because CallStack.Copy
// allocates an exact-length Frames slice at every capture site and the only
// mutators (PushFID, Pop) run on env.Runtime.Stack rather than on a capture --
// but it is right for a different reason than the row claimed.
//
// If Copy ever stops aliasing (a deep copy, or dropping Native), this fails
// and the row's history paragraph should be revisited rather than left
// asserting something that is no longer so.
func TestCopyAliasesCallStackAcrossHeaders(t *testing.T) {
	t.Parallel()
	cs := &CallStack{Frames: []CallFrame{{FID: "f"}}}
	a := Error(nil)
	a.SetCallStack(cs)
	b := a.Copy()
	if a.CallStack() != b.CallStack() {
		t.Fatal("(*LVal).Copy no longer aliases the *CallStack across two headers.\n" +
			"The *CallStack exemption row in walkers.go records that it DOES, as the correction to a\n" +
			"false reason. If Copy changed, update that row rather than leaving it describing the old\n" +
			"behaviour -- which is exactly the failure the row's own history paragraph is about.")
	}
}

// TestCopyDeAliasesMapPayloadAcrossHeaders is the negative control for the
// copier's *MapData memo (lisp/copier.go).  Its name is the one it had as
// the pin of (*LVal).Copy's knownDefectiveWalkers row, when it asserted
// the DEFECT: two headers over one sorted map -- the shape
// `(quasiquote (unquote m))` produces, and the shape issues #576 and #585
// are about -- came out of Copy as two maps, because copyMapData ran once
// per header and nothing memoised it.  The copier memoises the *MapData
// per payload, the row was deleted, and this test now asserts the fix: the
// two copied headers share one map, and a write through one is seen
// through the other, exactly as in the source.  Going red here means the
// memo has been lost and the #576/#585 defect is back in Copy.
func TestCopyDeAliasesMapPayloadAcrossHeaders(t *testing.T) {
	t.Parallel()
	m := SortedMap()
	if rc := m.MapSet(String("k"), Int(1)); rc.Type == LError {
		t.Fatal(rc)
	}
	alias := &LVal{}
	*alias = *m // a second header over the same *MapData, as quasiquote makes
	if m.Native != alias.Native {
		t.Fatal("the fixture did not build two headers over one payload; this test proves nothing")
	}
	probe := QExpr([]*LVal{m, alias})

	cp := probe.Copy()
	if cp.Type == LError {
		t.Fatal(cp)
	}
	if len(cp.Cells) != 2 {
		t.Fatalf("copy has %d cells, want 2", len(cp.Cells))
	}
	if cp.Cells[0].Native != cp.Cells[1].Native {
		t.Fatal("(*LVal).Copy rebuilt ONE sorted map as TWO across its two headers.\n" +
			"That is the issue #576 / #585 defect back in Copy: the copier's *MapData memo\n" +
			"(lisp/copier.go, copier.mapData) is no longer keeping two headers over one payload\n" +
			"together. This test was the pin of Copy's knownDefectiveWalkers row and became its\n" +
			"negative control when the memo landed; do not re-add the row, restore the memo.")
	}
	if cp.Cells[0].Native == m.Native {
		t.Fatal("the copy shares the source's *MapData; the memo is returning the original instead of a copy")
	}
	// And the aliasing is OBSERVABLE, not just a pointer equality: a write
	// through one name in the copy is seen through the other, as in the
	// source, and not in the source.
	if rc := cp.Cells[0].MapSet(String("k"), Int(2)); rc.Type == LError {
		t.Fatal(rc)
	}
	got, _ := cp.Cells[1].Map().Get(String("k"))
	if got == nil || got.Type == LError {
		t.Fatalf("second header lost the key entirely: %v", got)
	}
	if got.Int != 2 {
		t.Fatal("a write through one copied header is not seen through the other; the pointer check above\n" +
			"is measuring something other than one shared map.")
	}
	if orig, _ := m.Map().Get(String("k")); orig == nil || orig.Int != 1 {
		t.Fatalf("the write through the copy reached the source (%v)", orig)
	}
}

// TestCopySharesBytesPayloadAcrossHeaders is the negative control for the
// copier's *[]byte memo, under the name it had as the pin of (*LVal).Copy's
// second knownDefectiveWalkers row (issue #551).  Then it asserted the
// DEFECT: LBytes fell to Copy's default arm, which rebuilt Cells and
// nothing else, so the *[]byte rode across in `*cp = *v` and copy and
// source appended into one backing array.  The copier rebuilds the buffer
// once per payload; this test now asserts that a copy owns its bytes, and
// that two headers over one buffer come out as two headers over ONE copied
// buffer.  Going red here means the copy shares or splits the buffer again.
func TestCopySharesBytesPayloadAcrossHeaders(t *testing.T) {
	t.Parallel()
	src := Bytes([]byte("ab"))
	cp := src.Copy()
	if cp.Type == LError {
		t.Fatal(cp)
	}
	if cp.Native == src.Native {
		t.Fatal("(*LVal).Copy shares the *[]byte with its source again: issue #551 is back.\n" +
			"The copier's *[]byte memo (lisp/copier.go, copier.byteSlice) rebuilds the buffer once per\n" +
			"payload; this test was the pin of Copy's knownDefectiveWalkers row and became its negative\n" +
			"control when the memo landed. Do not re-add the row, restore the copy.")
	}
	// Observable, not merely pointer-inequal: a write through the copy is
	// NOT seen through the source.
	bs := cp.Native.(*[]byte)
	(*bs)[0] = 'z'
	if got := string(*src.Native.(*[]byte)); got != "ab" {
		t.Fatalf("a write through the copy reached the source (%q); the buffer is still shared.", got)
	}
	// Two headers over one buffer are two headers over ONE copied buffer:
	// the memo, not merely a copy per header.
	alias := &LVal{}
	*alias = *src // a second header over the same *[]byte, as quasiquote makes
	pair := QExpr([]*LVal{src, alias}).Copy()
	if pair.Cells[0].Native != pair.Cells[1].Native {
		t.Fatal("two headers over one *[]byte were copied into two buffers; the memo is gone and a\n" +
			"write through one copied name is invisible through the other.")
	}
}
