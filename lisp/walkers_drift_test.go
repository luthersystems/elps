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

// The memo drift guard, all three halves.
//
// Issue #585 is the whole argument for this file.  One walker's per-payload
// memos were added for issue #576 and guarded by a harness that only ever
// drove that walker.  The detach/copy walker had the identical defect,
// nothing compared the two, and it stayed live for the whole time the first
// guard was green.  A comparison between the walkers is what would have
// caught it, and it has to be a comparison a machine makes.
//
//   - The REGISTRY half asserts that every walker which rebuilds payload
//     storage memoises the same set of cross-walker payload kinds.  A kind
//     added to one walker and not the other fails here.
//   - The SOURCE SCAN half reads this package's own source and asserts that
//     every memo-shaped struct field — a map keyed by a pointer type —
//     belongs to a registered walker, and that every field the registry
//     names still exists.  The registry alone cannot catch a BRAND NEW
//     walker nobody told it about, and cannot catch a memo DELETED from a
//     registered one; the scan catches both, deterministically, without any
//     test having to generate the value shape the memo protects.
//   - The PAYLOAD SCAN half reads the walkers' `v.Native` type switches and
//     asserts that every payload type they copy is either a registered memo
//     kind or an exempted one.  That is issue #585 stated exactly: a
//     payload the walker rebuilds but does not memoise is a payload two
//     headers come apart over.

// The payload scan's file set is DERIVED, not written down (the idea comes
// from PR #616).  A hardcoded list of files is only as good as the day it
// was written: a walker that grows a `v.Native` type switch in a new file
// silently falls outside a list, and that blind spot is how a payload arm
// ships unexamined.  The property that makes a site payload handling at all
// is not which file it sits in — it is that the function ASSIGNS to a
// header's Native field, which is where a payload is rebuilt or re-pointed
// and therefore where two names for one payload are born.  So
// nativeWritingFiles below selects the files by that property, and adding a
// walker adds nothing here.
//
// What stays out of scope, deliberately: a payload named by a type
// ASSERTION rather than a case arm (the template compiler's
// `v.Native.(*funData)`, detach's NativeCloner arm).  #616 collects those
// too; doing it here would need a payload-kind or exemption design for each
// spelling first, and that is a change to the rules rather than to the
// scan's reach.

// mustRebuild names the walkers that MUST declare Rebuilds.  Without this
// pin the registry half is disableable in one edit: set the detacher's
// Rebuilds to false and the cross-walker comparison below has nothing left
// to compare, so it passes vacuously while the exact drift it exists to
// catch — issue #585, a payload kind memoised in one walker and not in the
// other — goes unreported.  All three halves of the guard stayed green
// under that mutation until this pin existed.
//
// A walker legitimately leaving this set is a deliberate change to what the
// guard covers, and should be made deliberately, here.
var mustRebuild = []string{"detacher", "templateCompiler", "copier"}

// checkRebuildingWalkers is the registry half, as a pure function over a
// registry, so a negative control can hand it a weakened one.  It returns
// one line per problem.
func checkRebuildingWalkers(memos []walkerMemo) (problems, notes []string) {
	rebuilding := map[string]bool{}
	clean := 0
	var reference *walkerMemo
	for i := range memos {
		m := &memos[i]
		if !m.Rebuilds {
			continue
		}
		rebuilding[m.Walker] = true
		// A walker that declares no memos at all never becomes the
		// reference: it is the thing being described, and letting it define
		// the standard would report every memoising walker as wrong.
		if isUnmemoisedWalker(m.Walker) {
			notes = append(notes, describeNote(m.Walker))
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
					"Every walker that rebuilds payload storage must memoise the same cross-walker payload\n"+
					"kinds: a kind memoised in one and not the other is the issue #585 defect, where two\n"+
					"headers over one payload came apart in the copy while the other walker's guard stayed\n"+
					"green. A memo only one walker can have belongs in that walker's Local set, not here.",
				m.Walker, got, reference.Walker, want))
		}
	}
	// A walker with an unmemoisedWalkers row must still declare no memo
	// fields, or the row is dead and the list has stopped shrinking.
	for i := range memos {
		m := &memos[i]
		if !isUnmemoisedWalker(m.Walker) {
			continue
		}
		if len(m.Fields) != 0 || len(m.Payloads) != 0 {
			problems = append(problems, fmt.Sprintf(
				"walker %s has an unmemoisedWalkers row but now declares memo fields %v.\n"+
					"DELETE the row: the list is shrink-only, it exists only to say the registry check has\n"+
					"nothing to compare this walker against, and a row that outlives that state describes\n"+
					"code that no longer exists.", m.Walker, m.Fields))
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
	// Counted over MEMOISING walkers only: a walker with an
	// unmemoisedWalkers row is the subject of the comparison, not a
	// participant in it, so admitting one here would let that list restore
	// a vacuous guard.
	if clean < 2 {
		problems = append(problems, fmt.Sprintf(
			"only %d memoising walker(s) declare Rebuilds; the cross-walker comparison needs at\n"+
				"least two to compare anything. A registry with one rebuilding walker is a guard that\n"+
				"cannot fail.",
			clean))
	}
	if reference == nil {
		problems = append(problems, "no walker declares Rebuilds; the registry has lost its subject")
	} else if len(reference.Payloads) == 0 {
		problems = append(problems, "the rebuilding walkers declare no payload memos at all")
	}
	return problems, notes
}

// describeNote renders a walker's unmemoisedWalkers row for the report.
func describeNote(walker string) string {
	var b strings.Builder
	fmt.Fprintf(&b, "UNMEMOISED walker %s (unmemoisedWalkers, lisp/walkers.go):", walker)
	for _, n := range registeredWalkerNotes() {
		if n.Walker != walker {
			continue
		}
		fmt.Fprintf(&b, "\n  %s", n.Note)
	}
	return b.String()
}

// TestRebuildingWalkersMemoiseTheSamePayloadKinds is the registry half.
//
// A walker on the unmemoised list is REPORTED, not passed over: the log
// below is the only place a reader learns that a registered walker was
// described rather than compared.
func TestRebuildingWalkersMemoiseTheSamePayloadKinds(t *testing.T) {
	problems, notes := checkRebuildingWalkers(registeredWalkerMemos())
	for _, p := range problems {
		t.Error(p)
	}
	for _, n := range notes {
		t.Log(n)
	}
	// Every note must name a REGISTERED walker. A row for a walker nobody
	// registers describes nothing any check can see.
	registered := map[string]bool{}
	for _, m := range registeredWalkerMemos() {
		registered[m.Walker] = true
	}
	for _, n := range registeredWalkerNotes() {
		if !registered[n.Walker] {
			t.Errorf("unmemoisedWalkers has a row for %q, which is not a registered walker.\n"+
				"Register it or delete the row: a row for an unregistered walker exempts nothing and\n"+
				"describes a walker no check can see.", n.Walker)
		}
	}
	if len(notes) == 0 && len(registeredWalkerNotes()) != 0 {
		t.Error("unmemoisedWalkers is non-empty but no walker was reported, so the list is no longer\n" +
			"connected to the check it is supposed to soften.")
	}
}

// TestRegistryHalfCannotBeDisabledByDroppingRebuilds is the negative
// control for the check above: the precise mutation the adversarial review
// of #599 found — set a walker's Rebuilds to false and drop a payload kind
// from it — must now be reported.  Before mustRebuild existed this mutation
// left all three halves of the drift guard green.
func TestRegistryHalfCannotBeDisabledByDroppingRebuilds(t *testing.T) {
	weakened := registeredWalkerMemos()
	var found bool
	for i := range weakened {
		if weakened[i].Walker != "detacher" {
			continue
		}
		found = true
		weakened[i].Rebuilds = false
		var kept []payloadKind
		for _, k := range weakened[i].Payloads {
			if k != payloadBytes {
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
	if p, _ := checkRebuildingWalkers(registeredWalkerMemos()); len(p) != 0 {
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

	registered := map[string]map[string]payloadKind{} // struct -> field -> kind
	for _, m := range walkerMemos {
		byField := map[string]payloadKind{}
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
	kindOf := map[string]payloadKind{
		"*[]byte":  payloadBytes,
		"*MapData": payloadSortedMap,
	}
	exempt := map[string]bool{}
	for _, e := range memoExemptions {
		exempt[e.Subject] = true
	}
	used := map[string]bool{}

	files, err := nativeWritingFiles()
	if err != nil {
		t.Fatal(err)
	}
	scanned := 0
	for _, file := range files {
		types, err := nativeSwitchCaseTypes(file)
		if err != nil {
			t.Fatal(err)
		}
		scanned += len(types)
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
				"Memoise it in every rebuilding walker, or add a row to memoExemptions in\n"+
				"lisp/walkers.go stating why it cannot be aliased across two headers.",
				file, typ)
		}
	}
	if scanned == 0 {
		t.Fatalf("no v.Native type switch found in any of the %d Native-writing files (%s);\n"+
			"the payload scan has stopped looking.", len(files), strings.Join(files, ", "))
	}
	for _, e := range memoExemptions {
		if !strings.HasPrefix(e.Subject, "lisp.") && !used[e.Subject] {
			t.Errorf("memoExemptions has a row for payload type %s, which no walker copies any more; delete it.", e.Subject)
		}
	}
}

// nativeWritingFiles returns this package's production files holding a
// function that ASSIGNS to a header's Native field.  That is the property
// the payload scan keys on instead of a file list: a walker that rebuilds
// or re-points payload storage writes the field, and a file that only
// READS a payload (error.go's message rendering) cannot put two names on
// one payload and is not scanned.
//
// A composite literal is deliberately not a write: `&LVal{Native: x}` MINTS
// a payload onto a header that had none, while an assignment moves one onto
// a header copied from somewhere else.
func nativeWritingFiles() ([]string, error) {
	names, err := filepath.Glob("*.go")
	if err != nil {
		return nil, err
	}
	sort.Strings(names)
	fset := token.NewFileSet()
	var out []string
	for _, name := range names {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		src, err := os.ReadFile(name) //nolint:gosec // a fixed glob over this package's own directory
		if err != nil {
			return nil, err
		}
		f, err := parser.ParseFile(fset, name, src, parser.SkipObjectResolution)
		if err != nil {
			return nil, fmt.Errorf("%s: %w", name, err)
		}
		if writesNativeField(f) {
			out = append(out, name)
		}
	}
	return out, nil
}

// writesNativeField reports whether the tree assigns to a `.Native` field.
func writesNativeField(n ast.Node) bool {
	found := false
	ast.Inspect(n, func(n ast.Node) bool {
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

// TestThePayloadScanReachesEveryRebuildingWalker is the canary for the
// property above.  The two rebuilding walkers in the registry live in
// detach.go and template_plan.go, and both write a Native field, so both
// must be selected; if the matcher stops recognising the write, the payload
// scan quietly covers nothing and this is what says so.
func TestThePayloadScanReachesEveryRebuildingWalker(t *testing.T) {
	files, err := nativeWritingFiles()
	if err != nil {
		t.Fatal(err)
	}
	selected := map[string]bool{}
	for _, f := range files {
		selected[f] = true
	}
	for _, want := range []string{"detach.go", "template_plan.go"} {
		if !selected[want] {
			t.Errorf("%s holds a walker with Rebuilds: true but was not selected by the payload scan.\n"+
				"Either the walker stopped writing a Native field -- in which case its registry row in\n"+
				"lisp/walkers.go is wrong -- or writesNativeField has stopped recognising the write and\n"+
				"the scan now covers whatever happens to be left. Selected: %v", want, files)
		}
	}
	t.Logf("payload scan reads %d Native-writing files: %s", len(files), strings.Join(files, ", "))
}

func memoisedByEveryRebuildingWalker(kind payloadKind) bool {
	for _, m := range walkerMemos {
		if !m.Rebuilds {
			continue
		}
		// A walker that declares no memos is excluded here for the same
		// reason it never becomes the reference above: it is the subject of
		// the comparison. Including it would report detach.go and the
		// template compiler -- which memoise both payload kinds correctly --
		// as the offenders, pointing every reader at the wrong file. Delete
		// the walker's unmemoisedWalkers row and this exclusion stops
		// applying, which is the weakening that proves the exclusion is not
		// a hiding place.
		if isUnmemoisedWalker(m.Walker) {
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

func kindSet(kinds []payloadKind) string {
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

// TestWalkerMemosCannotBeEditedByACaller is the control for registeredWalkerMemos's
// deep copy.  The function's doc promises a caller cannot edit the
// registry; before the copy was made deep that promise was false — the
// returned structs shared Fields, Payloads, Local and Graph with the
// package state, so a caller building a weakened variant (exactly what
// TestRegistryHalfCannotBeDisabledByDroppingRebuilds does) silently
// rewrote what every later caller read.
//
// Reverting the deep copy to a shallow one must fail here.
func TestWalkerMemosCannotBeEditedByACaller(t *testing.T) {
	// Snapshot the expected state as IMMUTABLE STRINGS before scribbling.
	// Holding a []walkerMemo as the "before" is not good enough: under a
	// shallow copy that snapshot aliases the same backing arrays as the
	// scribbled copy, so it is corrupted too and the comparison passes.
	// That is how the first version of this test stayed green under the
	// very weakening it exists to catch.
	type snap struct{ walker, payloads, local, graph, fields string }
	take := func() []snap {
		var out []snap
		for _, m := range registeredWalkerMemos() {
			fields := make([]string, 0, len(m.Fields))
			for k, v := range m.Fields {
				fields = append(fields, fmt.Sprintf("%s=%s", k, v))
			}
			sort.Strings(fields)
			out = append(out, snap{
				walker:   m.Walker,
				payloads: kindSet(m.Payloads),
				local:    kindSet(m.Local),
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
	scribbled := registeredWalkerMemos()
	for i := range scribbled {
		for j := range scribbled[i].Payloads {
			scribbled[i].Payloads[j] = payloadValue
		}
		for j := range scribbled[i].Local {
			scribbled[i].Local[j] = payloadValue
		}
		for j := range scribbled[i].Graph {
			scribbled[i].Graph[j] = payloadValue
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
				"  payloads: %s, was %s\n  local:    %s, was %s\n  graph:    %s, was %s\n  fields:   %s, was %s\n"+
				"registeredWalkerMemos returns a SHALLOW copy again, so its doc comment is false and any caller\n"+
				"that edits a returned row corrupts what every later caller reads.",
				after[i].walker,
				after[i].payloads, before[i].payloads,
				after[i].local, before[i].local,
				after[i].graph, before[i].graph,
				after[i].fields, before[i].fields)
		}
	}
}
