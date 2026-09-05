// Copyright © 2026 The ELPS authors

// Coverage for the macro-expansion metadata channel, and for the walker
// arm that had never executed.
//
// Two gaps from the #600 audit, and they land together because the first
// is a prerequisite for the second. Macro-expansion metadata exists only
// when a Debugger is ATTACHED (dormant is enough -- lisp/env.go gates the
// context on `Runtime.Debugger != nil`, not on IsEnabled), and the guard's
// environments never attached one. So the stamp walker's metadata branch
// never ran, and the field Fork drops had nothing to drop.

package elpstest

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/walkraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// dormantDebugger is attached but never enabled: every hook is inert and
// IsEnabled reports false, so the interpreter skips the debug paths --
// except the one that matters here, which keys on ATTACHMENT.
type dormantDebugger struct{}

func (dormantDebugger) IsEnabled() bool                                { return false }
func (dormantDebugger) OnEval(*lisp.LEnv, *lisp.LVal) bool             { return false }
func (dormantDebugger) OnFunEntry(*lisp.LEnv, *lisp.LVal, *lisp.LEnv)  {}
func (dormantDebugger) OnFunReturn(*lisp.LEnv, *lisp.LVal, *lisp.LVal) {}
func (dormantDebugger) AfterFunCall(*lisp.LEnv) bool                   { return false }
func (dormantDebugger) OnError(*lisp.LEnv, *lisp.LVal) bool            { return false }
func (dormantDebugger) WaitIfPaused(*lisp.LEnv, *lisp.LVal) lisp.DebugAction {
	return lisp.DebugContinue
}

func debuggedEnv() (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Debugger = dormantDebugger{}
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, lisp.GoError(rc)
	}
	return env, nil
}

// runtimeBuiltExpansion retains a macro expansion whose recorded call-site
// arguments are MUTABLE and template-owned.
//
// Every part is load-bearing, and each was measured rather than guessed:
//
//   - the call form is built at RUNTIME with `list`, so its nodes are not
//     sealed parse-tree nodes. A quoted literal call (`'(defun f (x) x)`)
//     records 12 args, all 12 sealed, and sealed args are not a leak.
//     This shape records 15, of which 5 are unsealed.
//   - `defun` is a BUILTIN macro, so its expansion contains synthetic
//     nodes, and only synthetic nodes are stamped.
//   - `macroexpand-1` RETAINS the stamped tree in a binding. An ordinary
//     macro call evaluates its expansion and discards it, so nothing
//     reachable carries metadata at all -- which is why an env built from
//     ordinary lisp cannot exercise this channel.
const runtimeBuiltExpansion = `
(set 'form (list 'defun 'leaky (list 'x) 'x))
(set 'expansion (macroexpand-1 form))
(set 'probe (list form expansion))
`

func TestForkDoesNotLeakMacroExpansionMetadata(t *testing.T) {
	t.Parallel()
	env, err := debuggedEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", runtimeBuiltExpansion); rc.Type == lisp.LError {
		t.Fatal(rc)
	}

	// The fixture must actually carry what the property is about, or the
	// check below passes by having nothing to look at. This is the
	// anti-vacuity assertion, and it is the one that fails first if macro
	// stamping, the debugger gate, or `macroexpand-1`'s retention change.
	meta, unsealed := 0, 0
	for v := range reachableValues(env) {
		m, ok := v.MacroExpansion()
		if !ok {
			continue
		}
		meta++
		for _, a := range m.Args {
			if a != nil && !a.IsSealed() {
				unsealed++
			}
		}
	}
	if meta == 0 {
		t.Fatal("no value in the template carries macro-expansion metadata, so this test is checking\n" +
			"nothing. Metadata needs a Debugger ATTACHED (dormant is enough), a BUILTIN macro (only\n" +
			"synthetic nodes are stamped), and macroexpand-1 to RETAIN the stamped tree.")
	}
	if unsealed == 0 {
		t.Fatal("the template's expansion metadata records only SEALED args, which are not a leak:\n" +
			"they are immutable and Fork shares them outright anyway. The fixture must build its\n" +
			"call form at runtime so the recorded nodes are mutable.")
	}

	if w := CheckForkTemplate(env); len(w) != 0 {
		t.Errorf("a correct Fork produced %d witness(es):\n%s", len(w), renderWitnesses(w))
	}
}

// TestForkOwnExpansionMetadataIsNotALeak is the positive control, and
// without it the property is satisfied by a walker that strips metadata
// everywhere -- including from where it belongs.
//
// The FORK builds and expands its own call form. The recorded args are
// then fork-owned nodes the template never had, so the same check that
// fires on inherited metadata must stay silent here.
func TestForkOwnExpansionMetadataIsNotALeak(t *testing.T) {
	t.Parallel()
	env, err := debuggedEnv()
	if err != nil {
		t.Fatal(err)
	}
	fork, err := env.Fork()
	if err != nil {
		t.Fatal(err)
	}
	// A fork inherits no Debugger -- lisp/fork.go is explicit that "Profiler
	// and Debugger do not travel" -- so an embedder that wants debug
	// metadata in a transaction attaches one to the fork. That is the
	// configuration this control models, and it is the ONLY one in which a
	// fork can legitimately hold expansion metadata at all.
	fork.Runtime.Debugger = dormantDebugger{}
	if rc := fork.LoadString("tx.lisp", runtimeBuiltExpansion); rc.Type == lisp.LError {
		t.Fatal(rc)
	}

	own, unsealed := 0, 0
	for v := range reachableValues(fork) {
		m, ok := v.MacroExpansion()
		if !ok {
			continue
		}
		own++
		for _, a := range m.Args {
			if a != nil && !a.IsSealed() {
				unsealed++
			}
		}
	}
	if own == 0 || unsealed == 0 {
		t.Fatalf("the fork carries no expansion metadata with mutable args (meta=%d unsealed=%d), so\n"+
			"this control does not distinguish 'metadata that belongs here' from 'no metadata at all'",
			own, unsealed)
	}
	if w := macroExpansionLeaks(env, fork, "a fork"); len(w) != 0 {
		t.Errorf("a fork's OWN expansion metadata was reported as a leak:\n%s\n"+
			"The property is about metadata reaching TEMPLATE values. Reporting a fork's own\n"+
			"metadata would be satisfied by any walker that strips the field everywhere, which is\n"+
			"not the contract -- under a debugger the metadata is the feature.", renderWitnesses(w))
	}
}

// TestStampWalkerRunsItsMetadataBranch closes the second gap: the stamp
// walker's metadata path never executed under the guard, because
// stampedCopy's `if s.ctx != nil` branch needs an attached debugger and
// NewForkCheckEnv attached none. The existing 582 revert-proof row trips
// only because that mutation ALSO writes cp.source, which is encoded.
func TestStampWalkerRunsItsMetadataBranch(t *testing.T) {
	t.Parallel()
	for _, w := range Walkers() {
		if w.Name != "macro-stamp" {
			continue
		}
		got, err := CheckWalker(w, AliasCheck{
			NewEnv:  debuggedEnv,
			Program: runtimeBuiltExpansion,
		})
		if err != nil {
			t.Fatalf("macro-stamp under a dormant debugger: %v", err)
		}
		for _, wit := range got {
			t.Errorf("%s", wit)
		}
		return
	}
	t.Fatal("no macro-stamp walker is registered; this test is no longer checking anything")
}

func renderWitnesses(w []Witness) string {
	var b strings.Builder
	for _, x := range w {
		b.WriteString(x.String())
		b.WriteString("\n")
	}
	return b.String()
}

// TestMacroExpansionBehaviourPerWalker encodes the three behaviours rather
// than averaging them.
//
//	Fork          DROPS the metadata (lisp/fork.go, cp.macroExpansion = nil)
//	detach        DROPS it (lisp/detach.go)
//	(*LVal).Copy  DROPS it (#604, lisp/copier.go) -- it used to COPY it, a
//	              private tree whose metadata still recorded the SOURCE
//	              tree's nodes
//
// Copy's old behaviour was the same shape as the Fork leak this file's
// first test is about, and it mattered most on the TextLoader path, whose
// whole purpose is to hand each evaluation a PRIVATE tree.  This test was
// written to assert that behaviour and REPORT the seal state of the
// recorded args rather than give a verdict, because the verdict belonged
// to whoever changed Copy.  The measurement it made -- 1 of 3 recorded
// args NOT sealed, a pointer into a mutable source node -- is the verdict:
// #604 makes Copy drop the record like the other two walkers, and
// TestCopyDropsMacroExpansionMetadata (lisp/copier_test.go) is the
// control on Copy's side.  The Copy arm here now asserts the drop, and
// the anti-vacuity moves onto the SOURCE: the fixture must still record
// an unsealed arg, or the drop is asserted over nothing.
func TestMacroExpansionBehaviourPerWalker(t *testing.T) {
	t.Parallel()
	env, err := debuggedEnv()
	if err != nil {
		t.Fatal(err)
	}
	if rc := env.LoadString("p.lisp", runtimeBuiltExpansion); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	src := env.Get(lisp.Symbol("expansion"))
	if src == nil || src.Type == lisp.LError {
		t.Fatal(src)
	}
	if _, ok := src.MacroExpansion(); !ok {
		t.Fatal("the source value carries no expansion metadata, so none of the three assertions below\n" +
			"is testing anything. See runtimeBuiltExpansion for what the fixture needs.")
	}

	if m, ok := src.Copy().MacroExpansion(); ok {
		t.Errorf("(*LVal).Copy carries macro-expansion metadata across again (%s, %d recorded args).\n"+
			"#604 made Copy drop it, as Fork and detach do: the record's shared context points at the\n"+
			"SOURCE tree's nodes, so a copy that keeps it is a private tree with a back-pointer into\n"+
			"the tree it was copied from. TestCopyDropsMacroExpansionMetadata (lisp/copier_test.go) is\n"+
			"the control; if this is intended, the walker-contract table's macroExpansion row for Copy\n"+
			"(FingerprintOptions.MacroExpansion) says DROPS and is now wrong.", m.Name, len(m.Args))
	}
	// detach REFUSES a tree containing a function value, and the expansion
	// of a `defun` contains one -- so the detach arm uses the smallest
	// metadata-carrying node that detaches, found rather than assumed.
	detSrc := src
	var det *lisp.LVal
	for _, cand := range append([]*lisp.LVal{src}, src.Cells...) {
		if _, ok := cand.MacroExpansion(); !ok {
			continue
		}
		if d, derr := walkraw.Detach(cand); derr == nil {
			detSrc, det = cand, d
			break
		}
	}
	if det == nil {
		t.Fatalf("no metadata-carrying node in the fixture could be detached, so the detach arm of this test checks nothing. Last subject: %v", detSrc)
	}
	if _, ok := det.MacroExpansion(); ok {
		t.Error("detach now carries macro-expansion metadata across; lisp/detach.go drops it, and the\n" +
			"contract table's row says DROPS.")
	}

	fork, err := env.Fork()
	if err != nil {
		t.Fatal(err)
	}
	for v := range reachableValues(fork) {
		if _, ok := v.MacroExpansion(); ok {
			t.Errorf("a fork carries macro-expansion metadata; lisp/fork.go drops it, and a fork inherits\n" +
				"no Debugger either, so there is no path by which a fork should hold any.")
			break
		}
	}

	// The seal state of what the SOURCE records: the input to the question
	// this test used to leave open (whether Copy carrying the record was a
	// harm on the TextLoader path).  It stays as the anti-vacuity for the
	// Copy arm above: a fixture recording only sealed args would make the
	// drop an assertion over nothing worth dropping.
	m, _ := src.MacroExpansion()
	sealed, unsealed := 0, 0
	for _, a := range m.Args {
		if a == nil {
			continue
		}
		if a.IsSealed() {
			sealed++
		} else {
			unsealed++
		}
	}
	t.Logf("the source records %d args: %d sealed, %d NOT sealed; (*LVal).Copy carries none across (#604)", len(m.Args), sealed, unsealed)
	if unsealed == 0 {
		t.Fatal("the source's recorded args are all sealed, which are immutable and shared by design;\n" +
			"the fixture no longer records a mutable node, so the Copy arm asserts a drop of nothing\n" +
			"that could leak. See runtimeBuiltExpansion.")
	}

	// And the fingerprint option exists so the difference is visible at
	// all: with it off, a copy that dropped the metadata is
	// indistinguishable from one that kept it.
	on := func(v *lisp.LVal) string {
		return FingerprintValue(v, FingerprintOptions{MacroExpansion: true}).String()
	}
	off := func(v *lisp.LVal) string {
		return FingerprintValue(v, FingerprintOptions{}).String()
	}
	if off(detSrc) != off(det) {
		t.Log("note: source and detached copy already differ with the option off, so the check below " +
			"is weaker than intended")
	}
	if on(detSrc) == on(det) {
		t.Error("with MacroExpansion set, a value and a DETACHED copy that dropped its metadata\n" +
			"fingerprint identically. The token is not being emitted, so the option records nothing\n" +
			"and the three behaviours above cannot be told apart by the fingerprint at all.")
	}
}
