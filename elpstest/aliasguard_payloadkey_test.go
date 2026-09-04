// Copyright © 2026 The ELPS authors

// Controls for WHICH HEADERS CARRY A PAYLOAD.
//
// Every surface in this package that asks "does this value hold mutable
// storage a fork could share" used to key on `v.Type == LNative`. That is
// wrong twice over. LVal.Native is SHARED STORAGE -- LBytes keeps its
// *[]byte there, LSortMap its *MapData, LFun its *funData -- and an embedder
// can annotate an ordinary node with a payload of its own. Keying on the
// type saw only the last of the four.
//
// The measured consequence is #603's: a Reader that annotates a SEALED
// LSExpr reaches every fork by reference, because a sealed value is shared
// outright before the native policy runs, so its NativeCloner is never
// consulted -- and CheckTransactions with ExpectNoSharedNatives reported
// nothing, because the census only looked under LNative.

package elpstest

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

type payloadKeyProbe struct{ n int }

func payloadKeyEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	return env
}

func censusPaths(env *lisp.LEnv) map[string]string {
	out := map[string]string{}
	for p, path := range reachableNatives(env) {
		out[path] = fmt.Sprintf("%T", p)
	}
	return out
}

// TestCensusSeesPayloadsOnNonNativeHeaders is the paired control the widened
// key exists for.
//
// LNative -> censused (it always was). LBytes and LSortMap -> censused now
// (they were not). LString -> NOT censused, because it holds no payload at
// all: that is the negative half, and without it the test would pass on a
// census that simply reported everything.
func TestCensusSeesPayloadsOnNonNativeHeaders(t *testing.T) {
	t.Parallel()
	env := payloadKeyEnv(t)
	prog := `
(set 'm (sorted-map "k" 1))
(set 'b (to-bytes "abc"))
(set 's "a string, which holds no payload")
`
	if rc := env.LoadString("p.lisp", prog); rc.Type == lisp.LError {
		t.Fatal(rc)
	}
	env.PutGlobal(lisp.Symbol("na"), lisp.Native(&payloadKeyProbe{n: 1}))

	paths := censusPaths(env)
	for _, name := range []string{"user:na", "user:b", "user:m"} {
		if _, ok := paths[name]; !ok {
			t.Errorf("the census did not reach %s.\n"+
				"Native is shared storage: LBytes keeps a *[]byte there and LSortMap a *MapData, so a\n"+
				"census keyed on `v.Type == LNative` sees neither -- and a fork that shared one would\n"+
				"be reported by nothing. census: %v", name, paths)
		}
	}
	if _, ok := paths["user:s"]; ok {
		t.Error("the census reached a STRING, which carries no payload. The key has become\n" +
			"`anything reachable` rather than `anything holding mutable storage`, and every witness\n" +
			"it produces from now on is noise.")
	}
	// The other way the key can go wrong: counting KERNEL-owned payloads.
	// Every builtin is an LFun holding a *funData, and packages are shared
	// between forks by design, so a census that counts them reports the
	// standard library instead of the embedder's payloads -- measured at 142
	// entries for this graph's three real ones before kernelOwnedPayload
	// bounded it.
	if len(paths) > 8 {
		t.Errorf("the census returned %d entries for a graph with four payloads in it. It is counting\n"+
			"kernel-owned storage -- *funData on every builtin is the usual culprit -- which turns a\n"+
			"census of embedder payloads into a census of the standard library. census: %v",
			len(paths), paths)
	}
}

// TestCensusSeesAnAnnotationOnASealedNode is #603's shape, which is the
// reason the key was widened rather than merely tidied.
//
// A sealed node is shared OUTRIGHT by Fork, before the native policy runs,
// so an embedder annotation on one reaches every fork by reference and its
// NativeCloner is never consulted. Nothing in this package could see that:
// the annotation sits on an LSExpr, and the census only looked under
// LNative. It is now visible, which is what makes
// TestLoadCacheTopology_NativeAnnotationGapStillOpen (#603) able to close.
func TestCensusSeesAnAnnotationOnASealedNode(t *testing.T) {
	t.Parallel()
	env := payloadKeyEnv(t)
	sealed := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	sealed.SealAST()
	sealed.Native = &payloadKeyProbe{n: 2}
	env.PutGlobal(lisp.Symbol("annotated"), sealed)

	if !sealed.IsSealed() {
		t.Fatal("the fixture is not sealed, so it does not model the shape this test is about")
	}
	if _, ok := censusPaths(env)["user:annotated"]; !ok {
		t.Errorf("an embedder annotation on a SEALED LSExpr is invisible to the cross-fork census.\n"+
			"Fork shares a sealed value outright, before the native policy runs, so this payload\n"+
			"reaches every fork by reference with its NativeCloner never consulted -- and this census\n"+
			"is the surface that should report it. census: %v", censusPaths(env))
	}
}

// TestFingerprintEncodesAnAnnotationIdentity is the fingerprint half of the
// same widening: two headers over ONE annotation must be distinguishable
// from two headers over two equal annotations, or a walker that de-aliased
// them would fingerprint identically.
func TestFingerprintEncodesAnAnnotationIdentity(t *testing.T) {
	t.Parallel()
	shared := &payloadKeyProbe{n: 3}

	one := func(a, b any) string {
		x := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
		x.Native = a
		y := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
		y.Native = b
		return FingerprintValue(lisp.QExpr([]*lisp.LVal{x, y}), FingerprintOptions{}).String()
	}
	aliased := one(shared, shared)
	distinct := one(&payloadKeyProbe{n: 3}, &payloadKeyProbe{n: 3})
	if aliased == distinct {
		t.Error("two headers over ONE annotation fingerprint the same as two headers over two equal\n" +
			"annotations. The fingerprint cannot see annotation sharing at all, so a walker that\n" +
			"de-aliased one -- or interned two into one -- would pass unnoticed.")
	}
}

// TestFingerprintIgnoresACellViewLink is the pre-#602 half of option (b).
//
// A cell view records its root in Native and its offset in Int (PR #602).
// That is a REFERENCE, not a payload: the root is reachable state, and the
// link itself must get no identity ordinal and must not stop the walk.
//
// This constructs the shape BY HAND, so it is meaningful on this branch --
// where no constructor produces one -- and stays meaningful under #602,
// where every cdr does. It covers a CONSISTENT link and a STALE one
// together, and asserts they are treated ALIKE: the cheap predicate
// (isCellViewLink here, v.IsCellView() after the restack) deliberately does
// not distinguish them, because a stale link is one Fork copies privately
// rather than a finding. Telling them apart is the validated resolver's job
// (v.CellView()), which arrives with the restack along with following the
// root as a probe site.
func TestFingerprintIgnoresACellViewLink(t *testing.T) {
	t.Parallel()
	root := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})

	plain := lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)})

	consistent := lisp.QExpr(root.Cells[1:])
	consistent.Native = root
	consistent.Int = 1

	stale := lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(3)})
	stale.Native = root
	stale.Int = 99 // a link that no longer describes the header

	fp := func(v *lisp.LVal) string {
		return FingerprintValue(v, FingerprintOptions{}).String()
	}
	base := fp(plain)
	for name, v := range map[string]*lisp.LVal{"consistent": consistent, "stale": stale} {
		if !isCellViewLink(v) {
			t.Fatalf("%s: the fixture does not read as a cell-view link, so this test is not\n"+
				"exercising what it claims", name)
		}
		if got := fp(v); got != base {
			t.Errorf("a %s cell-view link changed the fingerprint of the value carrying it.\n"+
				"The link is a reference to a root, not a payload: giving it an identity ordinal\n"+
				"changes the fingerprint of every program containing a cdr once PR #602 lands, and\n"+
				"a fork that failed to re-point would be reported as a shared NATIVE rather than as\n"+
				"a de-aliased cell -- the wrong witness for the wrong bug.\n  got:  %s\n  want: %s",
				name, got, base)
		}
	}

	// And the walk does not stop at the link: the view's own cells are still
	// fingerprinted, which is what an opaque-payload arm would have lost.
	if !strings.Contains(fp(consistent), "int(2)") {
		t.Errorf("the walk stopped at the cell-view link instead of walking the view's own Cells.\n"+
			"fingerprint: %s", fp(consistent))
	}
}

// TestCensusIgnoresACellViewLink is the census half of the same rule.
func TestCensusIgnoresACellViewLink(t *testing.T) {
	t.Parallel()
	env := payloadKeyEnv(t)
	root := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	view := lisp.QExpr(root.Cells[1:])
	view.Native = root
	view.Int = 1
	env.PutGlobal(lisp.Symbol("view"), view)

	if _, ok := censusPaths(env)["user:view"]; ok {
		t.Errorf("the cross-fork census counted a cell-view link as a payload. Within one\n"+
			"environment that pointer is the intended root; across forks the root differs per fork,\n"+
			"so every view becomes a census entry and a fork that failed to re-point is reported as\n"+
			"sharing a native. census: %v", censusPaths(env))
	}
}
