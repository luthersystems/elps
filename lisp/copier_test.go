// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"errors"
	"fmt"
	"maps"
	"slices"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// The controls for lisp/copier.go: each defect it fixes, as a program that
// must behave under (*LVal).Copy exactly as it behaves on a cold
// environment, and the alias guard's oracle over the walker.

func copierEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("init: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		t.Fatalf("in-package: %v", rc)
	}
	return env
}

func copierEval(t *testing.T, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("test.lisp", src)
	if v.Type == lisp.LError {
		t.Fatalf("%s: %v", src, v)
	}
	return v
}

// copierRepro runs program on a cold environment, records what probe
// returns there (the reference), then binds a COPY of `pair` on a second
// environment under the same name and requires probe to agree, and the
// source to be untouched by it.
func copierRepro(t *testing.T, program, probe, wantOriginal string) {
	t.Helper()
	cold := copierEnv(t)
	copierEval(t, cold, program)
	want := copierEval(t, cold, probe).String()

	env := copierEnv(t)
	copierEval(t, env, program)
	src := env.GetGlobal(lisp.Symbol("pair"))
	cp := src.Copy()
	if cp.Type == lisp.LError {
		t.Fatalf("copy: %v", cp)
	}
	if rc := env.PutGlobal(lisp.Symbol("pair"), cp); rc.Type == lisp.LError {
		t.Fatalf("rebind: %v", rc)
	}
	got := copierEval(t, env, probe).String()
	if got != want {
		t.Errorf("through the copy: %s\n  cold: %s\n  copy: %s", probe, want, got)
	}
	if orig := src.String(); orig != wantOriginal {
		t.Errorf("the source moved under a write through the copy: %s, want %s", orig, wantOriginal)
	}
}

// TestCopyMemoisesMapPayloadAcrossHeaders is the #576/#585 shape under
// Copy: two names for one sorted map, a write through one, a read through
// the other.  Before lisp/copier.go the copy held two maps and read nil.
func TestCopyMemoisesMapPayloadAcrossHeaders(t *testing.T) {
	copierRepro(t,
		`(set 'a (sorted-map "k" 1)) (set 'b (quasiquote (unquote a))) (set 'pair (list a b))`,
		`(assoc! (first pair) "y" 7) (list (get (second pair) "y") (get (first pair) "y"))`,
		`'((sorted-map "k" 1) (sorted-map "k" 1))`)
}

// TestCopyMemoisesBytesPayloadAcrossHeaders is the same shape over a bytes
// value.  Before lisp/copier.go the copy SHARED the source's buffer: the
// through-the-copy read agreed with the cold run for the wrong reason, and
// the source moved.
func TestCopyMemoisesBytesPayloadAcrossHeaders(t *testing.T) {
	copierRepro(t,
		`(set 'a (to-bytes "abc")) (set 'b (quasiquote (unquote a))) (set 'pair (list a b))`,
		`(append! (first pair) 7) (list (length (second pair)) (length (first pair)))`,
		`'(#<bytes 97 98 99> #<bytes 97 98 99>)`)
}

// TestCopyRebuildsAContainerHeldAsAMapValue: a bytes value parked inside a
// map is the copy's own, so a write through the copy's map does not reach
// the source.  This is the value-walking half of the fix.
func TestCopyRebuildsAContainerHeldAsAMapValue(t *testing.T) {
	copierRepro(t,
		`(set 'buf (to-bytes "abc")) (set 'pair (list (sorted-map "raw" buf) buf))`,
		`(append! (get (first pair) "raw") 7) (list (length (second pair)) (length (get (first pair) "raw")))`,
		`'((sorted-map "raw" #<bytes 97 98 99>) #<bytes 97 98 99>)`)
}

// copierProgram is the alias guard's historical shape (#599's aliasProgram):
// two names for one sorted map, two names for one bytes value, a map
// reaching itself through the second header, all nested inside a list and a
// map.
const copierProgram = `
(set 'a (sorted-map "k" 1))
(set 'b (quasiquote (unquote a)))
(assoc! a "self" b)
(set 'buf (to-bytes "abc"))
(set 'buf2 (quasiquote (unquote buf)))
(set 'probe (list a b buf buf2 (sorted-map "inner" a "raw" buf)))
`

// copierProbe is a pre-order walk of the value reachable from v: for every
// slot it visits it records the index of the FIRST slot that held the same
// header, so the resulting sequence describes the value's internal sharing
// independently of the addresses involved.  It also collects every header and
// every mutable payload (a *MapData, a bytes buffer) the walk reaches.  A
// repeat is recorded but not descended into, so a cycle terminates.
type copierProbe struct {
	first   map[*lisp.LVal]int
	ids     []int
	headers map[*lisp.LVal]bool
	maps    map[*lisp.MapData]bool
	buffers map[*[]byte]bool
}

func newCopierProbe() *copierProbe {
	return &copierProbe{
		first:   make(map[*lisp.LVal]int),
		headers: make(map[*lisp.LVal]bool),
		maps:    make(map[*lisp.MapData]bool),
		buffers: make(map[*[]byte]bool),
	}
}

func (p *copierProbe) walk(v *lisp.LVal) {
	if v == nil {
		p.ids = append(p.ids, -1)
		return
	}
	// Sharing is a property of containers and payload headers: a leaf has
	// no storage two slots could share and no cycle to close, so the copier
	// does not memoise it and this probe does not record its identity (a
	// leaf reached twice reads as two leaves, on the source and on the copy
	// alike).
	memoised := cap(v.Cells) > 0 || v.Native != nil
	if memoised {
		if id, ok := p.first[v]; ok {
			p.ids = append(p.ids, id)
			return
		}
	}
	id := len(p.ids)
	p.ids = append(p.ids, id)
	if memoised {
		p.first[v] = id
	}
	p.headers[v] = true
	switch v.Type {
	case lisp.LSortMap:
		p.maps[v.Map()] = true
		for _, k := range v.MapKeys().Cells {
			p.walk(v.MapGet(k))
		}
	case lisp.LBytes:
		if buf, ok := v.Native.(*[]byte); ok {
			p.buffers[buf] = true
		}
	default:
		for _, c := range v.Cells {
			p.walk(c)
		}
	}
}

// TestCopyMeetsTheAliasGuard drives (*LVal).Copy over the historical
// aliasing shape and pins the two properties the copier exists for: the copy
// has the SAME internal sharing as the source (two names for one map are two
// names for one map on the copy, and the map that reaches itself still
// does), and it shares NONE of the source's mutable payloads -- no header, no
// *MapData, no bytes buffer.  Deleting either payload memo in lisp/copier.go
// turns this red (the map memo splits the self-referencing map; the bytes
// memo splits buf from buf2), and so does deleting the header memo.
//
// Ported from #604 as a package-local oracle: the original drove the same
// value through elpstest.CheckWalker and asserted the copier's row in
// elpstest.Walkers(); neither the alias-guard harness nor the walker registry
// exists on this branch, so the registry assertion is gone and the guard's
// sharing/payload properties are asserted here directly.
func TestCopyMeetsTheAliasGuard(t *testing.T) {
	t.Parallel()
	assertCopierMeetsTheAliasGuard(t, func(v *lisp.LVal) *lisp.LVal { return v.Copy() })
}

// assertCopierMeetsTheAliasGuard is the guard itself, over any walk that
// claims to be Copy: build the historical aliasing shape on a cold
// environment, copy the probe value through the walk under test, and compare
// the two values' sharing and payloads.
func assertCopierMeetsTheAliasGuard(t *testing.T, walk func(*lisp.LVal) *lisp.LVal) {
	t.Helper()
	env := copierEnv(t)
	copierEval(t, env, copierProgram)
	src := env.GetGlobal(lisp.Symbol("probe"))
	if src.Type == lisp.LError {
		t.Fatalf("probe: %v", src)
	}
	cp := walk(src)
	if cp.Type == lisp.LError {
		t.Fatalf("copy: %v", cp)
	}

	sp, dp := newCopierProbe(), newCopierProbe()
	sp.walk(src)
	dp.walk(cp)

	if len(sp.ids) != len(dp.ids) {
		t.Fatalf("the copy has a different shape: %d slots, source has %d", len(dp.ids), len(sp.ids))
	}
	for i := range sp.ids {
		if sp.ids[i] != dp.ids[i] {
			t.Errorf("slot %d: the copy aliases slot %d, the source aliases slot %d", i, dp.ids[i], sp.ids[i])
		}
	}
	if len(sp.headers) != len(dp.headers) {
		t.Errorf("the copy reaches %d headers, the source reaches %d", len(dp.headers), len(sp.headers))
	}
	for h := range dp.headers {
		if sp.headers[h] {
			t.Errorf("the copy holds one of the source's headers (%v)", h.Type)
		}
	}
	for m := range dp.maps {
		if sp.maps[m] {
			t.Errorf("the copy holds one of the source's *MapData payloads")
		}
	}
	for b := range dp.buffers {
		if sp.buffers[b] {
			t.Errorf("the copy holds one of the source's bytes buffers")
		}
	}
	if len(dp.maps) == 0 || len(dp.buffers) == 0 {
		t.Fatalf("anti-vacuity: the walk reached %d maps and %d bytes buffers", len(dp.maps), len(dp.buffers))
	}
}

// TestCopyTerminatesOnACycle: a value that contains itself copies to a value
// that contains ITSELF -- the copy, not the original -- and a subtree
// reachable twice is copied once.  Copy used to recurse without bound on
// the first shape (lisp/package_admit.go classifies cycles before copying
// for that reason).
func TestCopyTerminatesOnACycle(t *testing.T) {
	t.Parallel()
	cyclic := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	cyclic.Cells = append(cyclic.Cells, cyclic)
	cp := cyclic.Copy()
	if cp == cyclic {
		t.Fatal("Copy returned the original")
	}
	if cp.Cells[1] != cp {
		t.Errorf("the copied cycle closes onto %p, want the copy %p (original %p)", cp.Cells[1], cp, cyclic)
	}

	shared := lisp.QExpr([]*lisp.LVal{lisp.Int(2)})
	pair := lisp.QExpr([]*lisp.LVal{shared, shared})
	pcp := pair.Copy()
	if pcp.Cells[0] != pcp.Cells[1] {
		t.Errorf("a subtree reachable twice was copied twice")
	}
	if pcp.Cells[0] == shared {
		t.Errorf("the copy holds the source's subtree")
	}
}

type copierCloner struct{ clones int }

func (c *copierCloner) CloneNative() interface{} { return &copierCloner{clones: c.clones + 1} }

// TestCopyClonesANativeClonerOncePerPayload: a NativeCloner reachable under
// two headers is cloned once, and the clone is shared by both copied
// headers -- the detacher's rule in copy mode.  A payload that is not a
// NativeCloner stays shared by reference.
func TestCopyClonesANativeClonerOncePerPayload(t *testing.T) {
	t.Parallel()
	payload := &copierCloner{}
	h1 := lisp.Native(payload)
	h2 := lisp.Native(payload)
	pair := lisp.QExpr([]*lisp.LVal{h1, h2})
	cp := pair.Copy()
	c0, ok0 := cp.Cells[0].Native.(*copierCloner)
	c1, ok1 := cp.Cells[1].Native.(*copierCloner)
	if !ok0 || !ok1 {
		t.Fatalf("copied natives are %T and %T", cp.Cells[0].Native, cp.Cells[1].Native)
	}
	if c0 == payload || c1 == payload {
		t.Errorf("the copy shares the NativeCloner payload with the source")
	}
	if c0 != c1 {
		t.Errorf("two headers over one NativeCloner payload were cloned twice (%p, %p)", c0, c1)
	}
	if c0.clones != 1 {
		t.Errorf("clone count %d, want 1", c0.clones)
	}

	plain := &strings.Builder{}
	ph := lisp.Native(plain)
	if got := ph.Copy().Native; got != plain {
		t.Errorf("a native payload that is not a NativeCloner must be shared by reference; got %p, want %p", got, plain)
	}
}

// TestCopySharesTheCallStack pins the one payload Copy deliberately shares:
// an LError's *CallStack, immutable by construction (CallStack.Copy
// allocates exact-length Frames at every capture site and nothing writes a
// captured stack).
func TestCopySharesTheCallStack(t *testing.T) {
	t.Parallel()
	env := copierEnv(t)
	err := env.LoadString("err.lisp", `(error 'test "boom")`)
	if err.Type != lisp.LError {
		t.Fatalf("want an error value, got %v", err)
	}
	st := err.CallStack()
	if st == nil {
		t.Skip("the error carries no call stack on this path")
	}
	if cp := err.Copy(); cp.CallStack() != st {
		t.Errorf("Copy rebuilt the call stack; it is shared by design")
	}
}

// TestCopyLeafAllocatesLikeAStructCopy pins the cost of a leaf: copying a
// value with nothing to alias allocates exactly what the struct copy
// allocates -- the copier and its inline memo live on Copy's stack.  Copy is called on leaves inside
// hot builtins (insert-sorted's binary search), and
// TestVectorBuiltinAllocations pins those counts as equalities; this is the
// same property at Copy's own boundary.
func TestCopyLeafAllocatesLikeAStructCopy(t *testing.T) {
	leaf := lisp.Int(7)
	n := testing.AllocsPerRun(200, func() { leaf.Copy() })
	if n != 1 {
		t.Errorf("copying an int allocated %v times, want 1 (the header)", n)
	}
	sym := lisp.Symbol("x")
	if n := testing.AllocsPerRun(200, func() { sym.Copy() }); n != 1 {
		t.Errorf("copying a symbol allocated %v times, want 1", n)
	}
}

// TestCopySmallWalkDoesNotAllocateAMemo: a walk that fits the inline memo
// allocates one header per node and one cells slice per list, nothing for
// the memo itself -- the copier stays on Copy's stack.
func TestCopySmallWalkDoesNotAllocateAMemo(t *testing.T) {
	list := lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3)})
	n := testing.AllocsPerRun(200, func() { list.Copy() })
	// 4 headers + 1 cells slice.
	if n != 5 {
		t.Errorf("copying a 3-element list allocated %v times, want 5", n)
	}
}

// TestCopyMemoSpillsPastTheInlineArray: past the inline memo the walk
// spills to a map and stays correct -- a subtree shared across the spill
// boundary is still copied once, and a cycle still closes onto the copy.
func TestCopyMemoSpillsPastTheInlineArray(t *testing.T) {
	t.Parallel()
	shared := lisp.QExpr([]*lisp.LVal{lisp.Int(0)})
	cells := make([]*lisp.LVal, 0, 64)
	cells = append(cells, shared)
	// Containers, not leaves: only a memoised header counts towards the
	// spill, and a leaf is not memoised.
	for i := range 60 {
		cells = append(cells, lisp.QExpr([]*lisp.LVal{lisp.Int(i)}))
	}
	cells = append(cells, shared)
	big := lisp.QExpr(cells)
	big.Cells = append(big.Cells, big)
	cp := big.Copy()
	if cp.Cells[0] != cp.Cells[61] {
		t.Errorf("a subtree shared across the memo spill was copied twice")
	}
	if cp.Cells[0] == shared {
		t.Errorf("the copy holds the source's subtree")
	}
	if cp.Cells[62] != cp {
		t.Errorf("the cycle closes onto %p, want the copy %p", cp.Cells[62], cp)
	}
}

// The dormant debugger the fixture needs is lisp_test's existing one
// (macro_stamp_shared_ast_test.go): attached but never enabled, which is
// what gates macro-expansion metadata (lisp/env.go keys on
// Runtime.Debugger != nil, not on IsEnabled).

// copierReachable walks everything reachable from v through cells and
// sorted-map values, once per header.
// copierReachable collects the headers the copier memoises under v: the
// containers and payload headers, not the leaves (lisp/copier.go's memoise
// gate).  A leaf is still descended through -- it has nothing under it.
func copierReachable(v *lisp.LVal, seen map[*lisp.LVal]bool) {
	if v == nil || seen[v] {
		return
	}
	if cap(v.Cells) > 0 || v.Native != nil {
		seen[v] = true
	}
	for _, c := range v.Cells {
		copierReachable(c, seen)
	}
	if v.Type == lisp.LSortMap && v.Map() != nil {
		for _, k := range v.Map().Keys().Cells {
			val, _ := v.Map().Get(k)
			copierReachable(val, seen)
		}
	}
}

// TestCopyDropsMacroExpansionMetadata: a copied tree carries no
// macro-expansion metadata, and therefore no pointer through that
// metadata into the tree it was copied from.  The fixture is the alias
// guard's macro-expansion fixture (#599): a call form built at
// RUNTIME (so its nodes are unsealed), expanded by a BUILTIN macro (only
// synthetic nodes are stamped) under an attached debugger, and RETAINED by
// macroexpand-1.  Anti-vacuity first: the source must carry metadata whose
// recorded args include an unsealed source node, or the property has
// nothing to look at.  Then two assertions, the second walker-independent:
// no node reachable from the copy reports MacroExpansion(), and no *LVal
// reachable from the copy's metadata args is in the source's reachable
// set.  Before lisp/copier.go, Copy carried the record across and the
// second assertion failed on the unsealed args.
func TestCopyDropsMacroExpansionMetadata(t *testing.T) {
	t.Parallel()
	env := copierEnv(t)
	env.Runtime.Debugger = dormantDebugger{}
	copierEval(t, env, `
(set 'form (list 'defun 'leaky (list 'x) 'x))
(set 'expansion (macroexpand-1 form))
`)
	src := env.GetGlobal(lisp.Symbol("expansion"))
	if src.Type == lisp.LError {
		t.Fatal(src)
	}
	srcSet := map[*lisp.LVal]bool{}
	copierReachable(src, srcSet)
	copierReachable(env.GetGlobal(lisp.Symbol("form")), srcSet)
	meta, unsealed := 0, 0
	for v := range srcSet {
		m, ok := v.MacroExpansion()
		if !ok {
			continue
		}
		meta++
		for _, a := range m.Args {
			if a != nil && !a.IsSealed() && srcSet[a] {
				unsealed++
			}
		}
	}
	if meta == 0 || unsealed == 0 {
		t.Fatalf("the source carries %d node(s) with metadata and %d unsealed recorded arg(s) in its own tree;\n"+
			"the fixture no longer builds what this test is about (see the comment)", meta, unsealed)
	}

	cp := src.Copy()
	if cp.Type == lisp.LError {
		t.Fatal(cp)
	}
	cpSet := map[*lisp.LVal]bool{}
	copierReachable(cp, cpSet)
	for v := range cpSet {
		m, ok := v.MacroExpansion()
		if !ok {
			continue
		}
		t.Errorf("a copied node carries macro-expansion metadata (%s); Copy drops it as Fork and detach do", m.Name)
		for _, a := range m.Args {
			if srcSet[a] {
				t.Errorf("the copy's metadata records a node of the SOURCE tree (sealed=%t): a private tree with a\n"+
					"back-pointer into the tree it was copied from", a.IsSealed())
			}
		}
	}
}

// copierStringMap is a minimal custom Map implementation over string keys:
// the extension point NewMapData/SortedMapFromData exposes to embedders,
// which is what the copier's non-sortedmap arms exist for.  It is NOT a
// StringKeyRanger; the fixtures below opt into that interface, or into a
// failing Entries, one at a time.
type copierStringMap struct{ m map[string]*lisp.LVal }

func newCopierStringMap(kv map[string]*lisp.LVal) *copierStringMap {
	m := &copierStringMap{m: make(map[string]*lisp.LVal, len(kv))}
	for k, v := range kv {
		m.m[k] = v
	}
	return m
}

func (m *copierStringMap) sortedKeys() []string {
	keys := make([]string, 0, len(m.m))
	for k := range m.m {
		keys = append(keys, k)
	}
	slices.Sort(keys)
	return keys
}

func (m *copierStringMap) Len() int { return len(m.m) }

func (m *copierStringMap) Get(k *lisp.LVal) (*lisp.LVal, bool) {
	switch k.Type {
	case lisp.LString, lisp.LSymbol:
		if v, ok := m.m[k.Str]; ok {
			return v, true
		}
		return lisp.Nil(), false
	default:
		return lisp.Errorf("unhashable type: %s", k.Type), false
	}
}

func (m *copierStringMap) Set(k, v *lisp.LVal) *lisp.LVal {
	switch k.Type {
	case lisp.LString, lisp.LSymbol:
		m.m[k.Str] = v
		return lisp.Nil()
	default:
		return lisp.Errorf("unhashable type: %s", k.Type)
	}
}

func (m *copierStringMap) Del(k *lisp.LVal) *lisp.LVal {
	switch k.Type {
	case lisp.LString, lisp.LSymbol:
		delete(m.m, k.Str)
		return lisp.Nil()
	default:
		return lisp.Errorf("unhashable type: %s", k.Type)
	}
}

func (m *copierStringMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	keys := m.sortedKeys()
	if len(buf) < len(keys) {
		return lisp.Errorf("buffer has insufficient length")
	}
	for i, k := range keys {
		buf[i] = lisp.QExpr([]*lisp.LVal{lisp.String(k), m.m[k]})
	}
	return lisp.Int(len(keys))
}

func (m *copierStringMap) Keys() *lisp.LVal {
	keys := m.sortedKeys()
	cells := make([]*lisp.LVal, len(keys))
	for i, k := range keys {
		cells[i] = lisp.String(k)
	}
	return lisp.QExpr(cells)
}

// copierFailingRanger is a StringKeyRanger that emits one entry and then
// reports the failure Entries would have reported -- the documented shape
// of a partial walk (lisp/maps.go: "every caller discards a partial walk on
// error").
type copierFailingRanger struct{ *copierStringMap }

func (m *copierFailingRanger) RangeStringKeys(fn func(key string, val *lisp.LVal)) error {
	for i, k := range m.sortedKeys() {
		if i >= 1 {
			break
		}
		fn(k, m.m[k])
	}
	return errors.New("ranger failed part-way")
}

// copierFailingEntries fails on the copier's generic Entries path, and does
// it AFTER a value has already been copied and stored: the first pair is
// well formed, the second carries an unhashable key, so MapData.Set rejects
// it.  Partial traversal, on the arm that has no ranger.
type copierFailingEntries struct{ *copierStringMap }

func (m *copierFailingEntries) Entries(buf []*lisp.LVal) *lisp.LVal {
	keys := m.sortedKeys()
	if len(keys) < 2 {
		return lisp.Errorf("fixture needs at least two entries")
	}
	if len(buf) < len(keys) {
		return lisp.Errorf("buffer has insufficient length")
	}
	buf[0] = lisp.QExpr([]*lisp.LVal{lisp.String(keys[0]), m.m[keys[0]]})
	buf[1] = lisp.QExpr([]*lisp.LVal{lisp.Int(2), m.m[keys[1]]})
	for i := 2; i < len(keys); i++ {
		buf[i] = lisp.QExpr([]*lisp.LVal{lisp.String(keys[i]), m.m[keys[i]]})
	}
	return lisp.Int(len(keys))
}

// copierSafeWalk collects every header reachable from v.  A sorted-map's
// values are walked under a recover because this helper is pointed at the
// output of a FAILED copy: before the fix that output could hold a
// half-built *MapData whose backing is nil, and reading it panics.  A panic
// there is itself a defect, but it is not the one being asserted, and the
// assertions below say more about the failure than a stack trace does.
func copierSafeWalk(v *lisp.LVal, seen map[*lisp.LVal]bool) {
	if v == nil || seen[v] {
		return
	}
	seen[v] = true
	for _, c := range v.Cells {
		copierSafeWalk(c, seen)
	}
	if v.Type == lisp.LSortMap {
		func() {
			defer func() { _ = recover() }()
			for _, k := range v.MapKeys().Cells {
				copierSafeWalk(v.MapGet(k), seen)
			}
		}()
	}
}

// assertFailedCopyFailsAsAWhole copies v, whose walk reaches a sorted-map
// that CANNOT be copied, and pins the contract a failed copy has to keep:
// the copy fails AS A WHOLE, and nothing the abandoned walk built escapes.
//
// Handing back a container with the failure parked in one cell is not a
// failed copy -- it is a SUCCESSFUL copy of a broken value.  The caller
// sees a list rather than an error, and the list's other cells hold
// whatever the abandoned walk had managed to build: a header over the
// SOURCE's map, or a header over the half-built payload the walk seeded
// before it failed, which panics on the first write.  So the assertions
// are, in order: the top-level result is an error; no sorted-map header is
// reachable from it at all; and, as the property in its observable form, a
// write through any map that IS reachable neither panics nor lands in the
// source.
func assertFailedCopyFailsAsAWhole(t *testing.T, v *lisp.LVal, srcMD *lisp.MapData, srcHeaders ...*lisp.LVal) {
	t.Helper()
	cp := v.Copy()
	if cp.Type != lisp.LError {
		t.Errorf("Copy returned a %v, want an error: a copy whose sorted-map arm failed must fail as a\n"+
			"whole rather than hand back a container with the failure parked in one cell.", cp.Type)
	}
	seen := map[*lisp.LVal]bool{}
	copierSafeWalk(cp, seen)
	for h := range seen {
		if slices.Contains(srcHeaders, h) {
			t.Errorf("the copy holds one of the source's headers (%v)", h.Type)
		}
		if h.Type != lisp.LSortMap {
			continue
		}
		t.Errorf("a sorted-map is reachable from the result of a FAILED copy (it carries the SOURCE's\n"+
			"*MapData: %t). A failed copy must publish no map header at all.", h.Map() == srcMD)
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Errorf("writing through that escaped sorted-map panicked (%v): it is a header over a\n"+
						"HALF-BUILT payload, seeded so a self-reference could close onto it and never finished.", r)
				}
			}()
			h.MapSet("mutation-probe", lisp.Int(99))
		}()
	}
	if _, ok := srcMD.Get(lisp.String("mutation-probe")); ok {
		t.Errorf("a write through the copy reached the SOURCE's map")
	}
}

// TestCopyFailedMapCopyLeavesNoSourceBackedHeader: when copying a
// sorted-map's payload FAILS, the whole Copy fails and nothing the failed
// walk built is reachable from what it returns.
//
// The original defect was one header deep.  The header memo is seeded with
// `c.remember(v, cp)` right after `*cp = *v` -- a struct copy that still
// holds the SOURCE's Native -- and the LSortMap arm only then replaces it,
// so an arm that errored out and walked away left that seeded cp memoised:
// a SECOND encounter of the same header yielded a sorted-map sharing the
// source's map, through which a write landed in the source.  Overwriting cp
// in place with the error closed that.
//
// It is not enough, and the shapes below say why.  The PAYLOAD memo is
// seeded too -- an empty *MapData published into c.maps before the entries
// are walked, so a map that reaches itself closes onto its own copy -- and
// a header the failing walk copies while holding that seed is memoised over
// the half-built payload.  Overwriting the FAILING header repairs that one
// header; the other one is already parked in the enclosing container's
// cell, over a *MapData whose backing was never assigned, and a write
// through it panics.  Copy's answer was a list whose first cell was an
// error and whose second was a live grenade, and Copy itself reported
// success.  Nothing short of failing the whole walk fixes that, because the
// half-built header exists by the time the failure is known.
//
// The shapes: one header reached twice, two headers over one payload, and
// the three self-referential ones where the map's own entry is a header
// over it -- through the other header in both cell orders, and through the
// same header.  All of them on both failing arms, the custom
// StringKeyRanger and the generic Entries path.  (Only the Entries arm
// copies a value before it fails, so only it builds the half-built header;
// the ranger arm collects its entries before copying any of them, and fails
// with the container's cells simply holding two errors.  Both are covered
// by the same contract.)
func TestCopyFailedMapCopyLeavesNoSourceBackedHeader(t *testing.T) {
	t.Parallel()
	for _, tt := range []struct {
		wrap func(*copierStringMap) lisp.Map
		name string
	}{
		{name: "string-key ranger fails part-way", wrap: func(m *copierStringMap) lisp.Map {
			return &copierFailingRanger{m}
		}},
		{name: "entries path fails after a partial copy", wrap: func(m *copierStringMap) lisp.Map {
			return &copierFailingEntries{m}
		}},
	} {
		t.Run(tt.name, func(t *testing.T) {
			// newFixture builds the failing payload and two DISTINCT
			// headers over it.  When self is non-nil, the header it picks
			// is parked in the map's first entry -- key "a", which is the
			// entry both failing arms reach before they fail -- so the map
			// reaches itself, and the copier meets that header while the
			// half-built payload is seeded in c.maps.
			newFixture := func(self func(m1, m2 *lisp.LVal) *lisp.LVal) (*lisp.MapData, *lisp.LVal, *lisp.LVal) {
				base := newCopierStringMap(map[string]*lisp.LVal{
					"a": lisp.Int(1), "b": lisp.Int(2), "c": lisp.Int(3),
				})
				md := lisp.NewMapData(tt.wrap(base))
				m1, m2 := lisp.SortedMapFromData(md), lisp.SortedMapFromData(md)
				if self != nil {
					base.m["a"] = self(m1, m2)
				}
				return md, m1, m2
			}
			throughM1 := func(m1, _ *lisp.LVal) *lisp.LVal { return m1 }
			t.Run("one header reached twice", func(t *testing.T) {
				md, m1, _ := newFixture(nil)
				assertFailedCopyFailsAsAWhole(t, lisp.QExpr([]*lisp.LVal{m1, m1}), md, m1)
			})
			t.Run("two headers over one payload", func(t *testing.T) {
				md, m1, m2 := newFixture(nil)
				assertFailedCopyFailsAsAWhole(t, lisp.QExpr([]*lisp.LVal{m1, m2}), md, m1, m2)
			})
			t.Run("self through the other header, that header second", func(t *testing.T) {
				md, m1, m2 := newFixture(throughM1)
				assertFailedCopyFailsAsAWhole(t, lisp.QExpr([]*lisp.LVal{m2, m1}), md, m1, m2)
			})
			t.Run("self through the other header, that header first", func(t *testing.T) {
				md, m1, m2 := newFixture(throughM1)
				assertFailedCopyFailsAsAWhole(t, lisp.QExpr([]*lisp.LVal{m1, m2}), md, m1, m2)
			})
			t.Run("self through the same header", func(t *testing.T) {
				md, m1, _ := newFixture(throughM1)
				assertFailedCopyFailsAsAWhole(t, lisp.QExpr([]*lisp.LVal{m1, m1}), md, m1)
			})
		})
	}
}

// copierCloneSeq is the external state a host clone hook draws on.  Only
// TestCopyMapValueCloneOrderIsDeterministic touches it, and that test does
// not run in parallel.
var copierCloneSeq int

// copierSeqCloner is a NativeCloner held BY VALUE, so the copier's native
// memo (pointer payloads only) is out of the picture and every clone is a
// fresh call into the hook.  The hook takes the next number from a counter
// -- an id allocator, a sequence, an rng: the ordinary shape of a host's
// CloneNative -- so the number a key's clone ends up with records the
// ORDER the copier walked the map's values in.
type copierSeqCloner struct{ seq int }

func (c copierSeqCloner) CloneNative() interface{} {
	copierCloneSeq++
	return copierSeqCloner{seq: copierCloneSeq}
}

// copierCloneAssignment copies m with the sequence counter reset and reads
// back which number each key's value was cloned with.
func copierCloneAssignment(t *testing.T, m *lisp.LVal) map[string]int {
	t.Helper()
	copierCloneSeq = 0
	cp := m.Copy()
	if cp.Type == lisp.LError {
		t.Fatalf("copy: %v", cp)
	}
	got := make(map[string]int)
	for _, k := range cp.MapKeys().Cells {
		c, ok := cp.MapGet(k).Native.(copierSeqCloner)
		if !ok {
			t.Fatalf("key %v: value is %T, want a copierSeqCloner clone", k, cp.MapGet(k).Native)
		}
		got[k.Str] = c.seq
	}
	if len(got) == 0 {
		t.Fatal("anti-vacuity: the copy reached no map values")
	}
	return got
}

// copierOrderRanger is a StringKeyRanger that yields its entries in a
// DIFFERENT order on every other call -- the interface's contract is
// "exactly once per entry, in unspecified order", so this is a conforming
// implementation, and the copier may not depend on the order it happens to
// get.
type copierOrderRanger struct {
	*copierStringMap
	calls int
}

func (m *copierOrderRanger) RangeStringKeys(fn func(key string, val *lisp.LVal)) error {
	keys := m.sortedKeys()
	m.calls++
	if m.calls%2 == 0 {
		slices.Reverse(keys)
	}
	for _, k := range keys {
		fn(k, m.m[k])
	}
	return nil
}

// copierOrderEntries is the same alternating implementation on the GENERIC
// arm: a custom Map with no StringKeyRanger, so the copier reaches it
// through Entries.  The Map interface (lisp/maps.go) documents Keys as
// returning "a sorted list" and says NOTHING about the order of Entries, so
// yielding a different permutation per call is conforming -- an embedder's
// map over a Go map would do it without trying -- and the copier may not
// depend on the order it happens to get.
type copierOrderEntries struct {
	*copierStringMap
	calls int
}

func (m *copierOrderEntries) Entries(buf []*lisp.LVal) *lisp.LVal {
	keys := m.sortedKeys()
	if len(buf) < len(keys) {
		return lisp.Errorf("buffer has insufficient length")
	}
	m.calls++
	if m.calls%2 == 0 {
		slices.Reverse(keys)
	}
	for i, k := range keys {
		buf[i] = lisp.QExpr([]*lisp.LVal{lisp.String(k), m.m[k]})
	}
	return lisp.Int(len(keys))
}

// TestCopyMapValueCloneOrderIsDeterministic: two copies of one sorted map
// assign the same clone to the same key.
//
// The copier walks a map's values to copy them, and c.copy can call a host
// hook per value -- NativeCloner.CloneNative, which the embedder writes and
// which may draw on state outside the value (a counter, an id allocator, an
// rng).  It walked a `sortedmap` with `for k, v := range m0.m`, Go map
// order, which is randomised per iteration, and the custom-ranger path in
// whatever order the ranger happened to yield.  So two copies of IDENTICAL
// input called the hook in different orders and the same key came back with
// different clones -- a determinism break on a value-copy primitive, which
// on a replicated execution model (a phylum runs on every endorsing peer)
// is a consensus hazard, not just a surprise.
//
// 64 keys and THREE copies, not two: Go randomises a map walk by picking a
// start position, so two walks of a small map coincide often enough to see
// (about one run in twenty at 32 keys and two copies -- measured, on the
// unfixed walker).  Three walks of a 64-key map agreeing by chance is rare
// enough that the unfixed code fails every run.  The ranger case needs
// neither: it yields in opposite orders by construction.
func TestCopyMapValueCloneOrderIsDeterministic(t *testing.T) {
	const n = 64
	assertStable := func(t *testing.T, m *lisp.LVal) {
		t.Helper()
		want := copierCloneAssignment(t, m)
		for i := range 2 {
			got := copierCloneAssignment(t, m)
			if !maps.Equal(want, got) {
				t.Errorf("copy %d assigned different clones to the same keys than copy 0:\n first: %v\n  this: %v",
					i+1, want, got)
			}
		}
	}
	t.Run("stock sorted map", func(t *testing.T) {
		m := lisp.SortedMap()
		for i := range n {
			if rc := m.MapSet(fmt.Sprintf("k%02d", i), lisp.Native(copierSeqCloner{})); rc.Type == lisp.LError {
				t.Fatalf("set: %v", rc)
			}
		}
		assertStable(t, m)
	})
	t.Run("string-key ranger", func(t *testing.T) {
		kv := make(map[string]*lisp.LVal, n)
		for i := range n {
			kv[fmt.Sprintf("k%02d", i)] = lisp.Native(copierSeqCloner{})
		}
		assertStable(t, lisp.SortedMapFromData(lisp.NewMapData(&copierOrderRanger{copierStringMap: newCopierStringMap(kv)})))
	})
	// The generic Entries arm.  0d3ece9 left it alone on the claim that
	// "Entries is sorted by contract"; the Map interface makes no such
	// promise -- only Keys does -- so a conforming embedder map reached
	// through this arm called the host's clone hook in whatever order it
	// yielded, and two copies of one map disagreed.
	t.Run("custom entries order", func(t *testing.T) {
		kv := make(map[string]*lisp.LVal, n)
		for i := range n {
			kv[fmt.Sprintf("k%02d", i)] = lisp.Native(copierSeqCloner{})
		}
		assertStable(t, lisp.SortedMapFromData(lisp.NewMapData(&copierOrderEntries{copierStringMap: newCopierStringMap(kv)})))
	})
}

// copierNestedCloneAssignment is copierCloneAssignment for a map whose
// values are one-element LISTS holding the cloner rather than the cloner
// itself: the shape the leaf fast path must NOT take, since the hook still
// runs, one level down.
func copierNestedCloneAssignment(t *testing.T, m *lisp.LVal) map[string]int {
	t.Helper()
	copierCloneSeq = 0
	cp := m.Copy()
	if cp.Type == lisp.LError {
		t.Fatalf("copy: %v", cp)
	}
	got := make(map[string]int)
	for _, k := range cp.MapKeys().Cells {
		v := cp.MapGet(k)
		if len(v.Cells) == 0 {
			continue // one of the scalar values
		}
		c, ok := v.Cells[0].Native.(copierSeqCloner)
		if !ok {
			t.Fatalf("key %v: nested value is %T, want a copierSeqCloner clone", k, v.Cells[0].Native)
		}
		got[k.Str] = c.seq
	}
	if len(got) < 2 {
		t.Fatalf("anti-vacuity: the copy reached %d nested cloners; with fewer than two the assignment"+
			" is ordered by construction and this control proves nothing", len(got))
	}
	return got
}

// TestCopyMapWithANestedNativeStillCopiesInKeyOrder is the negative control
// for the leaf fast path.
//
// The sort exists only because c.copy may call a host hook, so a map whose
// values can run no host code may skip it -- and the predicate for that is
// exactly the one copyNode uses to decide what to memoise: no cell storage
// and no payload.  The hazard is weakening it to something cheaper-looking,
// "the value is not itself a native", which is wrong: a LIST value carries
// a whole subtree, and a NativeCloner several levels down is reached by the
// same walk in the same order.
//
// So: a map of scalars with two nested one-element lists, each holding a
// cloner, over all three arms.  The scalars make the fast path's scan look
// satisfied right up to the two values that are not leaves, and the two
// cloners make the order observable -- with one of them the assignment
// would be the same however the map was walked, which is why
// copierNestedCloneAssignment refuses to run on fewer.
func TestCopyMapWithANestedNativeStillCopiesInKeyOrder(t *testing.T) {
	const n = 64
	nested := func() *lisp.LVal {
		return lisp.QExpr([]*lisp.LVal{lisp.Native(copierSeqCloner{})})
	}
	assertStable := func(t *testing.T, m *lisp.LVal) {
		t.Helper()
		want := copierNestedCloneAssignment(t, m)
		for i := range 2 {
			got := copierNestedCloneAssignment(t, m)
			if !maps.Equal(want, got) {
				t.Errorf("copy %d assigned different clones to the same keys than copy 0:\n first: %v\n  this: %v",
					i+1, want, got)
			}
		}
	}
	kv := func() map[string]*lisp.LVal {
		out := make(map[string]*lisp.LVal, n+2)
		for i := range n {
			out[fmt.Sprintf("k%02d", i)] = lisp.String(fmt.Sprintf("v%02d", i))
		}
		out["n0"], out["n1"] = nested(), nested()
		return out
	}
	t.Run("stock sorted map", func(t *testing.T) {
		m := lisp.SortedMap()
		for k, v := range kv() {
			if rc := m.MapSet(k, v); rc.Type == lisp.LError {
				t.Fatalf("set: %v", rc)
			}
		}
		assertStable(t, m)
	})
	t.Run("string-key ranger", func(t *testing.T) {
		assertStable(t, lisp.SortedMapFromData(lisp.NewMapData(&copierOrderRanger{copierStringMap: newCopierStringMap(kv())})))
	})
	t.Run("custom entries order", func(t *testing.T) {
		assertStable(t, lisp.SortedMapFromData(lisp.NewMapData(&copierOrderEntries{copierStringMap: newCopierStringMap(kv())})))
	})
}
