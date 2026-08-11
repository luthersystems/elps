// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"strings"
	"testing"
	"unsafe"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// These tests prove the evaluator's AST→value seals (lisp/astseal.go): a
// cached, shared parse tree evaluated by a runtime must not become reachable
// from that runtime's value domain, and mutating any value the runtime
// produced must not corrupt the cached tree.
//
// The setup simulates substrate's parse cache: one program is parsed once
// and the resulting []*LVal is retained (the "cache") while an environment
// evaluates it.  Without the seals the value domain aliases the cache —
// TestEvalDoesNotLeakCachedAST fails on the unsealed tree with dozens of
// shared container nodes, and TestValueMutationDoesNotCorruptCachedAST shows
// Cells surgery on a returned value rewriting the cached tree in place.
//
// Sharing policy asserted here (must match astseal.go):
//   - Container nodes (LSExpr, LQuote — anything owning Cells storage) and
//     Cells backing arrays: ZERO sharing, unconditionally.  Containers are
//     the corruptible class — a buggy builtin mutates a list via its Cells.
//   - Leaf atoms (symbols, keywords, strings, numbers): sharing tolerated
//     while copyAtomsAtLeakPoints is false (the measured decision).  Atoms
//     have no Cells; corrupting one requires a direct Go field write, the
//     class covered by the elpsvet freshness rule.  The test still asserts
//     every shared node IS a leaf atom, so flipping the decision to full
//     copying only ever shrinks the shared set.

// leakProgram exercises every sealed leak point through one cached tree:
// quote evaluation (a), quasiquote literal skeletons (b), macro arguments
// captured during expansion (c), lambda/defun bodies embedding code, and
// self-evaluating atoms/keywords.
const leakProgram = `
(set 'g-quoted-list '(alpha beta (gamma 1 2.5 "deep") "lit" 42 :kw))
(set 'g-quoted-sym 'plain-symbol)
(set 'g-string "string-literal")
(set 'g-int 1337)
(set 'g-keyword :kwlit)
(set 'g-qq (quasiquote (skel (nested lit "qs") (unquote (+ 1 2)) (unquote-splicing (list 3 4)))))
(defun leak-quoter () '(from defun (nested body lit)))
(set 'g-from-fun (leak-quoter))
(defmacro leak-mac (x) (set 'g-macro-leak (list x x)) (quasiquote (quote (unquote x))))
(set 'g-macro-val (leak-mac (payload (deep list) "mstr" 7)))
(set 'g-lambda (lambda (a b) (cons a '(in lambda lit))))
(set 'g-lambda-res (funcall g-lambda 'x 'y))
'(top level trailing quoted (chunk))
`

// parseCached parses src once, standing in for a substrate-style parse
// cache whose []*LVal outlives — and is shared by — many runtimes.
func parseCached(t testing.TB, src string) []*lisp.LVal {
	t.Helper()
	p := parser.NewReader()
	exprs, err := p.Read("cached-ast", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return exprs
}

func newLeakTestEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	return env
}

// ptrSet records the identity of every *LVal in a graph plus the identity of
// every non-empty Cells backing array (a value can share a backing array
// with the cache without sharing the container header — Quote's shallow
// copy is exactly that shape — so headers alone would under-count).
type ptrSet struct {
	nodes map[*lisp.LVal]bool
	cells map[**lisp.LVal]bool // key: &cells[0] — backing array identity
}

func newPtrSet() *ptrSet {
	return &ptrSet{nodes: make(map[*lisp.LVal]bool), cells: make(map[**lisp.LVal]bool)}
}

func (s *ptrSet) addCellsBacking(v *lisp.LVal) {
	if len(v.Cells) > 0 {
		s.cells[unsafe.SliceData(v.Cells)] = true //nolint:gosec // identity probe only; the pointer is never dereferenced
	}
}

// walkAST records every node of cached parser output.  Parser output is a
// strict tree of Cells; the seen-set guards against surprises anyway.
func walkAST(v *lisp.LVal, s *ptrSet) {
	if v == nil || s.nodes[v] {
		return
	}
	s.nodes[v] = true
	s.addCellsBacking(v)
	for _, c := range v.Cells {
		walkAST(c, s)
	}
}

const leakWalkDepth = 64

// walkValues records everything reachable from the value domain: Cells of
// every value INCLUDING function objects (their formals/body are reachable
// from the value domain and must be sealed), closure scope bindings, and
// sorted-map entries.
func walkValues(v *lisp.LVal, depth int, s *ptrSet, stats *walkStats) {
	if v == nil || depth <= 0 || s.nodes[v] {
		return
	}
	s.nodes[v] = true
	s.addCellsBacking(v)
	switch v.Type {
	case lisp.LFun:
		stats.funs++
		for _, c := range v.Cells {
			walkValues(c, depth-1, s, stats)
		}
		if fenv := v.Env(); fenv != nil {
			for _, sv := range fenv.Scope {
				walkValues(sv, depth-1, s, stats)
			}
		}
	case lisp.LSortMap:
		md := v.Map()
		if md == nil {
			return
		}
		buf := make([]*lisp.LVal, md.Len())
		if lerr := md.Entries(buf); lerr.Type == lisp.LError {
			return
		}
		for _, pair := range buf {
			walkValues(pair, depth-1, s, stats)
		}
	default:
		for _, c := range v.Cells {
			walkValues(c, depth-1, s, stats)
		}
	}
}

func isLeafAtom(v *lisp.LVal) bool {
	switch v.Type { //nolint:exhaustive // atoms are exactly these types; everything else is not a leaf
	case lisp.LSymbol, lisp.LQSymbol, lisp.LString, lisp.LInt, lisp.LFloat:
		return len(v.Cells) == 0
	}
	return false
}

func TestEvalDoesNotLeakCachedAST(t *testing.T) {
	exprs := parseCached(t, leakProgram)

	astSet := newPtrSet()
	for _, e := range exprs {
		walkAST(e, astSet)
	}
	if len(astSet.nodes) < 60 {
		t.Fatalf("anti-vacuity: cached AST walk saw only %d nodes; the program or walk is broken", len(astSet.nodes))
	}

	env := newLeakTestEnv(t)
	valSet := newPtrSet()
	stats := &walkStats{}
	// Values returned during evaluation are part of the value domain even if
	// nothing binds them (the trailing quoted literal covers that path).
	for i, e := range exprs {
		r := env.Eval(e)
		if r.Type == lisp.LError {
			t.Fatalf("eval expr %d: %v", i, r)
		}
		walkValues(r, leakWalkDepth, valSet, stats)
	}
	// Everything durable: every symbol of every package in the registry.
	// The registry surface is sealed (exp-seal-registry); Package.Symbol
	// performs a raw table lookup returning the stored *LVal, so pointer
	// identity — which the leak walk depends on — is preserved.
	for _, name := range env.Runtime.Registry.PackageNames() {
		pkg := env.Runtime.Registry.Package(name)
		for _, sym := range pkg.SymbolNames() {
			if v, ok := pkg.Symbol(sym); ok {
				walkValues(v, leakWalkDepth, valSet, stats)
			}
		}
	}
	if stats.funs < 2 {
		t.Fatalf("anti-vacuity: value walk saw only %d LFun values (want >= 2: leak-quoter and g-lambda at minimum)", stats.funs)
	}
	// Anti-vacuity: the walk must have reached the stored quoted list.
	if g := env.Get(lisp.Symbol("g-quoted-list")); g.Type == lisp.LError || !valSet.nodes[g] {
		t.Fatalf("anti-vacuity: value walk did not reach g-quoted-list (%v)", g)
	}

	sharedContainers := 0
	sharedAtoms := 0
	for p := range valSet.nodes {
		if !astSet.nodes[p] {
			continue
		}
		if isLeafAtom(p) {
			sharedAtoms++
			continue
		}
		sharedContainers++
		if sharedContainers <= 20 {
			t.Errorf("value domain aliases cached AST container: %s", describeLVal(p))
		}
	}
	sharedBacking := 0
	for c := range valSet.cells {
		if astSet.cells[c] {
			sharedBacking++
		}
	}
	if sharedContainers > 0 || sharedBacking > 0 {
		t.Fatalf("cached AST leaked into the value domain: %d shared container nodes, %d shared Cells backing arrays (%d cached nodes, %d value nodes; %d leaf atoms shared under the documented atom policy)",
			sharedContainers, sharedBacking, len(astSet.nodes), len(valSet.nodes), sharedAtoms)
	}
	t.Logf("zero container/backing sharing; %d leaf atoms shared (allowed while copyAtomsAtLeakPoints=false); cached=%d nodes, value=%d nodes",
		sharedAtoms, len(astSet.nodes), len(valSet.nodes))
}

// fingerprintAST computes a structural digest of the cached tree: type,
// flags, scalar payloads, and Cells shape of every node in traversal order.
// Any in-place corruption of the tree changes the digest.
func fingerprintAST(exprs []*lisp.LVal) string {
	h := sha256.New()
	seen := make(map[*lisp.LVal]int)
	var walk func(v *lisp.LVal)
	walk = func(v *lisp.LVal) {
		if v == nil {
			_, _ = fmt.Fprint(h, "<nil>")
			return
		}
		if id, ok := seen[v]; ok {
			_, _ = fmt.Fprintf(h, "@%d", id)
			return
		}
		seen[v] = len(seen)
		_, _ = fmt.Fprintf(h, "(t=%d q=%v sp=%v s=%q i=%d f=%g n=%d", v.Type, v.Quoted, v.Spliced, v.Str, v.Int, v.Float, len(v.Cells))
		for _, c := range v.Cells {
			walk(c)
		}
		_, _ = fmt.Fprint(h, ")")
	}
	for _, e := range exprs {
		walk(e)
	}
	return hex.EncodeToString(h.Sum(nil))
}

// corruptValue performs the Cells surgery a buggy future builtin could:
// overwriting element pointers, growing the list, and reordering children —
// on every container reachable from v.  It deliberately does NOT write
// scalar fields of leaf atoms: that is the vet-covered class outside the
// container seal (see the sharing policy at the top of this file).
func corruptValue(v *lisp.LVal, depth int, seen map[*lisp.LVal]bool) {
	if v == nil || depth <= 0 || seen[v] || v.Type == lisp.LFun {
		return
	}
	seen[v] = true
	children := append([]*lisp.LVal(nil), v.Cells...)
	if len(v.Cells) > 0 {
		v.Cells[0] = lisp.Symbol("CORRUPTED")
		v.Cells = append(v.Cells, lisp.Int(0xdead))
		for i, j := 0, len(v.Cells)-1; i < j; i, j = i+1, j-1 {
			v.Cells[i], v.Cells[j] = v.Cells[j], v.Cells[i]
		}
		v.Quoted = !v.Quoted
	}
	for _, c := range children {
		corruptValue(c, depth-1, seen)
	}
}

func TestValueMutationDoesNotCorruptCachedAST(t *testing.T) {
	exprs := parseCached(t, leakProgram)
	before := fingerprintAST(exprs)

	env := newLeakTestEnv(t)
	var results []*lisp.LVal
	for i, e := range exprs {
		r := env.Eval(e)
		if r.Type == lisp.LError {
			t.Fatalf("eval expr %d: %v", i, r)
		}
		results = append(results, r)
	}

	// Simulate the buggy builtin: corrupt every value the program returned
	// and every value it stored.
	seen := make(map[*lisp.LVal]bool)
	for _, r := range results {
		corruptValue(r, leakWalkDepth, seen)
	}
	for _, name := range []string{"g-quoted-list", "g-qq", "g-from-fun", "g-macro-leak", "g-macro-val", "g-lambda-res"} {
		g := env.Get(lisp.Symbol(name))
		if g.Type == lisp.LError {
			t.Fatalf("missing global %s: %v", name, g)
		}
		corruptValue(g, leakWalkDepth, seen)
	}
	if len(seen) < 10 {
		t.Fatalf("anti-vacuity: corruption pass touched only %d values", len(seen))
	}

	after := fingerprintAST(exprs)
	if before != after {
		t.Fatalf("mutating evaluator-returned values corrupted the cached AST:\n  before %s\n  after  %s", before, after)
	}
}

// TestSubstrate378ClassKilled reproduces, ELPS-side, the exact shape of
// luthersystems/substrate#378: a program is parsed once into a shared cache;
// runtime 1 evaluates it and native code then mutates the returned value's
// backing storage in place (substrate's elpspath did exactly this to the
// list bound to q).  Without the evaluator seals (exp-ast-leakpoints) that
// surgery rewrote the cached tree, so every later runtime evaluating the
// same cache saw the corrupted program.  With the seals merged the cache
// fingerprint is unchanged and a second runtime sees pristine data.
// TestValueMutationDoesNotCorruptCachedAST covers the general class; this
// test pins the #378 reproduction specifically.
func TestSubstrate378ClassKilled(t *testing.T) {
	const src = `
(set 'q '(10 20 30))
q
`
	exprs := parseCached(t, src)
	before := fingerprintAST(exprs)

	// Runtime 1: evaluate the cached program and take q's returned value.
	env1 := newLeakTestEnv(t)
	var q1 *lisp.LVal
	for i, e := range exprs {
		r := env1.Eval(e)
		if r.Type == lisp.LError {
			t.Fatalf("env1 eval expr %d: %v", i, r)
		}
		q1 = r
	}
	if q1 == nil || len(q1.Cells) != 3 {
		t.Fatalf("anti-vacuity: expected q = (10 20 30), got %v", q1)
	}
	// Native-code surgery on the value's backing storage, elpspath-style:
	// overwrite an element, grow the list, reorder the remainder.
	q1.Cells[0] = lisp.Int(999)
	q1.Cells = append(q1.Cells, lisp.String("injected"))
	q1.Cells[1], q1.Cells[2] = q1.Cells[2], q1.Cells[1]
	// Corrupt through the durable binding too, not just the returned value.
	if g := env1.Get(lisp.Symbol("q")); g.Type != lisp.LError {
		corruptValue(g, leakWalkDepth, map[*lisp.LVal]bool{})
	}

	// The cached AST must be exactly what it was before runtime 1 ran.
	if after := fingerprintAST(exprs); after != before {
		t.Fatalf("substrate#378 shape reproduced: value surgery in runtime 1 corrupted the shared parse cache\n  before %s\n  after  %s", before, after)
	}

	// Runtime 2 evaluates the same cached exprs and must see pristine data.
	// Under elpscheck the ownership checker forbids this sharing pattern by
	// design (one tree, two Runtimes), so the replay only runs in the
	// production configuration — the one substrate's cache actually uses.
	if elpscheckActive {
		t.Log("elpscheck build: skipping the second-runtime replay (ownership checker forbids cross-runtime AST sharing by design); fingerprint assertion above still ran")
		return
	}
	env2 := newLeakTestEnv(t)
	var q2 *lisp.LVal
	for i, e := range exprs {
		r := env2.Eval(e)
		if r.Type == lisp.LError {
			t.Fatalf("env2 eval expr %d: %v", i, r)
		}
		q2 = r
	}
	want := []int{10, 20, 30}
	if len(q2.Cells) != len(want) {
		t.Fatalf("runtime 2 saw corrupted q: %v", q2)
	}
	for i, n := range want {
		if q2.Cells[i].Type != lisp.LInt || q2.Cells[i].Int != n {
			t.Fatalf("runtime 2 saw corrupted q[%d]: %v (want %d)", i, q2.Cells[i], n)
		}
	}
}
