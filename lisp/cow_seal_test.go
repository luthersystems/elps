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

// These tests prove the sealing design (lisp/seal.go): a cached, shared
// parse tree evaluated by a runtime is handed to the value domain by
// reference — no eager copies — and every kernel mutation path that could
// write the shared tree either refuses with the catchable
// modify-literal-error condition (issue #378; the guarded sites used to
// copy-on-write silently) or leaves the tree untouched.  The property under
// test is therefore NOT pointer disjointness (aliasing is the design) but
// corruption-freedom: after any contract-respecting mutation activity in
// one runtime, the cached tree is bit-identical and a second runtime
// evaluating the same cache sees pristine data.
//
// The setup simulates substrate's parse cache: one program is parsed once
// and the resulting []*LVal is retained (the "cache") while environments
// evaluate it.

// sizeProgram pins the layout claim in lisp/seal.go: the sealed flag lives
// in an existing padding byte and LVal does not grow.
func TestLValSizeUnchanged(t *testing.T) {
	const wantSize = 112 // bytes on 64-bit platforms, unchanged by `sealed`
	if s := unsafe.Sizeof(lisp.LVal{}); s != wantSize {
		t.Fatalf("LVal is %d bytes (want %d): the sealed flag must not grow the struct", s, wantSize)
	}
}

// cowProgram exercises every former leak point through one cached tree:
// quote evaluation, quasiquote literal skeletons, macro arguments captured
// during expansion, lambda/defun bodies embedding code, and
// self-evaluating atoms/keywords.
const cowProgram = `
(set 'g-quoted-list '(alpha beta (gamma 1 2.5 "deep") "lit" 42 :kw))
(set 'g-quoted-sym 'plain-symbol)
(set 'g-string "string-literal")
(set 'g-int 1337)
(set 'g-keyword :kwlit)
(set 'g-qq (quasiquote (skel (nested lit "qs") (unquote (+ 1 2)) (unquote-splicing (list 3 4)))))
(defun leak-quoter () '(from defun (nested body lit)))
(set 'g-from-fun (leak-quoter))
(set 'g-nums '(9 3 7 1 8))
(defun num-quoter () '(6 2 4))
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

func newCowTestEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", rc)
	}
	return env
}

// walkAST visits every node of cached parser output.  Parser output is a
// strict tree of Cells; the seen-set guards against surprises anyway.
func walkAST(v *lisp.LVal, seen map[*lisp.LVal]bool, visit func(*lisp.LVal)) {
	if v == nil || seen[v] {
		return
	}
	seen[v] = true
	visit(v)
	for _, c := range v.Cells {
		walkAST(c, seen, visit)
	}
}

// TestParserSealsOutput asserts the set-point: every node of every parsed
// top-level expression is sealed, before any evaluation happens.
func TestParserSealsOutput(t *testing.T) {
	exprs := parseCached(t, cowProgram)
	seen := make(map[*lisp.LVal]bool)
	nodes, sealed := 0, 0
	for _, e := range exprs {
		walkAST(e, seen, func(v *lisp.LVal) {
			nodes++
			if v.IsSealed() {
				sealed++
			} else {
				t.Errorf("unsealed parser node: type=%v str=%q", v.Type, v.Str)
			}
		})
	}
	if nodes < 60 {
		t.Fatalf("anti-vacuity: cached AST walk saw only %d nodes; the program or walk is broken", nodes)
	}
	if sealed != nodes {
		t.Fatalf("parser output not fully sealed: %d of %d nodes sealed", sealed, nodes)
	}
}

// TestRuntimeValuesUnsealed asserts the other half of the set-point:
// runtime-constructed values do not carry the flag, so the copy-on-write
// guards can never trigger on them.
func TestRuntimeValuesUnsealed(t *testing.T) {
	env := newCowTestEnv(t)
	for _, src := range []string{
		`(list 1 2 3)`,
		`(cons 'a (list 'b))`, // cons of a sealed symbol into a fresh list
		`(vector 1 2 3)`,
		`(sorted-map "k" 1)`,
		`"runtime-string"`, // sealed atom returned as-is IS sealed; see below
	} {
		v := env.LoadString("test.lisp", src)
		if v.Type == lisp.LError {
			t.Fatalf("eval %q: %v", src, v)
		}
		switch src {
		case `"runtime-string"`:
			// A literal evaluates to the sealed parse node itself — that is
			// the whole point of the design (no copies).
			if !v.IsSealed() {
				t.Errorf("literal %q evaluated to an unsealed value; expected the sealed parse node", src)
			}
		default:
			if v.IsSealed() {
				t.Errorf("runtime-constructed value from %q is sealed", src)
			}
		}
	}
}

// TestSealPropagation asserts that kernel operations returning a header
// over a sealed value's backing array carry the flag, and that the two
// sanctioned copy operations clear it.
func TestSealPropagation(t *testing.T) {
	env := newCowTestEnv(t)
	if v := env.LoadString("test.lisp", `(set 'q '(10 20 30 40))`); v.Type == lisp.LError {
		t.Fatalf("setup: %v", v)
	}
	for _, tc := range []struct {
		src  string
		seal bool
	}{
		{`q`, true},
		{`(cdr q)`, true},              // shares q's backing array
		{`(rest q)`, true},             // shares q's backing array
		{`(slice 'list q 0 2)`, true},  // shares q's backing array (with spare capacity)
		{`(slice 'list q 1 4)`, true},  // shares q's backing array
		{`(car q)`, true},              // the sealed element itself
		{`(append 'list q 50)`, false}, // fresh backing by construction
		{`(reverse 'list q)`, false},   // fresh backing
		{`(map 'list (lambda (x) x) q)`, false},
		{`(slice 'vector (copy q) 0 2)`, false}, // copy clears the seal; the vector owns fresh backing
		{`(append 'vector (copy q) 50)`, false}, // ditto
		{`(stable-sort > (copy q))`, false},     // ditto: in-place sort of the mutable copy
	} {
		v := env.LoadString("test.lisp", tc.src)
		if v.Type == lisp.LError {
			t.Fatalf("eval %q: %v", tc.src, v)
		}
		if got := v.IsSealed(); got != tc.seal {
			t.Errorf("%s: IsSealed() = %v, want %v", tc.src, got, tc.seal)
		}
	}

	// The three guarded mutators refuse the sealed value itself: raising
	// the catchable modify-literal-error condition replaced the silent
	// copy-on-write these sites used to perform (issue #378).
	for _, src := range []string{
		`(slice 'vector q 0 2)`,
		`(append 'vector q 50)`,
		`(stable-sort > q)`,
	} {
		v := env.LoadString("test.lisp", src)
		if v.Type != lisp.LError {
			t.Fatalf("eval %q: expected modify-literal-error, got %v", src, v)
		}
		if v.Str != lisp.CondModifyLiteral {
			t.Errorf("%s: condition = %q, want %q", src, v.Str, lisp.CondModifyLiteral)
		}
	}

	q := env.Get(lisp.Symbol("q"))
	if !q.IsSealed() {
		t.Fatalf("anti-vacuity: q is not sealed")
	}
	if cp := q.Copy(); cp.IsSealed() {
		t.Errorf("Copy() of a sealed list is still sealed; copy-on-write depends on the fresh storage being mutable")
	} else if len(cp.Cells) != 4 || cp.Cells[0].IsSealed() {
		// Copy is deep on Cells (copyCells calls Copy per element), so the
		// entire copied tree is fresh and unsealed — Copy-then-mutate works
		// at any depth.
		t.Errorf("Copy() left a sealed node in the copied tree: %v", cp)
	}
	dt, err := lisp.Detach(q)
	if err != nil {
		t.Fatalf("detach: %v", err)
	}
	seen := make(map[*lisp.LVal]bool)
	walkAST(dt, seen, func(v *lisp.LVal) {
		if v.IsSealed() {
			t.Errorf("detach left a sealed node in the hermetic copy: %v", v)
		}
	})
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
		_, _ = fmt.Fprintf(h, "(t=%d q=%v sp=%v s=%q i=%d f=%g n=%d", v.Type, v.IsQuoted(), lisp.SplicedFlag(v), v.Str, v.Int, v.Float, len(v.Cells))
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

// TestValueMutationDoesNotCorruptCachedAST is the CoW adaptation of the
// eager-copy design's test of the same name.  There, arbitrary Go-level
// Cells surgery on returned values was harmless because returned values
// were copies.  Here returned values ARE the shared tree, so the test
// exercises the mutation surface the design actually guards:
//
//   - every lisp-reachable mutator that can touch a list (stable-sort's
//     in-place sort, append 'vector's spare-capacity write, reached both
//     directly and through backing-sharing views from cdr/rest/slice), and
//   - the sanctioned Go-embedder pattern for values that report IsSealed:
//     copy first, mutate the copy (the libelpspath contract).
//
// After all of it, the cached tree must be bit-identical.
func TestValueMutationDoesNotCorruptCachedAST(t *testing.T) {
	exprs := parseCached(t, cowProgram)
	before := fingerprintAST(exprs)

	env := newCowTestEnv(t)
	var results []*lisp.LVal
	for i, e := range exprs {
		r := env.Eval(e)
		if r.Type == lisp.LError {
			t.Fatalf("eval expr %d: %v", i, r)
		}
		results = append(results, r)
	}

	// Lisp-reachable mutation attempts against the sealed values the
	// program stored.  Every one of these rewrote (or could rewrite) the
	// shared tree before the seal guards existed; every one is now refused
	// with the catchable modify-literal-error condition (issue #378 — the
	// guards used to copy-on-write silently instead), and the tree stays
	// bit-identical either way, which is the property this test exists for.
	for _, src := range []string{
		`(stable-sort > g-nums)`,
		`(stable-sort < (cdr g-nums))`,
		`(stable-sort > (slice 'list g-nums 1 4))`,
		`(append 'vector (slice 'list g-quoted-list 0 1) 999)`,
		`(append 'vector (cdr g-quoted-list) 999)`,
		`(append 'vector (rest g-from-fun) 999)`,
		`(append! (slice 'vector g-quoted-list 0 2) 999)`,
		`(stable-sort > (num-quoter))`,
		`(stable-sort > (cdr (num-quoter)))`,
	} {
		v := env.LoadString("mutate.lisp", src)
		if v.Type != lisp.LError {
			t.Fatalf("mutation attempt %q was not refused: %v", src, v)
		}
		if v.Str != lisp.CondModifyLiteral {
			t.Errorf("mutation attempt %q: condition = %q, want %q", src, v.Str, lisp.CondModifyLiteral)
		}
	}

	// The Go-embedder pattern: a value that reports IsSealed must be
	// copied before surgery; the surgery then lands on the private copy.
	// (An unsealed value is runtime storage and may be mutated in place —
	// which is also what an unguarded embedder does to EVERY value when
	// sealing is disabled, so this pass corrupts the cache and fails the
	// fingerprint below if the flag ever stops arriving.)
	mutated := 0
	for _, r := range results {
		if len(r.Cells) == 0 {
			continue
		}
		target := r
		if target.IsSealed() {
			target = target.Copy()
		}
		target.Cells[0] = lisp.Symbol("CORRUPTED")
		target.Cells = append(target.Cells, lisp.Int(0xdead))
		mutated++
	}
	if mutated < 2 {
		t.Fatalf("anti-vacuity: embedder-pattern pass mutated only %d values", mutated)
	}

	after := fingerprintAST(exprs)
	if before != after {
		t.Fatalf("mutation through the guarded surface corrupted the cached AST:\n  before %s\n  after  %s", before, after)
	}
}

// TestSubstrate378ClassKilled reproduces, ELPS-side, the shape of
// luthersystems/substrate#378: a program is parsed once into a shared
// cache; runtime 1 evaluates it and then mutates the value bound to q.
// Under the copy-on-write design the value IS the shared tree node, so the
// mutation is either refused by a guarded lisp-level mutator (the
// catchable modify-literal-error condition) or happens through the
// sanctioned embedder pattern (check IsSealed, copy first — the elpspath
// contract).  Either way the cache fingerprint is unchanged and a second
// runtime sees pristine data.
func TestSubstrate378ClassKilled(t *testing.T) {
	const src = `
(set 'q '(10 20 30))
q
`
	exprs := parseCached(t, src)
	before := fingerprintAST(exprs)

	// Runtime 1: evaluate the cached program and take q's returned value.
	env1 := newCowTestEnv(t)
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

	// The flag is the contract: the evaluator handed us a shared tree node
	// and says so.  Without sealing this reports false and the surgery
	// below lands on the cache (that IS #378) — the fingerprint assertion
	// catches it.
	target := q1
	if target.IsSealed() {
		target = target.Copy()
	}
	// Native-code surgery, elpspath-style: overwrite an element, grow the
	// list, reorder the remainder — on the private copy.
	target.Cells[0] = lisp.Int(999)
	target.Cells = append(target.Cells, lisp.String("injected"))
	target.Cells[1], target.Cells[2] = target.Cells[2], target.Cells[1]

	// Lisp-level mutators driven at the durable binding are refused with
	// the catchable modify-literal-error condition (issue #378; they used
	// to copy-on-write silently), leaving the cache untouched.
	for _, msrc := range []string{
		`(stable-sort > q)`,
		`(append 'vector (slice 'list q 0 1) 999)`,
	} {
		v := env1.LoadString("mutate.lisp", msrc)
		if v.Type != lisp.LError {
			t.Fatalf("env1 %q was not refused: %v", msrc, v)
		}
		if v.Str != lisp.CondModifyLiteral {
			t.Errorf("env1 %q: condition = %q, want %q", msrc, v.Str, lisp.CondModifyLiteral)
		}
	}

	// The cached AST must be exactly what it was before runtime 1 ran.
	if after := fingerprintAST(exprs); after != before {
		t.Fatalf("substrate#378 shape reproduced: value mutation in runtime 1 corrupted the shared parse cache\n  before %s\n  after  %s", before, after)
	}

	// Runtime 2 evaluates the same cached exprs and must see pristine data.
	// This replay now runs in checked builds too: the ownership checker
	// exempts sealed nodes (cross-runtime sharing of sealed trees is the
	// sanctioned design — see the Allowlist section of
	// lisp/ownership_check_elpscheck.go), so the elpscheck configuration
	// exercises the exact sharing pattern substrate's cache uses instead
	// of skipping it.
	env2 := newCowTestEnv(t)
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

// TestStableSortLiteralPristine pins the stable-sort finding: before the
// seal guard, (stable-sort > q) on a quoted literal sorted the literal's
// cells in place, so the SECOND evaluation of the defun body saw an
// already-sorted literal — observable in a single environment, and cache
// corruption across environments.  The guard now refuses the sort with the
// catchable modify-literal-error condition (issue #378; it used to
// copy-on-write silently), and the literal stays pristine on every call.
func TestStableSortLiteralPristine(t *testing.T) {
	env := newCowTestEnv(t)
	setup := `(defun s () (let ([q '(1 2 3)]) (ignore-errors (stable-sort > q)) q))`
	if v := env.LoadString("test.lisp", setup); v.Type == lisp.LError {
		t.Fatalf("setup: %v", v)
	}
	for call := 1; call <= 2; call++ {
		v := env.LoadString("test.lisp", `(s)`)
		if v.Type == lisp.LError {
			t.Fatalf("call %d: %v", call, v)
		}
		want := []int{1, 2, 3}
		if len(v.Cells) != len(want) {
			t.Fatalf("call %d: q = %v, want (1 2 3)", call, v)
		}
		for i, n := range want {
			if v.Cells[i].Type != lisp.LInt || v.Cells[i].Int != n {
				t.Fatalf("call %d: literal was mutated in place: q = %v, want (1 2 3)", call, v)
			}
		}
	}
	// The refusal is the named condition, not a generic error.
	v := env.LoadString("test.lisp", `(stable-sort > '(1 2 3))`)
	if v.Type != lisp.LError {
		t.Fatalf("sort of a literal was not refused: %v", v)
	}
	if v.Str != lisp.CondModifyLiteral {
		t.Fatalf("sort of a literal raised %q, want %q", v.Str, lisp.CondModifyLiteral)
	}
	// The functional contract survives through copy — the sanctioned
	// remedy the error message names.
	v = env.LoadString("test.lisp", `(stable-sort > (copy '(1 2 3)))`)
	if v.Type == lisp.LError {
		t.Fatalf("sort of a copy: %v", v)
	}
	if len(v.Cells) != 3 || v.Cells[0].Int != 3 || v.Cells[1].Int != 2 || v.Cells[2].Int != 1 {
		t.Fatalf("stable-sort return value wrong: %v, want (3 2 1)", v)
	}
}

// TestAppendVectorSliceBackingPristine pins the append finding: before the
// guards, (append 'vector (slice 'list lit 0 1) 99) appended into the
// literal's spare backing capacity, overwriting the literal's second
// element ((1 2 3) became (1 99 3)) — observable in a single environment,
// and cache corruption across environments.  The guard now refuses the
// append with the catchable modify-literal-error condition (issue #378; it
// used to copy-on-write silently), and the literal stays pristine.
func TestAppendVectorSliceBackingPristine(t *testing.T) {
	env := newCowTestEnv(t)
	for i, src := range []string{
		`(set 'lit '(1 2 3))`,
		`(set 's1 (slice 'list lit 0 1))`,
	} {
		if v := env.LoadString("test.lisp", src); v.Type == lisp.LError {
			t.Fatalf("expr %d %q: %v", i, src, v)
		}
	}
	refused := env.LoadString("test.lisp", `(append 'vector s1 99)`)
	if refused.Type != lisp.LError {
		t.Fatalf("(append 'vector s1 99) was not refused: %v", refused)
	}
	if refused.Str != lisp.CondModifyLiteral {
		t.Errorf("(append 'vector s1 99): condition = %q, want %q", refused.Str, lisp.CondModifyLiteral)
	}
	v := env.LoadString("test.lisp", `lit`)
	if v.Type == lisp.LError {
		t.Fatalf("lit: %v", v)
	}
	want := []int{1, 2, 3}
	if len(v.Cells) != len(want) {
		t.Fatalf("lit = %v, want (1 2 3)", v)
	}
	for i, n := range want {
		if v.Cells[i].Type != lisp.LInt || v.Cells[i].Int != n {
			t.Fatalf("literal backing was written through the slice: lit = %v, want (1 2 3)", v)
		}
	}
}

// TestSealedSetSourceFrozen pins the metadata freeze: SetSource on a
// sealed node is a no-op, so embedder code cannot restamp shared parse
// metadata (and stampMacroExpansion's guard has the same effect inside the
// kernel).
func TestSealedSetSourceFrozen(t *testing.T) {
	exprs := parseCached(t, `(a b c)`)
	node := exprs[0]
	if !node.IsSealed() {
		t.Fatalf("anti-vacuity: parse output not sealed")
	}
	locBefore, okBefore := node.Source()
	if !okBefore {
		t.Fatalf("anti-vacuity: parser node has no source")
	}
	node.SetSource(nil)
	locAfter, okAfter := node.Source()
	if !okAfter || locAfter != locBefore {
		t.Fatalf("SetSource modified a sealed node: before %v/%v after %v/%v", locBefore, okBefore, locAfter, okAfter)
	}
}

// TestMacroexpandRestDoesNotLaunderSealedBacking pins a leak the original CoW
// sweep missed: the macroexpand / macroexpand-1 builtins used to slice
// form.Cells[1:] of a (possibly sealed) input directly and hand it to the
// macro, so a `&rest` parameter became an UNSEALED header over the sealed
// literal's backing array.  An in-place mutator in the macro body
// (stable-sort, or append! through slice 'vector) then rewrote the shared
// literal — the substrate#378 class, reachable from pure lisp through
// (macroexpand '(m ...)).  The normal call path never had this hole because
// evalSExprCells copies arguments into a fresh newCells slice; the
// macroexpand builtins are the one path that reached a macro without copying.
func TestMacroexpandRestDoesNotLaunderSealedBacking(t *testing.T) {
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"macroexpand+stable-sort", `
(defmacro sortm (&rest xs) (stable-sort > xs) '())
(set 'lit '(sortm 1 3 2))
(macroexpand lit)
`},
		{"macroexpand-1+stable-sort", `
(defmacro sortm (&rest xs) (stable-sort > xs) '())
(macroexpand-1 '(sortm 5 1 9))
`},
		{"macroexpand+append!-slice-vector", `
(defmacro clob (&rest xs) (append! (slice 'vector xs 0 1) 99) '())
(set 'lit '(clob 1 2 3))
(macroexpand lit)
`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			exprs := parseCached(t, tc.src)
			before := fingerprintAST(exprs)
			env := newCowTestEnv(t)
			for i, e := range exprs {
				if r := env.Eval(e); r.Type == lisp.LError {
					t.Fatalf("eval expr %d: %v", i, r)
				}
			}
			if after := fingerprintAST(exprs); after != before {
				t.Fatalf("macroexpand laundered sealed backing into a mutable &rest binding:"+
					" the parsed literal was rewritten in place\n  before %s\n  after  %s", before, after)
			}
		})
	}
}
