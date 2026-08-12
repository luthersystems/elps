// Copyright © 2026 The ELPS authors

package main

import (
	"fmt"
	"math/rand"
	"strings"
)

// The generator.
//
// This is NOT a random s-expression generator.  A uniform one spends almost
// all of its budget on programs that fail arity or type checking before they
// touch anything interesting, and the defects this harness exists to catch --
// four of them found by hand in the seal work, every one a corruption or a
// semantics change and NONE of them caught by a unit test -- all live on a
// narrow surface:
//
//   1. a QUOTED LITERAL reaching a MUTATING builtin.  A quoted literal is part
//      of the parse tree, so mutating it in place edits the program.  The
//      sealed branch's copy-on-write guards exist precisely here.
//   2. RE-EVALUATION of the same parsed form.  Corruption of a literal is
//      invisible on the first call and only shows on the second, which is why
//      every generated program calls its function more than once.
//   3. CAPACITY GAMES on append/slice/concat.  A slice that shares a backing
//      array with its parent writes through it when appended to under
//      capacity.
//   4. NESTED containers under the elpspath ?-family, whose copying variants
//      must not alias what they copy and whose ! variants must not write into
//      a sealed node.
//
// So the generator is template-driven with random fill: a bank of statement
// shapes aimed at 1-4, instantiated with randomly chosen literals, operators
// and predicates.  Uniform random noise is still generated (genExpr) but as
// the FILL, where it perturbs a shape that is already on the interesting
// surface, rather than as the whole program.
//
// Everything the generator emits must be DETERMINISTIC across two runs in two
// different interpreters in the same process.  That rules out gensym (its
// counter is per-environment but its printed form leaks into values), time,
// randomness and anything touching the filesystem or the environment.  It
// does NOT rule out errors: an error is an outcome to compare like any other,
// and the error-vs-panic changes on the sealed branch are exactly the kind of
// drift worth diffing.

// enableElpspath gates every shape that uses the `elpspath` package.
//
// It has to be a switch rather than a constant because elpspath does NOT
// exist on origin/main -- it was adopted onto the sealed branch from a
// production-scale phylum's runtime (b7ad5ca).  Against origin/main the whole
// package is an ADDITION, and diffing an addition against its own absence
// produces "unknown package: elpspath" on every single program, which is
// noise, not signal.  The elpspath surface is instead diffed in a second run
// whose left-hand tree is the adoption commit itself, which isolates exactly
// the seal-era changes to it (0442c52, 95bbd0c).  See README.md.
var enableElpspath = false

// Gen holds the generator state for one program.
type Gen struct {
	rng    *rand.Rand
	fresh  int
	nested int
}

// NewGen returns a generator seeded deterministically.  The same seed always
// produces the same program, which is what makes a divergence reproducible
// from its seed alone.
func NewGen(seed int64) *Gen { return &Gen{rng: rand.New(rand.NewSource(seed))} }

func (g *Gen) pick(ss []string) string { return ss[g.rng.Intn(len(ss))] }
func (g *Gen) chance(n int) bool       { return g.rng.Intn(n) == 0 }

func (g *Gen) name(prefix string) string {
	g.fresh++
	return fmt.Sprintf("%s%d", prefix, g.fresh)
}

// ---------------------------------------------------------------------------
// literals
// ---------------------------------------------------------------------------

var (
	intLits    = []string{"0", "1", "2", "3", "-1", "7", "42", "-13"}
	floatLits  = []string{"0.0", "1.5", "-2.25", "3.0"}
	strLits    = []string{`"a"`, `"b"`, `"ab"`, `"zz"`, `""`, `"key"`, `"m"`}
	boolLits   = []string{"true", "false"}
	atomLits   = []string{"()", "'sym", "'other"}
	typeSpecs  = []string{"'list", "'vector"}
	lessPreds  = []string{"<", ">", "<=", ">=", "string<", "string>"}
	numPreds   = []string{"<", ">", "<=", ">="}
	numOps     = []string{"+", "-", "*", "max", "min"}
	predicates = []string{"int?", "string?", "nil?", "list?", "vector?", "number?", "true?"}
)

func (g *Gen) atom() string {
	switch g.rng.Intn(6) {
	case 0:
		return g.pick(floatLits)
	case 1:
		return g.pick(strLits)
	case 2:
		return g.pick(boolLits)
	case 3:
		return g.pick(atomLits)
	default:
		return g.pick(intLits)
	}
}

// quotedList emits a QUOTED list literal -- a node of the parse tree itself.
// Handing one of these to a mutating builtin is defect class 1.
func (g *Gen) quotedList() string {
	n := 1 + g.rng.Intn(4)
	parts := make([]string, n)
	for i := range parts {
		if g.chance(5) && g.nested < 2 {
			g.nested++
			parts[i] = g.quotedInner()
			g.nested--
		} else {
			parts[i] = g.unquotedAtom()
		}
	}
	return "'(" + strings.Join(parts, " ") + ")"
}

// quotedInner is a nested list INSIDE an already-quoted literal, so it carries
// no quote mark of its own.
func (g *Gen) quotedInner() string {
	n := 1 + g.rng.Intn(3)
	parts := make([]string, n)
	for i := range parts {
		parts[i] = g.unquotedAtom()
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// unquotedAtom is an atom safe to place inside a quoted literal: no `()`,
// which would read as nil, and no bare symbol that the printer renders
// differently from how it was written.
func (g *Gen) unquotedAtom() string {
	switch g.rng.Intn(5) {
	case 0:
		return g.pick(strLits)
	case 1:
		return g.pick(floatLits)
	case 2:
		return g.pick(boolLits)
	default:
		return g.pick(intLits)
	}
}

func (g *Gen) vectorLit() string {
	n := g.rng.Intn(5)
	parts := make([]string, n)
	for i := range parts {
		parts[i] = g.atom()
	}
	return "(vector " + strings.Join(parts, " ") + ")"
}

func (g *Gen) bytesLit() string {
	return fmt.Sprintf("(to-bytes %s)", g.pick([]string{`"abc"`, `"a"`, `""`, `"hello"`, `"xyzzy"`}))
}

func (g *Gen) mapLit() string {
	n := 1 + g.rng.Intn(3)
	parts := make([]string, 0, 2*n)
	for i := 0; i < n; i++ {
		parts = append(parts, fmt.Sprintf(`"k%d"`, i), g.mapValue())
	}
	return "(sorted-map " + strings.Join(parts, " ") + ")"
}

func (g *Gen) mapValue() string {
	if g.nested >= 2 {
		return g.atom()
	}
	g.nested++
	defer func() { g.nested-- }()
	switch g.rng.Intn(6) {
	case 0:
		return g.mapLit()
	case 1:
		return g.vectorLit()
	case 2:
		return g.quotedList()
	default:
		return g.atom()
	}
}

// numAtom is an atom that is safe in a numeric context.  Feeding `'sym` to
// `<` is a legitimate program and its error is compared like any other, but a
// generator that does it most of the time spends its budget proving that both
// trees reject bad types identically, which nobody doubted.
func (g *Gen) numAtom() string { return g.pick(intLits) }

// numQuotedList is a quoted literal whose elements are all numbers, so the
// ordering builtins accept it.  This is the shape the stable-sort defect
// lived on.
func (g *Gen) numQuotedList() string {
	n := 1 + g.rng.Intn(4)
	parts := make([]string, n)
	for i := range parts {
		parts[i] = g.numAtom()
	}
	return "'(" + strings.Join(parts, " ") + ")"
}

// numVector is the vector counterpart.  `append!` requires a vector -- lists
// are immutable in that direction -- so every in-place append shape is built
// on one of these.
func (g *Gen) numVector() string {
	n := g.rng.Intn(5)
	parts := make([]string, n)
	for i := range parts {
		parts[i] = g.numAtom()
	}
	return "(vector " + strings.Join(parts, " ") + ")"
}

// numSeq is a sequence of numbers in either representation.
func (g *Gen) numSeq() string {
	switch g.rng.Intn(4) {
	case 0:
		return g.numVector()
	case 1:
		return fmt.Sprintf("(make-sequence %s %s)", g.pick([]string{"0", "1", "-2"}), g.pick([]string{"3", "5", "0"}))
	default:
		return g.numQuotedList()
	}
}

// seq returns an expression of sequence type, biased toward quoted literals.
func (g *Gen) seq() string {
	switch g.rng.Intn(10) {
	case 0, 1, 2, 3:
		return g.quotedList()
	case 4, 5:
		return g.vectorLit()
	case 6:
		return g.bytesLit()
	case 7:
		return fmt.Sprintf("(make-sequence %s %s)", g.pick(intLits), g.pick(intLits))
	case 8:
		return fmt.Sprintf("(list %s %s)", g.atom(), g.atom())
	default:
		return g.quotedList()
	}
}

// listySeq is a sequence that `concat` accepts under a 'list or 'vector type
// specifier.  Bytes are excluded: concat refuses to mix them with a list
// specifier, which is a type check, not a semantic difference worth budget.
func (g *Gen) listySeq() string {
	switch g.rng.Intn(6) {
	case 0, 1, 2:
		return g.quotedList()
	case 3, 4:
		return g.vectorLit()
	default:
		return fmt.Sprintf("(list %s %s)", g.atom(), g.atom())
	}
}

// nestedContainer is the elpspath surface: a document with maps, vectors and
// quoted lists inside each other.
func (g *Gen) nestedContainer() string {
	return fmt.Sprintf(`(sorted-map "a" %s "b" (vector %s %s) "c" %s)`,
		g.mapLit(), g.atom(), g.quotedList(), g.vectorLit())
}

func (g *Gen) lambda1() string {
	v := g.name("x")
	switch g.rng.Intn(5) {
	case 0:
		return fmt.Sprintf("(lambda (%s) (%s %s))", v, g.pick(predicates), v)
	case 1:
		return fmt.Sprintf("(lambda (%s) (%s %s %s))", v, g.pick(numOps), v, g.numAtom())
	case 2:
		return fmt.Sprintf("(lambda (%s) (to-string %s))", v, v)
	case 3:
		return fmt.Sprintf("(lambda (%s) %s)", v, v)
	default:
		return fmt.Sprintf("(lambda (%s) (list %s %s))", v, v, g.atom())
	}
}

func (g *Gen) pred1() string {
	if g.chance(2) {
		return "(function " + g.pick(predicates) + ")"
	}
	v := g.name("p")
	return fmt.Sprintf("(lambda (%s) (%s %s %s))", v, g.pick([]string{"<", ">", "=", "<=", ">="}), v, g.pick(intLits))
}

// ---------------------------------------------------------------------------
// free-form expressions (the fill)
// ---------------------------------------------------------------------------

// genExpr produces an arbitrary expression of bounded depth.  It is the noise
// term: it goes where a shape needs "some value" and its job is to be varied,
// not to be well typed.
func (g *Gen) genExpr(depth int) string {
	if depth <= 0 {
		return g.atom()
	}
	switch g.rng.Intn(16) {
	case 0:
		return fmt.Sprintf("(%s %s %s)", g.pick(numOps), g.numAtom(), g.genExpr(depth-1))
	case 1:
		return fmt.Sprintf("(if %s %s %s)", g.genExpr(depth-1), g.genExpr(depth-1), g.genExpr(depth-1))
	case 2:
		return fmt.Sprintf("(length %s)", g.seq())
	case 3:
		return fmt.Sprintf("(car %s)", g.listySeq())
	case 4:
		return fmt.Sprintf("(cdr %s)", g.listySeq())
	case 5:
		return fmt.Sprintf("(nth %s %s)", g.listySeq(), g.pick([]string{"0", "1", "2"}))
	case 6:
		return fmt.Sprintf("(concat %s %s %s)", g.pick(typeSpecs), g.listySeq(), g.listySeq())
	case 7:
		return fmt.Sprintf("(reverse %s %s)", g.pick(typeSpecs), g.listySeq())
	case 8:
		return fmt.Sprintf("(map %s %s %s)", g.pick(typeSpecs), g.lambda1(), g.numSeq())
	case 9:
		return fmt.Sprintf("(foldl %s %s %s)", g.pick([]string{"+", "max", "min"}), g.numAtom(), g.numSeq())
	case 10:
		return fmt.Sprintf("(slice %s %s %s %s)", g.pick(typeSpecs), g.listySeq(), g.pick([]string{"0", "1"}), g.pick([]string{"1", "2", "0"}))
	case 11:
		return fmt.Sprintf("(select %s %s %s)", g.pick(typeSpecs), g.pred1(), g.numSeq())
	case 12:
		return fmt.Sprintf("(to-string %s)", g.genExpr(depth-1))
	case 13:
		return g.seq()
	case 14:
		return fmt.Sprintf("(sorted-map %s %s)", g.pick(strLits), g.genExpr(depth-1))
	default:
		return g.atom()
	}
}

// ---------------------------------------------------------------------------
// statement shapes
// ---------------------------------------------------------------------------

// shape emits one statement aimed at a specific defect class.  Each shape
// leaves its work in a probe global so the side-effect oracle can read it back
// even when the program's final value hides the damage.
func (g *Gen) shape() string {
	n := 22
	if !enableElpspath {
		// Shapes 11..15 are the elpspath ones; fold them onto the rest.
		n = 17
	}
	k := g.rng.Intn(n)
	if !enableElpspath && k >= 11 {
		k += 5
	}
	switch k {

	// --- 1. quoted literal into a mutating builtin -------------------------
	case 0:
		// append! onto a quoted literal.  Under the seal design this must
		// copy; without it the literal in the parse tree grows on every call.
		return fmt.Sprintf("(set 'vec (append! %s %s))", g.numVector(), g.atom())
	case 1:
		return fmt.Sprintf("(set 'lst (append %s %s %s))", g.pick(typeSpecs), g.quotedList(), g.atom())
	case 2:
		return fmt.Sprintf("(set 'byt (append-bytes! %s %s))", g.bytesLit(), g.pick([]string{`"z"`, `""`, `(to-bytes "q")`}))

	// --- 2. sort, the shape whose defect only showed on the SECOND call ----
	case 3:
		return fmt.Sprintf("(set 'lst (stable-sort %s %s))", g.pick(numPreds), g.numQuotedList())
	case 4:
		return fmt.Sprintf("(set 'vec (stable-sort %s %s))", g.pick(numPreds), g.numVector())
	case 5:
		return fmt.Sprintf("(set 'lst (insert-sorted %s %s %s %s))",
			g.pick(typeSpecs), g.numQuotedList(), g.pick(numPreds), g.numAtom())

	// --- 3. capacity games on append/slice/concat -------------------------
	case 6:
		// A slice of a literal, appended to.  If the slice shares its parent's
		// backing array and there is spare capacity, the append writes through
		// into the parent -- and the parent is a parse-tree node.
		s := g.name("s")
		return fmt.Sprintf("(let ([%s (slice %s %s 0 %s)]) (set 'lst (append %s %s %s)) (set 'g0 %s))",
			s, g.pick(typeSpecs), g.quotedList(), g.pick([]string{"0", "1", "2"}),
			g.pick(typeSpecs), s, g.atom(), s)
	case 7:
		q := g.name("q")
		return fmt.Sprintf("(let ([%s %s]) (set 'g0 (append! %s %s)) (set 'g1 %s))",
			q, g.numVector(), q, g.atom(), q)
	case 8:
		return fmt.Sprintf("(set 'lst (concat %s %s %s %s))",
			g.pick(typeSpecs), g.listySeq(), g.listySeq(), g.listySeq())

	// --- 4. sorted-map mutation ------------------------------------------
	case 9:
		m := g.name("m")
		return fmt.Sprintf("(let ([%s %s]) (assoc! %s %s %s) (dissoc! %s %s) (set 'sm %s))",
			m, g.mapLit(), m, g.pick(strLits), g.genExpr(1), m, g.pick([]string{`"k0"`, `"k1"`, `"nope"`}), m)
	case 10:
		return fmt.Sprintf("(set 'sm (assoc %s %s %s))", g.mapLit(), g.pick(strLits), g.genExpr(1))

	// --- 5. elpspath, copying and mutating -------------------------------
	case 11:
		d := g.name("d")
		return fmt.Sprintf("(let ([%s %s]) (set 'g0 (elpspath:?set %s %s %s)) (set 'sm %s))",
			d, g.nestedContainer(), d, g.pathStep(), g.genExpr(1), d)
	case 12:
		d := g.name("d")
		return fmt.Sprintf("(let ([%s %s]) (elpspath:?set! %s %s %s) (set 'sm %s))",
			d, g.nestedContainer(), d, g.pathStep(), g.genExpr(1), d)
	case 13:
		d := g.name("d")
		return fmt.Sprintf("(let ([%s %s]) (set 'g0 (elpspath:?del %s %s)) (set 'sm %s))",
			d, g.nestedContainer(), d, g.pathStep(), d)
	case 14:
		d := g.name("d")
		return fmt.Sprintf("(let ([%s %s]) (elpspath:?%s! %s %s) (set 'sm %s))",
			d, g.nestedContainer(), g.pick([]string{"del", "nil"}), d, g.pathStep(), d)
	case 15:
		return fmt.Sprintf("(set 'g0 (elpspath:? %s %s))", g.nestedContainer(), g.pathStep())

	// --- 6. macros with quoted arguments and quasiquote splices ----------
	case 16:
		m := g.name("mac")
		return fmt.Sprintf("(defmacro %s (a) (quasiquote (list (unquote a) (unquote a))))\n(set 'g1 (%s %s))",
			m, m, g.quotedList())
	case 17:
		m := g.name("mac")
		return fmt.Sprintf("(defmacro %s (&rest xs) (quasiquote (list (unquote-splicing xs) %s)))\n(set 'g1 (%s %s %s))",
			m, g.atom(), m, g.atom(), g.atom())
	case 18:
		return fmt.Sprintf("(set 'g1 (macroexpand-1 (quote (%s %s))))",
			g.pick([]string{"if", "let", "cond"}), g.atom())

	// --- 7. apply/funcall over a SHARED argument list --------------------
	case 19:
		a := g.name("args")
		return fmt.Sprintf("(let ([%s %s]) (set 'g0 (apply (function list) %s)) (set 'g1 (apply (function list) %s)) (set 'lst %s))",
			a, g.quotedList(), a, a, a)
	case 20:
		return fmt.Sprintf("(set 'g0 (funcall (lambda (&rest xs) (append 'list xs %s)) %s %s))",
			g.atom(), g.atom(), g.atom())

	default:
		return fmt.Sprintf("(set 'acc %s)", g.genExpr(2+g.rng.Intn(2)))
	}
}

// pathStep emits one elpspath step, covering keys, indices, negative indices,
// the iterator and ranges.
func (g *Gen) pathStep() string {
	switch g.rng.Intn(8) {
	case 0:
		return `"a" "k0"`
	case 1:
		return `"b" 0`
	case 2:
		return `"b" -1`
	case 3:
		return `"c" 0`
	case 4:
		return `"b" '*`
	case 5:
		return `"b" '(range 0 1)`
	case 6:
		return `"nope"`
	default:
		return `"a"`
	}
}

// ---------------------------------------------------------------------------
// programs
// ---------------------------------------------------------------------------

// Program generates one complete program.
//
// The invariant that makes this harness able to see corruption at all: the
// generated function is defined ONCE and called two or three times, and the
// value of every call is retained.  A mutation that damages a quoted literal
// inside the function body is invisible in call 1 and shows up as a different
// value in call 2 -- which is exactly how the stable-sort defect behaved.
func (g *Gen) Program() string {
	var b strings.Builder
	b.WriteString(preamble())

	nStmt := 1 + g.rng.Intn(3)
	fn := g.name("f")
	b.WriteString("(defun " + fn + " (n)\n")
	for i := 0; i < nStmt; i++ {
		b.WriteString("  " + g.shape() + "\n")
	}
	b.WriteString("  (list n " + g.genExpr(2) + "))\n")

	calls := 2 + g.rng.Intn(2)
	b.WriteString("(set 'acc ())\n")
	for i := 0; i < calls; i++ {
		fmt.Fprintf(&b, "(set 'g2 (%s %d))\n", fn, i)
		fmt.Fprintf(&b, "(set 'acc (cons g2 acc))\n")
	}
	b.WriteString("acc\n")
	return b.String()
}

// TopLevelProgram generates a program with no function wrapper: the same
// shapes evaluated directly at the top level.  Some defects need the function
// frame to appear and some need its absence, so the driver generates both.
func (g *Gen) TopLevelProgram() string {
	var b strings.Builder
	b.WriteString(preamble())
	n := 2 + g.rng.Intn(4)
	for i := 0; i < n; i++ {
		b.WriteString(g.shape() + "\n")
	}
	b.WriteString(g.genExpr(3) + "\n")
	return b.String()
}

// preamble is what every generated program starts with.  It is empty unless
// the elpspath shapes are enabled, because `use-package` on a package the
// left-hand tree does not have aborts the program before it reaches anything
// worth diffing.
func preamble() string {
	if enableElpspath {
		return "(use-package 'elpspath)\n"
	}
	return ""
}
