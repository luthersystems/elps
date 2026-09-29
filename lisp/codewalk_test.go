// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type row = struct {
	Expr   string
	Result string
	Output string
}

func TestMacroExpandAll(t *testing.T) {
	defs := elpstest.TestSequence{
		{`(defmacro m (x) (quasiquote (list (unquote x))))`, "()", ""},
		{`(defmacro twice (x) (quasiquote (progn (unquote x) (unquote x))))`, "()", ""},
	}
	seq := func(rows ...row) elpstest.TestSequence {
		return append(append(elpstest.TestSequence{}, defs...), rows...)
	}
	tests := elpstest.TestSuite{
		{"head and nested", seq(
			row{`(macroexpand-all '(twice (m 1)))`, `'(progn (list 1) (list 1))`, ""},
			row{`(macroexpand-all '(foo (bar (m 1))))`, `'(foo (bar (list 1)))`, ""},
			row{`(macroexpand-all '(foo 1))`, `'(foo 1)`, ""},
			row{`(macroexpand-all '())`, `'()`, ""},
			row{`(macroexpand '(foo (m 1)))`, `'(foo (m 1))`, ""},
		)},
		{"quoted data", seq(
			row{`(macroexpand-all '(quote (m 1)))`, `'(quote (m 1))`, ""},
			row{`(macroexpand-all '(list '(m 1)))`, `'(list '(m 1))`, ""},
			row{`(macroexpand-all '(quasiquote ((m 1) (unquote (m 2)) (unquote-splicing (m 3)))))`,
				`'(quasiquote ((m 1) (unquote (list 2)) (unquote-splicing (list 3))))`, ""},
			// Nested quasiquote and quote do not delay unquote in ELPS.
			row{`(macroexpand-all '(quasiquote (a (quasiquote (b (unquote (m 1)))))))`,
				`'(quasiquote (a (quasiquote (b (unquote (list 1))))))`, ""},
			row{`(macroexpand-all '(quasiquote (a '(unquote (m 1)))))`,
				`'(quasiquote (a '(unquote (list 1))))`, ""},
			// lisp:unquote is data inside a template.
			row{`(macroexpand-all '(quasiquote (a (lisp:unquote (m 1)))))`,
				`'(quasiquote (a (lisp:unquote (m 1))))`, ""},
			row{`(equal? (eval (macroexpand-all '(quasiquote (a (quasiquote (unquote (m 1)))))))
			             (quasiquote (a (quasiquote (unquote (m 1))))))`, "true", ""},
			row{`(macroexpand-all '(help m))`, `'(help m)`, ""},
			row{`(macroexpand-all '(qualified-symbol m))`, `'(qualified-symbol m)`, ""},
		)},
		{"binding forms", seq(
			row{`(macroexpand-all '(let ((a (m 1))) (m a)))`, `'(let ((a (list 1))) (list a))`, ""},
			row{`(macroexpand-all '(let* ((a (m 1)) (b (m a))) (m b)))`, `'(let* ((a (list 1)) (b (list a))) (list b))`, ""},
			row{`(macroexpand-all '(lambda (x &optional y) (m x)))`, `'(lambda (x &optional y) (list x))`, ""},
			row{`(macroexpand-all '(defun f (x) (m x)))`, `'(defun f (x) (list x))`, ""},
			row{`(macroexpand-all '(defmacro g (x) (m x)))`, `'(defmacro g (x) (list x))`, ""},
			row{`(macroexpand-all '(flet ((f (x) (m x))) (f (m 1))))`, `'(flet ((f (x) (list x))) (f (list 1)))`, ""},
			row{`(macroexpand-all '(labels ((f (x) (m x))) (f (m 1))))`, `'(labels ((f (x) (list x))) (f (list 1)))`, ""},
			row{`(macroexpand-all '(dotimes (i (m 3) (m i)) (m i)))`, `'(dotimes (i (list 3) (list i)) (list i))`, ""},
			row{`(macroexpand-all '(expr (m %)))`, `'(lisp:lambda (%) (list %))`, ""},
			row{`(macroexpand-all '(test "t" (m 1)))`, `'(test "t" (list 1))`, ""},
			row{`(macroexpand-all '(benchmark "b" (n) (m n)))`, `'(benchmark "b" (n) (list n))`, ""},
		)},
		{"lexical shadowing", seq(
			// A local function, variable or parameter named m is not the macro.
			row{`(macroexpand-all '(flet ((m (y) y)) (m (twice 1))))`, `'(flet ((m (y) y)) (m (progn 1 1)))`, ""},
			row{`(macroexpand-all '(labels ((m (y) (m y))) (m 1)))`, `'(labels ((m (y) (m y))) (m 1))`, ""},
			row{`(macroexpand-all '(let ((m list)) (m 1)))`, `'(let ((m list)) (m 1))`, ""},
			row{`(macroexpand-all '(lambda (m) (m 1)))`, `'(lambda (m) (m 1))`, ""},
			// flet function bodies are outside the flet's own names.
			row{`(macroexpand-all '(flet ((m (y) (m y))) 1))`, `'(flet ((m (y) (list y))) 1)`, ""},
			// let inits are outside the let; let* inits see earlier names.
			row{`(macroexpand-all '(let ((m 1) (b (m 2))) b))`, `'(let ((m 1) (b (list 2))) b)`, ""},
			row{`(macroexpand-all '(let* ((m list) (b (m 2))) b))`, `'(let* ((m list) (b (m 2))) b)`, ""},
		)},
		{"macrolet", seq(
			row{`(macroexpand-all '(macrolet ((sq (v) (quasiquote (* (unquote v) (unquote v))))) (sq (m 2))))`,
				`'(macrolet ((sq (v) (quasiquote (* (unquote v) (unquote v))))) (* (list 2) (list 2)))`, ""},
			// A macrolet name shadows a global macro of the same name.
			row{`(macroexpand-all '(macrolet ((m (v) v)) (m 7)))`, `'(macrolet ((m (v) v)) 7)`, ""},
		)},
		{"other special forms", seq(
			row{`(macroexpand-all '(if (m 1) (m 2) (m 3)))`, `'(if (list 1) (list 2) (list 3))`, ""},
			row{`(macroexpand-all '(cond ((m 1) (m 2)) (:else (m 3))))`, `'(cond ((list 1) (list 2)) (:else (list 3)))`, ""},
			row{`(macroexpand-all '(handler-bind ((condition (lambda (c &rest a) (m c)))) (m 1)))`,
				`'(handler-bind ((condition (lambda (c &rest a) (list c)))) (list 1))`, ""},
			row{`(macroexpand-all '(with-cleanup ((m 1)) (m 2)))`, `'(with-cleanup ((list 1)) (list 2))`, ""},
			row{`(macroexpand-all '(set! x (m 1)))`, `'(set! x (list 1))`, ""},
			row{`(macroexpand-all '(function m))`, `'(function m)`, ""},
			row{`(macroexpand-all '(thread-first (m 1) (m 2) m))`, `'(thread-first (list 1) (m 2) m)`, ""},
			row{`(macroexpand-all '(and (m 1) (or (m 2)) (progn (m 3))))`, `'(and (list 1) (or (list 2)) (progn (list 3)))`, ""},
			row{`(macroexpand-all '(lisp:when (m 1) (m 2)))`, `'(lisp:when (list 1) (list 2))`, ""},
			row{`(macroexpand-all '(ignore-errors (assert (m 1))))`, `'(ignore-errors (assert (list 1)))`, ""},
		)},
		{"bracket syntax", seq(
			// [...] reads as a quoted list; where a form reads it as
			// structure, it is structure, not data.
			row{`(macroexpand-all '(let ([a (m 1)]) (m a)))`, `'(let ('(a (list 1))) (list a))`, ""},
			row{`(macroexpand-all '(cond [(m 1) (m 2)]))`, `'(cond '((list 1) (list 2)))`, ""},
			row{`(macroexpand-all '(dotimes [i (m 3)] (m i)))`, `'(dotimes '(i (list 3)) (list i))`, ""},
			row{`(macroexpand-all '(with-cleanup [(m 1)] (m 2)))`, `'(with-cleanup '((list 1)) (list 2))`, ""},
			row{`(macroexpand-all '(flet ([f (x) (m x)]) (f 1)))`, `'(flet ('(f (x) (list x))) (f 1))`, ""},
			row{`(macroexpand-all '(handler-bind ([condition (lambda (c &rest a) (m c))]) 1))`,
				`'(handler-bind ('(condition (lambda (c &rest a) (list c)))) 1)`, ""},
		)},
		{"expansion is equivalent", seq(
			row{`(eval (macroexpand-all '(let ((a 2)) (twice (m a)))))`, `'(2)`, ""},
		)},
		{"errors", seq(
			row{`(macroexpand-all 3)`, `test:1:1: lisp:macroexpand-all: first argument is not a list: int`, ""},
			row{`(defmacro loop-forever () '(loop-forever))`, "()", ""},
			row{`(ignore-errors (macroexpand-all '(list (loop-forever))))`, "()", ""},
		)},
	}
	elpstest.RunTestSuite(t, tests)
}

// macroexpand-all never writes to its (sealed) input and the result shares
// no cells array with it.
func TestMacroExpandAllDoesNotMutateSealedInput(t *testing.T) {
	src := `
(defmacro sortm (&rest xs) (stable-sort > xs) (quasiquote (list (unquote-splicing xs))))
(defmacro twice (x) (quasiquote (progn (unquote x) (unquote x))))
(set 'lit '(let ((a (sortm 1 3 2))) (twice (sortm 5 4 6))))
(set 'out (macroexpand-all lit))
out
`
	exprs := parseCached(t, src)
	before := fingerprintAST(exprs)
	env := newCowTestEnv(t)
	var last *lisp.LVal
	for i, e := range exprs {
		last = env.Eval(e)
		require.NotEqual(t, lisp.LError, last.Type, "expr %d: %v", i, last)
	}
	assert.Equal(t, before, fingerprintAST(exprs), "macroexpand-all rewrote the parsed literal")
	assert.Equal(t, "'(let ((a (list 3 2 1))) (progn (list 6 5 4) (list 6 5 4)))", last.String())
}

// Walk keeps the source location of every list it rebuilds and returns
// untouched subtrees as the same nodes.
func TestCodeWalkerPreservesSourceAndSharing(t *testing.T) {
	exprs := parseCached(t, "(foo\n  (bar 1)\n  (m 2))")
	form := exprs[0]
	w := &lisp.CodeWalker{
		Expand1: func(f *lisp.LVal) (*lisp.LVal, bool) {
			if f.Cells[0].Str != "m" {
				return nil, false
			}
			return lisp.SExpr([]*lisp.LVal{lisp.Symbol("list"), f.Cells[1]}), true
		},
	}
	out := w.Walk(form)
	require.NotSame(t, form, out)
	assert.Equal(t, "(foo (bar 1) (list 2))", out.String())
	src, ok := out.Source()
	require.True(t, ok)
	orig, _ := form.Source()
	assert.Equal(t, orig, src)
	assert.Same(t, form.Cells[1], out.Cells[1], "an unchanged subtree is shared")
	assert.Same(t, form.Cells[2].Cells[1], out.Cells[2].Cells[1], "macro arguments keep their nodes")
	assert.False(t, out.IsSealed())
	assert.True(t, form.IsSealed())
	assert.Equal(t, "(foo (bar 1) (m 2))", form.String())

	// Nothing to expand: the input itself comes back.
	same := parseCached(t, "(foo (bar 1))")[0]
	assert.Same(t, same, w.Walk(same))
}

// The walker reports bindings, scopes, references and data.
func TestCodeWalkerEvents(t *testing.T) {
	form := parseCached(t, `(let ((a 1)) (flet ((f (x) (+ x a))) (f 'q (quote r) :k)))`)[0]
	var got []string
	w := &lisp.CodeWalker{Visit: func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkBind:
			got = append(got, "bind:"+n.Node.Str)
		case lisp.WalkRef:
			s := "ref:" + n.Node.Str
			if n.Bound {
				s += "(bound)"
			}
			got = append(got, s)
		case lisp.WalkData:
			got = append(got, "data:"+n.Node.String())
		case lisp.WalkEnter:
			got = append(got, "enter:"+n.Op)
		case lisp.WalkLeave:
			got = append(got, "leave:"+n.Op)
		default:
		}
		return true
	}}
	w.Walk(form)
	assert.Equal(t, strings.Join([]string{
		"enter:let", "bind:a",
		"enter:flet", "bind:x", "ref:+", "ref:x(bound)", "ref:a(bound)", "leave:flet",
		"enter:flet", "bind:f",
		"ref:f(bound)", "data:'q", "data:r",
		"leave:flet", "leave:let",
	}, " "), strings.Join(got, " "))
}

// A special operator without a known shape is opaque: nothing inside it
// is walked or expanded.
func TestCodeWalkerOpaqueSpecialOp(t *testing.T) {
	form := parseCached(t, `(my-op (m 1))`)[0]
	w := &lisp.CodeWalker{
		SpecialOp: func(h *lisp.LVal) (string, bool) { return h.Str, h.Str == "my-op" },
		Expand1: func(f *lisp.LVal) (*lisp.LVal, bool) {
			t.Fatalf("expanded inside an opaque form: %v", f)
			return nil, false
		},
		Visit: func(n *lisp.WalkNode) bool {
			if n.Event == lisp.WalkRef {
				t.Fatalf("walked inside an opaque form: %v", n.Node)
			}
			return true
		},
	}
	assert.Same(t, form, w.Walk(form))
}

// Source analysis preserves the tooling's historical interpretation, including
// interleaved let declarations, flet's outer closure scope and template refs.
func TestCodeWalkerSourceAnalysis(t *testing.T) {
	tests := []struct {
		source string
		events []string
	}{
		{`(let ((x first) (y second)) (+ x y))`, []string{"enter:let", "outer", "ref:first", "inner", "bind:x=first", "outer", "ref:second", "inner", "bind:y=second", "ref:+", "ref:x", "ref:y", "end:", "leave:let", "end:let"}},
		{`(flet ((f (x) (f x))) (f 1))`, []string{"enter:flet", "bind:f", "outer-function", "bind:x", "ref:f", "ref:x", "end:", "leave:flet", "ref:f", "end:", "leave:flet", "end:flet"}},
		{`(quasiquote [known (unquote missing extra) '(unquote held)])`, []string{"template:known", "ref:missing", "ref:extra", "template:unquote", "template:held", "end:quasiquote"}},
		{`(macrolet ((m (x) omitted)) (m unknown))`, []string{"enter:macrolet", "bind:m", "ref:m", "ref:unknown", "end:", "leave:macrolet", "end:macrolet"}},
		{`(cond (true yes) [no ignored] malformed (else final))`, []string{"ref:yes", "ref:final", "end:cond"}},
		{`(test-let "t" ((x 1)) x)`, []string{"enter:test-let", "outer", "inner", "bind:x", "ref:x", "leave:test-let", "end:test-let"}},
	}
	for _, tt := range tests {
		t.Run(tt.source, func(t *testing.T) {
			form := parseCached(t, tt.source)[0]
			before := fingerprintAST([]*lisp.LVal{form})
			var got []string
			w := &lisp.CodeWalker{SourceAnalysis: true, Visit: func(n *lisp.WalkNode) bool {
				switch n.Event {
				case lisp.WalkEnter:
					if n.Outer {
						if n.Function {
							got = append(got, "outer-function")
						} else {
							got = append(got, "outer")
						}
					} else {
						got = append(got, "enter:"+n.Op)
					}
				case lisp.WalkLeave:
					if n.Node == nil {
						got = append(got, "inner")
					} else {
						got = append(got, "leave:"+n.Op)
					}
				case lisp.WalkBind:
					s := "bind:" + n.Node.Str
					if n.Init != nil {
						s += "=" + n.Init.String()
					}
					got = append(got, s)
				case lisp.WalkRef:
					got = append(got, "ref:"+n.Node.Str)
				case lisp.WalkData:
					if n.Template && n.Node.Type == lisp.LSymbol && !n.Node.IsQuoted() {
						got = append(got, "template:"+n.Node.Str)
					}
				case lisp.WalkEnd:
					got = append(got, "end:"+n.Op)
				case lisp.WalkForm, lisp.WalkSet, lisp.WalkDefine, lisp.WalkLiteral:
				}
				return true
			}}
			require.Same(t, form, w.Walk(form))
			require.Equal(t, tt.events, got)
			require.Equal(t, before, fingerprintAST([]*lisp.LVal{form}))
		})
	}
}

func TestCodeWalkerCustomBinding(t *testing.T) {
	form := parseCached(t, `(custom name meta (x) (+ x missing))`)[0]
	var refs []string
	var definitions []*lisp.WalkNode
	w := &lisp.CodeWalker{SourceAnalysis: true,
		BindingForm: func(v *lisp.LVal) *lisp.CodeBinding {
			if v.Cells[0].Str == "custom" {
				return &lisp.CodeBinding{NameIndex: 1, FormalsIndex: 3}
			}
			return nil
		},
		Visit: func(n *lisp.WalkNode) bool {
			if n.Event == lisp.WalkDefine {
				cp := *n
				definitions = append(definitions, &cp)
			}
			if n.Event == lisp.WalkRef {
				refs = append(refs, n.Node.Str)
			}
			return true
		},
	}
	require.Same(t, form, w.Walk(form))
	require.Equal(t, []string{"custom", "meta", "+", "x", "missing"}, refs)
	require.Len(t, definitions, 1)
	require.Same(t, form, definitions[0].Owner)
	require.Same(t, form.Cells[3], definitions[0].Formals)
}

func TestCodeWalkerSourceVisitsSharedCodePerOccurrence(t *testing.T) {
	shared := parseCached(t, `(f x)`)[0]
	form := lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), shared, shared})
	refs := 0
	w := &lisp.CodeWalker{SourceAnalysis: true, Visit: func(n *lisp.WalkNode) bool {
		if n.Event == lisp.WalkRef {
			refs++
		}
		return true
	}}
	w.Walk(form)
	require.Equal(t, 5, refs) // progn plus f/x at each occurrence
}

func TestCodeWalkerSourceDefinitionMetadata(t *testing.T) {
	for _, op := range []string{"defun", "defmacro", "deftype"} {
		t.Run(op, func(t *testing.T) {
			form := parseCached(t, "("+op+" name (x) x)")[0]
			var definition *lisp.WalkNode
			w := &lisp.CodeWalker{SourceAnalysis: true, Visit: func(n *lisp.WalkNode) bool {
				if n.Event == lisp.WalkDefine {
					cp := *n
					definition = &cp
				}
				return true
			}}
			require.Same(t, form, w.Walk(form))
			require.NotNil(t, definition)
			require.Same(t, form, definition.Owner)
			require.Same(t, form.Cells[1], definition.Node)
			require.Same(t, form.Cells[2], definition.Formals)
			require.Equal(t, op, definition.Op)
		})
	}
}

// Adding source syntax for ordinary macros must not suppress their runtime
// expansion through the default classifier.
func TestCodeWalkerExpandsSourceOnlyForms(t *testing.T) {
	for _, op := range []string{"deftype", "test-let", "test-let*"} {
		t.Run(op, func(t *testing.T) {
			form := parseCached(t, "("+op+" name () body)")[0]
			calls := 0
			w := &lisp.CodeWalker{Expand1: func(v *lisp.LVal) (*lisp.LVal, bool) {
				calls++
				require.Same(t, form, v)
				return lisp.Int(7), true
			}}
			require.Equal(t, lisp.Int(7), w.Walk(form))
			require.Equal(t, 1, calls)
		})
	}
}
