// Copyright © 2026 The ELPS authors

package lisp

import (
	"math"
	"strconv"
	"strings"
)

// FormShape describes which parts of a special form are evaluated code,
// which are binding names, and which are unevaluated data.  Code walkers
// (CodeWalker, macroexpand-all, the astutil helpers built on them) use it to
// descend into exactly the parts of a form that are code.
//
// Every special operator in DefaultSpecialOps has a shape
// (TestEverySpecialOpHasAShape fails for a new operator without one).  A
// special operator an embedder registers has no shape; a walker treats a
// form headed by one as opaque and descends into none of its arguments.
type FormShape uint8

// The shapes of the builtin special forms.
const (
	// ShapeUnknown is the zero value: the walker does not know the form's
	// syntax and treats it as opaque.
	ShapeUnknown FormShape = iota
	// ShapeForms: every argument is an evaluated form (progn, if, and, or,
	// when, unless, while, default, assert, ignore-errors).
	ShapeForms
	// ShapeQuote: (quote datum).  The datum is data.
	ShapeQuote
	// ShapeQuasiquote: (quasiquote template).  The template is data except
	// for its unquote and unquote-splicing holes, which are code.
	ShapeQuasiquote
	// ShapeLambda: (lambda formals body...).
	ShapeLambda
	// ShapeDefun: (defun name formals body...) and defmacro.  These are
	// macros, but their expansion embeds a function value rather than
	// source, so walkers keep them as written and walk the body.
	ShapeDefun
	// ShapeLet: (let ((name init)...) body...).  Inits see the outer scope.
	ShapeLet
	// ShapeLetSeq: (let* ((name init)...) body...).  Each init sees the
	// names bound before it.
	ShapeLetSeq
	// ShapeFlet: (flet ((name formals body...)...) body...).  Function
	// bodies see the outer scope.
	ShapeFlet
	// ShapeLabels: (labels ((name formals body...)...) body...).  Function
	// bodies see every name the form binds.
	ShapeLabels
	// ShapeMacrolet: (macrolet ((name formals body...)...) body...).
	ShapeMacrolet
	// ShapeHandlerBind: (handler-bind ((condition-type handler)...) body...).
	// Condition types are data; handlers and body are code.
	ShapeHandlerBind
	// ShapeCond: (cond (test body...)...).
	ShapeCond
	// ShapeDotimes: (dotimes (name count [result]) body...).  count sees
	// the outer scope; result and body see name.
	ShapeDotimes
	// ShapeSetBang: (set! name expr).  name is assigned, expr is code.
	ShapeSetBang
	// ShapeFunction: (function name).  name is a reference, not a call.
	ShapeFunction
	// ShapeExpr: (expr pattern).  pattern is the body of a function whose
	// formals are the %-placeholders it uses.
	ShapeExpr
	// ShapeWithCleanup: (with-cleanup (cleanup-form...) body...).
	ShapeWithCleanup
	// ShapeThread: (thread-first value step...) and thread-last.  Each step
	// is a function call missing one argument; step heads must be regular
	// functions and are never macro-expanded.
	ShapeThread
	// ShapeTest: (test name body...).  name is data.
	ShapeTest
	// ShapeBenchmark: (benchmark name (count) body...).  name is data and
	// count is bound in body.
	ShapeBenchmark
	// ShapeData: every argument is unevaluated data (help,
	// qualified-symbol).
	ShapeData
)

var shapeNames = [...]string{
	ShapeUnknown:     "unknown",
	ShapeForms:       "forms",
	ShapeQuote:       "quote",
	ShapeQuasiquote:  "quasiquote",
	ShapeLambda:      "lambda",
	ShapeDefun:       "defun",
	ShapeLet:         "let",
	ShapeLetSeq:      "let*",
	ShapeFlet:        "flet",
	ShapeLabels:      "labels",
	ShapeMacrolet:    "macrolet",
	ShapeHandlerBind: "handler-bind",
	ShapeCond:        "cond",
	ShapeDotimes:     "dotimes",
	ShapeSetBang:     "set!",
	ShapeFunction:    "function",
	ShapeExpr:        "expr",
	ShapeWithCleanup: "with-cleanup",
	ShapeThread:      "thread",
	ShapeTest:        "test",
	ShapeBenchmark:   "benchmark",
	ShapeData:        "data",
}

func (s FormShape) String() string {
	if int(s) < len(shapeNames) {
		return shapeNames[s]
	}
	return "unknown"
}

// specialFormShapes maps the name of each builtin special operator (and the
// two builtin definition macros walkers keep as written) to its shape.
var specialFormShapes = map[string]FormShape{
	"function":         ShapeFunction,
	"set!":             ShapeSetBang,
	"assert":           ShapeForms,
	"quote":            ShapeQuote,
	"quasiquote":       ShapeQuasiquote,
	"lambda":           ShapeLambda,
	"expr":             ShapeExpr,
	"thread-first":     ShapeThread,
	"thread-last":      ShapeThread,
	"dotimes":          ShapeDotimes,
	"labels":           ShapeLabels,
	"macrolet":         ShapeMacrolet,
	"flet":             ShapeFlet,
	"let*":             ShapeLetSeq,
	"let":              ShapeLet,
	"progn":            ShapeForms,
	"handler-bind":     ShapeHandlerBind,
	"ignore-errors":    ShapeForms,
	"with-cleanup":     ShapeWithCleanup,
	"cond":             ShapeCond,
	"if":               ShapeForms,
	"when":             ShapeForms,
	"unless":           ShapeForms,
	"default":          ShapeForms,
	"while":            ShapeForms,
	"or":               ShapeForms,
	"and":              ShapeForms,
	"help":             ShapeData,
	"test":             ShapeTest,
	"benchmark":        ShapeBenchmark,
	"qualified-symbol": ShapeData,

	// Builtin macros whose expansion is not source.
	"defun":    ShapeDefun,
	"defmacro": ShapeDefun,
}

// SpecialFormShape returns the shape of the builtin special operator (or
// builtin definition macro) named name, unqualified, or ShapeUnknown.
func SpecialFormShape(name string) FormShape {
	return specialFormShapes[name]
}

// DefaultSpecialOpName is the static classifier CodeWalker uses when no
// environment is available.  It reports the builtin form a head symbol
// names, reading an unqualified name or one qualified by the lisp package
// as the builtin.  It cannot see a package that shadows a builtin name; a
// walker with an environment (LEnv.MacroExpandAll) resolves heads instead.
func DefaultSpecialOpName(head *LVal) (string, bool) {
	if head == nil || head.Type != LSymbol || head.quoted {
		return "", false
	}
	name := head.Str
	if rest, ok := strings.CutPrefix(name, DefaultLangPackage+":"); ok {
		name = rest
	} else if strings.Contains(name, ":") {
		return "", false
	}
	if _, ok := specialFormShapes[name]; ok {
		return name, true
	}
	return "", false
}

// WalkEvent classifies a WalkNode.
type WalkEvent uint8

// Events a CodeWalker reports to its visitor.
const (
	// WalkForm is a compound form in code position, after macro
	// expansion.  Op names the special form ("" for a function call).
	WalkForm WalkEvent = iota
	// WalkRef is a symbol evaluated as a variable or function reference.
	WalkRef
	// WalkSet is a symbol assigned by set!.
	WalkSet
	// WalkBind is a name a binding form introduces into the current scope.
	WalkBind
	// WalkDefine is the global name a defun or defmacro defines.
	WalkDefine
	// WalkLiteral is a self-evaluating value in code position: a number,
	// string, keyword, (), or a value a macro spliced in.
	WalkLiteral
	// WalkData is an unevaluated datum: a quoted value, a condition type,
	// a test name.  The walker never descends into data.
	WalkData
	// WalkEnter opens a scope.  Node is the form (or function binding)
	// that creates it.
	WalkEnter
	// WalkLeave closes the scope the matching WalkEnter opened.
	WalkLeave
)

// WalkNode is one event of a code walk.
type WalkNode struct {
	// Node is the value the event is about.  For WalkForm it is the form
	// after expansion; the walker never modifies it.
	Node *LVal
	// Op is the builtin form name for WalkForm, WalkEnter, WalkLeave and
	// WalkBind ("lambda", "let", ...), "" for a function call.
	Op string
	// Depth is the nesting depth, 0 for the walked form.  Everything
	// inside a form is deeper than the form, and everything inside a scope
	// deeper than its WalkEnter, so a visitor can keep its own context
	// stack by popping entries at least as deep as each new event.
	Depth int
	// Shape is the shape of Op.
	Shape FormShape
	// Opaque marks a WalkForm the walker does not descend into: a special
	// operator without a known shape, or a call to a local macro the
	// walker cannot expand.
	Opaque bool
	// Head marks a WalkRef that is the head of a function call.
	Head bool
	// Bound marks a WalkRef or WalkSet whose symbol is lexically bound
	// inside the walked form (by a let, lambda, flet, ...).  A reference
	// that is not bound is free in the walked form: a global, or a local
	// of code enclosing it.
	Bound bool
	// Function marks a WalkEnter/WalkLeave pair around a function body:
	// lambda, defun, defmacro, one flet/labels/macrolet binding, and expr.
	// The body of a let, flet, dotimes, test or benchmark is a scope but
	// not a function.
	Function bool
	// Event is the kind of event.
	Event WalkEvent
}

// CodeVisitor receives the events of a code walk.  For WalkForm, returning
// false skips the form's arguments; the return value is otherwise ignored.
// The *WalkNode is reused for the next event: copy it to keep it.
type CodeVisitor func(n *WalkNode) bool

// LocalMacroExpander expands a call to one macro bound by macrolet.
type LocalMacroExpander func(form *LVal) *LVal

// CodeWalker walks ELPS code, expanding macros and reporting what each part
// of every form is.  The walk understands every builtin special form's
// binding shape, tracks lexical bindings (so a local function or variable
// that shadows a macro name is not expanded), and never descends into
// quoted data.
//
// The walk never writes to its input.  Walk returns a new tree when any
// macro was expanded: every list on the path to an expansion is a fresh
// header over a fresh cells array carrying the original's source location
// and quoting, and every subtree the walk did not change is returned as the
// same node.  So a sealed program literal can be walked, and the result
// shares no mutable storage with it.
type CodeWalker struct {
	// SpecialOp classifies a head symbol that is not lexically bound.  It
	// returns the builtin form name the symbol denotes, or false for a
	// function or macro.  Nil means DefaultSpecialOpName.
	SpecialOp func(head *LVal) (string, bool)

	// Expand1 expands form once when its head names a macro.  ok is false
	// when it is not a macro call.  An expansion that fails returns an
	// LError, which stops the walk.  Nil disables expansion: Walk then only
	// visits.
	Expand1 func(form *LVal) (expansion *LVal, ok bool)

	// DefineLocalMacro builds the expander for one macrolet binding
	// (name formals body...).  Nil, or a nil result, leaves calls to the
	// local macro unexpanded and reports them as opaque forms.  An LError
	// result stops the walk.
	DefineLocalMacro func(binding *LVal) (LocalMacroExpander, *LVal)

	// Visit receives the walk's events.  It may be nil.
	Visit CodeVisitor

	// Replace, when set, is called after Visit for every WalkRef, WalkSet
	// and WalkBind event.  A non-nil result takes the symbol's place in
	// the returned tree (a rename); the input is still not written.  The
	// names expr binds are synthesized, so replacing them changes nothing.
	Replace func(n *WalkNode) *LVal

	err     *LVal
	scopes  []walkScope
	scratch WalkNode

	// MaxDepth bounds form nesting (default DefaultMaxEvalNesting) and
	// MaxExpansions the macro expansions of one form's head (default
	// DefaultMaxMacroExpansionDepth).
	MaxDepth      int
	MaxExpansions int

	// KeepGoing makes failures local: a macro call whose expansion fails
	// or does not terminate is walked unexpanded as a function call, and
	// a form nested past MaxDepth is reported as opaque, instead of
	// stopping the walk.  Tools that must see all of a file (lint) set
	// it; macroexpand-all does not.
	KeepGoing bool
}

type walkScope struct {
	names  map[string]LocalMacroExpander
	macros bool
}

// Walk walks form as code and returns it with every macro call expanded.
// A failed expansion returns the LError.
func (w *CodeWalker) Walk(form *LVal) *LVal {
	w.scopes = w.scopes[:0]
	w.err = nil
	out := w.form(form, 0)
	if w.err != nil {
		return w.err
	}
	return out
}

func (w *CodeWalker) visit(n WalkNode) bool {
	if w.Visit == nil {
		return true
	}
	// One node per walker, so an event does not allocate.
	w.scratch = n
	return w.Visit(&w.scratch)
}

func (w *CodeWalker) fail(err *LVal) *LVal {
	if w.err == nil {
		w.err = err
	}
	return err
}

func (w *CodeWalker) maxDepth() int {
	if w.MaxDepth > 0 {
		return w.MaxDepth
	}
	return DefaultMaxEvalNesting
}

func (w *CodeWalker) maxExpansions() int {
	if w.MaxExpansions > 0 {
		return w.MaxExpansions
	}
	return DefaultMaxMacroExpansionDepth
}

func (w *CodeWalker) push(macros bool) {
	w.scopes = append(w.scopes, walkScope{macros: macros})
}

func (w *CodeWalker) pop() {
	w.scopes = w.scopes[:len(w.scopes)-1]
}

// bind records name in the innermost scope.  mac is the local macro's
// expander (nil for a variable or function, or a local macro that cannot be
// expanded).
func (w *CodeWalker) bind(name *LVal, mac LocalMacroExpander) {
	if name == nil || name.Type != LSymbol || len(w.scopes) == 0 {
		return
	}
	s := &w.scopes[len(w.scopes)-1]
	if s.names == nil {
		s.names = make(map[string]LocalMacroExpander)
	}
	s.names[name.Str] = mac
}

// lookup reports whether name is lexically bound, whether by a macrolet,
// and the local macro's expander.
func (w *CodeWalker) lookup(name string) (bound, macro bool, mac LocalMacroExpander) {
	for i := len(w.scopes) - 1; i >= 0; i-- {
		if m, ok := w.scopes[i].names[name]; ok {
			return true, w.scopes[i].macros, m
		}
	}
	return false, false, nil
}

func (w *CodeWalker) isBound(sym *LVal) bool {
	bound, _, _ := w.lookup(sym.Str)
	return bound
}

func (w *CodeWalker) emitBind(name *LVal, op string, depth int, mac LocalMacroExpander) *LVal {
	if name == nil || name.Type != LSymbol {
		return name
	}
	w.bind(name, mac)
	return w.emit(WalkNode{Event: WalkBind, Node: name, Op: op, Shape: SpecialFormShape(op), Depth: depth})
}

// emit visits a WalkRef, WalkSet or WalkBind event and returns the node to
// put in its place: Replace's result, or the node itself.
func (w *CodeWalker) emit(n WalkNode) *LVal {
	w.visit(n)
	if w.Replace != nil {
		if r := w.Replace(&n); r != nil {
			return r
		}
	}
	return n.Node
}

// form walks one value in code position.
func (w *CodeWalker) form(v *LVal, depth int) *LVal {
	if w.err != nil || v == nil {
		return v
	}
	if depth > w.maxDepth() {
		if w.KeepGoing {
			w.visit(WalkNode{Event: WalkForm, Node: v, Opaque: true, Depth: depth})
			return v
		}
		return w.fail(Errorf("code nesting depth exceeds maximum: %d", w.maxDepth()))
	}
	switch {
	case v.quoted || v.Type == LQuote:
		w.visit(WalkNode{Event: WalkData, Node: v, Depth: depth})
		return v
	case v.Type == LSymbol:
		if isKeyword(v.Str) {
			w.visit(WalkNode{Event: WalkLiteral, Node: v, Depth: depth})
		} else {
			return w.emit(WalkNode{Event: WalkRef, Node: v, Depth: depth, Bound: w.isBound(v)})
		}
		return v
	case v.Type != LSExpr || len(v.Cells) == 0:
		w.visit(WalkNode{Event: WalkLiteral, Node: v, Depth: depth})
		return v
	}

	// A compound form.  Expand its head until it is no longer a macro,
	// classifying it on the way: op is the special form it names, if any.
	op, isOp := "", false
	for n := 0; ; n++ {
		head := v.Cells[0]
		if head.Type != LSymbol || head.quoted {
			break
		}
		var exp *LVal
		if bound, macro, mac := w.lookup(head.Str); bound {
			if !macro {
				break
			}
			if mac == nil {
				w.visit(WalkNode{Event: WalkForm, Node: v, Opaque: true, Depth: depth})
				return v
			}
			if n >= w.maxExpansions() {
				return w.expansionFailed(v, depth, Errorf("macro expansion depth exceeds maximum: %d", w.maxExpansions()))
			}
			if exp = mac(v); exp == nil {
				w.visit(WalkNode{Event: WalkForm, Node: v, Opaque: true, Depth: depth})
				return v
			}
		} else {
			if op, isOp = w.specialOp(head); isOp || w.Expand1 == nil {
				break
			}
			if n >= w.maxExpansions() {
				return w.expansionFailed(v, depth, Errorf("macro expansion depth exceeds maximum: %d", w.maxExpansions()))
			}
			var ok bool
			if exp, ok = w.Expand1(v); !ok {
				break
			}
			if exp == nil {
				exp = Nil()
			}
		}
		if exp.Type == LError {
			return w.expansionFailed(v, depth, exp)
		}
		if exp.Type != LSExpr || exp.quoted || len(exp.Cells) == 0 {
			return w.form(exp, depth)
		}
		v = exp
	}

	if !isOp {
		if !w.visit(WalkNode{Event: WalkForm, Node: v, Depth: depth}) {
			return v
		}
		return w.call(v, depth)
	}
	shape := SpecialFormShape(op)
	if shape == ShapeUnknown {
		w.visit(WalkNode{Event: WalkForm, Node: v, Op: op, Opaque: true, Depth: depth})
		return v
	}
	if !w.visit(WalkNode{Event: WalkForm, Node: v, Op: op, Shape: shape, Depth: depth}) {
		return v
	}
	return w.special(v, op, shape, depth)
}

// expansionFailed stops the walk with err, or, with KeepGoing, walks v
// unexpanded as a function call.
func (w *CodeWalker) expansionFailed(v *LVal, depth int, err *LVal) *LVal {
	if !w.KeepGoing {
		return w.fail(err)
	}
	if !w.visit(WalkNode{Event: WalkForm, Node: v, Depth: depth}) {
		return v
	}
	return w.call(v, depth)
}

func (w *CodeWalker) specialOp(head *LVal) (string, bool) {
	if w.SpecialOp != nil {
		return w.SpecialOp(head)
	}
	return DefaultSpecialOpName(head)
}

// call walks a function call: the head is a reference, the arguments code.
func (w *CodeWalker) call(v *LVal, depth int) *LVal {
	b := newRebuild(v)
	head := v.Cells[0]
	if head.Type == LSymbol && !head.quoted && !isKeyword(head.Str) {
		b.set(0, w.emit(WalkNode{Event: WalkRef, Node: head, Head: true, Depth: depth + 1, Bound: w.isBound(head)}))
	} else {
		b.set(0, w.form(head, depth+1))
	}
	w.forms(b, 1, depth)
	return b.done()
}

// forms walks v's cells from index i on as code.
func (w *CodeWalker) forms(b *rebuild, i int, depth int) {
	for ; i < len(b.orig.Cells); i++ {
		b.set(i, w.form(b.orig.Cells[i], depth+1))
	}
}

func (w *CodeWalker) data(v *LVal, depth int) {
	if v != nil {
		w.visit(WalkNode{Event: WalkData, Node: v, Depth: depth})
	}
}

func (w *CodeWalker) special(v *LVal, op string, shape FormShape, depth int) *LVal {
	b := newRebuild(v)
	cells := v.Cells
	d := depth + 1
	// The scopes special() opens are function bodies except dotimes,
	// test and benchmark (whose bodies the test runner calls, never a
	// handler).  let, flet and friends open their scopes in let and flet.
	fn := shape == ShapeLambda || shape == ShapeDefun || shape == ShapeExpr
	enter := func(node *LVal, macros bool) {
		w.push(macros)
		w.visit(WalkNode{Event: WalkEnter, Node: node, Op: op, Shape: shape, Depth: depth, Function: fn})
	}
	leave := func(node *LVal) {
		w.visit(WalkNode{Event: WalkLeave, Node: node, Op: op, Shape: shape, Depth: depth, Function: fn})
		w.pop()
	}
	switch shape {
	case ShapeForms:
		w.forms(b, 1, depth)
	case ShapeData:
		for _, c := range cells[1:] {
			w.data(c, d)
		}
	case ShapeQuote:
		for _, c := range cells[1:] {
			w.data(c, d)
		}
	case ShapeQuasiquote:
		for i := 1; i < len(cells); i++ {
			b.set(i, w.template(cells[i], d))
		}
	case ShapeFunction:
		for i := 1; i < len(cells); i++ {
			if c := cells[i]; c.Type == LSymbol {
				b.set(i, w.emit(WalkNode{Event: WalkRef, Node: c, Depth: d, Bound: w.isBound(c)}))
			} else {
				w.data(c, d)
			}
		}
	case ShapeSetBang:
		if len(cells) > 1 {
			if cells[1].Type == LSymbol {
				b.set(1, w.emit(WalkNode{Event: WalkSet, Node: cells[1], Depth: d, Bound: w.isBound(cells[1])}))
			} else {
				w.data(cells[1], d)
			}
			w.forms(b, 2, depth)
		}
	case ShapeLambda:
		if len(cells) > 1 {
			enter(v, false)
			b.set(1, w.formals(cells[1], op, d))
			w.forms(b, 2, depth)
			leave(v)
		}
	case ShapeDefun:
		if len(cells) > 1 {
			if cells[1].Type == LSymbol {
				w.visit(WalkNode{Event: WalkDefine, Node: cells[1], Op: op, Shape: shape, Depth: d})
			} else {
				w.data(cells[1], d)
			}
		}
		if len(cells) > 2 {
			enter(v, false)
			b.set(2, w.formals(cells[2], op, d))
			w.forms(b, 3, depth)
			leave(v)
		}
	case ShapeTest:
		if len(cells) > 1 {
			w.data(cells[1], d)
			enter(v, false)
			w.forms(b, 2, depth)
			leave(v)
		}
	case ShapeBenchmark:
		if len(cells) > 2 {
			w.data(cells[1], d)
			enter(v, false)
			b.set(2, w.formals(cells[2], op, d))
			w.forms(b, 3, depth)
			leave(v)
		} else {
			for _, c := range cells[1:] {
				w.data(c, d)
			}
		}
	case ShapeLet, ShapeLetSeq:
		if len(cells) > 1 {
			b.set(1, w.let(cells[1], v, op, shape == ShapeLetSeq, d, func() { w.forms(b, 2, depth) }))
		}
	case ShapeFlet, ShapeLabels, ShapeMacrolet:
		if len(cells) > 1 {
			b.set(1, w.flet(cells[1], v, op, shape, d, func() { w.forms(b, 2, depth) }))
		}
	case ShapeHandlerBind:
		if len(cells) > 1 {
			b.set(1, w.pairs(cells[1], d, func(pb *rebuild, pair *LVal) {
				if len(pair.Cells) > 0 {
					w.data(pair.Cells[0], d+1)
				}
				w.forms(pb, 1, d)
			}))
			w.forms(b, 2, depth)
		}
	case ShapeCond:
		for i := 1; i < len(cells); i++ {
			clause := cells[i]
			// A clause is structure even when written [test body...],
			// which reads as a quoted list: opCond reads its cells.
			if clause.Type != LSExpr || len(clause.Cells) == 0 {
				b.set(i, w.form(clause, d))
				continue
			}
			cb := newRebuild(clause)
			if t := clause.Cells[0]; t.Type == LSymbol && !t.quoted && (t.Str == "else" || t.Str == ":else") {
				w.visit(WalkNode{Event: WalkLiteral, Node: t, Depth: d + 1})
			} else {
				cb.set(0, w.form(t, d+1))
			}
			w.forms(cb, 1, d)
			b.set(i, cb.done())
		}
	case ShapeDotimes:
		if len(cells) > 1 {
			ctrl := cells[1]
			if ctrl.Type != LSExpr {
				w.data(ctrl, d)
				w.forms(b, 2, depth)
				break
			}
			cb := newRebuild(ctrl)
			if len(ctrl.Cells) > 1 {
				cb.set(1, w.form(ctrl.Cells[1], d+1))
			}
			enter(v, false)
			if len(ctrl.Cells) > 0 {
				cb.set(0, w.emitBind(ctrl.Cells[0], op, d+1, nil))
			}
			w.forms(b, 2, depth)
			if len(ctrl.Cells) > 2 {
				for i := 2; i < len(ctrl.Cells); i++ {
					cb.set(i, w.form(ctrl.Cells[i], d+1))
				}
			}
			leave(v)
			b.set(1, cb.done())
		}
	case ShapeExpr:
		if len(cells) > 1 {
			enter(v, false)
			for _, name := range exprFormalNames(cells[1]) {
				w.emitBind(name, op, d, nil)
			}
			w.forms(b, 1, depth)
			leave(v)
		}
	case ShapeWithCleanup:
		if len(cells) > 1 {
			cl := cells[1]
			if cl.Type == LSExpr {
				cb := newRebuild(cl)
				w.forms(cb, 0, d)
				b.set(1, cb.done())
			} else {
				b.set(1, w.form(cl, d))
			}
			w.forms(b, 2, depth)
		}
	case ShapeThread:
		if len(cells) > 1 {
			b.set(1, w.form(cells[1], d))
		}
		for i := 2; i < len(cells); i++ {
			step := cells[i]
			switch {
			case step.Type == LSExpr && !step.quoted && len(step.Cells) > 0:
				// Step heads are regular functions: never expanded.
				if !w.visit(WalkNode{Event: WalkForm, Node: step, Depth: d}) {
					continue
				}
				b.set(i, w.call(step, d))
			default:
				b.set(i, w.form(step, d))
			}
		}
	default: // ShapeUnknown forms are opaque and never reach here
	}
	return b.done()
}

// formals binds the names in a lambda list and returns it with any names
// Replace substituted.  Markers are skipped.
func (w *CodeWalker) formals(formals *LVal, op string, depth int) *LVal {
	if formals == nil || formals.Type != LSExpr {
		return formals
	}
	fb := newRebuild(formals)
	for i, f := range formals.Cells {
		if f.Type != LSymbol {
			continue
		}
		switch f.Str {
		case OptArgSymbol, VarArgSymbol, KeyArgSymbol:
			continue
		}
		fb.set(i, w.emitBind(f, op, depth, nil))
	}
	return fb.done()
}

// pairList holds the rebuilds of a binding list and of each well-formed
// (list) binding in it, so a form can walk the bindings in more than one
// pass (inits, then names) and still return one rebuilt list.
type pairList struct {
	lb  *rebuild
	pbs []*rebuild // nil for a binding that is not a list
}

func (w *CodeWalker) newPairList(list *LVal, depth int) *pairList {
	pl := &pairList{lb: newRebuild(list), pbs: make([]*rebuild, len(list.Cells))}
	for i, pair := range list.Cells {
		if pair.Type == LSExpr {
			pl.pbs[i] = newRebuild(pair)
		} else {
			w.data(pair, depth+1)
		}
	}
	return pl
}

// each calls fn for each well-formed binding in order.
func (pl *pairList) each(fn func(pb *rebuild, pair *LVal)) {
	for _, pb := range pl.pbs {
		if pb != nil {
			fn(pb, pb.orig)
		}
	}
}

func (pl *pairList) done() *LVal {
	for i, pb := range pl.pbs {
		if pb != nil {
			pl.lb.set(i, pb.done())
		}
	}
	return pl.lb.done()
}

// pairs walks a list of binding pairs, calling fn with each well-formed
// pair and a rebuild over it.  It returns the rebuilt list.
func (w *CodeWalker) pairs(list *LVal, depth int, fn func(pb *rebuild, pair *LVal)) *LVal {
	if list == nil || list.Type != LSExpr {
		w.data(list, depth)
		return list
	}
	pl := w.newPairList(list, depth)
	pl.each(fn)
	return pl.done()
}

// let walks a let or let* binding list and then the body.
func (w *CodeWalker) let(list, form *LVal, op string, seq bool, depth int, body func()) *LVal {
	sh := SpecialFormShape(op)
	if list == nil || list.Type != LSExpr {
		w.data(list, depth)
		w.push(false)
		body()
		w.pop()
		return list
	}
	pl := w.newPairList(list, depth)
	bindName := func(pb *rebuild, pair *LVal) {
		if len(pair.Cells) > 0 {
			pb.set(0, w.emitBind(pair.Cells[0], op, depth+2, nil))
		}
	}
	if seq {
		w.push(false)
		w.visit(WalkNode{Event: WalkEnter, Node: form, Op: op, Shape: sh, Depth: depth - 1})
		pl.each(func(pb *rebuild, pair *LVal) {
			w.forms(pb, 1, depth+1)
			bindName(pb, pair)
		})
	} else {
		pl.each(func(pb *rebuild, _ *LVal) { w.forms(pb, 1, depth+1) })
		w.push(false)
		w.visit(WalkNode{Event: WalkEnter, Node: form, Op: op, Shape: sh, Depth: depth - 1})
		pl.each(bindName)
	}
	body()
	w.visit(WalkNode{Event: WalkLeave, Node: form, Op: op, Shape: sh, Depth: depth - 1})
	w.pop()
	return pl.done()
}

// flet walks a flet, labels or macrolet binding list and then the body.
func (w *CodeWalker) flet(list, form *LVal, op string, shape FormShape, depth int, body func()) *LVal {
	sh := SpecialFormShape(op)
	enter := func(macros bool) {
		w.push(macros)
		w.visit(WalkNode{Event: WalkEnter, Node: form, Op: op, Shape: sh, Depth: depth - 1})
	}
	leave := func() {
		w.visit(WalkNode{Event: WalkLeave, Node: form, Op: op, Shape: sh, Depth: depth - 1})
		w.pop()
	}
	if list == nil || list.Type != LSExpr {
		w.data(list, depth)
		enter(shape == ShapeMacrolet)
		body()
		leave()
		return list
	}
	pl := w.newPairList(list, depth)
	// One function binding: (name formals body...).
	fn := func(pb *rebuild, bind *LVal) {
		if len(bind.Cells) < 2 {
			return
		}
		w.push(false)
		w.visit(WalkNode{Event: WalkEnter, Node: bind, Op: op, Shape: sh, Depth: depth + 1, Function: true})
		pb.set(1, w.formals(bind.Cells[1], op, depth+2))
		w.forms(pb, 2, depth+1)
		w.visit(WalkNode{Event: WalkLeave, Node: bind, Op: op, Shape: sh, Depth: depth + 1, Function: true})
		w.pop()
	}
	bindName := func(pb *rebuild, bind *LVal) {
		if len(bind.Cells) == 0 {
			return
		}
		var mac LocalMacroExpander
		if shape == ShapeMacrolet && w.DefineLocalMacro != nil && w.err == nil {
			m, lerr := w.DefineLocalMacro(bind)
			if lerr != nil && lerr.Type == LError {
				w.fail(lerr)
			}
			mac = m
		}
		pb.set(0, w.emitBind(bind.Cells[0], op, depth+1, mac))
	}
	switch shape {
	case ShapeLabels:
		enter(false)
		pl.each(bindName)
		pl.each(fn)
	default:
		pl.each(fn)
		enter(shape == ShapeMacrolet)
		pl.each(bindName)
	}
	body()
	leave()
	return pl.done()
}

// template walks a quasiquote template.  As at run time (docs/lang.md,
// "Quasiquote traversal"), every bare unquote and unquote-splicing in the
// template is a hole evaluated by this quasiquote: nested quasiquote,
// quote and reader quotes do not delay them, and lisp:unquote is data.
func (w *CodeWalker) template(v *LVal, depth int) *LVal {
	if w.err != nil || v == nil {
		return v
	}
	if depth > w.maxDepth() {
		if w.KeepGoing {
			return v
		}
		return w.fail(Errorf("code nesting depth exceeds maximum: %d", w.maxDepth()))
	}
	if (v.Type != LSExpr && v.Type != LQuote) || len(v.Cells) == 0 {
		return v
	}
	b := newRebuild(v)
	if h := v.Cells[0]; v.Type == LSExpr && h.Type == LSymbol && len(v.Cells) == 2 &&
		(h.Str == "unquote" || h.Str == "unquote-splicing") {
		b.set(1, w.form(v.Cells[1], depth+1))
		return b.done()
	}
	for i, c := range v.Cells {
		b.set(i, w.template(c, depth+1))
	}
	return b.done()
}

// exprFormalNames returns the formals (expr pattern) binds, as opExpr
// computes them.  A pattern opExpr would reject binds nothing.
func exprFormalNames(pattern *LVal) []*LVal {
	n, short, nopt, vargs, err := countExprArgs(pattern)
	if err != nil {
		return nil
	}
	var names []*LVal
	if short {
		names = append(names, Symbol("%"))
	} else {
		if n > MaxExprFormals {
			n = MaxExprFormals
		}
		for i := 1; i <= n; i++ {
			names = append(names, Symbol("%"+strconv.Itoa(i)))
		}
	}
	if nopt > 0 {
		names = append(names, Symbol("%"+OptArgSymbol))
	}
	if vargs {
		names = append(names, Symbol("%"+VarArgSymbol))
	}
	return names
}

// rebuild collects the walked children of one list, allocating a fresh
// header and cells array only when a child changed.  The input is never
// written.
type rebuild struct {
	orig  *LVal
	cells []*LVal
}

func newRebuild(orig *LVal) *rebuild {
	return &rebuild{orig: orig}
}

func (b *rebuild) set(i int, v *LVal) {
	if b.cells == nil {
		if v == b.orig.Cells[i] {
			return
		}
		b.cells = make([]*LVal, len(b.orig.Cells))
		copy(b.cells, b.orig.Cells)
	}
	b.cells[i] = v
}

// done returns the original list when nothing changed, or a fresh, unsealed
// list with the original's type, quoting and (a copy of its) source
// location.  Formatting metadata and debugger expansion records are not
// carried over: a rebuilt list is code, not a formatted source node.
func (b *rebuild) done() *LVal {
	if b.cells == nil {
		return b.orig
	}
	return &LVal{
		Type:   b.orig.Type,
		Cells:  b.cells,
		quoted: b.orig.quoted,
		source: copyLocation(b.orig.source),
	}
}

// MacroExpandAll returns form with every macro call expanded, in env: the
// head, and every nested form in code position, recursively, until no macro
// call remains.  It is the Go API behind macroexpand-all.
//
// Heads resolve in env, so a package that shadows a builtin or defines its
// own macros is honored, and a lexical binding of a macro name (a let, flet,
// labels or lambda parameter inside form) suppresses expansion of that name
// within its scope.  Local macros bound by macrolet inside form are
// expanded; each is built in env the way macrolet builds it at run time,
// so a local macro whose expansion reads a runtime local variable cannot be
// expanded statically and returns an error.  defun and defmacro are kept as
// written, with their bodies expanded, because their own expansion embeds
// a compiled function rather than source.  Quoted data is never entered.
//
// form is not modified: see CodeWalker for what the result shares with it.
// A failed expansion returns the LError.
func (env *LEnv) MacroExpandAll(form *LVal) *LVal {
	maxDepth := env.Runtime.MaxEvalNestingDepth()
	if maxDepth == 0 { // the embedder disabled the nesting limit
		maxDepth = math.MaxInt
	}
	w := &CodeWalker{
		SpecialOp:        env.resolveSpecialOp,
		Expand1:          env.expandOnce,
		DefineLocalMacro: env.defineLocalMacro,
		MaxDepth:         maxDepth,
		MaxExpansions:    env.Runtime.MaxMacroExpansions(),
	}
	return w.Walk(form)
}

// resolveSpecialOp reports the builtin form head denotes in env.
func (env *LEnv) resolveSpecialOp(head *LVal) (string, bool) {
	v := env.Get(head)
	if v.Type != LFun || v.Builtin() == nil || v.Package() != env.Runtime.Registry.Lang {
		return "", false
	}
	// Get names the value after the symbol it was looked up by, so a
	// qualified lookup reports "lisp:if".
	name := v.Str
	if rest, ok := strings.CutPrefix(name, env.Runtime.Registry.Lang+":"); ok {
		name = rest
	}
	switch v.FunType {
	case LFunSpecialOp:
		return name, true
	case LFunMacro:
		if SpecialFormShape(name) == ShapeDefun {
			return name, true
		}
	default:
	}
	return "", false
}

// expandOnce expands form once if its head is a macro in env.
func (env *LEnv) expandOnce(form *LVal) (*LVal, bool) {
	mac := env.Get(form.Cells[0])
	if IsInternalPanic(mac) {
		return mac, true
	}
	if mac.Type != LFun || !mac.IsMacro() {
		return nil, false
	}
	return env.callMacro(mac, form)
}

func (env *LEnv) callMacro(mac, form *LVal) (*LVal, bool) {
	if lerr := env.checkLimits(env.evalCtx); lerr != nil {
		return lerr, true
	}
	mark := env.MacroCall(mac, macroArgList(form))
	if mark.Type == LError {
		return mark, true
	}
	if mark.Type != LMarkMacExpand {
		return env.Errorf("internal error: macro did not return expansion marker: %v", mark.Type), true
	}
	return mark.Cells[0], true
}

// defineLocalMacro builds one macrolet binding's macro as opMacrolet does.
func (env *LEnv) defineLocalMacro(bind *LVal) (LocalMacroExpander, *LVal) {
	if len(bind.Cells) < 2 || bind.Cells[0].Type != LSymbol {
		return nil, nil
	}
	fenv := NewEnv(env)
	mac := fenv.Lambda(bind.Cells[1], bind.Cells[2:])
	if mac.Type == LError {
		return nil, mac
	}
	mac.FunType = LFunMacro //elps:mutates evaluate as a macro: mac is the closure fenv.Lambda freshly allocated above
	return func(form *LVal) *LVal {
		exp, _ := env.callMacro(mac, form)
		return exp
	}, nil
}

func builtinMacroExpandAll(env *LEnv, args *LVal) *LVal {
	form := args.Cells[0]
	if form.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", form.Type)
	}
	if form.IsNil() {
		return form
	}
	// The evaluated argument of (macroexpand-all '(...)) is quoted data;
	// walk the code it spells.  shallowUnquote shares the (possibly sealed)
	// cells array, which the walk never writes.
	code := form
	if form.quoted {
		code = shallowUnquote(form)
	}
	r := env.MacroExpandAll(code)
	if r.Type == LError {
		return r
	}
	if r == code {
		return form
	}
	return Quote(r)
}
