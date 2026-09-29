// Copyright © 2026 The ELPS authors

package lisp

import (
	"math"
	"strconv"
	"strings"
)

// formKind is one builtin special form: each special operator in
// langSpecialOps, plus the two builtin definition macros (defun, defmacro)
// whose expansion embeds a function value rather than source, so walkers
// keep them as written.  kindNone is a form the walker does not know (an
// embedder's special operator): it is opaque, and none of its arguments are
// walked.
//
// Every switch over a formKind lists every kind and has no default arm, so
// the exhaustive linter reports a switch that misses a newly added kind
// (TestFormKindSwitchesHaveNoDefault enforces the no-default rule, which
// .golangci.yml's default-signifies-exhaustive would otherwise let hide a
// gap), and TestEverySpecialOpHasAKind fails until a new special operator
// gets a kind.
type formKind uint8

// The builtin special forms.
const (
	kindNone            formKind = iota
	kindFunction                 // function
	kindSetBang                  // set!
	kindAssert                   // assert
	kindQuote                    // quote
	kindQuasiquote               // quasiquote
	kindLambda                   // lambda
	kindExpr                     // expr
	kindThreadFirst              // thread-first
	kindThreadLast               // thread-last
	kindDotimes                  // dotimes
	kindLabels                   // labels
	kindMacrolet                 // macrolet
	kindFlet                     // flet
	kindLetSeq                   // let*
	kindLet                      // let
	kindProgn                    // progn
	kindHandlerBind              // handler-bind
	kindIgnoreErrors             // ignore-errors
	kindWithCleanup              // with-cleanup
	kindCond                     // cond
	kindIf                       // if
	kindWhen                     // when
	kindUnless                   // unless
	kindDefault                  // default
	kindWhile                    // while
	kindOr                       // or
	kindAnd                      // and
	kindHelp                     // help
	kindTest                     // test
	kindBenchmark                // benchmark
	kindQualifiedSymbol          // qualified-symbol
	kindDefun                    // defun
	kindDefmacro                 // defmacro
)

// formKinds maps each builtin special form's name to its kind.
var formKinds = map[string]formKind{
	"function":         kindFunction,
	"set!":             kindSetBang,
	"assert":           kindAssert,
	"quote":            kindQuote,
	"quasiquote":       kindQuasiquote,
	"lambda":           kindLambda,
	"expr":             kindExpr,
	"thread-first":     kindThreadFirst,
	"thread-last":      kindThreadLast,
	"dotimes":          kindDotimes,
	"labels":           kindLabels,
	"macrolet":         kindMacrolet,
	"flet":             kindFlet,
	"let*":             kindLetSeq,
	"let":              kindLet,
	"progn":            kindProgn,
	"handler-bind":     kindHandlerBind,
	"ignore-errors":    kindIgnoreErrors,
	"with-cleanup":     kindWithCleanup,
	"cond":             kindCond,
	"if":               kindIf,
	"when":             kindWhen,
	"unless":           kindUnless,
	"default":          kindDefault,
	"while":            kindWhile,
	"or":               kindOr,
	"and":              kindAnd,
	"help":             kindHelp,
	"test":             kindTest,
	"benchmark":        kindBenchmark,
	"qualified-symbol": kindQualifiedSymbol,
	"defun":            kindDefun,
	"defmacro":         kindDefmacro,
}

// specialFormKind returns the kind of the builtin special form named name,
// unqualified, or kindNone.
func specialFormKind(name string) formKind {
	return formKinds[name]
}

// opensFunction reports whether the scope a form of kind k opens is a
// function body -- code that may run later, from wherever the function is
// called.  The body of a let, flet, dotimes, test or benchmark is a scope
// but not a function (test and benchmark bodies are run by the test runner,
// never by a handler).
func (k formKind) opensFunction() bool {
	switch k {
	case kindLambda, kindExpr, kindDefun, kindDefmacro:
		return true
	case kindNone, kindFunction, kindSetBang, kindAssert, kindQuote, kindQuasiquote,
		kindThreadFirst, kindThreadLast, kindDotimes, kindLabels, kindMacrolet, kindFlet,
		kindLetSeq, kindLet, kindProgn, kindHandlerBind, kindIgnoreErrors, kindWithCleanup,
		kindCond, kindIf, kindWhen, kindUnless, kindDefault, kindWhile, kindOr, kindAnd,
		kindHelp, kindTest, kindBenchmark, kindQualifiedSymbol:
		return false
	}
	return false
}

// defaultSpecialOpName is the static classifier CodeWalker uses when no
// environment is available.  It reports the builtin form a head symbol
// names, reading an unqualified name or one qualified by the lisp package
// as the builtin.  It cannot see a package that shadows a builtin name; a
// walker with an environment (LEnv.MacroExpandAll) resolves heads instead.
func defaultSpecialOpName(head *LVal) (string, bool) {
	if head == nil || head.Type != LSymbol || head.quoted {
		return "", false
	}
	name := head.Str
	if rest, ok := strings.CutPrefix(name, DefaultLangPackage+":"); ok {
		name = rest
	} else if strings.Contains(name, ":") {
		return "", false
	}
	if _, ok := formKinds[name]; ok {
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

// localMacroExpander is the compiled macro a macrolet binding defines,
// expanded through the walker's environment.
type localMacroExpander = *LVal

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
	// function or macro.  Nil means defaultSpecialOpName.
	SpecialOp func(head *LVal) (string, bool)

	// Expand1 expands form once when its head names a macro.  ok is false
	// when it is not a macro call.  An expansion that fails returns an
	// LError, which stops the walk.  Nil disables expansion: Walk then only
	// visits.
	Expand1 func(form *LVal) (expansion *LVal, ok bool)

	// env, when set (MacroExpandAll), compiles macrolet bindings so calls
	// to local macros are expanded.  Without it they are left unexpanded
	// and their arguments are not walked.
	env *LEnv

	// Visit receives the walk's events.  It may be nil.
	Visit CodeVisitor

	err     *LVal
	scopes  []walkScope
	memo    map[walkMemoKey]*LVal
	step    func() *LVal // charges one step; nil: free
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

	nextScope int
}

type walkScope struct {
	names  map[string]localMacroExpander
	id     int
	macros bool
}

// walkMemoKey identifies one walk of a shared node: the same node in the
// same scope (and the same mode, code or quasiquote template) walks to the
// same result.
type walkMemoKey struct {
	node     *LVal
	scope    int
	template bool
}

func (w *CodeWalker) scopeID() int {
	if len(w.scopes) == 0 {
		return 0
	}
	return w.scopes[len(w.scopes)-1].id
}

// charge accounts for walking one list, through the step hook
// MacroExpandAll installs.
func (w *CodeWalker) charge() *LVal {
	if w.step == nil {
		return nil
	}
	return w.step()
}

// Walk walks form as code and returns it with every macro call expanded.
// A failed expansion returns the LError.
func (w *CodeWalker) Walk(form *LVal) *LVal {
	w.scopes = w.scopes[:0]
	w.err = nil
	w.memo = nil
	w.nextScope = 0
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
	w.nextScope++
	w.scopes = append(w.scopes, walkScope{macros: macros, id: w.nextScope})
}

func (w *CodeWalker) pop() {
	w.scopes = w.scopes[:len(w.scopes)-1]
}

// bind records name in the innermost scope.  mac is the local macro's
// expander (nil for a variable or function, or a local macro that cannot be
// expanded).
func (w *CodeWalker) bind(name *LVal, mac localMacroExpander) {
	if name == nil || name.Type != LSymbol || len(w.scopes) == 0 {
		return
	}
	s := &w.scopes[len(w.scopes)-1]
	if s.names == nil {
		s.names = make(map[string]localMacroExpander)
	}
	s.names[name.Str] = mac
}

// lookup reports whether name is lexically bound, whether by a macrolet,
// and the local macro's expander.
func (w *CodeWalker) lookup(name string) (bound, macro bool, mac localMacroExpander) {
	// As in LEnv.Get, a qualified symbol or keyword resolves in a package
	// (or to itself), never in a lexical scope.
	if strings.IndexByte(name, ':') >= 0 {
		return false, false, nil
	}
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

func (w *CodeWalker) emitBind(name *LVal, op string, depth int, mac localMacroExpander) {
	if name == nil || name.Type != LSymbol {
		return
	}
	w.bind(name, mac)
	w.visit(WalkNode{Event: WalkBind, Node: name, Op: op, Depth: depth})
}

// form walks one value in code position.
func (w *CodeWalker) form(v *LVal, depth int) *LVal {
	if w.err != nil || v == nil {
		return v
	}
	if depth > w.maxDepth() {
		if w.KeepGoing {
			w.visit(WalkNode{Event: WalkForm, Node: v, Depth: depth})
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
			w.visit(WalkNode{Event: WalkRef, Node: v, Depth: depth, Bound: w.isBound(v)})
		}
		return v
	case v.Type != LSExpr || len(v.Cells) == 0:
		w.visit(WalkNode{Event: WalkLiteral, Node: v, Depth: depth})
		return v
	}

	// Code built by macros can share structure (a DAG).  A node reached
	// twice in one scope walks to the same result, so it is walked once;
	// otherwise shared structure costs a walk per path, exponential in
	// the depth of sharing.
	key := walkMemoKey{node: v, scope: w.scopeID()}
	if r, ok := w.memo[key]; ok {
		return r
	}
	if lerr := w.charge(); lerr != nil {
		return w.fail(lerr)
	}
	r := w.compound(v, depth)
	if w.memo == nil {
		w.memo = make(map[walkMemoKey]*LVal)
	}
	w.memo[key] = r
	return r
}

// compound walks a non-empty, unquoted list in code position.
func (w *CodeWalker) compound(v *LVal, depth int) *LVal {
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
				w.visit(WalkNode{Event: WalkForm, Node: v, Depth: depth})
				return v
			}
			if n >= w.maxExpansions() {
				return w.expansionFailed(v, depth, Errorf("macro expansion depth exceeds maximum: %d", w.maxExpansions()))
			}
			if exp, _ = w.env.callMacro(mac, v); exp == nil {
				w.visit(WalkNode{Event: WalkForm, Node: v, Depth: depth})
				return v
			}
		} else {
			if op, isOp = w.specialOp(head); isOp || w.Expand1 == nil {
				break
			}
			// Only Expand1 knows whether the head is a macro, so the
			// limit is checked once it has expanded: a chain of exactly
			// MaxExpansions expansions ending in a function call is fine.
			var ok bool
			if exp, ok = w.Expand1(v); !ok {
				break
			}
			if n >= w.maxExpansions() {
				return w.expansionFailed(v, depth, Errorf("macro expansion depth exceeds maximum: %d", w.maxExpansions()))
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
	kind := specialFormKind(op)
	if kind == kindNone {
		w.visit(WalkNode{Event: WalkForm, Node: v, Op: op, Depth: depth})
		return v
	}
	if !w.visit(WalkNode{Event: WalkForm, Node: v, Op: op, Depth: depth}) {
		return v
	}
	return w.special(v, op, kind, depth)
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
	return defaultSpecialOpName(head)
}

// call walks a function call: the head is a reference, the arguments code.
func (w *CodeWalker) call(v *LVal, depth int) *LVal {
	b := newRebuild(v)
	head := v.Cells[0]
	if head.Type == LSymbol && !head.quoted && !isKeyword(head.Str) {
		w.visit(WalkNode{Event: WalkRef, Node: head, Head: true, Depth: depth + 1, Bound: w.isBound(head)})
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

func (w *CodeWalker) special(v *LVal, op string, kind formKind, depth int) *LVal {
	b := newRebuild(v)
	cells := v.Cells
	d := depth + 1
	// let, flet and friends open their scopes in let and flet.
	fn := kind.opensFunction()
	enter := func(node *LVal, macros bool) {
		w.push(macros)
		w.visit(WalkNode{Event: WalkEnter, Node: node, Op: op, Depth: depth, Function: fn})
	}
	leave := func(node *LVal) {
		w.visit(WalkNode{Event: WalkLeave, Node: node, Op: op, Depth: depth, Function: fn})
		w.pop()
	}
	switch kind {
	case kindAssert, kindProgn, kindIgnoreErrors, kindIf, kindWhen, kindUnless,
		kindDefault, kindWhile, kindOr, kindAnd:
		w.forms(b, 1, depth)
	case kindHelp, kindQualifiedSymbol:
		for _, c := range cells[1:] {
			w.data(c, d)
		}
	case kindQuote:
		for _, c := range cells[1:] {
			w.data(c, d)
		}
	case kindQuasiquote:
		for i := 1; i < len(cells); i++ {
			b.set(i, w.template(cells[i], d))
		}
	case kindFunction:
		for _, c := range cells[1:] {
			if c.Type == LSymbol {
				w.visit(WalkNode{Event: WalkRef, Node: c, Depth: d, Bound: w.isBound(c)})
			} else {
				w.data(c, d)
			}
		}
	case kindSetBang:
		if len(cells) > 1 {
			if cells[1].Type == LSymbol {
				w.visit(WalkNode{Event: WalkSet, Node: cells[1], Depth: d, Bound: w.isBound(cells[1])})
			} else {
				w.data(cells[1], d)
			}
			w.forms(b, 2, depth)
		}
	case kindLambda:
		if len(cells) > 1 {
			enter(v, false)
			w.formals(cells[1], op, d)
			w.forms(b, 2, depth)
			leave(v)
		}
	case kindDefun, kindDefmacro:
		if len(cells) > 1 {
			if cells[1].Type == LSymbol {
				w.visit(WalkNode{Event: WalkDefine, Node: cells[1], Op: op, Depth: d})
			} else {
				w.data(cells[1], d)
			}
		}
		if len(cells) > 2 {
			enter(v, false)
			w.formals(cells[2], op, d)
			w.forms(b, 3, depth)
			leave(v)
		}
	case kindTest:
		if len(cells) > 1 {
			w.data(cells[1], d)
			enter(v, false)
			w.forms(b, 2, depth)
			leave(v)
		}
	case kindBenchmark:
		if len(cells) > 2 {
			w.data(cells[1], d)
			enter(v, false)
			w.formals(cells[2], op, d)
			w.forms(b, 3, depth)
			leave(v)
		} else {
			for _, c := range cells[1:] {
				w.data(c, d)
			}
		}
	case kindLet, kindLetSeq:
		if len(cells) > 1 {
			b.set(1, w.let(cells[1], v, op, kind == kindLetSeq, d, func() { w.forms(b, 2, depth) }))
		}
	case kindFlet, kindLabels, kindMacrolet:
		if len(cells) > 1 {
			b.set(1, w.flet(cells[1], v, op, kind, d, func() { w.forms(b, 2, depth) }))
		}
	case kindHandlerBind:
		if len(cells) > 1 {
			b.set(1, w.pairs(cells[1], d, func(pb *rebuild, pair *LVal) {
				if len(pair.Cells) > 0 {
					w.data(pair.Cells[0], d+1)
				}
				w.forms(pb, 1, d)
			}))
			w.forms(b, 2, depth)
		}
	case kindCond:
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
	case kindDotimes:
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
				w.emitBind(ctrl.Cells[0], op, d+1, nil)
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
	case kindExpr:
		if len(cells) > 1 {
			enter(v, false)
			for _, name := range exprFormalNames(cells[1]) {
				w.emitBind(name, op, d, nil)
			}
			w.forms(b, 1, depth)
			leave(v)
			// expr infers its parameters from the placeholders its
			// pattern uses, so expanding a macro in the pattern could
			// change the function's arity.  An expanded pattern is
			// lowered to a lambda with the originally inferred formals.
			if out := b.done(); out != v && len(cells) == 2 {
				if formals := exprLambdaList(cells[1]); formals != nil {
					lowered := SExpr([]*LVal{Symbol(DefaultLangPackage + ":lambda"), formals, out.Cells[1]})
					lowered.source = copyLocation(v.source)
					return lowered
				}
			}
		}
	case kindWithCleanup:
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
	case kindThreadFirst, kindThreadLast:
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
	case kindNone: // opaque forms never reach here
	}
	return b.done()
}

// formals binds the names in a lambda list.  Markers are skipped.
func (w *CodeWalker) formals(formals *LVal, op string, depth int) {
	if formals == nil || formals.Type != LSExpr {
		return
	}
	for _, f := range formals.Cells {
		if f.Type != LSymbol {
			continue
		}
		switch f.Str {
		case OptArgSymbol, VarArgSymbol, KeyArgSymbol:
			continue
		}
		w.emitBind(f, op, depth, nil)
	}
}

// pairs walks a list of binding pairs, calling fn with each well-formed
// pair and a rebuild over it.  It returns the rebuilt list.
func (w *CodeWalker) pairs(list *LVal, depth int, fn func(pb *rebuild, pair *LVal)) *LVal {
	if list == nil || list.Type != LSExpr {
		w.data(list, depth)
		return list
	}
	lb := newRebuild(list)
	for i, pair := range list.Cells {
		if pair.Type != LSExpr {
			w.data(pair, depth+1)
			continue
		}
		pb := newRebuild(pair)
		fn(pb, pair)
		lb.set(i, pb.done())
	}
	return lb.done()
}

// let walks a let or let* binding list and then the body.
func (w *CodeWalker) let(list, form *LVal, op string, seq bool, depth int, body func()) *LVal {
	if list == nil || list.Type != LSExpr {
		w.data(list, depth)
		w.push(false)
		body()
		w.pop()
		return list
	}
	var out *LVal
	if seq {
		w.push(false)
		w.visit(WalkNode{Event: WalkEnter, Node: form, Op: op, Depth: depth - 1})
		out = w.pairs(list, depth, func(pb *rebuild, pair *LVal) {
			w.forms(pb, 1, depth+1)
			if len(pair.Cells) > 0 {
				w.emitBind(pair.Cells[0], op, depth+2, nil)
			}
		})
	} else {
		out = w.pairs(list, depth, func(pb *rebuild, _ *LVal) {
			w.forms(pb, 1, depth+1)
		})
		w.push(false)
		w.visit(WalkNode{Event: WalkEnter, Node: form, Op: op, Depth: depth - 1})
		for _, pair := range list.Cells {
			if pair.Type == LSExpr && len(pair.Cells) > 0 {
				w.emitBind(pair.Cells[0], op, depth+2, nil)
			}
		}
	}
	body()
	w.visit(WalkNode{Event: WalkLeave, Node: form, Op: op, Depth: depth - 1})
	w.pop()
	return out
}

// flet walks a flet, labels or macrolet binding list and then the body.
func (w *CodeWalker) flet(list, form *LVal, op string, kind formKind, depth int, body func()) *LVal {
	enter := func(macros bool) {
		w.push(macros)
		w.visit(WalkNode{Event: WalkEnter, Node: form, Op: op, Depth: depth - 1})
	}
	leave := func() {
		w.visit(WalkNode{Event: WalkLeave, Node: form, Op: op, Depth: depth - 1})
		w.pop()
	}
	if list == nil || list.Type != LSExpr {
		w.data(list, depth)
		enter(kind == kindMacrolet)
		body()
		leave()
		return list
	}
	// One function binding: (name formals body...).
	fn := func(pb *rebuild, bind *LVal) {
		if len(bind.Cells) < 2 {
			return
		}
		w.push(false)
		w.visit(WalkNode{Event: WalkEnter, Node: bind, Op: op, Depth: depth + 1, Function: true})
		w.formals(bind.Cells[1], op, depth+2)
		w.forms(pb, 2, depth+1)
		w.visit(WalkNode{Event: WalkLeave, Node: bind, Op: op, Depth: depth + 1, Function: true})
		w.pop()
	}
	bindNames := func() {
		// Local macros are compiled before any is bound: they do not see
		// each other.
		macs := make([]localMacroExpander, len(list.Cells))
		if kind == kindMacrolet && w.env != nil && w.err == nil {
			menv := w.macroEnv()
			for i, bind := range list.Cells {
				if bind.Type == LSExpr && len(bind.Cells) >= 2 && bind.Cells[0].Type == LSymbol {
					fn := NewEnv(menv).Lambda(bind.Cells[1], bind.Cells[2:])
					if fn.Type == LError {
						w.fail(fn)
						return
					}
					fn.FunType = LFunMacro //elps:mutates evaluate as a macro: fn is the closure Lambda freshly allocated above
					macs[i] = fn
				}
			}
		}
		for i, bind := range list.Cells {
			if bind.Type != LSExpr || len(bind.Cells) == 0 {
				continue
			}
			w.emitBind(bind.Cells[0], op, depth+1, macs[i])
		}
	}
	var out *LVal
	if kind == kindLabels {
		enter(false)
		bindNames()
		out = w.pairs(list, depth, fn)
	} else {
		out = w.pairs(list, depth, fn)
		enter(kind == kindMacrolet)
		bindNames()
	}
	body()
	leave()
	return out
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
	key := walkMemoKey{node: v, scope: w.scopeID(), template: true}
	if r, ok := w.memo[key]; ok {
		return r
	}
	if lerr := w.charge(); lerr != nil {
		return w.fail(lerr)
	}
	r := w.templateList(v, depth)
	if w.memo == nil {
		w.memo = make(map[walkMemoKey]*LVal)
	}
	w.memo[key] = r
	return r
}

func (w *CodeWalker) templateList(v *LVal, depth int) *LVal {
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

// exprLambdaList returns the lambda list (expr pattern) builds, markers
// included, as opExpr builds it, or nil when opExpr would reject pattern.
func exprLambdaList(pattern *LVal) *LVal {
	n, short, nopt, vargs, err := countExprArgs(pattern)
	if err != nil || n > MaxExprFormals {
		return nil
	}
	var cells []*LVal
	if short {
		cells = append(cells, Symbol("%"))
	} else {
		for i := 1; i <= n; i++ {
			cells = append(cells, Symbol("%"+strconv.Itoa(i)))
		}
	}
	if nopt > 0 {
		cells = append(cells, Symbol(OptArgSymbol), Symbol("%"+OptArgSymbol))
	}
	if vargs {
		cells = append(cells, Symbol(VarArgSymbol), Symbol("%"+VarArgSymbol))
	}
	return SExpr(cells)
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
		SpecialOp:     env.resolveSpecialOp,
		Expand1:       env.expandOnce,
		env:           env,
		MaxDepth:      maxDepth,
		MaxExpansions: env.Runtime.MaxMacroExpansions(),
		step:          func() *LVal { return env.checkLimits(env.evalCtx) },
	}
	return w.Walk(form)
}

// resolveSpecialOp reports the form head is bound to in env, by what the
// binding is rather than how head is spelled.  The builtin special
// operators and definition macros are recognized by the identity their
// registration gave them (FID and package), so an alias or a qualified
// spelling resolves the same way.  Any other special operator -- one an
// embedder registered, in any package -- is reported under its
// package-qualified name, which has no kind, so the walker treats its
// form as opaque.
func (env *LEnv) resolveSpecialOp(head *LVal) (string, bool) {
	v := env.Get(head)
	if v.Type != LFun || v.Builtin() == nil {
		return "", false
	}
	lang := v.Package() == env.Runtime.Registry.Lang
	switch v.FunType {
	case LFunSpecialOp:
		name := registeredName(v.FID(), "<special-op ``")
		if lang && specialFormKind(name) != kindNone {
			return name, true
		}
		return v.Package() + ":" + name, true // no kind: opaque
	case LFunMacro:
		if name := registeredName(v.FID(), "<builtin-macro ``"); lang && (specialFormKind(name) == kindDefun || specialFormKind(name) == kindDefmacro) {
			return name, true
		}
	default:
	}
	return "", false
}

// registeredName extracts NAME from a registration FID "<kind “NAME”>".
func registeredName(fid, prefix string) string {
	name, ok := strings.CutPrefix(fid, prefix)
	if !ok {
		return ""
	}
	name, _ = strings.CutSuffix(name, "''>")
	return name
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

// macroEnv returns the environment a macrolet binding is compiled in: the
// walker's environment with the lexical scope around the macrolet.  At run
// time macrolet builds its macros in the live lexical environment, so a
// macro body may call an enclosing local macro, which works here too, or
// read a local variable, whose value only exists at run time: each local
// name is bound to an error saying so, rather than letting the lookup
// fall through to a global of the same name.
func (w *CodeWalker) macroEnv() *LEnv {
	menv := w.env
	for _, sc := range w.scopes {
		if len(sc.names) == 0 {
			continue
		}
		menv = NewEnv(menv)
		for name, mac := range sc.names {
			v := mac
			if v == nil {
				v = w.env.Errorf("macroexpand-all: %s is a local variable, whose value exists only at run time; a macrolet expander cannot read it ahead of time", name)
			}
			menv.Put(Symbol(name), v)
		}
	}
	return menv
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
