// Copyright © 2026 The ELPS authors

package elpsutil

import (
	"fmt"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Template is a quasiquote-style form, read once by the elps reader and
// expanded by Go macros into fresh syntax.
//
// The source is one ordinary elps form: symbols, lists, (), strings, numbers
// and 'form are copied as written.  Placeholders use the forms elps
// quasiquote uses, naming a declared parameter:
//
//	(unquote name)           the argument, inserted by pointer
//	(unquote-splicing name)  the argument's cells, spliced into the enclosing list
//
// A placeholder is recognised anywhere in the form, including under a quote,
// so '(unquote name) quotes the argument.  unquote-splicing must appear
// directly inside a list, not at the top level or directly under a quote.
// Because every unquote form is a placeholder, a template cannot contain a
// literal unquote form or a nested quasiquote: in
// (quasiquote (a (unquote x))) the inner unquote is substituted.  Build such
// a form in Go and pass it as an argument.
//
// Expand makes none of the checks that quasiquote makes.  ExpandChecked
// expands as quasiquote would, under env's limits, with its errors.
//
// A template stores no LVals.  Each Expand constructs fresh, unsealed,
// unlocated syntax, as the Go macro contract in lisp.LEnv.AddMacros requires:
// the evaluator locates it at the macro call site.  Arguments are inserted by
// pointer, and () uses the immutable lisp.Nil singleton.  Expansion neither
// reads nor evaluates anything, and templates may be expanded concurrently.
type Template struct {
	root       templateNode
	paramCount int
	// quotedHead is true when a list in the template has a quoted unquote
	// or unquote-splicing head, ('unquote x): Expand copies it, but
	// quasiquote reads it as an unquote, so ExpandChecked refuses it.
	quotedHead bool
	// widest is the most cells a list of the template has, less its
	// splices, and deepest a bound on the value depth of its deepest node:
	// each quote counts as a level, though the walk counts a single quote
	// as a flag, so the bound is at or above the walk's depth.  splices
	// are the parameters the template splices.  ExpandChecked reads them
	// to skip quasiquote's walk when no check of it can fail (see fits).
	widest, deepest int
	splices         []int
}

type templateKind uint8

const (
	templateList templateKind = iota
	templateSymbol
	templateString
	templateInt
	templateFloat
	templateArg
	templateSplice
	templateQuote
)

type templateNode struct {
	str      string
	children []templateNode
	float    float64
	num      int
	index    int
	kind     templateKind
}

// MustTemplate reads src with the elps reader and compiles it, with params
// declaring placeholder names in argument order.  It is meant for package
// initialization and panics if src is not exactly one form, fails to parse,
// or contains a value the reader produces that a template cannot copy; on an
// undeclared placeholder, a duplicate or unused parameter, a malformed
// unquote, or a misplaced unquote-splicing.
func MustTemplate(src string, params ...string) *Template {
	c := templateCompiler{params: make(map[string]int, len(params)), used: make([]bool, len(params))}
	for i, name := range params {
		if _, exists := c.params[name]; exists {
			templatePanic("duplicate parameter %q", name)
		}
		c.params[name] = i
	}
	forms, err := parser.NewReader().Read("template", strings.NewReader(src))
	if err != nil {
		templatePanic("%v", err)
	}
	if len(forms) != 1 {
		templatePanic("expected one form, got %d", len(forms))
	}
	root := c.compile(forms[0], false)
	if root.kind == templateSplice {
		templatePanic("unquote-splicing must be directly inside a list")
	}
	for i, name := range params {
		if !c.used[i] {
			templatePanic("unused parameter %q", name)
		}
	}
	t := &Template{root: root, paramCount: len(params), quotedHead: root.hasQuotedHead()}
	t.widest, t.deepest = root.bounds(0, &t.splices)
	return t
}

func templatePanic(format string, args ...any) {
	panic("elpsutil.MustTemplate: " + fmt.Sprintf(format, args...))
}

type templateCompiler struct {
	params map[string]int
	used   []bool
}

// compile converts a parsed form.  unquoted is true when the caller has
// already accounted for v's own quote, so v is compiled as its bare form.
func (c *templateCompiler) compile(v *lisp.LVal, unquoted bool) templateNode {
	if v.Type == lisp.LQuote {
		return templateNode{kind: templateQuote, children: []templateNode{c.compile(v.Cells[0], false)}}
	}
	if v.IsQuoted() && !unquoted {
		inner := c.compile(v, true)
		if inner.kind == templateSplice {
			templatePanic("unquote-splicing must be directly inside a list")
		}
		return templateNode{kind: templateQuote, children: []templateNode{inner}}
	}
	switch v.Type {
	case lisp.LSymbol:
		return templateNode{kind: templateSymbol, str: v.Str}
	case lisp.LString:
		return templateNode{kind: templateString, str: v.Str}
	case lisp.LInt:
		return templateNode{kind: templateInt, num: v.Int}
	case lisp.LFloat:
		return templateNode{kind: templateFloat, float: v.Float}
	case lisp.LSExpr:
		if kind, ok := c.placeholderKind(v); ok {
			if len(v.Cells) != 2 || v.Cells[1].Type != lisp.LSymbol || v.Cells[1].IsQuoted() {
				templatePanic("%s takes one parameter name", v.Cells[0].Str)
			}
			name := v.Cells[1].Str
			i, ok := c.params[name]
			if !ok {
				templatePanic("undeclared placeholder %q", name)
			}
			c.used[i] = true
			return templateNode{kind: kind, index: i}
		}
		children := make([]templateNode, len(v.Cells))
		for i, cell := range v.Cells {
			children[i] = c.compile(cell, false)
		}
		return templateNode{kind: templateList, children: children}
	default:
		templatePanic("unsupported %s value in template", v.Type)
		return templateNode{}
	}
}

func (c *templateCompiler) placeholderKind(v *lisp.LVal) (templateKind, bool) {
	if len(v.Cells) == 0 || v.Cells[0].Type != lisp.LSymbol || v.Cells[0].IsQuoted() {
		return 0, false
	}
	switch v.Cells[0].Str {
	case "unquote":
		return templateArg, true
	case "unquote-splicing":
		return templateSplice, true
	}
	return 0, false
}

// Expand returns a fresh expansion, inserting args by pointer without copying
// or evaluating them.  Quoted forms use lisp.Quote, as the reader does; Quote
// may copy an argument's header to add a quote without changing the argument.
// Expand panics if the argument count differs from the declared parameter
// count or a spliced argument is not an unquoted list (nil is accepted).
func (t *Template) Expand(args ...*lisp.LVal) *lisp.LVal {
	if len(args) != t.paramCount {
		panic(fmt.Sprintf("elpsutil.Template.Expand: expected %d arguments, got %d", t.paramCount, len(args)))
	}
	return t.root.expand(args)
}

// ExpandChecked returns what (quasiquote <template>) evaluates to in env when
// each placeholder's parameter is bound to its argument: the expansion, or
// the error the quasiquote raised.  It runs quasiquote's own walk
// (lisp.LEnv.QuasiquoteWith), so it makes the same checks in the same order
// with the same messages: the value depth limit at each node, quote
// wrappers included, and the allocation limit as each list adds a cell,
// where a splice adds its argument's length.  Where the quasiquote
// evaluated a placeholder's parameter, ExpandChecked checks env's context,
// as that evaluation did.  It charges no step and does not check the step
// limit or budget, so under a step limit the defmacro can fail where
// ExpandChecked does not.  A spliced argument that is not a list is
// quasiquote's error, not a panic, and a nil argument is an error.
//
// A Go macro that replaces a defmacro whose body was a quasiquote of the
// same form calls it, so the expansion fails where the defmacro's failed
// and a handler that catches the error reads the same message
// (luthersystems/elps#894).  The expansion is fresh syntax, as Expand's
// is, with arguments inserted by pointer.  When no check of the walk can
// fail, which is so for a small template under the default limits,
// ExpandChecked skips the walk and costs what Expand costs.  Otherwise it
// builds the form before it walks it, about twice Expand's cost.  It
// panics if the argument count differs from the declared parameter count,
// and on a template with a list whose head is a quoted unquote, such as
// (f ('unquote y)): Expand copies that list, but quasiquote reads it as an
// unquote of y, so no defmacro expands as the template does.
func (t *Template) ExpandChecked(env *lisp.LEnv, args ...*lisp.LVal) *lisp.LVal {
	if len(args) != t.paramCount {
		panic(fmt.Sprintf("elpsutil.Template.ExpandChecked: expected %d arguments, got %d", t.paramCount, len(args)))
	}
	if t.quotedHead {
		panic("elpsutil.Template.ExpandChecked: the template has a list with a quoted unquote head, which quasiquote reads as an unquote; use Expand")
	}
	if t.fits(env, args) {
		// No check of the walk can fail, so its only possible error is
		// the context check at each placeholder, and every one of them
		// sees the same context.
		if t.paramCount > 0 {
			if err := env.CheckContext(); err != nil {
				return lisp.Error(err)
			}
		}
		return t.root.expand(args)
	}
	return env.QuasiquoteWith(t.root.source(), func(expr *lisp.LVal) *lisp.LVal {
		if err := env.CheckContext(); err != nil {
			return lisp.Error(err)
		}
		return args[expr.Int]
	})
}

// fits reports whether quasiquote's walk of t with args would pass every
// check it makes and build what Expand builds.  The walk builds the source
// form and then a copy of it, about twice Expand's cost, and a Go macro
// expands on every call of the function whose body holds it
// (luthersystems/substrate#973).  The template's shape is known only here,
// so the bound is here and not in package lisp.
//
// The walk fails a node at value depth deepest or more, and a list when
// its cells pass the allocation limit, where a splice adds its argument's
// length and an unquote one cell.  It does not enter an argument, so only
// the splices' lengths depend on args.  The walk also treats some
// arguments in its own way: it returns an error argument as its result,
// splices an argument with the spliced flag (lisp.Splice) even in an
// unquote position, and fails on a nil argument and on a splice of a value
// that is not an unquoted list.  fits is false for each of these, so the
// walk handles them.  TestExpandCheckedArgumentKinds compares the two paths
// for each kind of argument.
func (t *Template) fits(env *lisp.LEnv, args []*lisp.LVal) bool {
	limit := env.Runtime.MaxAllocBytes() - t.widest
	if limit < 0 || t.deepest >= env.Runtime.ValueDepthLimit() {
		return false
	}
	for _, arg := range args {
		if arg == nil || arg.IsError() || arg.IsSpliced() {
			return false
		}
	}
	spliced := 0
	for _, i := range t.splices {
		arg := args[i]
		if arg.Type != lisp.LSExpr || arg.IsQuoted() {
			return false
		}
		// No list can hold more than widest cells plus every splice.
		if spliced += len(arg.Cells); spliced > limit {
			return false
		}
	}
	return true
}

// bounds returns the most cells a list under n has, less its splices, and
// the value depth of the deepest node, where n is at depth.  It appends
// each splice's parameter to splices.
func (n *templateNode) bounds(depth int, splices *[]int) (int, int) {
	widest, deepest := 0, depth
	count := 0
	for i := range n.children {
		child := &n.children[i]
		if child.kind == templateSplice {
			*splices = append(*splices, child.index)
		} else {
			count++
		}
		w, d := child.bounds(depth+1, splices)
		widest, deepest = max(widest, w), max(deepest, d)
	}
	if n.kind == templateList {
		widest = max(widest, count)
	}
	return widest, deepest
}

// hasQuotedHead reports whether n or a node under it is a list whose head
// is a once-quoted unquote or unquote-splicing symbol.
func (n *templateNode) hasQuotedHead() bool {
	if n.kind == templateList && len(n.children) > 0 {
		// One quote only: source() builds 'unquote as a quoted symbol,
		// which quasiquote reads as an unquote, but ''unquote as an
		// LQuote wrapper, which it does not.
		if head := &n.children[0]; head.kind == templateQuote {
			sym := &head.children[0]
			if sym.kind == templateSymbol && (sym.str == "unquote" || sym.str == "unquote-splicing") {
				return true
			}
		}
	}
	for i := range n.children {
		if n.children[i].hasQuotedHead() {
			return true
		}
	}
	return false
}

// source returns the template as quasiquote source: the form MustTemplate
// read, with each placeholder's parameter name replaced by its index.
// It is fresh syntax, built as expand builds an expansion.
func (n *templateNode) source() *lisp.LVal {
	switch n.kind {
	case templateArg:
		return lisp.Cells{lisp.Symbol("unquote"), lisp.Int(n.index)}.SExpr()
	case templateSplice:
		return lisp.Cells{lisp.Symbol("unquote-splicing"), lisp.Int(n.index)}.SExpr()
	case templateQuote:
		return lisp.Quote(n.children[0].source())
	case templateList:
		if len(n.children) == 0 {
			return lisp.Nil()
		}
		cells := make([]*lisp.LVal, len(n.children))
		for i := range n.children {
			cells[i] = n.children[i].source()
		}
		return lisp.SExpr(cells)
	default:
		return n.expand(nil)
	}
}

func (n *templateNode) expand(args []*lisp.LVal) *lisp.LVal {
	switch n.kind {
	case templateSymbol:
		return lisp.Symbol(n.str)
	case templateString:
		return lisp.String(n.str)
	case templateInt:
		return lisp.Int(n.num)
	case templateFloat:
		return lisp.Float(n.float)
	case templateArg:
		return args[n.index]
	case templateQuote:
		return lisp.Quote(n.children[0].expand(args))
	default:
		// Lists expand below; splices are handled by their enclosing list.
	}
	// Compute the exact size before allocating, so a list with a spliced
	// argument needs one cells slice.  Index, never range by value: a loop
	// copy passed to the pointer receiver would escape, one allocation per
	// child.
	size := len(n.children)
	for i := range n.children {
		child := &n.children[i]
		if child.kind == templateSplice {
			arg := args[child.index]
			if arg == nil || arg.Type != lisp.LSExpr || arg.IsQuoted() {
				panic(fmt.Sprintf("elpsutil.Template.Expand: splice argument %d must be an unquoted list or nil", child.index+1))
			}
			size += len(arg.Cells) - 1
		}
	}
	if size == 0 {
		return lisp.Nil()
	}
	cells := make([]*lisp.LVal, 0, size)
	for i := range n.children {
		child := &n.children[i]
		if child.kind == templateSplice {
			cells = append(cells, args[child.index].Cells...)
		} else {
			cells = append(cells, child.expand(args))
		}
	}
	return lisp.SExpr(cells)
}
