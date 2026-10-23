// Copyright © 2026 The ELPS authors

package lisp

import (
	"errors"
	"fmt"
)

// FormTemplate is a quasiquote-style form, compiled once, that Go macros
// expand into fresh syntax.  elpsutil.MustTemplate reads one from source
// with the elps reader; NewFormTemplate compiles one from a form already
// read.
//
// The form is one ordinary elps form: symbols, lists, (), strings, numbers
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
// A template stores no LVals.  Each Expand constructs fresh, unsealed,
// unlocated syntax, as the Go macro contract in LEnv.AddMacros requires:
// the evaluator locates it at the macro call site.  Arguments are inserted by
// pointer, and () uses the immutable Nil singleton.  Expansion neither
// reads nor evaluates anything, and templates may be expanded concurrently.
type FormTemplate struct {
	src        string // the core template's source, for an ungenerated one
	root       tmplNode
	paramCount int
	missing    bool // a core template the generated file lacks
}

type tmplKind uint8

const (
	tmplList tmplKind = iota
	tmplSymbol
	tmplString
	tmplInt
	tmplFloat
	tmplArg
	tmplSplice
	tmplQuote
)

type tmplNode struct {
	str      string
	children []tmplNode
	float    float64
	num      int
	index    int
	kind     tmplKind
	splice   bool // a list with a spliced child
}

// NewFormTemplate compiles form, a form the elps reader produced, with
// params declaring placeholder names in argument order.  It returns an
// error if form contains a value a template cannot copy, an undeclared
// placeholder, a duplicate or unused parameter, a malformed unquote, or a
// misplaced unquote-splicing.
func NewFormTemplate(form *LVal, params ...string) (*FormTemplate, error) {
	if form == nil {
		return nil, errors.New("nil form")
	}
	c := tmplCompiler{params: make(map[string]int, len(params)), used: make([]bool, len(params))}
	for i, name := range params {
		if _, exists := c.params[name]; exists {
			return nil, fmt.Errorf("duplicate parameter %q", name)
		}
		c.params[name] = i
	}
	root := c.compile(form, false)
	if c.err == nil && root.kind == tmplSplice {
		c.err = errMisplacedSplice
	}
	if c.err != nil {
		return nil, c.err
	}
	for i, used := range c.used {
		if !used {
			return nil, fmt.Errorf("unused parameter %q", params[i])
		}
	}
	return &FormTemplate{root: root, paramCount: len(params)}, nil
}

var errMisplacedSplice = errors.New("unquote-splicing must be directly inside a list")

type tmplCompiler struct {
	params map[string]int
	err    error
	used   []bool
}

// compile converts a parsed form.  unquoted is true when the caller has
// already accounted for v's own quote, so v is compiled as its bare form.
// It records the first error in c.err.
func (c *tmplCompiler) compile(v *LVal, unquoted bool) tmplNode {
	if c.err != nil {
		return tmplNode{}
	}
	if v.Type == LQuote {
		return tmplNode{kind: tmplQuote, children: []tmplNode{c.compile(v.Cells[0], false)}}
	}
	if v.IsQuoted() && !unquoted {
		inner := c.compile(v, true)
		if inner.kind == tmplSplice && c.err == nil {
			c.err = errMisplacedSplice
		}
		return tmplNode{kind: tmplQuote, children: []tmplNode{inner}}
	}
	switch v.Type {
	case LSymbol:
		return tmplNode{kind: tmplSymbol, str: v.Str}
	case LString:
		return tmplNode{kind: tmplString, str: v.Str}
	case LInt:
		return tmplNode{kind: tmplInt, num: v.Int}
	case LFloat:
		return tmplNode{kind: tmplFloat, float: v.Float}
	case LSExpr:
		if kind, ok := placeholderKind(v); ok {
			if len(v.Cells) != 2 || v.Cells[1].Type != LSymbol || v.Cells[1].IsQuoted() {
				c.err = fmt.Errorf("%s takes one parameter name", v.Cells[0].Str)
				return tmplNode{}
			}
			name := v.Cells[1].Str
			i, ok := c.params[name]
			if !ok {
				c.err = fmt.Errorf("undeclared placeholder %q", name)
				return tmplNode{}
			}
			c.used[i] = true
			return tmplNode{kind: kind, index: i}
		}
		children := make([]tmplNode, len(v.Cells))
		splice := false
		for i, cell := range v.Cells {
			children[i] = c.compile(cell, false)
			splice = splice || children[i].kind == tmplSplice
		}
		return tmplNode{kind: tmplList, children: children, splice: splice}
	default:
		c.err = fmt.Errorf("unsupported %s value in template", v.Type)
		return tmplNode{}
	}
}

func placeholderKind(v *LVal) (tmplKind, bool) {
	if len(v.Cells) == 0 || v.Cells[0].Type != LSymbol || v.Cells[0].IsQuoted() {
		return 0, false
	}
	switch v.Cells[0].Str {
	case "unquote":
		return tmplArg, true
	case "unquote-splicing":
		return tmplSplice, true
	}
	return 0, false
}

// Expand returns a fresh expansion, inserting args by pointer without copying
// or evaluating them.  Quoted forms use Quote, as the reader does; Quote may
// copy an argument's header to add a quote without changing the argument.
// Expand panics if the argument count differs from the declared parameter
// count or a spliced argument is not an unquoted list (nil is accepted).
// The zero FormTemplate expands to ().
func (t *FormTemplate) Expand(args ...*LVal) *LVal {
	if t.missing {
		panic(fmt.Sprintf("lisp.FormTemplate.Expand: core template is not in coreformtemplates_gen.go; run go generate ./lisp: %s", t.src))
	}
	if len(args) != t.paramCount {
		panic(fmt.Sprintf("lisp.FormTemplate.Expand: expected %d arguments, got %d", t.paramCount, len(args)))
	}
	return t.root.expand(args)
}

func (n *tmplNode) expand(args []*LVal) *LVal {
	switch n.kind {
	case tmplSymbol:
		return Symbol(n.str)
	case tmplString:
		return String(n.str)
	case tmplInt:
		return Int(n.num)
	case tmplFloat:
		return Float(n.float)
	case tmplArg:
		return args[n.index]
	case tmplQuote:
		return Quote(n.children[0].expand(args))
	default:
		// Lists expand below; splices are handled by their enclosing list.
	}
	// Each list's header and cells are one allocation (newSExprCap).  A list
	// with a spliced argument computes its exact size first, so it too needs
	// one cells slice.  Index, never range by value: a loop copy passed to the
	// pointer receiver would escape, one allocation per child.
	size := len(n.children)
	if n.splice {
		for i := range n.children {
			child := &n.children[i]
			if child.kind == tmplSplice {
				arg := args[child.index]
				if arg == nil || arg.Type != LSExpr || arg.IsQuoted() {
					panic(fmt.Sprintf("lisp.FormTemplate.Expand: splice argument %d must be an unquoted list or nil", child.index+1))
				}
				size += len(arg.Cells) - 1
			}
		}
	}
	if size == 0 {
		return Nil()
	}
	list, cells := newSExprCap(size)
	for i := range n.children {
		child := &n.children[i]
		if child.kind == tmplSplice {
			cells = append(cells, args[child.index].Cells...)
		} else {
			cells = append(cells, child.expand(args))
		}
	}
	list.Cells = cells //elps:mutates header freshly allocated by newSExprCap above for this expansion and not yet visible to anything else
	return list
}

//go:generate go test -run TestCoreFormTemplatesGenerated -update-core-form-templates .

// coreFormSource is one core template's source and parameters, which the
// test that writes coreformtemplates_gen.go reads.
type coreFormSource struct {
	src    string
	params []string
}

// coreFormSources lists every core template, in the order package lisp
// declares them.
var coreFormSources []coreFormSource

// coreFormTemplate returns the core template for src, which package lisp's
// own Go macros expand.  Package lisp cannot import the elps reader, so it
// does not read src: coreformtemplates_gen.go holds each core template
// compiled, keyed by its source and parameters, and
// TestCoreFormTemplatesGenerated fails when that file is out of date.  A
// template missing from the file panics when expanded.
func coreFormTemplate(src string, params ...string) *FormTemplate {
	coreFormSources = append(coreFormSources, coreFormSource{src, params})
	if t, ok := coreFormTemplates[coreFormKey(src, params)]; ok {
		return t
	}
	return &FormTemplate{src: src, missing: true}
}

func coreFormKey(src string, params []string) string {
	return fmt.Sprintf("%q %q", params, src)
}
