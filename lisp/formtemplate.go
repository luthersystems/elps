// Copyright © 2026 The ELPS authors

package lisp

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// FormTemplate is a quasiquote-style form built once and expanded by Go macros.
// It supports lists, symbols, 'form (Quote), ,name (argument substitution) and
// ,@name (splicing an argument's Cells into a list). It is not a Lisp reader:
// pass strings and numbers as arguments rather than writing literals in src.
//
// A template stores no LVals. Each Expand constructs fresh syntax, as required
// by the Go macro contract in LEnv.AddMacros: the evaluator may locate that
// syntax in place. Arguments are inserted by pointer, and () uses the immutable
// Nil singleton. Templates may be reused concurrently; ownership of arguments
// remains with the caller.
type FormTemplate struct {
	root       formTemplateNode
	paramCount int
}

// MustFormTemplate compiles one form, with params declaring placeholder names
// in argument order. It is intended for package initialization and panics on
// malformed syntax, undeclared placeholders, duplicate or unused parameters,
// trailing input, or a splice outside a list (including directly under ').
// Syntax errors report a zero-based byte offset in src.
//
// Symbols are maximal runs excluding whitespace and ()',";[]. Tokens starting
// with a digit, # or a backtick are also rejected; strings, comments, brackets
// and other reader syntax are unsupported.
func MustFormTemplate(src string, params ...string) *FormTemplate {
	p := formTemplateReader{
		src:    src,
		params: make(map[string]int, len(params)),
		used:   make([]bool, len(params)),
	}
	for i, name := range params {
		if _, exists := p.params[name]; exists {
			panic(fmt.Sprintf("lisp.MustFormTemplate: duplicate parameter %q", name))
		}
		p.params[name] = i
	}
	root := p.form(false)
	p.skipSpace()
	if p.offset != len(src) {
		p.fail(p.offset, "trailing input")
	}
	for i, used := range p.used {
		if !used {
			panic(fmt.Sprintf("lisp.MustFormTemplate: unused parameter %q", params[i]))
		}
	}
	return &FormTemplate{root: root, paramCount: len(params)}
}

// Expand returns a fresh expansion, inserting args by pointer without copying
// or evaluating them. Quoted forms use Quote, just like the Lisp reader; Quote
// may copy an argument's header to add a quote without changing the argument.
// Expand panics if the argument count differs from the declared parameter count
// or a spliced argument is not an unquoted list (Nil is accepted).
func (t *FormTemplate) Expand(args ...*LVal) *LVal {
	if len(args) != t.paramCount {
		panic(fmt.Sprintf("lisp.FormTemplate.Expand: expected %d arguments, got %d", t.paramCount, len(args)))
	}
	return t.root.expand(args)
}

type formTemplateKind uint8

const (
	formTemplateList formTemplateKind = iota
	formTemplateSymbol
	formTemplateArg
	formTemplateSplice
	formTemplateQuote
)

type formTemplateNode struct {
	symbol   string
	children []formTemplateNode
	index    int
	kind     formTemplateKind
}

func (n *formTemplateNode) expand(args []*LVal) *LVal {
	switch n.kind {
	case formTemplateSymbol:
		return Symbol(n.symbol)
	case formTemplateArg:
		return args[n.index]
	case formTemplateQuote:
		return Quote(n.children[0].expand(args))
	default:
		// Lists expand below; splices are handled by their enclosing list.
	}
	// Splices occur only as direct children of lists. Compute the exact size
	// before allocating so expanding a rest argument needs one cells slice.
	size := len(n.children)
	for i := range n.children {
		// Index, never range by value: a loop copy passed to the pointer
		// receiver below would escape, one allocation per child.
		child := &n.children[i]
		if child.kind == formTemplateSplice {
			arg := args[child.index]
			if arg == nil || arg.Type != LSExpr || arg.IsQuoted() {
				panic(fmt.Sprintf("lisp.FormTemplate.Expand: splice argument %d must be an unquoted list or nil", child.index+1))
			}
			size += len(arg.Cells) - 1
		}
	}
	if size == 0 {
		return Nil()
	}
	cells := make([]*LVal, 0, size)
	for i := range n.children {
		child := &n.children[i]
		if child.kind == formTemplateSplice {
			cells = append(cells, args[child.index].Cells...)
		} else {
			cells = append(cells, child.expand(args))
		}
	}
	return SExpr(cells)
}

type formTemplateReader struct {
	params map[string]int
	src    string
	used   []bool
	offset int
}

func (p *formTemplateReader) fail(offset int, format string, args ...any) {
	panic(fmt.Sprintf("lisp.MustFormTemplate: offset %d: %s", offset, fmt.Sprintf(format, args...)))
}

func (p *formTemplateReader) skipSpace() {
	for p.offset < len(p.src) {
		r, size := utf8.DecodeRuneInString(p.src[p.offset:])
		if !unicode.IsSpace(r) {
			return
		}
		p.offset += size
	}
}

func (p *formTemplateReader) form(inList bool) formTemplateNode {
	p.skipSpace()
	start := p.offset
	if start == len(p.src) {
		p.fail(start, "expected a form")
	}
	switch p.src[p.offset] {
	case '(':
		p.offset++
		node := formTemplateNode{kind: formTemplateList}
		for {
			p.skipSpace()
			if p.offset == len(p.src) {
				p.fail(start, "unclosed list")
			}
			if p.src[p.offset] == ')' {
				p.offset++
				return node
			}
			node.children = append(node.children, p.form(true))
		}
	case ')':
		p.fail(start, "unexpected ')'")
	case '\'':
		p.offset++
		return formTemplateNode{kind: formTemplateQuote, children: []formTemplateNode{p.form(false)}}
	case ',':
		p.offset++
		kind := formTemplateArg
		if p.offset < len(p.src) && p.src[p.offset] == '@' {
			if !inList {
				p.fail(start, "splice must be directly inside a list")
			}
			kind = formTemplateSplice
			p.offset++
		}
		name := p.symbol()
		index, ok := p.params[name]
		if !ok {
			p.fail(start, "undeclared placeholder %q", name)
		}
		p.used[index] = true
		return formTemplateNode{kind: kind, index: index}
	}
	return formTemplateNode{kind: formTemplateSymbol, symbol: p.symbol()}
}

func (p *formTemplateReader) symbol() string {
	start := p.offset
	for p.offset < len(p.src) {
		r, size := utf8.DecodeRuneInString(p.src[p.offset:])
		if unicode.IsSpace(r) || strings.ContainsRune("()',\";[]", r) {
			break
		}
		if p.offset == start && (unicode.IsDigit(r) || r == '#' || r == '`') {
			p.fail(start, "unsupported token starting with %q; pass literals as arguments", r)
		}
		p.offset += size
	}
	if p.offset == start {
		p.fail(start, "expected a symbol")
	}
	return p.src[start:p.offset]
}
