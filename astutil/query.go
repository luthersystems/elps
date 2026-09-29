// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// Role is what a node is in the code around it.
type Role uint8

// The roles ClassifyNodes assigns.  Only RoleData and RoleSyntax have a
// consumer outside this package; the others are kept unexported.
const (
	// roleNone: the node is not in the classified form, or is inside a
	// form the walker does not enter (an embedder's special operator).
	roleNone Role = iota
	// roleCode: an evaluated form, reference, set! target or literal, or a
	// name a form binds or defines.
	roleCode
	// RoleData: unevaluated data -- a quoted value or anything inside one,
	// a quasiquote template outside its holes, a condition type.
	RoleData
	// RoleSyntax: structure a special form reads but does not evaluate: a
	// binding list, one [name init] binding, a lambda list, a cond clause,
	// a dotimes control list.  A binding written with brackets reads as a
	// quoted list; its role is still syntax, not data.
	RoleSyntax
)

// Roles is the result of ClassifyNodes.
type Roles struct {
	m map[*lisp.LVal]Role
}

// Role returns the role of node, which must be a node of the classified
// form (compared by identity).
func (r *Roles) Role(node *lisp.LVal) Role {
	return r.m[node]
}

// ClassifyNodes walks form as code with lisp.CodeWalker (pass expanded
// code) and assigns every node in it a Role, so a tool can tell a binding
// position or structural list from quoted data, which the reader
// represents the same way.  Each node is visited once, so data that shares
// structure, or is cyclic, costs one visit per node.
func ClassifyNodes(form *lisp.LVal) *Roles {
	r := &Roles{m: make(map[*lisp.LVal]Role)}
	// mark sets role on v and everything below it, once per node.
	var mark func(v *lisp.LVal, role Role)
	mark = func(v *lisp.LVal, role Role) {
		if v == nil || r.m[v] == role {
			return
		}
		r.m[v] = role
		for _, c := range v.Cells {
			mark(c, role)
		}
	}
	var holes []*lisp.LVal
	var markTemplate func(v *lisp.LVal)
	markTemplate = func(v *lisp.LVal) {
		if v == nil || r.m[v] == RoleData {
			return
		}
		r.m[v] = RoleData
		if v.Type == lisp.LSExpr && len(v.Cells) == 2 && v.Cells[0].Type == lisp.LSymbol &&
			(v.Cells[0].Str == "unquote" || v.Cells[0].Str == "unquote-splicing") {
			r.m[v.Cells[0]] = RoleData
			holes = append(holes, v.Cells[1])
			return
		}
		for _, c := range v.Cells {
			markTemplate(c)
		}
	}
	ExpandAll(form, nil, "", func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkForm:
			if n.Op == "quasiquote" {
				// The template is data except for its holes, which the
				// walk reaches next; the syntax pass starts again at each.
				for _, c := range n.Node.Cells[1:] {
					markTemplate(c)
				}
			}
			r.m[n.Node] = roleCode
		case lisp.WalkRef, lisp.WalkLiteral, lisp.WalkBind, lisp.WalkDefine, lisp.WalkSet:
			r.m[n.Node] = roleCode
		case lisp.WalkData:
			mark(n.Node, RoleData)
		case lisp.WalkEnter, lisp.WalkLeave, lisp.WalkEnd:
		}
		return true
	})
	// Whatever the walk did not report, below a node it did, is syntax.
	seen := make(map[*lisp.LVal]bool)
	var syntax func(v *lisp.LVal)
	syntax = func(v *lisp.LVal) {
		if v == nil || seen[v] {
			return
		}
		seen[v] = true
		switch r.m[v] {
		case RoleData:
			return
		case roleNone:
			r.m[v] = RoleSyntax
		case roleCode, RoleSyntax:
		}
		for _, c := range v.Cells {
			syntax(c)
		}
	}
	syntax(form)
	for _, h := range holes {
		syntax(h)
	}
	return r
}

// Enclosure is one construct around a call FindCalls found.
type Enclosure struct {
	// Form is the special form, or for a function body the form (or
	// flet/labels/macrolet binding) whose body it is.
	Form *lisp.LVal
	// Op is the special form's name: "let", "handler-bind", "quasiquote",
	// "lambda", ...
	Op string
	// Function marks a function body -- code that may run later, from
	// wherever the function is called -- rather than the form itself.  A
	// call inside a lambda has an Enclosure for the lambda form and one,
	// with Function set, for its body.
	Function bool
}

// CallSite is a call FindCalls found.
type CallSite struct {
	// Form is the call.
	Form *lisp.LVal
	// Name is the head as written.
	Name string
	// Enclosing lists the special forms and function bodies around the
	// call, outermost first.  Ordinary function calls are not listed.
	Enclosing []Enclosure
}

// FindCalls returns every call in form, in code position, whose head is one
// of names (bare, or qualified by the lisp package) and is not lexically
// bound in form, with the special forms and function bodies around it.  A
// macro whose body must not contain some call (inside a lambda, a handler,
// a quasiquote) can reject it by inspecting Enclosing.  Calls inside quoted
// data are not calls and are not returned.  Pass expanded code.
//
// Ancestry is structural: a call's enclosures are the special forms and
// function bodies on its path from form, so a sibling that follows a form
// is never taken to be inside it.
func FindCalls(form *lisp.LVal, names ...string) []CallSite {
	want := make(map[string]bool, len(names))
	for _, n := range names {
		want[n] = true
	}
	// parent links each node to the list holding it (first path wins for
	// shared nodes, which the walker visits once).
	parent := make(map[*lisp.LVal]*lisp.LVal)
	var link func(v *lisp.LVal)
	link = func(v *lisp.LVal) {
		for _, c := range v.Cells {
			if c == nil {
				continue
			}
			if _, ok := parent[c]; ok || c == form {
				continue
			}
			parent[c] = v
			link(c)
		}
	}
	if form != nil {
		link(form)
	}
	ops := make(map[*lisp.LVal]string)       // special forms
	functions := make(map[*lisp.LVal]string) // nodes whose body is a function
	type call struct {
		form *lisp.LVal
		name string
	}
	var calls []call
	var current *lisp.LVal
	ExpandAll(form, nil, "", func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkForm:
			current = n.Node
			if n.Op != "" {
				ops[n.Node] = n.Op
			}
		case lisp.WalkEnter:
			if n.Function {
				functions[n.Node] = n.Op
			}
		case lisp.WalkRef:
			if n.Head && !n.Bound && current != nil &&
				want[strings.TrimPrefix(n.Node.Str, lisp.DefaultLangPackage+":")] {
				calls = append(calls, call{form: current, name: n.Node.Str})
			}
		case lisp.WalkSet, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData, lisp.WalkLeave, lisp.WalkEnd:
		}
		return true
	})
	sites := make([]CallSite, 0, len(calls))
	for _, c := range calls {
		var encl []Enclosure
		for a := parent[c.form]; a != nil; a = parent[a] {
			// Innermost first; reversed below.  A lambda form is both a
			// special form and the owner of a function body: the body
			// Enclosure is the inner of the two.
			if op, ok := functions[a]; ok {
				encl = append(encl, Enclosure{Form: a, Op: op, Function: true})
			}
			if op, ok := ops[a]; ok {
				encl = append(encl, Enclosure{Form: a, Op: op})
			}
		}
		for i, j := 0, len(encl)-1; i < j; i, j = i+1, j-1 {
			encl[i], encl[j] = encl[j], encl[i]
		}
		sites = append(sites, CallSite{Form: c.form, Name: c.name, Enclosing: encl})
	}
	return sites
}
