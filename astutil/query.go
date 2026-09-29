// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"

	"github.com/luthersystems/elps/lisp"
)

// FreeVarsIn returns the names in scope -- the local variables bound around
// form -- that form uses freely, one node per name, in order of first use.
// It is FreeVars restricted to scope: what a closure over form captures, or
// what code moved out of form's context would need passed in.  Pass
// expanded code (see AnalyzeScopes).
func FreeVarsIn(form *lisp.LVal, scope []string) []*lisp.LVal {
	in := make(map[string]bool, len(scope))
	for _, s := range scope {
		in[s] = true
	}
	out := []*lisp.LVal{}
	for _, v := range FreeVars(form) {
		if in[v.Str] {
			out = append(out, v)
		}
	}
	return out
}

// Role is what a node is in the code around it.
type Role uint8

// The roles ClassifyNodes assigns.
const (
	// RoleNone: the node is not in the classified form, or is inside a
	// form the walker treats as opaque.
	RoleNone Role = iota
	// RoleCode: an evaluated form, reference or literal.
	RoleCode
	// RoleBinding: a name a binding form introduces (a let name, a lambda
	// parameter, a dotimes variable, a flet function name).
	RoleBinding
	// RoleDefine: the global name a defun or defmacro defines.
	RoleDefine
	// RoleSet: a set! target.
	RoleSet
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
// form (compared by identity).  A node that appears at two places in the
// form (a macro can splice one argument twice) has the role of the last.
func (r *Roles) Role(node *lisp.LVal) Role {
	return r.m[node]
}

// ClassifyNodes walks form as code (pass expanded code) and assigns every
// node in it a Role, so a tool can tell a binding position or structural
// list from quoted data, which the reader represents the same way.
func ClassifyNodes(form *lisp.LVal) *Roles {
	r := &Roles{m: make(map[*lisp.LVal]Role)}
	markAll := func(v *lisp.LVal, role Role) {
		var mark func(v *lisp.LVal)
		mark = func(v *lisp.LVal) {
			if v == nil {
				return
			}
			r.m[v] = role
			for _, c := range v.Cells {
				mark(c)
			}
		}
		mark(v)
	}
	opaque := map[*lisp.LVal]bool{}
	WalkCode(form, func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkForm:
			switch {
			case n.Opaque:
				opaque[n.Node] = true
			case n.Op == "quasiquote":
				// The whole template is data; the walk reaches the holes
				// next and marks their code.
				for _, c := range n.Node.Cells[1:] {
					markAll(c, RoleData)
				}
			}
			r.m[n.Node] = RoleCode
		case lisp.WalkRef, lisp.WalkLiteral:
			r.m[n.Node] = RoleCode
		case lisp.WalkBind:
			r.m[n.Node] = RoleBinding
		case lisp.WalkDefine:
			r.m[n.Node] = RoleDefine
		case lisp.WalkSet:
			r.m[n.Node] = RoleSet
		case lisp.WalkData:
			markAll(n.Node, RoleData)
		default:
		}
		return true
	})
	// Whatever the walk did not report, below a node it did, is syntax.
	var syntax func(v *lisp.LVal)
	syntax = func(v *lisp.LVal) {
		if v == nil || opaque[v] {
			return
		}
		switch r.m[v] {
		case RoleData, RoleBinding, RoleDefine, RoleSet:
			return
		case RoleNone:
			r.m[v] = RoleSyntax
		default:
		}
		for _, c := range v.Cells {
			syntax(c)
		}
	}
	syntax(form)
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
func FindCalls(form *lisp.LVal, names ...string) []CallSite {
	want := make(map[string]bool, len(names))
	for _, n := range names {
		want[n] = true
	}
	type entry struct {
		enc   Enclosure
		depth int
		scope bool
	}
	var stack []entry
	var sites []CallSite
	var current *lisp.LVal
	WalkCode(form, func(n *lisp.WalkNode) bool {
		isScope := n.Event == lisp.WalkEnter || n.Event == lisp.WalkLeave
		for len(stack) > 0 {
			top := stack[len(stack)-1]
			if top.scope || top.depth < n.Depth || (isScope && top.depth == n.Depth) {
				break
			}
			stack = stack[:len(stack)-1]
		}
		switch n.Event {
		case lisp.WalkEnter:
			if n.Function {
				stack = append(stack, entry{enc: Enclosure{Form: n.Node, Op: n.Op, Function: true}, depth: n.Depth, scope: true})
			}
		case lisp.WalkLeave:
			// Forms inside the scope are deeper and were popped above.
			if n.Function && len(stack) > 0 && stack[len(stack)-1].enc.Form == n.Node {
				stack = stack[:len(stack)-1]
			}
		case lisp.WalkForm:
			current = n.Node
			if n.Op != "" && !n.Opaque {
				stack = append(stack, entry{enc: Enclosure{Form: n.Node, Op: n.Op}, depth: n.Depth})
			}
		case lisp.WalkRef:
			if !n.Head || n.Bound || current == nil {
				break
			}
			if !want[strings.TrimPrefix(n.Node.Str, lisp.DefaultLangPackage+":")] {
				break
			}
			site := CallSite{Form: current, Name: n.Node.Str}
			for _, e := range stack {
				site.Enclosing = append(site.Enclosing, e.enc)
			}
			sites = append(sites, site)
		default:
		}
		return true
	})
	return sites
}

// ContainsCall reports whether form calls any of names in code position
// (see FindCalls).
func ContainsCall(form *lisp.LVal, names ...string) bool {
	return len(FindCalls(form, names...)) > 0
}
