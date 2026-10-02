// Copyright © 2026 The ELPS authors

package astutil

import (
	"strings"

	"github.com/luthersystems/elps/internal/codewalk"
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
		case lisp.WalkEnter, lisp.WalkLeave:
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
// bound where it occurs, with the special forms and function bodies around
// it.  A macro whose body must not contain some call (inside a lambda, a
// handler, a quasiquote) can reject it by inspecting Enclosing.  Calls
// inside quoted data are not calls and are not returned.  Pass expanded
// code.
//
// Code built by macros can share structure.  A shared call is reported once
// per place it occurs in code, each with its own enclosures and lexical
// context.  The work and the number of sites are bounded; FindCalls drops
// what does not fit.  A check that must not miss a call uses FindCallSites,
// which says when the result is incomplete.
func FindCalls(form *lisp.LVal, names ...string) []CallSite {
	sites, _ := FindCallSites(form, names...)
	return sites
}

// maxCallSites bounds the distinct sites one query reports: code that shares
// structure exponentially has exponentially many places.  Sites identical to
// one already reported (same call, same enclosures) do not count against it
// once it is reached.
const maxCallSites = 64

// maxCallWork bounds the lists one query walks, across every occurrence of
// shared structure.
const maxCallWork = 1 << 16

// FindCallSites is FindCalls reporting whether the result is complete.
// complete is false when the query ran out of work or site budget; a
// caller checking that no call occurs in some context must then assume
// one does.
func FindCallSites(form *lisp.LVal, names ...string) ([]CallSite, bool) {
	var sites []CallSite
	var complete bool
	want := make(map[string]bool, len(names))
	for _, n := range names {
		want[n] = true
	}
	type entry struct {
		enc   Enclosure
		depth int
	}
	var stack []entry
	var current *lisp.LVal
	complete = true
	full := false
	add := func(site CallSite) {
		if len(sites) >= maxCallSites {
			sites = dedupSites(sites)
		}
		if len(sites) >= maxCallSites {
			for _, s := range sites {
				if sameSite(s, site) {
					return
				}
			}
			complete, full = false, true
			return
		}
		sites = append(sites, site)
	}
	w := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		if full {
			return false
		}
		switch n.Event {
		case lisp.WalkLeave:
			return true
		case lisp.WalkEnter:
			// A form's scope is entered at the form's own depth: drop what is
			// deeper, then keep the form itself if it is on top.
			stack = popDeeper(stack, n.Depth+1, false, func(e entry) int { return e.depth })
			keep := len(stack) > 0 && stack[len(stack)-1].enc.Form == n.Node
			stack = popDeeper(stack, n.Depth, keep, func(e entry) int { return e.depth })
			if n.Function {
				stack = append(stack, entry{Enclosure{Form: n.Node, Op: n.Op, Function: true}, n.Depth})
			}
			return true
		case lisp.WalkForm, lisp.WalkRef, lisp.WalkSet, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData:
		}
		stack = popDeeper(stack, n.Depth, false, func(e entry) int { return e.depth })
		switch n.Event {
		case lisp.WalkForm:
			current = n.Node
			if n.Op != "" {
				stack = append(stack, entry{Enclosure{Form: n.Node, Op: n.Op}, n.Depth})
			}
		case lisp.WalkRef:
			if n.Head && !n.Bound && current != nil &&
				want[strings.TrimPrefix(n.Node.Str, lisp.DefaultLangPackage+":")] {
				encl := make([]Enclosure, len(stack))
				for i, e := range stack {
					encl[i] = e.enc
				}
				add(CallSite{Form: current, Name: n.Node.Str, Enclosing: encl})
			}
		case lisp.WalkSet, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData, lisp.WalkEnter, lisp.WalkLeave:
		}
		return true
	}}
	if !codewalk.Occurrences(w, maxCallWork, form) {
		complete = false
	}
	return sites, complete
}

// popDeeper drops the entries at least as deep as depth, except a top entry
// keep asks to retain.
func popDeeper[T any](stack []T, depth int, keep bool, d func(T) int) []T {
	n := len(stack)
	if keep {
		n--
	}
	for n > 0 && d(stack[n-1]) >= depth {
		n--
	}
	if keep {
		return append(stack[:n], stack[len(stack)-1])
	}
	return stack[:n]
}

func sameSite(a, b CallSite) bool {
	if a.Form != b.Form || a.Name != b.Name || len(a.Enclosing) != len(b.Enclosing) {
		return false
	}
	for i := range a.Enclosing {
		if a.Enclosing[i] != b.Enclosing[i] {
			return false
		}
	}
	return true
}

// dedupSites drops sites identical to an earlier one.
func dedupSites(sites []CallSite) []CallSite {
	out := sites[:0]
	for _, s := range sites {
		dup := false
		for _, o := range out {
			if sameSite(o, s) {
				dup = true
				break
			}
		}
		if !dup {
			out = append(out, s)
		}
	}
	return out
}
