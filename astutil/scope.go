// Copyright © 2026 The ELPS authors

package astutil

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

// Scope is one lexical scope of a walked form: a lambda or defun body, a
// let, a flet function or body, a dotimes, ...  The root scope stands for
// the code around the form and binds nothing.
type Scope struct {
	// Node is the form (or flet/labels/macrolet function binding) that
	// opens the scope; nil for the root.
	Node *lisp.LVal
	// Op is the builtin form that opens the scope ("let", "lambda", ...).
	Op string
	// Function reports whether the scope is a function body, whose code
	// may run after the scope that created it has been left (a closure).
	Function bool
	// Parent is the enclosing scope, nil for the root.
	Parent *Scope
	// Children are the scopes directly inside this one, in walk order.
	Children []*Scope
	// Bindings are the names this scope introduces, in walk order.
	Bindings []*Binding
	// Refs are the references (and set! targets) whose innermost
	// enclosing scope is this one, in walk order.
	Refs []*Ref

	start, end int // event index range
}

// Binding is one name a binding form introduces.
type Binding struct {
	// Name is the symbol node that introduces the binding.  For an expr
	// placeholder (%, %1, ...) the node is synthesized and has no source.
	Name *lisp.LVal
	// Scope is the scope the binding belongs to.
	Scope *Scope
	// Refs are the references and set! targets that resolve to it.
	Refs []*Ref
	// Synthetic marks a binding with no binding occurrence in the source
	// (expr placeholders); it cannot be renamed.
	Synthetic bool
	// Op is the form that binds it ("let", "lambda", "macrolet", ...).
	Op string
	// Keyword marks a parameter after &key, whose name callers pass as a
	// keyword; renaming it would change the function's interface.
	Keyword bool

	index int // event index of the binding
}

// Ref is a symbol evaluated as a reference, or assigned by set!.
type Ref struct {
	// Node is the symbol node.
	Node *lisp.LVal
	// Binding is the binding it resolves to, nil when it is free in the
	// walked form.
	Binding *Binding
	// Scope is the innermost scope enclosing the reference.
	Scope *Scope
	// Set marks a set! target.  Head marks the head of a function call.
	Set, Head bool

	index int // event index
}

// Scopes is the binding structure of one walked form.
type Scopes struct {
	// Root is the scope around the form.
	Root *Scope
	// Bindings and Refs list every binding and reference, in walk order.
	Bindings []*Binding
	Refs     []*Ref

	points []pointEvent // WalkForm events, for LiveAcross
	loops  []loop       // while forms and dotimes scopes
}

type pointEvent struct {
	node       *lisp.LVal
	start, end int
	scope      *Scope
}

// AnalyzeScopes walks form as code (without expanding macros: pass it the
// output of ExpandAll or lisp.LEnv.MacroExpandAll) and returns its scopes,
// bindings and references, each reference resolved to the binding it
// denotes or marked free.
func AnalyzeScopes(form *lisp.LVal) *Scopes {
	sc := &Scopes{Root: &Scope{}}
	stack := []*Scope{sc.Root}
	type formMark struct {
		op    string
		node  *lisp.LVal
		index int
		depth int
		scope *Scope
	}
	var forms []formMark
	type depthMark struct {
		index, depth int
		scope        bool // a WalkEnter/WalkLeave event
	}
	var events []depthMark
	index := 0
	WalkCode(form, func(n *lisp.WalkNode) bool {
		i := index
		index++
		top := stack[len(stack)-1]
		events = append(events, depthMark{index: i, depth: n.Depth,
			scope: n.Event == lisp.WalkEnter || n.Event == lisp.WalkLeave})
		switch n.Event {
		case lisp.WalkEnter:
			s := &Scope{Node: n.Node, Op: n.Op, Function: n.Function, Parent: top, start: i}
			top.Children = append(top.Children, s)
			stack = append(stack, s)
		case lisp.WalkLeave:
			top.end = i
			if n.Op == "dotimes" {
				sc.loops = append(sc.loops, loop{start: top.start, end: i, scope: top})
			}
			stack = stack[:len(stack)-1]
		case lisp.WalkBind:
			b := &Binding{Name: n.Node, Scope: top, Op: n.Op, Keyword: n.Keyword, index: i}
			_, hasSource := n.Node.Source()
			b.Synthetic = n.Op == "expr" && !hasSource
			top.Bindings = append(top.Bindings, b)
			sc.Bindings = append(sc.Bindings, b)
		case lisp.WalkRef, lisp.WalkSet:
			r := &Ref{Node: n.Node, Scope: top, Set: n.Event == lisp.WalkSet, Head: n.Head, index: i}
			r.Binding = resolve(stack, n.Node.Str)
			if r.Binding != nil {
				r.Binding.Refs = append(r.Binding.Refs, r)
			}
			top.Refs = append(top.Refs, r)
			sc.Refs = append(sc.Refs, r)
		case lisp.WalkForm:
			forms = append(forms, formMark{node: n.Node, op: n.Op, index: i, depth: n.Depth, scope: top})
		default:
		}
		return true
	})
	sc.Root.end = index
	// A form ends before the first later event no deeper than it.  A
	// form's own scope boundaries share its depth; an enclosing scope's
	// boundaries are shallower.
	for _, f := range forms {
		end := index - 1
		for _, e := range events[f.index+1:] {
			if e.depth < f.depth || (!e.scope && e.depth == f.depth) {
				end = e.index - 1
				break
			}
		}
		sc.points = append(sc.points, pointEvent{node: f.node, start: f.index, end: end, scope: f.scope})
		if f.op == "while" {
			sc.loops = append(sc.loops, loop{start: f.index, end: end})
		}
	}
	return sc
}

type loop struct {
	start, end int
	scope      *Scope // the dotimes scope; nil for while
}

func resolve(stack []*Scope, name string) *Binding {
	for i := len(stack) - 1; i >= 0; i-- {
		bs := stack[i].Bindings
		for j := len(bs) - 1; j >= 0; j-- {
			if bs[j].Name.Str == name {
				return bs[j]
			}
		}
	}
	return nil
}

// FreeVars returns the symbols form references or assigns that no binding
// inside form introduces, one per name, in order of first occurrence.
// ELPS is a Lisp-1, so the functions a form calls are among them; quoted
// data never is.  Pass expanded code (see AnalyzeScopes).
func FreeVars(form *lisp.LVal) []*lisp.LVal {
	return uniqueNodes(AnalyzeScopes(form).freeRefs(nil))
}

func (sc *Scopes) freeRefs(s *Scope) []*Ref {
	var out []*Ref
	for _, r := range sc.Refs {
		if r.Binding == nil && (s == nil || s.encloses(r.Scope)) {
			out = append(out, r)
		}
	}
	return out
}

func uniqueNodes(refs []*Ref) []*lisp.LVal {
	out := []*lisp.LVal{}
	seen := map[string]bool{}
	for _, r := range refs {
		if !seen[r.Node.Str] {
			seen[r.Node.Str] = true
			out = append(out, r.Node)
		}
	}
	return out
}

// encloses reports whether t is s or nested inside s.
func (s *Scope) encloses(t *Scope) bool {
	for ; t != nil; t = t.Parent {
		if t == s {
			return true
		}
	}
	return false
}

// refs calls fn for each reference inside s, nested scopes included.
func (s *Scope) refs(fn func(*Ref)) {
	for _, r := range s.Refs {
		fn(r)
	}
	for _, c := range s.Children {
		c.refs(fn)
	}
}

// Free returns the references inside s, nested scopes included, that do not
// resolve to a binding of s or of a scope inside it: free variables of the
// whole form and bindings of enclosing scopes.  For a function scope, these
// are what the closure uses from outside.
func (s *Scope) Free() []*Ref {
	var out []*Ref
	s.refs(func(r *Ref) {
		if r.Binding == nil || !s.encloses(r.Binding.Scope) {
			out = append(out, r)
		}
	})
	return out
}

// Captured returns the bindings of enclosing scopes that code inside s
// uses, once each, in order of first use: what a closure over s captures.
func (s *Scope) Captured() []*Binding {
	var out []*Binding
	seen := map[*Binding]bool{}
	for _, r := range s.Free() {
		if r.Binding != nil && !seen[r.Binding] {
			seen[r.Binding] = true
			out = append(out, r.Binding)
		}
	}
	return out
}

// Used reports whether the binding is read: a reference that is not a set!.
func (b *Binding) Used() bool {
	for _, r := range b.Refs {
		if !r.Set {
			return true
		}
	}
	return false
}

// Live is the result of LiveAcross for one point.
type Live struct {
	// Point is the form the predicate selected.
	Point *lisp.LVal
	// Live are the bindings whose values may still be read after Point
	// completes, in binding order.
	Live []*Binding
}

// LiveAcross returns, for every form in body the point predicate selects
// (in walk order), the bindings introduced inside body that are in scope
// at the point and whose value may be read after it completes: a program
// suspended at the point must keep exactly these (plus body's free
// variables, see FreeVars) to resume.  Pass expanded code.
//
// Order is ELPS evaluation order, left to right.  The answer is
// conservative where control flow is not: a read anywhere in an enclosing
// while or dotimes body (which runs again) counts as after the point, a
// dotimes variable is live throughout its body, and a read inside a
// function created before the point counts as after it, since the closure
// may be called later.  Arguments of the point itself are consumed by it.
func LiveAcross(body *lisp.LVal, point func(*lisp.LVal) bool) []Live {
	return AnalyzeScopes(body).LiveAcross(point)
}

// LiveAcross is LiveAcross over already-analyzed scopes.
func (sc *Scopes) LiveAcross(point func(*lisp.LVal) bool) []Live {
	var out []Live
	for _, p := range sc.points {
		if !point(p.node) {
			continue
		}
		res := Live{Point: p.node, Live: []*Binding{}}
		for _, b := range sc.Bindings {
			if b.index > p.start || !b.Scope.encloses(p.scope) {
				continue
			}
			if sc.liveAfter(b, p) {
				res.Live = append(res.Live, b)
			}
		}
		out = append(out, res)
	}
	return out
}

func (sc *Scopes) liveAfter(b *Binding, p pointEvent) bool {
	for _, l := range sc.loops {
		if l.scope != nil && l.scope == b.Scope && l.start <= p.start && p.end <= l.end {
			return true // the dotimes counter
		}
	}
	for _, r := range b.Refs {
		if r.Set {
			continue
		}
		if r.index > p.end {
			return true
		}
		if r.index >= p.start { // inside the point
			continue
		}
		for _, l := range sc.loops {
			if l.start <= p.start && p.end <= l.end && l.start <= r.index && r.index <= l.end && b.index < l.start {
				return true
			}
		}
		for s := r.Scope; s != nil && s != b.Scope; s = s.Parent {
			if s.Function {
				return true
			}
		}
	}
	return false
}

// renamable reports whether Rename can rename b: not an expr placeholder
// (no source occurrence), not an &key parameter (its name is the keyword
// callers pass) and not a macrolet name (calls to local macros are opaque
// to an unexpanded walk, so their uses cannot be found).
func (b *Binding) renamable() bool {
	return !b.Synthetic && !b.Keyword && b.Op != "macrolet"
}

// FreshNames maps every renamable binding (see Rename) in the analyzed form to a name
// made of prefix and a counter (prefix1, prefix2, ...) that occurs nowhere
// in form, for a hygienic Rename that makes every local distinct.
func (sc *Scopes) FreshNames(form *lisp.LVal, prefix string) map[*Binding]string {
	used := map[string]bool{}
	var collect func(v *lisp.LVal)
	collect = func(v *lisp.LVal) {
		if v == nil {
			return
		}
		if v.Type == lisp.LSymbol {
			used[v.Str] = true
		}
		for _, c := range v.Cells {
			collect(c)
		}
	}
	collect(form)
	out := make(map[*Binding]string)
	n := 0
	for _, b := range sc.Bindings {
		if !b.renamable() {
			continue
		}
		for {
			n++
			name := fmt.Sprintf("%s%d", prefix, n)
			if !used[name] {
				out[b] = name
				break
			}
		}
	}
	return out
}

// Rename returns form, which must be the form sc was computed from, with
// each binding in names renamed, together with every reference to it.  The
// renaming is hygienic: if a new name would change what any reference in
// the form denotes -- a renamed binding capturing another reference, or
// another binding capturing a renamed reference -- Rename returns an error
// and no form.  form is not modified; renamed symbols keep their source
// locations.  An expr placeholder, an &key parameter or a macrolet name
// cannot be renamed.
func (sc *Scopes) Rename(form *lisp.LVal, names map[*Binding]string) (*lisp.LVal, error) {
	byIndex := make(map[int]string)
	for b, name := range names {
		if !b.renamable() {
			return nil, fmt.Errorf("cannot rename %s: an expr placeholder, &key parameter or macrolet name", b.Name.Str)
		}
		byIndex[b.index] = name
		for _, r := range b.Refs {
			byIndex[r.index] = name
		}
	}
	index := 0
	w := &lisp.CodeWalker{
		KeepGoing: true,
		Visit:     func(*lisp.WalkNode) bool { index++; return true },
		Replace: func(n *lisp.WalkNode) *lisp.LVal {
			name, ok := byIndex[index-1]
			if !ok || name == n.Node.Str {
				return nil
			}
			sym := lisp.Symbol(name)
			if loc, ok := n.Node.Source(); ok {
				sym.SetSource(&loc)
			}
			return sym
		},
	}
	out := w.Walk(form)
	if out != nil && out.Type == lisp.LError {
		return nil, fmt.Errorf("rename: %v", out)
	}
	// Hygiene: every reference must still denote the same binding.
	after := AnalyzeScopes(out)
	if len(after.Refs) != len(sc.Refs) || len(after.Bindings) != len(sc.Bindings) {
		return nil, errors.New("rename changed the form's structure")
	}
	pos := make(map[*Binding]int, len(sc.Bindings))
	for i, b := range sc.Bindings {
		pos[b] = i
	}
	apos := make(map[*Binding]int, len(after.Bindings))
	for i, b := range after.Bindings {
		apos[b] = i
	}
	for i, r := range sc.Refs {
		ar := after.Refs[i]
		want, got := -1, -1
		if r.Binding != nil {
			want = pos[r.Binding]
		}
		if ar.Binding != nil {
			got = apos[ar.Binding]
		}
		if want != got {
			return nil, fmt.Errorf("renaming would capture %s (now %s) at reference %d", r.Node.Str, ar.Node.Str, i+1)
		}
	}
	return out, nil
}
