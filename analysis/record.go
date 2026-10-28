// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
)

// RecordMethods maps the name of a record type to the methods that
// s:defmethod calls add to it, and each method to its signature after the
// receiver.  A nil signature means the formals cannot be read, or that two
// s:defmethod calls give the method different arities.
//
// The lint checks record-unknown-member and record-method-arity read it
// from Result.RecordMethods.  A type's s:make-record is often in one file
// and an s:defmethod for it in another.  Without the workspace's methods,
// a call to such a method reads as an unknown member.
//
// A type is keyed by its name without a package.  Two packages with a type
// of one name share one method set; the checks then miss a finding, never
// report a false one.
type RecordMethods map[string]map[string]*Signature

// recordDefmethod is the head of the call that adds a method to a record
// type.
const recordDefmethod = "s:defmethod"

// ScanRecordMethods returns the methods that the s:defmethod calls in exprs
// add.  A call is (s:defmethod TYPE NAME (recv formals...) body...), with
// TYPE and NAME symbols.  It finds the calls in code position anywhere in
// exprs: an s:defmethod inside a let is a call too.
func ScanRecordMethods(exprs []*lisp.LVal) RecordMethods {
	var out RecordMethods
	for _, top := range exprs {
		if !mentionsSymbol(top, recordDefmethod) {
			continue
		}
		sites, _ := astutil.FindCallSites(top, recordDefmethod)
		for _, site := range sites {
			c := site.Form.Cells
			if len(c) < 4 || c[1].Type != lisp.LSymbol || c[2].Type != lisp.LSymbol {
				continue
			}
			if out == nil {
				out = make(RecordMethods)
			}
			out.add(c[1].Str, c[2].Str, RecordMethodSignature(c[3]))
		}
	}
	return out
}

// Merge adds the methods of o to m.  It returns m, or a new map when m is
// nil and o is not empty.
func (m RecordMethods) Merge(o RecordMethods) RecordMethods {
	for typ, methods := range o {
		for name, sig := range methods {
			if m == nil {
				m = make(RecordMethods)
			}
			m.add(typ, name, sig)
		}
	}
	return m
}

// add records one method.  A method added twice with different arities
// gets a nil signature: the checks cannot know which call wins at run time.
func (m RecordMethods) add(typ, name string, sig *Signature) {
	methods := m[typ]
	if methods == nil {
		methods = make(map[string]*Signature)
		m[typ] = methods
	}
	if old, ok := methods[name]; ok && !sameArity(old, sig) {
		sig = nil
	}
	methods[name] = sig
}

// sameArity reports whether a and b accept the same numbers of arguments.
func sameArity(a, b *Signature) bool {
	if a == nil || b == nil {
		return a == b
	}
	return a.MinArity() == b.MinArity() && a.MaxArity() == b.MaxArity()
}

// RecordMethodFormals returns a method's formals, the receiver first, with
// each typed formal, [name type] (luthersystems/elps#905), replaced by its
// name.  It returns nil when formals is not a list with a receiver.
func RecordMethodFormals(formals *lisp.LVal) []*lisp.LVal {
	if formals == nil || formals.Type != lisp.LSExpr || len(formals.Cells) == 0 {
		return nil
	}
	names := make([]*lisp.LVal, 0, len(formals.Cells))
	for _, p := range formals.Cells {
		if p.Type == lisp.LSExpr && len(p.Cells) == 2 && p.Cells[0].Type == lisp.LSymbol {
			p = p.Cells[0]
		}
		names = append(names, p)
	}
	return names
}

// RecordMethodSignature returns the signature of a method's formals after
// the receiver, or nil when they are not a list with a receiver.
func RecordMethodSignature(formals *lisp.LVal) *Signature {
	names := RecordMethodFormals(formals)
	if names == nil {
		return nil
	}
	return &Signature{Params: lisp.ParseFormals(lisp.SExpr(names[1:]))}
}

// RecordDefmethod is one s:defmethod call in code position in a top-level
// form.
type RecordDefmethod struct {
	// Call is the s:defmethod call.
	Call *lisp.LVal
	// NotLoadScope is "" when the call runs at load.  Otherwise it names
	// the outermost form that keeps the call from running at load: the
	// head of a call or macro call around it, or the CodeWalker's name for
	// the function body that holds it (defun, lambda, flet, ...).
	NotLoadScope string
}

// maxRecordWalk bounds the lists RecordDefmethods walks in one top form.
// A call past the budget is not returned, so a check that reads the
// result misses a finding and makes no false one.
const maxRecordWalk = 1 << 16

// RecordDefmethods returns the s:defmethod calls in top, each with whether
// it runs at load.
//
// A template publishes a record type with its methods as they stand at
// publication, so only a s:defmethod that runs at load is part of the
// type.  A call runs at load when nothing around it defers it or hides it:
//
//   - A function body (defun, lambda, a flet or labels function) runs at
//     call time.
//   - The arguments of a function call or a macro call run when the callee
//     decides.  Macros are not expanded, so a macro such as an endpoint
//     definition counts as a call.  The value of a set or defconst runs at
//     load, so those two calls are the exception.
//   - Every other form the CodeWalker knows (let, let*, progn, if, when,
//     the body of flet and labels, ...) runs its parts at load.
//
// The rule reads only the walker's scope and call events, so it follows
// the walker's grammar for every special form.  One walk covers all calls
// in top.
func RecordDefmethods(top *lisp.LVal) []RecordDefmethod {
	// frames holds the forms around the walk position that defer it, each
	// with the depth of its event.  The walker reports everything inside a
	// form or scope deeper than the form or scope, so a frame ends at the
	// first event that is not deeper.
	type frame struct {
		where string
		depth int
	}
	var frames []frame
	var out []RecordDefmethod
	seen := make(map[*lisp.LVal]bool)
	where := func() string {
		if len(frames) == 0 {
			return ""
		}
		return frames[len(frames)-1].where
	}
	push := func(depth int, name string) {
		// The outermost form names the finding: it is the one to move.
		if outer := where(); outer != "" {
			name = outer
		}
		frames = append(frames, frame{where: name, depth: depth})
	}
	cw := &lisp.CodeWalker{KeepGoing: true, Visit: func(n *lisp.WalkNode) bool {
		for len(frames) > 0 && frames[len(frames)-1].depth >= n.Depth {
			frames = frames[:len(frames)-1]
		}
		switch n.Event {
		case lisp.WalkEnter:
			if n.Function {
				push(n.Depth, n.Op)
			}
		case lisp.WalkForm:
			if n.Op != "" {
				break
			}
			head := astutil.HeadSymbol(n.Node)
			if head == recordDefmethod && !seen[n.Node] {
				seen[n.Node] = true
				out = append(out, RecordDefmethod{Call: n.Node, NotLoadScope: where()})
			}
			if !loadScopeCall(head) {
				push(n.Depth, head)
			}
		case lisp.WalkRef, lisp.WalkSet, lisp.WalkBind, lisp.WalkDefine, lisp.WalkLiteral, lisp.WalkData, lisp.WalkLeave:
		}
		return true
	}}
	codewalk.Occurrences(cw, maxRecordWalk, top)
	return out
}

// loadScopeCall reports whether head is a call whose value argument runs
// at load: a set or a defconst.  Both are calls to the CodeWalker, which
// knows only special forms.
func loadScopeCall(head string) bool {
	switch strings.TrimPrefix(head, lisp.DefaultLangPackage+":") {
	case "set", "defconst":
		return true
	}
	return false
}

// mentionsSymbol reports whether form holds the symbol name.  It skips the
// call-site query for the forms that cannot hold a call, most of a usual
// file.
func mentionsSymbol(form *lisp.LVal, name string) bool {
	found := false
	astutil.Walk([]*lisp.LVal{form}, func(node, _ *lisp.LVal, _ int) {
		found = found || node.IsSymbol(name)
	})
	return found
}
