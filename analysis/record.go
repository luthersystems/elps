// Copyright © 2026 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/astutil"
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
