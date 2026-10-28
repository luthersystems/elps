// Copyright © 2026 The ELPS authors

package libschema

import "github.com/luthersystems/elps/lisp"

// Type support for lisp:match (luthersystems/elps#905).  LoadPackage
// registers matchIs and matchField, so [name T] and (T :field p ...)
// patterns work in each VM that loads package s.

// matchIs is the type test of a [name T] pattern: s:is?.
func matchIs(env *lisp.LEnv, v, typ *lisp.LVal) *lisp.LVal {
	return Is(env, v, typ)
}

// matchField reads field name of the record object v for a
// (T :field p ...) pattern, as a call (v 'name) reads it: a field that is
// not set reads as ().  A name that is not a field of v, a hidden field of
// a Go record, or a v that is not a record object is an error.
func matchField(env *lisp.LEnv, v *lisp.LVal, name string) (*lisp.LVal, *lisp.LVal) {
	if o, ok := lispObjectOf(v); ok {
		if !o.desc.hasField(name) {
			return nil, env.ErrorConditionf(UnknownOperation, "match: %s has no field %s", o.desc.display, name)
		}
		f, ok := o.fields.Map().Get(lisp.String(name))
		if !ok {
			return lisp.Nil(), nil
		}
		return f, nil
	}
	if t := goObjectType(v); t != nil {
		r, ok := t.Record(v)
		i, known := t.index[name]
		if !ok || !known || !t.spec.Fields[i].Getter {
			return nil, env.ErrorConditionf(UnknownOperation, "match: %s has no field %s", t.spec.DisplayName, name)
		}
		return r.At(i), nil
	}
	return nil, env.ErrorConditionf(BadArgs, "match: a field pattern needs a record object, got %v", v)
}
