// Copyright © 2026 The ELPS authors

// Package vetpolicy holds the type checks that more than one elps analyzer
// makes about the template publication contract.  cmd/elpsvet and
// elpsvet/nativepayload both import it, so the two analyzers cannot drift
// apart on what a marker type or the LVal header field is.
package vetpolicy

import (
	"go/types"
)

// LispPkgPath is the import path of the interpreter core.
const LispPkgPath = "github.com/luthersystems/elps/lisp"

// TemplatePolicyPkgPath is the internal package declaring the Immutable
// marker contract.  The method is matched together with this path so that a
// downstream type with a same-named method of its own cannot claim the tier.
const TemplatePolicyPkgPath = "github.com/luthersystems/elps/internal/templatepolicy"

// templateImmutableMethod is the one method of
// internal/templatepolicy.Immutable.  It is UNEXPORTED, which is the point:
// only a type embedding templatepolicy.Marker can have it, and only this
// module can embed that.
const templateImmutableMethod = "templateImmutable"

// lTypeFieldName is the lisp.LVal field naming the HEADER a value is.
const lTypeFieldName = "Type"

// DeclaresTemplateImmutable reports whether t is a STRUCT VALUE whose method
// set carries internal/templatepolicy.Immutable's unexported
// templateImmutable().
//
// Both halves are the runtime's.  The method is matched by name AND by
// declaring package, which is how the runtime's type assertion behaves: an
// unexported method is only satisfiable by embedding templatepolicy.Marker,
// which nothing outside this module can import.  The struct-value half is
// the half that is easy to lose.  A *T inherits T's method set and so would
// pass an assertion, but the runtime also requires
// reflect.TypeOf(payload).Kind() == reflect.Struct, because a caller holding
// the pointer can replace the whole pointee no matter how private its
// fields are.  A pointer form needs TemplateWithNativePolicy approval
// instead.
func DeclaresTemplateImmutable(t types.Type) bool {
	if _, ok := t.Underlying().(*types.Struct); !ok {
		return false
	}
	ms := types.NewMethodSet(t)
	for i := range ms.Len() {
		fn, ok := ms.At(i).Obj().(*types.Func)
		if !ok || fn.Name() != templateImmutableMethod {
			continue
		}
		if fn.Pkg() == nil || fn.Pkg().Path() != TemplatePolicyPkgPath {
			continue
		}
		sig, ok := fn.Type().(*types.Signature)
		if ok && sig.Params().Len() == 0 && sig.Results().Len() == 0 {
			return true
		}
	}
	return false
}

// IsLValTypeField reports whether obj is the lisp.LVal.Type field object,
// the header discriminant the template inventory switches on.  It matches
// the field object, never the receiver's spelled type, so ErrorVal,
// conversions and embedding all resolve to the same field.
func IsLValTypeField(obj types.Object) bool {
	v, ok := obj.(*types.Var)
	return ok && v.IsField() && v.Name() == lTypeFieldName && v.Pkg() != nil && v.Pkg().Path() == LispPkgPath
}
