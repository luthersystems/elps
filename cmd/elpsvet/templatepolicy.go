// Copyright © 2026 The ELPS authors

package main

import (
	"go/types"
)

// templatePolicyPkgPath is the internal package declaring the Immutable
// marker contract.  The method is matched together with this path so that a
// downstream type with a same-named method of its own cannot claim the tier.
const templatePolicyPkgPath = "github.com/luthersystems/elps/internal/templatepolicy"

// templateImmutableMethod is the one method of
// internal/templatepolicy.Immutable.
const templateImmutableMethod = "templateImmutable"

// declaresTemplateImmutable reports whether t is a STRUCT VALUE whose method
// set carries internal/templatepolicy.Immutable's unexported
// templateImmutable().  It is the marker tier of elpsnativepayload
// (elpsvet/nativepayload), which keeps its own copy; the two must agree.
func declaresTemplateImmutable(t types.Type) bool {
	if _, ok := t.Underlying().(*types.Struct); !ok {
		return false
	}
	ms := types.NewMethodSet(t)
	for i := range ms.Len() {
		fn, ok := ms.At(i).Obj().(*types.Func)
		if !ok || fn.Name() != templateImmutableMethod {
			continue
		}
		if fn.Pkg() == nil || fn.Pkg().Path() != templatePolicyPkgPath {
			continue
		}
		sig, ok := fn.Type().(*types.Signature)
		if ok && sig.Params().Len() == 0 && sig.Results().Len() == 0 {
			return true
		}
	}
	return false
}

// isLValTypeField reports whether obj is the lisp.LVal.Type field object,
// matched by object rather than by the receiver's spelled type.
func isLValTypeField(obj types.Object) bool {
	v, ok := obj.(*types.Var)
	return ok && v.IsField() && v.Name() == "Type" && v.Pkg() != nil && v.Pkg().Path() == lispPkgPath
}
