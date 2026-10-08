// Package anypayload is the fixture of an embedder that hides
// interface-typed payloads, run with -anypayload set.
package anypayload

import (
	"example.com/dec"
	"github.com/luthersystems/elps/lisp"
)

func dynamic(err error, x interface{}) []*lisp.LVal {
	return []*lisp.LVal{
		lisp.Native(err), // want `lisp\.Native payload type error is not statically known .* annotate //embedvet:allow`
		lisp.Value(x),    // want `lisp\.Value payload type interface\{\} is not statically known`
	}
}

func generic[T any](x T) *lisp.LVal {
	return lisp.Native(x) // want `payload type T is not statically known`
}

// A concrete audited payload stays exempt with the flag set.
func allowlisted(d dec.Decimal) *lisp.LVal { return lisp.Native(d) }
