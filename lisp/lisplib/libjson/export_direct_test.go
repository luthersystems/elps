// Copyright © 2026 The ELPS authors

package libjson

import "github.com/luthersystems/elps/lisp"

// LoadIndirectForTest exposes the original two-step decode so the fuzz
// differential can compare it with the direct decoder (elps#689).
func LoadIndirectForTest(b []byte, opts LoadOpts) *lisp.LVal {
	return (&Serializer{}).loadIndirect(b, opts)
}
