// Package registrytext is the fixture of an embedder's DurableConfig.Registry
// text.  The diagnostic says where to list a new codec in the embedder's own
// words.
package registrytext

import "github.com/luthersystems/elps/lisp"

// widget has no codec and no TransientNative method.
type widget struct {
	n int
}

func build() *lisp.LVal {
	return lisp.Native(&widget{}) // want `libjson\.DurableCodec\[\*registrytext\.widget\] and list it in the embed codec table, give the type`
}
