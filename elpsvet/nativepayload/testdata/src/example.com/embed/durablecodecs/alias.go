package durablecodecs

import (
	"github.com/luthersystems/elps/lisp"
	dur "github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// aliased is durable through a codec declared with an aliased import and
// a type alias.
type aliased struct{ n int }

type aliasedCodec = dur.DurableCodec[*aliased]

var AliasCodec aliasedCodec

func builtAliased() *lisp.LVal {
	return lisp.Native(&aliased{})
}
