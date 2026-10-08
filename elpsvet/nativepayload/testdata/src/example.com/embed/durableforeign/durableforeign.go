// Package durableforeign is the elpsdurablenative fixture for a codec of
// another module that this module does not re-declare.  The module's
// registry does not hold it, so a dump of the native fails at run time,
// and the analyzer reports the construction.
package durableforeign

import (
	"github.com/luthersystems/elps/durableelps"
	"github.com/luthersystems/elps/lisp"
)

func builtForeign() *lisp.LVal {
	return lisp.Native(durableelps.NewThing()) // want `payload type \*durableelps\.Thing has no durable codec`
}
