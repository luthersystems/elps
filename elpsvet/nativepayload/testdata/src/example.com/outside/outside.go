// Package outside is a package of another module that elps's own
// elpsdurablenative meets when a module runs cmd/elpsvet over its tree.
// The rule checks only the module it was built for, so nothing here is
// reported: not the unmarked native and not the registry that leaves out
// elps's codecs.
package outside

import (
	"github.com/luthersystems/elps/durableelps"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

type handle struct{ n int }

func unmarked() *lisp.LVal { return lisp.Native(&handle{}) }

func registry() {
	_, _ = libjson.NewFrozenDurableRegistry()
	_ = durableelps.Codec
}
