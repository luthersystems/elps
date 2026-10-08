// Package durableregistry is the elpsdurablenative fixture for an
// embedder package that builds the registry.
package durableregistry

import (
	"example.com/embed/durablecodecs"
	"github.com/luthersystems/elps/durableelps"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

type local struct{ n int }

// localCodec is unexported, which is fine in the package that builds the
// registry.
var localCodec = libjson.DurableCodec[*local]{Name: "test:local", Version: 1}

// A codec of another module (durableelps.Codec) need not be listed.
func complete() {
	_, _ = libjson.NewFrozenDurableRegistry(durablecodecs.DurableCodec, durablecodecs.ForeignCodec, durablecodecs.BothCodec, durablecodecs.AliasCodec, localCodec)
}

func missing() {
	_, _ = libjson.NewFrozenDurableRegistry(durablecodecs.DurableCodec, durablecodecs.BothCodec, durablecodecs.AliasCodec, localCodec) // want `libjson\.NewFrozenDurableRegistry does not list the durable codec durablecodecs\.ForeignCodec`
}

// A spread list is not checked.
func spread(codecs []libjson.DurableEntry) {
	_, _ = libjson.NewFrozenDurableRegistry(codecs...)
}

// A codec exported by an imported package makes its type durable here,
// in this module or another.
func builtImported() (*lisp.LVal, *lisp.LVal) {
	return lisp.Native(durablecodecs.NewHandle()), lisp.Native(durableelps.NewThing())
}

func builtLocal() *lisp.LVal {
	return lisp.Native(&local{})
}
