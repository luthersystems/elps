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

// thingCodec re-declares a codec of another module in this one, under
// this module's name, which makes *durableelps.Thing durable here.
var thingCodec = durableelps.Codec.WithName("embed:thing")

// A codec of another module (durableelps.Codec) need not be listed.
func complete() {
	_, _ = libjson.NewFrozenDurableRegistry(durablecodecs.DurableCodec, durablecodecs.ForeignCodec, durablecodecs.BothCodec, durablecodecs.AliasCodec, localCodec, thingCodec)
}

func missing() {
	_, _ = libjson.NewFrozenDurableRegistry(durablecodecs.DurableCodec, durablecodecs.BothCodec, durablecodecs.AliasCodec, localCodec, thingCodec) // want `libjson\.NewFrozenDurableRegistry does not list the durable codec durablecodecs\.ForeignCodec`
}

// A spread list is not checked.
func spread(codecs []libjson.DurableEntry) {
	_, _ = libjson.NewFrozenDurableRegistry(codecs...)
}

// A codec exported by an imported package of this module makes its type
// durable here, and so does a codec of another module that this module
// re-declares (thingCodec).
func builtImported() (*lisp.LVal, *lisp.LVal) {
	return lisp.Native(durablecodecs.NewHandle()), lisp.Native(durableelps.NewThing())
}

func builtLocal() *lisp.LVal {
	return lisp.Native(&local{})
}

// A codec renamed in the call (Codec.WithName) counts as listed.
func renamed() {
	_, _ = libjson.NewFrozenDurableRegistry(durablecodecs.DurableCodec.WithName("test:renamed"), durablecodecs.ForeignCodec, durablecodecs.BothCodec, durablecodecs.AliasCodec, localCodec, thingCodec)
}
