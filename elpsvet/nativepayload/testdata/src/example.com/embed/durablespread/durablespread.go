// Package durablespread is the elpsdurablenative fixture for a package
// whose only registry call spreads a slice.  The spread lists nothing the
// analyzer can read, so an unexported codec here is still reported.
package durablespread

import "github.com/luthersystems/elps/lisp/lisplib/libjson"

type thing struct{ n int }

var hidden = libjson.DurableCodec[*thing]{Name: "test:hidden", Version: 1} // want `durable codec hidden is unexported`

func spread() {
	_, _ = libjson.NewFrozenDurableRegistry([]libjson.DurableEntry{hidden}...)
}
