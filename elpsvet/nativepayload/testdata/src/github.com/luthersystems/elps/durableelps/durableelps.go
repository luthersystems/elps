// Package durableelps is the elpsdurablenative fixture for elps's own
// configuration: module github.com/luthersystems/elps and the marker
// //elpsvet:transient.
package durableelps

import (
	"regexp"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Thing is durable through an exported codec that no elps registry lists.
type Thing struct{ n int }

var Codec = libjson.DurableCodec[*Thing]{Name: "elps:thing", Version: 1}

// NewThing returns a durable payload.
func NewThing() *Thing { return &Thing{} }

func built() *lisp.LVal { return lisp.Native(NewThing()) }

// A duration is a type of another module, so the site marker applies.
func duration(d time.Duration) *lisp.LVal {
	return lisp.Native(d) //elpsvet:transient a duration in this fixture is never saved
}

func durationUnmarked(d time.Duration) *lisp.LVal {
	return lisp.Native(d) // want `lisp\.Native payload type time\.Duration has no durable codec and is not marked transient; declare a package-level libjson\.DurableCodec\[time\.Duration\] and pass it to libjson\.NewFrozenDurableRegistry`
}

// compiled is an elps type, so the marker cannot mark it.
type compiled struct{ re *regexp.Regexp }

func regexpMarked(re *regexp.Regexp) *lisp.LVal {
	return lisp.Native(compiled{re: re}) //elpsvet:transient a reason // want `belongs to github\.com/luthersystems/elps, so //elpsvet:transient cannot mark it`
}

// The embedder's marker means nothing here.
func otherMarker(d time.Duration) *lisp.LVal {
	return lisp.Native(d) //embedvet:transient another module's marker // want `time\.Duration has no durable codec`
}

// Untyped nil and interface payloads are not reported.
func nothing(x interface{}) []*lisp.LVal {
	return []*lisp.LVal{lisp.Native(nil), lisp.Native(x)}
}
