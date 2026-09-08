package consumer

import (
	"github.com/luthersystems/elps/lisp"
	"lookalike"
)

type Alias = lisp.CachedSource
type Defined lisp.CachedSource

var cache map[string]*lisp.CachedSource
var aliases []*Alias
var value lisp.CachedSource
var raw []*lisp.LVal                           // want "package-level var raw keeps"
var foreign map[string]*lookalike.CachedSource // want "package-level var foreign keeps"
var defined *Defined                           // want "package-level var defined keeps"
var program lisp.Program                       // want "package-level var program keeps"
var wrapper struct {                           // want "package-level var wrapper keeps"
	Cache map[string]*lisp.CachedSource
	Raw   *lisp.LVal
}
