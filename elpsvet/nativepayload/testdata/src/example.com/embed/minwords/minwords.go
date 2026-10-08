// Package minwords is the fixture of an embedder that sets AllowMinWords to
// 5.  A justification of fewer words is reported; one of five words is
// accepted.  A construction with no want comment asserts no diagnostic.
package minwords

import "github.com/luthersystems/elps/lisp"

// cache is a mutable handle.
type cache struct {
	m map[string]int
}

func markers(c *cache) []*lisp.LVal {
	a := lisp.Native(c) /*embedvet:allow published once, only read*/ // want `payload type \*minwords\.cache is not a known-safe value type`
	b := lisp.Native(c) /*embedvet:allow published once and only read*/
	return []*lisp.LVal{a, b}
}
