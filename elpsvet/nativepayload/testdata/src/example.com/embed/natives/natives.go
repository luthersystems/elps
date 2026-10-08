// Package natives is the fixture of an embedder's elpsnativepayload:
// an audited row of another module (dec.Decimal), an exempting call
// (probe.Capture), the embedder's allow marker (//embedvet:allow) and its
// own fix text.  Interface-typed payloads are hidden.  A construction with
// no want comment asserts no diagnostic.
package natives

import (
	"time"
	"unsafe"

	"example.com/dec"
	"example.com/embed/probe"
	"github.com/luthersystems/elps/lisp"
)

// tree is a mutable handle that holds a transaction context.
type tree struct {
	ctx probe.Context
	n   int
}

func newTree(ctx probe.Context) *tree { return &tree{ctx: ctx} }

func openUndeclared(ctx probe.Context) *lisp.LVal {
	return lisp.Native(newTree(ctx)) // want `lisp\.Native payload type \*natives\.tree is not a known-safe value type: .* audited immutable payloads; declare the capture with probe\.Capture`
}

// openDeclared calls the exempting function, so nothing it builds is
// reported, closures included.
func openDeclared(ctx probe.Context) (*lisp.LVal, func() *lisp.LVal) {
	probe.Capture(ctx, "open")
	return lisp.Native(newTree(ctx)), func() *lisp.LVal { return lisp.Native(newTree(ctx)) }
}

func declareElsewhere(ctx probe.Context) { probe.Capture(ctx, "open") }

// A call in a callee does not exempt the caller.
func buildFromElsewhere(ctx probe.Context) *lisp.LVal {
	declareElsewhere(ctx)
	return lisp.Native(newTree(ctx)) // want `payload type \*natives\.tree is not a known-safe value type`
}

type cache struct{ hits int }

func builtins(b []byte, p unsafe.Pointer, u uintptr) []*lisp.LVal {
	return []*lisp.LVal{
		lisp.Native(&cache{}),           // want `payload type \*natives\.cache is not a known-safe value type`
		lisp.Native(map[string]int{}),   // want `payload type map\[string\]int is not a known-safe value type`
		lisp.Native(b),                  // want `payload type \[\]byte is not a known-safe value type`
		lisp.Native(p),                  // want `payload type unsafe\.Pointer is not a known-safe value type`
		lisp.Native(u),                  // want `payload type uintptr is not a known-safe value type`
		lisp.NativeOf[*cache](&cache{}), // want `lisp\.NativeOf payload type \*natives\.cache`
	}
}

var pkgNative = lisp.Native(&cache{}) // want `payload type \*natives\.cache is not a known-safe value type`

// The audited row is true at every site.
func allowlisted(d dec.Decimal) []*lisp.LVal {
	return []*lisp.LVal{lisp.Native(d), lisp.NativeOf(d), &lisp.LVal{Native: d}}
}

func rawTime(t time.Time) *lisp.LVal {
	return lisp.Native(t) // want `payload type time\.Time is not a known-safe value type`
}

type version string

func basics(s string, i int, v version) []*lisp.LVal {
	return []*lisp.LVal{lisp.Native(s), lisp.Native(i), lisp.Native(v), lisp.Native(3.5), lisp.Native(true)}
}

func bypasses(v *lisp.LVal, c *cache) *lisp.LVal {
	v.Native = c                                     // want `LVal\.Native assignment payload type \*natives\.cache`
	return &lisp.LVal{Type: lisp.LNative, Native: c} // want `LVal\.Native literal payload type \*natives\.cache`
}

// A kernel slot built outside package lisp is reported as a misuse.
func kernelSlot(b []byte) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LBytes, Native: &b} // want `payload type \*\[\]byte is a kernel representation slot`
}

func address(v *lisp.LVal) *interface{} {
	return &v.Native // want `address of LVal\.Native taken: .* annotate //embedvet:allow`
}

// Interface-typed payloads are hidden in this configuration.
func dynamic(err error, x interface{}) []*lisp.LVal {
	return []*lisp.LVal{lisp.Native(err), lisp.Value(x)}
}

// allowedByDoc carries the marker on the function.
//
//embedvet:allow the cache is published once and only read afterwards
func allowedByDoc(c *cache) *lisp.LVal {
	return lisp.Native(c)
}

func allowedPlacements(c, d *cache) []*lisp.LVal {
	//embedvet:allow standalone form for the next line
	a := lisp.Native(c)
	b := lisp.Native(c) //embedvet:allow trailing form for this line
	e := lisp.Native(d) // want `payload type \*natives\.cache is not a known-safe value type`
	f := lisp.Native(d) /*embedvet:allow too short*/                         // want `payload type \*natives\.cache is not a known-safe value type`
	g := lisp.Native(d) /*elpsvet:allow-native elps marker, another module*/ // want `payload type \*natives\.cache is not a known-safe value type`
	return []*lisp.LVal{a, b, e, f, g}
}
