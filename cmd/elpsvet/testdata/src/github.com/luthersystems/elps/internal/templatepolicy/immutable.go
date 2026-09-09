// Package templatepolicy is a minimal stub of
// github.com/luthersystems/elps/internal/templatepolicy for analysistest.
// Only the shape the elpsnativepayload rule matches matters: an interface
// whose one method is UNEXPORTED, and the marker struct that supplies it.
//
// The import path is load-bearing twice over.  The rule matches the method
// by name AND by declaring package, so a lookalike declared elsewhere does
// not claim the tier; and Go's internal rule means only a fixture package
// under github.com/luthersystems/elps/ can import this at all, which is why
// the marker fixtures live in ../nativemarker rather than beside the rest.
package templatepolicy

// Immutable mirrors the real contract.
type Immutable interface {
	templateImmutable()
}

// Marker mirrors the real embeddable marker.
type Marker struct{}

func (Marker) templateImmutable() {}
