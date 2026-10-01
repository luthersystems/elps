// Package markerfields exercises the elpsmarkerfields rule: a struct value
// that carries templatepolicy.Marker must hold only immutable fields.
//
// It sits under github.com/luthersystems/elps/ because only that tree can
// import internal/templatepolicy.
package markerfields

import (
	"time"
	"unsafe"

	"github.com/luthersystems/elps/internal/templatepolicy"
)

// clean holds only value fields, a nested value struct and an array.
type clean struct {
	templatepolicy.Marker
	n    int
	s    string
	f    float64
	b    bool
	pair struct{ a, b int }
	arr  [4]byte
	k    kind
}

type kind string

type hasMap struct {
	templatepolicy.Marker
	m map[string]int // want `marked struct hasMap: field m is a map`
}

type hasSlice struct {
	templatepolicy.Marker
	s []byte // want `marked struct hasSlice: field s is a slice`
}

type hasPointer struct {
	templatepolicy.Marker
	p *int // want `marked struct hasPointer: field p is a pointer`
}

type hasFunc struct {
	templatepolicy.Marker
	fn func() // want `marked struct hasFunc: field fn is a func`
}

type hasChan struct {
	templatepolicy.Marker
	c chan int // want `marked struct hasChan: field c is a chan`
}

type hasInterface struct {
	templatepolicy.Marker
	v any // want `marked struct hasInterface: field v is an interface`
}

type hasUintptr struct {
	templatepolicy.Marker
	u uintptr // want `marked struct hasUintptr: field u is a uintptr`
}

type hasUnsafePointer struct {
	templatepolicy.Marker
	u unsafe.Pointer // want `marked struct hasUnsafePointer: field u is an unsafe.Pointer`
}

type inner struct {
	n int
	m map[string]int
}

type nestedStruct struct {
	templatepolicy.Marker
	in inner // want `marked struct nestedStruct: field in.m is a map`
}

type nestedArray struct {
	templatepolicy.Marker
	arr [2]struct{ p *int } // want `marked struct nestedArray: field arr\[\].p is a pointer`
}

// foreign reaches a pointer inside another package's struct (time.Time.loc).
type foreign struct {
	templatepolicy.Marker
	t time.Time // want `marked struct foreign: field t.loc is a pointer`
}

// marked reaches the marker through an embedded struct; the method set still
// carries templateImmutable, so the rule applies.
type marked struct{ templatepolicy.Marker }

type indirect struct {
	marked
	m map[int]int // want `marked struct indirect: field m is a map`
}

// fieldAllowed carries a reasoned allow on the field.
type fieldAllowed struct {
	templatepolicy.Marker
	p *int //elpsvet:allow-marker pointee is never written after construction
}

// typeAllowed carries a reasoned allow on the type.
//
//elpsvet:allow-marker every reachable value is frozen at construction
type typeAllowed struct {
	templatepolicy.Marker
	m map[string]int
}

type lineAboveAllowed struct {
	templatepolicy.Marker
	//elpsvet:allow-marker slice backing array is private and never written
	s []byte
}

// A short reason does not suppress.
type shortAllow struct {
	templatepolicy.Marker
	p *int //elpsvet:allow-marker too short // want `marked struct shortAllow: field p is a pointer`
}

// Another marker does not suppress.
type otherMarker struct {
	templatepolicy.Marker
	p *int //elpsvet:allow-native pointee is never written // want `marked struct otherMarker: field p is a pointer`
}

// Unmarked structs are out of scope.
type unmarked struct {
	m map[string]int
	p *int
}

// A generic marked struct with a type-parameter field cannot be checked.
type generic[T any] struct {
	templatepolicy.Marker
	v T // want `marked struct generic: field v is a type parameter`
}

// A type declared in a function body is checked.
func local() any {
	type localMarked struct {
		templatepolicy.Marker
		m map[string]int // want `marked struct localMarked: field m is a map`
	}
	return localMarked{}
}

// An anonymous struct literal that embeds the marker is checked.
var anonymous = struct {
	templatepolicy.Marker
	m map[string]int // want `marked struct struct literal: field m is a map`
}{}

// A defined type over a marked struct is its own audit.
//
//elpsvet:allow-marker frozen map is never written after construction
type frozen struct {
	templatepolicy.Marker
	m map[string]int
}

type mutable frozen // want `marked struct mutable: field m is a map`

// A type over a struct whose field carries the allow inherits that audit.
type fieldAudited fieldAllowed

// An allow on a nested field covers that field.
type nestedAllowed struct {
	templatepolicy.Marker
	inner struct { // want `marked struct nestedAllowed: field inner.p is a pointer`
		//elpsvet:allow-marker map stays frozen after construction
		m map[string]int
		p *int
	}
}

// A comment after the closing brace allows nothing.
type trailingBrace struct {
	templatepolicy.Marker
	m map[string]int // want `marked struct trailingBrace: field m is a map`
	s []int } //elpsvet:allow-marker slice stays frozen after construction // want `marked struct trailingBrace: field s is a slice`

// An alias is the same type and is checked at its declaration.
type aliasOfClean = clean

var _ = []any{clean{}, unmarked{}, kind(""), anonymous, local, mutable{}, fieldAudited{}, aliasOfClean{}}
