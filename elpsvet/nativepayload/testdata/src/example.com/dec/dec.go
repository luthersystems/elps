// Package dec stands in for an immutable decimal type of another module.
package dec

type Decimal struct {
	value *int
	exp   int32
}
