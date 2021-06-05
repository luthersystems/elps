package libjson_test

import (
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

func TestMapImpl(t *testing.T) {
	m := libjson.SortedMap{}
	m.Set(lisp.String("a"), lisp.Int(1))
	m.Set(lisp.Symbol("b"), lisp.Int(2))
	m.Set(lisp.String("c"), lisp.Int(3))
	elpstest.AssertSortedMap(t, m)
}
