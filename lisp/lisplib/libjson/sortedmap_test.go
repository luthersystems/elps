package libjson_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
)

func TestMapImpl(t *testing.T) {
	m := libjson.SortedMap{}
	m.Set(lisp.String("a"), lisp.Int(1))
	m.Set(lisp.Symbol("b"), lisp.Int(2))
	m.Set(lisp.String("c"), lisp.Int(3))
	elpstest.AssertSortedMap(t, m)
}

// TestBatchStringMatchesConstructor is the guard on the one assumption
// SortedMap.Entries makes about a type it does not own.
//
// Entries batches its key LVals -- `lisp.LVal{Type: lisp.LString, Str: k}`
// carved out of one array rather than n calls to lisp.String -- because the
// per-key allocation was 13% of every object the libjson benchmark suite
// allocated (issue #379, item 6).  That literal is a copy of lisp.String's
// body written in another package, so it is correct only for as long as
// lisp.String stays a two-field struct literal.  If a future LString ever
// needs a third field -- an unexported one this package could not set even if
// it knew -- the literal would silently produce a subtly different value.
//
// reflect.DeepEqual reads unexported fields, so this compares the WHOLE
// struct, not the part libjson can see.
//
// Red-proof: adding any initialised field to lisp.String's literal fails this.
func TestBatchStringMatchesConstructor(t *testing.T) {
	for _, s := range []string{"", "a", "a key with spaces", "\x00 <&>"} {
		batched := lisp.LVal{Type: lisp.LString, Str: s}
		assert.True(t, reflect.DeepEqual(&batched, lisp.String(s)),
			"a batched LString for %q is no longer what lisp.String builds: "+
				"%#v vs %#v", s, &batched, lisp.String(s))
	}
}
