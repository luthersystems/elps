package libjson_test

import (
	"reflect"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/internal/jsonraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
)

func TestMapImpl(t *testing.T) {
	m := jsonraw.Wrap(map[string]any{}).Map()
	m.Set(lisp.String("a"), lisp.Int(1))
	m.Set(lisp.Symbol("b"), lisp.Int(2))
	m.Set(lisp.String("c"), lisp.Int(3))
	elpstest.AssertSortedMap(t, m)
}

// TestBatchStringMatchesConstructor guards the decoder map's batched keys
// against drift from the ordinary lisp.String constructor.
//
// Entries batches its key LVals -- `lisp.LVal{Type: lisp.LString, Str: k}`
// carved out of one array rather than n calls to lisp.String -- because the
// per-key allocation was 13% of every object the libjson benchmark suite
// allocated (issue #379, item 6). If lisp.String initializes another field,
// the batching code must initialize it too.
//
// reflect.DeepEqual reads unexported fields, so this compares the WHOLE
// struct, not the part libjson can see.
//
// Red-proof: adding any initialised field to lisp.String's literal fails this.
func TestBatchStringMatchesConstructor(t *testing.T) {
	for _, s := range []string{"", "a", "a key with spaces", "\x00 <&>"} {
		value := jsonraw.Wrap(map[string]any{s: lisp.Int(1)})
		batched := value.MapEntries().Cells[0].Cells[0]
		assert.True(t, reflect.DeepEqual(batched, lisp.String(s)),
			"a batched LString for %q is no longer what lisp.String builds: "+
				"%#v vs %#v", s, batched, lisp.String(s))
	}
}
