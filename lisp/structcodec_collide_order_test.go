// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// When two keys collide and a value cannot be encoded, the error is that of
// the collision in every run (luthersystems/elps#934).  Equal keys sort in
// walk order, so a value error found first would depend on the walk.
func TestEncodeMapCollisionComesBeforeValueErrors(t *testing.T) {
	type S string
	for range 200 {
		_, err := lisp.Encode(map[any]any{"a": make(chan int), S("a"): 1})
		if err == nil {
			t.Fatal("no error")
		}
		if want := `two keys convert to the map key`; !strings.Contains(err.Error(), want) {
			t.Fatalf("error %q does not contain %q", err, want)
		}
	}
}
