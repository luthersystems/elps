package libelpspath

import (
	"github.com/luthersystems/elps/lisp"
	"testing"
)

func TestPathValueDepth(t *testing.T) {
	v := lisp.Int(7)
	for range 2000 {
		v = lisp.SExpr([]*lisp.LVal{v})
	}
	if _, err := copyLVal(v); err == nil {
		t.Fatal("expected ordinary depth error")
	}
}

func TestPathTraversalDepth(t *testing.T) {
	steps := make([]*lisp.LVal, 2000)
	paths := make([]Path, len(steps))
	v := lisp.Int(7)
	for i := range steps {
		steps[i] = lisp.Symbol("*")
		paths[i] = Index(0)
		v = lisp.SExpr([]*lisp.LVal{v})
	}
	if _, err := ArgsToPath(steps); err == nil {
		t.Error("expected path depth error")
	}
	for _, call := range []func() (*lisp.LVal, error){
		func() (*lisp.LVal, error) { return setChain(v, lisp.Int(9), paths) },
		func() (*lisp.LVal, error) { return deleteChain(v, paths) },
		func() (*lisp.LVal, error) { return nullChain(v, paths) },
	} {
		if _, err := call(); err == nil {
			t.Error("expected traversal depth error")
		}
	}
}
