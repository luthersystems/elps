package libelpspath

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// The copy walk used to read lisp.MaxValueDepth directly, so a runtime
// configured with WithMaxValueDepth got the default here. 2048 is above the
// 1024 floor that option accepts; 3000 is over it and 1500 is under it.
const (
	runtimeDepthLimit = 2048
	overRuntimeLimit  = 3000
	underRuntimeLimit = 1500
)

func deepList(depth int) *lisp.LVal {
	nodes := make([]lisp.LVal, depth+1)
	cells := make([]*lisp.LVal, depth)
	nodes[depth].Type = lisp.LInt
	nodes[depth].Int = 7
	for i := range depth {
		cells[i] = &nodes[i+1]
		nodes[i].Type = lisp.LSExpr
		nodes[i].Cells = cells[i : i+1 : i+1]
	}
	return &nodes[0]
}

func TestCopyHonoursRuntimeDepthLimit(t *testing.T) {
	t.Parallel()

	if _, err := copyLVal(deepList(overRuntimeLimit), runtimeDepthLimit); err == nil ||
		!strings.Contains(err.Error(), "maximum: 2048") {
		t.Fatalf("copy accepted a %d-deep value under a %d limit: %v", overRuntimeLimit, runtimeDepthLimit, err)
	}
	if _, err := copyLVal(deepList(underRuntimeLimit), runtimeDepthLimit); err != nil {
		t.Fatalf("copy rejected a %d-deep value under a %d limit: %v", underRuntimeLimit, runtimeDepthLimit, err)
	}
	// The limit-free spelling keeps the default.
	if _, err := copyLVal(deepList(overRuntimeLimit), 0); err != nil {
		t.Fatalf("copy at the default limit rejected a %d-deep value: %v", overRuntimeLimit, err)
	}
}

// The builtins are where the runtime is in reach, so they are what carries the
// configured limit into the walk, through setPath/deletePath/nilPath.
func TestQueryBuiltinHonoursRuntimeDepthLimit(t *testing.T) {
	t.Parallel()

	set := func(depth int) *lisp.LVal {
		env := lisp.NewEnv(nil)
		if rc := lisp.WithMaxValueDepth(runtimeDepthLimit)(env); rc.Type == lisp.LError {
			t.Fatalf("configuring the limit failed: %s", rc)
		}
		// The deep value is OFF the path, so the copy has to walk it.
		doc := lisp.QExpr([]*lisp.LVal{deepList(depth), lisp.Int(1)})
		return BuiltinQuerySet(env, lisp.SExpr([]*lisp.LVal{doc, lisp.Int(1), lisp.Int(2)}))
	}

	got := set(overRuntimeLimit)
	if got.Type != lisp.LError || !strings.Contains(got.String(), "maximum: 2048") {
		t.Fatalf("?set accepted a %d-deep value under a %d limit: %s", overRuntimeLimit, runtimeDepthLimit, got)
	}
	if got := set(underRuntimeLimit); got.Type == lisp.LError {
		t.Fatalf("?set rejected a %d-deep value under a %d limit: %s", underRuntimeLimit, runtimeDepthLimit, got)
	}
}

// The limit travels as an argument now, so every step that recurses has to
// pass it on: a chain into a nested document, and an iterator running a chain
// per element, are the two that do.
func TestNestedStepsCarryRuntimeDepthLimit(t *testing.T) {
	t.Parallel()

	run := func(t *testing.T, depth int, steps ...*lisp.LVal) *lisp.LVal {
		t.Helper()
		env := lisp.NewEnv(nil)
		if rc := lisp.WithMaxValueDepth(runtimeDepthLimit)(env); rc.Type == lisp.LError {
			t.Fatalf("configuring the limit failed: %s", rc)
		}
		// Each element is (deep 1); the write lands on index 1 and the deep
		// value at index 0 is the off-path copy.
		element := func() *lisp.LVal {
			return lisp.QExpr([]*lisp.LVal{deepList(depth), lisp.Int(1)})
		}
		doc := lisp.QExpr([]*lisp.LVal{element(), element()})
		args := append([]*lisp.LVal{doc}, steps...)
		args = append(args, lisp.Int(2))
		return BuiltinQuerySet(env, lisp.SExpr(args))
	}

	for _, tc := range []struct {
		name  string
		steps []*lisp.LVal
	}{
		{"chain", []*lisp.LVal{lisp.Int(0), lisp.Int(1)}},
		{"iterator", []*lisp.LVal{lisp.Symbol("*"), lisp.Int(1)}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := run(t, overRuntimeLimit, tc.steps...)
			if got.Type != lisp.LError || !strings.Contains(got.String(), "maximum: 2048") {
				t.Fatalf("accepted a %d-deep value under a %d limit: %s", overRuntimeLimit, runtimeDepthLimit, got)
			}
			if got := run(t, underRuntimeLimit, tc.steps...); got.Type == lisp.LError {
				t.Fatalf("rejected a %d-deep value under a %d limit: %s", underRuntimeLimit, runtimeDepthLimit, got)
			}
		})
	}
}
