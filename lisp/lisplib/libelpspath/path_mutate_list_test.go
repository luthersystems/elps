package libelpspath

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// fpAST deep-fingerprints an LVal graph: type, scalar fields, quote flag,
// and cells recursively. Any in-place write to a parsed AST changes the
// digest. This mirrors the parse-cache audit harness in
// internal/substrate/elpsutil.
func fpAST(vs []*lisp.LVal) string {
	h := sha256.New()
	seen := map[*lisp.LVal]int{}
	var walk func(v *lisp.LVal, d int)
	walk = func(v *lisp.LVal, d int) {
		if v == nil || d > 64 {
			_, _ = fmt.Fprint(h, "<nil>")
			return
		}
		if id, ok := seen[v]; ok {
			_, _ = fmt.Fprintf(h, "cyc:%d;", id)
			return
		}
		seen[v] = len(seen)
		_, _ = fmt.Fprintf(h, "t=%d q=%v s=%q i=%d f=%g", v.Type, v.IsQuoted(), v.Str, v.Int, v.Float)
		_, _ = fmt.Fprintf(h, " n=%d{", len(v.Cells))
		for _, c := range v.Cells {
			walk(c, d+1)
		}
		_, _ = fmt.Fprint(h, "}")
	}
	for _, v := range vs {
		walk(v, 0)
	}
	return hex.EncodeToString(h.Sum(nil))
}

// newMutateTestEnv returns a fresh env with the elpspath package loaded and
// the user package active, analogous to a warm phylum env.
func newMutateTestEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if lerr := lisp.InitializeUserEnv(env); lerr.Type == lisp.LError {
		t.Fatalf("InitializeUserEnv: %v", lerr)
	}
	if lerr := LoadPackage(env); lerr.Type == lisp.LError {
		t.Fatalf("LoadPackage: %v", lerr)
	}
	if lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage)); lerr.Type == lisp.LError {
		t.Fatalf("InPackage: %v", lerr)
	}
	return env
}

// parseOnce parses src exactly once. The returned exprs are shared between
// evaluating environments, modelling the shiro parse cache handing one AST
// to every warm env.
func parseOnce(t *testing.T, src string) []*lisp.LVal {
	t.Helper()
	exprs, err := parser.NewReader().Read("test.lisp", strings.NewReader(src))
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	return exprs
}

// evalAll evaluates exprs in order, recovering any Go panic so the test can
// still inspect the (possibly corrupted) AST afterwards. On the unfixed
// tree the list ! ops panic inside the LArray dims bookkeeping AFTER the
// backing cells have already been shifted.
func evalAll(env *lisp.LEnv, exprs []*lisp.LVal) (results []*lisp.LVal, panicked interface{}) {
	defer func() { panicked = recover() }()
	for _, e := range exprs {
		results = append(results, env.EvalContext(context.Background(), e))
	}
	return results, nil
}

func assertList102030(t *testing.T, v *lisp.LVal) {
	t.Helper()
	if v == nil || v.Type != lisp.LSExpr {
		t.Fatalf("expected list, got %v", v)
	}
	if len(v.Cells) != 3 {
		t.Fatalf("expected 3 elements, got %d: %v", len(v.Cells), v)
	}
	for i, want := range []int{10, 20, 30} {
		if c := v.Cells[i]; c.Type != lisp.LInt || c.Int != want {
			t.Fatalf("element %d: want %d, got %v (full: %v)", i, want, c, v)
		}
	}
}

// TestMutateListRejected is the regression test for the elpspath list
// corruption bug (substrate#378): the in-place (!) path ops obtained a
// list's live cell backing via toCells and shifted it in place before
// panicking on the LArray dims layout. When the list is a quoted program
// literal shared through the parse cache, the corruption is visible to
// every other environment evaluating the same AST.
//
// The shapes cover every mutating entry point reachable from the ?-family
// builtins: index and range steps for ?set!/?del!/?nil! (dotPath mutators
// only touch sorted-maps and cannot alias list cells).
//
// For each mutating op shape the same source is parsed ONCE and the same
// []*lisp.LVal is evaluated in two fresh envs, asserting:
//
//	(a) env1 observes a catchable lisp error (handler-bind => 'caught),
//	(b) the AST fingerprint is unchanged after env1's evaluation,
//	(c) env2 still sees the pristine literal '(10 20 30).
func TestMutateListRejected(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name string
		op   string // second toplevel form, wrapped in handler-bind
	}{
		{"?del!", `(elpspath:?del! q 1)`},
		{"?set!", `(elpspath:?set! q 1 99)`},
		{"?nil!", `(elpspath:?nil! q 1)`},
		{"?del!-range", `(elpspath:?del! q '(range 0 2))`},
		{"?set!-range", `(elpspath:?set! q '(range 0 2) (vector 7))`},
		{"?nil!-range", `(elpspath:?nil! q '(range 0 2))`},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := `(set 'q '(10 20 30))` + "\n" +
				`(handler-bind ((condition (lambda (&rest _) 'caught))) ` + tc.op + `)` + "\n" +
				`q`
			exprs := parseOnce(t, src)
			pristine := fpAST(exprs)

			env1 := newMutateTestEnv(t)
			results1, panicked := evalAll(env1, exprs)
			if panicked != nil {
				t.Errorf("(a) env1 panicked instead of raising a catchable error: %v", panicked)
			} else if got := results1[1]; got.Type != lisp.LSymbol || got.Str != "caught" {
				t.Errorf("(a) env1: handler-bind did not catch an error; got %v", got)
			}

			if got := fpAST(exprs); got != pristine {
				t.Errorf("(b) AST literal fingerprint CHANGED after env1 evaluation — shared-AST corruption")
			}

			if elpscheckEnabled {
				// Evaluating the same AST in a second runtime is exactly
				// the cross-runtime sharing the elpscheck ownership
				// checker forbids (panic: "LVal used by two Runtimes").
				// The modeled deployment — a warm parse cache handing one
				// AST to many environments — predates that invariant, so
				// assertion (c) only runs in the default build.
				t.Log("skipping (c): elpscheck ownership checker forbids cross-runtime AST sharing")
				return
			}

			env2 := newMutateTestEnv(t)
			results2, panicked2 := evalAll(env2, exprs)
			if panicked2 != nil {
				t.Fatalf("(c) env2 panicked: %v", panicked2)
			}
			assertList102030(t, results2[2])
		})
	}
}

// TestMutateArrayMapInPlace pins the documented in-place semantics of the !
// ops for arrays and sorted-maps: the guard added for lists must not change
// them. Each script mutates through the original binding and then reads the
// binding back, so a copy-on-write regression would fail here.
func TestMutateArrayMapInPlace(t *testing.T) {
	t.Parallel()
	for _, tc := range []struct {
		name     string
		src      string
		expected string // expression evaluated in a fresh env for comparison
	}{
		{
			name:     "vector ?del!",
			src:      `(set 'v (vector 10 20 30)) (elpspath:?del! v 1) v`,
			expected: `(vector 10 30)`,
		},
		{
			name:     "vector ?set!",
			src:      `(set 'v (vector 10 20 30)) (elpspath:?set! v 1 99) v`,
			expected: `(vector 10 99 30)`,
		},
		{
			name:     "vector ?nil!",
			src:      `(set 'v (vector 10 20 30)) (elpspath:?nil! v 1) v`,
			expected: `(vector 10 () 30)`,
		},
		{
			name:     "vector range ?del!",
			src:      `(set 'v (vector 10 20 30)) (elpspath:?del! v '(range 0 2)) v`,
			expected: `(vector 30)`,
		},
		{
			name:     "vector range ?set!",
			src:      `(set 'v (vector 10 20 30)) (elpspath:?set! v '(range 0 2) (vector 7)) v`,
			expected: `(vector 7 30)`,
		},
		{
			name:     "map ?set!",
			src:      `(set 'm (sorted-map "a" 1)) (elpspath:?set! m "a" 2) m`,
			expected: `(sorted-map "a" 2)`,
		},
		{
			name:     "map ?del!",
			src:      `(set 'm (sorted-map "a" 1 "b" 2)) (elpspath:?del! m "a") m`,
			expected: `(sorted-map "b" 2)`,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			env := newMutateTestEnv(t)
			got := env.LoadStringContext(context.Background(), "test.lisp", tc.src)
			if got.Type == lisp.LError {
				t.Fatalf("eval: %v", got)
			}
			want := env.LoadStringContext(context.Background(), "expected.lisp", tc.expected)
			if want.Type == lisp.LError {
				t.Fatalf("eval expected: %v", want)
			}
			if got.String() != want.String() {
				t.Fatalf("in-place semantics changed: got %v, want %v", got, want)
			}
		})
	}
}
