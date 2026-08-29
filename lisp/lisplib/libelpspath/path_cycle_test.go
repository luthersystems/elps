// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"runtime/debug"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/luthersystems/elps/lisp"
)

// cyclicVector returns a vector that contains itself, the value issue #393's
// reproduction builds with (append! v v).
func cyclicVector() *lisp.LVal {
	v := lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})
	v.Cells[1].Cells = append(v.Cells[1].Cells, v)
	return v
}

// cyclicMap returns a sorted-map that holds itself under each of keys, the
// shape that makes an unrolling walk exponential rather than merely deep.
func cyclicMap(keys ...string) *lisp.LVal {
	m := lisp.SortedMap()
	for _, k := range keys {
		m.MapSet(k, m)
	}
	return m
}

// cyclicList returns a list that contains itself.
func cyclicList() *lisp.LVal {
	l := lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
	l.Cells = append(l.Cells, l)
	return l
}

// TestTypeCheckRefusesCyclicValue pins the gate every builtin runs. A cyclic
// value has no finite path result, so OKSimpleType must refuse it with an
// ordinary error the builtins turn into a catchable condition — not recurse
// between OKSimpleType and okSimpleContainerType until the stack overflows.
// See issue #393.
func TestTypeCheckRefusesCyclicValue(t *testing.T) {
	for name, v := range map[string]*lisp.LVal{
		"vector":    cyclicVector(),
		"list":      cyclicList(),
		"map":       cyclicMap("k"),
		"map-wide":  cyclicMap("a", "b"),
		"nested":    lisp.Vector([]*lisp.LVal{cyclicMap("k")}),
		"map-value": mapHolding("outer", cyclicVector()),
	} {
		t.Run(name, func(t *testing.T) {
			err := OKSimpleType(v)
			require.Error(t, err, "a cyclic value must be refused")
			assert.ErrorIs(t, err, errCyclicValue)
		})
	}
}

// TestCopyRefusesCyclicValue pins the same answer from the copy walk. The
// copy is guarded separately from the gate because the exported Path
// interface lets a Go embedder reach a copy without coming through the gate.
func TestCopyRefusesCyclicValue(t *testing.T) {
	for name, v := range map[string]*lisp.LVal{
		"vector":   cyclicVector(),
		"list":     cyclicList(),
		"map":      cyclicMap("k"),
		"map-wide": cyclicMap("a", "b"),
		"nested":   lisp.Vector([]*lisp.LVal{cyclicMap("k")}),
	} {
		t.Run(name, func(t *testing.T) {
			_, err := copyLVal(v)
			require.Error(t, err, "a cyclic value has no finite copy")
			assert.ErrorIs(t, err, errCyclicValue)
		})
	}
}

// TestAcyclicValuesAreUnaffectedByTheGuard pins the property the guard exists
// to preserve: it is invisible to any value that is not cyclic, at any depth.
// Both cases nest past the guard depth, where the path set is live, and a DAG
// — a value that merely shares a substructure with itself — must still be
// accepted and copied in full rather than read as a cycle.
func TestAcyclicValuesAreUnaffectedByTheGuard(t *testing.T) {
	deep := lisp.SortedMap()
	inner := deep
	for range 200 {
		next := lisp.SortedMap()
		inner.MapSet("k", next)
		inner = next
	}
	inner.MapSet("k", lisp.String("leaf"))

	shared := lisp.SortedMap()
	shared.MapSet("k", lisp.String("v"))
	dag := lisp.SortedMap()
	dag.MapSet("a", shared)
	dag.MapSet("b", shared)

	for name, v := range map[string]*lisp.LVal{"deep": deep, "dag": dag, "deep-dag": mapHolding("d", dag)} {
		t.Run(name, func(t *testing.T) {
			require.NoError(t, OKSimpleType(v), "an acyclic value must be accepted at any depth")
			cp, err := copyLVal(v)
			require.NoError(t, err)
			assert.Equal(t, v.String(), cp.String(), "an acyclic value must be copied in full")
		})
	}
}

func mapHolding(key string, v *lisp.LVal) *lisp.LVal {
	m := lisp.SortedMap()
	m.MapSet(key, v)
	return m
}

// cyclicWalkEnv names the walk a helper subprocess should run. It is read,
// never written, by anything that ships.
const cyclicWalkEnv = "ELPS_TEST_ELPSPATH_CYCLIC_WALK"

// TestCyclicValueDoesNotKillTheProcess is the assertion issue #393 actually
// needs, and it cannot be made in-process. An unbounded walk over a cyclic
// value does not panic — it grows the goroutine stack past the runtime's
// limit, and a stack overflow is a fatal runtime error that recover() cannot
// catch, handler-bind never sees, and no in-process assertion would survive
// to report. So each walk runs in a helper subprocess and the assertion is on
// that process's exit status.
//
// Red-proof: with either guard reverted, the corresponding subtest fails with
// the helper's "fatal error: stack overflow" in the captured output rather
// than passing.
func TestCyclicValueDoesNotKillTheProcess(t *testing.T) {
	for _, name := range []string{"typecheck", "copy", "builtin-get", "builtin-set-copy"} {
		t.Run(name, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), time.Minute)
			defer cancel()
			//nolint:gosec // os.Args[0] is this test binary
			cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=TestCyclicWalkHelper", "-test.v")
			cmd.Env = append(os.Environ(), cyclicWalkEnv+"="+name)
			out, err := cmd.CombinedOutput()
			require.NoError(t, err, "the %s walk must not kill the process:\n%s", name, out)
			assert.Contains(t, string(out), "RESULT: refused",
				"the %s walk must refuse the cyclic value:\n%s", name, out)
		})
	}
}

// TestCyclicWalkHelper is the subprocess half of
// TestCyclicValueDoesNotKillTheProcess. It is inert unless armed.
func TestCyclicWalkHelper(t *testing.T) {
	walk := os.Getenv(cyclicWalkEnv)
	if walk == "" {
		t.Skip("set " + cyclicWalkEnv + " to run one cyclic walk in this process")
	}
	// An unguarded walk dies at the default 1GB stack limit, which costs a
	// second and a gigabyte before the run fails. Failing at 16MB proves the
	// same thing far more cheaply, and no guarded walk comes near it.
	debug.SetMaxStack(16 << 20)

	var refused bool
	switch walk {
	case "typecheck":
		refused = OKSimpleType(cyclicVector()) != nil
	case "copy":
		_, err := copyLVal(cyclicMap("a", "b"))
		refused = err != nil
	case "builtin-get":
		// The reproduction from issue #393, driven through the builtin:
		// (set 'v (vector 1 2)) (append! v v) (elpspath:? v 0)
		env := testEnv(t)
		lerr := BuiltinQueryGet(env, lisp.QExpr([]*lisp.LVal{cyclicVector(), lisp.Int(0)}))
		refused = lerr.Type == lisp.LError
	case "builtin-set-copy":
		env := testEnv(t)
		args := lisp.QExpr([]*lisp.LVal{cyclicMap("a", "b"), lisp.String("k"), lisp.String("v")})
		lerr := BuiltinQuerySet(env, args)
		refused = lerr.Type == lisp.LError
	default:
		t.Fatalf("unknown walk: %s", walk)
	}
	if refused {
		fmt.Println("RESULT: refused")
	}
}

func testEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = nil
	return env
}
