package libelpspath

import (
	"context"
	"fmt"
	"github.com/luthersystems/elps/lisp"
	"os"
	"os/exec"
	"runtime/debug"
	"strconv"
	"strings"
	"testing"
	"time"
)

func TestPathValueDepth(t *testing.T) {
	if size := os.Getenv("ELPS_PATH_DEPTH"); size != "" {
		debug.SetMaxStack(8 << 20)
		depth, _ := strconv.Atoi(size)
		nodes := make([]lisp.LVal, depth+1)
		cells := make([]*lisp.LVal, depth)
		nodes[depth].Type = lisp.LInt
		nodes[depth].Int = 7
		for i := range depth {
			cells[i] = &nodes[i+1]
			nodes[i].Type = lisp.LSExpr
			nodes[i].Cells = cells[i : i+1 : i+1]
		}
		cp, err := copyLVal(&nodes[0])
		if depth >= lisp.MaxValueDepth {
			if err == nil || !strings.Contains(err.Error(), "value nesting depth exceeds maximum") {
				t.Fatalf("expected ordinary depth error: %v", err)
			}
		} else {
			if err != nil {
				t.Fatal(err)
			}
			for range depth {
				if len(cp.Cells) != 1 {
					t.Fatal("lost container")
				}
				cp = cp.Cells[0]
			}
			if cp.Int != 7 {
				t.Fatal("lost leaf")
			}
		}
		return
	}
	for _, depth := range []int{100_000, 3_000_000} {
		t.Run(fmt.Sprint(depth), func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), 45*time.Second)
			defer cancel()
			cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestPathValueDepth$", "-test.count=1") //nolint:gosec // this test binary with a fixed selector
			cmd.Env = append(os.Environ(), "ELPS_PATH_DEPTH="+fmt.Sprint(depth))
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("depth child: %v\n%s", err, out)
			}
		})
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
