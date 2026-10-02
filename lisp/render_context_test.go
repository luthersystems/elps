// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"context"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

// Arm only after evaluation; cancellation then occurs inside rendering.
type traversalContext struct {
	context.Context
	cancel context.CancelFunc
	checks int
	armed  bool
}

func (c *traversalContext) Err() error {
	if c.armed {
		c.checks++
		if c.checks == 100 {
			c.cancel()
		}
	}
	return c.Context.Err()
}

func TestRenderContextAfterLoadDeadline(t *testing.T) {
	for _, mode := range []string{"cancelled", "during-traversal"} {
		t.Run(mode, func(t *testing.T) {
			if os.Getenv("ELPS_TEST_RENDER_CONTEXT") != mode {
				// The child's bound is CPU time, not wall time (#789):
				// process start and load under -race on a starved runner
				// took more than a 5s wall-clock deadline.  The child uses
				// well under a second of CPU.
				//nolint:gosec // Re-execute the regression under an external bound.
				cmd := exec.CommandContext(t.Context(), os.Args[0], "-test.run=^TestRenderContextAfterLoadDeadline$/^"+mode+"$")
				cmd.Env = append(os.Environ(), "ELPS_TEST_RENDER_CONTEXT="+mode)
				out, err := testdeadline.RunChild(cmd, 20*time.Second)
				if err != nil {
					t.Fatalf("render failed: %v\n%s", err, out)
				}
				return
			}
			env := lisp.NewEnv(nil)
			if err := lisp.GoError(lisp.InitializeUserEnv(env, lisp.WithReader(parser.NewReader()), lisp.WithMaxAlloc(1<<30))); err != nil {
				t.Fatal(err)
			}
			base, cancel := context.WithCancel(t.Context())
			defer cancel()
			ctx := &traversalContext{Context: base, cancel: cancel}
			v := env.LoadStringContext(ctx, "render", `(set 'x '(1)) (dotimes (i 18) (set 'x (list x x))) x`)
			if v.Type == lisp.LError {
				t.Fatal("evaluation failed")
			}
			if mode == "cancelled" {
				cancel()
			} else {
				ctx.armed = true
			}
			var s string
			if cpu, ok := testdeadline.Within(time.Second, func() { s = env.RenderContext(ctx, v) }); !ok {
				t.Fatalf("rendering did not stop promptly: used %v of CPU time", cpu)
			}
			if !strings.Contains(s, "#<truncated>") {
				t.Fatalf("post-evaluation rendering ignored cancellation (%d output bytes)", len(s))
			}
			if mode == "during-traversal" && (ctx.checks < 100 || ctx.checks > 200) {
				t.Fatalf("unexpected cancellation checks: %d", ctx.checks)
			}
		})
	}
}
