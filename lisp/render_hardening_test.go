// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/testdeadline"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser"
)

const renderDAG = `(set 'x '(1)) (dotimes (i 40) (set 'x (list x x))) `
const renderRing = `(let ((root (vector)) (tail ()))
 (set! tail root)
 (dotimes (i 1100) (let ((next (vector))) (append! tail next next) (set! tail next)))
 (append! tail root) (debug-print root))`

func TestRenderHardeningDeadline(t *testing.T) {
	for _, mode := range []string{"debug-dag", "error-dag", "string-dag", "branching-ring"} {
		t.Run(mode, func(t *testing.T) {
			if os.Getenv("ELPS_TEST_RENDER_HARDENING") == mode {
				env := lisp.NewEnv(nil)
				env.Runtime.Reader = parser.NewReader()
				if rc := lisp.InitializeUserEnv(env); rc.Type == lisp.LError {
					t.Fatal(rc)
				}
				var out bytes.Buffer
				env.Runtime.Stderr = &out
				src := renderDAG
				switch mode {
				case "debug-dag":
					src += `(debug-print "dag" x)`
				case "error-dag":
					src += `(error 'boom x)`
				case "string-dag":
					src += `x`
				case "branching-ring":
					src = renderRing
				}
				v := env.LoadString("render", src)
				switch mode {
				case "debug-dag":
					if v.Type != lisp.LError || !strings.Contains(v.String(), "allocation size exceeds maximum") {
						t.Fatal("expected ordinary allocation error")
					}
				case "error-dag":
					e := (*lisp.ErrorVal)(v)
					for _, s := range []string{e.Error(), e.ErrorMessage(), v.String()} {
						if len(s) > lisp.DefaultMaxAlloc || !strings.Contains(s, "#<truncated>") {
							t.Fatal("error rendering must truncate")
						}
					}
					out.Reset()
					if _, err := e.WriteTrace(&out); err != nil {
						t.Fatal(err)
					}
					if !strings.Contains(out.String(), "#<truncated>") {
						t.Fatal("trace must truncate condition data")
					}
				case "string-dag":
					s := v.String()
					if len(s) > lisp.DefaultMaxAlloc || !strings.Contains(s, "#<truncated>") {
						t.Fatal("String must truncate")
					}
				case "branching-ring":
					if v.Type == lisp.LError {
						if !strings.Contains(v.String(), "allocation size exceeds maximum") {
							t.Fatal(v)
						}
					} else if out.Len() > lisp.DefaultMaxAlloc || !strings.Contains(out.String(), "#<cycle>") {
						t.Fatal("branching ring must be bounded and detect its cycle")
					}
				}
				return
			}
			// The child's bound is CPU time, not wall time (#789): process
			// start under -race on a starved runner can outlast any
			// wall-clock deadline.  A render that does not stop keeps using
			// CPU, so it still reaches the bound.  A starved runner may not
			// give it that CPU before the binary's -timeout, whose panic
			// skips cleanup and would leave the child running, so Backstop
			// kills it first and the subtest fails by name.
			backstop, stop := testdeadline.Backstop(t)
			defer stop()
			//nolint:gosec // Re-execute this test binary under an external bound.
			cmd := exec.CommandContext(backstop, os.Args[0], "-test.run=^TestRenderHardeningDeadline$/^"+mode+"$")
			cmd.Env = append(os.Environ(), "ELPS_TEST_RENDER_HARDENING="+mode)
			out, err := testdeadline.RunChild(cmd, 20*time.Second)
			if err != nil {
				t.Fatalf("render failed: %v\n%s", err, out)
			}
		})
	}
}

// diagnosticText inspects condition data under an explicit default budget.
// Allocation/cancellation tests assert the original failure, while the render
// policy tests separately assert truncation under the originating policy.
func diagnosticText(v *lisp.LVal) string { return lisp.NewEnv(nil).Render(v) }
