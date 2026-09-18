// Copyright © 2026 The ELPS authors

package debugger

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/require"
)

func TestFormatValueGraphDeadline(t *testing.T) {
	for _, mode := range []string{"dag", "cycle"} {
		t.Run(mode, func(t *testing.T) {
			if os.Getenv("ELPS_TEST_DEBUG_RENDER") != mode {
				// The deadline exists to turn a runaway render or a fatal
				// stack overflow in the child into a test failure, so it
				// only needs to be far below the package timeout. It also
				// covers the child's process start-up and teardown, which
				// under -race on a loaded CI runner has exceeded ten
				// seconds AFTER the child printed PASS; a minute keeps the
				// guard and stops that from reading as a failure.
				ctx, cancel := context.WithTimeout(t.Context(), time.Minute)
				defer cancel()
				//nolint:gosec // Isolate runaway rendering and fatal stack overflow.
				cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestFormatValueGraphDeadline$/^"+mode+"$")
				cmd.Env = append(os.Environ(), "ELPS_TEST_DEBUG_RENDER="+mode)
				out, err := cmd.CombinedOutput()
				t.Logf("child output bytes=%d: %s", len(out), out[:min(len(out), 500)])
				require.NoError(t, err, "deadline: %v", ctx.Err())
				return
			}
			v := lisp.Int(1)
			if mode == "dag" {
				for range 40 {
					v = lisp.SExpr([]*lisp.LVal{v, v})
				}
			} else {
				v = lisp.SExpr([]*lisp.LVal{nil})
				v.Cells[0] = v
			}
			for _, eng := range []*Engine{nil, New()} {
				s := FormatValueWith(v, eng)
				fmt.Printf("%s rendered bytes=%d\n", mode, len(s))
				require.LessOrEqual(t, len(s), lisp.DefaultMaxAlloc)
				if mode == "dag" {
					require.Contains(t, s, "#<truncated>")
				} else {
					require.True(t, strings.Contains(s, "cycle") || strings.Contains(s, "truncated"), s)
				}
			}
		})
	}
}

// One response budget covers every value it formats. The limit is above the
// floor the runtime applies to diagnostic text (MaxAlloc caps program data,
// not the text describing it), so this measures the sharing, not the floor.
func TestValueFormatterResponseBudget(t *testing.T) {
	const limit = 128 << 10
	env := lisp.NewEnv(nil)
	env.Runtime.MaxAlloc = limit
	eng := New()
	eng.pausedEnv = env
	f := NewValueFormatter(nil, eng)
	var out strings.Builder
	for range 100 {
		out.WriteString(f.Format(lisp.String(strings.Repeat("x", 2000))))
	}
	require.LessOrEqual(t, out.Len(), limit)
	require.Contains(t, out.String(), "#<truncated>")
	require.True(t, f.Exhausted())
}

type cancelFormatContext struct {
	context.Context
	checks int
}

func (c *cancelFormatContext) Err() error {
	c.checks++
	if c.checks > 100 {
		return context.Canceled
	}
	return nil
}

func TestFormatValueWithCancellation(t *testing.T) {
	env := lisp.NewEnv(nil)
	env.Runtime.MaxAlloc = 1 << 30
	eng := New()
	eng.pausedEnv = env
	v := lisp.Int(1)
	for range 40 {
		v = lisp.SExpr([]*lisp.LVal{v, v})
	}
	ctx := &cancelFormatContext{Context: t.Context()}
	var text string
	fn := lisp.Fun("inspect", lisp.Formals(), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		text = FormatValueWith(v, eng)
		return lisp.Nil()
	})
	env.EvalContext(ctx, lisp.SExpr([]*lisp.LVal{fn}))
	require.Equal(t, "#<truncated>", text)
	require.Greater(t, ctx.checks, 100)
	require.Less(t, ctx.checks, 200)
}
