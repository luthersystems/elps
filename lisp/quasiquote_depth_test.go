// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"os"
	"os/exec"
	"runtime/debug"
	"strconv"
	"strings"
	"testing"
	"time"
)

func TestQuasiquoteMixedDepth(t *testing.T) {
	if setting := os.Getenv("ELPS_QUASIQUOTE_LIMIT"); setting != "" {
		debug.SetMaxStack(8 << 20)
		limit, err := strconv.Atoi(setting)
		if err != nil {
			t.Fatal(err)
		}
		env := initSafetyTestEnv(t)
		if limit != MaxValueDepth {
			if got := WithMaxValueDepth(limit)(env); got.Type == LError {
				t.Fatal(got)
			}
		}
		for _, edges := range []int{limit - 1, limit, limit + 1, limit * 75 / 64} {
			t.Run(strconv.Itoa(edges), func(t *testing.T) {
				// Alternate real quote-wrapper and list edges, including a
				// quote-wrapped leaf at the odd boundaries.
				v := depthTestValue(edges)
				for node, i := v, 0; i < edges; node, i = node.Cells[0], i+1 {
					if i%2 == 0 {
						node.Type = LQuote
					}
				}
				got := findAndUnquote(env, v, 0)
				if edges >= limit {
					if got.Type != LError || IsInternalPanic(got) || !strings.Contains(got.String(), "value nesting depth exceeds maximum: "+setting) {
						t.Fatalf("%d value edges: expected ordinary depth error at %d, got %s", edges, limit, got.Type)
					}
					return
				}
				for range edges / 2 {
					if got.Type != LSExpr || !got.quoted || len(got.Cells) != 1 {
						t.Fatal("lost quoted list below the depth limit")
					}
					got = got.Cells[0]
				}
				if got.Type != LQuote || len(got.Cells) != 1 || got.Cells[0].Int != 7 {
					t.Fatal("lost quote-wrapped leaf below the depth limit")
				}
			})
		}
		return
	}
	for _, limit := range []int{1024, MaxValueDepth} {
		t.Run(strconv.Itoa(limit), func(t *testing.T) {
			ctx, cancel := context.WithTimeout(t.Context(), 45*time.Second)
			defer cancel()
			cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestQuasiquoteMixedDepth$", "-test.count=1") //nolint:gosec // executes this test binary with a fixed selector
			cmd.Env = append(os.Environ(), "ELPS_QUASIQUOTE_LIMIT="+strconv.Itoa(limit))
			if out, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("mixed-depth child: %v\n%s", err, out)
			}
		})
	}
}
