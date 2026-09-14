// Copyright © 2026 The ELPS authors

package cmd

import (
	"bytes"
	"context"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
)

func TestRunPrintDAGDeadline(t *testing.T) {
	if os.Getenv("ELPS_TEST_RUN_DAG") == "1" {
		resetRunFlags(t)
		runExpression, runPrint = true, true
		var out bytes.Buffer
		if err := runElps([]string{`(set 'x '(1)) (dotimes (i 40) (set 'x (list x x))) x`}, &out); err != nil {
			t.Fatal(err)
		}
		if out.Len() > lisp.DefaultMaxAlloc+1 || !strings.Contains(out.String(), "#<truncated>") {
			t.Fatal("top-level output must truncate")
		}
		return
	}
	ctx, cancel := context.WithTimeout(t.Context(), 5*time.Second)
	defer cancel()
	//nolint:gosec // Re-execute this test binary under an external deadline.
	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestRunPrintDAGDeadline$")
	cmd.Env = append(os.Environ(), "ELPS_TEST_RUN_DAG=1")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("top-level rendering failed: %v (deadline: %v)\n%s", err, ctx.Err(), out)
	}
}
