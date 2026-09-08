// Copyright © 2026 The ELPS authors

package lisplib_test

import (
	"context"
	"math"
	"os"
	"os/exec"
	"runtime"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
)

// TestTemplateForkEscapedLeafDoesNotRetainVM protects the ordinary Go lifetime
// of a small result. It runs in an isolated subprocess so unrelated parallel
// tests cannot inflate the process-wide heap measurements (issue #622).
func TestTemplateForkEscapedLeafDoesNotRetainVM(t *testing.T) {
	const childFlag = "ELPS_TEMPLATE_RETENTION_CHILD"
	if os.Getenv(childFlag) != "1" {
		executable, err := os.Executable()
		if err != nil {
			t.Fatal(err)
		}
		ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
		defer cancel()
		// #nosec G204 -- os.Executable returns this test binary; all arguments are fixed, not user-controlled.
		command := exec.CommandContext(ctx, executable, "-test.run=^TestTemplateForkEscapedLeafDoesNotRetainVM$", "-test.v")
		command.Env = append(os.Environ(), childFlag+"=1")
		output, err := command.CombinedOutput()
		if err != nil {
			t.Fatalf("isolated retention test: %v\n%s", err, output)
		}
		t.Logf("%s", output)
		return
	}

	env := newTemplateTestEnv(t)
	rows := make([]*lisp.LVal, 4096)
	for i := range rows {
		row := lisp.SortedMap()
		if rc := row.MapSet("number", lisp.Int(i)); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		data := make([]byte, 256)
		data[0], data[len(data)-1] = byte(i), byte(i>>8)
		if rc := row.MapSet("blob", lisp.Bytes(data)); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		rows[i] = row
	}
	for _, binding := range []struct {
		name  string
		value *lisp.LVal
	}{
		{"bulk", lisp.QExpr(rows)},
		{"escape-scalar", lisp.Int(7)},
		{"escape-bytes", lisp.Bytes([]byte("small-independent"))},
	} {
		if rc := env.PutGlobal(lisp.Symbol(binding.name), binding.value); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
	}
	template := snapshotFixture(t, env)
	for _, shape := range []string{"environment", "scalar", "bytes"} {
		t.Run(shape, func(t *testing.T) {
			// The 4-to-12-VM slope removes fixed template retention and GC
			// bootstrap costs. A scalar or raw byte slice references no bulk data.
			const warm, additional = 4, 8
			keep := make([]any, 0, warm+additional)
			for range warm {
				keep = append(keep, escapedTemplateLeaf(t, template, shape))
			}
			before := templateSettledHeap()
			for range additional {
				keep = append(keep, escapedTemplateLeaf(t, template, shape))
			}
			after := templateSettledHeap()
			perVM := (after - before) / additional
			t.Logf("retained bytes per VM: %d", perVM)
			if shape == "environment" {
				if perVM < 1<<20 {
					t.Fatalf("full-VM positive control retained only %d bytes", perVM)
				}
			} else if perVM > 16<<10 {
				t.Fatalf("independent %s retains unrelated VM data: %d bytes per result", shape, perVM)
			}
			runtime.KeepAlive(keep)
		})
	}
	runtime.KeepAlive(template)
}

//go:noinline
func templateSettledHeap() int64 {
	runtime.GC()
	runtime.GC()
	var stats runtime.MemStats
	runtime.ReadMemStats(&stats)
	if stats.HeapAlloc > math.MaxInt64 {
		panic("heap measurement exceeds the signed-difference range")
	}
	return int64(stats.HeapAlloc)
}

//go:noinline
func escapedTemplateLeaf(t *testing.T, template *lisp.Template, shape string) any {
	t.Helper()
	env := forkTemplateFixture(t, template)
	bulk := env.GetGlobal(lisp.Symbol("bulk"))
	if len(bulk.Cells) != 4096 || bulk.Cells[4095].MapGet("number").Int != 4095 {
		t.Fatal("fork did not construct the full graph")
	}
	switch shape {
	case "environment":
		return env
	case "scalar":
		value := env.GetGlobal(lisp.Symbol("escape-scalar"))
		if value.Type != lisp.LInt || value.Int != 7 {
			t.Fatalf("scalar: got %v want 7", value)
		}
		return value
	case "bytes":
		value := env.GetGlobal(lisp.Symbol("escape-bytes")).Bytes()
		if string(value) != "small-independent" {
			t.Fatalf("byte result: got %q", value)
		}
		return value
	default:
		t.Fatalf("unknown escape shape %q", shape)
		return nil
	}
}
