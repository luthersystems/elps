// Copyright © 2026 The ELPS authors

package libjson

import (
	"context"
	"os"
	"os/exec"
	"runtime/debug"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/lisp"
)

func jsonDepthValue(depth int) *lisp.LVal {
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

func TestJSONValueDepth(t *testing.T) {
	if mode := os.Getenv("ELPS_JSON_DEPTH"); mode != "" {
		debug.SetMaxStack(8 << 20)
		v := jsonDepthValue(3_000_000)
		s := DefaultSerializer()
		var err error
		if mode == "GoValue" {
			err, _ = s.GoValue(v, false).(error)
		} else {
			env := lisp.NewEnv(nil)
			got := s.DumpStringBuiltin(env, lisp.SExpr([]*lisp.LVal{v, lisp.Nil()}))
			err = lisp.GoError(got)
			if lisp.IsInternalPanic(got) {
				t.Fatal("internal panic")
			}
		}
		if err == nil || !strings.Contains(err.Error(), "value nesting depth exceeds maximum: 1024") {
			t.Fatalf("expected ordinary depth error, got %v", err)
		}
		return
	}
	for _, mode := range []string{"dump-string", "GoValue"} {
		t.Run(mode, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(context.Background(), 45*time.Second)
			defer cancel()
			cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestJSONValueDepth$", "-test.count=1") //nolint:gosec // executes this test binary, not a command supplied by the program under test
			cmd.Env = append(os.Environ(), "ELPS_JSON_DEPTH="+mode)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("child failed (%v): %.800s", err, out)
			}
		})
	}
}

func TestJSONValueDepthControl(t *testing.T) {
	got, err := Dump(jsonDepthValue(1000), false)
	if err != nil {
		t.Fatal(err)
	}
	want := strings.Repeat("[", 1000) + "7" + strings.Repeat("]", 1000)
	if string(got) != want {
		t.Fatal("dump lost content")
	}
	if _, err := Dump(jsonDepthValue(100_000), false); err == nil {
		t.Fatal("expected depth error")
	}
}

func TestJSONValueDepthConfiguration(t *testing.T) {
	env := lisp.NewEnv(nil)
	lisp.WithMaxValueDepth(8)(env)
	s := DefaultSerializer()
	for _, call := range []func(*lisp.LEnv, *lisp.LVal) *lisp.LVal{s.DumpStringBuiltin, s.DumpBytesBuiltin, s.DumpMessageBuiltin} {
		got := call(env, lisp.SExpr([]*lisp.LVal{jsonDepthValue(20), lisp.Bool(false)}))
		if got.Type != lisp.LError || !strings.Contains(got.String(), "maximum: 8") {
			t.Fatalf("ignored runtime limit: %s", got)
		}
	}
	v := jsonDepthValue(2000)
	if _, ok := s.GoSlice(v, false); ok {
		t.Fatal("GoSlice accepted excessive depth")
	}
	m := lisp.SortedMap()
	m.Map().Set(lisp.String("deep"), v)
	if _, ok := s.GoMap(m, false); ok {
		t.Fatal("GoMap accepted excessive depth")
	}
}
