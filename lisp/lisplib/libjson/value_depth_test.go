// Copyright © 2026 The ELPS authors

package libjson

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"runtime/debug"
	"strconv"
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
		depth, _ := strconv.Atoi(os.Getenv("ELPS_JSON_SIZE"))
		v := jsonDepthValue(depth)
		start := time.Now()
		s := DefaultSerializer()
		var err error
		if mode == "GoValue" {
			x := s.GoValue(v, false)
			err, _ = x.(error)
			if err == nil {
				for range depth {
					xs, ok := x.([]interface{})
					if !ok || len(xs) != 1 {
						t.Fatal("lost container")
					}
					x = xs[0]
				}
				if x != 7 {
					t.Fatal("lost leaf")
				}
			}
		} else {
			env := lisp.NewEnv(nil)
			got := s.DumpStringBuiltin(env, lisp.SExpr([]*lisp.LVal{v, lisp.Nil()}))
			err = lisp.GoError(got)
			if depth < lisp.MaxValueDepth && (got.Type != lisp.LString || got.Str != strings.Repeat("[", depth)+"7"+strings.Repeat("]", depth)) {
				t.Fatal("JSON lost content")
			}
			if lisp.IsInternalPanic(got) {
				t.Fatal("internal panic")
			}
		}
		if depth >= lisp.MaxValueDepth && (err == nil || !strings.Contains(err.Error(), "value nesting depth exceeds maximum: 1000000")) {
			t.Fatalf("expected ordinary depth error, got %v", err)
		}
		if depth < lisp.MaxValueDepth && err != nil {
			t.Fatal(err)
		}
		fmt.Printf("%s depth=%d walker_wall=%s\n", mode, depth, time.Since(start))
		return
	}
	for _, depth := range []int{100_000, 3_000_000} {
		for _, mode := range []string{"dump-string", "GoValue"} {
			t.Run(fmt.Sprintf("%d/%s", depth, mode), func(t *testing.T) {
				ctx, cancel := context.WithTimeout(context.Background(), 45*time.Second)
				defer cancel()
				cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestJSONValueDepth$", "-test.count=1") //nolint:gosec // executes this test binary, not a command supplied by the program under test
				cmd.Env = append(os.Environ(), "ELPS_JSON_DEPTH="+mode, "ELPS_JSON_SIZE="+fmt.Sprint(depth))
				start := time.Now()
				out, err := cmd.CombinedOutput()
				t.Logf("process_wall=%s %s", time.Since(start), out)
				if err != nil {
					t.Fatalf("child failed (%v): %.800s", err, out)
				}
			})
		}
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
	got, err = Dump(jsonDepthValue(100_000), false)
	if err != nil || string(got) != strings.Repeat("[", 100_000)+"7"+strings.Repeat("]", 100_000) {
		t.Fatalf("incomplete 100k JSON: %v", err)
	}

}

func TestJSONValueDepthConfiguration(t *testing.T) {
	env := lisp.NewEnv(nil)
	lisp.WithMaxValueDepth(1024)(env)
	s := DefaultSerializer()
	for _, call := range []func(*lisp.LEnv, *lisp.LVal) *lisp.LVal{s.DumpStringBuiltin, s.DumpBytesBuiltin, s.DumpMessageBuiltin} {
		got := call(env, lisp.SExpr([]*lisp.LVal{jsonDepthValue(1100), lisp.Bool(false)}))
		if got.Type != lisp.LError || !strings.Contains(got.String(), "maximum: 1024") {
			t.Fatalf("ignored runtime limit: %s", got)
		}
	}
	v := jsonDepthValue(lisp.MaxValueDepth + 1)
	if _, ok := s.GoSlice(v, false); ok {
		t.Fatal("GoSlice accepted excessive depth")
	}
	m := lisp.SortedMap()
	m.Map().Set(lisp.String("deep"), v)
	if _, ok := s.GoMap(m, false); ok {
		t.Fatal("GoMap accepted excessive depth")
	}
}

func TestJSONValueDepthRaisedLimit(t *testing.T) {
	if os.Getenv("ELPS_JSON_RAISED_DEPTH") == "1" {
		debug.SetMaxStack(8 << 20)
		const depth = lisp.MaxValueDepth + 1
		env := lisp.NewEnv(nil)
		if rc := lisp.WithMaxValueDepth(depth + 1024)(env); rc.Type == lisp.LError {
			t.Fatal(rc)
		}
		got := DefaultSerializer().DumpStringBuiltin(env, lisp.SExpr([]*lisp.LVal{jsonDepthValue(depth), lisp.Nil()}))
		if got.Type != lisp.LString || got.Str != strings.Repeat("[", depth)+"7"+strings.Repeat("]", depth) {
			t.Fatal("raised limit did not encode the complete value")
		}
		return
	}
	ctx, cancel := context.WithTimeout(t.Context(), 45*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestJSONValueDepthRaisedLimit$", "-test.count=1") //nolint:gosec // this test binary with a fixed selector
	cmd.Env = append(os.Environ(), "ELPS_JSON_RAISED_DEPTH=1")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("raised-limit child: %v\n%s", err, out)
	}
}
