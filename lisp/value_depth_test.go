// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"fmt"
	"github.com/luthersystems/elps/parser/token"
	"os"
	"os/exec"
	"runtime/debug"
	"strings"
	"testing"
	"time"
)

// Contiguous storage keeps the three-million-node regression affordable and
// avoids spending its deadline on millions of separate allocator calls.
func depthTestValue(depth int) *LVal {
	nodes := make([]LVal, depth+1)
	cells := make([]*LVal, depth)
	nodes[depth].Type = LInt
	nodes[depth].Int = 7
	for i := range depth {
		cells[i] = &nodes[i+1]
		nodes[i].Type = LSExpr
		nodes[i].Cells = cells[i : i+1 : i+1]
	}
	return &nodes[0]
}

func TestValueWalkDepth(t *testing.T) {
	if mode := os.Getenv("ELPS_DEPTH_WALK"); mode != "" {
		debug.SetMaxStack(8 << 20)
		v := depthTestValue(3_000_000)
		var err error
		switch mode {
		case "equal":
			err = GoError(v.Equal(v))
		case "quasiquote":
			err = GoError(findAndUnquote(initSafetyTestEnv(t), v, 0))
		case "stamp":
			err = GoError(stampMacroExpansion(v, &token.Location{File: "depth"}, nil, NewEnv(nil).Runtime))
		case "classify":
			err = GoError(admitSymbolValue(v))
		case "seal":
			v.SealAST()
			for n := v; len(n.Cells) > 0; n = n.Cells[0] {
				if !n.IsSealed() {
					t.Fatal("unsealed child")
				}
			}
			return
		case "locate":
			locateExpansionTree(v, &token.Location{File: "depth", Pos: 1}, nil)
			for n := v; len(n.Cells) > 0; n = n.Cells[0] {
				if n.source == nil {
					t.Fatal("unlocated child")
				}
			}
			return
		case "copy":
			got := builtinCopy(initSafetyTestEnv(t), SExpr([]*LVal{v}))
			err = GoError(got)
			if IsInternalPanic(got) {
				t.Fatal("internal panic")
			}
		case "Copy":
			err = GoError(v.Copy())
		case "detach":
			_, err = v.detach()
		case "template":
			env := NewEnv(nil)
			env.scope["deep"] = v
			_, err = NewTemplate(env)
		case "GoValue":
			err, _ = GoValue(v).(error)
		case "format-string":
			got := builtinFormatString(initSafetyTestEnv(t), SExpr([]*LVal{String("{}"), v}))
			if got.Type != LString || !strings.Contains(got.Str, "#<depth-limit>") {
				t.Fatalf("expected U4 depth marker, got %s", got)
			}
			return
		}
		if err == nil || !strings.Contains(err.Error(), "value nesting depth exceeds maximum: 1024") {
			t.Fatalf("expected ordinary depth error, got %v", err)
		}
		return
	}
	for _, mode := range []string{"copy", "Copy", "detach", "template", "GoValue", "format-string", "equal", "quasiquote", "stamp", "classify", "seal", "locate"} {
		t.Run(mode, func(t *testing.T) {
			ctx, cancel := context.WithTimeout(context.Background(), 45*time.Second)
			defer cancel()
			cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestValueWalkDepth$", "-test.count=1") //nolint:gosec // executes this test binary, not a command supplied by the program under test
			cmd.Env = append(os.Environ(), "ELPS_DEPTH_WALK="+mode)
			out, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("child failed (%v): %.800s", err, out)
			}
		})
	}
}

func TestValueWalkDepthControls(t *testing.T) {
	for _, depth := range []int{1000, 100_000} {
		t.Run(fmt.Sprint(depth), func(t *testing.T) {
			v := depthTestValue(depth)
			copied, err := v.deepCopy()
			if depth > 1024 {
				if err == nil {
					t.Fatal("expected depth error")
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			for range depth {
				copied = copied.Cells[0]
			}
			if copied.Int != 7 {
				t.Fatal("copy lost leaf")
			}
			got := builtinFormatString(initSafetyTestEnv(t), SExpr([]*LVal{String("{}"), v}))
			want := strings.Repeat("(", depth) + "7" + strings.Repeat(")", depth)
			if got.Type != LString || got.Str != want {
				t.Fatal("format lost content")
			}
		})
	}
}

func TestValueDepthConfiguration(t *testing.T) {
	env := initSafetyTestEnv(t)
	for _, limit := range []int{-1, 0, 8, MaxValueDepth + 1} {
		WithMaxValueDepth(limit)(env)
		want := MaxValueDepth
		if limit == 8 {
			want = 8
		}
		if got := env.Runtime.ValueDepthLimit(); got != want {
			t.Fatalf("limit %d: got %d", limit, got)
		}
	}
	WithMaxValueDepth(8)(env)
	got := builtinCopy(env, SExpr([]*LVal{depthTestValue(20)}))
	if got.Type != LError || !strings.Contains(got.String(), "maximum: 8") {
		t.Fatalf("copy ignored option: %s", got)
	}
	source := NewEnv(nil)
	WithMaxValueDepth(8)(source)
	tmpl, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if got := vm.Runtime.ValueDepthLimit(); got != 8 {
		t.Fatalf("template lost option: %d", got)
	}
	source.scope["deep"] = depthTestValue(20)
	if _, err := NewTemplate(source); err == nil || !strings.Contains(err.Error(), "maximum: 8") {
		t.Fatalf("publication ignored option: %v", err)
	}
}

func TestValueDepthCatchable(t *testing.T) {
	env := initSafetyTestEnv(t)
	v := depthTestValue(2000)
	handler := SExpr([]*LVal{Symbol("condition"), SExpr([]*LVal{Symbol("lambda"), Formals("c", VarArgSymbol, "data"), Int(99)})})
	for _, name := range []string{"copy", "equal?"} {
		call := SExpr([]*LVal{Symbol(name), Quote(v)})
		if name == "equal?" {
			call.Cells = append(call.Cells, Quote(v))
		}
		got := env.Eval(SExpr([]*LVal{Symbol("handler-bind"), SExpr([]*LVal{handler}), call}))
		if got.Type != LInt || got.Int != 99 {
			t.Fatalf("%s did not raise catchable error: %s", name, got)
		}
	}
	if got := env.Eval(Int(42)); got.Int != 42 {
		t.Fatal("environment did not survive")
	}
}

func TestValueDepthContainerEdges(t *testing.T) {
	for _, kind := range []string{"map", "vector", "quote", "tag", "error"} {
		t.Run(kind, func(t *testing.T) {
			v := Int(7)
			for range 1100 {
				switch kind {
				case "map":
					m := SortedMap()
					m.Map().Set(String("x"), v)
					v = m
				case "vector":
					v = Vector([]*LVal{v})
				case "quote":
					v = &LVal{Type: LQuote, Cells: []*LVal{v}}
				case "tag":
					v = &LVal{Type: LTaggedVal, Str: "tag", Cells: []*LVal{v}}
				case "error":
					v = &LVal{Type: LError, Str: "error", Cells: []*LVal{v}}
				}
			}
			if _, err := v.deepCopy(); err == nil {
				t.Fatal("deep copy lost guard")
			}
			if got := v.Copy(); got.Type != LError || !strings.Contains(got.String(), "value nesting depth exceeds") {
				t.Fatal("Copy lost guard")
			}
			if kind == "map" || kind == "vector" || kind == "quote" {
				if _, ok := GoValue(v).(error); !ok {
					t.Fatal("GoValue lost guard")
				}
			}
		})
	}
}

func TestGoConversionRootDepth(t *testing.T) {
	v := depthTestValue(MaxValueDepth + 1)
	if _, ok := GoSlice(v); ok {
		t.Error("GoSlice must count its root container")
	}
	m := SortedMap()
	m.Map().Set(String("deep"), depthTestValue(MaxValueDepth))
	if _, ok := GoMap(m); ok {
		t.Error("GoMap must count its root container")
	}
}
