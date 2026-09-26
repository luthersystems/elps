// Copyright © 2026 The ELPS authors

package lisp

import (
	"context"
	"fmt"
	"github.com/luthersystems/elps/parser/token"
	"os"
	"os/exec"
	"runtime/debug"
	"strconv"
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
		depth, _ := strconv.Atoi(os.Getenv("ELPS_DEPTH_SIZE"))
		v := depthTestValue(depth)
		var got *LVal
		var err error
		start := time.Now()
		switch mode {
		case "equal":
			other := depthTestValue(depth)
			got = v.Equal(other)
			if depth < MaxValueDepth {
				if got.Type == LError || !True(got) {
					t.Fatal("equal values differ")
				}
				leaf := other
				for range depth {
					leaf = leaf.Cells[0]
				}
				leaf.Int = 8
				got = v.Equal(other)
				if got.Type == LError || True(got) {
					t.Fatal("different leaves compare equal")
				}
				got = nil
			}
		case "quasiquote":
			got = findAndUnquote(initSafetyTestEnv(t), v, 0)
		case "stamp":
			got = stampMacroExpansion(v, &token.Location{File: "depth"}, nil, NewEnv(nil).Runtime)
		case "classify":
			got = admitSymbolValue(v, MaxValueDepth)
		case "seal":
			v.SealAST()
			got = v
		case "locate":
			locateExpansionTree(v, &token.Location{File: "depth", Pos: 1}, nil)
			got = v
		case "copy":
			got = builtinCopy(initSafetyTestEnv(t), SExpr([]*LVal{v}))
		case "Copy":
			got = v.Copy()
		case "detach":
			got, err = v.detach()
		case "template", "template-sealed":
			if mode == "template-sealed" {
				v.SealAST()
			}
			env := NewEnv(nil)
			env.scope = scopeOf(map[string]*LVal{"deep": v})
			var tmpl *Template
			tmpl, err = NewTemplate(env)
			if err == nil {
				var vm *LEnv
				vm, err = tmpl.NewVM()
				if err == nil {
					got = vm.scope.val("deep")
				}
			}
		case "GoValue":
			x := GoValue(v)
			err, _ = x.(error)
			if err == nil {
				for range depth {
					xs, ok := x.([]any)
					if !ok || len(xs) != 1 {
						t.Fatal("conversion lost container")
					}
					x = xs[0]
				}
				if x != 7 {
					t.Fatal("conversion lost leaf")
				}
			}
		case "format-string":
			got = builtinFormatString(initSafetyTestEnv(t), SExpr([]*LVal{String("{}"), v}))
			// Rendering answers with its own fixed 1024-level marker at
			// every value depth; the value-depth limit no longer decides
			// whether printing a value is an error.
			want := strings.Repeat("(", 1024) + "#<depth-limit>" + strings.Repeat(")", 1024)
			if got.Type != LString || got.Str != want {
				t.Fatal("incorrect bounded rendered text")
			}
			got = nil
		}
		if got != nil && got.Type == LError {
			if IsInternalPanic(got) {
				t.Fatal("internal panic")
			}
			err = GoError(got)
		}
		if depth >= MaxValueDepth && mode != "seal" && mode != "locate" && mode != "format-string" {
			if err == nil || !strings.Contains(err.Error(), "value nesting depth exceeds maximum: 1000000") {
				t.Fatalf("expected ordinary depth error, got %v", err)
			}
		} else {
			if err != nil {
				t.Fatal(err)
			}
			if got != nil {
				for range depth {
					if got.Type != LSExpr || len(got.Cells) != 1 {
						t.Fatal("lost container")
					}
					if mode == "seal" && !got.IsSealed() {
						t.Fatal("unsealed child")
					}
					if (mode == "locate" || mode == "stamp") && got.source == nil {
						t.Fatal("unlocated child")
					}
					got = got.Cells[0]
				}
				if got.Type != LInt || got.Int != 7 {
					t.Fatal("lost leaf")
				}
			}
		}
		fmt.Printf("%s depth=%d walker_wall=%s\n", mode, depth, time.Since(start))
		if got := NewEnv(nil).Eval(Int(42)); got.Int != 42 {
			t.Fatal("process did not survive")
		}
		return
	}
	for _, depth := range []int{100_000, 3_000_000} {
		t.Run(strconv.Itoa(depth), func(t *testing.T) {
			for _, mode := range []string{"copy", "Copy", "detach", "template", "template-sealed", "GoValue", "format-string", "equal", "quasiquote", "stamp", "classify", "seal", "locate"} {
				t.Run(mode, func(t *testing.T) {
					ctx, cancel := context.WithTimeout(context.Background(), 45*time.Second)
					defer cancel()
					cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestValueWalkDepth$", "-test.count=1") //nolint:gosec // executes this test binary with a fixed test selector
					cmd.Env = append(os.Environ(), "ELPS_DEPTH_WALK="+mode, "ELPS_DEPTH_SIZE="+strconv.Itoa(depth))
					start := time.Now()
					out, err := cmd.CombinedOutput()
					t.Logf("process_wall=%s %s", time.Since(start), out)
					if err != nil {
						t.Fatalf("child failed: %v", err)
					}
				})
			}
		})
	}
}

// The external-deadline controls above inspect every container and the leaf.
func TestValueWalkDepthControls(t *testing.T) {
	v := depthTestValue(100_000)
	copied, err := v.deepCopy()
	if err != nil {
		t.Fatal(err)
	}
	for range 100_000 {
		if copied == v {
			t.Fatal("copy aliases source")
		}
		copied = copied.Cells[0]
		v = v.Cells[0]
	}
	if copied.Int != 7 {
		t.Fatal("lost leaf")
	}
}

func TestValueDepthConfiguration(t *testing.T) {
	env := initSafetyTestEnv(t)
	for _, limit := range []int{-1, 0, 8, 1024, MaxValueDepth + 1} {
		rc := WithMaxValueDepth(limit)(env)
		if limit < 1024 {
			if rc.Type != LError {
				t.Fatal("accepted invalid limit")
			}
			continue
		}
		if rc.Type == LError || env.Runtime.ValueDepthLimit() != limit {
			t.Fatalf("limit %d rejected", limit)
		}
	}

	WithMaxValueDepth(1024)(env)
	got := builtinCopy(env, SExpr([]*LVal{depthTestValue(1100)}))
	if got.Type != LError || !strings.Contains(got.String(), "maximum: 1024") {
		t.Fatalf("copy ignored option: %s", got)
	}
	source := NewEnv(nil)
	WithMaxValueDepth(1024)(source)
	tmpl, err := NewTemplate(source)
	if err != nil {
		t.Fatal(err)
	}
	vm, err := tmpl.NewVM()
	if err != nil {
		t.Fatal(err)
	}
	if got := vm.Runtime.ValueDepthLimit(); got != 1024 {
		t.Fatalf("template lost option: %d", got)
	}
	source.scope = scopeOf(map[string]*LVal{"deep": depthTestValue(1100)})
	if _, err := NewTemplate(source); err == nil || !strings.Contains(err.Error(), "maximum: 1024") {
		t.Fatalf("publication ignored option: %v", err)
	}
}

func TestValueDepthCatchable(t *testing.T) {
	env := initSafetyTestEnv(t)
	WithMaxValueDepth(1024)(env)
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
			d := detacher{seen: make(map[*LVal]*LVal), runtime: &Runtime{MaxValueDepth: 1024}, shareOpaque: true}
			if _, err := d.detach(v); err == nil {
				t.Fatal("deep copy lost guard")
			}
			if got, _ := v.copyWithRuntime(&Runtime{MaxValueDepth: 1024}); got.Type != LError || !strings.Contains(got.String(), "value nesting depth exceeds") {
				t.Fatal("Copy lost guard")
			}
			if kind == "map" || kind == "vector" || kind == "quote" {
				if _, ok := GoValue(v).(error); ok {
					t.Fatal("GoValue rejected valid depth")
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

// Prove that raising the default changes actual iterative traversal, not just
// the stored option. A subprocess deadline also bounds unexpected regressions.
func TestValueDepthRaisedLimit(t *testing.T) {
	if os.Getenv("ELPS_RAISED_DEPTH") == "1" {
		debug.SetMaxStack(8 << 20)
		const depth = MaxValueDepth + 1
		v := depthTestValue(depth)
		rt := &Runtime{MaxValueDepth: MaxValueDepth + 1024}
		cp, err := v.copyWithRuntime(rt)
		if err != nil {
			t.Fatal(err)
		}
		for range depth {
			if cp == v || len(cp.Cells) != 1 {
				t.Fatal("invalid copy")
			}
			cp = cp.Cells[0]
			v = v.Cells[0]
		}
		if cp.Int != 7 {
			t.Fatal("lost leaf")
		}
		return
	}
	ctx, cancel := context.WithTimeout(t.Context(), 45*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestValueDepthRaisedLimit$", "-test.count=1") //nolint:gosec // this test binary with a fixed selector
	cmd.Env = append(os.Environ(), "ELPS_RAISED_DEPTH=1")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("raised-limit child: %v\n%s", err, out)
	}
}
