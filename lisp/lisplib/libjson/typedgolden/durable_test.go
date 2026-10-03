// Copyright © 2026 The ELPS authors

package typedgolden_test

import (
	"errors"
	"os"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/luthersystems/elps/parser"
)

const durableGoldenFile = "testdata/durable.txt"

// goldenCounter is a pointer native and goldenPoint a value native.  Their
// codecs are part of the frozen corpus: test:counter saves its count and
// test:point saves (x y).
type goldenCounter struct{ n int }

type goldenPoint struct{ x, y int }

func durableRegistry(t *testing.T) *libjson.DurableRegistry {
	t.Helper()
	reg := libjson.NewDurableRegistry()
	must(t, libjson.RegisterNative[*goldenCounter](reg, "test:counter", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
			return lisp.Int(nativeOf[*goldenCounter](v).n), nil
		},
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
			if p.Type != lisp.LInt {
				return nil, errors.New("not an int")
			}
			return lisp.Native(&goldenCounter{n: p.Int}), nil
		},
	}))
	must(t, libjson.RegisterNative[goldenPoint](reg, "test:point", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
			p := nativeOf[goldenPoint](v)
			return list(lisp.Int(p.x), lisp.Int(p.y)), nil
		},
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
			if p.Type != lisp.LSExpr || len(p.Cells) != 2 {
				return nil, errors.New("not a pair")
			}
			return lisp.Native(goldenPoint{x: p.Cells[0].Int, y: p.Cells[1].Int}), nil
		},
	}))
	// test:box saves the value it holds, which can be any value.
	must(t, libjson.RegisterNative[*lisp.LVal](reg, "test:box", 1, libjson.NativeFuncs{
		Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return nativeOf[*lisp.LVal](v), nil },
		Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) { return lisp.Native(p), nil },
	}))
	reg.Freeze()
	return reg
}

// nativeOf returns v's native payload as a T, or T's zero value.
func nativeOf[T any](v *lisp.LVal) T {
	x, _ := v.Native.(T)
	return x
}

func must(t *testing.T, err error) {
	t.Helper()
	if err != nil {
		t.Fatal(err)
	}
}

func durableEnv(t *testing.T) *lisp.LEnv {
	t.Helper()
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	must(t, lisp.GoError(lisp.InitializeUserEnv(env)))
	must(t, lisp.GoError(env.LoadString("golden", `(defun golden-cmp (a b) (< a b))`)))
	return env
}

func vec(xs ...*lisp.LVal) *lisp.LVal { return lisp.Vector(xs) }

func tagged(name string, v *lisp.LVal) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LTaggedVal, Str: name, Cells: []*lisp.LVal{v}}
}

// durableCorpus is the value behind each line of testdata/durable.txt.
func durableCorpus(t *testing.T, env *lisp.LEnv) []struct {
	name string
	v    *lisp.LVal
} {
	t.Helper()
	m := smap(s("a"), lisp.Int(1))
	self := lisp.SortedMap()
	self.MapSetLVal(s("self"), self)
	selfVec := vec(lisp.Int(1))
	selfVec.Cells[1].Cells = append(selfVec.Cells[1].Cells, selfVec)
	selfVec.Cells[0].Cells[0] = lisp.Int(2)
	b := lisp.Bytes([]byte("hi"))
	l := list(lisp.Int(1), lisp.Int(2))
	tv := tagged("user:point", l)
	arr := lisp.Array(list(ints(2, 2)...), ints(1, 2, 3, 4))
	outer := lisp.SortedMap()
	inner := list(outer)
	outer.MapSetLVal(sym("inner"), inner)
	c := lisp.Native(&goldenCounter{n: 7})
	p := lisp.Native(goldenPoint{x: 1, y: 2})
	shared := smap(s("k"), s("v"))
	emptyVec, emptyMap, emptyBytes := vec(), lisp.SortedMap(), lisp.Bytes(nil)
	fn := env.LoadString("golden", `golden-cmp`)
	less := env.LoadString("golden", `<`)
	a, b2, c2 := smap(s("n"), lisp.Int(1)), smap(s("n"), lisp.Int(2)), smap(s("n"), lisp.Int(3))
	return []struct {
		name string
		v    *lisp.LVal
	}{
		{"tree", smap(sym("a"), list(lisp.Int(1), f(2), s("s")), s("b"), vec(sym(":k"), list()))},
		{"scalars", list(lisp.Int(0), f(0.5), f(1), s("~x"), sym("true"), sym(":k"), sym("x"), lisp.Bytes([]byte{0}), list())},
		{"two-locals-one-map", list(m, m)},
		{"map-in-two-lists", vec(list(m, lisp.Int(1)), list(m, lisp.Int(2)))},
		{"map-holds-itself", self},
		{"vector-holds-itself", selfVec},
		{"shared-bytes", smap(s("x"), b, s("y"), b)},
		{"shared-list-and-tagged", list(l, l, tv, tv)},
		{"shared-array-2x2", list(arr, arr)},
		{"map-list-cycle", outer},
		{"shared-empties", list(emptyVec, emptyVec, emptyMap, emptyMap, emptyBytes, emptyBytes)},
		{"id-order", list(a, b2, c2, c2, b2, a)},
		{"native-pointer-shared", list(c, c)},
		{"native-value", p},
		{"native-in-native", lisp.Native(lisp.Native(goldenPoint{x: 3, y: 4}))},
		{"native-payload-shares-sibling", list(shared, lisp.Native(shared))},
		{"native-payload-cycle", lisp.Native(self)},
		{"functions", list(fn, less, fn)},
		{"bptree-shape", lisp.Native(list(sym(":prefix"), s("p"), sym(":compare"), fn))},
	}
}

func encodeDurableCorpus(t *testing.T) []string {
	t.Helper()
	env := durableEnv(t)
	reg := durableRegistry(t)
	var lines []string
	for _, c := range durableCorpus(t, env) {
		b, err := libjson.DumpDurable(env, c.v, reg)
		if err != nil {
			t.Fatalf("%s: %v", c.name, err)
		}
		lines = append(lines, c.name+"\t"+string(b))
	}
	return lines
}

func TestDurableGoldenCorpus(t *testing.T) {
	if *update {
		if strconv.IntSize != 64 {
			t.Fatal("regenerate the golden file where int is 64 bits")
		}
		if err := os.WriteFile(durableGoldenFile, []byte(strings.Join(encodeDurableCorpus(t), "\n")+"\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	golden := readGolden(t, durableGoldenFile)
	env := durableEnv(t)
	reg := durableRegistry(t)
	cs := durableCorpus(t, env)
	if len(golden) != len(cs) {
		t.Fatalf("golden file has %d entries, corpus has %d", len(golden), len(cs))
	}
	for _, c := range cs {
		want, ok := golden[c.name]
		if !ok {
			t.Errorf("%s: missing from %s", c.name, durableGoldenFile)
			continue
		}
		got, err := libjson.DumpDurable(env, c.v, reg)
		if err != nil {
			t.Errorf("%s: %v", c.name, err)
			continue
		}
		if string(got) != want {
			t.Errorf("%s:\n got %s\nwant %s", c.name, got, want)
		}
		back, err := libjson.LoadDurable(env, []byte(want), reg)
		if err != nil {
			t.Errorf("%s: golden does not decode: %v", c.name, err)
			continue
		}
		if again, err := libjson.DumpDurable(env, back, reg); err != nil || string(again) != want {
			t.Errorf("%s: golden does not round-trip: %s (%v)", c.name, again, err)
		}
	}
}

// TestDurableExtendsTyped checks that durable mode adds to typed JSON and
// changes none of it: for every typed golden value, the durable document is
// the typed bytes inside the durable header, and the typed bytes are still
// the frozen ones.
func TestDurableExtendsTyped(t *testing.T) {
	env := durableEnv(t)
	golden := readGolden(t, goldenFile)
	for _, c := range corpus() {
		if c.v == nil {
			continue
		}
		typed, err := libjson.DumpTyped(c.v)
		if err != nil {
			t.Fatalf("%s: %v", c.name, err)
		}
		if string(typed) != golden[c.name] {
			t.Errorf("%s: typed bytes changed: %s", c.name, typed)
		}
		durable, err := libjson.DumpDurable(env, c.v, nil)
		if err != nil {
			t.Errorf("%s: %v", c.name, err)
			continue
		}
		if want := `["~#durable",[1,` + string(typed) + `]]`; string(durable) != want {
			t.Errorf("%s:\n got %s\nwant %s", c.name, durable, want)
		}
	}
}
