// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"maps"
	"math"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

var updateWalkerGoldens = flag.Bool("update", false, "regenerate value walker goldens")
var goldenHostError = errors.New("fixture map failure")

type goldenFixture struct {
	name  string
	build func(*goldenInput) *lisp.LVal
}

type goldenInput struct {
	trace   []string
	maps    map[*lisp.LVal]*goldenMap
	sources map[*lisp.LVal]int
}

// This corpus is also in libelpspath/walker_fixtures_test.go. Both copies are test code.
func goldenFixtures() []goldenFixture {
	leaf := func(v func() *lisp.LVal) func(*goldenInput) *lisp.LVal {
		return func(*goldenInput) *lisp.LVal { return v() }
	}
	fixtures := []goldenFixture{
		{"go-nil", leaf(func() *lisp.LVal { return nil })},
		{"invalid", leaf(func() *lisp.LVal { return &lisp.LVal{Type: lisp.LInvalid} })},
		{"out-of-range-type", leaf(func() *lisp.LVal { return &lisp.LVal{Type: lisp.LTypeMax} })},
		{"int", leaf(func() *lisp.LVal { return lisp.Int(-17) })},
		{"int-32-boundary", leaf(func() *lisp.LVal { return lisp.Int(2147483647) })},
		{"float", leaf(func() *lisp.LVal { return lisp.Float(0.25) })},
		{"float-whole", leaf(func() *lisp.LVal { return lisp.Float(7) })},
		{"float-negative-zero", leaf(func() *lisp.LVal { return lisp.Float(math.Copysign(0, -1)) })},
		{"float-nan", leaf(func() *lisp.LVal { return lisp.Float(math.NaN()) })},
		{"float-infinity", leaf(func() *lisp.LVal { return lisp.Float(math.Inf(1)) })},
		{"float-negative-infinity", leaf(func() *lisp.LVal { return lisp.Float(math.Inf(-1)) })},
		{"float-range", leaf(func() *lisp.LVal { return lisp.Float(1e21) })},
		{"string", leaf(func() *lisp.LVal { return lisp.String("sample") })},
		{"string-escapes", leaf(func() *lisp.LVal { return lisp.String("\"\\\n\x00<&>\u2028\u2029é") })},
		{"string-invalid-utf8", leaf(func() *lisp.LVal { return lisp.String("a\xff") })},
		{"string-leading-tilde", leaf(func() *lisp.LVal { return lisp.String("~text") })},
		{"string-double-tilde", leaf(func() *lisp.LVal { return lisp.String("~~text") })},
		{"string-typed-symbol", leaf(func() *lisp.LVal { return lisp.String("~$name") })},
		{"string-typed-float", leaf(func() *lisp.LVal { return lisp.String("~d7") })},
		{"string-typed-bytes", leaf(func() *lisp.LVal { return lisp.String("~bAQID") })},
		{"string-unknown-tag", leaf(func() *lisp.LVal { return lisp.String("~unknown") })},
		{"symbol", leaf(func() *lisp.LVal { return lisp.Symbol("name") })},
		{"keyword", leaf(func() *lisp.LVal { return lisp.Symbol(":key") })},
		{"true", leaf(func() *lisp.LVal { return lisp.Symbol("true") })},
		{"false", leaf(func() *lisp.LVal { return lisp.Symbol("false") })},
		{"json-null", leaf(func() *lisp.LVal { return lisp.Symbol("json:null") })},
		{"empty-symbol", leaf(func() *lisp.LVal { return lisp.Symbol("") })},
		{"symbol-invalid-utf8", leaf(func() *lisp.LVal { return lisp.Symbol("\xff") })},
		{"bytes", leaf(func() *lisp.LVal { return lisp.Bytes([]byte{0, 1, 255}) })},
		{"bytes-nil", leaf(func() *lisp.LVal { return lisp.Bytes(nil) })},
		{"bytes-empty", leaf(func() *lisp.LVal { return lisp.Bytes([]byte{}) })},
		{"error", leaf(func() *lisp.LVal { return lisp.ErrorCondition("fixture-error", goldenHostError) })},
		{"function", leaf(func() *lisp.LVal {
			return lisp.FunInPackage("fixture", "identity", lisp.Formals("x"), func(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal { return args.Cells[0] })
		})},
		{"native-string", leaf(func() *lisp.LVal { return lisp.Native("sample") })},
		{"native-number", leaf(func() *lisp.LVal { return lisp.Native(7) })},
		{"native-nil", leaf(func() *lisp.LVal { return lisp.Native(nil) })},
		{"native-container", leaf(func() *lisp.LVal { return lisp.Native(map[string]any{"items": []any{"text", true, nil}}) })},
		{"native-cycle", leaf(func() *lisp.LVal { m := map[string]any{}; m["self"] = m; return lisp.Native(m) })},
		{"nil-list", leaf(func() *lisp.LVal { return lisp.Nil() })},
		{"list", leaf(func() *lisp.LVal { return lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.String("x")}) })},
		{"quoted-list", leaf(func() *lisp.LVal { return lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("x")}) })},
		{"quote", leaf(func() *lisp.LVal { return lisp.Quote(lisp.Quote(lisp.Int(1))) })},
		{"tagged", leaf(func() *lisp.LVal { return goldenTagged(lisp.Int(1)) })},
		{"empty-vector", leaf(func() *lisp.LVal { return lisp.Vector(nil) })},
		{"vector", leaf(func() *lisp.LVal { return lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.String("x")}) })},
		{"array-multi", leaf(func() *lisp.LVal {
			return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(2), lisp.Int(1)}), []*lisp.LVal{lisp.Int(1), lisp.Int(2)})
		})},
		{"array-zero-rank", leaf(func() *lisp.LVal { return lisp.Array(lisp.QExpr(nil), []*lisp.LVal{lisp.Int(1)}) })},
		{"array-zero-dimension", leaf(func() *lisp.LVal { return lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(0), lisp.Int(2)}), nil) })},
		{"map-empty", leaf(lisp.SortedMap)},
		{"map-no-backing", leaf(func() *lisp.LVal { return lisp.SortedMapFromData(lisp.NewMapData(nil)) })},
		{"map-string-keys", leaf(func() *lisp.LVal { return goldenStockMap(lisp.String("b"), lisp.Int(2), lisp.String("a"), lisp.Int(1)) })},
		{"map-symbol-keys", leaf(func() *lisp.LVal {
			return goldenStockMap(lisp.Symbol("name"), lisp.Int(1), lisp.Symbol(":key"), lisp.Int(2))
		})},
		{"map-int-keys", leaf(func() *lisp.LVal {
			return goldenStockMap(lisp.Int(-2), lisp.String("negative"), lisp.Int(7), lisp.String("seven"))
		})},
		{"map-mixed-keys", leaf(func() *lisp.LVal {
			return goldenStockMap(lisp.Int(7), lisp.Int(1), lisp.String("a"), lisp.Int(2), lisp.Symbol("b"), lisp.Int(3))
		})},
		{"map-int-string-collision", leaf(func() *lisp.LVal { return goldenStockMap(lisp.Int(7), lisp.Int(1), lisp.String("7"), lisp.Int(2)) })},
		{"map-tilde-key", leaf(func() *lisp.LVal { return goldenStockMap(lisp.String("~key"), lisp.String("value")) })},
		{"map-invalid-utf8-key", leaf(func() *lisp.LVal { return goldenStockMap(lisp.String("\xff"), lisp.Int(1)) })},
		{"nested", leaf(func() *lisp.LVal {
			return goldenStockMap(lisp.String("items"), lisp.Vector([]*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Nil()}), goldenTagged(lisp.String("text")), goldenStockMap(lisp.String("flag"), lisp.Symbol("true"))}))
		})},
		{"nested-tilde-path", leaf(func() *lisp.LVal {
			return goldenStockMap(lisp.String("a\"\\"), lisp.Vector([]*lisp.LVal{lisp.Int(1), goldenTagged(lisp.Quote(lisp.Quote(lisp.String("~bad"))))}))
		})},
		{"nested-plain", leaf(func() *lisp.LVal {
			return goldenStockMap(lisp.String("items"), lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Float(0.25), lisp.Nil(), lisp.Symbol("true")}))
		})},
		{"list-nil-cell", leaf(func() *lisp.LVal { return lisp.SExpr([]*lisp.LVal{nil}) })},
		{"vector-nil-cell", leaf(func() *lisp.LVal { return lisp.Vector([]*lisp.LVal{nil}) })},
		{"map-nil-value", leaf(func() *lisp.LVal { return goldenStockMap(lisp.String("nil"), nil) })},
		{"tagged-nil-cell", leaf(func() *lisp.LVal { return goldenTagged(nil) })},
		{"tagged-no-cell", leaf(func() *lisp.LVal { return &lisp.LVal{Type: lisp.LTaggedVal, Str: "fixture:box"} })},
		{"tagged-two-cells", leaf(func() *lisp.LVal { v := goldenTagged(lisp.Int(1)); v.Cells = append(v.Cells, lisp.Int(2)); return v })},
		{"tagged-empty-name", leaf(func() *lisp.LVal { v := goldenTagged(lisp.Int(1)); v.Str = ""; return v })},
		{"tagged-invalid-name", leaf(func() *lisp.LVal { v := goldenTagged(lisp.Int(1)); v.Str = "\xff"; return v })},
		{"quote-no-cell", leaf(func() *lisp.LVal { return &lisp.LVal{Type: lisp.LQuote} })},
		{"quote-nil-cell", leaf(func() *lisp.LVal { return &lisp.LVal{Type: lisp.LQuote, Cells: []*lisp.LVal{nil}} })},
		{"array-no-cells", leaf(func() *lisp.LVal { return &lisp.LVal{Type: lisp.LArray} })},
		{"array-nil-dims", leaf(func() *lisp.LVal { return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{nil, lisp.QExpr(nil)}} })},
		{"array-nil-data", leaf(func() *lisp.LVal {
			return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(0)}), nil}}
		})},
		{"array-data-not-list", leaf(func() *lisp.LVal {
			return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(0)}), lisp.String("data")}}
		})},
		{"array-dimension-symbol", leaf(func() *lisp.LVal { v := lisp.Vector(nil); v.Cells[0].Cells[0] = lisp.Symbol("dimension"); return v })},
		{"array-nil-dimension", leaf(func() *lisp.LVal { v := lisp.Vector(nil); v.Cells[0].Cells[0] = nil; return v })},
		{"array-negative-dimension", leaf(func() *lisp.LVal { v := lisp.Vector(nil); v.Cells[0].Cells[0] = lisp.Int(-1); return v })},
		{"array-wrong-size", leaf(func() *lisp.LVal { v := lisp.Vector(nil); v.Cells[0].Cells[0] = lisp.Int(2); return v })},
		{"self-list", leaf(func() *lisp.LVal { v := lisp.SExpr([]*lisp.LVal{nil}); v.Cells[0] = v; return v })},
		{"self-vector", leaf(func() *lisp.LVal { v := lisp.Vector([]*lisp.LVal{nil}); v.Cells[1].Cells[0] = v; return v })},
		{"self-map", leaf(func() *lisp.LVal { v := lisp.SortedMap(); v.MapSetString("self", v); return v })},
		{"self-tagged", leaf(func() *lisp.LVal { v := goldenTagged(nil); v.Cells[0] = v; return v })},
		{"self-quote", leaf(func() *lisp.LVal { v := &lisp.LVal{Type: lisp.LQuote}; v.Cells = []*lisp.LVal{v}; return v })},
		{"branching-cycle", leaf(func() *lisp.LVal { v := lisp.SExpr([]*lisp.LVal{nil, nil}); v.Cells[0], v.Cells[1] = v, v; return v })},
		{"shared-small", leaf(func() *lisp.LVal { v := lisp.Vector([]*lisp.LVal{lisp.Int(1)}); return lisp.SExpr([]*lisp.LVal{v, v}) })},
		{"shared-dag-30-list", leaf(func() *lisp.LVal { return goldenDAG(30, false) })},
		{"shared-dag-30-vector", leaf(func() *lisp.LVal { return goldenDAG(30, true) })},
		{"charge-long-string", leaf(func() *lisp.LVal { return lisp.String(strings.Repeat("x", 3072)) })},
		{"charge-chunks", leaf(func() *lisp.LVal {
			return lisp.Vector([]*lisp.LVal{lisp.String(strings.Repeat("x", 1022)), lisp.String(strings.Repeat("y", 1024)), lisp.String(strings.Repeat("z", 1025))})
		})},
		{"charge-map-key", leaf(func() *lisp.LVal {
			return goldenStockMap(lisp.String("a"+strings.Repeat("x", 1024)), lisp.String("value"), lisp.String("b"), lisp.String(strings.Repeat("y", 2048)))
		})},
		{"charge-escaped-string", leaf(func() *lisp.LVal { return lisp.String(strings.Repeat("<", 1024)) })},
		{"charge-bytes", leaf(func() *lisp.LVal { return lisp.Bytes([]byte(strings.Repeat("x", 3072))) })},
		{"host-map", func(in *goldenInput) *lisp.LVal {
			return in.hostMap("host", []*lisp.LVal{goldenPair(lisp.String("a"), lisp.Int(1))}, nil)
		}},
		{"host-map-error", func(in *goldenInput) *lisp.LVal {
			v := in.hostMap("error", nil, nil)
			in.maps[v].err = goldenHostError
			return v
		}},
		{"host-map-bad-key", func(in *goldenInput) *lisp.LVal {
			return in.hostMap("bad-key", []*lisp.LVal{goldenPair(lisp.Float(0.25), lisp.Int(1))}, nil)
		}},
		{"host-map-key-collision", func(in *goldenInput) *lisp.LVal {
			return in.hostMap("collision", []*lisp.LVal{goldenPair(lisp.String("a"), lisp.Int(1)), goldenPair(lisp.Symbol("a"), lisp.Int(2))}, nil)
		}},
		{"host-map-key-order", func(in *goldenInput) *lisp.LVal {
			return in.hostMap("order", []*lisp.LVal{goldenPair(lisp.String("b"), lisp.Int(2)), goldenPair(lisp.String("a"), lisp.Int(1))}, nil)
		}},
		{"host-map-nil-entry", func(in *goldenInput) *lisp.LVal { return in.hostMap("nil-entry", []*lisp.LVal{nil}, nil) }},
		{"host-map-nil-key", func(in *goldenInput) *lisp.LVal {
			return in.hostMap("nil-key", []*lisp.LVal{goldenPair(nil, lisp.Int(1))}, nil)
		}},
		{"host-map-short-entry", func(in *goldenInput) *lisp.LVal {
			return in.hostMap("short-entry", []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.String("a")})}, nil)
		}},
		{"host-map-replace-sibling", func(in *goldenInput) *lisp.LVal {
			root := lisp.Vector([]*lisp.LVal{nil, lisp.String("before")})
			root.Cells[1].Cells[0] = in.hostMap("replace", []*lisp.LVal{goldenPair(lisp.String("a"), lisp.Int(1))}, func() { root.Cells[1].Cells[1] = lisp.String("after") })
			return root
		}},
		{"host-map-mutate-sibling", func(in *goldenInput) *lisp.LVal {
			later := lisp.SExpr([]*lisp.LVal{lisp.Int(1)})
			first := in.hostMap("mutate", []*lisp.LVal{goldenPair(lisp.String("a"), lisp.Int(1))}, func() { later.Cells[0] = lisp.Int(9) })
			return lisp.Vector([]*lisp.LVal{first, later})
		}},
		{"host-map-replace-map-sibling", func(in *goldenInput) *lisp.LVal {
			root := lisp.SortedMap()
			first := in.hostMap("replace-map", []*lisp.LVal{goldenPair(lisp.String("a"), lisp.Int(1))}, func() { root.MapSetString("z", lisp.String("after")) })
			root.MapSetString("a", first)
			root.MapSetString("z", lisp.String("before"))
			return root
		}},
		{"cycle-before-boundary", func(in *goldenInput) *lisp.LVal {
			v := in.hostMap("cycle-tail", []*lisp.LVal{goldenPair(lisp.String("self"), nil)}, nil)
			m := in.maps[v]
			m.pairs[0].Cells[1] = v
			calls := 0
			m.before = func() {
				calls++
				if calls == 3 {
					m.pairs[0].Cells[1] = lisp.Vector([]*lisp.LVal{lisp.Int(1)})
				}
			}
			return v
		}},
		{"wire-list", leaf(func() *lisp.LVal {
			return goldenWire("~#list", lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.String("~$name")}))
		})},
		{"wire-tagged", leaf(func() *lisp.LVal {
			return goldenWire("~#tagged", lisp.Vector([]*lisp.LVal{lisp.String("fixture:box"), lisp.Vector([]*lisp.LVal{lisp.Int(1)})}))
		})},
		{"wire-array", leaf(func() *lisp.LVal {
			return goldenWire("~#array", lisp.Vector([]*lisp.LVal{lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}), lisp.Vector([]*lisp.LVal{lisp.Int(1), lisp.Int(2)})}))
		})},
		{"wire-empty-list", leaf(func() *lisp.LVal { return goldenWire("~#list", lisp.Vector(nil)) })},
		{"wire-unknown", leaf(func() *lisp.LVal { return goldenWire("~#unknown", lisp.Vector(nil)) })},
		{"wire-nil-payload", leaf(func() *lisp.LVal { return goldenWire("~#list", nil) })},
		{"wire-tagged-cycle", leaf(func() *lisp.LVal {
			v := goldenWire("~#tagged", nil)
			v.Cells[1].Cells[1] = lisp.Vector([]*lisp.LVal{lisp.String("fixture:box"), v})
			return v
		})},
	}
	// Embedders can construct marks, even though evaluation consumes them before builtin calls.
	for _, typ := range []lisp.LType{lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand} {
		fixtures = append(fixtures, goldenFixture{fmt.Sprintf("mark-%d", typ), leaf(func() *lisp.LVal { return &lisp.LVal{Type: typ, Cells: []*lisp.LVal{lisp.String("sentinel")}} })})
	}
	for _, f := range append([]goldenFixture{}, fixtures[:38]...) {
		fixtures = append(fixtures, goldenFixture{"nested-" + f.name, func(in *goldenInput) *lisp.LVal { return lisp.Vector([]*lisp.LVal{lisp.Int(0), f.build(in)}) }})
	}
	for _, kind := range []string{"list", "vector", "map", "tagged", "quote"} {
		for _, n := range []int{3, 4, 63, 64, 65, 1024, 1025} {
			fixtures = append(fixtures, goldenFixture{fmt.Sprintf("depth-%s-%d", kind, n), leaf(func() *lisp.LVal { return goldenChain(kind, n, lisp.Int(1)) })})
		}
	}
	return fixtures
}

func TestValueWalkerFixtureCoverage(t *testing.T) {
	seen := make(map[lisp.LType]bool)
	for _, f := range goldenFixtures() {
		_, v := newGoldenInput(f)
		if v != nil {
			seen[v.Type] = true
		}
	}
	for typ := lisp.LInt; typ < lisp.LTypeMax; typ++ {
		if !seen[typ] {
			t.Errorf("fixture corpus omits LType %d (%s)", typ, typ)
		}
	}
	t.Logf("corpus: %d fixtures", len(goldenFixtures()))
}

func goldenTagged(v *lisp.LVal) *lisp.LVal {
	return &lisp.LVal{Type: lisp.LTaggedVal, Str: "fixture:box", Cells: []*lisp.LVal{v}}
}
func goldenWire(tag string, payload *lisp.LVal) *lisp.LVal {
	return lisp.Vector([]*lisp.LVal{lisp.String(tag), payload})
}
func goldenPair(k, v *lisp.LVal) *lisp.LVal { return lisp.QExpr([]*lisp.LVal{k, v}) }
func goldenStockMap(kv ...*lisp.LVal) *lisp.LVal {
	if len(kv)%2 != 0 {
		panic("fixture map requires key/value pairs")
	}
	v := lisp.SortedMap()
	for i := 0; i+1 < len(kv); i += 2 {
		v.MapSetLVal(kv[i], kv[i+1])
	}
	return v
}
func goldenChain(kind string, n int, v *lisp.LVal) *lisp.LVal {
	for range n {
		switch kind {
		case "list":
			v = lisp.SExpr([]*lisp.LVal{v})
		case "vector":
			v = lisp.Vector([]*lisp.LVal{v})
		case "map":
			v = goldenStockMap(lisp.String("next"), v)
		case "tagged":
			v = goldenTagged(v)
		case "quote":
			v = &lisp.LVal{Type: lisp.LQuote, Cells: []*lisp.LVal{v}}
		}
	}
	return v
}
func goldenDAG(n int, vector bool) *lisp.LVal {
	v := lisp.Int(1)
	for range n {
		if vector {
			v = lisp.Vector([]*lisp.LVal{v, v})
		} else {
			v = lisp.SExpr([]*lisp.LVal{v, v})
		}
	}
	return v
}

type goldenMap struct {
	name   string
	in     *goldenInput
	pairs  []*lisp.LVal
	err    error
	before func()
}

func (in *goldenInput) hostMap(name string, pairs []*lisp.LVal, before func()) *lisp.LVal {
	m := &goldenMap{name: name, in: in, pairs: pairs, before: before}
	v := lisp.SortedMapFromData(lisp.NewMapData(m))
	in.maps[v] = m
	return v
}
func (m *goldenMap) Len() int {
	if m.err != nil {
		return 1
	}
	return len(m.pairs)
}
func (m *goldenMap) Get(k *lisp.LVal) (*lisp.LVal, bool) {
	for _, p := range m.pairs {
		if p != nil && len(p.Cells) == 2 && p.Cells[0] != nil && p.Cells[0].Type == k.Type && p.Cells[0].Str == k.Str && p.Cells[0].Int == k.Int {
			return p.Cells[1], true
		}
	}
	return lisp.Nil(), false
}
func (*goldenMap) Set(*lisp.LVal, *lisp.LVal) *lisp.LVal {
	return lisp.Errorf("fixture map is read-only")
}
func (*goldenMap) Del(*lisp.LVal) *lisp.LVal { return lisp.Errorf("fixture map is read-only") }
func (m *goldenMap) Keys() *lisp.LVal {
	var keys []*lisp.LVal
	for _, p := range m.pairs {
		if p != nil && len(p.Cells) > 0 {
			keys = append(keys, p.Cells[0])
		}
	}
	return lisp.QExpr(keys)
}
func (m *goldenMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	m.in.trace = append(m.in.trace, "entries:"+m.name)
	if m.before != nil {
		m.before()
	}
	if m.err != nil {
		return lisp.Error(goldenHostError)
	}
	copy(buf, m.pairs)
	return lisp.Int(len(m.pairs))
}

func newGoldenInput(f goldenFixture) (*goldenInput, *lisp.LVal) {
	in := &goldenInput{maps: make(map[*lisp.LVal]*goldenMap)}
	v := f.build(in)
	_, in.sources = in.render(v)
	return in, v
}

// render records the complete graph with numbered references. It never invokes host code.
// Source numbers pin aliases without recording addresses or expanding shared graphs.
func (in *goldenInput) render(root *lisp.LVal) (string, map[*lisp.LVal]int) {
	ids := make(map[*lisp.LVal]int)
	var pending []*lisp.LVal
	ref := func(v *lisp.LVal) string {
		if v == nil {
			return "nil"
		}
		if ids[v] == 0 {
			ids[v] = len(ids) + 1
			pending = append(pending, v)
		}
		return "#" + strconv.Itoa(ids[v])
	}
	var out strings.Builder
	out.WriteString(ref(root))
	for i := 0; i < len(pending); i++ { //nolint:intrange // ref appends discovered values to pending during traversal.
		v := pending[i]
		fmt.Fprintf(&out, ";#%d=%s(q=%t", ids[v], v.Type, v.IsQuoted())
		if n := in.sources[v]; n != 0 {
			fmt.Fprintf(&out, ",source=#%d", n)
		}
		switch v.Type {
		case lisp.LInt:
			fmt.Fprintf(&out, ",int=%d", v.Int)
		case lisp.LFloat:
			fmt.Fprintf(&out, ",float=%s", strconv.FormatFloat(v.Float, 'g', -1, 64))
		case lisp.LBytes:
			fmt.Fprintf(&out, ",bytes=%s,nil=%t", hex.EncodeToString(v.Bytes()), v.Bytes() == nil)
		case lisp.LNative:
			fmt.Fprintf(&out, ",native=%T", v.Native)
		case lisp.LFun:
			fmt.Fprintf(&out, ",name=%s", v.Str)
		default:
			if v.Str != "" {
				fmt.Fprintf(&out, ",str=%s", strconv.Quote(v.Str))
			}
		}
		out.WriteString(")")
		if v.Type == lisp.LFun || v.Type == lisp.LNative {
			continue
		}
		if v.Type == lisp.LSortMap {
			out.WriteString("{")
			if m := in.maps[v]; m != nil {
				for j, p := range m.pairs {
					if j > 0 {
						out.WriteString(",")
					}
					out.WriteString(ref(p))
				}
			} else if pairs, ok := v.AppendMapKeyPairs(nil); ok {
				slices.SortFunc(pairs, func(a, b lisp.MapKeyPair) int {
					if a.Kind == lisp.LInt || b.Kind == lisp.LInt {
						if a.Kind != b.Kind {
							if a.Kind == lisp.LInt {
								return -1
							}
							return 1
						}
						if a.Int < b.Int {
							return -1
						}
						if a.Int > b.Int {
							return 1
						}
						return 0
					}
					return strings.Compare(a.Key, b.Key)
				})
				for j, p := range pairs {
					if j > 0 {
						out.WriteString(",")
					}
					fmt.Fprintf(&out, "%s:%s:%d=%s", p.Kind, strconv.Quote(p.Key), p.Int, ref(p.Val))
				}
			} else {
				out.WriteString("no-backing")
			}
			out.WriteString("}")
		} else if len(v.Cells) > 0 {
			out.WriteString("[")
			for j, c := range v.Cells {
				if j > 0 {
					out.WriteString(",")
				}
				out.WriteString(ref(c))
			}
			out.WriteString("]")
		}
	}
	return out.String(), ids
}

type goldenRecord struct {
	Name          string   `json:"name"`
	Output        string   `json:"output"`
	Error         string   `json:"error"`
	ErrorType     string   `json:"error_type"`
	Causes        []string `json:"causes"`
	Case          string   `json:"case,omitempty"`
	Path          string   `json:"path,omitempty"`
	Condition     string   `json:"condition,omitempty"`
	ConditionData []string `json:"condition_data,omitempty"`
	Trace         []string `json:"trace"`
	Panic         string   `json:"panic,omitempty"`
	State         string   `json:"state,omitempty"`
	After         string   `json:"input_after,omitempty"`
}

func goldenObserve(name string, in *goldenInput, run func() (*lisp.LVal, []byte, error), causes map[string]error) goldenRecord {
	var r goldenRecord
	r.Name = name
	r.Causes = []string{}
	func() {
		defer func() {
			if p := recover(); p != nil {
				r.Panic = fmt.Sprintf("%T: %v", p, p)
			}
			if strings.Contains(name, "host-map-replace") || strings.Contains(name, "host-map-mutate") || strings.Contains(name, "cycle-before-boundary") {
				for v, id := range in.sources {
					if id == 1 {
						r.After, _ = in.render(v)
						break
					}
				}
			}
			r.Trace = append([]string{}, in.trace...)
		}()
		goldenRun(&r, in, run, causes)
	}()
	return r
}

// goldenRun runs run and records its output, error and matching causes in r.
func goldenRun(r *goldenRecord, in *goldenInput, run func() (*lisp.LVal, []byte, error), causes map[string]error) {
	v, b, err := run()
	if v != nil {
		r.Output, _ = in.render(v)
	} else if b != nil {
		r.Output = string(b)
	}
	if err != nil {
		r.Error, r.ErrorType = err.Error(), fmt.Sprintf("%T", err)
		for _, name := range []string{"typed-limit", "charge", "host", "cycle", "depth", "operation-stopped"} {
			if cause := causes[name]; cause != nil && errors.Is(err, cause) {
				r.Causes = append(r.Causes, name)
			}
		}
	}
}

// compactWalkerGolden preserves expectations and reuses identical records by name.
func compactWalkerGolden(data []byte) ([]byte, error) {
	records, err := decodeWalkerGolden(data)
	if err != nil {
		return nil, err
	}
	var out bytes.Buffer
	out.WriteString("[\n")
	shared := make(map[string]string)
	for i, r := range records {
		name, ok := r["name"].(string)
		if !ok {
			return nil, fmt.Errorf("walker golden record %d: name is %T, want string", i, r["name"])
		}
		delete(r, "name")
		payload, err := json.Marshal(r)
		if err != nil {
			return nil, err
		}
		if prior := shared[string(payload)]; prior != "" {
			r = map[string]any{"same_as": prior}
		} else {
			shared[string(payload)] = name
		}
		r["name"] = name
		encoded, err := json.Marshal(r)
		if err != nil {
			return nil, err
		}
		if i > 0 {
			out.WriteString(",\n")
		}
		out.Write(encoded)
	}
	out.WriteString("\n]\n")
	return out.Bytes(), nil
}

func decodeWalkerGolden(data []byte) ([]map[string]any, error) {
	var records []map[string]any
	if err := json.Unmarshal(data, &records); err != nil {
		return nil, err
	}
	seen := make(map[string]map[string]any)
	for i, r := range records {
		name, ok := r["name"].(string)
		if !ok || name == "" || seen[name] != nil {
			return nil, fmt.Errorf("invalid or duplicate fixture name: %v", r["name"])
		}
		if prior, ok := r["same_as"].(string); ok {
			if seen[prior] == nil || len(r) != 2 {
				return nil, fmt.Errorf("fixture %s: invalid same_as reference %s", name, prior)
			}
			r = maps.Clone(seen[prior])
			r["name"] = name
		}
		for k, v := range r {
			if v == "" {
				delete(r, k)
				continue
			}
			if list, ok := v.([]any); ok && len(list) == 0 {
				delete(r, k)
				continue
			}
			r[k] = compactGoldenPayload(v)
		}
		records[i], seen[name] = r, r
	}
	return records, nil
}

// Large strings retain their byte length and SHA-256 digest, including error data.
func compactGoldenPayload(v any) any {
	switch v := v.(type) {
	case string:
		if len(v) > 512 {
			digest := sha256.Sum256([]byte(v))
			return map[string]any{"len": len(v), "sha256": hex.EncodeToString(digest[:])}
		}
	case []any:
		for i := range v {
			v[i] = compactGoldenPayload(v[i])
		}
	case map[string]any:
		for k := range v {
			v[k] = compactGoldenPayload(v[k])
		}
	}
	return v
}

func checkWalkerGolden(t *testing.T, name string, records []goldenRecord) {
	t.Helper()
	raw, err := json.Marshal(records)
	if err != nil {
		t.Fatal(err)
	}
	data, err := compactWalkerGolden(raw)
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join("testdata", name+".golden.json")
	if *updateWalkerGoldens {
		// Restrict fixture directory access to the owner and group.
		if err = os.MkdirAll(filepath.Dir(path), 0o750); err != nil {
			t.Fatal(err)
		}
		// Only the owner needs to read or write generated golden files.
		if err = os.WriteFile(path, data, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	want, err := os.ReadFile(path) //nolint:gosec // G304: the golden path comes from test-controlled names.
	if err != nil {
		t.Fatalf("%v; generate with -update on the unchanged walkers", err)
	}
	expected, err := decodeWalkerGolden(want)
	if err != nil {
		t.Fatal(err)
	}
	actual, err := decodeWalkerGolden(data)
	if err != nil {
		t.Fatal(err)
	}
	if len(expected) != len(actual) {
		t.Fatalf("%s: got %d fixtures, want %d", path, len(actual), len(expected))
	}
	for i := range actual {
		a, err := json.Marshal(expected[i])
		if err != nil {
			t.Fatal(err)
		}
		b, err := json.Marshal(actual[i])
		if err != nil {
			t.Fatal(err)
		}
		if bytes.Equal(a, b) {
			continue
		}
		offset := 0
		for offset < min(len(a), len(b)) && a[offset] == b[offset] {
			offset++
		}
		start := max(0, offset-80)
		// Hash-only expectations cannot locate a difference inside the original payload.
		t.Errorf("%s: fixture %s differs; encoded lengths want=%d got=%d, first differing offset=%d\nwant: %s\ngot:  %s",
			path, records[i].Name, len(a), len(b), offset,
			a[start:min(len(a), offset+160)], b[start:min(len(b), offset+160)])
	}
	t.Logf("%s: %d fixtures", name, len(records))
}
