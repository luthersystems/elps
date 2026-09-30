// Copyright © 2026 The ELPS authors

package typedgolden_test

import (
	"math"
	"os"
	"strconv"
	"strings"
	"testing"
	"unicode/utf8"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

const errorGoldenFile = "testdata/errors.txt"

// Host maps can retain colliding key types or supply a different member order.
type goldenMap struct{ pairs []*lisp.LVal }

func (m goldenMap) Len() int                            { return len(m.pairs) }
func (goldenMap) Get(*lisp.LVal) (*lisp.LVal, bool)     { return lisp.Nil(), false }
func (goldenMap) Set(*lisp.LVal, *lisp.LVal) *lisp.LVal { return lisp.Errorf("read-only") }
func (goldenMap) Del(*lisp.LVal) *lisp.LVal             { return lisp.Errorf("read-only") }
func (goldenMap) Keys() *lisp.LVal                      { return list() }
func (m goldenMap) Entries(buf []*lisp.LVal) *lisp.LVal {
	copy(buf, m.pairs)
	return lisp.Int(len(m.pairs))
}

func hostMap(keys ...*lisp.LVal) *lisp.LVal {
	pairs := make([]*lisp.LVal, len(keys))
	for i, k := range keys {
		pairs[i] = list(k, lisp.Int(i))
	}
	return lisp.SortedMapFromData(lisp.NewMapData(goldenMap{pairs: pairs}))
}

func errorCorpus() []struct {
	name string
	v    *lisp.LVal
	opts []libjson.TypedOption
} {
	cycle := lisp.Vector([]*lisp.LVal{nil})
	cycle.Cells[1].Cells[0] = cycle
	return []struct {
		name string
		v    *lisp.LVal
		opts []libjson.TypedOption
	}{
		{"value-tilde", s("~text"), nil},
		{"symbol-tilde", sym("~name"), nil},
		{"key-tilde", smap(s("~key"), lisp.Int(1)), nil},
		{"int-positive-overflow", big("9007199254740993"), nil},
		{"int-negative-overflow", big("-9007199254740993"), nil},
		{"float-nan", f(math.NaN()), nil},
		{"float-inf", f(math.Inf(1)), nil},
		{"float-neg-inf", f(math.Inf(-1)), nil},
		{"float-negative-zero", f(math.Copysign(0, -1)), nil},
		{"float-whole-out-of-range", f(1<<53 + 2), nil},
		{"value-invalid-utf8", s("\xff"), nil},
		{"key-invalid-utf8", smap(s("\xff"), lisp.Int(1)), nil},
		{"value-surrogate", s("\xed\xa0\x80"), nil},
		{"key-surrogate", smap(s("\xed\xa0\x80"), lisp.Int(1)), nil},
		{"int-key", smap(lisp.Int(9), lisp.Int(1)), nil},
		{"mixed-key-collision", hostMap(s("a"), sym("a")), nil},
		{"mixed-key-order", hostMap(sym("z"), s("a")), nil},
		{"unsupported-key", hostMap(f(1.5)), nil},
		{"depth", lisp.Vector([]*lisp.LVal{lisp.Vector(nil)}), []libjson.TypedOption{libjson.WithTypedMaxDepth(1)}},
		{"cycle", cycle, nil},
		{"unsupported", lisp.Fun("f", lisp.Formals(), nil), nil},
		{"limit-bytes", s("text"), []libjson.TypedOption{libjson.WithTypedMaxBytes(2)}},
		{"limit-values", lisp.Vector([]*lisp.LVal{lisp.Int(1)}), []libjson.TypedOption{libjson.WithTypedMaxValues(1)}},
	}
}

// Keep coverage requirements explicit so deleting a fixture and its golden
// together cannot silently drop a rejection or a realistic payload.
func TestCanonicalGoldenCoverage(t *testing.T) {
	plain, typed, failures := readGolden(t, canonicalGoldenFile), readGolden(t, goldenFile), readGolden(t, errorGoldenFile)
	for _, name := range []string{"order-records", "nested-records", "map-utf8-order", "int-2^53", "int-neg-2^53", "float-zero"} {
		if _, ok := plain[name]; !ok {
			t.Errorf("required canonical entry %s missing", name)
		}
		if _, ok := typed[name]; !ok {
			t.Errorf("required typed entry %s missing", name)
		}
	}
	for _, name := range []string{
		"value-tilde", "symbol-tilde", "key-tilde", "int-positive-overflow", "int-negative-overflow",
		"float-nan", "float-inf", "float-neg-inf", "float-negative-zero", "float-whole-out-of-range",
		"value-invalid-utf8", "key-invalid-utf8", "value-surrogate", "key-surrogate",
		"int-key", "mixed-key-collision", "mixed-key-order", "unsupported-key", "depth", "cycle",
		"unsupported", "limit-bytes", "limit-values",
	} {
		if _, ok := failures[name]; !ok {
			t.Errorf("required rejection entry %s missing", name)
		}
	}
	for _, c := range corpus() {
		if c.name != "order-records" && c.name != "nested-records" {
			continue
		}
		var maps, vectors, floats int
		var unicode, html bool
		var walk func(*lisp.LVal)
		walk = func(v *lisp.LVal) {
			switch v.Type {
			case lisp.LSortMap:
				maps++
				for _, pair := range v.MapEntries().Cells {
					if pair.Cells[0].Type != lisp.LString {
						t.Errorf("%s: realistic payload must have string keys", c.name)
					}
					walk(pair.Cells[1])
				}
			case lisp.LArray:
				vectors++
				for _, cell := range v.Cells[1].Cells {
					walk(cell)
				}
			case lisp.LFloat:
				floats++
			case lisp.LString:
				for _, r := range v.Str {
					unicode = unicode || r >= utf8.RuneSelf
				}
				html = html || strings.Contains(v.Str, "<>&")
			default: // Other plain JSON leaves (ints, booleans and null) are allowed.
			}
		}
		walk(c.v)
		if maps < 3 || vectors < 2 || floats == 0 || !unicode || !html {
			t.Errorf("%s lacks nested maps/vectors, floats, Unicode or <>& text", c.name)
		}
	}
}

func TestCanonicalErrorGoldenCorpus(t *testing.T) {
	if *update {
		if strconv.IntSize != 64 {
			t.Fatal("regenerate where int is 64 bits")
		}
		var lines []string
		for _, c := range errorCorpus() {
			_, err := libjson.Canonize(c.v, c.opts...)
			if err == nil {
				t.Fatalf("%s: expected rejection", c.name)
			}
			lines = append(lines, c.name+"\t"+err.Error())
		}
		if err := os.WriteFile(errorGoldenFile, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	golden := readGolden(t, errorGoldenFile)
	env := lisp.NewEnv(nil)
	if err := lisp.GoError(lisp.InitializeUserEnv(env)); err != nil {
		t.Fatal(err)
	}
	cs := errorCorpus()
	if len(cs) != len(golden) {
		t.Fatalf("rejection golden has %d entries, corpus has %d", len(golden), len(cs))
	}
	for _, c := range cs {
		want, ok := golden[c.name]
		if !ok {
			t.Errorf("%s: missing from %s", c.name, errorGoldenFile)
			continue
		}
		if c.v == nil { // Wide ints cannot be constructed on 32 bits.
			continue
		}
		v, err := libjson.Canonize(c.v, c.opts...)
		if v != nil || err == nil || err.Error() != want {
			t.Errorf("%s: value %v, error %v; want %s", c.name, v, err, want)
		}
		if len(c.opts) == 0 {
			failure := libjson.CanonizeBuiltin(env, lisp.SExpr([]*lisp.LVal{c.v}))
			if failure.Type != lisp.LError || failure.Str != "json:canonize-error" || len(failure.Cells) != 3 {
				t.Errorf("%s: expected catchable canonize condition, got %s", c.name, failure)
				continue
			}
			message := strings.TrimPrefix(want, "json:canonize: ")
			if failure.Cells[0].Type != lisp.LString || failure.Cells[0].Str != message {
				t.Errorf("%s: Lisp message %s; want %q", c.name, failure.Cells[0], message)
			}
		}
	}
}
