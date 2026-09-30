// Copyright © 2026 The ELPS authors

// Package typedgolden_test pins typed JSON output byte for byte across
// machines. CI runs it on linux/arm64, windows/amd64 and windows/386,
// and every run must produce exactly
// testdata/golden.txt.
package typedgolden_test

import (
	"flag"
	"math"
	"os"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

var update = flag.Bool("update", false, "rewrite testdata/golden.txt from the encoder")

const goldenFile = "testdata/golden.txt"
const canonicalGoldenFile = "testdata/canonical.txt"

// big returns an int that needs 64 bits, or nil where int is 32 bits.
func big(s string) *lisp.LVal {
	n, err := strconv.ParseInt(s, 10, strconv.IntSize)
	if err != nil {
		return nil
	}
	return lisp.Int(int(n))
}

func smap(kv ...*lisp.LVal) *lisp.LVal {
	m := lisp.SortedMap()
	for i := 0; i+1 < len(kv); i += 2 {
		if r := m.MapSetLVal(kv[i], kv[i+1]); r.Type == lisp.LError {
			panic(r.Str)
		}
	}
	return m
}

func list(xs ...*lisp.LVal) *lisp.LVal { return lisp.QExpr(xs) }

func ints(xs ...int) []*lisp.LVal {
	out := make([]*lisp.LVal, len(xs))
	for i, x := range xs {
		out[i] = lisp.Int(x)
	}
	return out
}

func f(x float64) *lisp.LVal  { return lisp.Float(x) }
func s(x string) *lisp.LVal   { return lisp.String(x) }
func sym(x string) *lisp.LVal { return lisp.Symbol(x) }

// corpus is the value behind each golden line.  A nil value is a 64-bit-only
// entry skipped where int is 32 bits; its line must then fail to decode.
func corpus() []struct {
	name string
	v    *lisp.LVal
} {
	astral := string(rune(0x1F600))
	var bigMap, bigArray *lisp.LVal
	if n := big("9007199254740992"); n != nil {
		bigMap = smap(n, n, big("-9007199254740992"), big("-9007199254740992"),
			big("9223372036854775807"), big("9223372036854775807"),
			big("-9223372036854775808"), big("-9223372036854775808"))
		bigArray = lisp.Array(list(lisp.Int(0), n), nil)
	}
	return []struct {
		name string
		v    *lisp.LVal
	}{
		{"int-zero", lisp.Int(0)},
		{"int-neg", lisp.Int(-1)},
		{"int-max32", lisp.Int(math.MaxInt32)},
		{"int-min32", lisp.Int(math.MinInt32)},
		{"int-2^31", big("2147483648")},
		{"int-2^53-1", big("9007199254740991")},
		{"int-neg-2^53+1", big("-9007199254740991")},
		{"int-2^53", big("9007199254740992")},
		{"int-neg-2^53", big("-9007199254740992")},
		{"int-2^53+1", big("9007199254740993")},
		{"int-neg-2^53-1", big("-9007199254740993")},
		{"int-max64", big("9223372036854775807")},
		{"int-min64", big("-9223372036854775808")},
		{"float-zero", f(0)},
		{"float-negzero", f(math.Copysign(0, -1))},
		{"float-one", f(1)},
		{"float-hundred", f(100)},
		{"float-0.1", f(0.1)},
		{"float-0.1+0.2", f(0.1 + 0.2)},
		{"float-third", f(1.0 / 3)},
		{"float-pi", f(math.Pi)},
		{"float-min-subnormal", f(5e-324)},
		{"float-neg-min-subnormal", f(-5e-324)},
		{"float-max-subnormal", f(2.225073858507201e-308)},
		{"float-min-normal", f(2.2250738585072014e-308)},
		{"float-max", f(math.MaxFloat64)},
		{"float-1e300", f(1e300)},
		{"float-below-1e21", f(999999999999999900000)},
		{"float-1e21", f(1e21)},
		{"float-2^70", f(1180591620717411303424)},
		{"float-1e-6", f(1e-6)},
		{"float-below-1e-6", f(math.Nextafter(1e-6, 0))},
		{"float-1e-7", f(1e-7)},
		{"float-nan", f(math.NaN())},
		{"float-inf", f(math.Inf(1))},
		{"float-neg-inf", f(math.Inf(-1))},
		{"string-ascii", s("hello world")},
		{"string-escapes", s("q\"b\\n\n\r\t\b\f\x00\x01\x1f\x7f</>&")},
		{"string-line-sep", s("a" + string(rune(0x2028)) + string(rune(0x2029)) + "b")},
		{"string-latin", s("caf" + string(rune(0xe9)))},
		{"string-decomposed", s("cafe" + string(rune(0x301)))},
		{"string-cjk", s(string([]rune{0x6771, 0x4eac}))},
		{"string-astral", s(astral)},
		{"string-bmp-max", s(string(rune(0xffff)))},
		{"string-tilde", s("~x")},
		{"string-caret", s("^ ")},
		{"string-backtick", s("`b")},
		{"string-empty", s("")},
		{"symbol", sym("lisp:set")},
		{"keyword", sym(":pending")},
		{"true", sym("true")},
		{"false", sym("false")},
		{"bytes-empty", lisp.Bytes(nil)},
		{"bytes-1", lisp.Bytes([]byte{0xff})},
		{"bytes-2", lisp.Bytes([]byte{0, 1})},
		{"bytes-3", lisp.Bytes([]byte{0xfb, 0xef, 0xbe})},
		{"nil", list()},
		{"list", list(lisp.Int(1), s("a"), sym(":k"), list())},
		{"vector", lisp.Vector([]*lisp.LVal{lisp.Int(1), f(1)})},
		{"empty-vector", lisp.Vector(nil)},
		{"nested-sequences", lisp.Vector([]*lisp.LVal{list(lisp.Vector(ints(1, 2)), list()), lisp.Vector(nil)})},
		{"array-2x3", lisp.Array(list(ints(2, 3)...), ints(1, 2, 3, 4, 5, 6))},
		{"array-rank0", lisp.Array(list(), ints(7))},
		{"array-zero-big-dim", bigArray},
		{"tagged", &lisp.LVal{Type: lisp.LTaggedVal, Str: "user:point", Cells: []*lisp.LVal{list(ints(1, 2)...)}}},
		{"map-mixed-keys", smap(s("b"), lisp.Int(1), sym("b"+"x"), lisp.Int(2), sym(":b"), lisp.Int(3),
			lisp.Int(-5), lisp.Int(4), lisp.Int(10), lisp.Int(5), sym("true"), lisp.Int(6), sym("false"), lisp.Int(7),
			s("~b"), lisp.Int(8), s("^b"), lisp.Int(9), s(""), lisp.Int(10), s("B"), lisp.Int(11),
			s("`b"), lisp.Int(12))},
		{"map-big-int-keys", bigMap},
		{"map-utf8-order", smap(s("z"), lisp.Int(1), s(string(rune(0xe9))), lisp.Int(2), s(string(rune(0xe000))), lisp.Int(3),
			s(string(rune(0xffff))), lisp.Int(4), s(astral), lisp.Int(5), s("a"), lisp.Int(6), s(string(rune(0x10000))), lisp.Int(7))},
		{"map-escaped-key-order", smap(s("Z"), lisp.Int(3), s("<"), lisp.Int(2), s("&"), lisp.Int(1),
			s("\\"), lisp.Int(4), s("a"), lisp.Int(5), s(string(rune(0x2028))), lisp.Int(6))},
		{"symbol-escapes", sym("x<>&" + string(rune(0x2028)) + string(rune(0x2029)))},
		{"keyword-escapes", sym(":x<>&" + string(rune(0x2028)) + string(rune(0x2029)))},
		{"tagged-name-escapes", &lisp.LVal{Type: lisp.LTaggedVal, Str: "user:<>&" + string(rune(0x2028)), Cells: []*lisp.LVal{lisp.Int(1)}}},
		{"map-nested", smap(sym("state"), smap(s("items"), list(smap(sym("sku"), s("A-1"), sym("qty"), lisp.Int(2))),
			s("total"), f(12.5)), sym("flags"), lisp.Vector([]*lisp.LVal{sym("true"), sym(":x")}))},
	}
}

func encodeCorpus(t *testing.T) []string {
	t.Helper()
	var lines []string
	for _, c := range corpus() {
		if c.v == nil {
			lines = append(lines, c.name+"\t")
			continue
		}
		b, err := libjson.DumpWith(c.v, libjson.DumpOpts{Typed: true})
		if err != nil {
			t.Fatalf("%s: %v", c.name, err)
		}
		lines = append(lines, c.name+"\t"+string(b))
	}
	return lines
}

func readGolden(t *testing.T, file string) map[string]string {
	t.Helper()
	b, err := os.ReadFile(file) //nolint:gosec // fixed repository golden paths supplied by tests
	if err != nil {
		t.Fatal(err)
	}
	golden := map[string]string{}
	for _, line := range strings.Split(strings.TrimSuffix(string(b), "\n"), "\n") {
		name, doc, ok := strings.Cut(line, "\t")
		if !ok {
			t.Fatalf("malformed golden line %q", line)
		}
		golden[name] = doc
	}
	return golden
}

func TestTypedGoldenCorpus(t *testing.T) {
	if *update {
		if strconv.IntSize != 64 {
			t.Fatal("regenerate the golden file where int is 64 bits")
		}
		if err := os.WriteFile(goldenFile, []byte(strings.Join(encodeCorpus(t), "\n")+"\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	golden := readGolden(t, goldenFile)
	cs := corpus()
	if len(golden) != len(cs) {
		t.Fatalf("golden file has %d entries, corpus has %d", len(golden), len(cs))
	}
	for _, c := range cs {
		want, ok := golden[c.name]
		if !ok {
			t.Errorf("%s: missing from %s", c.name, goldenFile)
			continue
		}
		if c.v == nil {
			// A 64-bit int on a 32-bit int platform: decoding must fail
			// loudly, never truncate.
			if _, err := loadTyped([]byte(want)); err == nil {
				t.Errorf("%s: %s decoded where int is %d bits", c.name, want, strconv.IntSize)
			}
			continue
		}
		got, err := libjson.DumpWith(c.v, libjson.DumpOpts{Typed: true})
		if err != nil {
			t.Errorf("%s: %v", c.name, err)
			continue
		}
		if string(got) != want {
			t.Errorf("%s:\n got %s\nwant %s", c.name, got, want)
		}
		back, err := loadTyped([]byte(want))
		if err != nil {
			t.Errorf("%s: golden does not decode: %v", c.name, err)
			continue
		}
		if again, err := libjson.DumpWith(back, libjson.DumpOpts{Typed: true}); err != nil || string(again) != want {
			t.Errorf("%s: golden does not round-trip: %s (%v)", c.name, again, err)
		}
	}
}

// Canonical images reuse every successful case of the typed corpus, so the
// adoption guarantee is pinned for symbols, lists, bytes, tags and maps too.
func TestCanonicalGoldenCorpus(t *testing.T) {
	if *update {
		if strconv.IntSize != 64 {
			t.Fatal("regenerate where int is 64 bits")
		}
		var lines []string
		for _, c := range corpus() {
			v, err := libjson.Canonize(c.v)
			if err != nil {
				continue
			}
			b, err := libjson.Dump(v, false)
			if err != nil {
				t.Fatal(err)
			}
			lines = append(lines, c.name+"\t"+string(b))
		}
		if err := os.WriteFile(canonicalGoldenFile, []byte(strings.Join(lines, "\n")+"\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	golden := readGolden(t, canonicalGoldenFile)
	checked := 0
	for _, c := range corpus() {
		want, ok := golden[c.name]
		if !ok {
			continue
		}
		checked++
		if c.v == nil {
			if _, err := loadTyped([]byte(want)); err == nil {
				t.Errorf("%s: wide canonical integer decoded on 32 bits", c.name)
			}
			continue
		}
		plain, err := libjson.DumpWith(c.v, libjson.DumpOpts{Canonize: true})
		if err != nil {
			t.Fatal(err)
		}
		typed, err := libjson.DumpWith(c.v, libjson.DumpOpts{Canonize: true, Typed: true})
		if err != nil {
			t.Fatal(err)
		}
		original, err := libjson.Dump(c.v, false)
		if err != nil {
			t.Fatal(err)
		}
		if string(plain) != want || string(typed) != want || string(original) != want {
			t.Errorf("%s: canonical %s, typed %s, original %s; want %s", c.name, plain, typed, original, want)
		}
		back := libjson.LoadWith(plain, libjson.LoadOpts{ExactIntegers: true})
		typedBack, err := loadTyped(plain)
		if err != nil {
			t.Fatalf("%s: canonical bytes do not typed-decode: %v", c.name, err)
		}
		typedAgain, err := libjson.DumpWith(typedBack, libjson.DumpOpts{Typed: true})
		if err != nil || string(typedAgain) != want {
			t.Errorf("%s: typed decode differs: %s (%v)", c.name, typedAgain, err)
		}
		again, err := libjson.DumpWith(back, libjson.DumpOpts{Typed: true})
		if err != nil || string(again) != want {
			t.Errorf("%s: exact plain decode differs: %s (%v)", c.name, again, err)
		}
	}
	if checked != len(golden) {
		t.Fatal("canonical golden contains unknown corpus cases")
	}
}

// Exercise the option API while preserving the golden harness's Go error checks.
func loadTyped(b []byte) (*lisp.LVal, error) {
	v := libjson.LoadWith(b, libjson.LoadOpts{Typed: true})
	if err := lisp.GoError(v); err != nil {
		return nil, err
	}
	return v, nil
}
