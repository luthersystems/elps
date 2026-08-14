// Copyright © 2018 The ELPS authors

package libjson_test

import (
	"encoding/json"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Encoding a native had no benchmark of its own before elps#410. These two
// isolate the validation cost at both ends of the size range it scales with,
// because the existing suite measures it only incidentally and understates it.
//
// The existing suite does NOT measure zero, and an earlier version of this
// comment claiming BenchmarkEncode "never reaches encodeNative at all" was
// simply wrong. Three of the 36 rows in stdEncodeTests are natives --
// lisp.Value on a map[string]interface{} falls through Value's type switch to
// lisp.Native, a map not being one of the kinds it converts -- so
// BenchmarkEncode pays for three validating decodes per iteration and moves
// measurably:
//
//	                         base (origin/main)          with the check
//	Encode-4                 19.22µs  10.05KiB   214     21.45µs  11.30KiB   231
//	Encode_stringNumbers-4   2.660µs  1.297KiB    30     2.699µs  1.297KiB    30
//	EncodeNativeSmall-4      412.3ns     208B      3     1.565µs     848B     17
//	EncodeNativeLarge-4      24.37µs  8.112KiB     3     101.4µs  45.53KiB  1159
//
// 10 interleaved rounds per arm, GOMAXPROCS=4, base = origin/main, compared
// with benchstat. Encode is +12.4% bytes and +7.9% allocs (p<0.001, and both
// exact -- every sample identical, since allocation counts here are
// deterministic) and +11.6% time. Treat the time figure as the soft one: it
// measured +8.4% on an earlier run of the same comparison on the same host,
// while the byte and alloc deltas reproduced exactly. The stringNumbers row
// does not move on any metric (p=0.27 on time, byte-identical otherwise).
//
// The stringNumbers row is flat for a mundane reason, stated here so it is not
// misread as evidence the check is free in that mode: stringNumberEncodeTests
// holds only lisp.Int and lisp.Float rows and contains no native at all, so it
// never reaches encodeNative. The asymmetry is between the two TABLES, not
// between the two modes -- under :string-numbers the check still runs, and
// still costs, whenever a document actually holds a native.
//
// Read the Encode row as the cost for a document that is ~8% native by count,
// not as a fleet-wide 8%: the check is charged per native encoded, and a
// document holding no native pays nothing because nothing else reaches
// encodeNative.
func benchNative(b *testing.B, src string) {
	b.Helper()
	raw := json.RawMessage(src)
	v := lisp.Native(&raw)
	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if _, err := libjson.Dump(v, false); err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkEncodeNativeSmall(b *testing.B) {
	benchNative(b, `{"a":1,"b":[2,3],"c":"str"}`)
}

func BenchmarkEncodeNativeLarge(b *testing.B) {
	var sb strings.Builder
	sb.WriteString(`{"users":[`)
	for i := range 60 {
		if i > 0 {
			sb.WriteByte(',')
		}
		sb.WriteString(`{"id":12345,"name":"a name here","tags":["x","y","z"],"score":1.5}`)
	}
	sb.WriteString(`]}`)
	benchNative(b, sb.String())
}
