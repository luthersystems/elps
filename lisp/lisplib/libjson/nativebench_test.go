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
// BenchmarkEncode pays for the check three times per iteration.
//
// The check is a full DECODE of the marshalled bytes -- jsonDecode, the
// function Load itself decodes with -- and these benchmarks are what price it.
// Against origin/main (00b6c29), 12 interleaved rounds per arm at
// GOMAXPROCS=4, -benchtime 500ms, compared with benchstat:
//
//	                        base (origin/main)     with the check
//	Encode-4                22.76us 10.05KiB 214   +11.65%  +12.44%   +7.94%
//	Encode_stringNumbers-4  3.402us 1.297KiB  30      ~        =        =
//	EncodeNativeSmall-4     461.7ns    208B    3   +319.2%  +307.7%  +466.7%
//	EncodeNativeLarge-4     25.56us 8.113KiB   3   +334.9%  +461.2%  +38533%
//
// Every moved row is p<=0.001 over 12 samples; both stringNumbers cells that
// read "=" are all-samples-equal, and its time row is p=0.551. The allocation
// column is the one to read first: 3 -> 1159 on EncodeNativeLarge is not a
// constant overhead but a count proportional to the DOCUMENT, because decoding
// into an interface{} materialises every value in it only to throw the whole
// thing away. A service passing large opaque blobs around as natives pays GC
// pressure in proportion to its traffic.
//
// That cost was measured, disliked, and then accepted on evidence. A
// hand-rolled byte scan restored allocations to base EXACTLY on all four arms
// and cut the native-encode time overhead to about +12% and +20%. It was
// dropped anyway, because measuring the two implementations END TO END in the
// downstream platform that is libjson's only heavy user -- interleaved, n=12,
// benchstat -- moved none of that platform's 20 benchmark rows: its natives
// are small (177 encodes across the whole suite, mean 25 bytes, largest 487),
// and a per-byte difference on bytes there are few of does not clear the noise
// floor of the gate that would have to defend it. See the comment on
// encoder.checkLoadable in encode.go for the full reasoning, and elps#412 for
// the change that would actually remove this cost instead of shrinking it.
//
// So these benchmarks are not a regression report to be explained away. They
// are the standing price of the invariant, kept measurable so that elps#412 --
// or any future attempt at a cheaper check -- has a number to beat and a place
// to prove it.
//
// The stringNumbers row is flat for a mundane reason, stated here so it is not
// misread as evidence the check is free in that mode:
// stringNumberEncodeTests holds only lisp.Int and lisp.Float rows and contains
// no native at all, so it never reaches encodeNative. The asymmetry is between
// the two TABLES, not between the two modes -- under :string-numbers the check
// still decodes, and still costs, whenever a document actually holds a native.
// UseNumber only changes which numbers it accepts.
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
