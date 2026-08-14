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
// The check shipped first as a full decode of the marshalled bytes, and these
// benchmarks are what showed that it could not stay one. Against origin/main,
// 12 interleaved rounds per arm at GOMAXPROCS=4, -benchtime 500ms, compared
// with benchstat:
//
//	                        base (origin/main)      decode          scan (now)
//	Encode-4                24.65us 10.05KiB 214    +11.6%  +12.4%  ~  =    =
//	Encode_stringNumbers-4  3.630us 1.297KiB  30      ~       =     ~  =    =
//	EncodeNativeSmall-4     490.2ns    208B    3    +280%   +308%  +11.6% = =
//	EncodeNativeLarge-4     26.70us 8.112KiB   3    +316%   +461%  +19.8% = =
//
// The decode column is the figure recorded when the fix landed, measured the
// same way on the same host but in a different session -- treat it as an order
// of magnitude, not as a number comparable digit for digit with the other two.
// The scan column is measured against the base beside it.
//
// What the scan restores exactly is ALLOCATION. Every arm is back to its base
// count with every sample equal: 214, 30, 3 and 3, against the decode's 231,
// 30, 17 and 1159. Bytes per op return to base too (the 0.02% on
// EncodeNativeLarge is two bytes of buffer-growth rounding, not work). That is
// the number that mattered: the decode allocated in proportion to the
// DOCUMENT, so a service passing large opaque blobs around as natives paid GC
// pressure proportional to its traffic.
//
// What it does NOT restore is time, and the two native rows are still
// significantly slower than main: +11.6% (p=0.001) and +19.8% (p=0.000) on 12
// samples. That is not noise and should not be reported as noise. It is also
// not removable: the check has to READ the bytes, and reading them costs a
// pass. The base row for EncodeNativeLarge is very nearly json.Marshal's own
// compaction of the same 4KiB document, which is itself a pass over the bytes
// at roughly 6ns each; this scan adds one at roughly 1.3ns each. Parity with
// main would mean checking the bytes without looking at them.
//
// The two whole-document rows ARE at parity (p=0.319 and p=0.630), which is
// the shape to expect: the check is charged per native encoded and per byte of
// that native, so a document that is 8% native by count barely moves.
//
// The stringNumbers row is flat for a mundane reason, stated here so it is not
// misread as evidence the check is free in that mode:
// stringNumberEncodeTests holds only lisp.Int and lisp.Float rows and contains
// no native at all, so it never reaches encodeNative. The asymmetry is between
// the two TABLES, not between the two modes -- under :string-numbers the check
// still walks the bytes to count nesting, and still costs, whenever a document
// actually holds a native. Only the number half of it is skipped.
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
