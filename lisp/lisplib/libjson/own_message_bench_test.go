// Copyright © 2018 The ELPS authors

package libjson_test

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// The `json:dump-message` path had no benchmark before elps#412, which is why
// elps#411 could ship a check on it costed only by proxy.  BenchmarkEncode
// prices natives that are Go maps, and BenchmarkEncodeNative* prices an
// embedder's *json.RawMessage; neither is libjson's own output, and neither
// moves when the elps#412 exemption is applied or removed.
//
// This one is the shape the downstream platform actually runs: a JSON-RPC
// envelope whose "result" member is the native `json:dump-message` returned,
// serialized by a second Dump.  That is one re-validated native per response,
// structurally -- not an occasional path.
//
// The three sizes are the ticket's, and the reason there are three is that the
// cost being removed is per-BYTE, not per-call: the check decodes the whole
// message into an interface{} and throws it away, so it allocates in
// proportion to the document.  A single small row would understate it by two
// orders of magnitude and invite the conclusion that elps#412 is noise.
//
// The payload is deliberately ordinary -- strings, ints, floats, nesting -- so
// the row measures the envelope encode and not some pathological document.
func benchOwnMessage(b *testing.B, rows int, wantBytes int) {
	b.Helper()
	env := newJSONEnv(b)
	s := libjson.DefaultSerializer()

	payload := lisp.SortedMap()
	items := make([]*lisp.LVal, rows)
	for i := range rows {
		row := lisp.SortedMap()
		row.MapSet("id", lisp.Int(1000000+i))
		row.MapSet("name", lisp.String(fmt.Sprintf("record number %d", i)))
		row.MapSet("score", lisp.Float(1.5))
		row.MapSet("tags", lisp.QExpr([]*lisp.LVal{
			lisp.String("alpha"), lisp.String("beta"), lisp.String("gamma"),
		}))
		items[i] = row
	}
	payload.MapSet("items", lisp.QExpr(items))
	payload.MapSet("count", lisp.Int(rows))

	msg := s.DumpMessageBuiltin(env, lisp.SExpr([]*lisp.LVal{payload, lisp.Nil()}))
	if msg.Type == lisp.LError {
		b.Fatalf("json:dump-message: %v", msg)
	}
	// The size is pinned, not logged.  These rows only mean anything as a
	// series -- small, medium, large -- and a payload that quietly drifted an
	// order of magnitude would still produce a plausible-looking table.
	size := s.MessageBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{msg}))
	if size.Type == lisp.LError {
		b.Fatalf("json:message-bytes: %v", size)
	}
	if got := len(size.Bytes()); got < wantBytes*9/10 || got > wantBytes*11/10 {
		b.Fatalf("payload is %d bytes, expected about %d: this row no longer "+
			"measures the size class its name claims", got, wantBytes)
	}

	// The envelope shirocore builds around every response.
	envelope := lisp.SortedMap()
	envelope.MapSet("jsonrpc", lisp.String("2.0"))
	envelope.MapSet("id", lisp.Int(1))
	envelope.MapSet("result", msg)

	b.ReportAllocs()
	b.ResetTimer()
	for range b.N {
		if _, err := libjson.Dump(envelope, false); err != nil {
			b.Fatal(err)
		}
	}
}

// BenchmarkEncodeOwnMessageSmall is a response of a few hundred bytes, the
// size the downstream platform's own instrumentation reports today (mean 25 B,
// max 487 B across its whole suite).  It is the row that says whether
// elps#412 is worth anything at current traffic.
func BenchmarkEncodeOwnMessageSmall(b *testing.B) { benchOwnMessage(b, 7, 570) }

// BenchmarkEncodeOwnMessageMedium is roughly 14 KB, the middle row of
// elps#412's table.
func BenchmarkEncodeOwnMessageMedium(b *testing.B) { benchOwnMessage(b, 172, 14600) }

// BenchmarkEncodeOwnMessageLarge is roughly 295 KB, the top row: a phylum that
// has started returning large opaque blobs.  The point of keeping it is that
// the saving must be shown to SCALE, not merely to exist.
//
// Its allocs/op column does not reproduce, and that is a property of the size
// class rather than a defect to fix here.  The true cost is 9.985 allocations
// per operation at the default GOGC -- a fifth of a percent below an integer --
// so the value `go test` prints, int64(mallocs)/int64(b.N), is 9 on one sample
// and 10 on the next.  The fraction is sync.Pool: encoding ~295 KB per
// operation forces a collection every few iterations, runtime poolCleanup drops
// every pool at every GC, and the next Get reallocates the per-P array via
// pin -> pinSlow.  It is charged to whichever operation runs after the
// collection, so it tracks GC cadence.  An alloc-profile diff over 2000 encodes
// puts the whole difference on sync.(*Pool).pinSlow and none of it on a pool
// MISS -- the encoder comes back from the pool essentially every time (0.001
// misses/op) -- so warming the pool before b.ResetTimer does not touch it, and
// nothing survives poolCleanup that could.
//
// The consequence is for the CI gate, not for this file: a 9-count row can only
// express a move of 11.11% or nothing, so cmd/benchgate reports a
// one-allocation step here as QUANTISED rather than as a regression.  See the
// quantisation-check note in that package's doc.  The Small and Medium rows above
// reproduce exactly and stay gateable at one allocation.
func BenchmarkEncodeOwnMessageLarge(b *testing.B) { benchOwnMessage(b, 3410, 295000) }
