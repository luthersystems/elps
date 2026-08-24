package libjson

import (
	"crypto/rand"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	mathrand "math/rand"
	"reflect"
	"slices"
	"strings"
	"sync"
	"testing"
	"unsafe"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// this function is internal because users are not supposed to construct
// literal SortedMap values in their applications =\
func literalSortedMap(m SortedMap) *lisp.LVal {
	return lisp.SortedMapFromData(lisp.NewMapData(m))
}

type encodeTest struct {
	v  *lisp.LVal
	js string
}

var stdEncodeTests = []encodeTest{
	{lisp.String(""), `""`},
	{lisp.String("\t"), `"\t"`},
	{lisp.String("\x05"), `"\u0005"`},
	{lisp.String("a\u2028\u2029"), `"a\u2028\u2029"`},
	{lisp.String("a&<>"), `"a\u0026\u003c\u003e"`},
	{lisp.String("hello"), `"hello"`},
	{lisp.String("🤷🏽\u200d♀️"), `"🤷🏽‍♀️"`}, // Woman Shrugging: Medium Skin Tone
	{lisp.Symbol(""), `""`},
	{lisp.Symbol("hello"), `"hello"`},
	{lisp.Symbol(lisp.TrueSymbol), `true`},
	{lisp.Symbol(lisp.FalseSymbol), `false`},
	{lisp.Nil(), `null`},
	{lisp.SExpr(nil), `null`},
	{lisp.QExpr(nil), `null`},
	{lisp.Int(0), `0`},
	{lisp.Int(1 << 60), `1152921504606846976`},
	{lisp.Int(-(1 << 60)), `-1152921504606846976`},
	{lisp.Float(0), `0`},
	{lisp.Float(-1.5e-7), `-1.5e-7`},
	{lisp.Float(1.125e21), `1.125e+21`},
	{lisp.Bytes([]byte("hello")), `"aGVsbG8="`},
	{lisp.Bytes([]byte{}), `""`},
	{lisp.Bytes([]byte(nil)), `null`}, // backwards compat with v1.13.0
	{
		// overflow the encoder's internal buffer
		lisp.Bytes([]byte("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")),
		`"TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdC4="`,
	},
	{lisp.SortedMap(), `{}`},
	{lisp.Value(map[string]interface{}{}), `{}`},
	{
		lisp.Value(map[string]interface{}{"a": "1"}),
		`{"a":"1"}`,
	},
	{
		lisp.Value(map[string]interface{}{"a": "1", "b": "2"}),
		`{"a":"1","b":"2"}`,
	},
	{literalSortedMap(SortedMap{}), `{}`},
	{literalSortedMap(nil), `{}`},
	{
		literalSortedMap(SortedMap{"a": lisp.String("1")}),
		`{"a":"1"}`,
	},
	{
		literalSortedMap(SortedMap{"a": lisp.String("1"), "b": lisp.String("2")}),
		`{"a":"1","b":"2"}`,
	},
	{lisp.Vector(nil), `[]`},
	{lisp.Vector([]*lisp.LVal{}), `[]`},
	{lisp.Vector([]*lisp.LVal{lisp.String("a")}), `["a"]`},
	{lisp.Vector([]*lisp.LVal{lisp.Float(1.5e-10), lisp.Int(100)}), `[1.5e-10,100]`},
}

var stringNumberEncodeTests = []encodeTest{
	{lisp.Int(0), `"0"`},
	{lisp.Int(1 << 60), `"1152921504606846976"`},
	{lisp.Int(-(1 << 60)), `"-1152921504606846976"`},
	{lisp.Float(0), `"0"`},
	{lisp.Float(-1.5e-7), `"-1.5e-7"`},
	{lisp.Float(1.125e21), `"1.125e+21"`},
}

func testEncode(t testing.TB) {
	for i, test := range stdEncodeTests {
		enc := getEncoder(false)
		if assert.NoError(t, enc.encode(test.v), "test %d: %v", i, test.v) {
			js := string(enc.bytes())
			assert.Equal(t, test.js, js, "test %d: %v", i, test.v)
		}
		putEncoder(enc)
	}
}

func testEncode_stringNumbers(t testing.TB) {
	for i, test := range stringNumberEncodeTests {
		enc := getEncoder(true)
		if assert.NoError(t, enc.encode(test.v), "test %d: %v", i, test.v) {
			js := string(enc.bytes())
			assert.Equal(t, test.js, js, "test %d: %v", i, test.v)
		}
		putEncoder(enc)
	}
}

func TestEncode(t *testing.T)               { testEncode(t) }
func TestEncode_stringNumbers(t *testing.T) { testEncode_stringNumbers(t) }

// unencodableTypes lists every lisp.LType the JSON encoder deliberately
// refuses.  Anything not listed here must have an entry in encoderFuncs.
var unencodableTypes = map[lisp.LType]string{
	lisp.LInvalid:       "not a real type",
	lisp.LError:         "errors are a control-flow value, not data",
	lisp.LQSymbol:       "legacy type with no JSON meaning",
	lisp.LFun:           "functions have no serialized form",
	lisp.LMarkTerminal:  "interpreter-internal marker",
	lisp.LMarkTailRec:   "interpreter-internal marker",
	lisp.LMarkMacExpand: "interpreter-internal marker",
}

// TestEncoderTypeCoverage is a drift guard on the JSON serialization surface.
//
// The encoder dispatches on the encoderFuncs table rather than a switch, so
// the exhaustive linter cannot see it: a newly added lisp.LType would get a
// nil table entry with nothing flagging it at build time.  Downstream this
// output is chaincode state, so a type that quietly serialized to nothing
// would be written to a ledger.  This test forces the choice -- register an
// encoder, or record here why the type has no JSON form -- and pins the
// runtime behaviour for the refused types (an error, never empty output).
func TestEncoderTypeCoverage(t *testing.T) {
	for typ := lisp.LInvalid; typ < lisp.LTypeMax; typ++ {
		reason, refused := unencodableTypes[typ]
		hasFunc := encoderFuncs[typ] != nil
		switch {
		case refused && hasFunc:
			t.Errorf("LType %v is listed as unencodable (%s) but has an encoder", typ, reason)
		case !refused && !hasFunc:
			t.Errorf("LType %v has no encoder and is not listed in unencodableTypes: "+
				"register an encoder in encode.go or add it to the list with a reason", typ)
		}
	}
}

// TestEncodeUnregisteredTypeErrors proves the refused types fail loudly.
func TestEncodeUnregisteredTypeErrors(t *testing.T) {
	for typ, reason := range unencodableTypes {
		enc := getEncoder(false)
		err := enc.encode(&lisp.LVal{Type: typ})
		require.Error(t, err, "encoding %v (%s) must fail", typ, reason)
		assert.Contains(t, err.Error(), "invalid type encountered")
		assert.Empty(t, enc.bytes(), "a refused type must not write partial output")
	}
}

func BenchmarkEncode(b *testing.B) {
	for range b.N {
		testEncode(b)
	}
}

func BenchmarkEncode_stringNumbers(b *testing.B) {
	for range b.N {
		testEncode_stringNumbers(b)
	}
}

func TestEncode_largeBytes(t *testing.T) {
	data := make([]byte, 4096)
	_, err := io.ReadFull(rand.Reader, data)
	require.NoError(t, err)
	enc := getEncoder(false)
	require.NoError(t, enc.encode(lisp.Bytes(data)))
	var s string
	err = json.Unmarshal(enc.bytes(), &s)
	require.NoError(t, err)
	decoded, err := base64.StdEncoding.DecodeString(s)
	require.NoError(t, err)
	require.Equal(t, data, decoded)
}

func TestEncoded_stringCompat(t *testing.T) {
	rand := mathrand.New(mathrand.NewSource(1234)) //#nosec G404
	for range 100 {
		s := string(randBytes(rand, 1024))
		canonical, err := json.Marshal(s)
		if !assert.NoError(t, err, "canonical encoding of %q", s) {
			continue
		}
		enc := getEncoder(true)
		if assert.NoError(t, enc.encode(lisp.String(s)), "elps encoding of %q", s) {
			assert.Equal(t, enc.bytes(), canonical)
		}
	}
}

func randBytes(r *mathrand.Rand, maxLen int) []byte {
	n := r.Intn(maxLen)
	b := make([]byte, n)
	for i := range b {
		b[i] = byte(r.Intn(256))
	}
	return b
}

func TestLoadMaxAllocArray(t *testing.T) {
	s := DefaultSerializer()

	t.Run("exceeds", func(t *testing.T) {
		// JSON array with 5 elements, limit 3.
		result := s.LoadMax([]byte(`[1,2,3,4,5]`), false, 3)
		require.Equal(t, lisp.LError, result.Type, "expected error, got: %v", result)
		assert.Contains(t, result.String(), "allocation size 5 exceeds maximum (3)")
	})

	t.Run("within limit", func(t *testing.T) {
		result := s.LoadMax([]byte(`[1,2,3]`), false, 10)
		require.NotEqual(t, lisp.LError, result.Type, "unexpected error: %v", result)
		assert.Equal(t, 3, result.Len())
	})
}

func TestLoadMaxAllocMap(t *testing.T) {
	s := DefaultSerializer()

	t.Run("exceeds", func(t *testing.T) {
		// JSON object with 3 keys, limit 2.
		result := s.LoadMax([]byte(`{"a":1,"b":2,"c":3}`), false, 2)
		require.Equal(t, lisp.LError, result.Type, "expected error, got: %v", result)
		assert.Contains(t, result.String(), "allocation size 3 exceeds maximum (2)")
	})

	t.Run("within limit", func(t *testing.T) {
		result := s.LoadMax([]byte(`{"a":1,"b":2}`), false, 10)
		require.NotEqual(t, lisp.LError, result.Type, "unexpected error: %v", result)
		assert.Equal(t, 2, result.Len())
	})
}

// cyclicMap returns a sorted-map holding itself under every key in keys, which
// is what (set 'm (sorted-map)) (assoc! m "k" m) builds.  See issue #390.
func cyclicMap(keys ...string) *lisp.LVal {
	m := lisp.SortedMap()
	for _, k := range keys {
		m.MapSet(k, m)
	}
	return m
}

// nestMaps returns depth maps nested one inside the next under key "k", with
// inner at the bottom.
func nestMaps(depth int, inner *lisp.LVal) *lisp.LVal {
	v := inner
	for range depth {
		m := lisp.SortedMap()
		m.MapSet("k", v)
		v = m
	}
	return v
}

// TestEncodeCyclicValueErrors pins the answer for a value that contains
// itself.  JSON has no representation for one, so the encoder refuses: an
// error the builtins turn into an ordinary elps condition, rather than a
// truncated document or -- as before the guard -- a stack overflow that kills
// the host process.  See issue #390.
func TestEncodeCyclicValueErrors(t *testing.T) {
	tests := []struct {
		name string
		v    *lisp.LVal
	}{
		{"map holding itself", cyclicMap("k")},
		{"map holding itself twice", cyclicMap("a", "b")},
		{"cycle below an acyclic root", nestMaps(3, cyclicMap("k"))},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			enc := getEncoder(false)
			err := enc.encode(test.v)
			require.ErrorIs(t, err, errCyclicValue)

			b, err := Dump(test.v, false)
			require.ErrorIs(t, err, errCyclicValue, "the error must reach the builtins")
			assert.Empty(t, b, "a refused value must not produce a partial document")
		})
	}
}

// TestEncoderFitsItsSizeClass is the guard on the regression the CI benchmark
// gate caught: adding two fields to the encoder for cycle tracking took it
// from 112 bytes to 120, which the allocator rounds to the 128-byte size
// class.  The encoder is heap-allocated once per document, so that charged 16
// bytes to every json:dump-* call in the process -- +5.6% of BenchmarkEncode's
// bytes, +7.2% of BenchmarkEncode_stringNumbers', with the allocation *count*
// unchanged, which is why an allocs-only assertion would not have seen it.
//
// The guard lives in encodeGuard, on the stack, for this reason.  A field
// added here is not free even when it is nil.
//
// Red-proof: adding `path map[*lisp.LVal]struct{}` and `depth int` back to
// encoder fails this at 128.
func TestEncoderFitsItsSizeClass(t *testing.T) {
	// The size class the encoder occupied before the cycle guard existed.
	// Growing past it is a cost paid by every document, so the field has to
	// earn it; shrinking below it is free to update this number downward.
	const sizeClass = 112
	assert.LessOrEqual(t, int(unsafe.Sizeof(encoder{})), sizeClass,
		"the encoder no longer fits the %d-byte size class, so every json:dump-* "+
			"in the process now allocates a larger block", sizeClass)
}

// TestEncodeAcyclicValueIsUnchangedBelowAndAboveTheGuard pins the property the
// guard exists to preserve: it is invisible to any value that is not cyclic,
// at any depth.
func TestEncodeAcyclicValueIsUnchangedBelowAndAboveTheGuard(t *testing.T) {
	for _, depth := range []int{1, encodeGuardDepth - 1, encodeGuardDepth, encodeGuardDepth + 1, 4 * encodeGuardDepth} {
		t.Run(fmt.Sprintf("depth=%d", depth), func(t *testing.T) {
			want := strings.Repeat(`{"k":`, depth) + "1" + strings.Repeat("}", depth)
			b, err := Dump(nestMaps(depth, lisp.Int(1)), false)
			require.NoError(t, err)
			assert.Equal(t, want, string(b))
		})
	}

	// Shared substructure is a DAG, not a cycle: both occurrences must
	// serialize in full even though the second reaches a value the encoder has
	// already written.
	shared := lisp.SortedMap()
	shared.MapSet("a", lisp.Int(1))
	dag := nestMaps(2*encodeGuardDepth, lisp.SExpr([]*lisp.LVal{shared, shared}))
	want := strings.Repeat(`{"k":`, 2*encodeGuardDepth) + `[{"a":1},{"a":1}]` + strings.Repeat("}", 2*encodeGuardDepth)
	b, err := Dump(dag, false)
	require.NoError(t, err)
	assert.Equal(t, want, string(b))
}

// TestPooledEncoderCarriesNoStateBetweenDocuments is the guard on the hazard
// pooling introduced (issue #379, item 6).
//
// Before the pool every encode started from a zero encoder, so no field could
// be stale by construction.  Now an encoder is handed back and reused, and
// every one of its fields means something about ONE document: stringNums
// decides how numbers are written, and nestedDeep and wroteNative are the two
// inputs to loadableBytes, which is what `json:dump-message` records as its
// loadability verdict.  A field that survived into the next document would
// either change that document's bytes or attach the previous document's
// verdict to it -- and the second is silent, because a message wrongly marked
// unloadable still serializes correctly, just through the check elps#412
// exists to skip.
//
// Red-proof: deleting any one of the four resets in getEncoder fails a case
// below.
func TestPooledEncoderCarriesNoStateBetweenDocuments(t *testing.T) {
	// A recycled encoder must be indistinguishable from a fresh one, so each
	// case dirties an encoder, returns it, and then asserts the next encode
	// looks exactly like the same encode on a pool that was never used.
	dirty := func(prep func(enc *encoder)) {
		enc := getEncoder(true)
		prep(enc)
		putEncoder(enc)
	}

	t.Run("stringNums", func(t *testing.T) {
		dirty(func(enc *encoder) {
			require.NoError(t, enc.encode(lisp.Int(1)))
			require.Equal(t, `"1"`, string(enc.bytes()), "test setup: not string numbers")
		})
		b, err := Dump(lisp.Int(1), false)
		require.NoError(t, err)
		assert.Equal(t, "1", string(b), "the previous encode's number format leaked")
	})

	t.Run("wroteNative", func(t *testing.T) {
		raw := json.RawMessage(`{"a":1}`)
		dirty(func(enc *encoder) {
			require.NoError(t, enc.encode(lisp.Native(&raw)))
			require.False(t, enc.loadableBytes(), "test setup: native not recorded")
		})
		enc := getEncoder(false)
		defer putEncoder(enc)
		require.NoError(t, enc.encode(lisp.Int(1)))
		assert.True(t, enc.loadableBytes(), "the previous encode's native leaked")
	})

	t.Run("nestedDeep", func(t *testing.T) {
		dirty(func(enc *encoder) {
			require.NoError(t, enc.encode(nestMaps(encodeGuardDepth+1, lisp.Int(1))))
			require.False(t, enc.loadableBytes(), "test setup: depth not recorded")
		})
		enc := getEncoder(false)
		defer putEncoder(enc)
		require.NoError(t, enc.encode(lisp.Int(1)))
		assert.True(t, enc.loadableBytes(), "the previous encode's depth leaked")
	})

	t.Run("buffer", func(t *testing.T) {
		dirty(func(enc *encoder) {
			require.NoError(t, enc.encode(lisp.String("a document that came first")))
		})
		b, err := Dump(lisp.Int(1), false)
		require.NoError(t, err)
		assert.Equal(t, "1", string(b), "the previous document's bytes leaked")
	})
}

// TestEncoderFieldsAreAccountedForInGetEncoder pins the encoder's field set,
// so that adding a field to it cannot be a silent decision.
//
// getEncoder is the whole of what stands between a recycled encoder and the
// document it is about to write: every field that says something about ONE
// document has to be put back to its start-of-document value there, or the
// previous document's state carries into the next one.
// TestPooledEncoderCarriesNoStateBetweenDocuments asserts that property for
// the fields that exist today, and it is structurally unable to assert it for
// a field that does not exist yet -- a field added to the struct and forgotten
// in getEncoder leaks across documents with every test in this package still
// green.
//
// So this test does the one thing a test can do about a field nobody has
// written yet: it makes the struct's shape something the author has to come
// here and change, with the reset rule in front of them when they do.
//
// "Reset it in getEncoder" is not the only acceptable answer.  scratch is not
// reset and does not need to be, because every use site writes the bytes it
// reads before it reads them.  That is an argument rather than an invariant
// the compiler checks, and the point of this test is that it gets made for a
// new field instead of assumed.
func TestEncoderFieldsAreAccountedForInGetEncoder(t *testing.T) {
	// Every field of encoder, in declaration order, as of the last time
	// someone checked the struct against getEncoder.  The trailing note on
	// each is how that field is accounted for.
	known := []string{
		"buf",         // Reset() in getEncoder
		"scratch",     // not reset: written before read at every use site
		"stringNums",  // reset in getEncoder
		"nestedDeep",  // reset in getEncoder
		"wroteNative", // reset in getEncoder
	}

	typ := reflect.TypeOf(encoder{})
	got := make([]string, typ.NumField())
	for i := range got {
		got[i] = typ.Field(i).Name
	}
	if !slices.Equal(got, known) {
		t.Fatalf("the encoder's fields changed:\n"+
			"  was: %v\n"+
			"  now: %v\n"+
			"Do not just update the list above to match. First check the new field "+
			"set against getEncoder: every field that means something about a single "+
			"document must be reset there, or a pooled encoder carries the previous "+
			"document's state into the next one -- silently, because a stale flag "+
			"changes a verdict rather than crashing anything. A field left out of "+
			"getEncoder needs the argument scratch has, that every use site writes "+
			"what it reads before reading it; buffer-like fields are exactly where "+
			"that argument is easiest to get wrong. Then extend the list, with a "+
			"note saying which of the two applies.",
			known, got)
	}
}

// TestDumpDoesNotAliasAPooledBuffer pins the property that makes pooling safe
// on the paths whose bytes escape: two documents dumped in sequence must not
// hand back slices that share storage, or writing the second would rewrite
// what the first caller is holding.
func TestDumpDoesNotAliasAPooledBuffer(t *testing.T) {
	first, err := Dump(lisp.String("first document"), false)
	require.NoError(t, err)
	kept := string(first)

	for range 8 {
		_, err := Dump(lisp.String("a completely different document"), false)
		require.NoError(t, err)
	}
	assert.Equal(t, kept, string(first),
		"a later dump wrote through a slice an earlier caller still holds")
}

// TestEncoderPoolConcurrent is the guard the -race build needs in order to say
// anything about the encoder pool.
//
// Every other test in this package is sequential, so before this existed
// `go test -race` exercised the pool with exactly one goroutine -- which
// cannot observe the failure mode a pool actually has.  That failure mode is
// two goroutines holding the same encoder, and it shows up as one document's
// bytes appearing inside another's, not as a crash.
//
// Both escape routes are covered because they differ: Dump DONATES its buffer
// (donateBuffer, so the pooled encoder must be left with none of it) while
// dumpString COPIES into a string (so the buffer is recycled while the caller
// keeps a copy).  A bug in either -- putting an encoder back while its array
// is still reachable by the caller -- lets a later document write over bytes
// someone else is still reading.
//
// Red-proof: deleting the `enc.buf = bytes.Buffer{}` line in donateBuffer
// fails this with corrupted documents under -race.
func TestEncoderPoolConcurrent(t *testing.T) {
	const goroutines = 8
	const iterations = 200

	// Distinct payload per goroutine, each big enough to grow a buffer past
	// its initial size, so a shared buffer corrupts a comparison rather than
	// coincidentally matching.
	payload := func(g int) (*lisp.LVal, string, string) {
		m := lisp.SortedMap()
		var want strings.Builder
		want.WriteByte('{')
		for i := range 24 {
			k := fmt.Sprintf("g%02d-key-%02d", g, i)
			v := fmt.Sprintf("g%02d-value-%02d-padding-padding", g, i)
			m.MapSet(k, lisp.String(v))
			if i > 0 {
				want.WriteByte(',')
			}
			fmt.Fprintf(&want, "%q:%q", k, v)
		}
		want.WriteByte('}')
		return m, want.String(), want.String()
	}

	var wg sync.WaitGroup
	errs := make(chan string, goroutines*2)
	for g := range goroutines {
		wg.Add(1)
		go func(g int) {
			defer wg.Done()
			v, wantBytes, wantString := payload(g)
			for range iterations {
				// The donating path.
				b, err := Dump(v, false)
				if err != nil {
					errs <- fmt.Sprintf("goroutine %d: Dump: %v", g, err)
					return
				}
				if string(b) != wantBytes {
					errs <- fmt.Sprintf("goroutine %d: Dump returned another goroutine's bytes:\n got %s\nwant %s", g, b, wantBytes)
					return
				}
				// The copying path.
				s, err := DefaultSerializer().dumpString(v, false)
				if err != nil {
					errs <- fmt.Sprintf("goroutine %d: dumpString: %v", g, err)
					return
				}
				if s != wantString {
					errs <- fmt.Sprintf("goroutine %d: dumpString returned another goroutine's bytes:\n got %s\nwant %s", g, s, wantString)
					return
				}
			}
		}(g)
	}
	wg.Wait()
	close(errs)
	for msg := range errs {
		t.Error(msg)
	}
}
