// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"fmt"
	"math"
	"math/big"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #350.  encoding/json decodes every JSON number into a float64, and a
// float64 holds 53 bits of integer precision, so json:load silently rounds
// every integer above 2^53.  These tests cover both halves of the fix:
//
//   - the OPT-IN half, LoadOpts.ExactIntegers, under which an integer that
//     fits round-trips exactly and one that does not is a loud error; and
//
//   - the DEFAULT half, which must keep rounding exactly as it always has,
//     because libjson decodes replicated state and two nodes on different
//     elps versions must not disagree about what a document means.
//
// The default-half assertions are not decoration.  They are the thing that
// makes the opt-in an opt-in, and a change that "fixes" them has silently
// widened the blast radius from one call site to every phylum in the fleet.

const (
	maxExactFloat = 1 << 53 // 9007199254740992: the largest 2^k a float64 holds
	int64Max      = math.MaxInt64
	int64Min      = math.MinInt64
)

func exact() libjson.LoadOpts { return libjson.LoadOpts{ExactIntegers: true} }

// TestLoadExactIntegers is the green half of the red-then-green for #350.
func TestLoadExactIntegers(t *testing.T) {
	tests := []struct {
		json string
		want int64
	}{
		{"9007199254740993", maxExactFloat + 1}, // 2^53+1, the value in the issue
		{"9223372036854775807", int64Max},       // int64 max
		{"-9223372036854775808", int64Min},      // int64 min
		{"9007199254740991", maxExactFloat - 1}, // just below 2^53: must be unaffected
		{"9007199254740992", maxExactFloat},     // 2^53 itself
		{"-9007199254740993", -(maxExactFloat + 1)},
		{"0", 0},
		{"1", 1},
		{"-1", -1},
	}
	for _, test := range tests {
		t.Run(test.json, func(t *testing.T) {
			v := libjson.LoadWith([]byte(test.json), exact())
			require.NotEqual(t, lisp.LError, v.Type, "load failed: %s", v)
			require.Equal(t, lisp.LInt, v.Type,
				"loaded %s as %s, not an int", v.String(), lisp.GetType(v).Str)
			assert.Equal(t, test.want, int64(v.Int))
			// Rendering must show the exact digits too.  A value that is
			// right in the machine and wrong on the screen is still wrong in
			// a log, an error message and a hash of a formatted record.
			assert.Equal(t, test.json, v.String())
		})
	}
}

// TestLoadRoundTripIntegers is the round-trip half: load, then dump, then
// assert the bytes are the bytes we started with, across the 2^53 boundary and
// at both int64 extremes.
func TestLoadRoundTripIntegers(t *testing.T) {
	var values []int64
	for delta := int64(-4); delta <= 4; delta++ {
		values = append(values, maxExactFloat+delta, -(maxExactFloat + delta))
	}
	for d := range 5 {
		delta := int64(d)
		values = append(values, int64Max-delta, int64Min+delta)
	}
	values = append(values, 0, 1, -1, 1<<31, 1<<62, -(1 << 62))

	for _, n := range values {
		text := strconv.FormatInt(n, 10)
		t.Run(text, func(t *testing.T) {
			v := libjson.LoadWith([]byte(text), exact())
			require.NotEqual(t, lisp.LError, v.Type, "load failed: %s", v)
			require.Equal(t, lisp.LInt, v.Type)
			require.Equal(t, n, int64(v.Int))

			out, err := libjson.Dump(v, false)
			require.NoError(t, err)
			assert.Equal(t, text, string(out), "round trip was not byte-identical")

			// And again, so a document that is read, written and read once
			// more -- every read-modify-write path there is -- is stable.
			again := libjson.LoadWith(out, exact())
			require.Equal(t, lisp.LInt, again.Type)
			assert.Equal(t, n, int64(again.Int))
		})
	}
}

// TestLoadRoundTripNestedIntegers proves the round trip holds inside the
// containers real documents use, not only for a bare number at top level.
func TestLoadRoundTripNestedIntegers(t *testing.T) {
	docs := []string{
		`[9007199254740993,9223372036854775807,-9223372036854775808]`,
		`{"id":9007199254740993,"n":-9007199254740993}`,
		`{"a":{"b":[{"c":9223372036854775807}]}}`,
		// Keys are emitted in sorted order and non-integer numbers keep
		// decoding as floats, so this is written the way it comes back out.
		`{"big":9223372036854775807,"float":1.5,"neg-zero":-0,"small":1}`,
	}
	for _, doc := range docs {
		t.Run(doc, func(t *testing.T) {
			v := libjson.LoadWith([]byte(doc), exact())
			require.NotEqual(t, lisp.LError, v.Type, "load failed: %s", v)
			out, err := libjson.Dump(v, false)
			require.NoError(t, err)
			assert.Equal(t, doc, string(out))
		})
	}
}

// TestLoadExactIntegerRangeFails pins the loud half of the contract: a value
// that cannot be represented is an error, never a rounded float.
func TestLoadExactIntegerRangeFails(t *testing.T) {
	tests := []string{
		"9223372036854775808",  // int64 max + 1
		"-9223372036854775809", // int64 min - 1
		"123456789012345678901234567890",
		"10000000000000000000000000000000000000000",
	}
	for _, test := range tests {
		t.Run(test, func(t *testing.T) {
			v := libjson.LoadWith([]byte(test), exact())
			require.Equal(t, lisp.LError, v.Type,
				"out-of-range integer loaded as %s (%s) instead of failing",
				lisp.GetType(v).Str, v.String())
			assert.Equal(t, "json:integer-range-error", v.Str,
				"the range failure must be a catchable condition")
			assert.Contains(t, v.String(), test)
		})
	}
}

// TestLoadExactIntegerRangeFailsNested proves the error propagates out of a
// container rather than being swallowed and leaving a hole in the document.
func TestLoadExactIntegerRangeFailsNested(t *testing.T) {
	for _, doc := range []string{
		`[1,2,9223372036854775808]`,
		`{"a":{"b":9223372036854775808}}`,
	} {
		v := libjson.LoadWith([]byte(doc), exact())
		require.Equal(t, lisp.LError, v.Type, "doc %s loaded as %s", doc, v.String())
		assert.Equal(t, "json:integer-range-error", v.Str)
	}
}

// TestLoadExactNonIntegers pins the syntactic rule.  A number written with a
// fraction or an exponent is a float in exact mode exactly as it is by
// default, so turning the option on moves integer literals and nothing else.
func TestLoadExactNonIntegers(t *testing.T) {
	tests := []struct {
		json string
		want float64
	}{
		{"1.5", 1.5},
		{"1.0", 1},
		{"1e2", 100},
		{"1E2", 100},
		{"9007199254740993.0", maxExactFloat}, // written as a float, stays lossy
		{"9223372036854775807e0", float64(int64Max)},
		{"-0", math.Copysign(0, -1)}, // parses to integer 0, but "0" != "-0"
		{"-0.0", math.Copysign(0, -1)},
		{"1e-400", 0}, // underflows to zero without an error, as it does today
	}
	for _, test := range tests {
		t.Run(test.json, func(t *testing.T) {
			v := libjson.LoadWith([]byte(test.json), exact())
			require.Equal(t, lisp.LFloat, v.Type,
				"loaded %s as %s, want a float", test.json, lisp.GetType(v).Str)
			// Bit equality, not epsilon: the point is that the option does not
			// perturb a float by so much as a ulp, and it also settles the
			// sign of zero, which "-0" turns on.
			assert.Equal(t, math.Float64bits(test.want), math.Float64bits(v.Float),
				"got %v, want %v", v.Float, test.want)
		})
	}
}

// TestLoadExactNegativeZeroRoundTrips is the reason "-0" is excluded from the
// integer rule: as the integer 0 it would re-encode as "0".
func TestLoadExactNegativeZeroRoundTrips(t *testing.T) {
	v := libjson.LoadWith([]byte("-0"), exact())
	require.Equal(t, lisp.LFloat, v.Type)
	out, err := libjson.Dump(v, false)
	require.NoError(t, err)
	assert.Equal(t, "-0", string(out))
}

// TestLoadExactAcceptsCanonicalLargeFloats pins the one case where an integer
// literal too large for a lisp int is NOT an error: when the literal is
// already the canonical rendering of the float it parses to, so taking the
// float discards nothing the document was carrying.
//
// This is not a softening of the loud-failure rule, it is what makes the rule
// survivable. This package renders every float in [2^63, 1e21) as plain
// digits, so without this a phylum holding an ordinary float of 1e19 would
// dump its state and then be unable to read it back. "Anything Dump can emit,
// Load can read" has to hold, or the option is a liveness bug.
func TestLoadExactAcceptsCanonicalLargeFloats(t *testing.T) {
	for _, text := range []string{
		"10000000000000000000",  // 1e19
		"100000000000000000000", // 1e20
		"9223372036854776000",   // the canonical text of float64(int64 max)
		"-9223372036854776000",
	} {
		t.Run(text, func(t *testing.T) {
			v := libjson.LoadWith([]byte(text), exact())
			require.Equal(t, lisp.LFloat, v.Type, "got %s: %s", lisp.GetType(v).Str, v)
			out, err := libjson.Dump(v, false)
			require.NoError(t, err)
			assert.Equal(t, text, string(out), "the float must re-encode to the same digits")
		})
	}
}

// TestLoadExactDumpLoadIsClosed is the invariant the case above exists to
// protect, stated directly: every float this package can emit, the option can
// read back.
func TestLoadExactDumpLoadIsClosed(t *testing.T) {
	floats := []float64{
		0, 1, -1, 1.5, 1e-7, 1e-6, 1e20, 1e21, 1e30, -1e19,
		float64(int64Max), float64(int64Min), maxExactFloat, maxExactFloat + 2,
		math.SmallestNonzeroFloat64, math.MaxFloat64, math.Copysign(0, -1),
	}
	for _, f := range floats {
		enc, err := libjson.Dump(lisp.Float(f), false)
		require.NoError(t, err)
		back := libjson.LoadWith(enc, exact())
		require.NotEqual(t, lisp.LError, back.Type,
			"the option refused this package's own output for %v: %s (%s)", f, enc, back)
		reenc, err := libjson.Dump(back, false)
		require.NoError(t, err)
		assert.Equal(t, string(enc), string(reenc), "not stable for %v", f)
	}
}

// TestLoadExactExponentFormNormalises pins the known asymmetry the fuzzer
// found, so that it is a decision rather than an accident.
//
// The integer rule is syntactic, and Dump normalises a float's text. A
// document written "100e7" is not an integer literal, so it decodes to a
// float; Dump writes that float as "1000000000", which IS an integer literal,
// so a re-read makes it an int. The value is correct throughout and stable
// from the second read on -- what changed is the document.
//
// The alternative, deciding int-vs-float from the VALUE rather than the text,
// would also make "1.0" an int and needs exact decimal arithmetic to be
// reproducible. It widens what the option touches and enlarges the surface
// that has to be identical on every node, for a case that machine-generated
// JSON does not produce: Go, JavaScript and Python all render 1e9 as plain
// digits already.
func TestLoadExactExponentFormNormalises(t *testing.T) {
	first := libjson.LoadWith([]byte("100e7"), exact())
	require.Equal(t, lisp.LFloat, first.Type)

	enc, err := libjson.Dump(first, false)
	require.NoError(t, err)
	require.Equal(t, "1000000000", string(enc))

	second := libjson.LoadWith(enc, exact())
	assert.Equal(t, lisp.LInt, second.Type,
		"after one rewrite the exponent form is gone and the value is an int")
	assert.Equal(t, 1000000000, second.Int)

	// Stable from here on.
	reenc, err := libjson.Dump(second, false)
	require.NoError(t, err)
	assert.Equal(t, "1000000000", string(reenc))
	third := libjson.LoadWith(reenc, exact())
	assert.Equal(t, lisp.LInt, third.Type)
}

// ---------------------------------------------------------------------------
// The default half: today's behaviour, pinned.
// ---------------------------------------------------------------------------

// TestIssue350HidingMechanism pins the mechanism that let this sit open since
// 2018: the rounded value still compares = to the integer it was meant to be.
// A program reads a corrupted identifier, checks it against the value it
// expected, matches, and carries on -- nothing signals.
//
// This test asserts the hiding mechanism is STILL THERE in default mode, which
// sounds backwards until you notice what it buys: it is the tripwire on the
// default. If someone later flips the default to exact integers, or "cleans
// up" the float64 case, this test fails and says so, instead of the fleet
// finding out. When the default is deliberately flipped, this test is the one
// that must be rewritten in the same commit -- on purpose, in the open.
func TestIssue350HidingMechanism(t *testing.T) {
	const text = "9007199254740993" // 2^53+1
	loaded := libjson.Load([]byte(text), false)

	require.Equal(t, lisp.LFloat, loaded.Type,
		"DEFAULT mode must still decode this as a float; if this fails the"+
			" default has been flipped and every consumer's (type x) changed")
	require.Equal(t, math.Float64bits(float64(maxExactFloat)), math.Float64bits(loaded.Float),
		"the value has been rounded down by one")

	// The hiding mechanism itself.
	eq := loaded.Equal(lisp.Int(maxExactFloat + 1))
	assert.True(t, lisp.True(eq),
		"the corrupted value must still compare = to its integer -- that is"+
			" WHY nothing reports the corruption, and pinning it here is what"+
			" stops the regression returning silently")

	// It also compares = to the value it was actually rounded TO, which is
	// how two distinct JSON documents become indistinguishable in memory.
	other := libjson.Load([]byte("9007199254740992"), false)
	assert.True(t, lisp.True(loaded.Equal(other)),
		"9007199254740993 and 9007199254740992 must be indistinguishable"+
			" after a default load -- the corruption is not detectable by"+
			" comparison, only by opting in")

	// And the same value under the opt-in is distinguishable, which is the
	// whole point.
	exactLoaded := libjson.LoadWith([]byte(text), exact())
	exactOther := libjson.LoadWith([]byte("9007199254740992"), exact())
	require.Equal(t, lisp.LInt, exactLoaded.Type)
	assert.False(t, lisp.True(exactLoaded.Equal(exactOther)),
		"under :exact-integers the two documents must be distinguishable")
}

// TestLoadDefaultUnchanged pins the default decode for the inputs the fix
// touches. Any diff here is a consensus-visible change to what a JSON
// document MEANS, on nodes that never opted in.
func TestLoadDefaultUnchanged(t *testing.T) {
	tests := []struct {
		json  string
		typ   lisp.LType
		value string // LVal.String()
		dump  string
	}{
		{"9007199254740993", lisp.LFloat, "9.007199254740992e+15", "9007199254740992"},
		{"9223372036854775807", lisp.LFloat, "9.223372036854776e+18", "9223372036854776000"},
		{"-9223372036854775808", lisp.LFloat, "-9.223372036854776e+18", "-9223372036854776000"},
		{"9007199254740991", lisp.LFloat, "9.007199254740991e+15", "9007199254740991"},
		{"1", lisp.LFloat, "1", "1"},
		{"-0", lisp.LFloat, "-0", "-0"},
		{"1e2", lisp.LFloat, "100", "100"},
		{"1.5", lisp.LFloat, "1.5", "1.5"},
		{"123456789012345678901234567890", lisp.LFloat, "1.2345678901234568e+29", "1.2345678901234568e+29"},
	}
	for _, test := range tests {
		t.Run(test.json, func(t *testing.T) {
			v := libjson.Load([]byte(test.json), false)
			require.Equal(t, test.typ, v.Type, "got %s", lisp.GetType(v).Str)
			assert.Equal(t, test.value, v.String())
			out, err := libjson.Dump(v, false)
			require.NoError(t, err)
			assert.Equal(t, test.dump, string(out))
		})
	}
}

// TestLoadStringNumbersUnchanged pins the other pre-existing option, including
// that it still WINS over exact-integers when a caller sets both.
func TestLoadStringNumbersUnchanged(t *testing.T) {
	for _, text := range []string{"9007199254740993", "9223372036854775807", "1", "1e2", "-0"} {
		v := libjson.Load([]byte(text), true)
		require.Equal(t, lisp.LString, v.Type)
		assert.Equal(t, text, v.Str)

		both := libjson.LoadWith([]byte(text), libjson.LoadOpts{StringNumbers: true, ExactIntegers: true})
		require.Equal(t, lisp.LString, both.Type,
			":string-numbers must take precedence over :exact-integers")
		assert.Equal(t, text, both.Str)
	}
}

// ---------------------------------------------------------------------------
// The dump side.
// ---------------------------------------------------------------------------

// TestDumpIntegersAreExact answers the question directly: does json:dump round
// large integers on the way OUT as well? It does not. encodeLInt uses
// strconv.AppendInt on the int64, which is exact for every int64. The rounding
// seen in a load-then-dump is entirely the load's -- the value handed to Dump
// is already a float by then.
func TestDumpIntegersAreExact(t *testing.T) {
	for _, n := range []int64{
		0, 1, -1, maxExactFloat - 1, maxExactFloat, maxExactFloat + 1,
		int64Max, int64Max - 1, int64Min, int64Min + 1,
	} {
		text := strconv.FormatInt(n, 10)
		out, err := libjson.Dump(lisp.Int(int(n)), false)
		require.NoError(t, err)
		assert.Equal(t, text, string(out), "Dump rounded an int on the way out")

		out, err = libjson.Dump(lisp.Int(int(n)), true)
		require.NoError(t, err)
		assert.Equal(t, `"`+text+`"`, string(out))
	}
}

// TestDumpFloatIsWhereTheDigitsGo shows the other side of the same coin: a
// float carrying a large integer dumps the float's digits, which is correct
// for a float and wrong for the integer the document contained. Dump has no
// way to tell the difference -- by then the information is gone. That is why
// the fix belongs on the load side.
func TestDumpFloatIsWhereTheDigitsGo(t *testing.T) {
	out, err := libjson.Dump(lisp.Float(float64(int64Max)), false)
	require.NoError(t, err)
	assert.Equal(t, "9223372036854776000", string(out))

	// The emitted digits overflow an int64, which is itself the point: what
	// comes back out is not a number the document could have contained.
	got, ok := new(big.Int).SetString(string(out), 10)
	require.True(t, ok)
	drift := new(big.Int).Sub(got, big.NewInt(int64Max))
	assert.Equal(t, "193", drift.String(),
		"int64 max does not survive a default load/dump; this pins the drift")
}

// ---------------------------------------------------------------------------
// Error shapes.
// ---------------------------------------------------------------------------

// TestLoadExactSyntaxErrors pins that malformed input is still reported as the
// catchable json:syntax-error condition under the opt-in. The opt-in has to
// use json.Decoder (UseNumber only exists there), and the decoder reports an
// empty document and trailing content in shapes json.Unmarshal does not, so
// without deliberate work an adopter's handler-bind would quietly stop firing.
func TestLoadExactSyntaxErrors(t *testing.T) {
	for _, text := range []string{
		"", " ", "{false:true}", "nulll", "1 2", "[1,2", `{"a":`, "[,]", "\x00",
	} {
		t.Run(fmt.Sprintf("%q", text), func(t *testing.T) {
			v := libjson.LoadWith([]byte(text), exact())
			require.Equal(t, lisp.LError, v.Type, "got %s", v.String())
			assert.Equal(t, "json:syntax-error", v.Str,
				"malformed input must stay catchable as json:syntax-error")

			// The default path agrees that this is malformed.
			d := libjson.Load([]byte(text), false)
			assert.Equal(t, lisp.LError, d.Type)
		})
	}
}

// TestLoadExactFloatOverflow keeps the one non-integer failure aligned with
// the default path, which also refuses a number that overflows a float64.
func TestLoadExactFloatOverflow(t *testing.T) {
	for _, text := range []string{"1e400", "-1e400"} {
		v := libjson.LoadWith([]byte(text), exact())
		require.Equal(t, lisp.LError, v.Type)
		assert.Contains(t, v.String(), "cannot unmarshal number")

		d := libjson.Load([]byte(text), false)
		require.Equal(t, lisp.LError, d.Type)
	}
}

// TestLoadExactMaxAlloc proves the allocation bound still applies on the new
// decode path.
func TestLoadExactMaxAlloc(t *testing.T) {
	doc := []byte(`[1,2,3,4,5]`)
	v := libjson.LoadWith(doc, libjson.LoadOpts{ExactIntegers: true, MaxAlloc: 3})
	require.Equal(t, lisp.LError, v.Type)
	assert.Contains(t, v.String(), "exceeds maximum")

	v = libjson.LoadWith(doc, libjson.LoadOpts{ExactIntegers: true, MaxAlloc: 5})
	require.NotEqual(t, lisp.LError, v.Type, "%s", v)
}

// TestLoadOptsZeroValueIsTodayExactly pins that the zero LoadOpts is the old
// behaviour, so an embedder that adopts LoadWith without setting anything gets
// no change at all.
func TestLoadOptsZeroValueIsTodayExactly(t *testing.T) {
	for _, text := range []string{
		"9007199254740993", "1", "1.5", "-0", "1e2", `{"a":[1,2.5,null,true]}`,
		"", "{false:true}", "1e400",
	} {
		want := libjson.Load([]byte(text), false)
		got := libjson.LoadWith([]byte(text), libjson.LoadOpts{})
		assert.Equal(t, want.Type, got.Type, "%q", text)
		assert.Equal(t, want.String(), got.String(), "%q", text)
	}
}
