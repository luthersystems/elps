// Copyright © 2018 The ELPS authors

package libjson_test

import (
	"encoding/json"
	"fmt"
	"go/token"
	"reflect"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/elpstest"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// newLispEnv returns an environment with the standard library loaded, so these
// tests reach `json:dump-message` through the builtin rather than through a Go
// shortcut a lisp program could not take.
func newLispEnv(t testing.TB) *lisp.LEnv {
	t.Helper()
	r := &elpstest.Runner{}
	t.Cleanup(r.Close)
	env, err := r.NewEnv(t)
	require.NoError(t, err)
	return env
}

func evalIn(t testing.TB, env *lisp.LEnv, src string) *lisp.LVal {
	t.Helper()
	v := env.LoadString("own-message-test", src)
	require.NotEqual(t, lisp.LError, v.Type, "evaluating %s: %v", src, v)
	return v
}

// dumpMessage calls the builtin directly.  It is the same mint site
// `json:dump-message` reaches, which is the point: there must be exactly one,
// and a test that built the value another way would not be testing it.
func dumpMessage(t testing.TB, env *lisp.LEnv, v *lisp.LVal) *lisp.LVal {
	t.Helper()
	s := libjson.DefaultSerializer()
	msg := s.DumpMessageBuiltin(env, lisp.SExpr([]*lisp.LVal{v, lisp.Nil()}))
	require.NotEqual(t, lisp.LError, msg.Type, "json:dump-message: %v", msg)
	return msg
}

func messageBytes(t testing.TB, env *lisp.LEnv, msg *lisp.LVal) []byte {
	t.Helper()
	s := libjson.DefaultSerializer()
	b := s.MessageBytesBuiltin(env, lisp.SExpr([]*lisp.LVal{msg}))
	require.NotEqual(t, lisp.LError, b.Type, "json:message-bytes: %v", b)
	return b.Bytes()
}

// TestDumpMessageIsExemptFromTheLoadabilityCheck pins elps#412's mechanism at
// the only level it is visible: which type the exemption is keyed on.
//
// Nothing about the OUTPUT changes when the check is skipped -- that is the
// whole point -- so a test comparing bytes would pass whether or not the
// exemption existed.  What this holds instead is that the mint site produced
// the exempt shape, and that the shape is one nothing outside this package can
// build: TestEmbedderCannotObtainTheExemption holds the other half.
func TestDumpMessageIsExemptFromTheLoadabilityCheck(t *testing.T) {
	env := newLispEnv(t)
	msg := evalIn(t, env, `(json:dump-message (sorted-map "a" 1 "b" (vector 1 2 3)))`)
	require.Equal(t, lisp.LNative, msg.Type)

	if _, isRaw := msg.Native.(*json.RawMessage); isRaw {
		t.Fatal("json:dump-message still returns a *json.RawMessage, so the " +
			"exemption cannot be keyed on the type -- elps#412 is not implemented")
	}
	require.Equal(t, "*libjson.ownMessage", fmt.Sprintf("%T", msg.Native),
		"the exempt type moved; the leak-prevention tests below name this")

	// Opaque by construction, checked rather than asserted.  An exported type
	// name would let an embedder build one outright; an exported field would
	// let them reflect a payload into an existing one with SetBytes, which
	// needs no unsafe and no cooperation from this package.
	rt := reflect.TypeOf(msg.Native).Elem()
	assert.False(t, token.IsExported(rt.Name()), "the exempt type is exported")
	rv := reflect.ValueOf(msg.Native).Elem()
	for i := range rt.NumField() {
		f := rt.Field(i)
		assert.False(t, f.IsExported(), "field %s of the exempt type is exported", f.Name)
		assert.False(t, rv.Field(i).CanSet(), "field %s is settable through reflection", f.Name)
	}
}

// TestEmbedderCannotObtainTheExemption is the guard on elps#412's one real
// risk.  An exemption that leaks reopens elps#410 SILENTLY, which is worse
// than the bug it started from: the original at least announced itself the
// moment json:load saw the document.
//
// The rows are the two independent ways a native carries bytes Load refuses --
// the out-of-range literal and nesting past the decoder's limit -- in every
// shape an embedder actually has: a *json.RawMessage, a json.RawMessage value,
// and the same bytes behind a struct field.  None of them can become an
// ownMessage.  This is what fails if that stops being true: an exported
// constructor, an exported field, or a second mint site fed from an argument.
//
// The two kinds of bytes are refused by DIFFERENT layers, and wantErr records
// which.  That distinction is not decoration -- it is the measurement of how
// much work the elps#410 check is really doing:
//
//   - The out-of-range literal is syntactically valid JSON, so json.Marshal
//     passes it through untouched and only checkLoadable, which decodes,
//     catches it.  That is the gap the check exists for, and the only one it
//     is the sole guard on.
//
//   - Nesting past the limit is caught EARLIER, by encoding/json itself:
//     json.Marshal compacts whatever MarshalJSON returns, and that compaction
//     applies the same 10000-deep bound the decoder does.  checkLoadable never
//     sees those bytes.
func TestEmbedderCannotObtainTheExemption(t *testing.T) {
	deep := json.RawMessage(strings.Repeat("[", 10001) + "1" + strings.Repeat("]", 10001))
	outOfRange := json.RawMessage("1E1000")

	// The elps#410 error: the loadability check refused the bytes.
	const byTheCheck = "unable to encode native value"
	// encoding/json refused them first, so the check never ran.
	const byEncodingJSON = "exceeded max depth"

	cases := []struct {
		name    string
		v       interface{}
		wantErr string
	}{
		{"pointer to RawMessage, out-of-range number", &outOfRange, byTheCheck},
		{"RawMessage value, out-of-range number", outOfRange, byTheCheck},
		{"RawMessage in a struct field, out-of-range number", struct {
			P json.RawMessage `json:"p"`
		}{outOfRange}, byTheCheck},
		{"pointer to RawMessage, nested past the decoder's limit", &deep, byEncodingJSON},
		{"RawMessage value, nested past the decoder's limit", deep, byEncodingJSON},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			v := lisp.Native(tc.v)
			require.NotEqual(t, "*libjson.ownMessage", fmt.Sprintf("%T", v.Native),
				"an embedder value reached the exempt type")

			// Refused at the root, exactly as before elps#412...
			if enc, err := libjson.Dump(v, false); err == nil {
				t.Fatalf("Dump emitted %.80s, which Load rejects -- elps#410 reopened", enc)
			} else if !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("refused, but not by the layer that should have refused it: "+
					"want an error containing %q, got %v", tc.wantErr, err)
			}

			// ...and refused inside an envelope, which is the row that
			// matters.  The exemption is applied during a walk, so a value
			// that escaped it in a container and not at the root would be a
			// hole nothing else here catches.
			envelope := lisp.SortedMap()
			envelope.MapSet("jsonrpc", lisp.String("2.0"))
			envelope.MapSet("result", v)
			if enc, err := libjson.Dump(envelope, false); err == nil {
				t.Fatalf("Dump emitted %.80s inside an envelope -- elps#410 reopened", enc)
			}
		})
	}
}

// TestDumpMessageOfAnUnvouchedDocumentIsStillRefused is the other half of the
// seal, and the reason the exemption is not the type alone.
//
// `json:dump-message` mints an ownMessage for every document, so the skip
// rides on the loadable flag the encoder set rather than on who called it.
// These are the two documents libjson writes and libjson will not read back,
// so these are the two for which the flag is false.  The gap itself predates
// elps#412 -- `json:dump` of either value produces the same bytes today, and
// always has -- but elps#412 must not make it SILENT by waving a document
// through on the strength of its author.
//
// What refuses them, as TestEmbedderCannotObtainTheExemption also records, is
// encoding/json rather than checkLoadable: both rows fail because they nest
// past the limit, and json.Marshal compacts a MarshalJSON result against that
// same limit before this package gets a word in.  So these rows hold the
// OUTCOME -- an unvouched document does not get written -- and not the
// mechanism.  The mechanism, that a false flag really does re-arm the check,
// is only reachable from inside the package; TestExemptionRidesOnTheFlag in
// own_message_internal_test.go holds it.
func TestDumpMessageOfAnUnvouchedDocumentIsStillRefused(t *testing.T) {
	env := newLispEnv(t)

	t.Run("nested past the decoder's limit", func(t *testing.T) {
		payload := lisp.Int(1)
		for range 10001 {
			m := lisp.SortedMap()
			m.MapSet("k", payload)
			payload = m
		}
		msg := dumpMessage(t, env, payload)

		// Premise: these really are bytes Load refuses.  Without it the row
		// could pass because the document is fine.
		require.Equal(t, lisp.LError, libjson.Load(messageBytes(t, env, msg), false).Type,
			"premise broken: Load accepts the document, so the row proves nothing")

		envelope := lisp.SortedMap()
		envelope.MapSet("result", msg)
		enc, err := libjson.Dump(envelope, false)
		require.Error(t, err, "Dump emitted %.80s, which Load rejects", enc)
		assert.Contains(t, err.Error(), "exceeded max depth")
	})

	t.Run("assembled over an embedder's native", func(t *testing.T) {
		// A two-deep lisp value whose only content is an embedder's native.
		// The native's own bytes clear checkLoadable in isolation -- they are
		// exactly at the decoder's limit -- but nesting COMPOSES, so what
		// json:dump-message produces is one level past it.  A rule that
		// vouched for output merely because libjson assembled it would wave
		// this through.
		deep := json.RawMessage(strings.Repeat("[", 10000) + "1" + strings.Repeat("]", 10000))
		require.NotEqual(t, lisp.LError, libjson.Load(deep, false).Type,
			"premise broken: the native alone must load, or the check refuses it first")

		payload := lisp.SortedMap()
		payload.MapSet("k", lisp.Native(&deep))
		msg := dumpMessage(t, env, payload)

		require.Equal(t, lisp.LError, libjson.Load(messageBytes(t, env, msg), false).Type,
			"premise broken: the composed document loads, so the row proves nothing")

		envelope := lisp.SortedMap()
		envelope.MapSet("result", msg)
		enc, err := libjson.Dump(envelope, false)
		require.Error(t, err, "Dump emitted %.80s, which Load rejects", enc)
	})
}

// TestDumpMessageOutputIsUnchanged is the byte-level half of "invisible".
//
// The exemption removes a READ of the marshalled bytes and nothing else: the
// bytes still go through json.Marshal, so compaction and HTML escaping are the
// same code they were.  This holds that -- an ownMessage in a document and the
// identical bytes as an embedder's *json.RawMessage in the same document must
// serialize the same way, in both number modes, including the characters
// encoding/json escapes on the way through.
func TestDumpMessageOutputIsUnchanged(t *testing.T) {
	env := newLispEnv(t)

	m := lisp.SortedMap()
	m.MapSet("a", lisp.Int(1))
	m.MapSet("html", lisp.String("<&>"))

	payloads := []*lisp.LVal{
		lisp.String("plain"),
		lisp.Int(42),
		lisp.Float(1.5),
		lisp.Nil(),
		lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("two")}),
		lisp.String(`<script>&"\` + "\n  "),
		lisp.Bytes([]byte{0, 1, 2, 250}),
		m,
	}

	for i, payload := range payloads {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			own := dumpMessage(t, env, payload)
			raw := json.RawMessage(messageBytes(t, env, own))

			for _, stringNums := range []bool{false, true} {
				envOwn := lisp.SortedMap()
				envOwn.MapSet("result", own)
				envRaw := lisp.SortedMap()
				envRaw.MapSet("result", lisp.Native(&raw))

				gotOwn, err := libjson.Dump(envOwn, stringNums)
				require.NoError(t, err)
				gotRaw, err := libjson.Dump(envRaw, stringNums)
				require.NoError(t, err)
				assert.Equal(t, string(gotRaw), string(gotOwn),
					"stringNums=%v: the exemption changed the bytes", stringNums)

				// And whatever it wrote, Load must still take back.
				assert.NotEqual(t, lisp.LError, libjson.Load(gotOwn, stringNums).Type)
			}
		})
	}
}

// TestOwnMessageIsInvisibleFromLisp records what a lisp program can see of
// elps#412, which is the property the change was asked to have.  Each
// observation is checked against the documented behaviour rather than against
// a golden blob, so the test says what is preserved instead of merely that
// something did not move.
//
// The one thing NOT preserved is recorded here so it is not met by surprise:
// the value's PRINTED form embeds the Go type, so `#<native value:
// *json.RawMessage>` becomes `#<native value: *libjson.ownMessage>`.  That is
// the rendering LVal.String gives every native, it is reachable from lisp
// through format-string and debug-print, and nothing on this side can change
// it -- %T prints the concrete type.  Asserted below so the change is pinned
// rather than implied.
func TestOwnMessageIsInvisibleFromLisp(t *testing.T) {
	env := newLispEnv(t)
	evalIn(t, env, `(set 'm (json:dump-message (sorted-map "a" 1 "b" (vector 1 2 3))))`)

	for _, tc := range []struct {
		expr string
		want string
	}{
		{`(type m)`, "native"},
		{`(to-string (json:message-bytes m))`, `{"a":1,"b":[1,2,3]}`},
		{`(json:dump-string (json:load-message m))`, `{"a":1,"b":[1,2,3]}`},
		{`(json:dump-string (sorted-map "env" m))`, `{"env":{"a":1,"b":[1,2,3]}}`},
		{`(format-string "{}" (nil? m))`, "false"},
		{`(format-string "{}" (bytes? m))`, "false"},
		{`(format-string "{}" (sorted-map? m))`, "false"},
		{`(format-string "{}" (json:message-bytes (json:dump-message m)))`,
			`#<bytes 123 34 97 34 58 49 44 34 98 34 58 91 49 44 50 44 51 93 125>`},

		// The one visible difference, pinned rather than hidden.
		{`(format-string "{}" m)`, "#<native value: *libjson.ownMessage>"},
	} {
		t.Run(tc.expr, func(t *testing.T) {
			got := evalIn(t, env, tc.expr)
			assert.Equal(t, tc.want, got.Str)
		})
	}
}

// TestMessageAccessorsStillTakeAnEmbeddersRawMessage holds the compatibility
// half of the type change.  json:message-bytes and json:load-message asserted
// on *json.RawMessage before elps#412, so an embedder handing one to lisp for
// those builtins to read is a supported thing to do, and the new type must not
// cost them that.
func TestMessageAccessorsStillTakeAnEmbeddersRawMessage(t *testing.T) {
	env := newLispEnv(t)
	s := libjson.DefaultSerializer()

	payload := lisp.SortedMap()
	payload.MapSet("a", lisp.Int(1))
	raw := json.RawMessage(`{"a":1}`)

	for _, tc := range []struct {
		name string
		v    *lisp.LVal
	}{
		{"embedder RawMessage", lisp.Native(&raw)},
		{"json:dump-message", dumpMessage(t, env, payload)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assert.Equal(t, `{"a":1}`, string(messageBytes(t, env, tc.v)))
			loaded := s.LoadMessageBuiltin(env, lisp.SExpr([]*lisp.LVal{tc.v, lisp.Nil()}))
			require.NotEqual(t, lisp.LError, loaded.Type, "json:load-message: %v", loaded)
			assert.Equal(t, lisp.LSortMap, loaded.Type)
		})
	}

	// A native that is not a message at all is still refused, with the error
	// text it has always had.
	for _, name := range []string{"message-bytes", "load-message"} {
		t.Run(name+" on a non-message native", func(t *testing.T) {
			args := lisp.SExpr([]*lisp.LVal{lisp.Native(42), lisp.Nil()})
			got := s.MessageBytesBuiltin(env, args)
			if name == "load-message" {
				got = s.LoadMessageBuiltin(env, args)
			}
			require.Equal(t, lisp.LError, got.Type)
			assert.Contains(t, got.String(), "argument is not a raw json-message: <nil>")
		})
	}
}
