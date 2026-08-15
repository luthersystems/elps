// Copyright © 2018 The ELPS authors

package libjson

import (
	"encoding/json"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"math"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestExemptionRidesOnTheFlag proves elps#412's skip is real, and that it is
// the loadable flag rather than the type alone that arms it.
//
// It is the one test that can: from outside the package there is no way to
// build an ownMessage holding bytes the check would refuse -- that is the
// whole point of TestEmbedderCannotObtainTheExemption -- so from outside, an
// exemption that had quietly stopped being applied would look identical to one
// that worked.  Every observable of a correct skip is "nothing changed".
//
// The payload is the elps#410 literal: syntactically valid JSON that
// json.Marshal is happy to pass through and that the decoder refuses, so it
// separates the two layers cleanly.  Nesting would not -- encoding/json's own
// compaction catches that before checkLoadable is reached, and the row would
// pass whether or not the skip existed.
//
// These bytes are a fiction: no encoder in this package can emit 1E1000, since
// encodeFloat and encodeInt both refuse anything a float64 cannot carry.  The
// fiction is deliberate.  The test asks what the exemption DOES, not whether
// its premise holds; FuzzDumpJSON is what holds the premise.
func TestExemptionRidesOnTheFlag(t *testing.T) {
	const unloadable = "1E1000"

	envelope := func(native interface{}) *lisp.LVal {
		m := lisp.SortedMap()
		m.MapSet("result", lisp.Native(native))
		return m
	}

	t.Run("vouched, so the check is skipped", func(t *testing.T) {
		b, err := Dump(envelope(&ownMessage{msg: json.RawMessage(unloadable), loadable: true}), false)
		require.NoError(t, err, "the exemption is not being applied -- elps#412 is inert")
		assert.Equal(t, `{"result":`+unloadable+`}`, string(b))

		// And the skip really did skip something: the same bytes are refused
		// when they are not exempt.  Without this the row above could pass
		// because the check had stopped working for everyone.
		assert.Equal(t, lisp.LError, Load(b, false).Type,
			"premise broken: these bytes load, so nothing was skipped")
	})

	t.Run("not vouched, so the check runs", func(t *testing.T) {
		_, err := Dump(envelope(&ownMessage{msg: json.RawMessage(unloadable), loadable: false}), false)
		require.Error(t, err, "an unvouched ownMessage was waved through on its type alone")
		assert.Contains(t, err.Error(), "unable to encode native value")
	})

	t.Run("an embedder's RawMessage is never exempt", func(t *testing.T) {
		raw := json.RawMessage(unloadable)
		_, err := Dump(envelope(&raw), false)
		require.Error(t, err, "elps#410 reopened for embedder bytes")
		assert.Contains(t, err.Error(), "unable to encode native value")
	})
}

// TestOwnMessageHasOneMintSite is the structural half of "it cannot leak".
//
// TestEmbedderCannotObtainTheExemption shows no embedder VALUE can reach the
// type today.  That argument rests on there being exactly one place the type
// is constructed, from this package's own output -- a second composite literal
// somewhere, fed from a builtin's argument, would defeat it without failing
// any behavioural test written against the sites that exist now.
//
// So this counts them, in the package's non-test sources.  A new one is not
// forbidden; it is required to come with a reason and an updated count, which
// is what puts it in front of a reviewer.
func TestOwnMessageHasOneMintSite(t *testing.T) {
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, ".", func(fi fs.FileInfo) bool {
		return !strings.HasSuffix(fi.Name(), "_test.go")
	}, 0)
	require.NoError(t, err)
	require.Contains(t, pkgs, "libjson")

	var sites []string
	ast.Inspect(pkgs["libjson"], func(n ast.Node) bool {
		lit, ok := n.(*ast.CompositeLit)
		if !ok {
			return true
		}
		if id, ok := lit.Type.(*ast.Ident); ok && id.Name == "ownMessage" {
			sites = append(sites, fset.Position(lit.Pos()).String())
		}
		return true
	})

	assert.Len(t, sites, 1,
		"ownMessage is constructed at %v; the elps#412 exemption is only as "+
			"narrow as the set of places that can mint one, so a new site "+
			"needs review rather than a bumped count", sites)
}

// TestOwnOutputLoadsWithExactIntegers settles the question elps#350 raised for
// elps#412: whether encoder.loadableBytes vouches for documents that a load
// with :exact-integers would refuse.
//
// It could.  That mode is allowed to REJECT an integer literal -- one too
// large for a lisp int is an error there and a rounded float by default -- and
// the elps#410 check that elps#412 skips never tested that mode either
// (checkLoadable decodes with the encoder's string-numbers setting and nothing
// else).  So a number libjson emits and :exact-integers refuses would be a
// third carve-out alongside nestedDeep and wroteNative.
//
// It is not one, and the reason is structural rather than lucky: loadNumber
// takes a float for an over-large integer literal in exactly one case, when
// the literal is ALREADY appendJSONFloat's rendering of that float, and
// appendJSONFloat is the only float rendering this package emits.  The two
// sides cannot drift because they are the same function.
//
// These are the boundaries that argument turns on -- either side of the int
// range, either side of the exponent-form cutoff, and the integral floats in
// between, which are the values that render as bare digits and so take the
// integer path.  FuzzDumpExactIntegers covers the space around them.
func TestOwnOutputLoadsWithExactIntegers(t *testing.T) {
	s := DefaultSerializer()
	for _, tc := range []struct {
		name string
		v    *lisp.LVal
	}{
		{"int max", lisp.Int(math.MaxInt)},
		{"int min", lisp.Int(math.MinInt)},
		{"float 2^63, one past int max", lisp.Float(9223372036854775808)},
		{"float -2^63-1024, one past int min", lisp.Float(-9223372036854776832)},
		{"float 1e19, plain digits", lisp.Float(1e19)},
		{"float 1e20, the last plain-digit decade", lisp.Float(1e20)},
		{"float 1e21, the first exponent-form decade", lisp.Float(1e21)},
		{"float 1e-6, the small-end cutoff", lisp.Float(1e-6)},
		{"float 1e-7, exponent form", lisp.Float(1e-7)},
		{"float 2.0, integral and small", lisp.Float(2)},
		{"float -0", lisp.Float(math.Copysign(0, -1))},
		{"max float64", lisp.Float(math.MaxFloat64)},
		{"2^53+1 as a float", lisp.Float(9007199254740993)},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// The document is the one substrate builds: the value inside the
			// envelope json:dump-message produces.
			b, loadable, err := s.dump(tc.v, false)
			require.NoError(t, err)
			require.True(t, loadable,
				"the encoder declined to vouch for %s, so this row proves nothing", b)

			got := LoadWith(b, LoadOpts{ExactIntegers: true})
			require.NotEqual(t, lisp.LError, got.Type,
				"loadableBytes vouched for %s, which the :exact-integers "+
					"decoder refuses: %v -- that is a third carve-out", b, got)
		})
	}
}
