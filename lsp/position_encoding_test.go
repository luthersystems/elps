// Copyright © 2026 The ELPS authors

package lsp

import (
	"encoding/json"
	"fmt"
	"testing"
	"unicode/utf16"
	"unicode/utf8"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Tests for elps#464 -- the server emitted and consumed BYTE columns where LSP
// counts Position.character in UTF-16 code units.
//
// The arithmetic is tested here; the user-visible half is in
// rename_utf8_test.go, where every case now runs under both wire encodings and
// the edits are APPLIED. That split is deliberate and mirrors elps#463's: a
// test asserting that utf16ColumnOf("(defun 加算", 7) == 7 passes the moment
// the arithmetic is right and never says what was at stake, so the arithmetic
// is pinned here and the consequence is pinned there.

// --- the conversions ---------------------------------------------------

// TestUTF16ColumnConversions pins both directions against hand-computed
// answers, at every width UTF-8 has and at the boundary cases a real client
// produces.
func TestUTF16ColumnConversions(t *testing.T) {
	for _, tc := range []struct {
		name    string
		line    string
		byteCol int
		u16Col  int
	}{
		// ASCII: the identity, and the overwhelmingly common case.
		{"ascii start", "(defun add (x y)", 0, 0},
		{"ascii mid", "(defun add (x y)", 7, 7},
		{"ascii end", "(defun add", 10, 10},

		// 2-byte runes (Latin-1 supplement): 2 bytes, 1 UTF-16 unit.
		{"two-byte before", "(defun éx", 7, 7},
		{"two-byte after one", "(defun éx", 9, 8},
		{"two-byte after both", "(defun éx", 10, 9},
		{"two-byte three of them", "(defun éèê", 13, 10},

		// 3-byte runes (CJK): 3 bytes, 1 UTF-16 unit. The displacement grows
		// with the number of characters to the left, which is the shape the
		// issue describes.
		{"cjk before", "(defun 加算 (a b)", 7, 7},
		{"cjk after one", "(defun 加算 (a b)", 10, 8},
		{"cjk after both", "(defun 加算 (a b)", 13, 9},
		{"cjk following ascii", "(defun 加算 (a b)", 14, 10},

		// 4-byte runes (outside the BMP): 4 bytes, 2 UTF-16 units -- a
		// surrogate PAIR, the one case where a UTF-16 column is not a
		// character count either.
		{"astral before", "(defun 𝛼𝛽 (a)", 7, 7},
		{"astral after one", "(defun 𝛼𝛽 (a)", 11, 9},
		{"astral after both", "(defun 𝛼𝛽 (a)", 15, 11},
		{"emoji after", "x 🙂 y", 6, 4},

		// Mixed, so that neither conversion can be a single multiplication.
		{"mixed", "é加𝛼z", 2 + 3 + 4, 1 + 1 + 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			assert.Equal(t, tc.u16Col, utf16ColumnOf(tc.line, tc.byteCol),
				"byte column %d of %q", tc.byteCol, tc.line)
			assert.Equal(t, tc.byteCol, byteColumnOf(tc.line, tc.u16Col),
				"utf-16 column %d of %q", tc.u16Col, tc.line)
		})
	}
}

// TestUTF16ColumnAgainstStdlib checks the conversion against
// unicode/utf16.Encode rather than against another hand-written table: at
// every rune boundary of a line, the UTF-16 column must equal the length of
// the standard library's own encoding of the prefix.
//
// This is the assertion that would catch a surrogate-pair miscount, since
// utf16.Encode is where the definition of a code unit actually lives.
func TestUTF16ColumnAgainstStdlib(t *testing.T) {
	for _, line := range []string{
		"(defun add (x y) (+ x y))",
		"(defun éx (a) a)",
		"(defun 加算 (a b) (+ a b))",
		"(defun 𝛼𝛽 (a) a)",
		"(set λ 1) ; ελληνικά",
		"mixed é加𝛼🙂 tail",
		"",
	} {
		t.Run(line, func(t *testing.T) {
			for byteCol := range len(line) + 1 {
				if byteCol < len(line) && !utf8.RuneStart(line[byteCol]) {
					continue // not a rune boundary: no UTF-16 column of its own
				}
				want := len(utf16.Encode([]rune(line[:byteCol])))
				got := utf16ColumnOf(line, byteCol)
				require.Equal(t, want, got, "utf16ColumnOf(%q, %d)", line, byteCol)
				require.Equal(t, byteCol, byteColumnOf(line, got),
					"byteColumnOf is not the inverse at byte %d of %q", byteCol, line)
			}
		})
	}
}

// TestUTF16ColumnEdgeCases pins the three rules that keep both conversions
// total. They matter because a client sends whatever column it likes -- a
// cursor at end of line, a stale position from a document that has since
// changed -- and a conversion that panicked would take the server down, while
// one that returned an inverted range would corrupt a rename.
func TestUTF16ColumnEdgeCases(t *testing.T) {
	const line = "(é)" // 5 bytes, 4 UTF-16 units

	t.Run("negative clamps to zero", func(t *testing.T) {
		assert.Equal(t, 0, utf16ColumnOf(line, -3))
		assert.Equal(t, 0, byteColumnOf(line, -3))
	})

	t.Run("past end of line extends one for one", func(t *testing.T) {
		// A cursor at end of line is routine; a column beyond it must stay
		// beyond it, and stay ORDERED, or a range can invert.
		assert.Equal(t, 4, utf16ColumnOf(line, 5))
		assert.Equal(t, 6, utf16ColumnOf(line, 7))
		assert.Less(t, utf16ColumnOf(line, 5), utf16ColumnOf(line, 7))
		assert.Equal(t, 5, byteColumnOf(line, 4))
		assert.Equal(t, 7, byteColumnOf(line, 6))
	})

	t.Run("inside a rune rounds down to its start", func(t *testing.T) {
		// Byte 2 is the continuation byte of "é". There is no UTF-16 column
		// for half a character, so it reports the column of the character.
		assert.Equal(t, 1, utf16ColumnOf(line, 2))
	})

	t.Run("inside a surrogate pair rounds down to its start", func(t *testing.T) {
		const astral = "a𝛼b" // "𝛼" is 4 bytes / 2 units, starting at unit 1
		assert.Equal(t, 1, byteColumnOf(astral, 2), "unit 2 is the low surrogate")
		assert.Equal(t, 5, byteColumnOf(astral, 3))
	})

	t.Run("invalid utf-8 counts one unit per byte", func(t *testing.T) {
		// DecodeRuneInString yields (RuneError, 1) for a lone continuation
		// byte, so the two conversions agree about the width of the damage
		// instead of drifting apart on it.
		bad := "a\xffb"
		assert.Equal(t, 3, utf16ColumnOf(bad, 3))
		assert.Equal(t, 3, byteColumnOf(bad, 3))
		assert.False(t, utf8.ValidString(bad), "fixture must be invalid UTF-8")
	})

	t.Run("empty line", func(t *testing.T) {
		assert.Equal(t, 0, utf16ColumnOf("", 0))
		assert.Equal(t, 0, byteColumnOf("", 0))
		assert.Equal(t, 3, utf16ColumnOf("", 3))
		assert.Equal(t, 3, byteColumnOf("", 3))
	})
}

// TestLineOf pins the line splitter the conversions index with, including the
// CRLF case: this package splits on "\n" throughout, so a "\r" stays on the
// line, and being ASCII it changes neither column.
func TestLineOf(t *testing.T) {
	const content = "one\ntwo\r\nthrée"
	assert.Equal(t, "one", lineOf(content, 0))
	assert.Equal(t, "two\r", lineOf(content, 1))
	assert.Equal(t, "thrée", lineOf(content, 2))
	assert.Empty(t, lineOf(content, 3), "a line past the end is empty, not a panic")
	assert.Empty(t, lineOf(content, -1))
	assert.Equal(t, 4, utf16ColumnOf(lineOf(content, 1), 4), "the trailing CR is one byte and one unit")
}

// --- negotiation -------------------------------------------------------

// TestClientPositionEncodings pins the raw-params read. It has to read raw
// JSON because general.positionEncodings is an LSP 3.17 field that glsp's
// protocol_3_16 InitializeParams does not name, so encoding/json drops it
// before initialize ever sees the typed struct -- which is the reason elps#464
// says the escape hatch "is not available on the protocol version in use".
func TestClientPositionEncodings(t *testing.T) {
	for _, tc := range []struct {
		name string
		raw  string
		want positionEncoding
	}{
		{"utf-8 offered", `{"capabilities":{"general":{"positionEncodings":["utf-16","utf-8"]}}}`, encodingUTF8},
		{"utf-8 only", `{"capabilities":{"general":{"positionEncodings":["utf-8"]}}}`, encodingUTF8},
		{"utf-16 only", `{"capabilities":{"general":{"positionEncodings":["utf-16"]}}}`, encodingUTF16},
		{"utf-32 only", `{"capabilities":{"general":{"positionEncodings":["utf-32"]}}}`, encodingUTF16},
		{"empty list", `{"capabilities":{"general":{"positionEncodings":[]}}}`, encodingUTF16},
		{"no general block", `{"capabilities":{}}`, encodingUTF16},
		{"a 3.16 client", `{"processId":1,"rootUri":null,"capabilities":{"textDocument":{}}}`, encodingUTF16},
		{"malformed", `{"capabilities":`, encodingUTF16},
		{"empty params", ``, encodingUTF16},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got := selectPositionEncoding(clientPositionEncodings(json.RawMessage(tc.raw)))
			assert.Equal(t, tc.want, got)
		})
	}
}

// TestInitializeNegotiatesPositionEncoding drives the real handler and reads
// the answer off the wire form, since the capability is added by a local
// wrapper struct rather than by protocol.ServerCapabilities.
func TestInitializeNegotiatesPositionEncoding(t *testing.T) {
	initialize := func(t *testing.T, raw string) (*Server, map[string]any) {
		t.Helper()
		s := testServer()
		ctx := &glsp.Context{
			Method: "initialize",
			Params: json.RawMessage(raw),
			Notify: func(string, any) {},
		}
		var params protocol.InitializeParams
		if raw != "" {
			require.NoError(t, json.Unmarshal([]byte(raw), &params))
		}
		res, err := s.initialize(ctx, &params)
		require.NoError(t, err)
		encoded, err := json.Marshal(res)
		require.NoError(t, err)
		var out struct {
			Capabilities map[string]any `json:"capabilities"`
		}
		require.NoError(t, json.Unmarshal(encoded, &out))
		return s, out.Capabilities
	}

	t.Run("client offers utf-8", func(t *testing.T) {
		s, caps := initialize(t, `{"capabilities":{"general":{"positionEncodings":["utf-8","utf-16"]}}}`)
		assert.Equal(t, encodingUTF8, s.positionEncoding())
		assert.Equal(t, "utf-8", caps["positionEncoding"],
			"a server that speaks bytes has to say so, or the client reads them as UTF-16")
	})

	t.Run("client offers only utf-16", func(t *testing.T) {
		s, caps := initialize(t, `{"capabilities":{"general":{"positionEncodings":["utf-16"]}}}`)
		assert.Equal(t, encodingUTF16, s.positionEncoding())
		assert.NotContains(t, caps, "positionEncoding",
			"an absent positionEncoding is how LSP 3.17 spells utf-16, and 3.16 clients ignore the field")
	})

	t.Run("a 3.16 client that negotiates nothing", func(t *testing.T) {
		s, caps := initialize(t, `{"capabilities":{"textDocument":{"hover":{}}}}`)
		assert.Equal(t, encodingUTF16, s.positionEncoding(),
			"UTF-16 is not negotiable before 3.17: a client that says nothing is owed it")
		assert.NotContains(t, caps, "positionEncoding")
	})

	t.Run("inlayHintProvider still advertised", func(t *testing.T) {
		// GUARD: the capability wrapper gained a field; the one already in it
		// must survive.
		_, caps := initialize(t, `{"capabilities":{}}`)
		assert.Equal(t, true, caps["inlayHintProvider"])
	})
}

// TestPositionEncodingDefaultsToUTF16 is the property that makes the default
// safe: a server that never completed a handshake owes the client UTF-16, so
// that has to be the ZERO value rather than something initialize sets.
func TestPositionEncodingDefaultsToUTF16(t *testing.T) {
	assert.Equal(t, encodingUTF16, testServer().positionEncoding())
	assert.Equal(t, encodingUTF16, positionEncoding(0))
}

// --- the inbound boundary ----------------------------------------------

// TestCursorPositionIsReadInWireUnits is the INBOUND half of elps#464, at the
// one boundary every position-taking handler goes through.
//
// A cursor to the right of a multi-byte character arrives in UTF-16 units. Read
// as a byte index it lands too far LEFT, so the server answered for whichever
// token happened to sit there -- or for nothing. The rows below put the target
// identifier after a multi-byte character so that the two columns differ, and
// ask four handlers that share the boundary.
func TestCursorPositionIsReadInWireUnits(t *testing.T) {
	for _, tc := range []struct {
		name    string
		content string
		// symbol is the identifier the cursor is placed on.
		symbol string
	}{
		{"after a two-byte rune", "(defun é (x) x)\n(defun target (y) y)\n(é 1) (target 2)\n", "target"},
		{"after a three-byte rune", "(defun 加算 (a) a)\n(defun target (y) y)\n(加算 1) (target 2)\n", "target"},
		{"after a surrogate pair", "(defun 𝛼 (a) a)\n(defun target (y) y)\n(𝛼 1) (target 2)\n", "target"},
		{"after an emoji in a string", "(defun target (y) y)\n(list \"🙂\" (target 2))\n", "target"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// The reference line is the last one: it holds both the non-ASCII
			// text and the call being pointed at.
			lines := 0
			for i := range tc.content {
				if tc.content[i] == '\n' {
					lines++
				}
			}
			refLine := lines - 1
			byteCol := indexOfSymbol(t, lineOf(tc.content, refLine), tc.symbol)

			s := renameTestServer(encodingUTF16)
			uri := fmt.Sprintf("file:///test/cursor-%s.lisp", tc.name)
			openDoc(s, uri, tc.content)
			wire := utf16ColumnOf(lineOf(tc.content, refLine), byteCol)
			require.NotEqual(t, byteCol, wire,
				"fixture must place the symbol where the two columns DIFFER, or it proves nothing")

			pos := protocol.Position{Line: safeUint(refLine), Character: safeUint(wire)}

			t.Run("hover", func(t *testing.T) {
				res, err := s.textDocumentHover(mockContext(), &protocol.HoverParams{
					TextDocumentPositionParams: protocol.TextDocumentPositionParams{
						TextDocument: protocol.TextDocumentIdentifier{URI: uri},
						Position:     pos,
					},
				})
				require.NoError(t, err)
				require.NotNil(t, res, "hover found no symbol at the cursor the client sent")
				content, ok := res.Contents.(protocol.MarkupContent)
				require.True(t, ok)
				assert.Contains(t, content.Value, tc.symbol)
			})

			t.Run("definition", func(t *testing.T) {
				res, err := s.textDocumentDefinition(mockContext(), &protocol.DefinitionParams{
					TextDocumentPositionParams: protocol.TextDocumentPositionParams{
						TextDocument: protocol.TextDocumentIdentifier{URI: uri},
						Position:     pos,
					},
				})
				require.NoError(t, err)
				require.NotNil(t, res, "definition found no symbol at the cursor the client sent")
			})

			t.Run("prepareRename", func(t *testing.T) {
				res, err := s.textDocumentPrepareRename(mockContext(), &protocol.PrepareRenameParams{
					TextDocumentPositionParams: protocol.TextDocumentPositionParams{
						TextDocument: protocol.TextDocumentIdentifier{URI: uri},
						Position:     pos,
					},
				})
				require.NoError(t, err)
				rwp, ok := res.(*protocol.RangeWithPlaceholder)
				require.True(t, ok, "prepareRename declined at the cursor the client sent")
				assert.Equal(t, tc.symbol, rwp.Placeholder)
			})

			t.Run("references", func(t *testing.T) {
				res, err := s.textDocumentReferences(mockContext(), &protocol.ReferenceParams{
					TextDocumentPositionParams: protocol.TextDocumentPositionParams{
						TextDocument: protocol.TextDocumentIdentifier{URI: uri},
						Position:     pos,
					},
					Context: protocol.ReferenceContext{IncludeDeclaration: true},
				})
				require.NoError(t, err)
				assert.NotEmpty(t, res, "references found nothing at the cursor the client sent")
			})
		})
	}
}

// indexOfSymbol returns the byte column of name in line.
func indexOfSymbol(t *testing.T, line, name string) int {
	t.Helper()
	for i := range len(line) - len(name) + 1 {
		if line[i:i+len(name)] == name {
			return i
		}
	}
	t.Fatalf("fixture line %q does not contain %q", line, name)
	return 0
}

// --- the outbound boundary (rename) ------------------------------------

// TestPrepareRenameRangeIsInWireUnits pins the outbound half at the one place
// this PR converts it. prepareRename's range is the oracle the fuzz harness
// compares rename's edits against, and it is what an editor pre-selects, so it
// has to be in the same unit as the edits.
func TestPrepareRenameRangeIsInWireUnits(t *testing.T) {
	const content = "(defun 加算 (a b) (+ a b))\n(加算 1 2)\n"
	// "加算" starts at byte 7 / unit 7 and is 6 bytes / 2 units wide.
	for _, tc := range []struct {
		name      string
		enc       positionEncoding
		wantStart protocol.UInteger
		wantEnd   protocol.UInteger
	}{
		{"utf-16", encodingUTF16, 7, 9},
		{"utf-8", encodingUTF8, 7, 13},
	} {
		t.Run(tc.name, func(t *testing.T) {
			s := renameTestServer(tc.enc)
			const uri = "file:///test/prepare-rename-units.lisp"
			openDoc(s, uri, content)
			res, err := s.textDocumentPrepareRename(mockContext(), &protocol.PrepareRenameParams{
				TextDocumentPositionParams: protocol.TextDocumentPositionParams{
					TextDocument: protocol.TextDocumentIdentifier{URI: uri},
					Position:     protocol.Position{Line: 0, Character: 7},
				},
			})
			require.NoError(t, err)
			rwp, ok := res.(*protocol.RangeWithPlaceholder)
			require.True(t, ok)
			assert.Equal(t, "加算", rwp.Placeholder)
			assert.Equal(t, tc.wantStart, rwp.Range.Start.Character)
			assert.Equal(t, tc.wantEnd, rwp.Range.End.Character,
				"the end of the range must be counted in the same unit as its start")
		})
	}
}
