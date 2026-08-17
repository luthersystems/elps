// Copyright © 2026 The ELPS authors

package lsp

import (
	"encoding/json"
	"os"
	"strings"
	"unicode/utf8"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// Position encoding (elps#464).
//
// LSP counts Position.character in UTF-16 code units. This server computes
// columns in BYTES from end to end: Scanner.LocStart derives Col from
// s.totalPos, which advances by the byte width of each rune, and position.go
// slices lines by byte throughout. On a line holding any non-ASCII character
// the two disagree, in both directions, by bytes-minus-UTF-16-units for the
// text to the left of the position.
//
// Two things are needed to close that, and this file holds both:
//
//  1. NEGOTIATION. LSP 3.17 lets a server declare `positionEncoding: "utf-8"`
//     when the client offers it, which makes the byte columns this server
//     already produces the correct answer with no conversion at all. glsp's
//     protocol_3_16 structs have neither the client capability nor the server
//     one -- but the raw initialize params are available on glsp.Context, and
//     server capabilities are already extended by hand for inlayHintProvider,
//     so both halves are reachable without changing protocol version. See
//     clientPositionEncodings and Server.initialize.
//
//  2. CONVERSION, for every client that does not offer utf-8 -- which is every
//     LSP 3.16 client, and the default this server must assume when nothing is
//     negotiated. utf16ColumnOf and byteColumnOf below are that conversion, and
//     Server.cursorAt / Server.byteColumnFromWire / Server.wireRange are the
//     encoding-aware wrappers handlers call: under utf-8 they are the
//     identity, so the negotiated path costs nothing and cannot be broken by a
//     conversion bug.
//
// SCOPE. This file is complete and this PR applies it to the INBOUND direction
// everywhere and to the OUTBOUND direction only on the rename path. Which
// outbound ranges still ship byte columns, and why, is enumerated in the PR
// body and in the comment on Server.wireRange.

// positionEncoding is the unit in which the client and this server agree to
// count Position.character.
type positionEncoding int32

const (
	// encodingUTF16 is what LSP specifies and the only thing LSP 3.16 has.
	// It is the zero value deliberately: a server that has not completed a
	// negotiation -- because the client is 3.16, because it offered no
	// encodings, or because initialize never ran -- owes the client UTF-16,
	// so the safe answer has to be the one you get by default.
	encodingUTF16 positionEncoding = iota

	// encodingUTF8 is selected only when the client explicitly lists "utf-8"
	// in its 3.17 general.positionEncodings capability. It makes every
	// conversion here the identity.
	encodingUTF8
)

// encodingUTF8Name and encodingUTF16Name are the wire spellings from the LSP
// 3.17 PositionEncodingKind enumeration.
const (
	encodingUTF8Name  = "utf-8"
	encodingUTF16Name = "utf-16"
)

// clientPositionEncodings reads capabilities.general.positionEncodings out of
// the RAW initialize params.
//
// It has to read the raw JSON because glsp's protocol_3_16
// GeneralClientCapabilities predates the field (it arrived in 3.17) and
// encoding/json drops what the struct does not name, so by the time initialize
// receives its typed *protocol.InitializeParams the list is gone. glsp keeps
// the undecoded params on the Context, which is the only place it survives.
//
// A malformed or absent capabilities block yields nil, which selects UTF-16 --
// the same answer as a client that said nothing, and the same answer LSP 3.16
// mandates.
func clientPositionEncodings(raw json.RawMessage) []string {
	if len(raw) == 0 {
		return nil
	}
	var params struct {
		Capabilities struct {
			General struct {
				PositionEncodings []string `json:"positionEncodings"`
			} `json:"general"`
		} `json:"capabilities"`
	}
	if err := json.Unmarshal(raw, &params); err != nil {
		return nil
	}
	return params.Capabilities.General.PositionEncodings
}

// selectPositionEncoding picks the encoding to use from the list the client
// offered.
//
// utf-8 is preferred whenever it is on offer, because it is this server's
// native unit: choosing it means no column is converted anywhere, so the
// negotiated path cannot be broken by a bug in the conversion. Any other list,
// including an empty one, gives UTF-16.
func selectPositionEncoding(offered []string) positionEncoding {
	for _, name := range offered {
		if name == encodingUTF8Name {
			return encodingUTF8
		}
	}
	return encodingUTF16
}

// positionEncodingName returns the wire spelling of an encoding.
func positionEncodingName(enc positionEncoding) string {
	if enc == encodingUTF8 {
		return encodingUTF8Name
	}
	return encodingUTF16Name
}

// lineOf returns the text of the 0-based line within content, without its
// trailing newline, or "" when the line does not exist.
//
// Lines are split on "\n" exactly as the rest of this package does, so a CRLF
// document leaves the "\r" on the end of the line. That is harmless here: "\r"
// is ASCII, so it changes neither a byte column nor a UTF-16 column.
func lineOf(content string, line int) string {
	if line < 0 {
		return ""
	}
	for range line {
		nl := strings.IndexByte(content, '\n')
		if nl < 0 {
			return ""
		}
		content = content[nl+1:]
	}
	if nl := strings.IndexByte(content, '\n'); nl >= 0 {
		return content[:nl]
	}
	return content
}

// isASCIIOnly reports whether s is entirely single-byte characters, in which
// case a byte column and a UTF-16 column are the same number and both
// conversions below are the identity. Almost every line of almost every source
// file takes this path.
func isASCIIOnly(s string) bool {
	for i := range len(s) {
		if s[i] >= utf8.RuneSelf {
			return false
		}
	}
	return true
}

// utf16RuneLen is the number of UTF-16 code units a rune occupies: 2 for
// anything outside the Basic Multilingual Plane (emoji, the mathematical
// alphabets, CJK extension blocks), 1 for everything else.
//
// utf8.DecodeRuneInString yields (RuneError, 1) for a byte that starts no
// valid sequence, and RuneError is in the BMP, so an invalid byte counts as
// exactly one unit. That keeps both conversions total and mutually inverse on
// invalid UTF-8 rather than making them disagree about how wide the damage is.
func utf16RuneLen(r rune) int {
	if r > 0xFFFF {
		return 2
	}
	return 1
}

// utf16LenOf is the width of a whole line in UTF-16 code units.
func utf16LenOf(line string) int {
	if isASCIIOnly(line) {
		return len(line)
	}
	units := 0
	for _, r := range line {
		units += utf16RuneLen(r)
	}
	return units
}

// utf16ColumnOf converts a 0-based BYTE column within line into the 0-based
// UTF-16 code-unit column LSP asks for. It is the OUTBOUND half.
//
// Total by construction, because the columns reaching it are computed from
// token offsets rather than supplied by a client, and a conversion that
// panicked on a stale offset would take the server down:
//
//   - at or before the start of the line -> 0;
//   - past the end of the line -> the line's width plus the overshoot, so a
//     column that was strictly greater than another stays strictly greater and
//     a range cannot invert itself;
//   - inside a multi-byte rune -> the start of that rune, since a position
//     part-way through a character has no UTF-16 column of its own.
func utf16ColumnOf(line string, byteCol int) int {
	if byteCol <= 0 {
		return 0
	}
	if byteCol >= len(line) {
		return utf16LenOf(line) + (byteCol - len(line))
	}
	if isASCIIOnly(line[:byteCol]) {
		return byteCol
	}
	units := 0
	for i := 0; i < byteCol; {
		r, size := utf8.DecodeRuneInString(line[i:])
		if i+size > byteCol {
			break // byteCol splits a rune: round down to its start
		}
		units += utf16RuneLen(r)
		i += size
	}
	return units
}

// byteColumnOf converts a 0-based UTF-16 code-unit column into the 0-based
// BYTE column this package uses internally. It is the INBOUND half and the
// exact inverse of utf16ColumnOf at every rune boundary.
//
// The same three edge rules apply, for the same reason -- a client may send
// any column at all, including one past the end of the line, which editors do
// routinely when the cursor sits at end of line:
//
//   - at or before the start -> 0;
//   - past the end -> the line's byte length plus the overshoot;
//   - between the two halves of a surrogate pair -> the start of that rune.
func byteColumnOf(line string, u16Col int) int {
	if u16Col <= 0 {
		return 0
	}
	if isASCIIOnly(line) {
		return u16Col
	}
	units := 0
	for i := 0; i < len(line); {
		if units >= u16Col {
			return i
		}
		r, size := utf8.DecodeRuneInString(line[i:])
		if units+utf16RuneLen(r) > u16Col {
			return i // u16Col splits a surrogate pair: round down
		}
		units += utf16RuneLen(r)
		i += size
	}
	return len(line) + (u16Col - units)
}

// positionEncoding returns the encoding negotiated during initialize, or
// UTF-16 when nothing was negotiated.
func (s *Server) positionEncoding() positionEncoding {
	return positionEncoding(s.posEncoding.Load())
}

// byteColumnFromWire converts a column as the CLIENT counted it into the byte
// column this package uses internally. Handlers call it exactly once, on the
// Position.character they read out of the request, after which every column in
// the request is in the server's own unit and nothing downstream changes.
func (s *Server) byteColumnFromWire(content string, line, wireCol int) int {
	if s.positionEncoding() == encodingUTF8 {
		return wireCol
	}
	return byteColumnOf(lineOf(content, line), wireCol)
}

// cursorAt converts a Position out of a request into the 0-based line and
// 0-based BYTE column this package works in.
//
// This is the whole INBOUND boundary for elps#464: every handler that reads a
// Position.character calls it exactly once, on entry, and everything
// downstream -- symbolAtPosition, wordAtPosition, scopeAtPosition,
// nodesAtPosition, enclosingCall -- keeps working in bytes, unchanged. A
// client's column is in UTF-16 code units unless it negotiated utf-8, so
// without this a cursor on a line holding any non-ASCII character was read as
// a byte index and hover, definition, references, rename and completion
// answered for the wrong token or for none.
func (s *Server) cursorAt(doc *Document, pos protocol.Position) (line, col int) {
	line = int(pos.Line)
	col = int(pos.Character)
	if doc == nil || s.positionEncoding() == encodingUTF8 {
		return line, col
	}
	doc.mu.Lock()
	content := doc.Content
	doc.mu.Unlock()
	return line, byteColumnOf(lineOf(content, line), col)
}

// wireRange converts a range whose columns are BYTE columns into the client's
// encoding, using text as the document the range points into.
//
// Both ends are converted together and against their own lines, which is the
// property elps#470 was about: a range whose two ends are counted differently
// deletes part of an identifier whatever the wire encoding turns out to be.
//
// This is applied ONLY on the rename path (see rename.go). Every other
// outbound range in this package still ships byte columns; they are cosmetic
// misplacements under UTF-16 rather than edits applied to a file unread, and
// converting them means converting semantic-token lengths in the same breath
// (elps#462 keeps those in bytes deliberately, so that a length agrees with
// the column it is added to). That remainder is listed in the PR body.
func (s *Server) wireRange(text string, rng protocol.Range) protocol.Range {
	if s.positionEncoding() == encodingUTF8 {
		return rng
	}
	rng.Start.Character = safeUint(utf16ColumnOf(lineOf(text, int(rng.Start.Line)), int(rng.Start.Character)))
	rng.End.Character = safeUint(utf16ColumnOf(lineOf(text, int(rng.End.Line)), int(rng.End.Character)))
	return rng
}

// documentTexts caches the text of every document a single request needs to
// convert ranges against, so a rename touching one file forty times reads it
// once.
type documentTexts struct {
	srv   *Server
	texts map[string]string
}

func (s *Server) newDocumentTexts() *documentTexts {
	return &documentTexts{srv: s, texts: make(map[string]string)}
}

// get returns the text of the document a URI names and whether it was found.
//
// An open document is authoritative: its content is what the client has in its
// buffer, which is what the client will apply an edit to. A file that is not
// open is read from disk, which is both the client's buffer (a client with
// unsaved edits would have sent didOpen) and the exact text the workspace
// index derived the range from, so the conversion is self-consistent with the
// column it is converting.
//
// A file that cannot be read yields false. The caller leaves the range in byte
// columns in that case: unconverted is what every range on main is, so it
// cannot be a regression, and dropping the edit instead would silently do half
// a rename.
func (d *documentTexts) get(uri string) (string, bool) {
	if text, ok := d.texts[uri]; ok {
		return text, text != ""
	}
	if doc := d.srv.docs.Get(uri); doc != nil {
		doc.mu.Lock()
		text := doc.Content
		doc.mu.Unlock()
		d.texts[uri] = text
		return text, true
	}
	source, err := os.ReadFile(uriToPath(uri))
	if err != nil {
		d.texts[uri] = ""
		return "", false
	}
	d.texts[uri] = string(source)
	return string(source), true
}

// rangeFor converts a byte-column range for the document a URI names, leaving
// it untouched when that document's text cannot be obtained.
func (d *documentTexts) rangeFor(uri string, rng protocol.Range) protocol.Range {
	if d.srv.positionEncoding() == encodingUTF8 {
		return rng
	}
	text, ok := d.get(uri)
	if !ok {
		return rng
	}
	return d.srv.wireRange(text, rng)
}
