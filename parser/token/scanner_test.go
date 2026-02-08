// Copyright © 2018 The ELPS authors

package token

import (
	"io"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestScannerTokenLength(t *testing.T) {
	const bufsize = 10
	r := byteFiller('x')
	s := newScannerBuf("", r, make([]byte, bufsize))
	var err error
	for i := 0; i < bufsize; i++ {
		err = s.ScanRune()
		if err != nil {
			t.Error(err)
		}
	}
	err = s.ScanRune()
	if err == nil {
		t.Errorf("expected token length error -- got rune: %v", s.Rune())
	}
}

func TestScannerEOF(t *testing.T) {
	r := &io.LimitedReader{
		R: byteFiller('x'),
		N: 10,
	}
	s := newScannerBuf("", r, make([]byte, 20))
	for i := 0; i < 10; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	s.EmitToken(0)

	t.Log(s.start, s.pos, s.next, string(s.buf))
	for i := 0; i < 10; i++ {
		tok := s.EmitToken(0)
		if tok.Text != "" {
			t.Errorf("Bad token text: %q", tok.Text)
		}
		err := s.ScanRune()
		if err != io.EOF {
			t.Fatalf("Not EOF: %q %q %v", s.Rune(), s.buf, err)
		}
		if !s.EOF() {
			t.Fatalf("Scanner does not think it is EOF")
		}
	}
}

func TestScannerAcceptSeq(t *testing.T) {
	r := &io.LimitedReader{
		R: byteFiller('x'),
		N: 10,
	}
	s := newScannerBuf("", r, make([]byte, 20))
	s.AcceptSeq(func(c rune) bool { return true })
	s.Ignore()
	if s.Accept(func(c rune) bool { return true }) {
		t.Fatal("not EOF")
	}
	if !s.EOF() {
		t.Fatal("not EOF")
	}
}

func TestScanner(t *testing.T) {
	r := byteFiller('x')
	s := newScannerBuf("", r, make([]byte, 20))

	var tokens []*Token
	for i := 0; i < 10; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	tokens = append(tokens, s.EmitToken(0))
	for i := 0; i < 7; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	tokens = append(tokens, s.EmitToken(1))
	for i := 0; i < 10; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	tokens = append(tokens, s.EmitToken(2))

	// we haven't technically moved beyond the last rune of the token.
	if s.totalPos != 26 {
		t.Errorf("Bad position: %v", s.totalPos)
	}
	assert.Equal(t, "xxxxxxxxxx", tokens[0].Text)
	assert.Equal(t, 0, tokens[0].Source.Pos)
	assert.Equal(t, "xxxxxxx", tokens[1].Text)
	assert.Equal(t, 10, tokens[1].Source.Pos)
	assert.Equal(t, "xxxxxxxxxx", tokens[2].Text)
	assert.Equal(t, 17, tokens[2].Source.Pos)
}

func TestScannerLoc(t *testing.T) {
	r := newSeqFiller([]byte("123456789\n"))
	s := newScannerBuf("test", r, make([]byte, 15))

	var tokens []*Token
	for i := 0; i < 10; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	tokens = append(tokens, s.EmitToken(0))
	for i := 0; i < 10; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	tokens = append(tokens, s.EmitToken(1))
	for i := 0; i < 5; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	tokens = append(tokens, s.EmitToken(2))
	for i := 0; i < 5; i++ {
		err := s.ScanRune()
		if err != nil {
			t.Fatalf("Scan failure: %v", err)
		}
	}
	tokens = append(tokens, s.EmitToken(3))

	// we haven't technically moved beyond the last rune of the token.
	if s.totalPos != 29 {
		t.Errorf("Bad position: %v", s.totalPos)
	}
	t.Log(tokens[0].Text, tokens[0].Source)
	t.Log(tokens[1].Text, tokens[1].Source)
	t.Log(tokens[2].Text, tokens[2].Source)
	t.Log(tokens[3].Text, tokens[3].Source)
	assert.Equal(t, 0, tokens[0].Source.Pos)
	assert.Equal(t, 10, tokens[1].Source.Pos)
	assert.Equal(t, 20, tokens[2].Source.Pos)
	assert.Equal(t, 25, tokens[3].Source.Pos)
	assert.Equal(t, "test:1:1", tokens[0].Source.String())
	assert.Equal(t, "test:2:1", tokens[1].Source.String())
	assert.Equal(t, "test:3:1", tokens[2].Source.String())
	assert.Equal(t, "test:3:6", tokens[3].Source.String())
}

type byteFiller byte

func (r byteFiller) Read(b []byte) (int, error) {
	for i := range b {
		b[i] = byte(r)
	}
	return len(b), nil
}

type seqFiller struct {
	seq []byte
	rem []byte
}

func newSeqFiller(seq []byte) *seqFiller {
	if len(seq) == 0 {
		panic("empty byte sequnce")
	}
	buf := make([]byte, len(seq))
	copy(buf, seq)
	return &seqFiller{
		seq: buf,
	}
}

func (r *seqFiller) Read(b []byte) (int, error) {
	if len(r.rem) == 0 {
		r.rem = r.seq
	}
	n := copy(b, r.rem)
	r.rem = r.rem[n:]
	return n, nil
}

func TestSeqFiller(t *testing.T) {
	r := newSeqFiller([]byte("xxxxxxxxx\n"))
	buf1 := make([]byte, 12)
	_, err := io.ReadFull(r, buf1)
	if err != nil {
		t.Fatal(err)
	}
	buf2 := make([]byte, 12)
	_, err = io.ReadFull(r, buf2)
	if err != nil {
		t.Fatal(err)
	}
	assert.Equal(t, "xxxxxxxxx\nxxxxxxxxx\nxxxx", string(buf1)+string(buf2))
}
