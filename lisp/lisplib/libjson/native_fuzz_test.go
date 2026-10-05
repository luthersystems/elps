// Copyright © 2026 The ELPS authors

package libjson

import (
	"encoding/json"
	"errors"
	"math"
	"math/big"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
)

// FuzzNativeEncodeMatchesLegacy compares the native encoder with the one it
// replaced (luthersystems/elps#802) on random lisp trees with native leaves.
//
// The bound walk refuses a native only when it is over the cap or nests past
// nativeNestLimit, and the trees here are small, so the two must agree
// exactly: the same bytes, the same loadable verdict, or the same error text.
// The one exception is under a small cap: the old encoder marshalled a native
// over the cap before it noticed, so where it failed anyway, the new one may
// fail with the cap's error instead.  Where the old one succeeded, the new one
// must succeed with the same bytes.  The leaves cover what the walk has to mirror from encoding/json:
// marshaler dispatch, struct tags and embedding, omitempty and omitzero, map
// key kinds, NaN, invalid numbers, unsupported types and cycles.  A native
// leaf can repeat, which exercises the memo.
func FuzzNativeEncodeMatchesLegacy(f *testing.F) {
	for _, seed := range [][]byte{
		{4, 6, 2, 4, 1},
		{2, 3, 4, 8, 4, 5, 4, 3, 255},
		{2, 4, 4, 14, 5, 0, 4, 9, 1},
		{3, 2, 4, 12, 7, 4, 13},
		{2, 3, 4, 15, 4, 10, 200, 4, 11, 3},
	} {
		f.Add(seed)
	}
	for _, seed := range fuzzseed.Adversarial() {
		f.Add(seed)
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		g := &nativeFuzzGen{data: data}
		v := g.lisp(0)
		for _, sn := range []bool{false, true} {
			for _, b := range []encodeBudget{{}, {maxBytes: 1 << 30}, {maxBytes: 24}, {maxBytes: 96}} {
				legacy := encodeMeter{legacyNatives: true}
				dumped, dumpedErr := DefaultSerializer().dumpLimit(v, sn, dumpOptions{limit: lisp.MaxValueDepth, budget: b, meter: legacy})
				wantB, wantLoadable, wantErr := dumped.bytes, dumped.loadable, dumpedErr
				dumped2, dumpedErr2 := DefaultSerializer().dumpLimit(v, sn, dumpOptions{limit: lisp.MaxValueDepth, budget: b, meter: encodeMeter{}})
				gotB, gotLoadable, gotErr := dumped2.bytes, dumped2.loadable, dumpedErr2
				var size encodeSizeError
				if wantErr != nil && gotErr != nil && errors.As(gotErr, &size) {
					continue
				}
				if (wantErr == nil) != (gotErr == nil) || (wantErr != nil && wantErr.Error() != gotErr.Error()) {
					t.Fatalf("sn=%v cap=%d: error %v, legacy %v", sn, b.maxBytes, gotErr, wantErr)
				}
				if string(wantB) != string(gotB) || wantLoadable != gotLoadable {
					t.Fatalf("sn=%v cap=%d: bytes %q (loadable %v), legacy %q (loadable %v)",
						sn, b.maxBytes, gotB, gotLoadable, wantB, wantLoadable)
				}
			}
		}
	})
}

// nativeFuzzGen turns fuzz bytes into a value.  Exhausted input reads as 0.
type nativeFuzzGen struct {
	data    []byte
	natives []*lisp.LVal
}

func (g *nativeFuzzGen) byte() byte {
	if len(g.data) == 0 {
		return 0
	}
	b := g.data[0]
	g.data = g.data[1:]
	return b
}

// small reads a byte as an int in [-128, 127].
func (g *nativeFuzzGen) small() int {
	return int(g.byte()) - 128
}

func (g *nativeFuzzGen) lisp(depth int) *lisp.LVal {
	op := g.byte() % 6
	if depth > 4 {
		op %= 2
	}
	switch op {
	case 0:
		return lisp.Int(g.small())
	case 1:
		return lisp.String(g.text())
	case 2:
		n := int(g.byte() % 4)
		cells := make([]*lisp.LVal, n)
		for i := range cells {
			cells[i] = g.lisp(depth + 1)
		}
		return lisp.QExpr(cells)
	case 3:
		m := lisp.SortedMap()
		for i := int(g.byte() % 3); i > 0; i-- {
			m.MapSet(g.text(), g.lisp(depth+1))
		}
		return m
	case 4:
		n := lisp.Native(g.goValue(0))
		g.natives = append(g.natives, n)
		return n
	default:
		if len(g.natives) == 0 {
			return lisp.Nil()
		}
		return g.natives[int(g.byte())%len(g.natives)]
	}
}

func (g *nativeFuzzGen) text() string {
	texts := []string{"", "a", "<&>", "\x00\x1f", "\xff", " ", `"\`, "k"}
	return texts[int(g.byte())%len(texts)]
}

// fuzzCounter is a json.Marshaler with a value receiver.
type fuzzCounter struct{ N int }

func (c fuzzCounter) MarshalJSON() ([]byte, error) {
	if c.N < 0 {
		return nil, errors.New("negative counter")
	}
	return json.Marshal(c.N)
}

// fuzzText is a TextMarshaler with a pointer receiver, so encoding/json uses
// it only when the value is addressable.
type fuzzText struct{ S string }

func (t *fuzzText) MarshalText() ([]byte, error) { return []byte("t:" + t.S), nil }

type fuzzInner struct {
	X int     `json:"x,omitempty"`
	Y string  `json:",string"`
	Z float64 `json:"z,omitzero"`
}

type fuzzEmbed struct {
	E any `json:"e"`
}

type fuzzStruct struct {
	A any            `json:"a"`
	B int            `json:"b,string"`
	C fuzzCounter    `json:"c"`
	D fuzzText       `json:"d"`
	M map[string]any `json:"m,omitempty"`
	S any            `json:"-"`
	h int
	fuzzInner
	*fuzzEmbed
}

func (g *nativeFuzzGen) goValue(depth int) any {
	op := g.byte() % 18
	if depth > 3 {
		op %= 5
	}
	switch op {
	case 0:
		return nil
	case 1:
		return g.byte()%2 == 0
	case 2:
		return g.small()
	case 3:
		switch g.byte() % 4 {
		case 0:
			return math.NaN()
		case 1:
			return math.Inf(1)
		case 2:
			return float32(g.byte()) / 3
		default:
			return float64(g.small()) * 1e-7
		}
	case 4:
		return g.text()
	case 5:
		n := int(g.byte() % 3)
		s := make([]any, n)
		for i := range s {
			s[i] = g.goValue(depth + 1)
		}
		return s
	case 6:
		m := map[string]any{}
		for i := int(g.byte() % 3); i > 0; i-- {
			m[g.text()] = g.goValue(depth + 1)
		}
		return m
	case 7:
		return map[int]any{g.small(): g.goValue(depth + 1)}
	case 8:
		s := fuzzStruct{
			A: g.goValue(depth + 1), B: int(g.byte()), C: fuzzCounter{N: g.small()},
			D: fuzzText{S: g.text()}, S: make(chan int), h: 1,
			fuzzInner: fuzzInner{X: int(g.byte() % 2), Y: g.text(), Z: float64(g.byte() % 2)},
		}
		if g.byte()%2 == 0 {
			s.fuzzEmbed = &fuzzEmbed{E: g.goValue(depth + 1)}
		}
		if g.byte()%2 == 0 {
			s.M = map[string]any{"v": g.goValue(depth + 1)}
		}
		return s
	case 9:
		if g.byte()%3 == 0 {
			return (*fuzzStruct)(nil)
		}
		s := &fuzzStruct{A: g.goValue(depth + 1), D: fuzzText{S: g.text()}}
		return s
	case 10:
		nums := []string{"", "1", "-0.5e3", "01", "1.", "abc", "1E1000"}
		return json.Number(nums[int(g.byte())%len(nums)])
	case 11:
		raws := []string{`{"a":1}`, "1E1000", "[", " [ 1 , 2 ] ", "null", `"<&>"`}
		raw := json.RawMessage(raws[int(g.byte())%len(raws)])
		if g.byte()%2 == 0 {
			return &raw
		}
		return raw
	case 12:
		x := big.NewInt(int64(g.small()))
		return x.Lsh(x, uint(g.byte()))
	case 13:
		return make(chan int)
	case 14:
		m := map[string]any{"k": g.goValue(depth + 1)}
		m["self"] = m
		return m
	case 15:
		if g.byte()%2 == 0 {
			return []byte(nil)
		}
		return []byte(g.text())
	case 16:
		return fuzzCounter{N: g.small()}
	default:
		return []fuzzText{{S: g.text()}}
	}
}
