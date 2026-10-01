package libjson

import (
	"errors"
	"fmt"
	"strconv"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func TestPlainEncodeEveryLType(t *testing.T) {
	tests := []struct {
		v          *lisp.LVal
		plain      string
		stringNums string
		err        string
	}{
		{&lisp.LVal{Type: lisp.LInvalid}, "", "", "invalid type encountered: 'INVALID"},
		{lisp.Int(7), "7", `"7"`, ""},
		{lisp.Float(1.5), "1.5", `"1.5"`, ""},
		{lisp.Errorf("failure"), "", "", "invalid type encountered: 'error"},
		{lisp.Symbol("name"), `"name"`, `"name"`, ""},
		{lisp.QExpr([]*lisp.LVal{lisp.Int(7)}), "[7]", `["7"]`, ""},
		{&lisp.LVal{Type: lisp.LFun}, "", "", "invalid type encountered: 'function"},
		{lisp.Quote(lisp.Quote(lisp.Int(7))), "7", `"7"`, ""},
		{lisp.String("text"), `"text"`, `"text"`, ""},
		{lisp.Bytes([]byte("x")), `"eA=="`, `"eA=="`, ""},
		{lisp.SortedMap(), "{}", "{}", ""},
		{lisp.Vector([]*lisp.LVal{lisp.Int(7)}), "[7]", `["7"]`, ""},
		{lisp.Native("native"), `"native"`, `"native"`, ""},
		{&lisp.LVal{Type: lisp.LTaggedVal, Str: "tag", Cells: []*lisp.LVal{lisp.Int(7)}}, "7", `"7"`, ""},
		{&lisp.LVal{Type: lisp.LMarkTerminal}, "", "", "invalid type encountered: '"},
		{&lisp.LVal{Type: lisp.LMarkTailRec}, "", "", "invalid type encountered: 'marker-tail-recursion"},
		{&lisp.LVal{Type: lisp.LMarkMacExpand}, "", "", "invalid type encountered: 'marker-macro-expansion"},
	}
	if len(tests) != int(lisp.LTypeMax) {
		t.Fatal("fixture table does not cover every type")
	}
	for i, tc := range tests {
		t.Run(strconv.Itoa(i), func(t *testing.T) {
			if tc.v.Type != lisp.LType(i) {
				t.Fatal("fixture type differs from its table index")
			}
			for _, stringNums := range []bool{false, true} {
				want := tc.plain
				if stringNums {
					want = tc.stringNums
				}
				for _, depth := range []int{0, 2 * encodeGuardDepth} {
					v := tc.v
					for range depth {
						v = lisp.QExpr([]*lisp.LVal{v})
					}
					b, err := Dump(v, stringNums)
					if tc.err != "" {
						if err == nil || err.Error() != tc.err || b != nil {
							t.Fatalf("Dump(depth=%d, stringNums=%v) = %q, %v, want nil, %q", depth, stringNums, b, err, tc.err)
						}
						continue
					}
					wrapped := want
					for range depth {
						wrapped = "[" + wrapped + "]"
					}
					if err != nil || string(b) != wrapped {
						t.Fatalf("Dump(depth=%d, stringNums=%v) = %q, %v, want %q, nil", depth, stringNums, b, err, wrapped)
					}
				}
			}
		})
	}
}

func TestPlainEncodeOutOfRange(t *testing.T) {
	for _, typ := range []lisp.LType{lisp.LTypeMax, lisp.LTypeMax + 1, ^lisp.LType(0)} {
		for _, deep := range []bool{false, true} {
			t.Run(fmt.Sprintf("%d/deep=%v", typ, deep), func(t *testing.T) {
				enc := getEncoder(false)
				defer putEncoder(enc)
				defer func() {
					if recover() == nil {
						t.Fatal("encoding an out-of-range tag did not panic")
					}
				}()
				v := &lisp.LVal{Type: typ}
				g := encodeGuard{limit: lisp.MaxValueDepth}
				if deep {
					_ = enc.encodeDeepValue(v, g)
				} else {
					_ = enc.encodeValue(v, g)
				}
			})
		}
	}
}

func TestEncodeMapKeyDefaultTypes(t *testing.T) {
	for _, typ := range []lisp.LType{
		lisp.LInvalid, lisp.LInt, lisp.LFloat, lisp.LError, lisp.LSymbol,
		lisp.LSExpr, lisp.LFun, lisp.LQuote, lisp.LString, lisp.LBytes,
		lisp.LSortMap, lisp.LArray, lisp.LNative, lisp.LTaggedVal,
		lisp.LMarkTerminal, lisp.LMarkTailRec, lisp.LMarkMacExpand,
		lisp.LTypeMax, lisp.LTypeMax + 1, ^lisp.LType(0),
	} {
		t.Run(fmt.Sprintf("%d", typ), func(t *testing.T) {
			enc := getEncoder(false)
			defer putEncoder(enc)
			err := enc.encodeMapKey(&lisp.LVal{Type: typ, Str: "key", Int: 7})
			if typ == lisp.LString || typ == lisp.LSymbol || typ == lisp.LInt {
				want := `"key"`
				if typ == lisp.LInt {
					want = `"7"`
				}
				if err != nil || string(enc.bytes()) != want {
					t.Fatalf("encodeMapKey = %q, %v, want %q, nil", enc.bytes(), err, want)
				}
				return
			}
			want := fmt.Sprintf("invalid map key type: %v", typ)
			if !errors.Is(err, invalidKeyTypeError(typ)) || err.Error() != want || len(enc.bytes()) != 0 {
				t.Fatalf("encodeMapKey = %q, %v, want empty bytes, %q", enc.bytes(), err, want)
			}
		})
	}
}
