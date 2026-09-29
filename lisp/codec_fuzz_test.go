// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"bytes"
	"encoding/hex"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// FuzzCanonicalCodec feeds arbitrary bytes to DecodeCanonical.
//
// ASSERTIONS
//
//  1. No panic escapes (DecodeCanonical has no recover of its own).
//  2. On error the value is nil; on success it is not.
//  3. Canonicality: whatever decodes re-encodes to exactly the input bytes.
//     The decoder accepts only what the encoder produces, so any accepted
//     alias (a non-minimal varint, a long float, misordered keys) fails
//     here.
//  4. Round trip: decoding the re-encoding gives the same bytes again.
//
// Decoding is pure Go and bounded by the codec's limits, so no watchdog or
// step budget is needed.
func FuzzCanonicalCodec(f *testing.F) {
	for _, h := range []string{
		"010100", "0101d804", "01023fc00000", "01033fb999999999999a", "01027fc00000",
		"010280000000", "01040368c3a9", "01050200ff", "010603616263", "0107026162",
		"010800", "0108020102040161", "0109010201020104", "01090100",
		"0109020203010201040106010801" + "0a010c",
		"010a03010e010007016101020401620104", "010a00",
		"010b0a757365723a706f696e740802010201" + "04",
		"010c0178" + "00", "01018000", "01033ff8000000000000", "010a020401620100040161" + "0100",
	} {
		b, err := hex.DecodeString(h)
		if err != nil {
			f.Fatal(err)
		}
		f.Add(b)
	}
	f.Fuzz(func(t *testing.T, data []byte) {
		v, err := lisp.DecodeCanonical(data)
		if err != nil {
			if v != nil {
				t.Fatalf("error %v with non-nil value", err)
			}
			return
		}
		if v == nil {
			t.Fatal("nil value without error")
		}
		enc, err := lisp.EncodeCanonical(v)
		if err != nil {
			t.Fatalf("decoded value does not re-encode: %v (%v)", err, v)
		}
		if !bytes.Equal(enc, data) {
			t.Fatalf("non-canonical input accepted:\n in  %x\n out %x", data, enc)
		}
		v2, err := lisp.DecodeCanonical(enc)
		if err != nil {
			t.Fatalf("re-encoding does not decode: %v", err)
		}
		enc2, err := lisp.EncodeCanonical(v2)
		if err != nil || !bytes.Equal(enc2, enc) {
			t.Fatalf("round trip unstable: %x vs %x (%v)", enc2, enc, err)
		}
	})
}
