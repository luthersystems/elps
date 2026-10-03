// Copyright © 2026 The ELPS authors

package libjson_test

import (
	"bytes"
	"errors"
	"os"
	"slices"
	"strings"
	"testing"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// boxed is a native that holds any value.  Each load makes a new box, so
// the box, not the value it holds, is the native's identity.
type boxed struct{ v *lisp.LVal }

// durableFuzzRegistry registers the codecs typedgolden's corpus uses, each
// strict, so that a payload a codec accepts is the payload it saves again.
// test:counter and test:point rebuild their payloads, so they do not keep
// sharing; test:box returns the payload it was given and keeps it.
func durableFuzzRegistry(t testing.TB) *libjson.DurableRegistry {
	t.Helper()
	reg := libjson.NewDurableRegistry()
	ints := func(p *lisp.LVal, n int) bool {
		if p.Type != lisp.LSExpr || len(p.Cells) != n {
			return false
		}
		for _, c := range p.Cells {
			if c.Type != lisp.LInt {
				return false
			}
		}
		return true
	}
	for _, err := range []error{
		libjson.RegisterNative[*counter](reg, "test:counter", 1, libjson.NativeFuncs{
			Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return lisp.Int(nativeOf[*counter](v).n), nil },
			Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
				if p.Type != lisp.LInt {
					return nil, errors.New("not an int")
				}
				return lisp.Native(&counter{n: p.Int}), nil
			},
		}, libjson.WithNativeCharge(3)),
		libjson.RegisterNative[point](reg, "test:point", 1, libjson.NativeFuncs{
			Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) {
				p := nativeOf[point](v)
				return lisp.QExpr([]*lisp.LVal{lisp.Int(p.x), lisp.Int(p.y)}), nil
			},
			Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) {
				if !ints(p, 2) {
					return nil, errors.New("not a pair of ints")
				}
				return lisp.Native(point{x: p.Cells[0].Int, y: p.Cells[1].Int}), nil
			},
		}),
		libjson.RegisterNative[*boxed](reg, "test:box", 1, libjson.NativeFuncs{
			Save: func(_ *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, error) { return nativeOf[*boxed](v).v, nil },
			Load: func(_ *lisp.LEnv, _ int, p *lisp.LVal) (*lisp.LVal, error) { return lisp.Native(&boxed{p}), nil },
		}, libjson.WithSharedPayload()),
	} {
		if err != nil {
			t.Fatal(err)
		}
	}
	reg.Freeze()
	return reg
}

// FuzzDurableJSON feeds arbitrary bytes to LoadDurable, with strict test
// native codecs registered and a global function defined.
//
// ASSERTIONS
//
//  1. No panic escapes (LoadDurable has no recover of its own).
//  2. On error the value is nil; on success it is not.
//  3. Canonicality: whatever decodes re-encodes to exactly the input bytes,
//     sharing, cycles and natives included.  The one exception is a
//     function: LoadDurable accepts any name its package binds the function
//     to, and DumpDurable writes the first in sorted order.  An input with
//     "~#fn" may re-encode to other bytes, which must then be a fixed point.
//  4. Roots: LoadDurableRoots accepts a subset of what LoadDurable accepts,
//     and DumpDurableRoots writes an accepted document back byte for byte.
//  5. Charges: two decodes of one input charge the same units in the same
//     order, and the first charge is ceil(n/1024) for n input bytes.
//  6. Limits: a decode under small depth and value limits either fails
//     with ErrTypedLimit where the default decode succeeds, or its value
//     re-encodes under the same limits.  The encoder and the decoder count
//     alike.
//
// Decoding is pure Go bounded by the typed limits, and the test codecs do
// constant work, so no watchdog or step budget is needed.
func FuzzDurableJSON(f *testing.F) {
	if b, err := os.ReadFile("typedgolden/testdata/durable.txt"); err == nil {
		for _, line := range strings.Split(strings.TrimSpace(string(b)), "\n") {
			if _, doc, ok := strings.Cut(line, "\t"); ok {
				f.Add([]byte(doc))
			}
		}
	}
	for _, s := range []string{
		`["~#durable",[1,1]]`,
		`["~#durable",[1,["~#list",[["~#fn","lisp:not"],["~#fn","s:not"]]]]]`,
		`["~#durable",[1,["~#fn","lisp:first"]]]`,
		`["~#durable",[1,["~#fn","user:car"]]]`,
		`["~#durable",[1,["~#obj",[0,[["~#obj",[1,["~#list",[["~#ref",0]]]]],["~#native",["test:box",1,["~#ref",1]]]]]]]]`,
		`["~#durable",[1,["~#list",[["~#native",["test:point",1,["~#obj",[0,["~#list",[1,2]]]]]],["~#ref",0]]]]]`,
		`["~#durable",[1,["~#array",[[["~#native",["test:box",1,1]],1],[1]]]]]`,
		`["~#durable",[1,["~#obj",[0,["~#ref",0]]]]]`,
		`["~#durable",[1,["~#list",[["~#obj",[0,["~#list",[3,1]]]],["~#ref",0]]]]]`,
		`["~#durable",[1,"\u003c\u003c\u003c"]]`,
		`["~#durable",[1,["~#obj",[0,{"self":["~#ref",0]}]]]]`,
		`["~#durable",[1,["~#list",[["~#obj",[0,[1]]],["~#ref",0]]]]]`,
		`["~#durable",[1,["~#list",[["~#obj",[0,["~#native",["test:counter",1,7]]]],["~#ref",0]]]]]`,
		`["~#durable",[1,["~#native",["test:point",1,5]]]]`,
		`["~#durable",[1,["~#obj",[0,["~#native",["test:point",1,["~#list",[["~#ref",0],1]]]]]]]]`,
		`["~#durable",[1,["~#fn","user:f"]]]`,
		`["~#durable",[1,["~#fn","lisp:\u003c"]]]`,
		`["~#durable",[1,["~#obj",[1,{}]]]]`,
		`["~#durable",[1,["~#obj",[0,{}]]]]`,
		`["~#durable",[1,["~#ref",0]]]`,
		`["~#durable",[1,["~#obj",[0,["~#obj",[1,{}]]]]]]`,
		`["~#durable",[2,1]]`,
		`["~#durable",[1,["~#obj",[0,["~#array",[[2,1],[["~#ref",0],2]]]]]]]`,
		`["~#durable",[1,["~#obj",[0,["~#tagged",["t",["~#ref",0]]]]]]]`,
		`["~#durable",[1,{"a":["~#obj",[0,"~baGk="]],"b":["~#ref",0]}]]`,
	} {
		f.Add([]byte(s))
	}
	for _, s := range fuzzseed.Adversarial() {
		f.Add(s)
		f.Add(append(append([]byte(`["~#durable",[1,`), s...), ']', ']'))
	}
	env := newTypedTestEnv(f)
	if err := lisp.GoError(env.LoadString("fuzz", `(defun f () 1)`)); err != nil {
		f.Fatal(err)
	}
	reg := durableFuzzRegistry(f)
	small := []libjson.TypedOption{libjson.WithTypedMaxDepth(8), libjson.WithTypedMaxValues(64)}
	f.Fuzz(func(t *testing.T, data []byte) {
		var charges [2][]int
		for i := range charges {
			_, _ = libjson.LoadDurable(env, data, reg, libjson.WithTypedCharge(func(n int) error {
				charges[i] = append(charges[i], n)
				return nil
			}))
		}
		if !slices.Equal(charges[0], charges[1]) {
			t.Fatalf("charges differ between two decodes: %v, %v", charges[0], charges[1])
		}
		if len(data) <= libjson.DefaultTypedMaxBytes && len(data) > 0 &&
			(len(charges[0]) == 0 || charges[0][0] != (len(data)+1023)/1024) {
			t.Fatalf("input charge %v for %d bytes", charges[0], len(data))
		}
		roots, rerr := libjson.LoadDurableRoots(env, data, reg)
		v, err := libjson.LoadDurable(env, data, reg)
		if rerr == nil {
			if err != nil {
				t.Fatalf("roots accepted input LoadDurable rejects: %v", err)
			}
			enc, derr := libjson.DumpDurableRoots(env, roots, reg)
			if derr != nil || (!bytes.Equal(enc, data) && !bytes.Contains(data, []byte(`"~#fn"`))) {
				t.Fatalf("roots do not re-encode:\n in  %q\n out %q (%v)", data, enc, derr)
			}
		}
		if err != nil {
			if v != nil {
				t.Fatalf("error %v with non-nil value", err)
			}
			return
		}
		if v == nil {
			t.Fatal("nil value without error")
		}
		enc, err := libjson.DumpDurable(env, v, reg)
		if err != nil {
			t.Fatalf("decoded value does not re-encode: %v", err)
		}
		if !bytes.Equal(enc, data) {
			if !bytes.Contains(data, []byte(`"~#fn"`)) {
				t.Fatalf("non-canonical input accepted:\n in  %q\n out %q", data, enc)
			}
			v2, lerr := libjson.LoadDurable(env, enc, reg)
			if lerr != nil {
				t.Fatalf("re-encoding does not decode: %v", lerr)
			}
			if enc2, derr := libjson.DumpDurable(env, v2, reg); derr != nil || !bytes.Equal(enc2, enc) {
				t.Fatalf("function names are not a fixed point: %q, %q (%v)", enc, enc2, derr)
			}
			return
		}
		lv, err := libjson.LoadDurable(env, data, reg, small...)
		if err != nil {
			if !errors.Is(err, libjson.ErrTypedLimit) {
				t.Fatalf("small limits fail without a limit error: %v", err)
			}
			if _, derr := libjson.DumpDurable(env, v, reg, small...); derr == nil {
				t.Fatalf("decoder refused under limits the encoder accepts: %v", err)
			}
			return
		}
		again, err := libjson.DumpDurable(env, lv, reg, small...)
		if err != nil || !bytes.Equal(again, data) {
			t.Fatalf("value decoded under limits does not re-encode under them: %q (%v)", again, err)
		}
	})
}
