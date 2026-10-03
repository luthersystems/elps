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

// functionNames returns the raw (still escaped) name of every
// ["~#fn","…"] in b, in order.
func functionNames(b []byte) [][]byte {
	const open = `["~#fn","`
	var names [][]byte
	for {
		i := bytes.Index(b, []byte(open))
		if i < 0 {
			return names
		}
		b = b[i+len(open):]
		j := 0
		for j < len(b) && b[j] != '"' {
			if b[j] == '\\' {
				j++
			}
			j++
		}
		names = append(names, b[:min(j, len(b))])
		b = b[min(j, len(b)):]
	}
}

// sameFunctions fails t unless the masked names of in and out resolve,
// position by position, to one function: the same package and FID.
func sameFunctions(t *testing.T, env *lisp.LEnv, in, out []byte) {
	t.Helper()
	a, b := functionNames(in), functionNames(out)
	if len(a) != len(b) {
		t.Fatalf("function count differs: %q, %q", in, out)
	}
	resolve := func(name []byte) *lisp.LVal {
		doc := append(append([]byte(`["~#durable",[1,["~#fn","`), name...), `"]]]`...)
		f, err := libjson.LoadDurable(env, doc, nil)
		if err != nil {
			t.Fatalf("function %q does not resolve: %v", name, err)
		}
		return f
	}
	for i := range a {
		fa, fb := resolve(a[i]), resolve(b[i])
		if fa.Package() != fb.Package() || fa.FID() != fb.FID() {
			t.Fatalf("function %q restored as %q (%s:%s, %s:%s)", a[i], b[i], fa.Package(), fa.FID(), fb.Package(), fb.FID())
		}
	}
}

// maskFunctionNames replaces the name in every ["~#fn","…"] with "?".  A
// "~#fn" token cannot occur inside a JSON string: its quotes would be
// escaped there.
func maskFunctionNames(b []byte) []byte {
	const open = `["~#fn","`
	var out []byte
	for {
		i := bytes.Index(b, []byte(open))
		if i < 0 {
			return append(out, b...)
		}
		out = append(out, b[:i+len(open)]...)
		b = b[i+len(open):]
		j := 0
		for j < len(b) && b[j] != '"' {
			if b[j] == '\\' {
				j++
			}
			j++
		}
		out = append(out, '?')
		b = b[min(j, len(b)):]
	}
}

// viewGraph builds, from fuzz bytes, a vector with spare capacity and up to
// four list or vector views over overlapping parts of its cells: data[0]
// sets the length, data[1] the spare capacity, and each later triple a
// view's kind, start and end.
func viewGraph(data []byte) *lisp.LVal {
	if len(data) < 2 {
		return nil
	}
	n := 1 + int(data[0]%10)
	cells := make([]*lisp.LVal, n, n+int(data[1]%6))
	for i := range cells {
		cells[i] = lisp.Int(i)
	}
	roots := []*lisp.LVal{{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(n)}), lisp.QExpr(cells)}}}
	for k := 2; k+2 < len(data) && len(roots) < 5; k += 3 {
		i, j := int(data[k+1])%(n+1), int(data[k+2])%(n+1)
		if i > j {
			i, j = j, i
		}
		view := cells[i:j:j]
		if data[k]%2 == 0 {
			roots = append(roots, &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Int(j - i)}), lisp.QExpr(view)}})
		} else if j > i {
			roots = append(roots, lisp.QExpr(view))
		}
	}
	return lisp.QExpr(roots)
}

// writeThrough writes a marker into the first cell of every root that has
// one, and appends to the vector, in place where its capacity allows.  The
// printed result shows which roots share storage.
func writeThrough(v *lisp.LVal) string {
	for k, r := range v.Cells {
		cells := r.Cells
		if r.Type == lisp.LArray {
			cells = r.Cells[1].Cells
		}
		if len(cells) > 0 {
			cells[0] = lisp.Int(100 + k)
		}
	}
	vec := v.Cells[0]
	vec.Cells[1].Cells = append(vec.Cells[1].Cells, lisp.Int(-1))
	vec.Cells[0].Cells[0] = lisp.Int(len(vec.Cells[1].Cells))
	for k, r := range v.Cells {
		cells := r.Cells
		if r.Type == lisp.LArray {
			cells = r.Cells[1].Cells
		}
		if len(cells) > 1 {
			cells[1] = lisp.Int(200 + k)
		}
	}
	return v.String()
}

// checkViews round-trips the view graph data describes and requires that
// writes through the restored values are seen exactly where they are seen
// through the original ones.
func checkViews(t *testing.T, env *lisp.LEnv, data []byte) {
	orig := viewGraph(data)
	if orig == nil {
		return
	}
	b, err := libjson.DumpDurable(env, orig, nil)
	if err != nil {
		t.Fatalf("view graph does not dump: %v", err)
	}
	back, err := libjson.LoadDurable(env, b, nil)
	if err != nil {
		t.Fatalf("view graph does not load: %v\n%s", err, b)
	}
	if again, err := libjson.DumpDurable(env, back, nil); err != nil || !bytes.Equal(again, b) {
		t.Fatalf("view graph is not canonical: %s, %s (%v)", b, again, err)
	}
	if want, got := writeThrough(viewGraph(data)), writeThrough(back); want != got {
		t.Fatalf("sharing differs after restore of %s:\n want %s\n got  %s", b, want, got)
	}
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
//     function name: LoadDurable accepts any name its package binds the
//     function to, and DumpDurable writes the first in sorted order.  So
//     the bytes are compared with every ["~#fn","…"] name masked, each
//     masked name must resolve to the same function (package and FID) as
//     the name in its place, and the names alone must reach a fixed point
//     after one more round trip.
//  4. Roots: LoadDurableRoots accepts a subset of what LoadDurable accepts,
//     and DumpDurableRoots writes an accepted document back byte for byte.
//  5. Charges: two decodes of one input charge the same units in the same
//     order, and the first charge is ceil(n/1024) for n input bytes.
//  6. Limits: a decode under small depth and value limits either fails
//     with ErrTypedLimit where the default decode succeeds, or its value
//     re-encodes under the same limits.  The encoder and the decoder count
//     alike.
//  7. Views: the same bytes also describe a vector and overlapping list
//     and vector views of it (viewGraph).  It round-trips canonically, and
//     writes through the restored values are seen where they are seen
//     through the original ones.
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
		"\x07\x03\x00\x01\x05\x01\x02\x07\x00\x00\x03",
		`["~#durable",[1,["~#list",[["~#view",[["~#obj",[0,["~#cells",4]]],0,4,4,[0,3,2,1]]],["~#view",[["~#ref",0],1,3,3,[]]]]]]]`,
		`["~#durable",[1,["~#list",[["~#array",[[4],["~#view",[["~#obj",[0,["~#cells",6]]],0,4,6,[3,2,1,0,null,null]]]]],["~#array",[[2],["~#view",[["~#ref",0],0,2,2,[]]]]]]]]]`,
		`["~#durable",[1,["~#array",[[4],["~#view",[["~#cells",6],0,4,6,[1,2,3,4,null,null]]]]]]]`,
		`["~#durable",[1,["~#lit",["~#list",[3,2,1]]]]]`,
		`["~#durable",[1,["~#list",[["~#array",[[1],["~#view",[["~#obj",[0,["~#cells",2]]],0,1,2,[0,["~#list",[1]]]]]]],["~#list",[["~#view",[["~#ref",0],1,1,1,[]]]]]]]]]`,
		`["~#durable",[1,["~#array",[[0],["~#lit",["~#view",[["~#cells",1],0,0,1,[null]]]]]]]]`,
		`["~#durable",[1,["~#list",[["~#closure",["user",["~#obj",[0,["~#env",[null,["n",0]]]]],["~#code",[true,null,["~#list",["~$set!","~$n",["~#list",["~$+","~$n",1]]]]]]]],["~#closure",["user",["~#ref",0],["~#code",[true,null,"~$n"]]]]]]]]`,
		`["~#durable",[1,["~#obj",[0,["~#closure",["user",["~#env",[null,["f",["~#ref",0]]]],["~#code",[true,["~#list",["~$n"]],["~#list",["~$f",["~#quote",["~#list",[1,["~#quote","~$x"]]]]]]]]]]]]]]`,
		`["~#durable",[1,["~#closure",["user",null,["~#code",[true,["~#list",["~$a","~$\u0026optional","~$b"]],["~#quote",["~#quote",1]],"~$a"]]]]]]`,
		`["~#durable",[1,["~#error",["my-condition",["boom",42,["~#list",[1,"two"]]]]]]]`,
		`["~#durable",[1,["~#obj",[0,["~#error",["c",[["~#list",[["~#ref",0]]]]]]]]]]`,
		`["~#durable",[1,["~#list",[["~#obj",[0,["~#error",["c",[]]]]],["~#ref",0]]]]]`,
		`["~#durable",[1,["~#list",[["~#lit",["~#view",[["~#obj",[0,["~#cells",3]]],0,3,3,[3,2,1]]]],["~#lit",["~#view",[["~#ref",0],1,2,2,[]]]]]]]]`,
		`["~#durable",[1,["~#list",["a",["~#obj",[0,["~#lit",["~#list",[3,2,1]]]]],"b",["~#ref",0]]]]]`,
		`["~#durable",[1,["~#list",[["~#array",[[2,3],["~#obj",[0,["~#list",[1,2,3,4,5,6]]]]]],["~#array",[[6],["~#ref",0]]]]]]]`,
		`["~#durable",[1,["~#list",[["~#view",[["~#obj",[0,["~#cells",3]]],0,2,2,[1,2]]],["~#view",[["~#ref",0],1,2,2,[null]]]]]]]`,
		`["~#durable",[1,["~#list",[["~#fn","lisp:not"],["~#fn","s:not"]]]]]`,
		`["~#durable",[1,["~#list",[["~#fn","lisp:first"],1.5,"~d2"]]]]`,
		`["~#durable",[1,["~#obj",[0,[["~#obj",[1,["~#list",[["~#obj",[2,["~#list",[["~#ref",1]]]]],["~#ref",0]]]]],["~#native",["test:box",1,["~#ref",2]]]]]]]]`,
		`["~#durable",[1,["~#array",[[0,"~n9007199254740993"],[]]]]]`,
		`["~#durable",[1,["~#array",[[0,"~bAA=="],[]]]]]`,
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
		checkViews(t, env, data)
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
			if derr == nil {
				sameFunctions(t, env, data, enc)
			}
			if derr != nil || !bytes.Equal(maskFunctionNames(enc), maskFunctionNames(data)) {
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
			if !bytes.Equal(maskFunctionNames(enc), maskFunctionNames(data)) {
				t.Fatalf("non-canonical input accepted:\n in  %q\n out %q", data, enc)
			}
			sameFunctions(t, env, data, enc)
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
