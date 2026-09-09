// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// lvalSamples holds one representative value per LType.  It is keyed by LType
// rather than written as a slice so that the coverage assertion below can name
// the type that is missing.
//
// LTypeMax is deliberately absent: it is a bound, not a type.
var lvalSamples = map[lisp.LType]func() *lisp.LVal{
	lisp.LInvalid: func() *lisp.LVal { return &lisp.LVal{} },
	lisp.LInt:     func() *lisp.LVal { return lisp.Int(42) },
	lisp.LFloat:   func() *lisp.LVal { return lisp.Float(1.5) },
	lisp.LError:   func() *lisp.LVal { return lisp.Errorf("boom %d", 1) },
	lisp.LSymbol:  func() *lisp.LVal { return lisp.Symbol("foo") },
	lisp.LQSymbol: func() *lisp.LVal { return lisp.QSymbol("pkg:sym") },
	lisp.LSExpr:   func() *lisp.LVal { return lisp.SExpr([]*lisp.LVal{lisp.Int(1), lisp.Symbol("a")}) },
	lisp.LFun: func() *lisp.LVal {
		return lisp.Fun("f", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
			return lisp.Nil()
		})
	},
	lisp.LQuote:   func() *lisp.LVal { return lisp.Quote(lisp.Quote(lisp.Symbol("foo"))) },
	lisp.LString:  func() *lisp.LVal { return lisp.String("hello") },
	lisp.LBytes:   func() *lisp.LVal { return lisp.Bytes([]byte{0x01, 0x02}) },
	lisp.LSortMap: func() *lisp.LVal { return lisp.SortedMap() },
	lisp.LArray:   func() *lisp.LVal { return lisp.Array(nil, []*lisp.LVal{lisp.Int(1), lisp.Int(2)}) },
	// A POINTER payload: %T renders the type, %v would render the address.
	lisp.LNative: func() *lisp.LVal { return lisp.Native(&struct{ N int }{7}) },
	lisp.LTaggedVal: func() *lisp.LVal {
		return &lisp.LVal{Type: lisp.LTaggedVal, Str: "my-type", Cells: []*lisp.LVal{lisp.Int(1)}}
	},
	lisp.LMarkTerminal: func() *lisp.LVal {
		return &lisp.LVal{Type: lisp.LMarkTerminal, Cells: []*lisp.LVal{lisp.Int(1)}}
	},
	lisp.LMarkTailRec: func() *lisp.LVal {
		return &lisp.LVal{Type: lisp.LMarkTailRec, Cells: []*lisp.LVal{lisp.Int(2), lisp.Symbol("f"), lisp.SExpr(nil)}}
	},
	lisp.LMarkMacExpand: func() *lisp.LVal {
		return &lisp.LVal{Type: lisp.LMarkMacExpand, Cells: []*lisp.LVal{lisp.Symbol("f")}}
	},
}

// TestStringNoAddressForEveryLType is the invariant behind issue #606: no
// LVal renders a heap address, for any LType.
//
// ELPS runs on a ledger where every peer must produce byte-identical output,
// so a rendering that embeds a pointer is nondeterministic across peers BY
// CONSTRUCTION -- the same value renders differently in two processes, and a
// copy renders differently from its source in one.  That is what an LQSymbol
// did: it had no arm in str/strNested and fell through to a default that
// printed %#v of the LVal, which prints LVal.source, a *token.Location.
//
// The loop walks every LType from the source (LType(0) up to the LTypeMax
// bound) rather than a hand-written list, so a newly added type fails here
// until somebody gives it a sample AND a rendering arm; it cannot fall into a
// default arm unnoticed.  A LOCATED value is used for the same reason: an
// unlocated one has a nil source, which prints no address even when the
// rendering is wrong.
func TestStringNoAddressForEveryLType(t *testing.T) {
	for typ := lisp.LType(0); typ < lisp.LTypeMax; typ++ {
		mk, ok := lvalSamples[typ]
		if !ok {
			t.Errorf("LType %d (%s) has no sample in lvalSamples: a new LType needs a sample here and a rendering arm in LVal.str or LVal.strNested", typ, typ)
			continue
		}
		v := mk()
		if v.Type != typ {
			t.Errorf("lvalSamples[%s] built a %s", typ, v.Type)
			continue
		}
		unlocated := v.String()
		v.SetSource(&token.Location{File: "test.lisp", Line: 3, Col: 7, Pos: 11})
		located := v.String()
		if strings.Contains(located, "0x") {
			t.Errorf("%s renders a heap address: %s", typ, located)
		}
		// An error renders its location deliberately, as file:line:col --
		// text, not an address -- so only it is exempt from this half.
		if typ != lisp.LError && located != unlocated {
			t.Errorf("%s renders differently once located:\n  unlocated: %s\n  located:   %s", typ, unlocated, located)
		}
		// A copy holds a different *token.Location, so a rendering that
		// leaked the pointer would differ here even within one process.
		if cp := v.Copy().String(); cp != located {
			t.Errorf("%s renders differently after Copy:\n  original: %s\n  copy:     %s", typ, located, cp)
		}
	}
	for typ := range lvalSamples {
		if typ >= lisp.LTypeMax {
			t.Errorf("lvalSamples holds LType %d, which is not a valid type", typ)
		}
	}
}

// TestQSymbolString pins the rendering an LQSymbol got in issue #606: the
// quoted symbol name, the same text a quoted LSymbol renders, and the same
// text the debugger's inspector has always shown for one.
func TestQSymbolString(t *testing.T) {
	loc := &token.Location{File: "test.lisp", Line: 1, Col: 1}
	qsym := func() *lisp.LVal {
		v := lisp.QSymbol("pkg:sym")
		v.SetSource(loc)
		return v
	}
	tests := []struct {
		name string
		val  *lisp.LVal
		want string
	}{
		{"bare", qsym(), "'pkg:sym"},
		{"in list", lisp.SExpr([]*lisp.LVal{lisp.Symbol("f"), qsym()}), "(f 'pkg:sym)"},
		{"quoted list", lisp.QExpr([]*lisp.LVal{qsym()}), "'('pkg:sym)"},
		{"double quoted", lisp.Quote(qsym()), "''pkg:sym"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if got := test.val.String(); got != test.want {
				t.Errorf("String() = %s, want %s", got, test.want)
			}
		})
	}
}
