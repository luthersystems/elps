// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// malformedHeaders are containers only Go host code can build: an array
// without its dims and data cells, or with either one nil or not a list, and
// a sorted-map without its map data.  Each must be refused with an error by
// ValidateDocument and by every read and write that steps into it, not crash
// the host (luthersystems/elps#963).
func malformedHeaders() map[string]func() *lisp.LVal {
	dims := func() *lisp.LVal { return lisp.QExpr([]*lisp.LVal{lisp.Int(1)}) }
	data := func() *lisp.LVal { return lisp.QExpr([]*lisp.LVal{lisp.Int(7)}) }
	return map[string]func() *lisp.LVal{
		"array-no-cells":      func() *lisp.LVal { return &lisp.LVal{Type: lisp.LArray} },
		"array-one-cell":      func() *lisp.LVal { return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{dims()}} },
		"array-nil-dims":      func() *lisp.LVal { return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{nil, data()}} },
		"array-nil-data":      func() *lisp.LVal { return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{dims(), nil}} },
		"array-dims-not-list": func() *lisp.LVal { return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{lisp.Int(1), data()}} },
		"array-data-not-list": func() *lisp.LVal {
			return &lisp.LVal{Type: lisp.LArray, Cells: []*lisp.LVal{dims(), lisp.String("data")}}
		},
		"map-no-data": func() *lisp.LVal { return &lisp.LVal{Type: lisp.LSortMap} },
	}
}

func TestValidateDocumentRefusesMalformedHeaders(t *testing.T) {
	t.Parallel()
	for name, build := range malformedHeaders() {
		for _, wrap := range []struct {
			name string
			fn   func(*lisp.LVal) *lisp.LVal
		}{
			{"top", func(v *lisp.LVal) *lisp.LVal { return v }},
			{"in-vector", func(v *lisp.LVal) *lisp.LVal { return lisp.Vector([]*lisp.LVal{lisp.Int(1), v}) }},
			{"in-map", func(v *lisp.LVal) *lisp.LVal {
				m := lisp.SortedMap()
				m.Map().Set(lisp.String("k"), v)
				return m
			}},
		} {
			t.Run(name+"/"+wrap.name, func(t *testing.T) {
				var err error
				func() {
					defer func() {
						if r := recover(); r != nil {
							t.Fatalf("ValidateDocument panicked: %v", r)
						}
					}()
					err = ValidateDocument(wrap.fn(build()))
				}()
				if err == nil || !strings.Contains(err.Error(), "invalid document: malformed") {
					t.Fatalf("ValidateDocument = %v, want an invalid document: malformed error", err)
				}
			})
		}
	}
}

func TestMalformedHeadersNeverPanic(t *testing.T) {
	t.Parallel()
	for name, build := range malformedHeaders() {
		for _, src := range []string{
			`(elpspath:? d)`,
			`(elpspath:? d 0)`,
			`(elpspath:? d "k")`,
			`(elpspath:? d '*)`,
			`(elpspath:? d '(range 0))`,
			`(elpspath:? d '(range 0 1))`,
			`(elpspath:?set d 0 9)`,
			`(elpspath:?set d "k" 9)`,
			`(elpspath:?set! d 0 9)`,
			`(elpspath:?del d 0)`,
			`(elpspath:?nil d '(range 0))`,
			`(elpspath:?set (vector 1 2) 0 d)`,
			`(elpspath:? (vector 1 d) 1 0)`,
			`(elpspath:? (sorted-map "k" d) "k" "j")`,
		} {
			env := noPanicEnv(t)
			env.PutGlobal(lisp.Symbol("d"), build())
			res := evalNoPanic(t, env, name, src)
			if res.Type != lisp.LError {
				t.Errorf("%s: %s = %v, want an error", name, src, res)
			}
		}
		// Under an iterator, an element the rest of the path cannot read
		// is (), as for a Go nil cell.
		env := noPanicEnv(t)
		env.PutGlobal(lisp.Symbol("d"), build())
		src := `(elpspath:? (vector 1 d) '* 0)`
		if res := evalNoPanic(t, env, name, src); res.String() != "(vector () ())" {
			t.Errorf("%s: %s = %v, want (vector () ())", name, src, res)
		}
	}
}

// Reads that step into a malformed header through the Go Path API, where
// nothing recovers a panic.
func TestMalformedHeadersPathAPI(t *testing.T) {
	t.Parallel()
	for name, build := range malformedHeaders() {
		for _, sel := range []string{".k", ".[0]", ".[]", ".[0:1]"} {
			t.Run(name+"/"+sel, func(t *testing.T) {
				p, err := ParseSelector(sel)
				if err != nil {
					t.Fatal(err)
				}
				func() {
					defer func() {
						if r := recover(); r != nil {
							t.Fatalf("Get panicked: %v", r)
						}
					}()
					if _, err := p.Get(build()); err == nil {
						t.Fatal("Get answered on a malformed header")
					}
				}()
			})
		}
	}
}

// A typed nil *MapData is not malformed: lisp reads it as an empty map.
func TestValidateDocumentAcceptsNilMapData(t *testing.T) {
	t.Parallel()
	if err := ValidateDocument(lisp.SortedMapFromData(nil)); err != nil {
		t.Fatalf("ValidateDocument(empty map from nil data) = %v, want nil", err)
	}
}
