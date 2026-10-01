// Copyright © 2026 The ELPS authors

package libjson

import (
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

func TestValueWalkerMutatingChargeGoldens(t *testing.T) {
	fixtures := []struct {
		name  string
		build func(*goldenInput) (*lisp.LVal, func())
	}{
		{"vector-separator", func(*goldenInput) (*lisp.LVal, func()) {
			v := lisp.Vector([]*lisp.LVal{lisp.String(strings.Repeat("x", 1021)), lisp.String("old")})
			return v, func() { v.Cells[1].Cells[1] = lisp.Int(9) }
		}},
		{"list-separator", func(*goldenInput) (*lisp.LVal, func()) {
			v := lisp.SExpr([]*lisp.LVal{lisp.String(strings.Repeat("x", 1011)), lisp.String("old")})
			return v, func() { v.Cells[1] = lisp.Int(9) }
		}},
		{"array-data-separator", func(*goldenInput) (*lisp.LVal, func()) {
			v := lisp.Array(lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.Int(2)}), []*lisp.LVal{lisp.String(strings.Repeat("x", 1003)), lisp.String("old")})
			return v, func() { v.Cells[1].Cells[1] = lisp.Int(9) }
		}},
		{"array-data-prelude", func(*goldenInput) (*lisp.LVal, func()) {
			dims := make([]*lisp.LVal, 505)
			for i := range dims {
				dims[i] = lisp.Int(1)
			}
			v := lisp.Array(lisp.QExpr(dims), []*lisp.LVal{lisp.String("old")})
			return v, func() { v.Cells[1].Cells[0] = lisp.Int(9) }
		}},
		{"array-dimension-separator", func(*goldenInput) (*lisp.LVal, func()) {
			dims := make([]*lisp.LVal, 507)
			for i := range dims {
				dims[i] = lisp.Int(1)
			}
			v := lisp.Array(lisp.QExpr(dims), []*lisp.LVal{lisp.String("old")})
			return v, func() { v.Cells[0].Cells[506] = lisp.Int(9) }
		}},
		{"map-key", func(in *goldenInput) (*lisp.LVal, func()) {
			pair := goldenPair(lisp.String(strings.Repeat("x", 1021)), lisp.String("old"))
			v := in.hostMap("charge-map", []*lisp.LVal{pair}, nil)
			return v, func() { pair.Cells[1] = lisp.Int(9) }
		}},
	}
	for _, walker := range []struct {
		name string
		walk func(*lisp.LVal, ...TypedOption) (*lisp.LVal, error)
	}{{"tag", Tag}, {"canonize", Canonize}} {
		t.Run(walker.name, func(t *testing.T) {
			var records []goldenRecord
			for _, f := range fixtures {
				var mutate func()
				in, v := newGoldenInput(goldenFixture{f.name, func(in *goldenInput) *lisp.LVal {
					var v *lisp.LVal
					v, mutate = f.build(in)
					return v
				}})
				calls := 0
				r := goldenObserve(f.name, in, func() (*lisp.LVal, []byte, error) {
					out, err := walker.walk(v, WithTypedCharge(func(n int) error {
						calls++
						in.trace = append(in.trace, "charge:"+strconv.Itoa(n))
						if calls == 2 {
							mutate()
						}
						return nil
					}))
					return out, nil, err
				}, nil)
				r.After, _ = in.render(v)
				records = append(records, r)
			}
			checkWalkerGolden(t, "valwalk-"+walker.name+"-mutating-charge", records)
		})
	}
}
