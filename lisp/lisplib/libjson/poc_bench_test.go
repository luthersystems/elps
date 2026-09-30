// Copyright © 2026 The ELPS authors

package libjson

import (
	"github.com/luthersystems/elps/lisp"
	"testing"
)

func BenchmarkCodecPoC(b *testing.B) {
	for _, arm := range []string{"separate", "unfused", "fused"} {
		for _, p := range []struct {
			name string
			v    *lisp.LVal
		}{{"frame", typedBenchFrame()}, {"records400", typedBenchRecords(400)}} {
			dump := func(v *lisp.LVal) ([]byte, error) {
				switch arm {
				case "separate":
					return dumpTypedSeparate(v)
				case "unfused":
					v, err := Tag(v)
					if err != nil {
						return nil, err
					}
					return Dump(v, false)
				default:
					return DumpTyped(v)
				}
			}
			load := func(b []byte) (*lisp.LVal, error) {
				switch arm {
				case "separate":
					return loadTypedSeparate(b)
				case "unfused":
					v := LoadWith(b, LoadOpts{Strict: true, ExactIntegers: true})
					if v.Type == lisp.LError {
						return nil, lisp.GoError(v)
					}
					return Untag(v)
				default:
					return LoadTyped(b)
				}
			}
			enc, err := dump(p.v)
			if err != nil {
				b.Fatal(err)
			}
			b.Run(arm+"/dump/"+p.name, func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if _, err := dump(p.v); err != nil {
						b.Fatal(err)
					}
				}
			})
			b.Run(arm+"/load/"+p.name, func(b *testing.B) {
				b.ReportAllocs()
				for b.Loop() {
					if _, err := load(enc); err != nil {
						b.Fatal(err)
					}
				}
			})
		}
	}
}
