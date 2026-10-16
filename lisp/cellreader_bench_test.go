// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// BenchmarkCellReader measures one successful read of each kind
// (luthersystems/elps#841).  Put and Get are the argument reads of
// substrate's cc:storage-put and cc:storage-get.  Each body calls the reader
// directly, as a builtin does: a call through a func value would move the
// reader to the heap and measure that allocation instead.
func BenchmarkCellReader(b *testing.B) {
	env := lisp.NewEnv(nil)
	str, sym, i, f := lisp.String("k"), lisp.Symbol("s"), lisp.Int(3), lisp.Float(1.5)
	byt := lisp.Bytes([]byte("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
	b.Run("Put", func(b *testing.B) {
		args := lisp.Cells{str, sym, byt}
		for b.Loop() {
			r := args.Read(env)
			r.Name()
			r.Name()
			r.Text()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("Get", func(b *testing.B) {
		args := lisp.Cells{str, str}
		for b.Loop() {
			r := args.Read(env)
			r.Name()
			r.Name()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("Str", func(b *testing.B) {
		args := lisp.Cells{str}
		for b.Loop() {
			r := args.Read(env)
			r.Str()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("Name", func(b *testing.B) {
		args := lisp.Cells{sym}
		for b.Loop() {
			r := args.Read(env)
			r.Name()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("Text", func(b *testing.B) {
		args := lisp.Cells{byt}
		for b.Loop() {
			r := args.Read(env)
			r.Text()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("Int", func(b *testing.B) {
		args := lisp.Cells{i}
		for b.Loop() {
			r := args.Read(env)
			r.Int()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("Float", func(b *testing.B) {
		args := lisp.Cells{f}
		for b.Loop() {
			r := args.Read(env)
			r.Float()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("Bytes", func(b *testing.B) {
		args := lisp.Cells{byt}
		for b.Loop() {
			r := args.Read(env)
			r.Bytes()
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("OptInt", func(b *testing.B) {
		args := lisp.Cells{i}
		for b.Loop() {
			r := args.Read(env)
			r.OptInt(0)
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
	b.Run("OptName", func(b *testing.B) {
		args := lisp.Cells{str}
		for b.Loop() {
			r := args.Read(env)
			r.OptName("")
			if r.Err().IsError() {
				b.Fatal(r.Err())
			}
		}
	})
}
