package libschema_test

import (
	"context"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libschema"
	"github.com/luthersystems/elps/parser"
)

// BenchmarkSchemaCaptures isolates validator construction from invocation. All
// VM setup and argument construction stay outside the timed loop, so explicit
// capture bookkeeping cannot hide inside parser or environment-load costs.
func BenchmarkSchemaCaptures(b *testing.B) {
	for _, name := range []string{"construct-gt", "construct-in", "construct-map", "validate-map"} {
		b.Run(name, func(b *testing.B) {
			env := lisp.NewEnv(nil)
			env.Runtime.Reader = parser.NewReader()
			for _, value := range []*lisp.LVal{lisp.InitializeUserEnv(env), libschema.LoadPackage(env)} {
				if value.Type == lisp.LError {
					b.Fatal(value)
				}
			}
			ctx := context.Background()
			call := func(name string, values ...*lisp.LVal) *lisp.LVal {
				result := env.FunCallContext(ctx, env.Get(lisp.Symbol("s:"+name)), lisp.QExpr(values))
				if result.Type == lisp.LError {
					b.Fatal(result)
				}
				return result
			}
			allowed := lisp.SortedMap()
			allowed.Map().Set(lisp.String("key"), lisp.Int(1))
			inner := call("in", allowed)
			validator := call("make-validator", lisp.String("T"), lisp.String("sorted-map"), inner)
			var fun, args *lisp.LVal
			switch name {
			case "construct-gt":
				fun, args = env.Get(lisp.Symbol("s:gt")), lisp.QExpr([]*lisp.LVal{lisp.Int(1)})
			case "construct-in":
				fun, args = env.Get(lisp.Symbol("s:in")), lisp.QExpr([]*lisp.LVal{allowed})
			case "construct-map":
				fun, args = env.Get(lisp.Symbol("s:make-validator")), lisp.QExpr([]*lisp.LVal{lisp.String("T"), lisp.String("sorted-map"), inner})
			case "validate-map":
				fun, args = env.Get(lisp.Symbol("s:validate")), lisp.QExpr([]*lisp.LVal{validator, allowed})
			}
			b.ReportAllocs()
			b.ResetTimer()
			var result *lisp.LVal
			for range b.N {
				result = env.FunCallContext(ctx, fun, args)
				if result.Type == lisp.LError {
					b.Fatal(result)
				}
			}
			b.StopTimer()
			if name == "validate-map" {
				if !result.IsNil() {
					b.Fatalf("successful validation returned %v, want nil", result)
				}
			} else {
				if result.Type != lisp.LFun {
					b.Fatalf("constructor returned %v, want validator function", result)
				}
				validator = result
			}
			valid, invalid := allowed, lisp.SortedMap()
			if name == "construct-gt" {
				valid, invalid = lisp.Int(2), lisp.Int(0)
			}
			if got := call("validate", validator, valid); !got.IsNil() {
				b.Fatalf("validator rejected valid input: %v", got)
			}
			bad := env.FunCallContext(ctx, env.Get(lisp.Symbol("s:validate")), lisp.QExpr([]*lisp.LVal{validator, invalid}))
			if bad.Type != lisp.LError || bad.Str != "failed-constraint" {
				b.Fatalf("invalid input returned %v, want failed-constraint", bad)
			}
		})
	}
}
