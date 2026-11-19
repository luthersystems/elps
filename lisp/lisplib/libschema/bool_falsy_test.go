package libschema_test

import (
	"context"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// TestBoolAndFalsyBuildNothingPerCall checks that a validation with s:bool or
// s:is-falsy builds no new validator. A new validator takes a name from the
// runtime's symbol counter, so (gensym) after the validations shows it
// (luthersystems/elps#994).
func TestBoolAndFalsyBuildNothingPerCall(t *testing.T) {
	for _, def := range []string{
		`(s:make-validator "T" s:bool)`,
		`(s:make-validator "T" s:bool (s:is-true))`,
		`(s:make-validator "T" s:any (s:is-falsy))`,
	} {
		t.Run(def, func(t *testing.T) {
			gensymAfter := func(n int) string {
				env := newSchemaEnv(t)
				src := "(set 'T " + def + ")" +
					strings.Repeat(" (ignore-errors (s:validate T true))", n) +
					" (gensym)"
				v := env.LoadStringContext(context.Background(), "test", src)
				require.NotEqual(t, lisp.LError, v.Type, "%v", v)
				return v.String()
			}
			assert.Equal(t, gensymAfter(0), gensymAfter(5))
		})
	}
}

// TestBoolAndFalsyResults pins what s:bool with constraints and s:is-falsy
// accept and refuse.
func TestBoolAndFalsyResults(t *testing.T) {
	env := newSchemaEnv(t)
	for _, tc := range []struct {
		typ, constraints, input, want string
	}{
		{"s:bool", "", "true", ""},
		{"s:bool", "", "false", ""},
		{"s:bool", "", `"true"`, "wrong-type"},
		{"s:bool", "", "1", "wrong-type"},
		{"s:bool", "(s:is-true)", "true", ""},
		{"s:bool", "(s:is-true)", "false", "failed-constraint"},
		{"s:bool", "(s:is-false)", "false", ""},
		{"s:bool", "(s:is-true) (s:is-false)", "true", "failed-constraint"},
		{"s:bool", "(s:is-true)", "1", "wrong-type"},
		{"s:any", "(s:is-falsy)", "false", ""},
		{"s:any", "(s:is-falsy)", "()", ""},
		{"s:any", "(s:is-falsy)", "0", ""},
		{"s:any", "(s:is-falsy)", `""`, ""},
		{"s:any", "(s:is-falsy)", `"false"`, ""},
		{"s:any", "(s:is-falsy)", "(sorted-map)", ""},
		{"s:any", "(s:is-falsy)", "true", "failed-constraint"},
		{"s:any", "(s:is-falsy)", "1", "failed-constraint"},
		{"s:any", "(s:is-falsy)", `"x"`, "failed-constraint"},
		{"s:any", "(s:is-falsy)", `(sorted-map "a" 1)`, "failed-constraint"},
	} {
		res := validate(t, env, tc.typ, tc.constraints, tc.input)
		if tc.want == "" {
			assert.True(t, res.IsNil(), "%s %s on %s: %v", tc.typ, tc.constraints, tc.input, res)
			continue
		}
		if assert.Equal(t, lisp.LError, res.Type, "%s %s on %s: %v", tc.typ, tc.constraints, tc.input, res) {
			assert.Equal(t, tc.want, res.Str, "%s %s on %s: %v", tc.typ, tc.constraints, tc.input, res)
		}
	}
}

// BenchmarkSchemaBoolFalsy times one validation with s:bool and with
// s:is-falsy.
func BenchmarkSchemaBoolFalsy(b *testing.B) {
	for _, tc := range []struct{ name, def, input string }{
		{"bool", `(s:make-validator "T" s:bool)`, "true"},
		{"bool-is-true", `(s:make-validator "T" s:bool (s:is-true))`, "true"},
		{"falsy", `(s:make-validator "T" s:any (s:is-falsy))`, "false"},
	} {
		b.Run(tc.name, func(b *testing.B) {
			env := newSchemaEnv(b)
			ctx := context.Background()
			validator := env.LoadStringContext(ctx, "bench", tc.def)
			input := env.LoadStringContext(ctx, "bench", tc.input)
			if validator.Type == lisp.LError || input.Type == lisp.LError {
				b.Fatal(validator, input)
			}
			fun := env.Get(lisp.Symbol("s:validate"))
			args := lisp.QExpr([]*lisp.LVal{validator, input})
			b.ReportAllocs()
			for b.Loop() {
				if res := env.FunCallContext(ctx, fun, args); !res.IsNil() {
					b.Fatal(res)
				}
			}
		})
	}
}
