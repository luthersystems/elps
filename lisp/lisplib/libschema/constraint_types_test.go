// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// validate runs (s:validate (s:make-validator "x" <typ> <constraints>) <input>).
func validate(t *testing.T, env *lisp.LEnv, typ, constraints, input string) *lisp.LVal {
	t.Helper()
	return env.LoadString("test", `(s:validate (s:make-validator "x" `+typ+` `+constraints+`) `+input+`)`)
}

// s:bool, s:is-true and s:is-false compared input.Str with "true"/"false"
// without checking the type, so the STRINGS "true" and "false" passed.  A JSON
// document {"ok": "true"} therefore satisfied (s:has-key "ok" s:bool).
func TestSchemaBoolRejectsStrings(t *testing.T) {
	env := newSchemaEnv(t)
	for _, in := range []string{`"true"`, `"false"`, `(get (json:load-string "{\"b\":\"true\"}") "b")`} {
		res := validate(t, env, "s:bool", "", in)
		require.Equal(t, lisp.LError, res.Type, "s:bool accepted string %s", in)
		assert.Equal(t, "wrong-type", res.Str)
	}
	for _, c := range []struct{ constraint, input string }{
		{"(s:is-true)", `"true"`},
		{"(s:is-false)", `"false"`},
	} {
		res := validate(t, env, "s:any", c.constraint, c.input)
		require.Equal(t, lisp.LError, res.Type, "%s accepted %s", c.constraint, c.input)
		assert.Equal(t, "failed-constraint", res.Str)
	}
	// A map key checked through s:has-key.
	res := env.LoadString("test", `(s:validate (s:make-validator "m" s:sorted-map (s:has-key "ok" s:bool))
	                                           (json:load-string "{\"ok\":\"true\"}"))`)
	assert.Equal(t, lisp.LError, res.Type, "s:has-key with s:bool accepted a string")

	// Real booleans still pass, however they are spelled.
	for _, in := range []string{`true`, `false`, `'true`, `(json:load-string "true")`} {
		res := validate(t, env, "s:bool", "", in)
		assert.True(t, res.IsNil(), "s:bool rejected %s: %v", in, res)
	}
	assert.True(t, validate(t, env, "s:any", "(s:is-true)", "true").IsNil())
	assert.True(t, validate(t, env, "s:any", "(s:is-false)", "false").IsNil())
	// Other symbols are still not booleans.
	assert.Equal(t, lisp.LError, validate(t, env, "s:bool", "", "'yes").Type)
}

// Every numeric constraint failed only when a "bad" comparison was true, and
// every comparison with NaN is false, so NaN satisfied s:positive AND
// s:negative, s:gt AND s:lt -- and a NaN bound accepted every input.
func TestSchemaNumericConstraintsRejectNaN(t *testing.T) {
	env := newSchemaEnv(t)
	for _, c := range []string{"(s:positive)", "(s:negative)", "(s:gt 0)", "(s:gte 0)", "(s:lt 0)", "(s:lte 0)"} {
		t.Run(c, func(t *testing.T) {
			res := validate(t, env, "s:number", c, "(/ 0.0 0.0)")
			require.Equal(t, lisp.LError, res.Type, "NaN satisfied %s", c)
			assert.Equal(t, "failed-constraint", res.Str)
		})
	}
	// The same through s:float, the type a JSON-decoded number has.
	res := validate(t, env, "s:float", "(s:gte 0)", `(json:load-string "0")`)
	require.True(t, res.IsNil(), "%v", res)
	res = validate(t, env, "s:float", "(s:positive)", "(/ 0.0 0.0)")
	assert.Equal(t, lisp.LError, res.Type, "NaN satisfied (s:positive) under s:float")

	// A NaN bound is refused when the constraint is built.
	for _, c := range []string{"s:gt", "s:gte", "s:lt", "s:lte"} {
		t.Run(c+" NaN bound", func(t *testing.T) {
			res := env.LoadString("test", `(`+c+` (/ 0.0 0.0))`)
			require.Equal(t, lisp.LError, res.Type, "(%s NaN) was accepted", c)
			assert.Equal(t, "failed-constraint", res.Str)
		})
	}
	// Ordinary numbers, including infinities, keep their answers.
	for _, tc := range []struct {
		constraint, input string
		pass              bool
	}{
		{"(s:positive)", "1", true},
		{"(s:positive)", "0", false},
		{"(s:positive)", "(/ 1.0 0.0)", true},
		{"(s:negative)", "(/ -1.0 0.0)", true},
		{"(s:negative)", "-0.5", true},
		{"(s:gt 1)", "1", false},
		{"(s:gte 1)", "1", true},
		{"(s:lt 1.5)", "1", true},
		{"(s:lte 1)", "1.0", true},
		{"(s:lte 1)", "2", false},
	} {
		res := validate(t, env, "s:number", tc.constraint, tc.input)
		assert.Equal(t, tc.pass, res.IsNil(), "%s %s: %v", tc.constraint, tc.input, res)
	}
}

// s:is-truthy documents "non-empty arrays/maps/bytes" as truthy, but tested
// len(input.Cells) for maps and bytes -- always 0, because a map lives in
// Map() and bytes in Bytes() -- so every map and bytes value was "not truthy"
// and s:is-falsy accepted non-empty ones.  The array case indexed the first
// dimension unguarded, so a zero-dimensional array panicked.
func TestSchemaTruthyMapsBytesAndArrays(t *testing.T) {
	env := newSchemaEnv(t)
	truthy := func(v string) *lisp.LVal { return validate(t, env, "s:any", "(s:is-truthy)", v) }
	falsy := func(v string) *lisp.LVal { return validate(t, env, "s:any", "(s:is-falsy)", v) }
	for _, v := range []string{`(sorted-map "a" 1)`, `(json:load-string "{\"a\":1}")`, `(to-bytes "a")`, `(vector 1)`, `true`} {
		assert.True(t, truthy(v).IsNil(), "non-empty %s is not truthy: %v", v, truthy(v))
		assert.Equal(t, lisp.LError, falsy(v).Type, "non-empty %s is falsy", v)
	}
	for _, v := range []string{`(sorted-map)`, `(json:load-string "{}")`, `(to-bytes "")`, `(vector)`, `false`} {
		assert.Equal(t, lisp.LError, truthy(v).Type, "empty %s is truthy", v)
		assert.True(t, falsy(v).IsNil(), "empty %s is not falsy: %v", v, falsy(v))
	}

	// Strings keep their documented rule: non-empty and not "false".
	assert.True(t, truthy(`"yes"`).IsNil())
	assert.Equal(t, lisp.LError, truthy(`"false"`).Type)
	assert.Equal(t, lisp.LError, truthy(`""`).Type)

	// Arrays built through the Go API: a zero-dimensional array holds one
	// element, and a multi-dimensional array is empty when any dimension is.
	for _, tc := range []struct {
		name   string
		dims   []*lisp.LVal
		cells  []*lisp.LVal
		truthy bool
	}{
		{"zero-dimensional", nil, []*lisp.LVal{lisp.Int(1)}, true},
		{"2x2", []*lisp.LVal{lisp.Int(2), lisp.Int(2)}, []*lisp.LVal{lisp.Int(1), lisp.Int(2), lisp.Int(3), lisp.Int(4)}, true},
		{"2x0", []*lisp.LVal{lisp.Int(2), lisp.Int(0)}, nil, false},
	} {
		t.Run(tc.name, func(t *testing.T) {
			arr := lisp.Array(lisp.QExpr(tc.dims), tc.cells)
			require.NotEqual(t, lisp.LError, arr.Type, "%v", arr)
			env.PutGlobal(lisp.Symbol("arr"), arr)
			res := truthy("arr")
			require.False(t, lisp.IsInternalPanic(res), "is-truthy panicked: %v", res)
			assert.Equal(t, tc.truthy, res.IsNil(), "%v", res)
		})
	}
}
