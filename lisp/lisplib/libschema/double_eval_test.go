// Copyright © 2026 The ELPS authors

package libschema_test

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Issue #737: getHandler evaluated an s-expression constraint (and looked up a
// symbol constraint) a second time, after function application had already
// evaluated the argument.  A quoted form such as '(s:gt 1) or a quoted symbol
// naming a validator was therefore accepted as if it had been written
// unquoted.  Constraints are ordinary evaluated arguments: a quoted form is
// data, not a constraint.
func TestSchemaConstraintNotEvaluatedTwice(t *testing.T) {
	env := newSchemaEnv(t)
	for _, src := range []string{
		`(s:deftype "T1" s:int '(s:gt 1))`,
		`(s:make-validator "T2" s:int '(s:gt 1))`,
		`(s:make-validator "T3" s:sorted-map (s:has-key "k" '(s:gt 1)))`,
		// Refused before #737 too (builtinIsNot checks); a regression guard.
		`(s:make-validator "T4" s:any (s:not '(s:is-true)))`,
		`(progn (s:deftype "T5" s:int) (s:make-validator "T6" s:sorted-map (s:has-key "k" 'T5)))`,
		`(s:make-validator "T7" s:int 's:int)`,
	} {
		res := env.LoadString("test", src)
		require.Equal(t, lisp.LError, res.Type, "quoted constraint accepted: %s -> %v", src, res)
		assert.Equal(t, "bad-arguments", res.Str, "%s", src)
	}
	// Unquoted constraints, and validators bound to symbols, still work.
	res := env.LoadString("test", `(progn (s:deftype "U1" s:int (s:gt 1))
	                                     (s:validate U1 2))`)
	assert.True(t, res.IsNil(), "%v", res)
	res = env.LoadString("test", `(s:validate U1 0)`)
	assert.Equal(t, "failed-constraint", res.Str)
	res = env.LoadString("test", `(s:validate (s:make-validator "U2" s:sorted-map (s:has-key "k" U1)) (sorted-map "k" 5))`)
	assert.True(t, res.IsNil(), "%v", res)
}

// A tagged-value validator's leading type-name string is not a constraint, but
// every constraint after it (and a leading non-string) is checked (#737).
func TestSchemaTaggedConstraintsChecked(t *testing.T) {
	env := newSchemaEnv(t)
	res := env.LoadString("test", `(progn (deftype abc (s) (to-string s))
	                                      (s:make-validator abc s:string (s:in "a")))`)
	require.NotEqual(t, lisp.LError, res.Type, "%v", res)
	for _, src := range []string{
		`(s:make-validator abc s:string '(s:in "a"))`,
		`(s:make-validator abc '(s:in "a"))`,
	} {
		res = env.LoadString("test", src)
		require.Equal(t, lisp.LError, res.Type, "%s -> %v", src, res)
		assert.Equal(t, "bad-arguments", res.Str, "%s", src)
	}
}

// A nested tagged-value type carries more than one leading type name; only
// the constraints after them are checked (review of #737).
func TestSchemaNestedTaggedTypeNames(t *testing.T) {
	env := newSchemaEnv(t)
	for _, src := range []string{
		`(progn (deftype outer (s) s) (s:validate (s:make-validator outer s:tagged-value "string" (s:in "a")) (new outer (new outer "a"))))`,
		`(progn (s:deftype "w" s:tagged-value s:tagged-value s:string (s:in "a")) ())`,
	} {
		res := env.LoadString("test", src)
		assert.True(t, res.IsNil(), "%s -> %v", src, res)
	}
	res := env.LoadString("test", `(s:make-validator "w2" s:tagged-value s:string '(s:in "a"))`)
	assert.Equal(t, "bad-arguments", res.Str, "%v", res)
}

// A validator used as the type argument keeps the constraints after it; they
// used to be dropped silently.
func TestSchemaValidatorTypeKeepsConstraints(t *testing.T) {
	env := newSchemaEnv(t)
	res := env.LoadString("test", `(progn (s:deftype "vi" s:int (s:gt 1)) (s:deftype "vy" vi (s:lt 3)) (s:validate vy 100))`)
	assert.Equal(t, "failed-constraint", res.Str, "%v", res)
	assert.True(t, env.LoadString("test", `(s:validate vy 2)`).IsNil())
	assert.Equal(t, "failed-constraint", env.LoadString("test", `(s:validate vy 0)`).Str)
	res = env.LoadString("test", `(s:make-validator "vz" vi '(s:lt 3))`)
	assert.Equal(t, "bad-arguments", res.Str, "%v", res)
}
