// Copyright © 2026 The ELPS authors

package libschema

import "github.com/luthersystems/elps/lisp"

// Built-in type names as plain checks (luthersystems/elps#905).
//
// s:int is the string "int", and s:has-key turns it into a new validator,
// which takes a gensym.  A typed formal, s:is? and a match pattern check a
// type name on each call, so they use this table instead: it builds
// nothing and takes no gensym, so later gensym names do not move.  Each
// check accepts what the validator getHandler builds accepts, with the
// same condition (TestTypeNameTable).

// typeNameChecks maps each built-in type name that needs no constraint to
// its check and the noun of its error message.  tagged-value needs a tag,
// so it is not here.
var typeNameChecks = map[string]struct {
	ok   func(v *lisp.LVal) bool
	noun string
}{
	String:    {func(v *lisp.LVal) bool { return v.Type == lisp.LString }, "a string"},
	Int:       {func(v *lisp.LVal) bool { return v.Type == lisp.LInt }, "an integer"},
	Float:     {func(v *lisp.LVal) bool { return v.Type == lisp.LFloat }, "a float"},
	Number:    {func(v *lisp.LVal) bool { return v.Type == lisp.LInt || v.Type == lisp.LFloat }, "a number"},
	Fun:       {func(v *lisp.LVal) bool { return v.Type == lisp.LFun }, "a function"},
	SortedMap: {func(v *lisp.LVal) bool { return v.Type == lisp.LSortMap }, "a sorted map"},
	Array:     {func(v *lisp.LVal) bool { return v.Type == lisp.LArray }, "an array"},
	Bool: {func(v *lisp.LVal) bool {
		return v.Type == lisp.LSymbol && (v.Str == lisp.TrueSymbol || v.Str == lisp.FalseSymbol)
	}, "a boolean"},
	Any:      {func(*lisp.LVal) bool { return true }, "anything"},
	List:     {func(v *lisp.LVal) bool { return v.Type == lisp.LSExpr }, "a list"},
	Sequence: {func(v *lisp.LVal) bool { return v.Type == lisp.LSExpr || v.Type == lisp.LArray }, "a list or vector"},
	Nil:      {func(v *lisp.LVal) bool { return v.IsNil() }, "nil"},
}

// isTypeName reports whether v is a type name typeNameChecks has.
func isTypeName(v *lisp.LVal) bool {
	if v.Type != lisp.LString {
		return false
	}
	_, ok := typeNameChecks[v.Str]
	return ok
}

// checkTypeName checks v against the type name, which isTypeName accepts.
// It returns nil when v passes.
func checkTypeName(env *lisp.LEnv, name string, v *lisp.LVal) *lisp.LVal {
	c := typeNameChecks[name]
	if c.ok(v) {
		return nil
	}
	return env.ErrorConditionf(WrongType, "Input was not %s", c.noun)
}
