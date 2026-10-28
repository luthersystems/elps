// Copyright © 2026 The ELPS authors

package libschema

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
)

// Typed method arguments (luthersystems/elps#905).
//
// A formal after the receiver is a name, or [name type].  The type is any
// value s:has-key accepts as a type, or :self for the type being defined.
// A type is resolved once, when the method is made; [name 'T] resolves T
// on each call instead, so it can name a type defined later.  A record type
// is checked as s:validate checks it: the type, the fields and :check.
// (s:ref T) checks the type only.  Each argument a call supplies is
// checked, an explicit () too; an absent &optional or &key argument is
// not.  &rest [xs type] checks each element, at one step each.
//
// The checks run in Go, after the argument count and before the body, so a
// bad call changes nothing.  A check costs no ELPS steps, except one step
// per &rest element and the steps of the Lisp code a check runs: a late
// type's lookup, a Lisp validator or a record type's :check.  A Lisp method keeps
// its types in its type's captures, so each fork of a template has its own;
// a Go method has check functions, as FieldSpec has.  Both run through
// argCheck.supplied and argError, so they fail the same way.

// ErrFailedConstraint marks the error of a Go check (FieldSpec.Check,
// RecordSpec.Check, MethodSpec.ArgChecks) as a failed constraint: wrap it,
// as in fmt.Errorf("%w: not positive", ErrFailedConstraint), and the error
// value has the condition failed-constraint.  Any other error has the
// condition wrong-type.  A Lisp validator makes the same choice: a value
// of the wrong type is wrong-type, a value that breaks a rule is
// failed-constraint.
var ErrFailedConstraint = errors.New("failed constraint")

// goCheckCondition returns the condition for the error of a Go check.
func goCheckCondition(err error) string {
	if errors.Is(err, ErrFailedConstraint) {
		return FailedConstraint
	}
	return WrongType
}

// argCheck is one checked formal after the receiver.
type argCheck struct {
	name string
	kind lisp.ParamKind
	// pos is the index of a required or optional argument, and the index
	// of the first element of a rest argument.
	pos int
}

// formalSlots returns the argCheck of each formal in formals, a formal list
// with no receiver, and npos, the number of positional formals.  The
// keyword arguments of a call start at npos.
func formalSlots(formals *lisp.LVal) (slots []argCheck, npos int) {
	for _, p := range lisp.ParseFormals(formals) {
		c := argCheck{name: p.Name, kind: p.Kind, pos: npos}
		if p.Kind == lisp.ParamRequired || p.Kind == lisp.ParamOptional {
			npos++
		}
		slots = append(slots, c)
	}
	return slots, npos
}

// slotNamed returns the slot of the formal name.
func slotNamed(slots []argCheck, name string) (argCheck, bool) {
	for _, c := range slots {
		if c.name == name {
			return c, true
		}
	}
	return argCheck{}, false
}

// supplied calls f with each argument that args, a call the method's arity
// accepts, supplies for c.  It returns the first error f returns, or nil.
// Each element of a rest argument costs env one step, as a Lisp loop over
// it would: the work grows with the call.
func (c argCheck) supplied(env *lisp.LEnv, args []*lisp.LVal, npos int, f func(*lisp.LVal) *lisp.LVal) *lisp.LVal {
	switch c.kind {
	case lisp.ParamRequired, lisp.ParamOptional:
		if c.pos < len(args) {
			return f(args[c.pos])
		}
	case lisp.ParamRest:
		for _, v := range args[min(c.pos, len(args)):] {
			if err := env.Step(); err != nil {
				return lisp.Error(err)
			}
			if lerr := f(v); lerr != nil {
				return lerr
			}
		}
	case lisp.ParamKey:
		for i := npos; i+1 < len(args); i += 2 {
			if k := args[i]; k.Type == lisp.LSymbol && len(k.Str) == len(c.name)+1 && k.Str[1:] == c.name {
				if lerr := f(args[i+1]); lerr != nil {
					return lerr
				}
			}
		}
	}
	return nil
}

// methodWho names a method in an error: its type's display name and its
// name.  When a signature is built, self is the type's value, which the
// type :self names.
type methodWho struct {
	self            *lisp.LVal
	display, method string
}

// argError returns lerr, the failed check of argument arg, with the type,
// the method and the argument in front of its message.  The condition does
// not change.
func argError(env *lisp.LEnv, who methodWho, arg string, lerr *lisp.LVal) *lisp.LVal {
	switch lerr.Str {
	case WrongType, FailedConstraint, BadArgs:
		if !lisp.IsInternalPanic(lerr) {
			return env.ErrorConditionf(lerr.Str, "%s: method %s: %s: %s", who.display, who.method, argLabel(arg), errorText(lerr))
		}
	}
	return lerr
}

// argLabel names arg in an error: "argument amount", or "return value".
func argLabel(arg string) string {
	if arg == "return value" {
		return arg
	}
	return "argument " + arg
}

// checkArgType checks v against typ, a resolved argument type, and
// returns nil when v passes.  A built-in type name is checked by the table
// (no validator is built); anything else is applied as s:validate applies
// it, so a record type checks the type, its fields and :check.  Use
// (s:ref T) to check the type only.
func checkArgType(env *lisp.LEnv, typ, v *lisp.LVal) *lisp.LVal {
	if isTypeName(typ) {
		return checkTypeName(env, typ.Str, v)
	}
	if r := applyConstraint(env, typ, v); r.IsError() {
		return r
	}
	return nil
}

// resolveArgType returns typ, a type expression's value, as checkArgType
// reads it: a built-in type name, a record type or a validator as it is,
// and anything else as s:has-key resolves it, which refuses a value that
// is not a type.
func resolveArgType(env *lisp.LEnv, typ *lisp.LVal) *lisp.LVal {
	if isTypeName(typ) || isValidator(typ) {
		return typ
	}
	if _, ok := typeRefOf(typ); ok {
		return typ
	}
	return getHandler(env, typ, "", nil)
}

// checkLateArgType checks v against a late type: thunk, a lambda of the
// defining package that returns the type's current value.  [x 'T] makes
// one, so a method can name a type that is defined later, or again on a
// reload.
func checkLateArgType(env *lisp.LEnv, thunk, v *lisp.LVal) *lisp.LVal {
	typ := env.FunCall(thunk, lisp.Nil())
	if typ.IsError() {
		return typ
	}
	typ = resolveArgType(env, typ)
	if typ.IsError() {
		return typ
	}
	return checkArgType(env, typ, v)
}

// A Lisp method's signature is Lisp data in its type's captures (the map
// typeSigs, from method name), so a fork of a template copies it with the
// types it holds:
//
//	(npos returns (name kind pos type late) ...)
//
// returns is (type late), or () when the method has no return type.  When late is true,
// type is a lambda that returns the type (checkLateArgType).
const (
	sigNpos = iota
	sigReturns
	sigArgs
)

// lispMethodSig builds the signature of the Lisp method fn from types, the
// evaluated list (name type name type ...) that s:method and s:defmethod
// build from the typed formals.  A late type, [x 'T], comes as (name) and
// a lambda that returns T.
func lispMethodSig(env *lisp.LEnv, who methodWho, fn, types *lisp.LVal) *lisp.LVal {
	formals := fn.Cells[0]
	if formals == nil || formals.Type != lisp.LSExpr || len(formals.Cells) == 0 {
		return env.ErrorConditionf(BadArgs, "%s: method %s: a typed method needs its formals", who.display, who.method)
	}
	if len(types.Cells)%2 != 0 {
		return env.ErrorConditionf(BadArgs, "%s: method %s: argument types must be name and type pairs", who.display, who.method)
	}
	slots, npos := formalSlots(lisp.SExpr(formals.Cells[1:]))
	sig := make([]*lisp.LVal, sigArgs, sigArgs+len(types.Cells)/2)
	sig[sigNpos], sig[sigReturns] = lisp.Int(npos), lisp.Nil()
	for i := 0; i < len(types.Cells); i += 2 {
		name, typ := types.Cells[i], types.Cells[i+1]
		late := name.Type == lisp.LSExpr && len(name.Cells) == 1
		if late {
			name = name.Cells[0]
		}
		if name.IsSymbol(":returns") {
			if !late {
				if typ = methodType(env, who, typ); typ.IsError() {
					return argError(env, who, "return value", typ)
				}
			}
			sig[sigReturns] = lisp.Cells{typ, lisp.Bool(late)}.List()
			continue
		}
		c, ok := slotNamed(slots, name.Str)
		if name.Type != lisp.LSymbol || !ok {
			return env.ErrorConditionf(BadArgs, "%s: method %s: %v is not a formal", who.display, who.method, name)
		}
		switch {
		case late && typ.Type != lisp.LFun:
			return env.ErrorConditionf(BadArgs, "%s: method %s: argument %s: a late type needs a lambda", who.display, who.method, c.name)
		case late:
		default:
			if typ = methodType(env, who, typ); typ.IsError() {
				return argError(env, who, c.name, typ)
			}
		}
		sig = append(sig, lisp.Cells{lisp.String(c.name), lisp.Int(int(c.kind)), lisp.Int(c.pos), typ, lisp.Bool(late)}.List())
	}
	return lisp.QExpr(sig)
}

// methodType resolves a type of a method's signature: :self is the type
// being defined.
func methodType(env *lisp.LEnv, who methodWho, typ *lisp.LVal) *lisp.LVal {
	if typ.IsSymbol(":self") && who.self != nil {
		return who.self
	}
	return resolveArgType(env, typ)
}

// checkLispReturn checks r, the value a method returned, against sig's
// return type.  It returns nil when r passes or sig has none.
func checkLispReturn(env *lisp.LEnv, who methodWho, sig, r *lisp.LVal) *lisp.LVal {
	ret := sig.Cells[sigReturns]
	if ret.IsNil() {
		return nil
	}
	var lerr *lisp.LVal
	if lisp.True(ret.Cells[1]) {
		lerr = checkLateArgType(env, ret.Cells[0], r)
	} else {
		lerr = checkArgType(env, ret.Cells[0], r)
	}
	if lerr != nil {
		return argError(env, who, "return value", lerr)
	}
	return nil
}

// checkLispArgs checks args, a call the method's arity accepts, against
// sig, a Lisp method's signature.
func checkLispArgs(env *lisp.LEnv, who methodWho, sig *lisp.LVal, args []*lisp.LVal) *lisp.LVal {
	npos := sig.Cells[sigNpos].Int
	for _, s := range sig.Cells[sigArgs:] {
		c := argCheck{name: s.Cells[0].Str, kind: lisp.ParamKind(s.Cells[1].Int), pos: s.Cells[2].Int}
		typ, late := s.Cells[3], lisp.True(s.Cells[4])
		lerr := c.supplied(env, args, npos, func(v *lisp.LVal) *lisp.LVal {
			if late {
				return checkLateArgType(env, typ, v)
			}
			return checkArgType(env, typ, v)
		})
		if lerr != nil {
			return argError(env, who, c.name, lerr)
		}
	}
	return nil
}

// goArg is a checked formal of a Go method.
type goArg struct {
	check func(*lisp.LVal) error
	argCheck
}

// goSig is the checked signature of a Go method.  It is Go data that never
// changes, so every runtime shares it.
type goSig struct {
	ret  func(*lisp.LVal) error
	args []goArg
	npos int
}

// newGoSig builds the signature of m, whose Formals checkFormals accepted.
// It returns nil when m has no checks.
func newGoSig(m *MethodSpec) (*goSig, error) {
	if len(m.ArgChecks) == 0 && m.ReturnCheck == nil {
		return nil, nil
	}
	if m.Formals == nil && len(m.ArgChecks) > 0 {
		return nil, errors.New("ArgChecks needs Formals")
	}
	slots, npos := formalSlots(m.Formals)
	sig := &goSig{npos: npos, ret: m.ReturnCheck}
	for _, c := range slots {
		if f, ok := m.ArgChecks[c.name]; ok {
			if f == nil {
				return nil, fmt.Errorf("argument %s: the check is nil", c.name)
			}
			sig.args = append(sig.args, goArg{argCheck: c, check: f})
		}
	}
	if len(sig.args) != len(m.ArgChecks) {
		for name := range m.ArgChecks {
			if _, ok := slotNamed(slots, name); !ok {
				return nil, fmt.Errorf("ArgChecks names %s, which is not a formal", name)
			}
		}
	}
	return sig, nil
}

// checkReturn checks r, the value the method returned.
func (s *goSig) checkReturn(env *lisp.LEnv, who methodWho, r *lisp.LVal) *lisp.LVal {
	if s.ret == nil || r.IsError() {
		return r
	}
	if err := s.ret(r); err != nil {
		return argError(env, who, "return value", env.ErrorConditionf(goCheckCondition(err), "%v", err))
	}
	return r
}

// check checks args, a call the method's arity accepts.
func (s *goSig) check(env *lisp.LEnv, who methodWho, args []*lisp.LVal) *lisp.LVal {
	for _, a := range s.args {
		lerr := a.supplied(env, args, s.npos, func(v *lisp.LVal) *lisp.LVal {
			if err := a.check(v); err != nil {
				return env.ErrorConditionf(goCheckCondition(err), "%v", err)
			}
			return nil
		})
		if lerr != nil {
			return argError(env, who, a.name, lerr)
		}
	}
	return nil
}
