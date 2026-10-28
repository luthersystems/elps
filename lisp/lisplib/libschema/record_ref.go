// Copyright © 2026 The ELPS authors

package libschema

import (
	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
)

// Type values and type references (luthersystems/elps#905).
//
// Each record type, Lisp or Go, has a Lisp value: its constructor.  A Lisp
// type's value is the one s:make-record returns.  A Go type's value is
// RecordType.Value.  Both values are schema types: s:has-key, s:of and
// s:validate accept them, and they check that a value is an object of the
// type with valid fields.
//
// A recordRef names a record type with no reference to a VM: a Lisp type
// by its recordDesc, which every fork of a template shares, and a Go type
// by its *RecordType.  Two refs are equal when they name the same type, in
// every VM and after a fork.

// recordRef names one record type.  At most one field is set.
type recordRef struct {
	lisp *recordDesc
	goT  *RecordType
}

// valid reports whether r names a type.
func (r recordRef) valid() bool { return r.lisp != nil || r.goT != nil }

// display returns the type's display name.
func (r recordRef) display() string {
	switch {
	case r.lisp != nil:
		return r.lisp.display
	case r.goT != nil:
		return r.goT.spec.DisplayName
	}
	return ""
}

// typeRefOf returns the record type that v names: the value of a Lisp or a
// Go type, or the validator s:has-key and s:of wrap a type in.
func typeRefOf(v *lisp.LVal) (recordRef, bool) {
	if lt, ok := lispTypeOf(v); ok {
		return recordRef{lisp: lt.desc}, true
	}
	if t := goTypeValueOf(v); t != nil {
		return recordRef{goT: t}, true
	}
	if isValidator(v) {
		if d := descOf(v); d.rec.valid() {
			return d.rec, true
		}
	}
	return recordRef{}, false
}

// objectRef returns the record type of v when v is a record object.
func objectRef(v *lisp.LVal) (recordRef, bool) {
	if o, ok := lispObjectOf(v); ok {
		return recordRef{lisp: o.desc}, true
	}
	if t := goObjectType(v); t != nil {
		return recordRef{goT: t}, true
	}
	return recordRef{}, false
}

// goTypeTag carries a Go record type in the captures of its Lisp value.
// Its Go type marks the value: Lisp code cannot build a native payload, so
// the mark cannot be forged.
type goTypeTag struct {
	templatepolicy.Marker
	t *RecordType //elpsvet:allow-marker a RecordType is immutable after NewRecordType
}

var _ templatepolicy.Immutable = goTypeTag{}

// TransientNative marks goTypeTag as never saved by a durable dump: a
// type saves by the name of the global that holds it.
func (goTypeTag) TransientNative() {}

// Value returns a new Lisp value for the type.  Bind it to a global, as
// Lisp code binds the value of s:make-record:
//
//	env.PutGlobal(lisp.Symbol("counter"), rt.Value())
//
// The value builds objects, (counter :n 1) or (counter field-map), with the
// fields that have a getter.  A field left out, and a field with no
// getter, starts as (); a left-out field whose check refuses () is
// required and fails with failed-constraint, as s:has-key does.  It is a
// schema type, so s:has-key, s:of and s:validate accept it.  Each call
// returns a new value, so no two VMs share one.
func (t *RecordType) Value() *lisp.LVal {
	return funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		Package:  t.spec.Package,
		FID:      "record-type:" + t.spec.FID,
		Name:     "record-type:" + t.spec.DisplayName,
		Formals:  lisp.Formals(lisp.VarArgSymbol, "fields"),
		Captures: lisp.Cells{lisp.Native(goTypeTag{t: t})}.List(),
		Eval:     evalGoType,
	})
}

// goTypeValueOf returns the Go type whose Lisp value is v, or nil.
func goTypeValueOf(v *lisp.LVal) *RecordType {
	if v == nil || v.Type != lisp.LFun || v.Builtin() == nil {
		return nil
	}
	caps := funraw.Captures(v)
	if caps == nil || len(caps.Cells) != 1 {
		return nil
	}
	tag, ok := lisp.NativeValue[goTypeTag](caps.Cells[0])
	if !ok {
		return nil
	}
	return tag.t
}

// evalGoType runs (type :field value ...) or (type field-map) for a Go
// type.  It reads the arguments as a Lisp type's constructor does.
func evalGoType(env *lisp.LEnv, args, caps *lisp.LVal) *lisp.LVal {
	tag, _ := lisp.NativeValue[goTypeTag](caps.Cells[0])
	t := tag.t
	m := constructorFields(env, t.spec.DisplayName, args.Cells)
	if m.IsError() {
		return m
	}
	fields := make([]*lisp.LVal, len(t.spec.Fields))
	given := make([]bool, len(fields))
	for i := range fields {
		fields[i] = lisp.Nil()
	}
	for k, v := range m.All() {
		key := mapKeyLVal(k)
		i, ok := -1, false
		if key.Type == lisp.LString {
			i, ok = t.index[key.Str]
		}
		if !ok || !t.spec.Fields[i].Getter {
			return env.ErrorConditionf(BadArgs, "%s has no field %s", t.spec.DisplayName, env.Render(key))
		}
		fields[i] = v
		given[i] = true
	}
	// A field left out is (), as in a Lisp type.  When its check refuses
	// (), the field is required: fail as s:has-key fails on a missing key.
	for i, f := range t.spec.Fields {
		if !given[i] && f.Check != nil && f.Check(fields[i]) != nil {
			return env.ErrorConditionf(FailedConstraint, "%s: missing required field %s", t.spec.DisplayName, f.Name)
		}
	}
	return t.New(env, fields...)
}

// goTypeValidator wraps the Go type t as a schema validator, as
// recordValidator wraps a Lisp type.
func goTypeValidator(env *lisp.LEnv, t *RecordType) *lisp.LVal {
	v := newValidator(env, lisp.Formals("input"), func(env *lisp.LEnv, input, _ *lisp.LVal) *lisp.LVal {
		return validateGoValue(env, t, input)
	})
	return describeAs(v, &schemaDesc{op: "record", name: t.spec.DisplayName, rec: recordRef{goT: t}})
}

// validateGoValue is applyConstraint for a Go type used as a validator:
// input must be an object of t, and its fields must pass the field checks
// and Check.
func validateGoValue(env *lisp.LEnv, t *RecordType, input *lisp.LVal) *lisp.LVal {
	r, ok := t.Record(input)
	if !ok {
		return env.ErrorConditionf(WrongType, "Input is not a %s record", t.spec.DisplayName)
	}
	if err := t.check(r.caps); err != nil {
		return env.ErrorConditionf(goCheckCondition(err), "%s: %v", t.spec.DisplayName, err)
	}
	return lisp.Nil()
}

// refValidator returns the validator of (s:ref T): it passes an object of
// the type ref and checks nothing else.
func refValidator(env *lisp.LEnv, ref recordRef) *lisp.LVal {
	v := newValidator(env, lisp.Formals("input"), func(env *lisp.LEnv, input, _ *lisp.LVal) *lisp.LVal {
		if got, ok := objectRef(input); ok && got == ref {
			return lisp.Nil()
		}
		return env.ErrorConditionf(WrongType, "Input is not a %s record", ref.display())
	})
	return describeAs(v, &schemaDesc{op: "ref", name: ref.display(), rec: ref})
}

// builtinRef is s:ref.
func builtinRef(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	ref, ok := typeRefOf(args.Cells[0])
	if !ok {
		return env.ErrorConditionf(BadArgs, "s:ref: argument is not a record type: %v", args.Cells[0])
	}
	return refValidator(env, ref)
}

// Ref returns the validator (s:ref T) for t: it passes an object of t and
// checks nothing else.  It is a validator of env, so make one for each VM.
func (t *RecordType) Ref(env *lisp.LEnv) *lisp.LVal {
	return refValidator(env, recordRef{goT: t})
}
