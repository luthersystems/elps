// Copyright © 2026 The ELPS authors

package libschema

import (
	"errors"
	"fmt"
	"strings"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Record types (luthersystems/elps#895).
//
// A record type declares fields and methods.  The type value is the
// constructor: calling it builds an object.  An object is a function value
// whose state is explicit (lisp.NewCapturedFun), and calling it dispatches
// in Go: (obj 'op args...) calls the method op with obj as its receiver, or
// returns the field op.  This replaces the hand-written
// (lambda (op &rest args) (cond ...)) closures phylum code uses for classes.
//
// An object saves as its type and its field map, never as code.  The type
// is a function a global holds, so a durable dump writes it by that global's
// name (libjson's unregistered builtin rule) and a load finds the type again
// by name.  The methods therefore never enter a saved document, and a load
// in a newer program version gets that version's methods.
//
// A Lisp type's state lives in its captures, so a template fork gets a
// private copy, as it does for a closure:
//
//	typeTag      recordTag: the immutable descriptor, shared by forks
//	typeFields   the field validators (s:has-key, s:may-have-key), in order
//	typeMethods  a sorted map from method name (a string) to function
//	typeCheck    the :check function, or ()
//	typeSelf     the type value itself, which each object it builds holds
//	typeSigs     a sorted map from the name of each typed method to its
//	             signature (lispMethodSig)
//
// An object's captures are (type fields state): the type value, a sorted map
// from field name (a string) to value, and the call state (objState).  A
// map rather than positions, so a type that adds or reorders fields still
// reads older objects by name.
const (
	typeTag = iota
	typeFields
	typeMethods
	typeCheck
	typeSelf
	typeSigs
	typeCells
)

const (
	objType = iota
	objFields
	// objState is nil while no method or :check runs on the object.
	// While one runs it is true, and the first s:set-field! replaces it with
	// the field map as it was before the outermost call, which a failed
	// call puts back.
	objState
	objCells
)

// UnknownOperation is the condition an object raises for an op its type
// does not define.  Its data is the op, as the hand-written cond raised it.
const UnknownOperation = "unknown-operation"

// recordObjectFID is the FID of every object of a Lisp record type.  The
// package is DefaultPackageName.  RecordCodec is registered under them.
const recordObjectFID = "record"

// RecordCodec is the durable codec of the objects of Lisp record types.
// Register it in the durable registry of every VM that saves such objects
// (libjson.NewFrozenDurableRegistry).  A dump writes an object as its type,
// by the name of the global that holds the type, and its field map.  Load
// checks the shape and rebuilds the object over the decoded captures; it
// runs no Lisp, so it cannot run the field validators.  Run ValidateLoaded
// on the decoded values before any Lisp uses them.
var RecordCodec = libjson.DurableFunCodec{
	Package: DefaultPackageName,
	FID:     recordObjectFID,
	Name:    "elps:s:record",
	Version: 1,
	Load:    loadLispObject,
}

// recordDesc is the immutable part of a record type: its names and its
// field names.  It is built once, when the type is built, and never written
// after, so every fork of a template shares it.
type recordDesc struct {
	index   map[string]int
	name    string
	display string
	fields  []string
}

// recordTag carries a recordDesc in a type's captures.  Its Go type marks
// the captures as a record type's: Lisp code cannot build a native payload,
// so the mark cannot be forged.
type recordTag struct {
	templatepolicy.Marker
	desc *recordDesc //elpsvet:allow-marker built once by the type's constructor and never written after
}

var _ templatepolicy.Immutable = recordTag{}

// TransientNative marks recordTag as never saved by a durable dump: a type
// saves by the name of the global that holds it, so a dump never walks the
// type's captures.
func (recordTag) TransientNative() {}

// hasField reports whether name is a field of the type.
func (d *recordDesc) hasField(name string) bool {
	_, ok := d.index[name]
	return ok
}

// lispType is a Lisp record type as its descriptor and its captures.
type lispType struct {
	desc *recordDesc
	caps *lisp.LVal
}

// lispObject is an object of a Lisp record type: the type and the
// object's field map.
type lispObject struct {
	lispType
	fields *lisp.LVal
	// objCaps are the object's captures (type fields state).
	objCaps *lisp.LVal
}

// lispTypeOf returns v as a lispType when v is a Lisp record type.
func lispTypeOf(v *lisp.LVal) (lispType, bool) {
	if v == nil || v.Type != lisp.LFun || v.Builtin() == nil {
		return lispType{}, false
	}
	caps := funraw.Captures(v)
	if caps == nil || len(caps.Cells) != typeCells {
		return lispType{}, false
	}
	tag, ok := lisp.NativeValue[recordTag](caps.Cells[typeTag])
	if !ok || tag.desc == nil {
		return lispType{}, false
	}
	return lispType{desc: tag.desc, caps: caps}, true
}

// lispObjectOf returns v as a lispObject when v is an object of a Lisp
// record type.
func lispObjectOf(v *lisp.LVal) (lispObject, bool) {
	if v == nil || v.Type != lisp.LFun || v.Builtin() == nil ||
		v.Package() != DefaultPackageName || v.FID() != recordObjectFID {
		return lispObject{}, false
	}
	caps := funraw.Captures(v)
	if caps == nil || len(caps.Cells) != objCells || caps.Cells[objFields].Type != lisp.LSortMap {
		return lispObject{}, false
	}
	t, ok := lispTypeOf(caps.Cells[objType])
	if !ok {
		return lispObject{}, false
	}
	return lispObject{lispType: t, fields: caps.Cells[objFields], objCaps: caps}, true
}

// objectFormals is the formal list of every object.  Each object gets its
// own list: a list one VM shares with another would alias across forks.
func objectFormals() *lisp.LVal {
	return lisp.Formals("op", lisp.VarArgSymbol, "args")
}

// newLispObject returns an object over caps, which are (type fields).
func newLispObject(desc *recordDesc, caps *lisp.LVal) *lisp.LVal {
	return funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		Package:  DefaultPackageName,
		FID:      recordObjectFID,
		Name:     desc.display,
		Formals:  objectFormals(),
		Captures: caps,
		Eval:     evalLispObject,
	})
}

// evalLispObject runs (obj op args...).  A method gets as its receiver an
// object over the same captures: the call cannot reach the value it was
// called through, but one over the same captures shares all its state.
func evalLispObject(env *lisp.LEnv, args, caps *lisp.LVal) *lisp.LVal {
	t, ok := lispTypeOf(caps.Cells[objType])
	if !ok {
		return env.Errorf("record object has no record type")
	}
	desc, tcaps := t.desc, t.caps
	if len(args.Cells) == 0 {
		return env.ErrorConditionf(BadArgs, "%s: no operation given", desc.display)
	}
	op, rest := args.Cells[0], args.Cells[1:]
	if op.Type != lisp.LSymbol {
		return env.ErrorCondition(UnknownOperation, op)
	}
	if m, ok := tcaps.Cells[typeMethods].Map().Get(lisp.String(op.Str)); ok && m.Type == lisp.LFun {
		// The call's shape is checked in Go, as for a Go method, so a
		// bad call is bad-arguments with no receiver in the count.
		if msg := lispMethodArity(m).checkCall(rest); msg != "" {
			return env.ErrorConditionf(BadArgs, "%s: method %s %s", desc.display, op.Str, msg)
		}
		call := lispCall{fn: m, who: methodWho{display: desc.display, method: op.Str}, args: rest}
		if sig, typed := tcaps.Cells[typeSigs].Map().Get(lisp.String(op.Str)); typed {
			if lerr := checkLispArgs(env, call.who, sig, rest); lerr != nil {
				return lerr
			}
			call.sig = sig
		}
		return callLispMethod(env, caps, call)
	}
	if desc.hasField(op.Str) {
		if len(rest) > 0 {
			return env.ErrorConditionf(BadArgs, "%s: field %s takes no arguments, got %d", desc.display, op.Str, len(rest))
		}
		v, ok := caps.Cells[objFields].Map().Get(lisp.String(op.Str))
		if !ok {
			return lisp.Nil()
		}
		return v
	}
	return env.ErrorCondition(UnknownOperation, op)
}

// lispCall is a call of a Lisp method whose arguments evalLispObject has
// checked: the method, its signature (or nil) and the arguments after the
// receiver.
type lispCall struct {
	fn, sig *lisp.LVal
	who     methodWho
	args    []*lisp.LVal
}

// run calls the method with args, the receiver first, and checks the value
// it returns against its :returns type.
func (c lispCall) run(env *lisp.LEnv, args []*lisp.LVal) *lisp.LVal {
	r := env.FunCall(c.fn, lisp.QExpr(args))
	if r.IsError() || c.sig == nil {
		return r
	}
	if lerr := checkLispReturn(env, c.who, c.sig, r); lerr != nil {
		return lerr
	}
	return r
}

// callLispMethod calls the method c on the object over caps, whose type
// evalLispObject has checked.
//
// A method may change several fields.  :check runs when the outermost
// method call on the object returns, so every object a caller sees keeps
// the type's rules.  A call that a method or :check makes on the same
// object runs without a check: the outer call checks the end state, and a
// :check that calls a method does not call itself.  A call that fails, or
// breaks :check, leaves the fields as they were before the outermost
// call: every header of the object reads its fields through caps, so
// putting the saved map back restores them all.
func callLispMethod(env *lisp.LEnv, caps *lisp.LVal, c lispCall) *lisp.LVal {
	t, _ := lispTypeOf(caps.Cells[objType])
	recv := newLispObject(t.desc, caps)
	args := make([]*lisp.LVal, 0, len(c.args)+1)
	args = append(args, recv)
	args = append(args, c.args...)
	if !caps.Cells[objState].IsNil() {
		return c.run(env, args)
	}
	caps.Cells[objState] = lisp.Bool(true) //elps:mutates marks the object's own call state
	defer func() {
		caps.Cells[objState] = lisp.Nil() //elps:mutates clears the object's own call state
	}()
	r := c.run(env, args)
	if !r.IsError() {
		if lerr := runRecordCheck(env, t.desc, t.caps, recv); lerr != nil {
			r = lerr
		}
	}
	if saved := caps.Cells[objState]; r.IsError() && saved.Type == lisp.LSortMap {
		caps.Cells[objFields] = saved //elps:mutates restores the object's own fields after a failed method
	}
	return r
}

// evalLispType runs (type :field value ...) or (type field-map).
func evalLispType(env *lisp.LEnv, args, tcaps *lisp.LVal) *lisp.LVal {
	desc := mustTag(tcaps)
	fields := constructorFields(env, desc.display, args.Cells)
	if fields.IsError() {
		return fields
	}
	if lerr := validateLispFields(env, desc, tcaps, fields); lerr != nil {
		return lerr
	}
	obj := newLispObject(desc, lisp.Cells{tcaps.Cells[typeSelf], fields, lisp.Nil()}.List())
	if lerr := runRecordCheck(env, desc, tcaps, obj); lerr != nil {
		return lerr
	}
	return obj
}

// mustTag returns the descriptor of a type's captures, which the type's
// constructor has already checked.
func mustTag(tcaps *lisp.LVal) *recordDesc {
	tag, _ := lisp.NativeValue[recordTag](tcaps.Cells[typeTag])
	return tag.desc
}

// constructorFields returns a new field map from a constructor's
// arguments: one sorted map, or keyword and value pairs.  The map is new,
// so the object owns it: a caller's later change to its map does not
// change the object.
func constructorFields(env *lisp.LEnv, display string, args []*lisp.LVal) *lisp.LVal {
	if len(args) == 1 && args[0].Type == lisp.LSortMap {
		return copyMap(env, args[0])
	}
	if len(args)%2 != 0 {
		return env.ErrorConditionf(BadArgs, "%s: give one field map, or keyword and value pairs", display)
	}
	fields := lisp.SortedMapSized(len(args) / 2)
	for i := 0; i < len(args); i += 2 {
		k := args[i]
		if k.Type != lisp.LSymbol || len(k.Str) < 2 || k.Str[0] != ':' {
			return env.ErrorConditionf(BadArgs, "%s: argument %d is not a keyword: %v", display, i+1, k)
		}
		key := lisp.String(k.Str[1:])
		if _, dup := fields.Map().Get(key); dup {
			return env.ErrorConditionf(BadArgs, "%s: field %s given twice", display, k.Str[1:])
		}
		if lerr := fields.Map().Set(key, args[i+1]); lerr.IsError() {
			return lerr
		}
	}
	return fields
}

// copyMap returns a new sorted map with the entries of src.  The values are
// shared.
func copyMap(env *lisp.LEnv, src *lisp.LVal) *lisp.LVal {
	m := src.Map()
	buf := make([]*lisp.LVal, m.Len())
	if lerr := m.Entries(buf); lerr.IsError() {
		return lerr
	}
	dst := lisp.SortedMapSized(len(buf))
	for _, e := range buf {
		if lerr := dst.Map().Set(e.Cells[0], e.Cells[1]); lerr.IsError() {
			return env.ErrorConditionf(BadArgs, "cannot copy field map: %v", lerr)
		}
	}
	return dst
}

// validateLispFields checks a whole field map: each key names a field, and
// each field validator accepts the map.  It returns nil when the map is
// valid.
func validateLispFields(env *lisp.LEnv, desc *recordDesc, tcaps, fields *lisp.LVal) *lisp.LVal {
	keys := fields.Map().Keys()
	if keys.IsError() {
		return keys
	}
	for _, k := range keys.Cells {
		if k.Type != lisp.LString {
			return env.ErrorConditionf(BadArgs, "%s: field key %v is not a string", desc.display, k)
		}
		if !desc.hasField(k.Str) {
			return env.ErrorConditionf(BadArgs, "%s has no field %s", desc.display, k.Str)
		}
	}
	for _, v := range tcaps.Cells[typeFields].Cells {
		if r := applyConstraint(env, v, fields); r.IsError() {
			return recordError(env, desc, r)
		}
	}
	return nil
}

// runRecordCheck calls the type's :check function, if any, with obj.  The
// function rejects obj by raising an error.
//
// While the check runs, the object is marked as in a call, so a method the
// check calls on it runs without a check of its own.
func runRecordCheck(env *lisp.LEnv, desc *recordDesc, tcaps, obj *lisp.LVal) *lisp.LVal {
	check := tcaps.Cells[typeCheck]
	if check.IsNil() {
		return nil
	}
	if caps := funraw.Captures(obj); caps != nil && len(caps.Cells) == objCells && caps.Cells[objState].IsNil() {
		caps.Cells[objState] = lisp.Bool(true) //elps:mutates marks the object's own call state
		defer func() {
			caps.Cells[objState] = lisp.Nil() //elps:mutates clears the object's own call state
		}()
	}
	if r := env.FunCall(check, lisp.Cells{obj}.List()); r.IsError() {
		return r
	}
	return nil
}

// recordError returns a validator's error with the record's name in front
// of its message.  The condition does not change.
func recordError(env *lisp.LEnv, desc *recordDesc, lerr *lisp.LVal) *lisp.LVal {
	switch lerr.Str {
	case WrongType, FailedConstraint, BadArgs:
		if !lisp.IsInternalPanic(lerr) {
			return env.ErrorConditionf(lerr.Str, "%s: %s", desc.display, errorText(lerr))
		}
	}
	// Any other condition, such as one a nested :check raised, keeps its
	// data and its stack.
	return lerr
}

// errorText returns the message of an error made by ErrorConditionf.
func errorText(lerr *lisp.LVal) string {
	parts := make([]string, 0, len(lerr.Cells))
	for _, c := range lerr.Cells {
		if c.Type == lisp.LString {
			parts = append(parts, c.Str)
			continue
		}
		parts = append(parts, c.String())
	}
	return strings.Join(parts, " ")
}

// fieldValidator returns the validator of the field name, or nil.
func fieldValidator(desc *recordDesc, tcaps *lisp.LVal, name string) *lisp.LVal {
	i, ok := desc.index[name]
	if !ok {
		return nil
	}
	return tcaps.Cells[typeFields].Cells[i]
}

// builtinRecordSet is s:set-field!.  It checks the field's validator, then
// replaces the field's value in the object.
func builtinRecordSet(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	obj, field, value := args.Cells[0], args.Cells[1], args.Cells[2]
	o, ok := lispObjectOf(obj)
	desc, tcaps, fields := o.desc, o.caps, o.fields
	if !ok {
		if goObjectType(obj) != nil {
			return env.ErrorConditionf(BadArgs, "s:set-field!: %v is a record type defined in Go; use its methods to change it", obj)
		}
		return env.ErrorConditionf(BadArgs, "s:set-field!: first argument is not a record object: %v", obj)
	}
	name, ok := fieldName(field)
	if !ok {
		return env.ErrorConditionf(BadArgs, "s:set-field!: field must be a symbol or a string: %v", field)
	}
	if !desc.hasField(name) {
		return env.ErrorConditionf(BadArgs, "s:set-field!: %s has no field %s", desc.display, name)
	}
	key := lisp.String(name)
	if v := fieldValidator(desc, tcaps, name); v != nil {
		probe := lisp.SortedMapSized(1)
		probe.Map().Set(key, value)
		if r := applyConstraint(env, v, probe); r.IsError() {
			return recordError(env, desc, r)
		}
	}
	if caps := o.objCaps; caps.Cells[objState].Type == lisp.LSymbol {
		// The first write in a call saves the fields as they were, for a
		// failed call to put back.
		saved := copyMap(env, fields)
		if saved.IsError() {
			return saved
		}
		caps.Cells[objState] = saved //elps:mutates saves the object's own fields before the first write of a call
	}
	if lerr := fields.Map().Set(key, value); lerr.IsError() { //elps:mutates s:set-field! writes the object's own field map, which its constructor copied
		return lerr
	}
	return value
}

// fieldName returns the field name a symbol or string names.
func fieldName(v *lisp.LVal) (string, bool) {
	switch v.Type {
	case lisp.LSymbol, lisp.LString:
		return v.Str, v.Str != ""
	default:
		return "", false
	}
}

// builtinRecordFields is s:fields.  It returns a copy of a Lisp object's
// field map.
func builtinRecordFields(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if o, ok := lispObjectOf(args.Cells[0]); ok {
		return copyMap(env, o.fields)
	}
	if t := goObjectType(args.Cells[0]); t != nil {
		return t.fieldMap(args.Cells[0])
	}
	return env.ErrorConditionf(BadArgs, "s:fields: argument is not a record object: %v", args.Cells[0])
}

// builtinIsRecord is s:record?.
func builtinIsRecord(_ *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	_, ok := lispObjectOf(args.Cells[0])
	return lisp.Bool(ok || goObjectType(args.Cells[0]) != nil)
}

// validateRecordValue is applyConstraint for a record type used as a
// validator: input must be an object of that type, with valid fields.
func validateRecordValue(env *lisp.LEnv, desc *recordDesc, tcaps, input *lisp.LVal) *lisp.LVal {
	o, ok := lispObjectOf(input)
	if !ok || o.desc != desc {
		return env.ErrorConditionf(WrongType, "Input is not a %s record", desc.display)
	}
	if lerr := validateLispFields(env, desc, tcaps, o.fields); lerr != nil {
		return lerr
	}
	if lerr := runRecordCheck(env, desc, tcaps, input); lerr != nil {
		return lerr
	}
	return lisp.Nil()
}

// recordValidator wraps the record type t as a schema validator, so that a
// record type can be the type of a field or of an s:of element.
func recordValidator(env *lisp.LEnv, t *lisp.LVal, desc *recordDesc) *lisp.LVal {
	v := newCapturedValidator(env, lisp.Formals("input"), lisp.Cells{t}.List(), func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		return applyConstraint(env, captures.Cells[0], input)
	})
	return describeAs(v, &schemaDesc{op: "record", name: desc.display, rec: recordRef{lisp: desc}})
}

// loadLispObject is RecordCodec's Load.  It runs no Lisp: it checks that
// the captures are a type and a field map, and rebuilds the object over
// them.  The field validators run later, in ValidateLoaded.
func loadLispObject(_ *lisp.LEnv, version int, caps *lisp.LVal) (*lisp.LVal, error) {
	if version != 1 {
		return nil, fmt.Errorf("record object: unknown version %d", version)
	}
	if caps == nil || len(caps.Cells) != objCells {
		return nil, errors.New("record object: saved captures are not a type, a field map and a call state")
	}
	t, ok := lispTypeOf(caps.Cells[objType])
	desc := t.desc
	if !ok {
		return nil, fmt.Errorf("record object: the saved type is not a record type (got %v); was its global rebound?", caps.Cells[objType].Type)
	}
	if caps.Cells[objFields].Type != lisp.LSortMap {
		return nil, fmt.Errorf("record object %s: saved fields are not a map", desc.display)
	}
	// A dump made during a method call saved the call state; the loaded
	// object is in no call.
	caps.Cells[objState] = lisp.Nil() //elps:mutates resets the call state of the object the load builds
	return newLispObject(desc, caps), nil
}

// RecordFields returns a copy of the field map of v when v is an object of
// a Lisp record type.  It is for Go code that inspects saved state; Lisp
// code uses s:fields.
func RecordFields(env *lisp.LEnv, v *lisp.LVal) (*lisp.LVal, bool) {
	o, ok := lispObjectOf(v)
	if !ok {
		return nil, false
	}
	return copyMap(env, o.fields), true
}

// IsRecordType reports whether v is a record type defined in Lisp.
func IsRecordType(v *lisp.LVal) bool {
	_, ok := lispTypeOf(v)
	return ok
}

// ValidateLoaded checks every object of a Lisp record type reachable from
// vals against its type: the field validators and the :check function.  A
// durable load cannot run them (RecordCodec runs no Lisp), so an embedder
// that loads saved values calls ValidateLoaded before any Lisp uses them,
// in the VM that will use them.  The type is the one the loading VM binds
// to the saved name, so values saved by an older program version are
// checked against the current fields.
//
// It walks lists, sorted maps, arrays, tagged values and the field maps of
// record objects.  It does not enter a closure's environment or a native
// payload.  It returns the first failure.
func ValidateLoaded(env *lisp.LEnv, vals ...*lisp.LVal) error {
	seen := make(map[*lisp.LVal]bool)
	stack := lisp.Cells(vals).Clone()
	for len(stack) > 0 {
		v := stack[len(stack)-1]
		stack = stack[:len(stack)-1]
		if v == nil || seen[v] {
			continue
		}
		seen[v] = true
		switch lisp.ShapeOf(v.Type) {
		case lisp.ShapeList:
			stack = append(stack, v.Cells...)
		case lisp.ShapeMap:
			entries := v.Map()
			buf := make([]*lisp.LVal, entries.Len())
			if lerr := entries.Entries(buf); lerr.IsError() {
				return lisp.GoError(lerr)
			}
			for _, e := range buf {
				stack = append(stack, e.Cells[1])
			}
		case lisp.ShapeArray:
			if _, data := v.ArrayParts(); data != nil {
				stack = append(stack, data.Cells...)
			}
		case lisp.ShapeTagged:
			stack = append(stack, v.UserData())
		case lisp.ShapeFun:
			o, ok := lispObjectOf(v)
			if !ok {
				continue
			}
			if lerr := validateLispFields(env, o.desc, o.caps, o.fields); lerr != nil {
				return lisp.GoError(lerr)
			}
			if lerr := runRecordCheck(env, o.desc, o.caps, v); lerr != nil {
				return lisp.GoError(lerr)
			}
			stack = append(stack, o.fields)
		case lisp.ShapeLeaf, lisp.ShapeError, lisp.ShapeNative, lisp.ShapeMark, lisp.ShapeInvalid:
		}
	}
	return nil
}
