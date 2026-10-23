// Copyright © 2026 The ELPS authors

package libschema

import (
	"github.com/luthersystems/elps/lisp"
)

// schemaDesc is the definition of a validator, kept as data so that tools
// can read a schema back: s:describe returns it, and an embedder (lint, the
// LSP, a check against a Go struct) can walk it.  A validator is a Go
// closure, so without it nothing outside the closure could tell what it
// checks.
//
// A schemaDesc is built once, when its validator is built, and never written
// after, so it is shared by every VM a template forks (see validatorTag).
type schemaDesc struct {
	op       string        // "type", "in", "gt", "has-key", ...
	name     string        // a type's name, as s:make-validator was given it
	typ      string        // a type's base type: "string", "sorted-map", ...
	key      string        // has-key, may-have-key, when: the key
	values   []descValue   // literal arguments: s:in's values, s:gt's bound, s:regexp's pattern
	children []*schemaDesc // the constraints or types it holds, in order
}

// descValue is one literal argument of a constraint, as immutable Go data.
type descValue struct {
	s    string
	f    float64
	i    int
	kind lisp.LType
}

// literal records v as a descValue.  A value that is not a number, string,
// symbol or keyword is kept as its type name, such as "sorted-map": printing
// it would cost each constructor of a large literal a walk of the value.
func literal(v *lisp.LVal) descValue {
	switch v.Type {
	case lisp.LInt:
		return descValue{kind: lisp.LInt, i: v.Int}
	case lisp.LFloat:
		return descValue{kind: lisp.LFloat, f: v.Float}
	case lisp.LString, lisp.LSymbol:
		return descValue{kind: v.Type, s: v.Str}
	default:
		return descValue{kind: lisp.LString, s: v.Type.String()}
	}
}

func (d descValue) lval() *lisp.LVal {
	switch d.kind {
	case lisp.LInt:
		return lisp.Int(d.i)
	case lisp.LFloat:
		return lisp.Float(d.f)
	case lisp.LSymbol:
		return lisp.Symbol(d.s)
	default:
		return lisp.String(d.s)
	}
}

// describeAs records d as the definition of the validator v, which its
// constructor has just built and no one else holds yet.  It returns v.
func describeAs(v *lisp.LVal, d *schemaDesc) *lisp.LVal {
	if v == nil || v.IsError() || !isValidator(v) {
		return v
	}
	v.Cells[validatorMarkerIndex] = lisp.Native(validatorTag{desc: d}) //elps:mutates construction-time tagging of a validator this package just built
	return v
}

// descOf returns the definition recorded for the validator v, or an
// "opaque" one for a validator built with NewValidator.
func descOf(v *lisp.LVal) *schemaDesc {
	if !isValidator(v) {
		return &schemaDesc{op: "opaque"}
	}
	tag, _ := lisp.NativeValue[validatorTag](v.Cells[validatorMarkerIndex])
	if tag.desc == nil {
		return &schemaDesc{op: "opaque"}
	}
	return tag.desc
}

// descsOf returns the definitions of vs.
func descsOf(vs []*lisp.LVal) []*schemaDesc {
	out := make([]*schemaDesc, 0, len(vs))
	for _, v := range vs {
		out = append(out, descOf(v))
	}
	return out
}

// literals records vs as descValues.
func literals(vs []*lisp.LVal) []descValue {
	out := make([]descValue, 0, len(vs))
	for _, v := range vs {
		out = append(out, literal(v))
	}
	return out
}

// lval returns d as a sorted map.  Fields that d does not use are left out.
func (d *schemaDesc) lval() *lisp.LVal {
	m := lisp.MapOf("op", d.op)
	if d.name != "" {
		m.MapSetLVal(lisp.String("name"), lisp.String(d.name))
	}
	if d.typ != "" {
		m.MapSetLVal(lisp.String("type"), lisp.String(d.typ))
	}
	if d.key != "" {
		m.MapSetLVal(lisp.String("key"), lisp.String(d.key))
	}
	if len(d.values) > 0 {
		cells := make([]*lisp.LVal, 0, len(d.values))
		for _, v := range d.values {
			cells = append(cells, v.lval())
		}
		m.MapSetLVal(lisp.String("values"), lisp.Vector(cells))
	}
	if len(d.children) > 0 {
		cells := make([]*lisp.LVal, 0, len(d.children))
		for _, c := range d.children {
			cells = append(cells, c.lval())
		}
		m.MapSetLVal(lisp.String("children"), lisp.Vector(cells))
	}
	return m
}

// builtinDescribe is s:describe.
func builtinDescribe(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	v := args.Cells[0]
	if !isValidator(v) {
		return env.ErrorConditionf(BadArgs, "describe: %s is not a schema validator", v.Type.String())
	}
	return descOf(v).lval()
}

// builtinMapOf is s:map-of: a constraint on a sorted map whose every key
// matches key-type and every value matches value-type.
func builtinMapOf(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	keyType := getHandler(env, args.Cells[0], "key", nil)
	if keyType.IsError() {
		return keyType
	}
	valType := getHandler(env, args.Cells[1], "value", nil)
	if valType.IsError() {
		return valType
	}
	v := newCapturedValidator(env, lisp.Formals("input"), lisp.Cells{keyType, valType}.List(), func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSortMap {
			return lisp.ErrorConditionf(WrongType, "Invalid input for 'map-of' - need a sorted map")
		}
		kt, vt := captures.Cells[0], captures.Cells[1]
		for k, val := range input.All() {
			key := mapKeyLVal(k)
			if applyConstraint(env, kt, key).IsError() {
				return lisp.ErrorConditionf(WrongType, "Key %s was of wrong type", key)
			}
			if applyConstraint(env, vt, val).IsError() {
				return lisp.ErrorConditionf(WrongType, "Value at key %s was of wrong type", key)
			}
		}
		return lisp.Nil()
	})
	return describeAs(v, &schemaDesc{op: "map-of", children: []*schemaDesc{descOf(keyType), descOf(valType)}})
}

func mapKeyLVal(k lisp.MapKey) *lisp.LVal {
	switch k.Type {
	case lisp.LSymbol:
		return lisp.Symbol(k.Str)
	case lisp.LInt:
		return lisp.Int(k.Int)
	default:
		return lisp.String(k.Str)
	}
}

// builtinCheckList checks a list.  () is the empty list, so it passes.
func builtinCheckList(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	return newCapturedValidator(env, lisp.Formals("input"), rest, func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSExpr {
			return lisp.ErrorConditionf(WrongType, "Input was not a list for type %s", name)
		}
		return applyConstraint(env, captures, input)
	})
}

// builtinCheckSequence checks a list or a vector, as a Go slice accepts
// either.
func builtinCheckSequence(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	return newCapturedValidator(env, lisp.Formals("input"), rest, func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		if input.Type != lisp.LSExpr && input.Type != lisp.LArray {
			return lisp.ErrorConditionf(WrongType, "Input was not a list or vector for type %s", name)
		}
		return applyConstraint(env, captures, input)
	})
}

// builtinCheckNil checks for (), so that (s:may-have-key "note" s:string
// s:nil) allows a nil note.
func builtinCheckNil(env *lisp.LEnv, name string, constraints []*lisp.LVal) *lisp.LVal {
	rest := builtinCheckAny(env, constraints)
	return newCapturedValidator(env, lisp.Formals("input"), rest, func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		if !input.IsNil() {
			return lisp.ErrorConditionf(WrongType, "Input was not nil for type %s", name)
		}
		return applyConstraint(env, captures, input)
	})
}

// argDesc is the definition of a type argument: a type name such as
// s:string, or a validator.
func argDesc(v *lisp.LVal) *schemaDesc {
	if v.Type == lisp.LString {
		return &schemaDesc{op: "type", typ: v.Str}
	}
	return descOf(v)
}

func argDescs(vs []*lisp.LVal) []*schemaDesc {
	out := make([]*schemaDesc, 0, len(vs))
	for _, v := range vs {
		out = append(out, argDesc(v))
	}
	return out
}

// literalConstraint records the literal arguments of a constraint such as
// s:in or s:gt.
func literalConstraint(op string, fn lisp.LBuiltin) lisp.LBuiltin {
	return func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		v := fn(env, args)
		if !isValidator(v) {
			return v
		}
		switch len(args.Cells) {
		case 0:
			return describeAs(v, &schemaDesc{op: op})
		case 1:
			// One allocation holds the definition and its one value.
			d := &oneValueDesc{one: [1]descValue{literal(args.Cells[0])}}
			d.op, d.values = op, d.one[:]
			return describeAs(v, &d.schemaDesc)
		}
		return describeAs(v, &schemaDesc{op: op, values: literals(args.Cells)})
	}
}

// oneValueDesc is a schemaDesc and the storage of its one value, so that
// s:gt and its siblings record their definition in one allocation.
type oneValueDesc struct {
	schemaDesc
	one [1]descValue
}

// keyedConstraint records the key and the types of s:has-key and
// s:may-have-key.
func keyedConstraint(op string, fn lisp.LBuiltin) lisp.LBuiltin {
	return func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		v := fn(env, args)
		key, _ := lisp.GoString(args.Cells[0])
		return describeAs(v, &schemaDesc{op: op, key: key, children: argDescs(args.Cells[1:])})
	}
}

// compositeConstraint records the constraints or types a constraint such as
// s:of or s:no-other-keys holds.
func compositeConstraint(op string, fn lisp.LBuiltin) lisp.LBuiltin {
	return func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return describeAs(fn(env, args), &schemaDesc{op: op, children: argDescs(args.Cells)})
	}
}

// whenConstraint records s:when: the key, its condition, the key the rest
// apply to and the rest.
func whenConstraint(fn lisp.LBuiltin) lisp.LBuiltin {
	return func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		v := fn(env, args)
		if len(args.Cells) < 3 {
			return v
		}
		key, _ := lisp.GoString(args.Cells[0])
		children := append([]*schemaDesc{argDesc(args.Cells[1])}, argDescs(args.Cells[3:])...)
		return describeAs(v, &schemaDesc{op: "when", key: key, values: []descValue{literal(args.Cells[2])}, children: children})
	}
}
