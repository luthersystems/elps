// Copyright © 2026 The ELPS authors

package libschema

import (
	"errors"
	"fmt"
	"strings"
	"sync"
	"unicode"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/libjson"
)

// Record types defined in Go (luthersystems/elps#895).
//
// A Go record type has the call syntax of a Lisp one, (obj 'op args...),
// but its fields and methods are Go.  Its objects keep their fields in
// positions (field i is capture i), and the dispatch closes over the
// immutable *RecordType, so a fork of a template shares the type and gets
// private copies of the fields.  A Go type is sealed: Lisp code cannot add
// methods to it or set its fields.
//
// Each Go type has its own durable codec (Codec).  Its Load runs the Go
// field checks and Check, which run no Lisp, so a loaded Go object is
// validated at once.

// RecordSpec describes a record type defined in Go.
type RecordSpec struct {
	// Check, if not nil, runs after the field checks when an object is
	// built or loaded.  It must not run Lisp.
	Check func(r Record) error
	// Default, if not nil, runs for an op that no method or getter
	// matches.  Without it such an op raises UnknownOp.  Like a method,
	// it runs under Check, and a call that fails leaves the fields as they
	// were.
	Default func(env *lisp.LEnv, r Record, op *lisp.LVal, args []*lisp.LVal) *lisp.LVal
	// Package and FID identify the objects of the type, as for
	// lisp.CapturedFun.  FID defaults to Name.
	Package string
	FID     string
	// Name is the type's name.
	Name string
	// Doc describes the type.
	Doc string
	// CodecName is the name the durable codec writes into every saved
	// document.  It must never change.  It defaults to Package+":"+FID.
	CodecName string
	// DisplayName is the name objects print as, #<DisplayName>.  It
	// defaults to Package+":"+Name.
	DisplayName string
	// UnknownOp is the condition raised for an unknown op.  It defaults to
	// UnknownOperation.
	UnknownOp string
	// Fields are the fields in capture order.
	Fields []FieldSpec
	// Methods are the methods.  A method may not have the name of a field
	// that has a getter.
	Methods []MethodSpec
}

// FieldSpec describes one field of a Go record type.
type FieldSpec struct {
	// Check, if not nil, checks a value of the field when an object is
	// built, when Record.Set changes the field, and when an object is
	// loaded.  It must not run Lisp.
	Check func(v *lisp.LVal) error
	// Name is the field's name.
	Name string
	// Getter makes (obj 'name) return the field.
	Getter bool
}

// MethodSpec describes one method of a Go record type.
type MethodSpec struct {
	// Call runs the method.  args are the arguments after the op.  When
	// Formals is set, their number is checked before Call runs; without
	// it, Call checks it.
	Call func(env *lisp.LEnv, r Record, args []*lisp.LVal) *lisp.LVal
	// Formals, if not nil, are the method's formals after the receiver,
	// as lisp.Formals builds them (&optional, &rest and &key allowed).  A
	// call with a number of arguments they do not accept fails with
	// bad-arguments, and an interface (NewInterface, s:make-interface)
	// checks them.  Without Formals the method accepts any number of
	// arguments as far as an interface is concerned.
	Formals *lisp.LVal
	// Name is the op that calls the method.
	Name string
	// Doc describes the method.
	Doc string
}

// RecordType is a record type defined in Go.  It is immutable and safe to
// share between runtimes and goroutines.
type RecordType struct {
	methods map[string]*MethodSpec
	// arity is the arity of each method, from its Formals.
	arity map[string]arity
	index map[string]int
	spec  RecordSpec
}

// goTypes maps the package and FID of each Go record type to the type, so
// that s:fields, s:set! and s:record? recognize a Go object.  A type is
// registered once, by NewRecordType, and never removed.
var goTypes sync.Map //elpsvet:allow written only by NewRecordType, which Go code calls at setup, never a builtin

func goTypeKey(pkg, fid string) string { return pkg + "\x00" + fid }

// NewRecordType checks spec, fills in its defaults and returns the type.
// It returns an error when another type already uses the package and FID.
func NewRecordType(spec RecordSpec) (*RecordType, error) {
	if spec.Package == "" || spec.Name == "" {
		return nil, errors.New("libschema: a record type needs a Package and a Name")
	}
	if spec.FID == "" {
		spec.FID = spec.Name
	}
	if spec.CodecName == "" {
		spec.CodecName = spec.Package + ":" + spec.FID
	}
	if spec.DisplayName == "" {
		spec.DisplayName = spec.Package + ":" + spec.Name
	}
	if spec.UnknownOp == "" {
		spec.UnknownOp = UnknownOperation
	}
	if strings.IndexFunc(spec.DisplayName, func(r rune) bool { return r == '>' || unicode.IsSpace(r) }) >= 0 {
		return nil, fmt.Errorf("libschema: record type %s: display name %q is not one word without '>'", spec.Name, spec.DisplayName)
	}
	t := &RecordType{
		spec:    spec,
		index:   make(map[string]int, len(spec.Fields)),
		methods: make(map[string]*MethodSpec, len(spec.Methods)),
		arity:   make(map[string]arity, len(spec.Methods)),
	}
	t.spec.Fields = append([]FieldSpec(nil), spec.Fields...)
	t.spec.Methods = append([]MethodSpec(nil), spec.Methods...)
	for i, f := range t.spec.Fields {
		if f.Name == "" {
			return nil, fmt.Errorf("libschema: record type %s: field %d has no name", spec.Name, i)
		}
		if _, dup := t.index[f.Name]; dup {
			return nil, fmt.Errorf("libschema: record type %s: field %s is declared twice", spec.Name, f.Name)
		}
		t.index[f.Name] = i
	}
	for i := range t.spec.Methods {
		m := &t.spec.Methods[i]
		switch {
		case m.Name == "" || m.Call == nil:
			return nil, fmt.Errorf("libschema: record type %s: method %d needs a Name and a Call", spec.Name, i)
		case t.methods[m.Name] != nil:
			return nil, fmt.Errorf("libschema: record type %s: method %s is declared twice", spec.Name, m.Name)
		}
		if j, ok := t.index[m.Name]; ok && t.spec.Fields[j].Getter {
			return nil, fmt.Errorf("libschema: record type %s: method %s has the name of a field with a getter", spec.Name, m.Name)
		}
		t.methods[m.Name] = m
		t.arity[m.Name] = anyArity
		if m.Formals != nil {
			if err := checkFormals(m.Formals); err != nil {
				return nil, fmt.Errorf("libschema: record type %s: method %s: %w", spec.Name, m.Name, err)
			}
			t.arity[m.Name] = arityOf(m.Formals)
			m.Formals = m.Formals.Copy()
		}
	}
	if _, loaded := goTypes.LoadOrStore(goTypeKey(spec.Package, spec.FID), t); loaded {
		return nil, fmt.Errorf("libschema: record type %s:%s is defined already", spec.Package, spec.FID)
	}
	return t, nil
}

// goObjectType returns the Go record type of v, or nil.
func goObjectType(v *lisp.LVal) *RecordType {
	if v == nil || v.Type != lisp.LFun || v.Builtin() == nil {
		return nil
	}
	t, ok := goTypes.Load(goTypeKey(v.Package(), v.FID()))
	if !ok {
		return nil
	}
	rt, _ := t.(*RecordType)
	if rt == nil || !rt.Is(v) {
		return nil
	}
	return rt
}

// Spec returns a copy of the type's spec, with the defaults filled in.
func (t *RecordType) Spec() RecordSpec {
	s := t.spec
	s.Fields = append([]FieldSpec(nil), t.spec.Fields...)
	s.Methods = append([]MethodSpec(nil), t.spec.Methods...)
	for i := range s.Methods {
		if s.Methods[i].Formals != nil {
			s.Methods[i].Formals = s.Methods[i].Formals.Copy()
		}
	}
	return s
}

// New builds an object from one value per field, in field order.  It runs
// the field checks and Check, and returns an error value when one fails.
// The object owns a new capture list; the values themselves are shared.
func (t *RecordType) New(env *lisp.LEnv, fields ...*lisp.LVal) *lisp.LVal {
	if len(fields) != len(t.spec.Fields) {
		return env.ErrorConditionf(BadArgs, "%s: %d fields given, %d expected", t.spec.DisplayName, len(fields), len(t.spec.Fields))
	}
	caps := lisp.QExpr(lisp.Cells(fields).Clone())
	if err := t.check(caps); err != nil {
		return env.ErrorConditionf(WrongType, "%s: %v", t.spec.DisplayName, err)
	}
	return t.object(caps)
}

// check runs the field checks and Check over caps.
func (t *RecordType) check(caps *lisp.LVal) error {
	for i, f := range t.spec.Fields {
		if f.Check == nil {
			continue
		}
		if err := f.Check(caps.Cells[i]); err != nil {
			return fmt.Errorf("field %s: %w", f.Name, err)
		}
	}
	if t.spec.Check != nil {
		return t.spec.Check(Record{caps: caps, t: t})
	}
	return nil
}

// object returns an object over caps.
func (t *RecordType) object(caps *lisp.LVal) *lisp.LVal {
	return funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		Package:  t.spec.Package,
		FID:      t.spec.FID,
		Name:     t.spec.DisplayName,
		Formals:  objectFormals(),
		Captures: caps,
		Eval:     t.eval,
	})
}

// eval runs (obj op args...) for an object of t.
func (t *RecordType) eval(env *lisp.LEnv, args, caps *lisp.LVal) *lisp.LVal {
	if len(args.Cells) == 0 {
		return env.ErrorConditionf(BadArgs, "%s: no operation given", t.spec.DisplayName)
	}
	op, rest := args.Cells[0], args.Cells[1:]
	obj := Record{caps: caps, t: t}
	if op.Type == lisp.LSymbol {
		if m := t.methods[op.Str]; m != nil {
			if a := t.arity[op.Str]; len(rest) < a.min || (a.max >= 0 && len(rest) > a.max) {
				return env.ErrorConditionf(BadArgs, "%s: method %s takes %v, got %d", t.spec.DisplayName, op.Str, a, len(rest))
			}
			return t.guarded(env, obj, func() *lisp.LVal { return m.Call(env, obj, rest) })
		}
		if i, ok := t.index[op.Str]; ok && t.spec.Fields[i].Getter {
			if len(rest) > 0 {
				return env.ErrorConditionf(BadArgs, "%s: field %s takes no arguments, got %d", t.spec.DisplayName, op.Str, len(rest))
			}
			return caps.Cells[i]
		}
	}
	if t.spec.Default != nil {
		return t.guarded(env, obj, func() *lisp.LVal { return t.spec.Default(env, obj, op, rest) })
	}
	return env.ErrorCondition(t.spec.UnknownOp, op)
}

// guarded runs call, a method or Default on obj.  Set checks one field;
// Check runs when call returns, so every object a caller sees, and every
// object a dump saves, loads again.  A call that fails, or breaks Check,
// leaves the fields as they were.
func (t *RecordType) guarded(env *lisp.LEnv, obj Record, call func() *lisp.LVal) *lisp.LVal {
	caps := obj.caps
	saved := lisp.Cells(caps.Cells).Clone()
	res := call()
	if !res.IsError() && t.spec.Check != nil {
		if err := t.spec.Check(obj); err != nil {
			res = env.ErrorConditionf(WrongType, "%s: %v", t.spec.DisplayName, err)
		}
	}
	if res.IsError() {
		copy(caps.Cells, saved) //elps:mutates restores the object's own fields after a failed method
	}
	return res
}

// Is reports whether v is an object of t.
func (t *RecordType) Is(v *lisp.LVal) bool {
	_, ok := t.Record(v)
	return ok
}

// Record returns the view over v's fields when v is an object of t.
func (t *RecordType) Record(v *lisp.LVal) (Record, bool) {
	if v == nil || v.Type != lisp.LFun || v.Builtin() == nil ||
		v.Package() != t.spec.Package || v.FID() != t.spec.FID {
		return Record{}, false
	}
	caps := funraw.Captures(v)
	if caps == nil || len(caps.Cells) != len(t.spec.Fields) {
		return Record{}, false
	}
	return Record{caps: caps, t: t}, true
}

// fieldMap returns a new sorted map of the fields of v that have a getter.
func (t *RecordType) fieldMap(v *lisp.LVal) *lisp.LVal {
	r, ok := t.Record(v)
	if !ok {
		return lisp.Errorf("%v is not a %s record", v, t.spec.DisplayName)
	}
	m := lisp.SortedMapSized(len(t.spec.Fields))
	for i, f := range t.spec.Fields {
		// A field with no getter is hidden from Lisp, here as in a call.
		if f.Getter {
			m.Map().Set(lisp.String(f.Name), r.At(i))
		}
	}
	return m
}

// Codec returns the type's durable codec.  Register it in the durable
// registry of every VM that saves the type's objects.  Load checks the
// number of fields, runs the field checks and Check, and rebuilds the
// object over the decoded captures.
func (t *RecordType) Codec() libjson.DurableFunCodec {
	return libjson.DurableFunCodec{
		Package: t.spec.Package,
		FID:     t.spec.FID,
		Name:    t.spec.CodecName,
		Version: 1,
		Load:    t.load,
	}
}

// load is the codec's Load.
func (t *RecordType) load(_ *lisp.LEnv, version int, caps *lisp.LVal) (*lisp.LVal, error) {
	if version != 1 {
		return nil, fmt.Errorf("%s: unknown version %d", t.spec.DisplayName, version)
	}
	if caps == nil || len(caps.Cells) != len(t.spec.Fields) {
		return nil, fmt.Errorf("%s: saved object has the wrong number of fields", t.spec.DisplayName)
	}
	if err := t.check(caps); err != nil {
		return nil, fmt.Errorf("%s: %w", t.spec.DisplayName, err)
	}
	return t.object(caps), nil
}

// Record is a view over the fields of an object of a Go record type.  It
// is valid only during the call that received it.
type Record struct {
	caps *lisp.LVal
	t    *RecordType
}

// Type returns the record's type.
func (r Record) Type() *RecordType { return r.t }

// At returns field i.
func (r Record) At(i int) *lisp.LVal { return r.caps.Cells[i] }

// Field returns the field name, or nil when the type has no such field.
func (r Record) Field(name string) *lisp.LVal {
	i, ok := r.t.index[name]
	if !ok {
		return nil
	}
	return r.caps.Cells[i]
}

// Set checks v with field i's check and stores it.  The type's Check runs
// when the method that called Set returns.  It returns nil, or an
// error value when the check fails, and then field i is not changed.
func (r Record) Set(env *lisp.LEnv, i int, v *lisp.LVal) *lisp.LVal {
	f := r.t.spec.Fields[i]
	if f.Check != nil {
		if err := f.Check(v); err != nil {
			return env.ErrorConditionf(WrongType, "%s: field %s: %v", r.t.spec.DisplayName, f.Name, err)
		}
	}
	r.caps.Cells[i] = v //elps:mutates a Go method writes its own object's field through the record view
	return nil
}
