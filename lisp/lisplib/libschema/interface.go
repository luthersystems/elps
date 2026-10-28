// Copyright © 2026 The ELPS authors

package libschema

import (
	"errors"
	"fmt"
	"slices"
	"sort"
	"strings"

	"github.com/luthersystems/elps/internal/lambdalist"
	"github.com/luthersystems/elps/lisp"
)

// Interfaces (luthersystems/elps#900).
//
// An interface names a set of methods, as a Go interface does.  A value
// satisfies it when it is a record object, of a Lisp or a Go type, whose
// type has each method, and each method accepts every call the interface
// allows.  The check is structural: the type does not declare the
// interface, and a method that s:defmethod adds later counts.
//
// An interface is a schema validator, so it is the type of a field, an
// s:of element or an s:validate call like any other type.  It is never
// saved: an object saves its own type.

// InterfaceMethod is one method of an interface.
type InterfaceMethod struct {
	// Formals are the method's formals after the receiver, as lisp.Formals
	// builds them.  &optional, &rest and &key are allowed.  Nil is ().  A
	// formal may be a typed formal, (name type) as [name type] reads: the
	// type is not evaluated and not part of the match; it documents the
	// method for readers and lint.
	Formals *lisp.LVal
	// Name is the op that calls the method.
	Name string
}

// InterfaceSpec describes an interface.
type InterfaceSpec struct {
	// Name is the interface's name.  Errors and descriptions use it.
	Name string
	// Methods are the methods a value must have.
	Methods []InterfaceMethod
}

// Interface is a set of methods that a record object must have.  It is
// immutable and safe to share between runtimes and goroutines.
type Interface struct {
	name    string
	methods []ifaceMethod
}

// ifaceMethod is a method of an interface with its signature.
type ifaceMethod struct {
	name    string
	formals string // the formals as written, for s:describe
	arity
}

// arity is the shape of the calls a function accepts after the receiver:
// at least min positional arguments and at most max, or any number when
// max is -1 (&rest).  keys are its &key names, sorted; a call may follow
// the max positional arguments with :key value pairs of those names.
type arity struct {
	keys     []string
	min, max int
	none     bool // no call at all: the function cannot take a receiver
}

// anyArity accepts any call.
var anyArity = arity{min: 0, max: -1}

// arityOf returns the arity of formals, a formal list checkFormals accepts.
func arityOf(formals *lisp.LVal) arity {
	a := arity{}
	for _, p := range lisp.ParseFormals(formals) {
		switch p.Kind {
		case lisp.ParamRequired:
			a.min++
			a.max++
		case lisp.ParamOptional:
			a.max++
		case lisp.ParamKey:
			a.keys = append(a.keys, p.Name)
		case lisp.ParamRest:
			a.max = -1
		}
	}
	if a.max == -1 {
		return arity{min: a.min, max: -1}
	}
	sort.Strings(a.keys)
	return a
}

// accepts reports whether a function of arity a accepts every call that a
// function of arity want accepts.
func (a arity) accepts(want arity) bool {
	switch {
	case a.none || want.none:
		return false
	case a.min > want.min:
		return false
	case a.max == -1:
		return true
	case want.max == -1 || a.max < want.max:
		return false
	case len(want.keys) == 0:
		return true
	}
	// The optional arguments take what comes first, so a function with
	// more positional slots would bind the keyword pairs to them.
	if a.max != want.max {
		return false
	}
	for _, k := range want.keys {
		if _, ok := slices.BinarySearch(a.keys, k); !ok {
			return false
		}
	}
	return true
}

// checkCall returns an error message when args is not a call that a
// function of arity a accepts, or "".
func (a arity) checkCall(args []*lisp.LVal) string {
	n := len(args)
	switch {
	case n < a.min:
		return fmt.Sprintf("takes %v, got %d", a, n)
	case a.max == -1 || n <= a.max:
		return ""
	case len(a.keys) == 0:
		return fmt.Sprintf("takes %v, got %d", a, n)
	case (n-a.max)%2 != 0:
		return "got an odd number of keyword arguments"
	}
	for i := a.max; i < n; i += 2 {
		k := args[i]
		if k.Type != lisp.LSymbol || !strings.HasPrefix(k.Str, ":") {
			return fmt.Sprintf("argument %v is not a keyword", k)
		}
		if _, ok := slices.BinarySearch(a.keys, k.Str[1:]); !ok {
			return fmt.Sprintf("has no keyword argument %v", k)
		}
	}
	return ""
}

// String describes the arity for an error message.
func (a arity) String() string {
	var s string
	switch {
	case a.none:
		return "no arguments, not even the receiver"
	case a.max == -1:
		s = fmt.Sprintf("%d or more arguments", a.min)
	case a.min == a.max:
		s = fmt.Sprintf("%d argument(s)", a.min)
	default:
		s = fmt.Sprintf("%d to %d arguments", a.min, a.max)
	}
	if len(a.keys) > 0 {
		s += " and keywords :" + strings.Join(a.keys, " :")
	}
	return s
}

// checkFormals returns an error when formals is not a formal list a lambda
// accepts.
func checkFormals(formals *lisp.LVal) error {
	if formals.Type != lisp.LSExpr {
		return fmt.Errorf("formals must be a list: %v", formals)
	}
	for _, p := range formals.Cells {
		if p.Type != lisp.LSymbol {
			return fmt.Errorf("formal %v is not a name", p)
		}
	}
	if _, msg := lambdalist.Validate(len(formals.Cells), func(i int) string { return formals.Cells[i].Str }); msg != "" {
		return errors.New(msg)
	}
	return nil
}

// NewInterface checks spec and returns the interface.
func NewInterface(spec InterfaceSpec) (*Interface, error) {
	if spec.Name == "" {
		return nil, errors.New("libschema: an interface needs a Name")
	}
	in := &Interface{name: spec.Name, methods: make([]ifaceMethod, 0, len(spec.Methods))}
	seen := make(map[string]bool, len(spec.Methods))
	for i, m := range spec.Methods {
		if m.Name == "" || m.Name[0] == ':' || m.Name[0] == '&' {
			return nil, fmt.Errorf("libschema: interface %s: method %d has no name", spec.Name, i)
		}
		if seen[m.Name] {
			return nil, fmt.Errorf("libschema: interface %s: method %s is declared twice", spec.Name, m.Name)
		}
		seen[m.Name] = true
		formals := m.Formals
		if formals == nil {
			formals = lisp.Formals()
		}
		text := formalsText(formals)
		formals = untypedFormals(formals)
		if err := checkFormals(formals); err != nil {
			return nil, fmt.Errorf("libschema: interface %s: method %s: %w", spec.Name, m.Name, err)
		}
		in.methods = append(in.methods, ifaceMethod{name: m.Name, formals: text, arity: arityOf(formals)})
	}
	return in, nil
}

// formalsText writes formals as "(a &optional [b s:string])".
func formalsText(formals *lisp.LVal) string {
	if formals.Type != lisp.LSExpr {
		return formals.String()
	}
	names := make([]string, len(formals.Cells))
	for i, p := range formals.Cells {
		if isTypedFormal(p) {
			names[i] = "[" + p.Cells[0].Str + " " + p.Cells[1].String() + "]"
			continue
		}
		names[i] = p.String()
	}
	return "(" + strings.Join(names, " ") + ")"
}

// isTypedFormal reports whether p is a typed formal, [name type] (#905).
func isTypedFormal(p *lisp.LVal) bool {
	return p.Type == lisp.LSExpr && len(p.Cells) == 2 && p.Cells[0].Type == lisp.LSymbol
}

// untypedFormals returns formals with each typed formal replaced by its
// name.  An interface matches on names and arity only: its types are a
// contract for the reader and for lint, not part of the match (#905).
func untypedFormals(formals *lisp.LVal) *lisp.LVal {
	if formals.Type != lisp.LSExpr || !slices.ContainsFunc(formals.Cells, isTypedFormal) {
		return formals
	}
	plain := make([]*lisp.LVal, len(formals.Cells))
	for i, p := range formals.Cells {
		if isTypedFormal(p) {
			p = p.Cells[0]
		}
		plain[i] = p
	}
	return lisp.SExpr(plain)
}

// Name returns the interface's name.
func (in *Interface) Name() string { return in.name }

// Satisfies returns nil when v is a record object that has each method of
// the interface, or an error that says what is missing.
func (in *Interface) Satisfies(v *lisp.LVal) error {
	return satisfies(in.name, in.methods, v)
}

// Validator returns the interface as a schema validator for env.  Its
// captures hold the interface as Lisp data, so a fork of a template gets
// its own copy.
func (in *Interface) Validator(env *lisp.LEnv) *lisp.LVal {
	ms := make([]*lisp.LVal, len(in.methods))
	for i, m := range in.methods {
		keys := make([]*lisp.LVal, len(m.keys))
		for j, k := range m.keys {
			keys[j] = lisp.String(k)
		}
		ms[i] = lisp.Cells{lisp.String(m.name), lisp.Int(m.min), lisp.Int(m.max), lisp.QExpr(keys)}.List()
	}
	caps := lisp.Cells{lisp.String(in.name), lisp.QExpr(ms)}.List()
	v := newCapturedValidator(env, lisp.Formals("input"), caps, func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		name := captures.Cells[0].Str
		cells := captures.Cells[1].Cells
		methods := make([]ifaceMethod, len(cells))
		for i, c := range cells {
			var keys []string
			for _, k := range c.Cells[3].Cells {
				keys = append(keys, k.Str)
			}
			methods[i] = ifaceMethod{name: c.Cells[0].Str, arity: arity{keys: keys, min: c.Cells[1].Int, max: c.Cells[2].Int}}
		}
		if err := satisfies(name, methods, input); err != nil {
			return env.ErrorConditionf(WrongType, "%v", err)
		}
		return lisp.Nil()
	})
	sigs := make([]descValue, len(in.methods))
	for i, m := range in.methods {
		sigs[i] = descValue{kind: lisp.LString, s: m.name + " " + m.formals}
	}
	return describeAs(v, &schemaDesc{op: "interface", name: in.name, values: sigs})
}

// satisfies is Interface.Satisfies over an interface's parts.
func satisfies(name string, methods []ifaceMethod, v *lisp.LVal) error {
	have, ok := methodArities(v)
	if !ok {
		return fmt.Errorf("Input is not a record object, so it is not a %s", name)
	}
	for _, m := range methods {
		a, ok := have(m.name)
		if !ok {
			return fmt.Errorf("Input is not a %s: it has no method %s", name, m.name)
		}
		if !a.accepts(m.arity) {
			return fmt.Errorf("Input is not a %s: method %s takes %v, and %s needs it to take %v", name, m.name, a, name, m.arity)
		}
	}
	return nil
}

// methodArities returns a lookup of the methods of v's type and their
// arities, and false when v is not a record object.
func methodArities(v *lisp.LVal) (func(string) (arity, bool), bool) {
	if o, ok := lispObjectOf(v); ok {
		methods := o.caps.Cells[typeMethods].Map()
		return func(name string) (arity, bool) {
			m, ok := methods.Get(lisp.String(name))
			if !ok || m.Type != lisp.LFun {
				return arity{}, false
			}
			return lispMethodArity(m), true
		}, true
	}
	if t := goObjectType(v); t != nil {
		return func(name string) (arity, bool) {
			if t.methods[name] == nil {
				return arity{}, false
			}
			return t.arity[name], true
		}, true
	}
	return nil, false
}

// lispMethodArity returns the arity of a Lisp method after its receiver,
// the first argument.
func lispMethodArity(m *lisp.LVal) arity {
	formals := m.Cells[0]
	if formals == nil || formals.Type != lisp.LSExpr {
		return anyArity
	}
	a := arityOf(formals)
	switch a.max {
	case -1:
		return arity{min: max(a.min-1, 0), max: -1}
	case 0:
		return arity{none: true}
	}
	a.min = max(a.min-1, 0)
	a.max--
	return a
}
