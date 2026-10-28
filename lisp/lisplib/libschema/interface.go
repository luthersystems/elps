// Copyright © 2026 The ELPS authors

package libschema

import (
	"errors"
	"fmt"

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
	// builds them.  &optional, &rest and &key are allowed.  Nil is ().
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

// ifaceMethod is a method of an interface with its argument counts.
type ifaceMethod struct {
	name string
	arity
}

// arity is the number of arguments a function accepts after the
// receiver: at least min, and at most max, or any number when max is -1.
type arity struct {
	min, max int
}

// anyArity accepts any number of arguments.
var anyArity = arity{min: 0, max: -1}

// arityOf returns the arity of formals, a formal list.
func arityOf(formals *lisp.LVal) arity {
	a := arity{}
	variadic := false
	for _, p := range lisp.ParseFormals(formals) {
		switch p.Kind {
		case lisp.ParamRequired:
			a.min++
			a.max++
		case lisp.ParamOptional:
			a.max++
		case lisp.ParamRest, lisp.ParamKey:
			variadic = true
		}
	}
	if variadic {
		a.max = -1
	}
	return a
}

// accepts reports whether a function of arity a accepts every call that a
// function of arity want accepts.
func (a arity) accepts(want arity) bool {
	if a.min > want.min {
		return false
	}
	if a.max == -1 {
		return true
	}
	return want.max != -1 && a.max >= want.max
}

// String describes the arity for an error message.
func (a arity) String() string {
	switch {
	case a.max == -1:
		return fmt.Sprintf("%d or more arguments", a.min)
	case a.min == a.max:
		return fmt.Sprintf("%d argument(s)", a.min)
	default:
		return fmt.Sprintf("%d to %d arguments", a.min, a.max)
	}
}

// checkFormals returns an error when formals is not a list of symbols.
func checkFormals(formals *lisp.LVal) error {
	if formals.Type != lisp.LSExpr {
		return fmt.Errorf("formals must be a list: %v", formals)
	}
	for _, p := range formals.Cells {
		if p.Type != lisp.LSymbol || p.Str == "" || p.Str[0] == ':' {
			return fmt.Errorf("formal %v is not a name", p)
		}
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
		if err := checkFormals(formals); err != nil {
			return nil, fmt.Errorf("libschema: interface %s: method %s: %w", spec.Name, m.Name, err)
		}
		in.methods = append(in.methods, ifaceMethod{name: m.Name, arity: arityOf(formals)})
	}
	return in, nil
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
		ms[i] = lisp.Cells{lisp.String(m.name), lisp.Int(m.min), lisp.Int(m.max)}.List()
	}
	caps := lisp.Cells{lisp.String(in.name), lisp.QExpr(ms)}.List()
	v := newCapturedValidator(env, lisp.Formals("input"), caps, func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		name := captures.Cells[0].Str
		cells := captures.Cells[1].Cells
		methods := make([]ifaceMethod, len(cells))
		for i, c := range cells {
			methods[i] = ifaceMethod{name: c.Cells[0].Str, arity: arity{min: c.Cells[1].Int, max: c.Cells[2].Int}}
		}
		if err := satisfies(name, methods, input); err != nil {
			return env.ErrorConditionf(WrongType, "%v", err)
		}
		return lisp.Nil()
	})
	return describeAs(v, &schemaDesc{op: "interface", name: in.name})
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

// lispMethodArity returns the arity of a Lisp method after its receiver.
func lispMethodArity(m *lisp.LVal) arity {
	formals := m.Cells[0]
	if formals == nil || formals.Type != lisp.LSExpr || len(formals.Cells) == 0 {
		return anyArity
	}
	return arityOf(lisp.SExpr(formals.Cells[1:]))
}
