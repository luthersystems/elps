// Copyright © 2026 The ELPS authors

package libschema

import (
	"errors"
	"fmt"
	"slices"
	"sort"
	"strings"

	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/internal/lambdalist"
	"github.com/luthersystems/elps/internal/templatepolicy"
	"github.com/luthersystems/elps/lisp"
)

// Interfaces (luthersystems/elps#900).
//
// An interface names a set of methods, as a Go interface does.  A value
// satisfies it when it is a record object, of a Lisp or a Go type, whose
// type has each method, and each method accepts every call the interface
// allows.  A field (with a getter, for a Go type) counts as a method
// that takes no arguments.  The check is structural: the type does not declare the
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
	// Embed are interfaces whose methods this one has too, as a Go
	// interface embeds one.  Their methods come first, in Embed order,
	// then Methods.  A name may come from more than one place only with
	// the same arity; a different arity is an error.
	Embed []*Interface
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
	from := make(map[string]string) // method name to the interface it came from
	for i, e := range spec.Embed {
		if e == nil {
			return nil, fmt.Errorf("libschema: interface %s: Embed[%d] is nil", spec.Name, i)
		}
		for _, m := range e.methods {
			if err := in.addEmbedded(m, e.name, from); err != nil {
				return nil, err
			}
		}
	}
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
		if err := in.addEmbedded(ifaceMethod{name: m.Name, formals: text, arity: arityOf(formals)}, spec.Name, from); err != nil {
			return nil, err
		}
	}
	return in, nil
}

// addEmbedded adds m, which comes from the interface src, unless in has a
// method of that name already.  The two must then have the same arity.
func (in *Interface) addEmbedded(m ifaceMethod, src string, from map[string]string) error {
	i := slices.IndexFunc(in.methods, func(h ifaceMethod) bool { return h.name == m.name })
	if i < 0 {
		in.methods = append(in.methods, m)
		from[m.name] = src
		return nil
	}
	if !in.methods[i].equal(m.arity) {
		return fmt.Errorf("libschema: interface %s: method %s takes %v in %s and %v in %s",
			in.name, m.name, in.methods[i].arity, from[m.name], m.arity, src)
	}
	return nil
}

// equal reports whether a and b accept the same calls.
func (a arity) equal(b arity) bool {
	return a.min == b.min && a.max == b.max && a.none == b.none && slices.Equal(a.keys, b.keys)
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

// ifaceTag carries an interface in the captures of its validator.  An
// Interface never changes, so every fork of a template shares it.  Its Go
// type marks the validator: Lisp code cannot build a native payload.
type ifaceTag struct {
	templatepolicy.Marker
	in *Interface //elpsvet:allow-marker an Interface is immutable after NewInterface
}

var _ templatepolicy.Immutable = ifaceTag{}

// TransientNative marks ifaceTag as never saved: an interface is never
// saved.
func (ifaceTag) TransientNative() {}

// Validator returns the interface as a schema validator for env.
func (in *Interface) Validator(env *lisp.LEnv) *lisp.LVal {
	caps := lisp.Cells{lisp.Native(ifaceTag{in: in})}.List()
	v := newCapturedValidator(env, lisp.Formals("input"), caps, func(env *lisp.LEnv, input, captures *lisp.LVal) *lisp.LVal {
		tag, _ := lisp.NativeValue[ifaceTag](captures.Cells[0])
		if err := tag.in.Satisfies(input); err != nil {
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

// interfaceOf returns the interface whose validator is v, or nil.
func interfaceOf(v *lisp.LVal) *Interface {
	if !isValidator(v) {
		return nil
	}
	caps := funraw.Captures(v)
	if caps == nil || len(caps.Cells) != 1 {
		return nil
	}
	tag, ok := lisp.NativeValue[ifaceTag](caps.Cells[0])
	if !ok {
		return nil
	}
	return tag.in
}

// satisfies is Interface.Satisfies over an interface's parts.
func satisfies(name string, methods []ifaceMethod, v *lisp.LVal) error {
	have, ok := methodArities(v)
	if !ok {
		return fmt.Errorf("Input is not a record object, so it is not a %s", name)
	}
	return matchMethods(name, methods, have, true)
}

// matchMethods checks the methods have looks up against an interface's
// methods.  A method that is missing is an error only when all is true.
func matchMethods(name string, methods []ifaceMethod, have func(string) (arity, bool), all bool) error {
	for _, m := range methods {
		a, ok := have(m.name)
		if !ok {
			if !all {
				continue
			}
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
		return lispTypeArities(o.caps), true
	}
	if t := goObjectType(v); t != nil {
		return t.methodArity, true
	}
	return nil, false
}

// fieldArity is the arity of a field read as a method: (obj 'field) takes
// no arguments.  A field satisfies an interface method that takes none.
var fieldArity = arity{}

// methodArity looks up the arity of a method of t.  A field with a getter
// counts as a method that takes no arguments.
func (t *RecordType) methodArity(name string) (arity, bool) {
	if t.methods[name] == nil {
		if i, ok := t.index[name]; ok && t.spec.Fields[i].Getter {
			return fieldArity, true
		}
		return arity{}, false
	}
	return t.arity[name], true
}

// lispTypeArities returns a lookup of the arities of the methods of the
// Lisp type with captures tcaps.  Each arity was stored when its method
// was added, so a lookup parses nothing.  A field counts as a method that
// takes no arguments.
func lispTypeArities(tcaps *lisp.LVal) func(string) (arity, bool) {
	desc := mustTag(tcaps)
	return func(name string) (arity, bool) {
		if a, ok := storedArity(tcaps, name); ok {
			return a, true
		}
		return fieldArity, desc.hasField(name)
	}
}

// storedArity returns the arity stored for the method name of the Lisp
// type with captures tcaps.
func storedArity(tcaps *lisp.LVal, name string) (arity, bool) {
	v, ok := tcaps.Cells[typeArities].Map().Get(lisp.String(name))
	if !ok {
		return arity{}, false
	}
	tag, _ := lisp.NativeValue[arityTag](v)
	return tag.a, true
}

// arityTag carries a Lisp method's arity in its type's captures.  It is
// built when the method is added and never written after.
type arityTag struct {
	templatepolicy.Marker
	a arity //elpsvet:allow-marker built when the method is added and never written after
}

var _ templatepolicy.Immutable = arityTag{}

// TransientNative marks arityTag as never saved: a type's captures are
// never saved.
func (arityTag) TransientNative() {}

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
