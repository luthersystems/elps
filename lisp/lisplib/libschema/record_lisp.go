// Copyright © 2026 The ELPS authors

package libschema

import (
	"github.com/luthersystems/elps/internal/funraw"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// The Lisp surface of record types: the macros s:make-record, s:method and
// s:defmethod, and the builtins they expand to.
//
// A library builtin runs in its own package (#736): it cannot see the
// caller's package, build a lambda there or bind a name there.  So the
// macros only build forms.  Each method becomes a lisp:lambda in the
// expansion, which the caller evaluates in its own package, and the type
// takes its package from those lambdas.  The expansion always holds at least
// one lambda (a marker when there are no methods), so every type has the
// package of the code that defined it.  A durable dump names the type by
// that package and the global that holds it.

// recordMacros are the record macros of package s.
//
//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddMacros)
var recordMacros = []*libutil.Builtin{
	libutil.FunctionDoc("make-record", lisp.Formals("name", lisp.VarArgSymbol, "forms"), macroMakeRecord,
		`Defines a record type and returns it.  The type is a
		constructor: (type :field value ...) or (type field-map) builds
		an object, which is checked against the fields.  Each form is a
		field, (s:has-key "f" ...) or (s:may-have-key "f" ...); a method,
		(s:method name (recv args...) body...), whose first formal is the
		object, as a Go method receiver is; or the option :check fn, a function of the new object that
		raises an error to refuse it.  Call an object as (obj 'name
		args...): a method runs, and a field name returns the field.  Any
		other name raises unknown-operation.  Bind the type with a
		top-level set: a saved object names that global.`),
	libutil.FunctionDoc("method", lisp.Formals("name", "formals", lisp.VarArgSymbol, "body"), macroMethod,
		`Declares a method inside s:make-record: (s:method name (recv
		args...) body...).  The first formal, recv, is the object the
		method is called on; the call (obj 'name args...) passes only
		args.  Outside s:make-record it is an error.`),
	libutil.FunctionDoc("defmethod", lisp.Formals("type", "name", "formals", lisp.VarArgSymbol, "body"), macroDefMethod,
		`Adds a method to a record type that the same package defined:
		(s:defmethod type name (recv args...) body...).  The first
		formal, recv, is the object, as in s:method.  It refuses a name that is a field of the type,
		and a name the type already has unless the body starts with
		:replace true.`),
}

// recordBuiltins are the record builtins of package s.
//
//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
var recordBuiltins = []*libutil.Builtin{
	libutil.FunctionDoc("%make-record", lisp.Formals("name", "fields", "methods", "check", "marker"), builtinMakeRecord,
		`The function s:make-record expands to.  Do not call it
		directly.`),
	libutil.FunctionDoc("%add-method", lisp.Formals("type", "name", "fn", "replace"), builtinAddMethod,
		`The function s:defmethod expands to.  Do not call it
		directly.`),
	libutil.FunctionDoc("set!", lisp.Formals("obj", "field", "value"), builtinRecordSet,
		`Sets a field of a record object, (s:set! obj 'field value),
		and returns the value.  The field's validator checks the value
		first; a failure leaves the object as it was.  Use it in method
		bodies only: code outside the type changes an object through its
		methods.`),
	libutil.FunctionDoc("fields", lisp.Formals("obj"), builtinRecordFields,
		`Returns a copy of a record object's fields as a sorted map.
		Store this map, not the object, in a ledger or other plain
		data; pass it back to the type to rebuild the object.`),
	libutil.FunctionDoc("record?", lisp.Formals("value"), builtinIsRecord,
		`Returns true if value is a record object.`),
}

// isMethodForm reports whether form is (s:method ...) or (method ...).
func isMethodForm(form *lisp.LVal) bool {
	if form.Type != lisp.LSExpr || form.IsNil() || len(form.Cells) == 0 {
		return false
	}
	head := form.Cells[0]
	return head.Type == lisp.LSymbol && (head.Str == DefaultPackageName+":method" || head.Str == "method")
}

// methodForm is the source of one method: its name, formals and body.
type methodForm struct {
	name, formals *lisp.LVal
	body          []*lisp.LVal
}

// methodLambda returns (lisp:lambda (recv args...) body...) for m.  The
// first formal is the receiver: the object the method is called on.
func methodLambda(env *lisp.LEnv, who string, m methodForm) *lisp.LVal {
	name, formals, body := m.name, m.formals, m.body
	if name.Type != lisp.LSymbol || name.Str == "" || name.Str[0] == ':' {
		return env.ErrorConditionf(BadArgs, "%s: method name must be a symbol: %v", who, name)
	}
	if formals.Type != lisp.LSExpr {
		return env.ErrorConditionf(BadArgs, "%s: method %s: formals must be a list: %v", who, name.Str, formals)
	}
	if len(formals.Cells) == 0 {
		return env.ErrorConditionf(BadArgs, "%s: method %s: the first formal is the receiver, the object the method is called on: (%s (obj args...) ...)", who, name.Str, name.Str)
	}
	for i, p := range formals.Cells {
		if p.Type != lisp.LSymbol {
			return env.ErrorConditionf(BadArgs, "%s: method %s: formal %v is not a symbol", who, name.Str, p)
		}
		if i == 0 && (p.Str == "" || p.Str[0] == '&' || p.Str[0] == ':') {
			return env.ErrorConditionf(BadArgs, "%s: method %s: the receiver %s must be a plain name", who, name.Str, p.Str)
		}
	}
	form := make([]*lisp.LVal, 0, len(body)+2)
	form = append(form, lisp.Symbol("lisp:lambda"), formals)
	form = append(form, body...)
	return lisp.SExpr(form)
}

// quoted returns (lisp:quote v).
func quoted(v *lisp.LVal) *lisp.LVal {
	return lisp.Cells{lisp.Symbol("lisp:quote"), v}.SExpr()
}

// listForm returns (lisp:list items...).
func listForm(items []*lisp.LVal) *lisp.LVal {
	form := make([]*lisp.LVal, 0, len(items)+1)
	form = append(form, lisp.Symbol("lisp:list"))
	form = append(form, items...)
	return lisp.SExpr(form)
}

// macroMakeRecord expands s:make-record into
//
//	(s:%make-record NAME (lisp:list FIELD...) (lisp:list 'm1 (lisp:lambda (recv ...) ...) ...) CHECK (lisp:lambda ()))
//
// The last lambda marks the caller's package when the type has no
// methods.
func macroMakeRecord(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name := args.Cells[0]
	forms := args.Cells[1:]
	var fields, methods []*lisp.LVal
	check := lisp.Nil()
	for i := 0; i < len(forms); i++ {
		form := forms[i]
		switch {
		case isMethodForm(form):
			if len(form.Cells) < 3 {
				return env.ErrorConditionf(BadArgs, "s:make-record: a method needs a name and formals: %v", form)
			}
			fn := methodLambda(env, "s:make-record", methodForm{name: form.Cells[1], formals: form.Cells[2], body: form.Cells[3:]})
			if fn.IsError() {
				return fn
			}
			methods = append(methods, quoted(form.Cells[1]), fn)
		case form.IsSymbol(":check"):
			if i+1 >= len(forms) {
				return env.ErrorConditionf(BadArgs, "s:make-record: :check needs a function")
			}
			i++
			check = forms[i]
		default:
			fields = append(fields, form)
		}
	}
	marker := lisp.Cells{lisp.Symbol("lisp:lambda"), lisp.Nil()}.SExpr()
	return lisp.Cells{
		lisp.Symbol(DefaultPackageName + ":%make-record"),
		name, listForm(fields), listForm(methods), check, marker,
	}.SExpr()
}

// macroMethod is s:method outside s:make-record, which is always an
// error: s:make-record reads its method forms without expanding them.
func macroMethod(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	return env.ErrorConditionf(BadArgs, "s:method is allowed only as a form of s:make-record; use s:defmethod to add a method to a type")
}

// macroDefMethod expands s:defmethod into
//
//	(s:%add-method TYPE 'NAME (lisp:lambda (recv ...) ...) REPLACE)
func macroDefMethod(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	typeForm, name, formals, body := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3:]
	replace := lisp.Nil()
	if len(body) >= 2 && body[0].IsSymbol(":replace") {
		replace = body[1]
		body = body[2:]
	}
	fn := methodLambda(env, "s:defmethod", methodForm{name: name, formals: formals, body: body})
	if fn.IsError() {
		return fn
	}
	return lisp.Cells{
		lisp.Symbol(DefaultPackageName + ":%add-method"),
		typeForm, quoted(name), fn, replace,
	}.SExpr()
}

// builtinMakeRecord builds a record type: s:%make-record.
func builtinMakeRecord(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	nameV, fieldsV, methodsV, check, marker := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3], args.Cells[4]
	name, ok := lisp.GoString(nameV)
	if !ok || name == "" {
		return env.ErrorConditionf(BadArgs, "s:make-record: the name must be a non-empty string: %v", nameV)
	}
	if marker.Type != lisp.LFun || marker.Package() == "" {
		return env.ErrorConditionf(BadArgs, "s:make-record: cannot tell the defining package; use the s:make-record macro")
	}
	pkg := marker.Package()
	desc := &recordDesc{name: name, display: pkg + ":" + name, index: make(map[string]int)}
	validators := make([]*lisp.LVal, 0, len(fieldsV.Cells))
	for _, f := range fieldsV.Cells {
		if f.IsError() {
			return f
		}
		d := descOf(f)
		if d.op != "has-key" && d.op != "may-have-key" {
			return env.ErrorConditionf(BadArgs, "%s: a field must be s:has-key or s:may-have-key, got %v", desc.display, f)
		}
		if desc.hasField(d.key) {
			return env.ErrorConditionf(BadArgs, "%s: field %s is declared twice", desc.display, d.key)
		}
		desc.index[d.key] = len(desc.fields)
		desc.fields = append(desc.fields, d.key)
		validators = append(validators, f)
	}
	if len(methodsV.Cells)%2 != 0 {
		return env.ErrorConditionf(BadArgs, "%s: methods must be name and function pairs", desc.display)
	}
	methods := lisp.SortedMapSized(len(methodsV.Cells) / 2)
	for i := 0; i < len(methodsV.Cells); i += 2 {
		mname, fn := methodsV.Cells[i], methodsV.Cells[i+1]
		if lerr := (methodTarget{desc: desc, pkg: pkg, methods: methods}).check(env, mname, fn); lerr != nil {
			return lerr
		}
		methods.Map().Set(lisp.String(mname.Str), fn)
	}
	if !check.IsNil() && check.Type != lisp.LFun {
		return env.ErrorConditionf(BadArgs, "%s: :check must be a function, got %v", desc.display, check)
	}
	tcaps := lisp.Cells{
		lisp.Native(recordTag{desc: desc}),
		lisp.QExpr(validators),
		methods,
		check,
		lisp.Nil(),
	}.List()
	t := funraw.NewCapturedBuiltin(funraw.CapturedBuiltin{
		Package:  pkg,
		FID:      "record-type:" + name,
		Name:     "record-type:" + desc.display,
		Formals:  lisp.Formals(lisp.VarArgSymbol, "fields"),
		Captures: tcaps,
		Eval:     evalLispType,
	})
	if t.IsError() {
		return t
	}
	tcaps.Cells[typeSelf] = t //elps:mutates the type's own captures, built above and not yet returned
	return t
}

// methodTarget is the type a method is added to: its descriptor, its
// package and its method map.  replace allows a method that the map has
// already.
type methodTarget struct {
	desc    *recordDesc
	methods *lisp.LVal
	pkg     string
	replace bool
}

// check checks one method before it enters the method map.
func (mt methodTarget) check(env *lisp.LEnv, name, fn *lisp.LVal) *lisp.LVal {
	desc, pkg, methods, replace := mt.desc, mt.pkg, mt.methods, mt.replace
	if name.Type != lisp.LSymbol || name.Str == "" {
		return env.ErrorConditionf(BadArgs, "%s: method name must be a symbol: %v", desc.display, name)
	}
	if fn.Type != lisp.LFun || fn.IsSpecialFun() {
		return env.ErrorConditionf(BadArgs, "%s: method %s is not a function", desc.display, name.Str)
	}
	if fn.Package() != pkg {
		return env.ErrorConditionf(BadArgs, "%s: method %s is defined in package %s; a type takes methods only from its own package, %s",
			desc.display, name.Str, fn.Package(), pkg)
	}
	if desc.hasField(name.Str) {
		return env.ErrorConditionf(BadArgs, "%s: method %s has the name of a field", desc.display, name.Str)
	}
	if _, dup := methods.Map().Get(lisp.String(name.Str)); dup && !replace {
		return env.ErrorConditionf(BadArgs, "%s: method %s is defined already; give :replace true to replace it", desc.display, name.Str)
	}
	return nil
}

// builtinAddMethod adds a method to a Lisp record type: s:%add-method.
func builtinAddMethod(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	t, name, fn, replace := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	lt, ok := lispTypeOf(t)
	desc, tcaps := lt.desc, lt.caps
	if !ok {
		if goObjectType(t) != nil {
			return env.ErrorConditionf(BadArgs, "s:defmethod: a record type defined in Go is sealed")
		}
		return env.ErrorConditionf(BadArgs, "s:defmethod: first argument is not a record type: %v", t)
	}
	methods := tcaps.Cells[typeMethods]
	target := methodTarget{desc: desc, pkg: t.Package(), methods: methods, replace: lisp.True(replace)}
	if lerr := target.check(env, name, fn); lerr != nil {
		return lerr
	}
	methods.Map().Set(lisp.String(name.Str), fn) //elps:mutates s:defmethod adds to the type's own method map
	return name
}
