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
		object, as a Go method receiver is; the option :check fn, a
		function of the new object that raises an error to refuse it;
		or the option :implements (iface...), interfaces the type must
		satisfy.  A method in the form that does not match an interface
		fails at once; a method that is missing fails at the first
		construction, so s:defmethod can still add it.  Call an object as (obj 'name
		args...): a method runs, and a field name returns the field.  Any
		other name raises unknown-operation.  A method call is atomic:
		if it raises an error, or :check refuses the object, the fields
		return to their state before the call.  Bind the type with a
		top-level set: a saved object names that global.`),
	libutil.FunctionDoc("method", lisp.Formals("name", "formals", lisp.VarArgSymbol, "body"), macroMethod,
		`Declares a method inside s:make-record: (s:method name (recv
		args...) body...).  The first formal, recv, is the object the
		method is called on; the call (obj 'name args...) passes only
		args.  A formal after recv may be [name type]: each argument
		the call gives for it is checked before the body runs.  The
		type :self is the type being defined, and [name 'T] resolves T
		on each call.  Outside s:make-record it is an error.`),
	libutil.FunctionDoc("defmethod", lisp.Formals("type", "name", "formals", lisp.VarArgSymbol, "body"), macroDefMethod,
		`Adds a method to a record type that the same package defined:
		(s:defmethod type name (recv args...) body...).  The first
		formal, recv, is the object, and a formal after it may be
		[name type], as in s:method.  It refuses a name that is a field of the type,
		and a name the type already has unless the body starts with
		:replace true.`),
	libutil.FunctionDoc("make-interface", lisp.Formals("name", lisp.VarArgSymbol, "methods"), macroMakeInterface,
		`Defines an interface, a set of methods, and returns it:
		(s:make-interface "payable" (pay (amount)) (cancel ())).  Each
		method is (name (args...)): its formals after the receiver,
		with &optional, &rest and &key allowed.  A formal may be [name
		type]; the type documents the method and is not matched.  The
		option :embed (iface...) adds the methods of other interfaces,
		as a Go interface embeds one; a name may come from two places
		only with the same arity.  A value satisfies the
		interface when it is a record object whose type has each method,
		and each method accepts every call the interface allows, as in
		Go.  The interface is a validator: use it as a field type, in
		s:of, or with s:validate.`),
}

// recordBuiltins are the record builtins of package s.
//
//elpsvet:allow package builtin table; formals are sealed by libutil at construction and shared via registrationFormals (lisp.LEnv.AddBuiltins)
var recordBuiltins = []*libutil.Builtin{
	libutil.FunctionDoc("%make-record", lisp.Formals("name", "fields", "methods", "check", "marker", "&optional", "sigs", "implements"), builtinMakeRecord,
		`The function s:make-record expands to.  Do not call it
		directly.`),
	libutil.FunctionDoc("%add-method", lisp.Formals("type", "name", "fn", "replace", "&optional", "sig"), builtinAddMethod,
		`The function s:defmethod expands to.  Do not call it
		directly.`),
	libutil.FunctionDoc("%make-interface", lisp.Formals("name", "methods", "&optional", "embed"), builtinMakeInterface,
		`The function s:make-interface expands to.  Do not call it
		directly.`),
	libutil.FunctionDoc("set-field!", lisp.Formals("obj", "field", "value"), builtinRecordSet,
		`Sets a field of a record object, (s:set-field! obj 'field value),
		and returns the value.  The field's validator checks the value
		first; a failure leaves the object as it was.  Use it in method
		bodies only: code outside the type changes an object through its
		methods.`),
	libutil.FunctionDoc("fields", lisp.Formals("obj"), builtinRecordFields,
		`Returns a copy of a record object's fields as a sorted map.
		Store this map, not the object, in a ledger or other plain
		data; pass it back to the type to rebuild the object.`),
	libutil.FunctionDoc("is?", lisp.Formals("value", "type"), builtinIs,
		`Returns true if value passes type, and false if not.  type is
		what a typed formal takes: a built-in type name (s:int), a
		record type (the full check), (s:ref T), an interface or
		another validator.  Only wrong-type and failed-constraint mean
		false; any other error, such as a type that is not a type or a
		condition a :check raises itself, propagates.`),
	libutil.FunctionDoc("as", lisp.Formals("value", "type"), builtinAs,
		`Returns value if it passes type, as s:is? checks it, and
		raises the check's error if not.  Use it to check a value in
		an expression: (let ((o (s:as x order))) ...).`),
	libutil.FunctionDoc("ref", lisp.Formals("type"), builtinRef,
		`Returns a type that passes an object of the record type and
		checks nothing else: no field check and no :check, so it is
		cheap.  The type itself, used as a type, checks every field.
		s:ref does not prove the fields are valid: a getter returns a
		field's own value, and code that changes it in place (assoc!)
		is not checked.  Use it for objects your own code made.`),
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
	// returns is the :returns type form, or nil.
	returns *lisp.LVal
	body    []*lisp.LVal
}

// methodLambda returns (lisp:lambda (recv args...) body...) for m, and the
// form that builds its argument types, (lisp:list 'name type ...), or nil
// when no formal has a type.  The first formal is the receiver: the object
// the method is called on.  A formal after it is a name or [name type]
// (#905); the lambda gets the plain names.
func methodLambda(env *lisp.LEnv, who string, m methodForm) (fn, types *lisp.LVal) {
	name, formals, body := m.name, m.formals, m.body
	if name.Type != lisp.LSymbol || name.Str == "" || name.Str[0] == ':' {
		return env.ErrorConditionf(BadArgs, "%s: method name must be a symbol: %v", who, name), nil
	}
	if formals.Type != lisp.LSExpr {
		return env.ErrorConditionf(BadArgs, "%s: method %s: formals must be a list: %v", who, name.Str, formals), nil
	}
	if len(formals.Cells) == 0 {
		return env.ErrorConditionf(BadArgs, "%s: method %s: the first formal is the receiver, the object the method is called on: (%s (obj args...) ...)", who, name.Str, name.Str), nil
	}
	plain := make([]*lisp.LVal, len(formals.Cells))
	var typeForms []*lisp.LVal
	for i, p := range formals.Cells {
		if i > 0 && p.Type == lisp.LSExpr {
			if len(p.Cells) != 2 || p.Cells[0].Type != lisp.LSymbol || p.Cells[0].Str == "" || p.Cells[0].Str[0] == '&' || p.Cells[0].Str[0] == ':' {
				return env.ErrorConditionf(BadArgs, "%s: method %s: a typed formal is [name type]: %v", who, name.Str, p), nil
			}
			typeForms = append(typeForms, typeForm(p.Cells[0], p.Cells[1])...)
			p = p.Cells[0]
		}
		if p.Type != lisp.LSymbol {
			if i == 0 {
				return env.ErrorConditionf(BadArgs, "%s: method %s: the receiver must be a plain name, with no type: %v", who, name.Str, p), nil
			}
			return env.ErrorConditionf(BadArgs, "%s: method %s: formal %v is not a symbol", who, name.Str, p), nil
		}
		if i == 0 && (p.Str == "" || p.Str[0] == '&' || p.Str[0] == ':') {
			return env.ErrorConditionf(BadArgs, "%s: method %s: the receiver %s must be a plain name", who, name.Str, p.Str), nil
		}
		plain[i] = p
	}
	if m.returns != nil {
		typeForms = append(typeForms, typeForm(lisp.Symbol(":returns"), m.returns)...)
	}
	form := make([]*lisp.LVal, 0, len(body)+2)
	form = append(form, lisp.Symbol("lisp:lambda"), lisp.SExpr(plain))
	form = append(form, body...)
	if typeForms != nil {
		types = listForm(typeForms)
	}
	return lisp.SExpr(form), types
}

// typeForm returns the pair of forms lispMethodSig reads for the type typ
// of name, a formal or :returns: 'name and typ.  For a late type, 'T, it
// is '(name) and a lambda in the caller's package that returns T.
func typeForm(name, typ *lisp.LVal) []*lisp.LVal {
	if typ.Type == lisp.LSymbol && typ.IsQuoted() {
		thunk := lisp.Cells{lisp.Symbol("lisp:lambda"), lisp.Nil(), lisp.Symbol(typ.Str)}.SExpr()
		return []*lisp.LVal{quoted(lisp.Cells{name}.SExpr()), thunk}
	}
	return []*lisp.LVal{quoted(name), typ}
}

// methodOptions reads the options at the start of a method body, in any
// order: :replace v (s:defmethod only) and :returns T.  It returns the
// body after them.
func methodOptions(body []*lisp.LVal, allowReplace bool) (replace, returns *lisp.LVal, rest []*lisp.LVal) {
	replace = lisp.Nil()
	for len(body) >= 2 {
		switch {
		case allowReplace && body[0].IsSymbol(":replace"):
			replace = body[1]
		case body[0].IsSymbol(":returns") && returns == nil:
			returns = body[1]
		default:
			return replace, returns, body
		}
		body = body[2:]
	}
	return replace, returns, body
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
//	(s:%make-record NAME (lisp:list FIELD...) (lisp:list 'm1 (lisp:lambda (recv ...) ...) ...) CHECK (lisp:lambda ())
//	                (lisp:list 'm1 (lisp:list 'arg TYPE ...) ...) (lisp:list IFACE...))
//
// The lambda (lisp:lambda ()) marks the caller's package when the type has
// no methods.  The list after it holds the argument types of each typed
// method, and the last list the interfaces of :implements.
func macroMakeRecord(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	name := args.Cells[0]
	forms := args.Cells[1:]
	var fields, methods, sigs, impls []*lisp.LVal
	check := lisp.Nil()
	for i := 0; i < len(forms); i++ {
		form := forms[i]
		switch {
		case isMethodForm(form):
			if len(form.Cells) < 3 {
				return env.ErrorConditionf(BadArgs, "s:make-record: a method needs a name and formals: %v", form)
			}
			_, returns, body := methodOptions(form.Cells[3:], false)
			fn, types := methodLambda(env, "s:make-record", methodForm{name: form.Cells[1], formals: form.Cells[2], returns: returns, body: body})
			if fn.IsError() {
				return fn
			}
			methods = append(methods, quoted(form.Cells[1]), fn)
			if types != nil {
				sigs = append(sigs, quoted(form.Cells[1]), types)
			}
		case form.IsSymbol(":check"):
			if i+1 >= len(forms) {
				return env.ErrorConditionf(BadArgs, "s:make-record: :check needs a function")
			}
			i++
			check = forms[i]
		case form.IsSymbol(":implements"):
			if i+1 >= len(forms) || forms[i+1].Type != lisp.LSExpr || forms[i+1].IsQuoted() {
				return env.ErrorConditionf(BadArgs, "s:make-record: :implements needs a list of interfaces: :implements (iface...)")
			}
			i++
			impls = append(impls, forms[i].Cells...)
		default:
			fields = append(fields, form)
		}
	}
	marker := lisp.Cells{lisp.Symbol("lisp:lambda"), lisp.Nil()}.SExpr()
	return lisp.Cells{
		lisp.Symbol(DefaultPackageName + ":%make-record"),
		name, listForm(fields), listForm(methods), check, marker, listForm(sigs), listForm(impls),
	}.SExpr()
}

// macroMethod is s:method outside s:make-record, which is always an
// error: s:make-record reads its method forms without expanding them.
func macroMethod(env *lisp.LEnv, _ *lisp.LVal) *lisp.LVal {
	return env.ErrorConditionf(BadArgs, "s:method is allowed only as a form of s:make-record; use s:defmethod to add a method to a type")
}

// macroDefMethod expands s:defmethod into
//
//	(s:%add-method TYPE 'NAME (lisp:lambda (recv ...) ...) REPLACE [(lisp:list 'arg TYPE ...)])
func macroDefMethod(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	typeV, name, formals, body := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3:]
	replace, returns, body := methodOptions(body, true)
	fn, types := methodLambda(env, "s:defmethod", methodForm{name: name, formals: formals, returns: returns, body: body})
	if fn.IsError() {
		return fn
	}
	form := lisp.Cells{
		lisp.Symbol(DefaultPackageName + ":%add-method"),
		typeV, quoted(name), fn, replace,
	}
	if types != nil {
		form = append(form, types)
	}
	return form.SExpr()
}

// builtinMakeRecord builds a record type: s:%make-record.
func builtinMakeRecord(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 5 {
		return args.ReqArg(env, len(args.Cells))
	}
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
	arities := lisp.SortedMapSized(len(methodsV.Cells) / 2)
	for i := 0; i < len(methodsV.Cells); i += 2 {
		mname, fn := methodsV.Cells[i], methodsV.Cells[i+1]
		if lerr := (methodTarget{desc: desc, pkg: pkg, methods: methods}).check(env, mname, fn); lerr != nil {
			return lerr
		}
		methods.Map().Set(lisp.String(mname.Str), fn)
		arities.Map().Set(lisp.String(mname.Str), lisp.Native(arityTag{a: lispMethodArity(fn)}))
	}
	impls := lisp.QExpr(nil)
	if len(args.Cells) > 6 {
		var lerr *lisp.LVal
		if impls, lerr = interfaceList(env, desc, args.Cells[6]); lerr != nil {
			return lerr
		}
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
		lisp.SortedMap(),
		arities,
		impls,
		lisp.Bool(len(impls.Cells) == 0),
	}.List()
	if lerr := checkImplements(env, desc, impls, lispTypeArities(tcaps), false); lerr != nil {
		return lerr
	}
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
	if len(args.Cells) > 5 {
		if lerr := addSigs(env, desc, t, tcaps, args.Cells[5]); lerr != nil {
			return lerr
		}
	}
	return t
}

// addSigs stores the argument types of each typed method of a new type.
// sigs is the list (name types name types ...) that s:make-record builds.
func addSigs(env *lisp.LEnv, desc *recordDesc, t, tcaps, sigs *lisp.LVal) *lisp.LVal {
	if len(sigs.Cells)%2 != 0 {
		return env.ErrorConditionf(BadArgs, "%s: method types must be name and type-list pairs", desc.display)
	}
	for i := 0; i < len(sigs.Cells); i += 2 {
		name := sigs.Cells[i]
		fn, ok := tcaps.Cells[typeMethods].Map().Get(lisp.String(name.Str))
		if name.Type != lisp.LSymbol || !ok {
			return env.ErrorConditionf(BadArgs, "%s: types given for %v, which is not a method", desc.display, name)
		}
		sig := lispMethodSig(env, methodWho{display: desc.display, method: name.Str, self: t}, fn, sigs.Cells[i+1])
		if sig.IsError() {
			return sig
		}
		tcaps.Cells[typeSigs].Map().Set(lisp.String(name.Str), sig) //elps:mutates the new type's own signature map, not yet returned
	}
	return nil
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
	if len(args.Cells) < 4 {
		return args.ReqArg(env, len(args.Cells))
	}
	t, name, fn, replace := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	lt, ok := lispTypeOf(t)
	desc, tcaps := lt.desc, lt.caps
	if !ok {
		if goTypeValueOf(t) != nil || goObjectType(t) != nil {
			return env.ErrorConditionf(BadArgs, "s:defmethod: a record type defined in Go is sealed")
		}
		return env.ErrorConditionf(BadArgs, "s:defmethod: first argument is not a record type: %v", t)
	}
	methods := tcaps.Cells[typeMethods]
	target := methodTarget{desc: desc, pkg: t.Package(), methods: methods, replace: lisp.True(replace)}
	if lerr := target.check(env, name, fn); lerr != nil {
		return lerr
	}
	key := lisp.String(name.Str)
	a := lispMethodArity(fn)
	only := func(n string) (arity, bool) { return a, n == name.Str }
	if lerr := checkImplements(env, desc, tcaps.Cells[typeImpls], only, false); lerr != nil {
		return lerr
	}
	sigs := tcaps.Cells[typeSigs].Map()
	if len(args.Cells) > 4 && !args.Cells[4].IsNil() {
		sig := lispMethodSig(env, methodWho{display: desc.display, method: name.Str, self: t}, fn, args.Cells[4])
		if sig.IsError() {
			return sig
		}
		sigs.Set(key, sig) //elps:mutates s:defmethod sets the method's types with the method
	} else if _, ok := sigs.Get(key); ok {
		sigs.Del(key) //elps:mutates :replace with an untyped method drops the old types
	}
	methods.Map().Set(key, fn)                                           //elps:mutates s:defmethod adds to the type's own method map
	tcaps.Cells[typeArities].Map().Set(key, lisp.Native(arityTag{a: a})) //elps:mutates the arity goes with the method
	return name
}

// interfaceList checks the interfaces of :implements and returns them as a
// list of validators.
func interfaceList(env *lisp.LEnv, desc *recordDesc, v *lisp.LVal) (*lisp.LVal, *lisp.LVal) {
	out := make([]*lisp.LVal, 0, len(v.Cells))
	for _, in := range v.Cells {
		if in.IsError() {
			return nil, in
		}
		if interfaceOf(in) == nil {
			return nil, env.ErrorConditionf(BadArgs, "%s: :implements takes interfaces (s:make-interface), got %v", desc.display, in)
		}
		out = append(out, in)
	}
	return lisp.QExpr(out), nil
}

// checkImplements checks the methods have looks up against each interface
// in impls.  A missing method is an error only when all is true.
func checkImplements(env *lisp.LEnv, desc *recordDesc, impls *lisp.LVal, have func(string) (arity, bool), all bool) *lisp.LVal {
	for _, v := range impls.Cells {
		in := interfaceOf(v)
		if err := matchMethods(in.name, in.methods, have, all); err != nil {
			return env.ErrorConditionf(WrongType, "%s: :implements %s: %v", desc.display, in.name, err)
		}
	}
	return nil
}

// checkImplementsAll checks, once per type, that the type has every
// method of each interface of :implements.  A method only matches or is
// added, so a type that passed once passes after each later s:defmethod.
func checkImplementsAll(env *lisp.LEnv, desc *recordDesc, tcaps *lisp.LVal) *lisp.LVal {
	if lisp.True(tcaps.Cells[typeImplOK]) {
		return nil
	}
	if lerr := checkImplements(env, desc, tcaps.Cells[typeImpls], lispTypeArities(tcaps), true); lerr != nil {
		return lerr
	}
	tcaps.Cells[typeImplOK] = lisp.Bool(true) //elps:mutates the type's own captures record the passed check
	return nil
}

// macroMakeInterface expands s:make-interface into
//
//	(s:%make-interface NAME '((m1 (formals...)) ...) (lisp:list EMBED...))
//
// The method specs are data: builtinMakeInterface checks them.  The
// interfaces of :embed are evaluated.
func macroMakeInterface(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	var specs, embed []*lisp.LVal
	forms := args.Cells[1:]
	for i := 0; i < len(forms); i++ {
		if !forms[i].IsSymbol(":embed") {
			specs = append(specs, forms[i])
			continue
		}
		if i+1 >= len(forms) || forms[i+1].Type != lisp.LSExpr || forms[i+1].IsQuoted() {
			return env.ErrorConditionf(BadArgs, "s:make-interface: :embed needs a list of interfaces: :embed (iface...)")
		}
		i++
		embed = append(embed, forms[i].Cells...)
	}
	return lisp.Cells{
		lisp.Symbol(DefaultPackageName + ":%make-interface"),
		args.Cells[0],
		quoted(lisp.SExpr(specs)),
		listForm(embed),
	}.SExpr()
}

// builtinMakeInterface builds an interface: s:%make-interface.
func builtinMakeInterface(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	if len(args.Cells) < 2 {
		return args.ReqArg(env, len(args.Cells))
	}
	nameV, specs := args.Cells[0], args.Cells[1]
	name, ok := lisp.GoString(nameV)
	if !ok || name == "" {
		return env.ErrorConditionf(BadArgs, "s:make-interface: the name must be a non-empty string: %v", nameV)
	}
	spec, bad := interfaceSpecOf(name, specs)
	if bad != nil {
		return env.ErrorConditionf(BadArgs, "s:make-interface: %s: a method is (name (args...)): %v", name, bad)
	}
	if len(args.Cells) > 2 {
		for _, v := range args.Cells[2].Cells {
			if v.IsError() {
				return v
			}
			e := interfaceOf(v)
			if e == nil {
				return env.ErrorConditionf(BadArgs, "s:make-interface: %s: :embed takes interfaces (s:make-interface), got %v", name, v)
			}
			spec.Embed = append(spec.Embed, e)
		}
	}
	in, err := NewInterface(spec)
	if err != nil {
		return env.ErrorConditionf(BadArgs, "s:make-interface: %v", err)
	}
	return in.Validator(env)
}

// interfaceSpecOf reads the method specs of s:make-interface.  It
// returns the first spec it cannot read as bad.
func interfaceSpecOf(name string, specs *lisp.LVal) (InterfaceSpec, *lisp.LVal) {
	spec := InterfaceSpec{Name: name}
	for _, m := range specs.Cells {
		im, ok := interfaceMethodOf(m)
		if !ok {
			return spec, m
		}
		spec.Methods = append(spec.Methods, im)
	}
	return spec, nil
}

// interfaceMethodOf reads one method spec of s:make-interface, (name
// (args...)).  NewInterface checks the formals.
func interfaceMethodOf(m *lisp.LVal) (InterfaceMethod, bool) {
	if m.Type != lisp.LSExpr || len(m.Cells) != 2 || m.Cells[0].Type != lisp.LSymbol {
		return InterfaceMethod{}, false
	}
	return InterfaceMethod{Name: m.Cells[0].Str, Formals: m.Cells[1]}, true
}
