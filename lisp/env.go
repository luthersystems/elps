// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"strings"

	"github.com/luthersystems/elps/parser/token"
)

// DefaultLangPackage is the name of default language package
const DefaultLangPackage = "lisp"

// DefaultUserPackage is the name of the entry point package for interpreting
// user code.
const DefaultUserPackage = "user"

// InitializeUserEnv creates the default user environment.
func InitializeUserEnv(env *LEnv, config ...Config) *LVal {
	env.Runtime.Registry.DefinePackage(DefaultLangPackage)
	env.Runtime.Registry.Lang = DefaultLangPackage
	env.Runtime.Package = env.Runtime.Registry.Packages[env.Runtime.Registry.Lang]
	env.Runtime.Package.Doc = `The core ELPS language package. Provides fundamental data types,
		control flow, function and macro definition, package management,
		error handling, collections, I/O, and the type system.`
	env.AddMacros(true)
	env.AddSpecialOps(true)
	env.AddBuiltins(true)
	rc := InitializeTypedef(env)
	if GoError(rc) != nil {
		return rc
	}
	env.Runtime.Registry.DefinePackage(DefaultUserPackage)
	env.Runtime.Registry.Packages[DefaultUserPackage].Doc = "The default user package for application code."
	rc = env.InPackage(Symbol(DefaultUserPackage))
	if GoError(rc) != nil {
		return rc
	}
	for _, fn := range config {
		lerr := fn(env)
		if lerr.Type == LError {
			return lerr
		}
	}
	return env.UsePackage(Symbol(env.Runtime.Registry.Lang))
}

// InitializeTypedef injects the meta-typedef object so `new` and `deftype` can
// be used to create user-defined types.  The name of the injected typedef
// object is not exported and it should not be handled without abstraction in
// general because user error can break the type system.
//
// See LEnv.TaggedValue for more information about creating tagged-values.
func InitializeTypedef(env *LEnv) *LVal {
	ctor := env.builtin(&langBuiltin{"typedef-ctor", Formals("name", "ctor"), func(env *LEnv, args *LVal) *LVal {
		sym := args.Cells[0]
		ctor := args.Cells[1]
		if sym.Type != LSymbol {
			return env.Errorf("first argument is not a symbol: %v", GetType(sym))
		}
		if ctor.Type != LFun {
			return env.Errorf("second argument is not a function: %v", GetType(ctor))
		}
		if ctor.IsSpecialFun() {
			return env.Errorf("second argument is not a regular function")
		}
		return QExpr([]*LVal{sym, ctor})
	}, ""})
	if ctor.Type == LError {
		return ctor
	}
	// Create a typedef for the typedef type that will use ctor to create new
	// typedefs. Pretty simple, really. *brain explosion*
	pkg := env.Runtime.Registry.Lang
	fqname := Symbol(fmt.Sprintf("%s:typedef", pkg))
	typedef := env.TaggedValue(fqname, QExpr([]*LVal{fqname, ctor}))
	if typedef.Type == LError {
		return typedef
	}
	env.Runtime.Registry.Packages[pkg].Put(Symbol("typedef"), typedef)
	return Nil()
}

// TODO(elps2): Remove the field LEnv.FunName

// LEnv is a lisp environment.
type LEnv struct {
	Loc     *token.Location
	Scope   map[string]*LVal
	FunName map[string]string
	Parent  *LEnv
	Runtime *Runtime
	ID      uint
}

// NewEnvRuntime initializes a new LEnv, like NewEnv, but it explicitly
// specifies the runtime to use.  NewEnvRuntime is only suitable for creating
// root LEnv object, so it does not take a parent argument.  When rt is nil
// StandardRuntime() called to create a new Runtime for the returned LEnv.  It
// is an error to use the same runtime object in multiple calls to
// NewEnvRuntime if the two envs are not in the same tree and doing so will
// have unspecified results.
func NewEnvRuntime(rt *Runtime) *LEnv {
	if rt == nil {
		rt = StandardRuntime()
	}
	env := &LEnv{
		ID:      rt.GenEnvID(),
		Scope:   make(map[string]*LVal),
		FunName: make(map[string]string),
		Runtime: rt,
	}
	return env
}

// NewEnv returns initializes and returns a new LEnv.
func NewEnv(parent *LEnv) *LEnv {
	return newEnvN(parent, 0)
}

// newEnvN creates a child LEnv with its Scope map pre-sized to hold n
// bindings.  Callers that know the number of bindings up front (let, let*,
// dotimes, etc.) can avoid map growth by passing the exact count.
func newEnvN(parent *LEnv, n int) *LEnv {
	var runtime *Runtime
	var loc *token.Location
	if parent != nil {
		runtime = parent.Runtime
		loc = parent.Loc
	} else {
		runtime = StandardRuntime()
		loc = nativeSource()
	}
	env := &LEnv{
		ID:      runtime.GenEnvID(),
		Loc:     loc,
		Scope:   make(map[string]*LVal, n),
		FunName: make(map[string]string),
		Parent:  parent,
		Runtime: runtime,
	}
	return env
}

func (env *LEnv) getFID() string {
	return fmt.Sprintf("_fun%d", env.ID)
}

func (env *LEnv) GenSym() *LVal {
	return Symbol(env.Runtime.GenSym())
}

func (env *LEnv) DefinePackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	env.Runtime.Registry.DefinePackage(name.Str)
	return Nil()
}

// SetPackageDoc sets the documentation string for the current package.
func (env *LEnv) SetPackageDoc(doc string) {
	env.Runtime.Package.Doc = doc
}

// SetSymbolDoc sets the documentation string for a symbol in the current package.
func (env *LEnv) SetSymbolDoc(name, doc string) {
	env.Runtime.Package.SymbolDocs[name] = doc
}

func (env *LEnv) InPackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	pkg := env.Runtime.Registry.Packages[name.Str]
	if pkg == nil {
		return env.Errorf("unknown package: %v", name.Str)
	}
	env.Runtime.Package = pkg
	return Nil()
}

func (env *LEnv) UsePackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	pkg := env.Runtime.Registry.Packages[name.Str]
	if pkg == nil {
		return env.Errorf("unknown package: %v", name.Str)
	}
	for _, sym := range pkg.Externals {
		v := pkg.Get(Symbol(sym))
		if v.Type == LError {
			return env.Errorf("package %s: %v", name.Str, v)
		}
		env.Runtime.Package.Put(Symbol(sym), v)
	}
	return Nil()
}

func (env *LEnv) LoadString(name, exprs string) *LVal {
	return env.Load(name, strings.NewReader(exprs))
}

// LoadFile attempts to use env.Runtime.Library to read a lisp source file and
// evaluate expressions it contains.  Any error encountered will prevent
// execution of loaded source and be returned.  After evaluating expressions
// the current package is restored to the current package at the time Load was
// called, in case loaded source made calls to “in-package”.  If
// env.Runtime.Reader has not been set then an error will be returned by Load.
func (env *LEnv) LoadFile(loc string) *LVal {
	if env.Runtime.Library == nil {
		return env.Errorf("no source library in environment runtime")
	}
	ctx := env.Runtime.sourceContext()
	name, loc, src, err := env.Runtime.Library.LoadSource(ctx, loc)
	if err != nil {
		return env.Errorf("library error: %v", err)
	}
	return env.LoadLocation(name, loc, bytes.NewReader(src))
}

// Load reads LVals from r and evaluates them as if in a progn.  The value
// returned by the last evaluated LVal will be retured.  After evaluating
// expressions the current package is restored to the current package at the
// time Load was called, in case loaded source made calls to “in-package”.
// If env.Runtime.Reader has not been set then an error will be returned by Load.
func (env *LEnv) Load(name string, r io.Reader) *LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for environment runtime")
	}

	exprs, err := env.Runtime.Reader.Read(name, r)
	if err != nil {
		return env.Error(err)
	}

	return env.load(exprs)
}

// LoadLocation attempts to use env.Runtime.Library to read a lisp source file,
// specifying its name and location explicity, and evaluate the expressions it
// contains.  Because the name and location of the stream are specfied
// explicitly LoadLocation does not depend explicity on an env.Runtime.Library
// implementation.
// Any error encountered will prevent execution of loaded source and
// be returned.  After evaluating expressions the current package is restored
// to the current package at the time Load was called, in case loaded source
// made calls to “in-package”.  If env.Runtime.Reader has not been set then
// an error will be returned by Load.
func (env *LEnv) LoadLocation(name string, loc string, r io.Reader) *LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for environment runtime")
	}

	reader, ok := env.Runtime.Reader.(LocationReader)
	if !ok {
		return env.Load(name, r)
	}
	exprs, err := reader.ReadLocation(name, loc, r)
	if err != nil {
		return env.Error(err)
	}

	return env.load(exprs)
}

func (env *LEnv) load(exprs []*LVal) *LVal {
	if len(exprs) == 0 {
		return Nil()
	}

	// Remember the current package and restore it for the caller after
	// evaluation completes.
	currPkg := env.Runtime.Package
	defer func() {
		// This should be fine as packages can't be deleted.  The runtime
		// registry should definitely still contain currPkg.
		env.Runtime.Package = currPkg
	}()

	ret := Nil()
	for _, expr := range exprs {
		ret = env.Eval(expr)
		if ret.Type == LError {
			return ret
		}
	}
	return ret
}

// Copy returns a new LEnv with a copy of env.Scope but a shared parent and
// stack (not quite a deep copy).
func (env *LEnv) Copy() *LEnv {
	if env == nil {
		return nil
	}
	cp := &LEnv{}
	*cp = *env
	cp.Scope = make(map[string]*LVal, len(env.Scope))
	for k, v := range env.Scope {
		cp.Scope[k] = v
	}
	return cp
}

// Get takes an LSymbol k and returns the LVal it is bound to in env.
func (env *LEnv) Get(k *LVal) *LVal {
	v := env.get(k)
	if v.Type == LFun {
		// Set the function's name here in case the same function is
		// defined with multiple names.  We want to try and use the name
		// the programmer used.
		v = FunRef(k, v)
	}
	return v
}

// GetFun returns a function referenced by the given LVal.  If fun is already
// an LFun, then fun is returned.  If fun is a symbol then GetFun looks for a
// function bound to the symbol.  If fun does not reference a symbol then an
// error is returned.
//
// GetFun is a suitable for backing an implementation of functional programing
// constructs, like funcall, map, reduce, etc.
func (env *LEnv) GetFun(fun *LVal) *LVal {
	if fun.Type == LSymbol {
		f := env.Get(fun)
		if f.Type == LError {
			return f
		}
		if f.Type != LFun {
			return env.Errorf("symbol %s not bound to a function: %v", fun, f.Type)
		}
		return f
	} else if fun.Type != LFun {
		return env.Errorf("first argument is not a function: %v", fun.Type)
	}
	return fun
}

// GetFunGlobal is like GetFun but only accesses the global package environment.
func (env *LEnv) GetFunGlobal(fun *LVal) *LVal {
	if fun.Type == LSymbol {
		f := env.GetGlobal(fun)
		if f.Type == LError {
			return f
		}
		if f.Type != LFun {
			return env.Errorf("symbol %s not bound to a function: %v", fun, f.Type)
		}
		return f
	} else if fun.Type != LFun {
		return env.Errorf("first argument is not a function: %v", fun.Type)
	}
	return fun
}

func (env *LEnv) get(k *LVal) *LVal {
	// LQSymbols are allowed...
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Nil()
	}
	// Return pre-allocated singletons for true/false instead of
	// allocating a fresh Symbol on every boolean lookup.
	if k.Str == TrueSymbol {
		return singletonTrue
	}
	if k.Str == FalseSymbol {
		return singletonFalse
	}
	colonIdx := strings.IndexByte(k.Str, ':')
	if colonIdx < 0 {
		// No colon — simple unqualified symbol (common case, zero allocs).
		return env.getSimple(k)
	}
	if colonIdx == 0 {
		// keyword like :foo
		return k
	}
	ns := k.Str[:colonIdx]
	name := k.Str[colonIdx+1:]
	if strings.IndexByte(name, ':') >= 0 {
		lerr := Errorf("illegal symbol: %q", k.Str)
		env.ErrorAssociate(lerr)
		return lerr
	}
	pkg := env.Runtime.Registry.Packages[ns]
	if pkg == nil {
		return env.Errorf("unknown package: %q", ns)
	}
	lerr := pkg.Get(Symbol(name))
	if lerr.Type == LError {
		env.ErrorAssociate(lerr)
	}
	return lerr
}

func (env *LEnv) getSimple(k *LVal) *LVal {
	for {
		v, ok := env.Scope[k.Str]
		if ok {
			return v
		}
		if env.Parent != nil {
			env = env.Parent
			continue
		}
		return env.packageGet(k)
	}
}

func (env *LEnv) packageGet(k *LVal) *LVal {
	lerr := env.Runtime.Package.Get(k)
	if lerr.Type == LError {
		env.ErrorAssociate(lerr)
	}
	return lerr
}

// GetFunName returns the function name (if any) known to be bound to the given
// function. If the function's FID is bound in its package then the global name
// of the function is returned.  When the function is bound within a local
// scope then the local name used to reference the function (if any) is
// returned.
func (env *LEnv) GetFunName(f *LVal) string {
	name := env.pkgFunName(f)
	if name != "" {
		return name
	}
	return f.Str
}

func (env *LEnv) pkgFunName(f *LVal) string {
	if f.Type != LFun {
		panic("not a function: " + f.Type.String())
	}
	pkgname := f.Package()
	if pkgname == "" {
		log.Printf("unknown package for function %s", f.FID())
		return ""
	}
	pkg := env.Runtime.Registry.Packages[pkgname]
	if pkg == nil {
		log.Printf("failed to find package %q", pkgname)
		return ""
	}
	return pkg.FunNames[f.FID()]
}

// Put takes an LSymbol k and binds it to v in env.  If k is already bound to a
// value the binding is updated so that k is bound to v.
func (env *LEnv) Put(k, v *LVal) *LVal {
	if k.Type != LSymbol && k.Type != LQSymbol {
		return env.Errorf("key is not a symbol: %v", k.Type)
	}
	if k.Str == TrueSymbol || k.Str == FalseSymbol {
		return env.Errorf("cannot rebind constant: %v", k.Str)
	}
	env.Scope[k.Str] = v
	return Nil()
}

// Update updates the binding of k to v within the scope of env.  Update can
// update either lexical or global bindings.  If k is not bound by env, an
// enclosing LEnv, or the current package an error condition is signaled.
func (env *LEnv) Update(k, v *LVal) *LVal {
	if k.Type != LSymbol && k.Type != LQSymbol {
		return env.Errorf("key is not a symbol: %v", k.Type)
	}
	if k.Str == TrueSymbol || k.Str == FalseSymbol {
		return env.Errorf("cannot rebind constant: %v", k.Str)
	}
	return env.update(k, v)
}

func (env *LEnv) update(k, v *LVal) *LVal {
	for {
		_, ok := env.Scope[k.Str]
		if ok {
			env.Scope[k.Str] = v
			return Nil()
		}
		if env.Parent == nil {
			lerr := env.Runtime.Package.Update(k, v)
			if lerr.Type == LError {
				env.ErrorAssociate(lerr)
				return lerr
			}
			return Nil()
		}
		env = env.Parent
	}
}

// GetGlobal takes LSymbol k and returns the value it is bound to in the
// current package.
func (env *LEnv) GetGlobal(k *LVal) *LVal {
	pieces := SplitSymbol(k)
	if pieces.Type == LError {
		env.ErrorAssociate(pieces)
		return pieces
	}
	if pieces.Len() == 2 {
		ns := pieces.Cells[0].Str
		if ns == "" {
			// keyword
			return k
		}
		pkg := env.Runtime.Registry.Packages[ns]
		if pkg == nil {
			return env.Errorf("unknown package: %q", ns)
		}
		lerr := pkg.Get(pieces.Cells[1])
		if lerr.Type == LError {
			env.ErrorAssociate(lerr)
		}
		return lerr
	}
	return env.packageGet(k)
}

// PutGlobal takes an LSymbol k and binds it to v in current package.
func (env *LEnv) PutGlobal(k, v *LVal) *LVal {
	pieces := SplitSymbol(k)
	if pieces.Type == LError {
		env.ErrorAssociate(pieces)
		return pieces
	}
	if pieces.Len() == 2 {
		ns := pieces.Cells[0].Str
		if ns == "" {
			return env.Errorf("value cannot be assigned to a keyword: %s", k.Str)
		}
		pkg := env.Runtime.Registry.Packages[ns]
		if pkg == nil {
			return env.Errorf("unknown package: %q", ns)
		}
		lerr := pkg.Put(pieces.Cells[1], v)
		if lerr.Type == LError {
			env.ErrorAssociate(lerr)
		}
		return lerr
	}

	lerr := env.Runtime.Package.Put(k, v)
	if lerr.Type == LError {
		env.ErrorAssociate(lerr)
		return lerr
	}
	return Nil()
}

// TaggedValue is a low-level function to create a tagged-value and should be
// used with great care and testing.  The first argument must be a symbol and
// is used as the type of the returned tagged-value.  The second argument is
// the value being tagged.
//
// The type of a tagged-value should be a qualified symbol (e.g. 'lisp:mytype).
// Unqualified type names can clash with primitive type symbols (e.g. 'string)
// which can lead to program failures.
func (env *LEnv) TaggedValue(typ *LVal, val *LVal) *LVal {
	if typ.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", GetType(typ))
	}
	return &LVal{
		Source: env.Loc,
		Type:   LTaggedVal,
		Str:    typ.Str,
		Cells:  []*LVal{val},
	}
}

// New takes a typedef along with a list of constructor arguments and returns
// an LTaggedValue containing the result of invoking the typedef's constructor
// with the given arguments.  A typedef is an LTaggedVal itself that wraps a
// list holding the defined type name along with a constructor.
//
// New requires that the system have typedef tagged-values.  Generally that
// will be enabled by calling InitializeTypedef or InitializeUserEnv when
// initializing the top-level enviornment.
func (env *LEnv) New(typ *LVal, args *LVal) *LVal {
	if typ.Type != LTaggedVal {
		return env.Errorf("first argument is not a typedef: %v", GetType(typ))
	}
	if typ.Str != fmt.Sprintf("%s:typedef", env.Runtime.Registry.Lang) {
		return env.Errorf("first argument is not a typedef: %v", GetType(typ))
	}
	if args.Type != LSExpr {
		return env.Errorf("second argument is not a list: %v", GetType(args))
	}
	tname := typ.Cells[0].Cells[0]
	ctor := typ.Cells[0].Cells[1]
	v := env.FunCall(ctor, args)
	if v.Type == LError {
		return v
	}
	return env.TaggedValue(tname, v)
}

// Lambda returns a new Lambda with fun.Env and fun.Package set automatically.
func (env *LEnv) Lambda(formals *LVal, body []*LVal) *LVal {
	if formals.Type != LSExpr {
		return env.Errorf("formals is not a list of symbols: %v", formals.Type)
	}
	cells := make([]*LVal, 0, len(body)+1)
	cells = append(cells, formals)
	cells = append(cells, body...)
	fenv := NewEnv(env)
	fun := &LVal{
		Type:   LFun,
		Source: env.Loc,
		Native: &LFunData{
			FID:     fenv.getFID(),
			Package: env.Runtime.Package.Name,
			Env:     fenv,
		},
		Cells: cells,
	}
	return fun
}

// BUG:  Because Go-defined functions don't have a lexical enivroment this
// method doesn't produce the expected behavior and can't be exported publicly
// because of that.
func (env *LEnv) builtin(f LBuiltinDef) *LVal {
	v := Fun(NewEnv(env).getFID(), f.Formals(), f.Eval)
	v.FunData().Package = env.Runtime.Package.Name
	return v
}

func (env *LEnv) Terminal(expr *LVal) *LVal {
	return &LVal{
		Type:   LMarkTerminal,
		Native: env,
		Cells:  []*LVal{expr},
	}
}

func (env *LEnv) root() *LEnv {
	for env.Parent != nil {
		env = env.Parent
	}
	return env
}

// AddMacros binds the given macros to their names in env.  When called with no
// arguments AddMacros adds the DefaultMacros to env.
func (env *LEnv) AddMacros(external bool, macs ...LBuiltinDef) {
	if len(macs) == 0 {
		macs = DefaultMacros()
	}
	pkg := env.Runtime.Package
	for _, mac := range macs {
		k := Symbol(mac.Name())
		exist := pkg.Get(k)
		if !exist.IsNil() && exist.Type != LError { // LError is ubound symbol
			panic(fmt.Sprintf("macro already defined: %v (= %v)", k, exist))
		}
		id := fmt.Sprintf("<builtin-macro ``%s''>", mac.Name())
		fn := Macro(id, mac.Formals(), mac.Eval)
		fn.Cells[1] = String(builtinDocstring(mac))
		fn.FunData().Package = pkg.Name
		pkg.Put(k, fn)
		if external {
			pkg.Externals = append(pkg.Externals, k.Str)
		}
	}
}

// AddSpecialOps binds the given special operators to their names in env.  When
// called with no arguments AddSpecialOps adds the DefaultSpecialOps to env.
func (env *LEnv) AddSpecialOps(external bool, ops ...LBuiltinDef) {
	if len(ops) == 0 {
		ops = DefaultSpecialOps()
	}
	pkg := env.Runtime.Package
	for _, op := range ops {
		k := Symbol(op.Name())
		exist := pkg.Get(k)
		if !exist.IsNil() && exist.Type != LError { // LError is ubound symbol
			panic(fmt.Sprintf("macro already defined: %v (= %v)", k, exist))
		}
		id := fmt.Sprintf("<special-op ``%s''>", op.Name())
		fn := SpecialOp(id, op.Formals(), op.Eval)
		fn.Cells[1] = String(builtinDocstring(op))
		fn.FunData().Package = pkg.Name
		pkg.Put(k, fn)
		if external {
			pkg.Externals = append(pkg.Externals, k.Str)
		}
	}
}

// AddBuiltins binds the given funs to their names in env.  When called with no
// arguments AddBuiltins adds the DefaultBuiltins to env.
func (env *LEnv) AddBuiltins(external bool, funs ...LBuiltinDef) {
	if len(funs) == 0 {
		funs = DefaultBuiltins()
	}
	pkg := env.Runtime.Package
	for _, f := range funs {
		k := Symbol(f.Name())
		exist := pkg.Get(k)
		if exist.Type != LError {
			panic("symbol already defined: " + f.Name())
		}
		id := fmt.Sprintf("<builtin-function ``%s''>", f.Name())
		v := Fun(id, f.Formals(), f.Eval)
		v.Cells[1] = String(builtinDocstring(f))
		v.FunData().Package = pkg.Name
		pkg.Put(k, v)
		if external {
			pkg.Externals = append(pkg.Externals, k.Str)
		}
	}
}

// Error returns an LError value with an error message given by rendering msg.
//
// Error may be called either with an error or with any number of *LVal values.
// It is invalid to pass an error argument with any other values and doing so
// will result in a runtime panic.
//
// Unlike the exported function, the Error method returns LVal with a copy
// env.Runtime.Stack.
func (env *LEnv) Error(msg ...interface{}) *LVal {
	return env.ErrorCondition("error", msg...)
}

// ErrorCondition returns an LError the given condition type and an error
// message computed by rendering msg.
//
// ErrorCondition may be called either with an error or with any number of
// *LVal values.  It is invalid to pass ErrorCondition an error argument with
// any other values and doing so will result in a runtime panic.
//
// Unlike the exported function, the ErrorCondition method returns an LVal with
// a copy env.Runtime.Stack.
func (env *LEnv) ErrorCondition(condition string, v ...interface{}) *LVal {
	// log.Printf("stack %v", env.Runtime.Stack.Copy())

	narg := len(v)
	cells := make([]*LVal, 0, len(v))
	for _, v := range v {
		switch v := v.(type) {
		case *LVal:
			cells = append(cells, v)
		case error:
			if narg > 1 {
				panic("invalid error argument")
			}
			return &LVal{
				Type:   LError,
				Str:    condition,
				Native: env.Runtime.Stack.Copy(),
				Cells:  []*LVal{Native(v)},
			}
		case string:
			cells = append(cells, String(v))
		default:
			cells = append(cells, Native(v))
		}
	}
	return &LVal{
		Type:   LError,
		Source: env.Loc,
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  cells,
	}
}

// Errorf returns an LError value with a formatted error message.
//
// Unlike the exported function, the Errorf method returns an LVal with a copy
// env.Runtime.Stack.
func (env *LEnv) Errorf(format string, v ...interface{}) *LVal {
	return env.ErrorConditionf("error", format, v...)
}

// ErrorConditionf returns an LError value with the given condition type and a
// a formatted error message rendered using fmt.Sprintf.
//
// Unlike the exported function, the ErrorConditionf method returns an LVal
// with a copy env.Runtime.Stack.
func (env *LEnv) ErrorConditionf(condition string, format string, v ...interface{}) *LVal {
	return &LVal{
		Source: env.Loc,
		Type:   LError,
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  []*LVal{String(fmt.Sprintf(format, v...))},
	}
}

// ErrorAssociate associates the LError value lerr with env's current call
// stack and source location.  ErrorAssociate panics if lerr is not LError.
func (env *LEnv) ErrorAssociate(lerr *LVal) {
	if lerr.Type != LError {
		panic("not an error: " + lerr.Type.String())
	}
	if lerr.CallStack() == nil {
		lerr.SetCallStack(env.Runtime.Stack.Copy())
	}
	// This check smells a little funny.  All objects are given a source
	// which may be a nativeSource() value which does not correspond to a
	// file and has an invalid position (-1).  When associating an error
	// the env's current location is probably more accurate than native
	// source (or it may also be native source).
	if lerr.Source == nil || lerr.Source.Pos < 0 {
		lerr.Source = env.Loc
	}
}

// Eval evaluates v in the context (scope) of env and returns the resulting
// LVal.  Eval does not modify v.
//
// NOTE:  Eval shouldn't unquote v during evaluation -- a difference between
// Eval and the “eval” builtin function, but it does.  For some reason macros
// won't work without this unquoting.
func (env *LEnv) Eval(v *LVal) *LVal {
eval:
	if v.Spliced {
		return env.Errorf("spliced value used as expression")
	}
	env.Loc = v.Source
	if v.Quoted {
		return v
	}
	switch v.Type {
	case LSymbol:
		colonIdx := strings.IndexByte(v.Str, ':')
		if colonIdx < 0 {
			// No colon — simple unqualified symbol (the common case).
			return env.Get(v)
		}
		if colonIdx == 0 {
			// Keyword like :foo
			return v
		}
		// Qualified symbol like pkg:name — check for extra colons.
		ns := v.Str[:colonIdx]
		name := v.Str[colonIdx+1:]
		if strings.IndexByte(name, ':') >= 0 {
			return env.Errorf("illegal symbol: %q", v.Str)
		}
		pkg := env.Runtime.Registry.Packages[ns]
		if pkg == nil {
			return env.Errorf("unknown package: %q", ns)
		}
		lerr := pkg.Get(Symbol(name))
		if lerr.Type == LError {
			env.ErrorAssociate(lerr)
		}
		return lerr
	case LSExpr:
		res := env.EvalSExpr(v)
		if res.Type == LMarkMacExpand {
			// A macro was just expanded and returned an unevaluated
			// expression.  We have to evaluate the result before we return.
			v = res.Cells[0]
			goto eval
		}
		if res.Type == LError {
			env.ErrorAssociate(res)
		}
		return res
	case LQuote:
		// this quote was unquoted... eval the underlying value
		v = v.Cells[0]
		goto eval
	default:
		return v
	}
}

// EvalSExpr evaluates s and returns the resulting LVal.
func (env *LEnv) EvalSExpr(s *LVal) *LVal {
	if s.Type != LSExpr {
		return env.Errorf("not an s-expression")
	}
	if len(s.Cells) == 0 {
		return Nil()
	}
	call := env.evalSExprCells(s)
	if call.Type == LError {
		env.ErrorAssociate(call)
		return call
	}
	fun := call.Cells[0] // call is not an empty expression -- fun is known LFun
	args := call
	args.Cells = args.Cells[1:]

	switch fun.FunType {
	case LFunNone:
		return env.FunCall(fun, args)
	case LFunSpecialOp:
		return env.SpecialOpCall(fun, args)
	case LFunMacro:
		return env.MacroCall(fun, args)
	default:
		panic(fmt.Sprintf("invalid function type %#v", fun.FunType))
	}
}

// MacroCall invokes macro fun with argument list args.
func (env *LEnv) MacroCall(fun, args *LVal) *LVal {
	if fun.Type != LFun {
		return env.Errorf("not a special function: %v", fun.Type)
	}
	if !fun.IsMacro() {
		return env.Errorf("not a special function: %v", fun.FunType)
	}

	// Capture the call-site location before entering the macro body so we
	// can stamp it onto expanded nodes that lack real source info.
	callSite := env.Loc

	// Push a frame onto the stack to represent the function's execution.
	err := env.Runtime.Stack.PushFID(env.Loc, fun.FID(), fun.Package(), env.GetFunName(fun))
	if err != nil {
		return env.Error(err)
	}
	defer env.Runtime.Stack.Pop()
	// Macros can't participate in tail-recursion optimization at all.  Enable
	// the TROBlock on the stack fram so TerminalFID never seeks past the
	// macro's callsite.
	env.Runtime.Stack.Top().TROBlock = true

	r := env.call(fun, args)
	if r == nil {
		_, _ = env.Runtime.Stack.DebugPrint(env.Runtime.getStderr())
		panic("nil LVal returned from function call")
	}
	if r.Type == LError {
		return r
	}

	// NOTE:  There should be no need to check for LMarkTailRec objects because
	// we block all tail-recursion for macro calls.

	// Stamp expanded nodes that have synthetic source locations (Pos < 0)
	// with the call-site location so errors point to where the macro was
	// invoked rather than "<native code>" or the macro definition site.
	stampMacroExpansion(r, callSite)

	// This is a lazy unquote.  Unquoting in this way appears to allow the
	// upcoming evaluation to produce the correct value for user defined
	// macros, which are typically using quasiquote.  Builtin macros can be
	// massaged to return a proper value.  I'm sure there is a bug where
	// something is unintentionally unquoted.  I will deal with
	// implementing a proper system for special operators at that point.
	r = shallowUnquote(r)
	return markMacExpand(r)
}

// SpecialOpCall invokes special operator fun with the argument list args.
func (env *LEnv) SpecialOpCall(fun, args *LVal) *LVal {
	if fun.Type != LFun {
		return env.Errorf("not a special function: %v", fun.Type)
	}
	if !fun.IsSpecialOp() {
		return env.Errorf("not a special function: %v", fun.FunType)
	}

	// Push a frame onto the stack to represent the function's execution.
	err := env.Runtime.Stack.PushFID(env.Loc, fun.FID(), fun.Package(), env.GetFunName(fun))
	if err != nil {
		return env.Error(err)
	}
	defer env.Runtime.Stack.Pop()

	// Special functions in general cannot be candidates for tail-recursion
	// optimization because they receive unevaluated arguments.  As such,
	// unwinding the stack would put them at risk of losing bindings to a
	// symbol which still has yet to be evaluated (after normal functions would
	// have all of their argument evaled already).  Furthermore, special
	// operators like ``let'' define a lexical scope which cannot be collapsed
	// by tail-recursion-optimization.

callf:
	r := env.call(fun, args)
	if r == nil {
		_, _ = env.Runtime.Stack.DebugPrint(env.Runtime.getStderr())
		panic("nil LVal returned from function call")
	}
	if r.Type == LError {
		return r
	}

	if r.Type == LMarkTailRec {
		// Tail recursion optimization is occurring.
		if decrementMarkTailRec(r) {
			env.Runtime.Stack.Top().HeightLogical += r.tailRecElided()
			err := env.Runtime.Stack.CheckHeight()
			if err != nil {
				return env.Error(err)
			}
			fun, args = extractMarkTailRec(r)
			goto callf
		}
		return r
	}

	return r
}

func (env *LEnv) FunCall(fun, args *LVal) *LVal {
	return env.funCall(fun, args)
}

func (env *LEnv) trace(fun *LVal) func() {
	if env.Runtime.Profiler == nil {
		return func() {}
	}
	// fun might be an anon function, so we need to convert it to get the
	// right type of LVal for filtering and labeling
	return env.Runtime.Profiler.Start(fun)
}

// FunCall invokes regular function fun with the argument list args.
func (env *LEnv) funCall(fun, args *LVal) *LVal {
	if fun.Type != LFun {
		return env.Errorf("not a function: %v", fun.Type)
	}
	if fun.IsSpecialFun() {
		return env.Errorf("not a regular function: %v", fun.FunType)
	}

	if env.Runtime.Profiler != nil {
		defer env.trace(fun)()
	}

	// Check for possible tail recursion before pushing to avoid hitting s when
	// checking.  But push FID onto the stack before popping to simplify
	// book-keeping.
	npop := env.Runtime.Stack.TerminalFID(fun.FID())

	// Push a frame onto the stack to represent the function's execution.
	err := env.Runtime.Stack.PushFID(env.Loc, fun.FID(), fun.Package(), env.GetFunName(fun))
	if err != nil {
		return env.Error(err)
	}
	defer env.Runtime.Stack.Pop()

	if npop > 0 {
		return markTailRec(npop, fun, args)
	}

callf:
	r := env.call(fun, args)
	if r == nil {
		_, _ = env.Runtime.Stack.DebugPrint(env.Runtime.getStderr())
		panic("nil LVal returned from function call")
	}
	if r.Type == LError {
		return r
	}

	if r.Type == LMarkTailRec {
		// Tail recursion optimization is occurring.
		done := decrementMarkTailRec(r)
		if done {
			env.Runtime.Stack.Top().HeightLogical += r.tailRecElided()
			err := env.Runtime.Stack.CheckHeight()
			if err != nil {
				return env.Error(err)
			}
			fun, args = extractMarkTailRec(r)
			goto callf
		}
	}

	return r
}

func extractMarkTailRec(mark *LVal) (fun, args *LVal) {
	return mark.tailRecFun(), mark.tailRecArgs()
}

// Decrement the tail recursion counter until it indicates 0 additional
// stack frames should be popped.  When that happens we can jump into the
// next call.
//
// mark must be LMarkTailRec
func decrementMarkTailRec(mark *LVal) (done bool) {
	if len(mark.Cells) != 4 {
		panic("invalid mark")
	}
	mark.Cells[0].Int--
	return mark.Cells[0].Int <= 0
}

func (env *LEnv) evalSExprCells(s *LVal) *LVal {
	loc := env.Loc
	defer func() { env.Loc = loc }()

	cells := s.Cells
	newCells := make([]*LVal, 1, len(s.Cells))
	if env.Runtime.Stack.Top() != nil {
		// Avoid tail recursion during argument evaluation by temporarily
		// resetting Terminal.  We don't want to push anything on the stack
		// here because that would causes improper error messages/stack-dumps
		// if an error is encountered while evaluating the arguments to a
		// function.
		if env.Runtime.Stack.Top().Terminal {
			env.Runtime.Stack.Top().Terminal = false
			defer func() { env.Runtime.Stack.Top().Terminal = true }()
		}
	}
	f := env.Eval(cells[0])
	cells = cells[1:]
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first element of expression is not a function: %v", f)
	}
	if f.Type == LMarkTailRec {
		_, _ = env.Runtime.Stack.DebugPrint(env.Runtime.getStderr())
		log.Panicf("tail-recursion optimization attempted during argument evaluation: %v", f.Cells)
	}

	newCells[0] = f
	if f.IsSpecialFun() {
		// Arguments to a macro are not evaluated but they aren't quoted
		// either.  This behavior is what allows ``unquote'' to properly
		// resolve macro argument symbols during and still produce valid code
		// during macro expansion.  That is, if x is a macro argument then what
		// do the following expressions return?
		//		(quasiquote (unquote x))             	  => {expression bound to x}
		//		(quasiquote (unquote '(if 1 '(1) '(2))))  => '(1)
		// If the value given to x was quoted by eval then ``unquote'' would
		// have to undo that quoting.  But unquote is not supposed to unquote
		// the value returned by (if 1 '(1) '(2)), it merely evaluates the
		// expression and produces '(1).
		newCells = append(newCells, cells...)
		return SExpr(newCells)
	}
	// Evaluate arguments before invoking f.
	for _, expr := range cells {
		v := env.Eval(expr)
		if v.Type == LError {
			return v
		}
		if v.Type == LMarkTailRec {
			_, _ = env.Runtime.Stack.DebugPrint(env.Runtime.getStderr())
			log.Panicf("tail-recursion optimization attempted during argument evaluation: %v", v.Cells)
		}

		newCells = append(newCells, v)
	}
	return SExpr(newCells)
}

// call invokes LFun fun with the list args.  In general it is not safe to call
// env.call bacause the stack must be setup for tail recursion optimization.
func (env *LEnv) call(fun *LVal, args *LVal) *LVal {
	fenv, list := env.bind(fun, args)
	if list.Type == LError {
		return list
	}

	// NOTE:  The book's suggestion of chaining env here seems like dynamic
	// scoping.

	fn := fun.Builtin()
	if fn != nil {
		// FIXME:  I think fun.Env is probably correct here.  But it wouldn't
		// surprise me if at least one builtin breaks when it switches.
		val := fn(env, list)
		if val.Type == LMarkTerminal {
			env.Runtime.Stack.Top().Terminal = true
			return val.Native.(*LEnv).Eval(val.Cells[0])
		}
		return val
	}

	// With formal arguments bound, we can switch into the function's package
	// namespace for the duration of the call.
	//
	// BUG:  This package-swap should occur for builtins as well but there is a
	// bootstrapping problem, where ``set'' (as well as defun/defmacro) needs
	// to modify the *package* namespace and not the "lisp" namespace.  Dynamic
	// variables may be required in order to work through this completely.
	outer := env.Runtime.Package
	pkg := fun.Package()
	if outer.Name != pkg {
		inner := env.Runtime.Registry.Packages[pkg]
		if inner != nil {
			env.Runtime.Package = inner
			defer func() {
				env.Runtime.Package = outer
			}()
		}
	}

	if list.Len() == 0 {
		return Nil()
	}
	body := list.Cells
	var ret *LVal
	for i := 0; i < len(body)-1; i++ {
		ret = fenv.Eval(body[i])
		if ret.Type == LError {
			return ret
		}
	}
	if !fun.IsMacro() {
		env.Runtime.Stack.Top().Terminal = true
	}
	return fenv.Eval(body[len(body)-1])
}

// If fun is a builtin bind returns an LEnv for executing fun and a list of
// arguments.  If fun is a lambda bind returns a non-nil lexical environment
// and a list of body expressions (subslice of fun.Cells).  If an error is
// encountered then bind returns it as the second argument.
//
// The bind function does not modify fun or args.
func (env *LEnv) bind(fun, args *LVal) (*LEnv, *LVal) {
	argsp := argParser{args: args.Cells}
	formals := argParser{args: fun.Cells[0].Cells}
	narg := len(args.Cells)

	funenv := fun.Env().Copy()
	putArg := func(k, v *LVal) {
		funenv.Put(k, v)
	}
	putVarArg := func(k *LVal, v *LVal) {
		funenv.Put(k, v)
	}
	var builtinArgs []*LVal
	if funenv == nil {
		// FIXME?: Builtins don't have lexical envs.  We just store the args in
		// the cells for builtin functions.  A side effect of this is that
		// bindFormalNext is required to make put() calls in the order args are
		// defined.
		if argsp.Len() > formals.Len() {
			builtinArgs = make([]*LVal, 0, argsp.Len())
		} else {
			builtinArgs = make([]*LVal, 0, formals.Len())
		}
		putArg = func(k, v *LVal) {
			builtinArgs = append(builtinArgs, v)
		}
		putVarArg = func(k *LVal, v *LVal) {
			builtinArgs = append(builtinArgs, v.Cells...)
		}
	}
	nformal := formals.Pos()
	for !formals.IsEOF() {
		ret := env.bindFormalNext(fun, &formals, &argsp, putArg, putVarArg)
		if ret.Type == LError {
			return nil, ret
		}
		if ret.Type == LFun {
			// This is where one might return new function that allows partial
			// binding of function arguments.
			return nil, env.Errorf("invalid number of arguments: %v", narg)
		}
		if !ret.IsNil() {
			panic("unexpected formal binding state")
		}
		if formals.Pos() == nformal {
			panic("no progress binding")
		}
		nformal = formals.Pos()
	}
	if !argsp.IsEOF() {
		return nil, env.Errorf("invalid number of arguments: %d", narg)
	}
	if funenv == nil {
		return env, QExpr(builtinArgs)
	}
	return funenv, QExpr(fun.Cells[1:])
}

type bindfunc func(k, v *LVal)

func (env *LEnv) bindFormalNext(fun *LVal, formals, args *argParser, put, putVarArgs bindfunc) *LVal {
	argSym := formals.Advance()
	switch {
	case argSym.Str == KeyArgSymbol:
		if formals.IsEOF() {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		keyCells := formals.Rest()
		keymap := make(map[string]*LVal, len(keyCells))
		keys := make([]string, 0, len(keyCells))
		if args.Rem()%2 != 0 {
			return env.Errorf("function called with an odd number of keyword arguments")
		}
		for !args.IsEOF() {
			key := args.Advance()
			val := args.Advance()
			if key.Type != LSymbol {
				return env.Errorf("argument is not a keyword: %v", key.Type)
			}
			if !isKeyword(key.Str) {
				return env.Errorf("argument is not a keyword: %v", key.Str)
			}
			keymap[key.Str[1:]] = val
			keys = append(keys, key.Str[1:])
		}
		for _, key := range keyCells {
			if strings.HasPrefix(key.Str, MetaArgPrefix) {
				return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
			}
			val, ok := keymap[key.Str]
			if !ok {
				put(key, Nil())
				continue
			}
			delete(keymap, key.Str)
			put(key, val)
		}
		if len(keymap) > 0 {
			// Scan through keys in the order they were given to provide a
			// logical, deterministic error message.
			for _, k := range keys {
				_, ok := keymap[k]
				if ok {
					return env.Errorf("unrecognized keyword argument: %v", k)
				}
			}
		}
		return Nil()
	case argSym.Str == OptArgSymbol:
		if formals.IsEOF() {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		for !formals.IsEOF() {
			argSym = formals.Peek()
			if strings.HasPrefix(argSym.Str, MetaArgPrefix) {
				return Nil()
			}
			formals.Advance()
			if args.IsEOF() {
				// No arguments left so we bind the optional arg to nil.
				put(argSym, Nil())
			} else {
				put(argSym, args.Advance())
			}
		}
		return Nil()
	case argSym.Str == VarArgSymbol:
		symbols := formals.Rest()
		if len(symbols) != 1 {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		argSym = symbols[0]
		if strings.HasPrefix(argSym.Str, MetaArgPrefix) {
			return env.Errorf("function formal argument list contains a control symbol at an invalid location: %v", argSym.Str)
		}
		putVarArgs(argSym, QExpr(args.Rest()))
		return Nil()
	case strings.HasPrefix(argSym.Str, MetaArgPrefix):
		return env.Errorf("function formal argument list contains invalid control symbol ``%s''", argSym.Str)
	default:
		if args.IsEOF() {
			return fun
		}
		// This is a normal (required) argument symbol.  Pull a value out of
		// args and bind it.
		put(argSym, args.Advance())
		return Nil()
	}
}

type argParser struct {
	args []*LVal
	i    int
}

func (p *argParser) Pos() int {
	return p.i
}

func (p *argParser) Len() int {
	return len(p.args)
}

func (p *argParser) Rem() int {
	return len(p.args) - p.i
}

func (p *argParser) IsEOF() bool {
	return p.i >= len(p.args)
}

func (p *argParser) Advance() *LVal {
	v := p.args[p.i]
	p.i += 1
	return v
}

func (p *argParser) Peek() *LVal {
	return p.args[p.i]
}

func (p *argParser) Rest() []*LVal {
	v := p.args[p.i:]
	p.i = len(p.args)
	return v
}

func isKeyword(sym string) bool {
	// TODO:  Fix this terrible test.
	return strings.HasPrefix(sym, ":")
}
