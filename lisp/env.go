// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"log"
	"runtime"
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
	env.Runtime.Package = env.Runtime.Registry.packages[env.Runtime.Registry.Lang]
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
	env.Runtime.Registry.packages[DefaultUserPackage].Doc = "The default user package for application code."
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
	fqname := Symbol(pkg + ":typedef")
	typedef := env.TaggedValue(fqname, QExpr([]*LVal{fqname, ctor}))
	if typedef.Type == LError {
		return typedef
	}
	env.Runtime.Registry.packages[pkg].Put(Symbol("typedef"), typedef)
	return Nil()
}

// TODO(elps2): Remove the field LEnv.FunName

// LEnv is a lisp environment.
//
// Field order is layout-sensitive: the pointer-bearing fields lead so the GC
// scan extent stops at 56 bytes instead of 64. Keep scalars (ID) trailing.
type LEnv struct {
	Loc     *token.Location
	Scope   map[string]*LVal
	FunName map[string]string
	Parent  *LEnv
	Runtime *Runtime
	evalCtx context.Context // transient: set by call() at builtin boundary
	ID      uint
}

// Context returns the context.Context currently associated with this
// environment.  If no context has been set, context.Background() is returned.
func (env *LEnv) Context() context.Context {
	if env.evalCtx != nil {
		return env.evalCtx
	}
	return context.Background()
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
	var evalCtx context.Context
	if parent != nil {
		runtime = parent.Runtime
		loc = parent.Loc
		evalCtx = parent.evalCtx
	} else {
		runtime = StandardRuntime()
	}
	env := &LEnv{
		ID:      runtime.GenEnvID(),
		Loc:     loc,
		Scope:   make(map[string]*LVal, n),
		FunName: make(map[string]string),
		Parent:  parent,
		Runtime: runtime,
		evalCtx: evalCtx,
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
	env.Runtime.Package.symbolDocs[name] = doc
}

func (env *LEnv) InPackage(name *LVal) *LVal {
	if name.Type != LSymbol && name.Type != LString {
		return env.Errorf("argument cannot be converted to string: %v", name.Type)
	}
	pkg := env.Runtime.Registry.packages[name.Str]
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
	pkg := env.Runtime.Registry.packages[name.Str]
	if pkg == nil {
		return env.Errorf("unknown package: %v", name.Str)
	}
	for _, sym := range pkg.externals {
		v := pkg.Get(Symbol(sym))
		if v.Type == LError {
			return env.Errorf("package %s: %v", name.Str, v)
		}
		env.Runtime.Package.Put(Symbol(sym), v)
	}
	return Nil()
}

// LoadString loads and evaluates expressions from a string.
//
// Deprecated: Use LoadStringContext for cancellation and timeout support.
func (env *LEnv) LoadString(name, exprs string) *LVal {
	return env.Load(name, strings.NewReader(exprs))
}

// LoadFile attempts to use env.Runtime.Library to read a lisp source file and
// evaluate expressions it contains.  Any error encountered will prevent
// execution of loaded source and be returned.  After evaluating expressions
// the current package is restored to the current package at the time Load was
// called, in case loaded source made calls to "in-package".  If
// env.Runtime.Reader has not been set then an error will be returned by Load.
//
// Deprecated: Use LoadFileContext for cancellation and timeout support.
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
// time Load was called, in case loaded source made calls to "in-package".
// If env.Runtime.Reader has not been set then an error will be returned by Load.
//
// Deprecated: Use LoadContext for cancellation and timeout support.
func (env *LEnv) Load(name string, r io.Reader) *LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for environment runtime")
	}

	exprs, err := env.Runtime.Reader.Read(name, r)
	if err != nil {
		return env.Error(err)
	}

	return env.load(env.evalCtx, exprs)
}

// LoadLocation attempts to use env.Runtime.Library to read a lisp source file,
// specifying its name and location explicity, and evaluate the expressions it
// contains.  Because the name and location of the stream are specfied
// explicitly LoadLocation does not depend explicity on an env.Runtime.Library
// implementation.
// Any error encountered will prevent execution of loaded source and
// be returned.  After evaluating expressions the current package is restored
// to the current package at the time Load was called, in case loaded source
// made calls to "in-package".  If env.Runtime.Reader has not been set then
// an error will be returned by Load.
//
// Deprecated: Use LoadLocationContext for cancellation and timeout support.
func (env *LEnv) LoadLocation(name string, loc string, r io.Reader) *LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for environment runtime")
	}

	reader, ok := env.Runtime.Reader.(LocationReader)
	if !ok {
		return env.Load(loc, r)
	}
	exprs, err := reader.ReadLocation(name, loc, r)
	if err != nil {
		return env.Error(err)
	}

	return env.load(env.evalCtx, exprs)
}

func (env *LEnv) load(ctx context.Context, exprs []*LVal) *LVal {
	if len(exprs) == 0 {
		return Nil()
	}
	// load is the funnel for every exported Load* entry point.  Treat the
	// whole load as one top-level evaluation so the step budget covers all
	// of its forms rather than refilling per form.
	defer env.Runtime.beginEval()()

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
		ret = env.eval(ctx, expr)
		if ret.Type == LError {
			break
		}
	}
	// Checked builds re-verify this load's sealed parse against its
	// seal-time fingerprint, on the error path too — an evaluation error
	// does not excuse corrupting the shared tree.  A no-op in production
	// builds; see lisp/seal_check_elpscheck.go.
	verifySealedLoadRoots(exprs)
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
		if err := env.ErrorAssociate(lerr); err != nil {
			return err
		}
		return lerr
	}
	pkg := env.Runtime.Registry.packages[ns]
	if pkg == nil {
		return env.Errorf("unknown package: %q", ns)
	}
	lerr := pkg.Get(Symbol(name))
	if lerr.Type == LError {
		if err := env.ErrorAssociate(lerr); err != nil {
			return err
		}
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
		if err := env.ErrorAssociate(lerr); err != nil {
			return err
		}
	}
	return lerr
}

// GetFunName returns the function name (if any) known to be bound to the given
// function. If the function's FID is bound in its package then the global name
// of the function is returned.  When the function is bound within a local
// scope then the local name used to reference the function (if any) is
// returned.
//
// Safety: the error path in this function is cosmetic-only and does not mask
// data corruption.  Every caller (MacroCall, SpecialOpCall, funCall, call,
// profiler) has already verified that f.Type == LFun before reaching here, so
// pkgFunName should never fail.  The fallback to f.Str only affects the
// human-readable name shown in error messages and stack traces — it cannot
// influence evaluation, binding, or control flow.  We log at BUG level so the
// issue is visible in diagnostics without changing the return type to an error
// that every caller would have to handle for an unreachable code path.
func (env *LEnv) GetFunName(f *LVal) string {
	name, err := env.pkgFunName(f)
	if err != nil {
		log.Printf("BUG: GetFunName: %v", err)
		return f.Str
	}
	if name != "" {
		return name
	}
	return f.Str
}

func (env *LEnv) pkgFunName(f *LVal) (string, error) {
	if f.Type != LFun {
		return "", fmt.Errorf("not a function: %v", f.Type)
	}
	pkgname := f.Package()
	if pkgname == "" {
		return "", fmt.Errorf("unknown package for function %s", f.FID())
	}
	pkg := env.Runtime.Registry.packages[pkgname]
	if pkg == nil {
		return "", fmt.Errorf("package not found: %q", pkgname)
	}
	return pkg.funNames[f.FID()], nil
}

// Put takes an LSymbol k and binds it to v in env.  If k is already bound to a
// value the binding is updated so that k is bound to v.
func (env *LEnv) Put(k, v *LVal) *LVal {
	// Ownership check (elpscheck builds only; no-op otherwise): a binding
	// is the durable way a value enters a runtime, so both the key and the
	// value are adopted/asserted here.
	checkOwnership(env.Runtime, k)
	checkOwnership(env.Runtime, v)
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
				if err := env.ErrorAssociate(lerr); err != nil {
					return err
				}
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
		if err := env.ErrorAssociate(pieces); err != nil {
			return err
		}
		return pieces
	}
	if pieces.Len() == 2 {
		ns := pieces.Cells[0].Str
		if ns == "" {
			// keyword
			return k
		}
		pkg := env.Runtime.Registry.packages[ns]
		if pkg == nil {
			return env.Errorf("unknown package: %q", ns)
		}
		lerr := pkg.Get(pieces.Cells[1])
		if lerr.Type == LError {
			if err := env.ErrorAssociate(lerr); err != nil {
				return err
			}
		}
		return lerr
	}
	return env.packageGet(k)
}

// PutGlobal takes an LSymbol k and binds it to v in current package.
func (env *LEnv) PutGlobal(k, v *LVal) *LVal {
	// Ownership check (elpscheck builds only; no-op otherwise).  This entry
	// covers the package-scope binding path, including the pkg.Put calls
	// below.  Package.Put itself is not instrumented — a Package has no
	// *Runtime reference to assert against; every evaluator-driven global
	// write funnels through here first.
	checkOwnership(env.Runtime, k)
	checkOwnership(env.Runtime, v)
	pieces := SplitSymbol(k)
	if pieces.Type == LError {
		if err := env.ErrorAssociate(pieces); err != nil {
			return err
		}
		return pieces
	}
	if pieces.Len() == 2 {
		ns := pieces.Cells[0].Str
		if ns == "" {
			return env.Errorf("value cannot be assigned to a keyword: %s", k.Str)
		}
		pkg := env.Runtime.Registry.packages[ns]
		if pkg == nil {
			return env.Errorf("unknown package: %q", ns)
		}
		lerr := pkg.Put(pieces.Cells[1], v)
		if lerr.Type == LError {
			if err := env.ErrorAssociate(lerr); err != nil {
				return err
			}
		}
		return lerr
	}

	lerr := env.Runtime.Package.Put(k, v)
	if lerr.Type == LError {
		if err := env.ErrorAssociate(lerr); err != nil {
			return err
		}
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
		source: env.Loc,
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
	if typ.Str != env.Runtime.Registry.Lang+":typedef" {
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
		source: env.Loc,
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
	return FunInPackage(env.Runtime.Package.Name, NewEnv(env).getFID(), f.Formals(), f.Eval)
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

// registrationFormals returns a builtin definition's formal argument list in
// a form safe to register into env's package.  Builtin definitions are
// commonly stored in package-level tables (var builtins = ...) whose
// Formals() lists are constructed once at Go program initialization;
// registering such a list directly would alias the same MUTABLE formals
// cells — and the parameter-name symbol cells inside them — across every
// environment built in the process (issue #363).
//
// Sealed formals are shared as-is.  The lisp package's own definition
// tables (langBuiltins/langMacros/langSpecialOps and the RegisterDefault*
// user tables) are sealed at construction — see sealDefaultFormals in
// builtins.go — which puts them under exactly the protection lisp-defined
// functions already rely on: a lambda's formals ARE sealed parser output
// aliased into every closure that shares the parse, guarded by the
// copy-on-write mutation checks (lisp/seal.go) and, in checked builds, the
// fingerprint verifier (VerifySealedASTs).  Sharing the sealed list keeps
// environment initialization from paying a deep copy per builtin per env:
// the eager copy this replaces cost ~90KiB and >1000 allocations on every
// LoadLibrary environment (the CI benchmark gate flagged it as a +9.3%
// B/op regression on libjson's Package/$load).
//
// Unsealed formals — a third-party LBuiltinDef implementation whose
// Formals() returns hand-built, unsealed, potentially shared cells — get
// the defensive deep copy.  The copy must be recursive: a shallow copy of
// the list cell alone would still share the symbol cells, just moving the
// trap one level down.  (*LVal).Copy is a deep copy over Cells, so it
// severs the whole graph while leaving Source location pointers shared,
// which is by design — locations are immutable.
func registrationFormals(formals *LVal) *LVal {
	if formals.IsSealed() {
		return formals
	}
	return formals.Copy()
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
		fn := MacroInPackage(pkg.Name, id, registrationFormals(mac.Formals()), mac.Eval)
		fn.Cells[1] = String(builtinDocstring(mac))
		pkg.Put(k, fn)
		if external {
			pkg.externals = append(pkg.externals, k.Str)
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
		fn := SpecialOpInPackage(pkg.Name, id, registrationFormals(op.Formals()), op.Eval)
		fn.Cells[1] = String(builtinDocstring(op))
		pkg.Put(k, fn)
		if external {
			pkg.externals = append(pkg.externals, k.Str)
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
		v := FunInPackage(pkg.Name, id, registrationFormals(f.Formals()), f.Eval)
		v.Cells[1] = String(builtinDocstring(f))
		pkg.Put(k, v)
		if external {
			pkg.externals = append(pkg.externals, k.Str)
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
				return ErrorConditionf("runtime", "invalid error argument: cannot mix error and *LVal arguments")
			}
			lerr := &LVal{
				Type:   LError,
				Str:    condition,
				Native: env.Runtime.Stack.Copy(),
				Cells:  []*LVal{Native(v)},
			}
			if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
				if d.OnError(env, lerr) {
					d.WaitIfPaused(env, lerr)
				}
			}
			return lerr
		case string:
			cells = append(cells, String(v))
		default:
			cells = append(cells, Native(v))
		}
	}
	lerr := &LVal{
		Type: LError,
		// Copy the location instead of aliasing env.Loc: the error value
		// escapes to the caller while evaluation continues, so it must not
		// share a *token.Location the evaluator (or a producing parser) may
		// still fix up in place.  Mirrors ErrorAssociate (d922290);
		// copyLocation preserves nil (the "<native code>" convention).
		source: copyLocation(env.Loc),
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  cells,
	}
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		if d.OnError(env, lerr) {
			d.WaitIfPaused(env, lerr)
		}
	}
	return lerr
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
	lerr := &LVal{
		// Copied, not aliased — see the ErrorCondition comment.
		source: copyLocation(env.Loc),
		Type:   LError,
		Str:    condition,
		Native: env.Runtime.Stack.Copy(),
		Cells:  []*LVal{String(fmt.Sprintf(format, v...))},
	}
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		if d.OnError(env, lerr) {
			d.WaitIfPaused(env, lerr)
		}
	}
	return lerr
}

// ErrorAssociate associates the LError value lerr with env's current call
// stack and source location.  ErrorAssociate returns an LError if lerr is
// not an error value (indicating a bug in the caller), or nil on success.
func (env *LEnv) ErrorAssociate(lerr *LVal) *LVal {
	if lerr.Type != LError {
		return env.Errorf("internal error: ErrorAssociate called with non-error: %v", lerr.Type)
	}
	if lerr.CallStack() == nil {
		lerr.SetCallStack(env.Runtime.Stack.Copy())
	}
	// This check smells a little funny.  An object's source may be absent
	// (nil — the "<native code>" convention) or carry an invalid position
	// (-1).  When associating an error
	// the env's current location is probably more accurate than native
	// source (or it may also be native source).
	if lerr.source == nil || lerr.source.Pos < 0 {
		// Copy the location instead of aliasing env.Loc.  The error value
		// escapes to the caller while evaluation continues, so it must not
		// share a *token.Location the evaluator may still reference.
		// copyLocation preserves nil (the "<native code>" convention).
		lerr.source = copyLocation(env.Loc) //elps:mutates stamps a location onto an in-flight error that had none; the location itself is copied, not aliased
	}
	return nil
}

// checkLimits is the fast-path evaluation limit check.  When neither a
// context nor a step limit is configured (the default), this is two nil/zero
// comparisons (~1-2ns) and returns nil immediately.
func (env *LEnv) checkLimits(ctx context.Context) *LVal {
	r := env.Runtime
	if ctx == nil && r.maxSteps == 0 {
		return nil
	}
	return env.checkLimitsSlow(ctx)
}

// checkLimitsSlow is the cold-path limit check.  It increments the step
// counter, checks the step limit, and checks the context for cancellation.
func (env *LEnv) checkLimitsSlow(ctx context.Context) *LVal {
	r := env.Runtime
	r.steps++
	if r.maxSteps > 0 && r.steps > r.maxSteps {
		return env.ErrorConditionf(CondStepLimitExceeded,
			"step limit exceeded (%d steps)", r.maxSteps)
	}
	if ctx != nil {
		if err := ctx.Err(); err != nil {
			return env.ErrorConditionf(CondContextCancelled,
				"context cancelled: %v", err)
		}
	}
	return nil
}

// Eval evaluates v in the context (scope) of env and returns the resulting
// LVal.  Eval does not modify v.
//
// Deprecated: Use EvalContext for cancellation and timeout support.
func (env *LEnv) Eval(v *LVal) *LVal {
	defer env.Runtime.beginEval()()
	return env.eval(env.evalCtx, v)
}

// eval is the core evaluation implementation.  It evaluates v in the context
// (scope) of env using the given context.Context for cancellation/timeout.
//
// eval includes a recover() safety net that converts any Go panic during
// evaluation into an LError, preventing panics from crashing the host process
// when ELPS is embedded.
//
// eval also carries the evaluator's recursion-depth guard.  Every nested
// evaluation passes through here, so incrementing a counter on entry and
// decrementing it on exit measures the Go stack the evaluator is consuming.
// This is NOT the same quantity as CallStack height: evalSExprCells evaluates
// a call's arguments before any frame is pushed, so nested arguments recurse
// through eval at physical height zero and MaxHeightPhysical never sees them
// (issue #316).  A counter rather than a frame is deliberate -- a frame here
// would corrupt the error messages and stack dumps produced while an argument
// is being evaluated, which is why evalSExprCells pushes none.
//
// NOTE:  eval shouldn't unquote v during evaluation -- a difference between
// Eval and the "eval" builtin function, but it does.  For some reason macros
// won't work without this unquoting.
func (env *LEnv) eval(ctx context.Context, v *LVal) (result *LVal) {
	env.Runtime.evalNesting++
	defer func() {
		env.Runtime.evalNesting--
		if r := recover(); r != nil {
			// Ownership violations (elpscheck builds only) must stay hard
			// panics: re-panic before the conversion below can launder the
			// finding into a catchable LError.  No-op in release builds.
			rethrowOwnershipViolation(r)
			// Tag the error with CondInternalPanic so it is distinguishable
			// from a lisp-level error: a panic is a host-code bug, and
			// ignore-errors / catch-all handler-bind must not silently
			// swallow it.  See the CondInternalPanic doc comment.
			result = env.ErrorConditionf(CondInternalPanic,
				"internal error (recovered panic): %v", r)
			// Capture the Go stack at the panic origin so any caller of
			// (*ErrorVal).WriteTrace (or direct readers of CallStack.GoStack)
			// can render it. This defer runs before the panic unwind
			// completes, so runtime.Stack reflects the panic site, not the
			// recover frame.
			if stack := result.CallStack(); stack != nil {
				buf := make([]byte, 16*1024)
				n := runtime.Stack(buf, false)
				stack.GoStack = buf[:n]
			}
		}
	}()
	// Ownership check (elpscheck builds only; no-op otherwise): eval is the
	// funnel every expression passes through, so the first evaluation of a
	// value adopts it for this Runtime and any later evaluation under a
	// different Runtime panics.  Placed after the deferred recover so the
	// evalNesting counter stays balanced when the check panics.
	checkOwnership(env.Runtime, v)
	if env.Runtime.evalNestingExceeded() {
		return env.ErrorConditionf(CondEvalNestingExceeded,
			"evaluation nesting depth exceeded maximum: %d"+
				" (expressions nested this deeply consume Go stack without pushing"+
				" call frames, so the limit stops the Go runtime from aborting the"+
				" process with an unrecoverable stack overflow; raise or disable it"+
				" with WithMaxEvalNesting)", env.Runtime.evalNesting)
	}
	macroDepth := 0
eval:
	if lerr := env.checkLimits(ctx); lerr != nil {
		return lerr
	}
	if v.spliced {
		return env.Errorf("spliced value used as expression")
	}
	env.Loc = v.source
	if v.source != nil {
		if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
			if d.OnEval(env, v) {
				d.WaitIfPaused(env, v)
			}
		}
	}
	if v.quoted {
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
		pkg := env.Runtime.Registry.packages[ns]
		if pkg == nil {
			return env.Errorf("unknown package: %q", ns)
		}
		lerr := pkg.Get(Symbol(name))
		if lerr.Type == LError {
			if err := env.ErrorAssociate(lerr); err != nil {
				return err
			}
		}
		return lerr
	case LSExpr:
		res := env.evalSExpr(ctx, v)
		// Post-call check: after a function call returns, the stack
		// frame has been popped and depth has decreased. If the debugger
		// is stepping out, this is where we catch tail-position returns
		// that would otherwise unwind without hitting OnEval.
		if d := env.Runtime.Debugger; d != nil && d.IsEnabled() && v.source != nil {
			if d.AfterFunCall(env) {
				d.WaitIfPaused(env, v)
			}
		}
		if res.Type == LMarkMacExpand {
			// A macro was just expanded and returned an unevaluated
			// expression.  We have to evaluate the result before we return.
			macroDepth++
			if macroDepth > env.Runtime.MaxMacroExpansions() {
				return env.Errorf("macro expansion depth exceeded (%d expansions)", macroDepth)
			}
			v = res.Cells[0]
			goto eval
		}
		if res.Type == LError {
			if err := env.ErrorAssociate(res); err != nil {
				return err
			}
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
	defer env.Runtime.beginEval()()
	return env.evalSExpr(env.evalCtx, s)
}

func (env *LEnv) evalSExpr(ctx context.Context, s *LVal) *LVal {
	if s.Type != LSExpr {
		return env.Errorf("not an s-expression")
	}
	if len(s.Cells) == 0 {
		return Nil()
	}
	call := env.evalSExprCells(ctx, s)
	if call.Type == LError {
		if err := env.ErrorAssociate(call); err != nil {
			return err
		}
		return call
	}
	fun := call.Cells[0] // call is not an empty expression -- fun is known LFun
	args := call
	args.Cells = args.Cells[1:] //elps:mutates decap of the call value evalSExprCells constructed just above with fresh backing

	switch fun.FunType {
	case LFunNone:
		return env.funCall(ctx, fun, args)
	case LFunSpecialOp:
		return env.specialOpCall(ctx, fun, args)
	case LFunMacro:
		return env.macroCall(ctx, fun, args)
	default:
		return env.Errorf("internal error: invalid function type %v", fun.FunType)
	}
}

// MacroCall invokes macro fun with argument list args.
func (env *LEnv) MacroCall(fun, args *LVal) *LVal {
	defer env.Runtime.beginEval()()
	return env.macroCall(env.evalCtx, fun, args)
}

func (env *LEnv) macroCall(ctx context.Context, fun, args *LVal) *LVal {
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

	r := env.call(ctx, fun, args)
	if r == nil {
		return env.Errorf("internal error: macro %s returned nil", env.GetFunName(fun))
	}
	if r.Type == LError {
		return r
	}

	// NOTE:  There should be no need to check for LMarkTailRec objects because
	// we block all tail-recursion for macro calls.

	// Stamp expanded nodes that have synthetic source locations (Pos < 0)
	// with the call-site location so errors point to where the macro was
	// invoked rather than "<native code>" or the macro definition site.
	// When a debugger is attached, also populate macroExpansionInfo on
	// each stamped node with unique IDs for step-into differentiation.
	var mctx *macroExpansionContext
	if env.Runtime.Debugger != nil {
		qualName := env.GetFunName(fun)
		if pkg := fun.Package(); pkg != "" {
			qualName = pkg + ":" + qualName
		}
		mctx = &macroExpansionContext{
			CallSite: callSite,
			Name:     qualName,
			DefSite:  fun.source,
			Args:     args.Cells,
		}
	}
	stampMacroExpansion(r, callSite, mctx, env.Runtime)

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
	defer env.Runtime.beginEval()()
	return env.specialOpCall(env.evalCtx, fun, args)
}

func (env *LEnv) specialOpCall(ctx context.Context, fun, args *LVal) *LVal {
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
	r := env.call(ctx, fun, args)
	if r == nil {
		return env.Errorf("internal error: special operator %s returned nil", env.GetFunName(fun))
	}
	if r.Type == LError {
		return r
	}

	if r.Type == LMarkTailRec {
		// Tail recursion optimization is occurring.
		if decrementMarkTailRec(r) {
			top := env.Runtime.Stack.Top()
			top.HeightLogical += r.tailRecElided()
			top.TailIterations++
			err := env.Runtime.Stack.CheckTailCall()
			if err != nil {
				return env.Error(err)
			}
			if lerr := env.checkLimits(ctx); lerr != nil {
				return lerr
			}
			fun, args = extractMarkTailRec(r)
			goto callf
		}
		return r
	}

	return r
}

// FunCall invokes regular function fun with the argument list args.
//
// Deprecated: Use FunCallContext for cancellation and timeout support.
func (env *LEnv) FunCall(fun, args *LVal) *LVal {
	defer env.Runtime.beginEval()()
	return env.funCall(env.evalCtx, fun, args)
}

// EvalContext evaluates v with the given context.  If ctx is cancelled or
// its deadline expires during evaluation, a CondContextCancelled error is
// returned.
func (env *LEnv) EvalContext(ctx context.Context, v *LVal) *LVal {
	defer env.Runtime.beginEval()()
	return env.eval(ctx, v)
}

// LoadContext reads LVals from r and evaluates them with the given context.
func (env *LEnv) LoadContext(ctx context.Context, name string, r io.Reader) *LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for environment runtime")
	}
	exprs, err := env.Runtime.Reader.Read(name, r)
	if err != nil {
		return env.Error(err)
	}
	return env.load(ctx, exprs)
}

// LoadFileContext loads and evaluates a source file with the given context.
func (env *LEnv) LoadFileContext(ctx context.Context, loc string) *LVal {
	if env.Runtime.Library == nil {
		return env.Errorf("no source library in environment runtime")
	}
	sctx := env.Runtime.sourceContext()
	name, loc, src, err := env.Runtime.Library.LoadSource(sctx, loc)
	if err != nil {
		return env.Errorf("library error: %v", err)
	}
	return env.LoadLocationContext(ctx, name, loc, bytes.NewReader(src))
}

// LoadStringContext loads and evaluates a string with the given context.
func (env *LEnv) LoadStringContext(ctx context.Context, name, exprs string) *LVal {
	return env.LoadContext(ctx, name, strings.NewReader(exprs))
}

// LoadLocationContext loads and evaluates a source stream at a given location
// with the given context.
func (env *LEnv) LoadLocationContext(ctx context.Context, name, loc string, r io.Reader) *LVal {
	if env.Runtime.Reader == nil {
		return env.Errorf("no reader for environment runtime")
	}
	reader, ok := env.Runtime.Reader.(LocationReader)
	if !ok {
		return env.LoadContext(ctx, loc, r)
	}
	exprs, err := reader.ReadLocation(name, loc, r)
	if err != nil {
		return env.Error(err)
	}
	return env.load(ctx, exprs)
}

// FunCallContext invokes regular function fun with args under the given
// context.  If ctx is cancelled or its deadline expires during the call,
// a CondContextCancelled error is returned.
func (env *LEnv) FunCallContext(ctx context.Context, fun, args *LVal) *LVal {
	defer env.Runtime.beginEval()()
	return env.funCall(ctx, fun, args)
}

func (env *LEnv) trace(fun *LVal) func() {
	if env.Runtime.Profiler == nil {
		return func() {}
	}
	// fun might be an anon function, so we need to convert it to get the
	// right type of LVal for filtering and labeling
	return env.Runtime.Profiler.Start(fun)
}

func (env *LEnv) funCall(ctx context.Context, fun, args *LVal) *LVal {
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
	// book-keeping.  When a debugger is attached, TRO is disabled globally
	// to provide predictable stepping and stack traces.
	npop := 0
	if env.Runtime.Debugger == nil {
		npop = env.Runtime.Stack.TerminalFID(fun.FID())
	}

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
	r := env.call(ctx, fun, args)
	if r == nil {
		return env.Errorf("internal error: function %s returned nil", env.GetFunName(fun))
	}
	if r.Type == LError {
		return r
	}

	if r.Type == LMarkTailRec {
		// Tail recursion optimization is occurring.
		done := decrementMarkTailRec(r)
		if done {
			top := env.Runtime.Stack.Top()
			top.HeightLogical += r.tailRecElided()
			top.TailIterations++
			err := env.Runtime.Stack.CheckTailCall()
			if err != nil {
				return env.Error(err)
			}
			if lerr := env.checkLimits(ctx); lerr != nil {
				return lerr
			}
			fun, args = extractMarkTailRec(r)
			goto callf
		}
	}

	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		d.OnFunReturn(env, fun, r)
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
	mark.Cells[0].Int-- //elps:mutates LMarkTailRec is evaluator-internal bookkeeping built by markTailRec; decrementing its counter is the mechanism
	return mark.Cells[0].Int <= 0
}

func (env *LEnv) evalSExprCells(ctx context.Context, s *LVal) *LVal {
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
	f := env.eval(ctx, cells[0])
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
		v := env.eval(ctx, expr)
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
// env.call because the stack must be setup for tail recursion optimization.
//
// At the builtin boundary, ctx is bridged onto env.evalCtx so that builtins
// calling env.Eval() see the correct context.
func (env *LEnv) call(ctx context.Context, fun *LVal, args *LVal) *LVal {
	fenv, list := env.bind(fun, args)
	if list.Type == LError {
		return list
	}
	if d := env.Runtime.Debugger; d != nil && d.IsEnabled() {
		// fenv != env means this is a user-defined function (not a builtin).
		// Builtins return the caller's env from bind.
		if fenv != nil && fenv != env {
			d.OnFunEntry(env, fun, fenv)
		}
	}

	// NOTE:  The book's suggestion of chaining env here seems like dynamic
	// scoping.

	fn := fun.Builtin()
	if fn != nil {
		// Bridge ctx onto env so builtins that call env.Eval() pick it up.
		// Save and restore to prevent stale ctx from leaking after the
		// builtin returns.
		prev := env.evalCtx
		env.evalCtx = ctx
		val := fn(env, list)
		env.evalCtx = prev
		if val == nil {
			return env.Errorf("internal error: builtin %s returned nil", env.GetFunName(fun))
		}
		if val.Type == LMarkTerminal {
			env.Runtime.Stack.Top().Terminal = true
			termEnv := val.Native.(*LEnv)
			termEnv.evalCtx = ctx
			return termEnv.eval(ctx, val.Cells[0])
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
		inner := env.Runtime.Registry.packages[pkg]
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
	for i := range len(body) - 1 {
		ret = fenv.eval(ctx, body[i])
		if ret.Type == LError {
			return ret
		}
	}
	if !fun.IsMacro() {
		env.Runtime.Stack.Top().Terminal = true
	}
	return fenv.eval(ctx, body[len(body)-1])
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
			return nil, env.Errorf("internal error: unexpected formal binding state")
		}
		if formals.Pos() == nformal {
			return nil, env.Errorf("internal error: no progress binding formals")
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
