// Copyright © 2018 The ELPS authors

package lisp

import (
	"fmt"
	"sync"

	macroexphook "github.com/luthersystems/elps/internal/macroexp/hook"
	"github.com/luthersystems/elps/parser/token"
)

func init() {
	// Inject the test-only metadata fabricator for in-repo debugger tests.
	// The typed surface lives in internal/macroexp; the untyped slot in
	// internal/macroexp/hook exists only to break the import cycle
	// (macroexp needs lisp's types, so lisp cannot import macroexp).  This
	// is deliberately the ONLY way to attach macro-expansion metadata from
	// outside the in-kernel stamp (stampMacroExpansion below), and
	// internal/ visibility limits it to this module.
	macroexphook.Attach = func(v *LVal, name string, callSite, defSite *token.Location, args []*LVal, id int64) {
		//elps:mutates test-only fabrication of debug metadata via internal/macroexp; unreachable outside this module
		v.macroExpansion = &macroExpansionInfo{
			macroExpansionContext: &macroExpansionContext{
				CallSite: callSite,
				Name:     name,
				DefSite:  defSite,
				Args:     args,
			},
			ID: id,
		}
	}
}

//elpsvet:allow user-registered macro table; formals are sealed (see sealDefaultFormals init in builtins.go / RegisterDefaultMacro) and shared via registrationFormals (env.go AddMacros)
var userMacros []*langBuiltin

//elpsvet:allow default macro table; formals are sealed (see sealDefaultFormals init in builtins.go / RegisterDefaultMacro) and shared via registrationFormals (env.go AddMacros)
var langMacros = []*langBuiltin{
	{"defmacro", Formals("name", "formals", VarArgSymbol, "expr"), macroDefmacro,
		`Defines a named macro in the current package. The body receives
		unevaluated forms and must return a form to be evaluated at the
		call site. Use quasiquote/unquote to construct the expansion.
		An optional leading string in the body serves as a docstring.`},
	{"defun", Formals("name", "formals", VarArgSymbol, "expr"), macroDefun,
		`Defines a named function in the current package.`},
	{"deftype", Formals("name", "constructor-formals", VarArgSymbol, "constructor-exprs"), macroDeftype,
		`Defines a tagged type constructor bound to name in the current
		package. The formals and body define a constructor function that
		computes user data for new instances created with (new name ...).
		Returns the qualified type symbol.`},
	{"curry-function", Formals("fun", VarArgSymbol, "args"), macroCurryFun,
		`Returns a new function that calls fun with args prepended to any
		additional arguments supplied at call time. Equivalent to
		(lambda (&rest rest) (apply fun arg1 arg2 ... rest)).`},
	// get-default is a macro because we only want to evaluate the expression
	// bound to default if the key doesn't exist in the map.
	{"get-default", Formals("map", "key", "default"), macroGetDefault,
		`Looks up key in a sorted-map, returning the associated value if
		found. If the key is not present, evaluates and returns default.
		The default expression is only evaluated when the key is missing
		(lazy evaluation).`},
	{"trace", Formals("expr", OptArgSymbol, "message"), macroTrace,
		`Evaluates expr, prints the result to stderr prefixed by message
		(default "TRACE") using debug-print, then returns the result.
		The expression is evaluated exactly once. Useful for debugging
		without altering control flow.`},
	{"defconst", Formals("name", "value", VarArgSymbol, "docstring"), macroDefconst,
		`Defines an exported, documented constant. Binds value to name
		in the current package and exports it. Optional trailing strings
		set the documentation (concatenated; empty strings produce
		paragraph breaks). Equivalent to (set 'name value docs...)
		followed by (export 'name).`},
}

// macroRegistrationIDs assigns each builtin-macro REGISTRATION an identity
// that outlives the environment it is bound into, so the expansion cache can
// tell two implementations apart when they share a name (see funData.impl
// and macrocache.go).
//
// A definition that lives in one of this package's process-global tables —
// langMacros, and everything RegisterDefaultMacro appends to userMacros — is
// the SAME *langBuiltin in every environment that registers it, so it keeps
// one id and its expansions stay shareable between environments.  Any other
// LBuiltinDef reaching AddMacros is an environment-local definition whose
// implementation this package cannot compare, so it gets a fresh id per
// registration: the cache then treats each environment's binding as a
// distinct macro, which costs cross-environment reuse and never serves one
// implementation's expansion for another.
//
//elpsvet:allow registration identity table; keys are process-global registration defs, values are ints, no LVals
var macroRegistrationIDs = struct {
	ids map[*langBuiltin]uint64
	seq uint64
	mu  sync.Mutex
}{}

func macroRegistrationID(def LBuiltinDef) uint64 {
	macroRegistrationIDs.mu.Lock()
	defer macroRegistrationIDs.mu.Unlock()
	macroRegistrationIDs.seq++
	fresh := macroRegistrationIDs.seq
	lb, ok := def.(*langBuiltin)
	if !ok {
		return fresh
	}
	if id, ok := macroRegistrationIDs.ids[lb]; ok {
		return id
	}
	if macroRegistrationIDs.ids == nil {
		macroRegistrationIDs.ids = make(map[*langBuiltin]uint64)
	}
	macroRegistrationIDs.ids[lb] = fresh
	return fresh
}

// RegisterDefaultMacro adds the given function to the list returned by
// DefaultMacros.
func RegisterDefaultMacro(name string, formals *LVal, fn LBuiltin) {
	userMacros = append(userMacros, &langBuiltin{name, sealedFormalsCopy(formals), fn, ""})
}

// DefaultMacros returns the default set of LBuiltinDef added to LEnv objects
// when LEnv.AddMacros is called without arguments.
func DefaultMacros() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langMacros)+len(userMacros))
	for i := range langMacros {
		ops[i] = langMacros[i]
	}
	offset := len(langMacros)
	for i := range userMacros {
		ops[offset+i] = userMacros[i]
	}
	return ops
}

func macroDefmacro(env *LEnv, args *LVal) *LVal {
	sym, formals, body := args.Cells[0], args.Cells[1], args.Cells[2:]
	if sym.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", sym.Type)
	}
	fun := env.Lambda(formals, body)
	if fun.Type == LError {
		fun.SetCallStack(env.Runtime.Stack.Copy())
		return fun
	}
	fun.FunType = LFunMacro //elps:mutates evaluate as a macro: fun is the closure env.Lambda freshly allocated above
	return SExpr([]*LVal{
		Symbol("lisp:progn"),
		SExpr([]*LVal{
			Symbol("lisp:set"),
			Quote(sym),
			fun,
		}),
		Nil(),
	})
}

func macroDefun(env *LEnv, args *LVal) *LVal {
	sym, formals, body := args.Cells[0], args.Cells[1], args.Cells[2:]
	if sym.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", sym.Type)
	}
	fun := env.Lambda(formals, body)
	if fun.Type == LError {
		fun.SetCallStack(env.Runtime.Stack.Copy())
		return fun
	}
	return SExpr([]*LVal{
		Symbol("lisp:progn"),
		SExpr([]*LVal{
			Symbol("lisp:set"),
			Quote(sym),
			fun,
		}),
		Nil(),
	})
}

func macroDefconst(env *LEnv, args *LVal) *LVal {
	sym, value, docstrings := args.Cells[0], args.Cells[1], args.Cells[2:]
	if sym.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", sym.Type)
	}
	// Build: (progn (set 'name value docstrings...) (export 'name) nil)
	setCells := []*LVal{Symbol("lisp:set"), Quote(sym), value}
	setCells = append(setCells, docstrings...)
	return SExpr([]*LVal{
		Symbol("lisp:progn"),
		SExpr(setCells),
		SExpr([]*LVal{Symbol("lisp:export"), Quote(sym)}),
		Nil(),
	})
}

func macroCurryFun(env *LEnv, args *LVal) *LVal {
	funExpr, argExprs := args.Cells[0], args.Cells[1:]
	argsym := env.GenSym()
	callCells := []*LVal{
		Symbol("lisp:apply"),
		funExpr,
	}
	callCells = append(callCells, argExprs...)
	callCells = append(callCells, argsym)
	return SExpr([]*LVal{
		Symbol("lambda"),
		SExpr([]*LVal{
			Symbol(VarArgSymbol),
			argsym,
		}),
		SExpr(callCells),
	})
}

func macroGetDefault(env *LEnv, args *LVal) *LVal {
	mapExpr, keyExpr, defExpr := args.Cells[0], args.Cells[1], args.Cells[2]
	mapSym, keySym := env.GenSym(), env.GenSym()
	let := QExpr([]*LVal{
		Symbol("lisp:let"),
		SExpr([]*LVal{
			SExpr([]*LVal{
				mapSym,
				mapExpr,
			}),
			SExpr([]*LVal{
				keySym,
				keyExpr,
			}),
		}),
		SExpr([]*LVal{
			Symbol("lisp:if"),
			SExpr([]*LVal{
				Symbol("lisp:key?"),
				mapSym,
				keySym,
			}),
			SExpr([]*LVal{
				Symbol("lisp:get"),
				mapSym,
				keySym,
			}),
			defExpr,
		}),
	})
	return let
}

func macroDeftype(env *LEnv, args *LVal) *LVal {
	pkg := env.Runtime.Registry.Lang
	psymbol := func(s string) *LVal {
		return Symbol(fmt.Sprintf("%s:%s", pkg, s))
	}
	name := args.Cells[0]
	formals := args.Cells[1]
	exprs := SExpr(args.Cells[2:])
	if name.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", GetType(name))
	}
	if formals.Type != LSExpr {
		return env.Errorf("second argument is not a list: %v", GetType(formals))
	}
	fqname := env.GenSym()
	lambda := SExpr([]*LVal{
		psymbol("lambda"),
		formals,
	})
	lambda.Cells = append(lambda.Cells, exprs.Cells...)
	return QExpr([]*LVal{
		psymbol("let*"),
		SExpr([]*LVal{
			QExpr([]*LVal{
				fqname,
				SExpr([]*LVal{
					psymbol("qualified-symbol"),
					name,
				}),
			}),
		}),
		SExpr([]*LVal{
			psymbol("set"),
			fqname,
			SExpr([]*LVal{
				psymbol("new"),
				psymbol("typedef"),
				fqname,
				lambda,
			}),
		}),
		fqname,
	})
}

// stampMacroExpansion walks the expanded AST and replaces synthetic source
// locations (Pos < 0) with the macro call site. Nodes with valid source
// locations (from parser or unquote) are left unchanged.
//
// When ctx is non-nil (debugger attached), each stamped node also gets a
// macroExpansionInfo with a unique, monotonically-increasing ID. The
// runtime's sequence counter is used to generate IDs.
//
// Singleton values (Nil(), Bool(true), Bool(false)) are skipped via
// identity check — they are shared, immutable, pre-allocated values
// and mutating one corrupts every reader of that singleton for the
// remainder of the process lifetime. See issue #274.
func stampMacroExpansion(v *LVal, callSite *token.Location, ctx *macroExpansionContext, rt *Runtime) {
	if v == nil || callSite == nil {
		return
	}
	// Identity-based guard: a type-based check would catch only the empty-
	// LSExpr singletonNil and miss singletonTrue/singletonFalse (which are
	// LSymbol with Source.Pos == -1). See issue #274.
	if isSingleton(v) {
		return
	}
	// Sealed subtrees are parsed program nodes spliced into the expansion
	// (macros receive their arguments unevaluated, so argument expressions
	// arrive as shared parse-tree nodes).  They must not be stamped: the
	// same node may be under evaluation in every environment sharing the
	// parse, so the write below would be cross-environment visible — and a
	// data race under concurrent environments.  Most parser nodes carry a
	// real location (Pos >= 0) and were never stamped, but the parser CAN
	// emit synthetic Pos < 0 locations (a funref's lisp:function head
	// symbol, a #^ head symbol mirroring a location-less operand), so
	// without this guard the stamp is reachable on shared storage.  A
	// sealed node's descendants are all sealed; skip the whole subtree.
	if v.sealed {
		return
	}
	if v.source == nil || v.source.Pos < 0 {
		v.source = callSite //elps:mutates debug-metadata stamp on macro-expansion output; sealed (shared) subtrees are skipped above
		if ctx != nil {
			//elps:mutates debug-metadata stamp on macro-expansion output; sealed (shared) subtrees are skipped above
			v.macroExpansion = &macroExpansionInfo{
				macroExpansionContext: ctx,
				ID:                    rt.nextMacroExpID(),
			}
		}
	}
	for _, child := range v.Cells {
		stampMacroExpansion(child, callSite, ctx, rt)
	}
}

type unquoteType int

const (
	unquoteNone unquoteType = iota
	unquoteValue
	unquoteSpliced
)

func getUnquoteType(v *LVal) (unquoteType, error) {
	if v.Type != LSExpr {
		return unquoteNone, nil
	}
	if len(v.Cells) < 1 {
		return unquoteNone, nil
	}
	if v.Cells[0].Type != LSymbol {
		return unquoteNone, nil
	}
	if v.Cells[0].Str == "unquote" {
		if len(v.Cells) != 2 {
			return unquoteValue, fmt.Errorf("%s: one argument expected (got %d)", v.Cells[0].Str, len(v.Cells)-1)
		}
		return unquoteValue, nil
	}
	if v.Cells[0].Str == "unquote-splicing" {
		if len(v.Cells) != 2 {
			return unquoteSpliced, fmt.Errorf("%s: one argument expected (got %d)", v.Cells[0].Str, len(v.Cells)-1)
		}
		return unquoteSpliced, nil
	}
	return unquoteNone, nil
}

func findAndUnquote(env *LEnv, v *LVal, depth int) *LVal {
	inner := v
	quoteLevel := 0
	if inner.quoted {
		quoteLevel += 1
	}
	for inner.Type == LQuote {
		quoteLevel += 1
		inner = inner.Cells[0]
	}
	if inner.Type != LSExpr {
		// back out of the entire quote chain and return v to leave the value
		// unchanged in the quasiquote.
		return v
	}
	v = inner

	unquote, err := getUnquoteType(v)
	if err != nil {
		env.loc = v.source
		return env.Error(err)
	}
	if unquote == unquoteSpliced {
		// v looks like ``(unquote-splicing expr)''
		expr := v.Cells[1]
		if depth == 0 || quoteLevel > 0 {
			env.loc = v.source
			return env.Errorf("unquote-splicing used in an invalid context")
		}
		return doUnquoteSpliced(env, expr)
	}
	if unquote == unquoteValue {
		// v looks like ``(unquote expr)''
		return doUnquoteValue(env, v.Cells[1], quoteLevel)
	}
	return doUnquoteSExpr(env, v, depth, quoteLevel)
}

func doUnquoteSpliced(env *LEnv, v *LVal) *LVal {
	x := env.Eval(v)
	if x.Type == LError {
		return x
	}
	x = Splice(x)
	return x
}

func doUnquoteValue(env *LEnv, v *LVal, quoteLevel int) *LVal {
	x := env.Eval(v)
	if x.Type == LError {
		return x
	}
	for range quoteLevel {
		x = Quote(x)
	}
	return x
}

func doUnquoteSExpr(env *LEnv, v *LVal, depth int, quoteLevel int) *LVal {
	// findAndUnquote all child expressions
	numSpliced := 0
	numExtended := 0
	cells := make([]*LVal, v.Len())
	for i := range v.Cells {
		cells[i] = findAndUnquote(env, v.Cells[i], depth+1)
		if cells[i].Type == LError {
			return cells[i]
		}
		if cells[i].spliced {
			numSpliced += 1
			numExtended += len(cells[i].Cells)
		}
	}
	// splice in children of children that were unquoted with
	// ``unquote-splicing''
	if numSpliced > 0 {
		newlen := len(cells) - numSpliced + numExtended
		newcells := make([]*LVal, 0, newlen)
		for _, v := range cells {
			if v.spliced {
				if v.Type != LSExpr {
					// TODO:  I believe it is incorrect to error out here.  But
					// splicing non-lists is not a major concern at the moment.
					return env.Errorf("%s: cannot splice non-list: %s", "unquote-splicing", v.Type)
				}
				newcells = append(newcells, v.Cells...)
			} else {
				newcells = append(newcells, v)
			}
		}
		cells = newcells
	}
	expr := SExpr(cells)
	//elps:aliases deliberate in-runtime alias on the quasiquote hot path: v is the (sealed) quasiquote template node whose location was frozen at parse time, and the fresh expansion header mirrors it as display metadata — copying here would cost an allocation per quasiquote evaluation
	expr.source = v.source
	for range quoteLevel {
		expr = Quote(expr)
	}
	return expr
}

func macroTrace(env *LEnv, args *LVal) *LVal {
	expr, msg := args.ReqArg(env, 0), args.KeyArg(1)
	if expr.Type == LError {
		return expr
	}
	sym := env.GenSym()
	if msg.IsNil() {
		msg = String("TRACE")
	}
	return SExpr([]*LVal{
		Symbol("lisp:let"),
		SExpr([]*LVal{
			SExpr([]*LVal{sym, expr})},
		),
		SExpr([]*LVal{Symbol("lisp:debug-print"), msg, sym}),
		sym,
	})
}
