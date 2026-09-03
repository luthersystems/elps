// Copyright © 2018 The ELPS authors

package lisp

import (
	"fmt"

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
// A macro is free to build its expansion with assoc! or append!, so the value
// handed to stampMacroExpansion can contain itself, and an unguarded walk over
// one overflows the goroutine stack and kills the process.  The walk is
// bounded the same way rendering is; see lisp/cycle.go and issue #390.
//
// callSite is stored by POINTER on every node the walk claims, so the caller
// must pass a Location the expansion may own -- not one a live parse tree also
// holds.  macroCall takes env.loc.Copy() for exactly this reason; passing
// env.loc itself put the caller's node and the whole expansion on one mutable
// object (issue #431).
//
// ---------------------------------------------------------------------------
// DANGER: THIS STAMP WRITES INTO STORAGE IT MAY NOT OWN.  READ BEFORE EDITING.
//
// stampMacroExpansion is handed whatever the macro body returned, and it has
// no way to tell fresh expansion output from storage that also belongs to
// someone else.  The table below is the history of the shared storage this
// stamp has written into; each was found in production or by fuzzing, not by
// review.  Where each was FIXED varies -- three are guarded here, the other
// two were closed at the site that handed the stamp the shared storage:
//
//	#274  (May 2026)  the singletons Nil/true/false: one write corrupted
//	                  every reader for the rest of the process.  Guarded
//	                  here: isSingleton, in stampGuarded.
//	#396  (Aug 2026)  the form being expanded, aliased into the macro's
//	                  &rest list.  NOT guarded here -- fixed upstream in
//	                  macroArgList (lisp/builtins.go), which builds that
//	                  list over a fresh array.
//	#370  (Aug 2026)  reader nodes emitted with synthetic locations.
//	#517  (Aug 2026)  sealed parse-tree subtrees: a cross-environment write
//	                  and a data race under concurrent environments.
//	                  Both guarded here: the v.sealed check in stampGuarded.
//	#431  (Aug 2026)  the caller's env.loc, shared by pointer and then
//	                  stored on every node the walk claimed.  NOT guarded
//	                  here -- fixed at the call site, which passes
//	                  env.loc.Copy() (LEnv.macroCall).  callSite is still
//	                  stored BY POINTER, so the caller must keep passing a
//	                  Location the expansion may own.
//	(this fix)        VALUES yielded by the expansion -- a builtin reached
//	                  through Get, a global sorted map -- are live bindings;
//	                  stamping them in place moved lisp:car's definition
//	                  site onto a macro call site for the rest of the
//	                  process, and the profiler reported it as such.
//
// This function's own exclusions are therefore three: isSingleton (#274),
// sealed (#370/#517), and the nil-callSite early return.  The rule below
// REPLACES adding more of them; it is not a fourth.
//
// The rule, enforced by stampGuarded and pinned by
// TestMacroExpansionStampsValuesOnPrivateHeaders: the stamp writes IN PLACE
// only to SYNTAX nodes (the node types the reader produces, sealableNodeType)
// that are unsealed and not singletons.  A node of any other type is a
// VALUE.  A value is never written to; it is replaced, in its expansion-owned
// parent (or at the root, by the return value), with a private header copy
// that carries the stamp and shares the value's storage.  If a new kind of
// shared storage turns up, extend the ownership rule rather than adding
// another identity exclusion.
//
// BEHAVIOUR CHANGE, confined to error-location attribution -- results are
// unchanged.  A value a macro yields now keeps its OWN location instead of
// acquiring the macro call site.  Where that location was previously NONE the
// stack note reads "unknown", so the nodes compose, flip and the `expr`
// operator synthesize for the function they return are now located at their
// construction site (setSynthesizedSource, lisp/lisp.go) rather than relying
// on this stamp reaching inside a function value to locate its body.
//
// Known residual (issue #582): an UNSEALED syntax container that is itself a
// binding -- a global list built at runtime by (list ...) and returned by a
// macro body -- is indistinguishable from expansion output and is still
// stamped in place, on every release since the stamp existed.
// ---------------------------------------------------------------------------
//
// stampMacroExpansion returns the expansion the caller must evaluate: v
// itself, or, when v is a value, its stamped private header copy.
func stampMacroExpansion(v *LVal, callSite *token.Location, ctx *macroExpansionContext, rt *Runtime) *LVal {
	if v == nil || callSite == nil {
		return v
	}
	if isValueNode(v) {
		return stampValueCopy(v, callSite, ctx, rt)
	}
	var st cycleState
	stampGuarded(v, callSite, ctx, rt, cycleGuard{state: &st})
	if st.cyclic {
		// The walk above stopped as soon as it knew the expansion contains
		// itself, leaving part of it unstamped.  Stamping is idempotent -- a
		// node that already has a real source location is left alone -- so
		// the rerun, which visits each node once, finishes the job.
		stampGuarded(v, callSite, ctx, rt, strictCycleGuard())
	}
	return v
}

// isValueNode reports whether v is a runtime VALUE rather than SYNTAX: a
// node of a type the reader never produces (a function, a native handle, a
// sorted map, a vector, bytes).  Such a node inside a macro expansion is a
// binding the macro body evaluated to or spliced in, not the expansion's
// own output, so the stamp must not write to it.  See the warning above
// stampMacroExpansion.
func isValueNode(v *LVal) bool {
	return !sealableNodeType(v.Type)
}

// stampValueCopy returns the node to put in a value's place in an
// expansion: v itself when it already carries a real location, otherwise a
// private header copy of v carrying the stamp.
//
// WHAT THE COPY SHARES AND WHAT IT PRIVATIZES, precisely.  Everything
// reached through a POINTER is shared -- the *funData behind an LFun, the
// *MapData behind an LSortMap, the []byte behind LBytes, and the Cells
// BACKING ARRAY -- so the copy is the same value to every reader, and a
// write through either header would be seen through the other.  What is
// private is the copy's own struct: source and macroExpansion (the point of
// the exercise), and alongside them the Cells slice HEADER, Str, Int, Float,
// Type, FunType, quoted, spliced and sealed.  A write to one of those
// through the copy would silently diverge from the binding rather than
// corrupting it -- which would be a different bug, not a safe one.  No such
// write is reachable through this path today: every `.Cells =`, `.Native =`,
// `.Str =`, `.Int =` and `.Type =` assignment in lisp/ and lisp/lisplib/ was
// audited, and the ones that land on a VALUE type write either a container
// the builtin itself constructed (the vector rebuilds in select/reject) or a
// shared INNER cell (append!'s `vec.Cells[1].Cells`, which is the same
// storage through either header and so behaves identically).  The
// checker that keeps the shared half honest across runtimes keys an LFun on
// its *funData for exactly this reason (ownershipKey,
// lisp/ownership_check_elpscheck.go): a private header must not read as a
// different function.
//
// The arrangement is not new, but the cover it gave was accidental and
// partial.  LEnv.Get returns a FunRef HEADER COPY for an LFun, so a function
// the macro body reached through an UNQUALIFIED symbol arrived here as a
// copy already and the stamp's write landed harmlessly on it.  A QUALIFIED
// symbol -- lisp:car -- resolves through pkg.Get instead and returns the raw
// binding, which is why the bug above exists at all.  Here the copy is
// deliberate, unconditional, and extended to every value type.
func stampValueCopy(v *LVal, callSite *token.Location, ctx *macroExpansionContext, rt *Runtime) *LVal {
	if v.source != nil && v.source.Pos >= 0 {
		return v
	}
	cp := *v
	cp.source = callSite
	if ctx != nil {
		cp.macroExpansion = &macroExpansionInfo{
			macroExpansionContext: ctx,
			ID:                    rt.nextMacroExpID(),
		}
	}
	return &cp
}

func stampGuarded(v *LVal, callSite *token.Location, ctx *macroExpansionContext, rt *Runtime, g cycleGuard) {
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
	//
	// Checked before the cycle guard descends: a sealed subtree is skipped
	// whole, so there is nothing below it to bound.
	if v.sealed {
		return
	}
	// Values never reach this walk: the root is diverted by
	// stampMacroExpansion and children by the loop below.  Keep the guard
	// anyway -- it is the ownership rule's last line of defence.
	if isValueNode(v) {
		return
	}
	// Only a node with children is entered on the guard's path: a leaf stamps
	// itself and reaches nothing, and stamping runs on every macro expansion.
	nested := len(v.Cells) > 0
	if nested {
		if g.abandoned() {
			return
		}
		var cyclic bool
		g, cyclic = g.descend(v)
		if cyclic {
			return
		}
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
	for i, child := range v.Cells {
		if child == nil {
			continue
		}
		if isValueNode(child) {
			// v is expansion-owned unsealed syntax, so its cells are the
			// expansion's to rewrite; the value itself is not.
			if sc := stampValueCopy(child, callSite, ctx, rt); sc != child {
				v.Cells[i] = sc //elps:mutates debug-metadata stamp on macro-expansion output: replaces a spliced value with its stamped private header copy; the value is never written
			}
			continue
		}
		stampGuarded(child, callSite, ctx, rt, g)
	}
	if nested && g.tracking() {
		g.ascend(v)
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
