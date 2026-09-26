// Copyright © 2018 The ELPS authors

package lisp

import (
	"fmt"
	"math"

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
		An optional leading string in the body serves as a docstring.
		Malformed lambda lists and duplicate parameter names are errors at
		creation; see lambda for the parameter-list rules.
		The lisp package is sealed after initialization; defining a name
		there signals: cannot rebind lisp package binding: name. Unqualified
		builtin shadowing in your own package remains legal.`},
	{"defun", Formals("name", "formals", VarArgSymbol, "expr"), macroDefun,
		`Defines a named function in the current package.
		Malformed lambda lists and duplicate parameter names are errors at
		creation; see lambda for parameter-list and duplicate keyword rules.
		The lisp package is sealed after initialization; defining a name there signals:
		cannot rebind lisp package binding: name. Unqualified builtin
		shadowing in your own package remains legal.`},
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
		found. If the key is not present or map is nil, evaluates and
		returns default. String and symbol keys with the same name are
		interchangeable, including in JSON-decoded maps. The default
		expression is evaluated only when needed (lazy evaluation).`},
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
	// Stamp the definition name so LEnv.Get can return the binding itself
	// rather than a renamed copy on every lookup (see LEnv.Get).
	fun.Str = sym.Str //elps:mutates records the definition name: fun is the closure env.Lambda freshly allocated above
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
	// Stamp the definition name so LEnv.Get can return the binding itself
	// rather than a renamed copy on every lookup (see LEnv.Get).
	fun.Str = sym.Str //elps:mutates records the definition name: fun is the closure env.Lambda freshly allocated above
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
				Symbol("lisp:if"),
				SExpr([]*LVal{Symbol("lisp:nil?"), mapSym}),
				Symbol("lisp:false"),
				SExpr([]*LVal{
					Symbol("lisp:key?"),
					mapSym,
					keySym,
				}),
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

// stampMacroExpansion returns the expansion the caller must evaluate, with
// every node that has no real source location (Pos < 0) stamped with the
// macro call site.  Nodes with a real location (from the parser or from
// unquote) keep it.
//
// When ctx is non-nil (debugger attached), each stamped node also gets a
// macroExpansionInfo with a unique, monotonically-increasing ID. The
// runtime's sequence counter is used to generate IDs.
//
// THE STAMP NEVER WRITES TO THE VALUE IT IS HANDED.  It is copy-on-write: a
// node that needs a stamp, or whose cells changed, is replaced by a private
// copy in the returned tree; everything else is shared.  See the warning
// below for why, and what it cost to learn.
//
// A macro is free to build its expansion with assoc! or append!, so the value
// handed to stampMacroExpansion can contain itself, and an unguarded walk over
// one overflows the goroutine stack and kills the process.  The walk is
// bounded the same way rendering is; see lisp/cycle.go and issue #390.
//
// callSite is stored by POINTER on every node the walk stamps, so the caller
// must pass a Location the expansion may own -- not one a live parse tree also
// holds.  macroCall takes env.loc.Copy() for exactly this reason; passing
// env.loc itself put the caller's node and the whole expansion on one mutable
// object (issue #431).
//
// ---------------------------------------------------------------------------
// DANGER: THIS STAMP IS HANDED STORAGE IT DOES NOT OWN.  READ BEFORE EDITING.
//
// stampMacroExpansion is handed whatever the macro body returned, and it has
// no way to tell fresh expansion output from storage that also belongs to
// someone else.  For most of its life it wrote the call site IN PLACE, and
// the table below is the history of the shared storage it wrote into; each
// was found in production or by fuzzing, not by review.  Where each was
// FIXED varies -- three were guarded here, two were closed at the site that
// handed the stamp the shared storage, and the last two closed the class:
//
//	#274  (May 2026)  the singletons Nil/true/false: one write corrupted
//	                  every reader for the rest of the process.  Guarded
//	                  here: isSingleton.
//	#396  (Aug 2026)  the form being expanded, aliased into the macro's
//	                  &rest list.  NOT guarded here -- fixed upstream in
//	                  macroArgList (lisp/builtins.go), which builds that
//	                  list over a fresh array.
//	#370  (Aug 2026)  reader nodes emitted with synthetic locations.
//	#517  (Aug 2026)  sealed parse-tree subtrees: a cross-environment write
//	                  and a data race under concurrent environments.
//	                  Both guarded here: the v.sealed check.
//	#431  (Aug 2026)  the caller's env.loc, shared by pointer and then
//	                  stored on every node the walk claimed.  NOT guarded
//	                  here -- fixed at the call site, which passes
//	                  env.loc.Copy() (LEnv.macroCall).  callSite is still
//	                  stored BY POINTER, so the caller must keep passing a
//	                  Location the expansion may own.
//	(value fix)       VALUES yielded by the expansion -- a builtin reached
//	                  through Get, a global sorted map -- are live bindings;
//	                  stamping them in place moved lisp:car's definition
//	                  site onto a macro call site for the rest of the
//	                  process, and the profiler reported it as such.
//	                  Closed by stamping a private header copy instead.
//	#582  (Sep 2026)  unsealed SYNTAX that is itself a binding -- a global
//	                  list built by (list ...) and returned by a LISP macro
//	                  body, or a located list holding an unlocated value --
//	                  is indistinguishable from expansion output.  The value
//	                  fix still wrote into it, in place: its cells were
//	                  overwritten with private copies of their values.
//	                  Closed by making the whole stamp copy-on-write.
//
// The rule, enforced by macroStamper and pinned by
// TestMacroExpansionStampNeverWritesTheExpansion: THE STAMP WRITES ONLY TO
// NODES IT ALLOCATED.  A node that needs a stamp, or whose cells changed, is
// replaced in the returned tree by a private copy; a node that needs neither
// is shared, as are sealed subtrees, singletons and located values.  The copy
// shares everything behind a pointer (the *funData behind an LFun, the
// *MapData behind an LSortMap, the []byte behind LBytes, an unchanged Cells
// backing array); its own struct -- source, macroExpansion and, when a cell
// changed, the cell slice -- is private.  Nothing is stamped in place, so
// there is no identity exclusion left to add: if a new kind of shared
// storage turns up, it is already not written to.
//
// The function's remaining guards are three, and all are about SHARING, not
// about writing: isSingleton (#274) and sealed (#370/#517) share the node
// outright instead of walking it, and the nil-callSite early return stamps
// nothing.
//
// COST, AND WHO PAYS IT.  An expansion in which nothing needs a stamp is
// returned as is, with no allocation
// (TestMacroExpansionStampAllocatesNothingWhenNothingNeedsAStamp).  A node
// that needs a stamp costs one header, plus a header and a cell slice for
// each ancestor up to the root, which is the storage the in-place stamp
// used to take from whoever owned the node.  A CYCLIC expansion
// (constructible from Go only) additionally pays for the walk that
// discovers the cycle before the memoised rerun.
//
// Only a LISP macro pays.  Its body builds its expansion with quasiquote,
// whose output carries the template's locations, so the usual lisp
// expansion needs no stamp at all; and an unlocated node it does return may
// be a binding (#582), which is exactly what the copy is for.  A GO macro
// synthesizes its expansion from fresh nodes, so the copies would cost it
// two allocations per node on every expansion -- measured at +23% allocs/op
// on a benchmark that expands libtesting's assert-equal in a loop, and the
// same shape is substrate's cc:infof on every phylum log line.  So
// LEnv.macroCall, not under a debugger, LOCATES A GO MACRO'S EXPANSION IN
// PLACE before the stamp sees it (locateExpansionTree, below): every
// unlocated unsealed syntax node it reaches gets the call site, stopping
// at the macro's arguments, and the stamp then shares the fresh nodes (an
// unlocated value or argument in the expansion, or a cycle, still costs
// the copies the stamp's rules give).  That is the in-place write this
// stamp gave up, confined to the one caller whose output is fresh BY
// CONTRACT: a Go macro's expansion consists of nodes it constructed and
// its arguments (see LEnv.AddMacros).  The arguments are the caller's
// nodes and may be bindings -- a runtime-built call form hands the macro
// raw bindings, through macroexpand-1 or a Go-side Eval -- which is why
// the locate stops at them.  A Go macro that returned a binding it looked
// up ITSELF, an unlocated runtime list from env.Get, would have that
// binding located, the #582 shape; none in this repository or in
// substrate does, and the contract is the rule against it.  Under a
// debugger the hand-off is skipped and the stamp copies a Go macro's
// expansion too, since the copy is where the expansion metadata the
// debugger reads is attached.
//
// BEHAVIOUR CHANGE, confined to error-location attribution -- results are
// unchanged.  A value a macro yields keeps its OWN location instead of
// acquiring the macro call site.  Where that location was previously NONE the
// stack note reads "unknown", so the nodes compose, flip and the `expr`
// operator synthesize for the function they return are located at their
// construction site (setSynthesizedSource, lisp/lisp.go) rather than relying
// on this stamp reaching inside a function value to locate its body.  A
// binding a macro body returns keeps its own (absent) location too; the
// stamp lands on the copy the caller evaluates, so an error raised while
// evaluating the expansion still reports the macro call site.
// ---------------------------------------------------------------------------
func stampMacroExpansion(v *LVal, callSite *token.Location, ctx *macroExpansionContext, rt *Runtime) *LVal {
	if v == nil || callSite == nil {
		return v
	}
	s := macroStamper{callSite: callSite, ctx: ctx, rt: rt}
	if ctx != nil {
		s.nextID, s.firstID = rt.macroExpSeq, rt.macroExpSeq
	}
	if isValueNode(v) {
		got := s.value(v)
		s.commitIDs()
		return got
	}
	var st cycleState
	got, fail := s.syntax(v, cycleGuard{state: &st})
	if fail != nil {
		return fail
	}
	if st.cyclic {
		// The walk above stopped as soon as it knew the expansion contains
		// itself, and whatever it built on the way is discarded -- copies
		// and expansion IDs alike, which is why the IDs are minted on the
		// stamper and only committed below.  The rerun visits each node
		// once and memoises its copies, so a back-edge lands on the copy
		// and the returned tree contains itself exactly where the
		// expansion did.
		s.copies = make(map[*LVal]stampMemo)
		s.visits = 0
		s.nextID = s.firstID
		got, fail = s.syntax(v, strictCycleGuard())
		if fail != nil {
			return fail
		}
	}
	s.commitIDs()
	return got
}

// locateExpansionTree writes callSite, in place, onto every unlocated
// unsealed syntax node reachable from v -- the expansion a Go macro has
// just returned, which is fresh by contract (see the warning above
// stampMacroExpansion, "who pays", and LEnv.AddMacros) -- stopping at the
// macro's arguments.  stampMacroExpansion then finds the fresh nodes
// located and shares them, so the usual Go macro expansion costs no
// copies; an unlocated value in it, an unlocated argument, or a cycle
// still costs the copies the stamp's own rules give.
//
// What is NOT written to, and why:
//
//   - The macro's ARGUMENTS (args), and everything under them.  They are the
//     caller's nodes, never the macro's: sealed reader nodes when the call
//     form was parsed, located copies when it came out of an enclosing
//     lisp macro's stamp -- and raw bindings when it was built at runtime
//     ((macroexpand-1 (list 'm l)), or a Go-side Eval of a runtime form).
//     That last one is the #582 shape arriving through an argument; the
//     walk stops at the argument and the stamp copies it, exactly as it
//     copies a binding a lisp macro returns.  The boundary is only built
//     when some argument is unsealed and unlocated, so a parsed call form
//     pays nothing for it.
//   - VALUES: a value in a Go macro's expansion is a binding it spliced in;
//     the stamp copies its header.
//   - Sealed subtrees and singletons, for the reasons the stamp's own walk
//     gives.  A node that already carries a real location keeps it.
//
// The walk is bounded like the stamp's (lisp/cycle.go): a Go macro can
// return a tree that contains itself.  A node is located BEFORE its cells
// are walked, and locating is idempotent, so the strict rerun over a cyclic
// tree finishes what the abandoned walk started.
func locateExpansionTree(v *LVal, callSite *token.Location, args []*LVal) {
	if v == nil || callSite == nil || isValueNode(v) {
		return
	}
	var boundary map[*LVal]struct{}
	for _, a := range args {
		if a == nil || a.sealed || isSingleton(a) || isValueNode(a) || !needsStamp(a) {
			continue
		}
		if boundary == nil {
			boundary = make(map[*LVal]struct{}, len(args))
		}
		boundary[a] = struct{}{}
	}
	var st cycleState
	locateGuarded(v, callSite, boundary, cycleGuard{state: &st})
	if st.cyclic {
		locateGuarded(v, callSite, boundary, strictCycleGuard())
	}
}

func locateGuarded(v *LVal, callSite *token.Location, boundary map[*LVal]struct{}, _ cycleGuard) {
	if v == nil || isSingleton(v) || v.sealed || isValueNode(v) {
		return
	}
	if _, arg := boundary[v]; arg {
		return
	}
	if len(v.Cells) == 0 {
		if needsStamp(v) {
			v.source = callSite //elps:mutates locates fresh Go macro syntax before publication
		}
		return
	}
	locateContainer(v, callSite, boundary)
}

func locateContainer(v *LVal, callSite *token.Location, boundary map[*LVal]struct{}) {
	var buf [16][]*LVal // stack-resident until an expansion nests deeper than 16
	pending := buf[:0]
	seen := make(map[*LVal]bool)
	for {
		if v != nil && !isSingleton(v) && !v.sealed && !isValueNode(v) {
			if _, arg := boundary[v]; !arg {
				if needsStamp(v) {
					v.source = callSite //elps:mutates locates fresh Go macro syntax before publication
				}
				// Leaves cannot cycle or schedule work. Only containers need
				// a memo and a by-value cursor for their remaining children.
				if len(v.Cells) > 0 && !seen[v] {
					seen[v] = true
					pending = append(pending, v.Cells)
				}
			}
		}
		for len(pending) > 0 && len(pending[len(pending)-1]) == 0 {
			pending[len(pending)-1] = nil
			pending = pending[:len(pending)-1]
		}
		if len(pending) == 0 {
			return
		}
		f := &pending[len(pending)-1]
		i := len(*f) - 1
		v, *f = (*f)[i], (*f)[:i]
	}
}

// isValueNode reports whether v is a runtime VALUE rather than SYNTAX: a
// node of a type the reader never produces (a function, a native handle, a
// sorted map, a vector, bytes).  Such a node inside a macro expansion is a
// binding the macro body evaluated to or spliced in, not the expansion's
// own output.  See the warning above stampMacroExpansion.
func isValueNode(v *LVal) bool {
	return !sealableNodeType(v.Type)
}

// needsStamp reports whether v carries no real source location.
func needsStamp(v *LVal) bool {
	return v.source == nil || v.source.Pos < 0
}

// macroStamper carries one stamp's parameters down the copy-on-write walk.
type macroStamper struct {
	callSite *token.Location
	ctx      *macroExpansionContext
	rt       *Runtime
	// copies memoises containers by identity, in one of two modes told
	// apart by the guard's strict flag.
	//
	// In the strict rerun over a cyclic expansion, a nested node maps to its
	// copy BEFORE its cells are walked, so the walk's second arrival at it
	// -- through the cycle -- lands on the copy.
	//
	// On the ordinary walk it is nil until the walk has done
	// sharedWalkBudget work, counted in visits below (lisp/sharing.go).  From then on every
	// container the walk FINISHES maps to its result -- the node itself when
	// nothing under it changed -- and the container's height, so a
	// container reached again through sharing (a DAG, not a cycle) is
	// answered from the memo rather than walked once per path.  A tree
	// never hits it, so its result is exactly the tree walk's.
	copies map[*LVal]stampMemo
	// visits counts the work the ordinary walk has done -- one per
	// container entered plus one per cell it holds.  Cells, not just
	// containers: a wide container revisited through sharing costs its
	// width every time.  Past sharedWalkBudget the memo above switches on,
	// and records a finished container only if its own walk cost at least
	// sharedMemoGrain.
	visits int
	// nextID is the expansion-ID counter for the walk in progress, seeded
	// from and committed back to Runtime.macroExpSeq (commitIDs) only for
	// the walk whose result is kept.  An abandoned walk over a cyclic
	// expansion mints IDs for copies it throws away; minting them here
	// rather than on the Runtime keeps the kept walk's IDs contiguous and
	// in pre-order, as the in-place stamp assigned them.  firstID is the
	// seed, so an abandoned walk can be rewound and a walk that minted
	// nothing can leave the Runtime untouched.
	nextID, firstID int64
}

// stampMemo is one container's entry in macroStamper.copies.
type stampMemo struct {
	// cp is the container's stamped counterpart.
	cp *LVal
	// height is the number of container levels the walk descended below
	// the container: 0 when none of its cells is a walked container.  A
	// memo hit at depth d must fail exactly where re-walking the subtree
	// would, which is when d+height reaches the value depth limit.  Unused
	// by the strict (cyclic) mode.
	height int
}

// commitIDs publishes the IDs the kept walk minted to the Runtime.  A walk
// that minted none -- a singleton or a fully located expansion -- writes
// nothing: the stamp is documented as a no-op on those, and the singleton
// race test exercises it from many goroutines on one Runtime.
func (s *macroStamper) commitIDs() {
	if s.ctx != nil && s.nextID != s.firstID {
		s.rt.macroExpSeq = s.nextID
	}
}

// stampedCopy returns a private header copy of v carrying the call site
// and, under a debugger, the expansion metadata.  v is never written to.
func (s *macroStamper) stampedCopy(v *LVal) *LVal {
	cp := *v
	cp.source = s.callSite //elps:aliases by design: callSite is the expansion-owned copy macroCall took (env.loc.Copy(), issue #431), shared by every node of one expansion
	if s.ctx != nil {
		s.nextID++
		cp.macroExpansion = &macroExpansionInfo{
			macroExpansionContext: s.ctx,
			ID:                    s.nextID,
		}
	}
	return &cp
}

// value returns the node to put in a value's place in an expansion: v itself
// when it already carries a real location, otherwise a private header copy
// of v carrying the stamp.
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
// binding, which is why the value bug existed at all.  Here the copy is
// deliberate, unconditional, and extended to every value type.
func (s *macroStamper) value(v *LVal) *LVal {
	if !needsStamp(v) {
		return v
	}
	return s.stampedCopy(v)
}

// syntax returns the stamped counterpart of the syntax node v: v itself when
// neither v nor anything under it needs a stamp, otherwise a private copy of
// v -- its own header and, when any cell changed, its own cell slice --
// carrying the stamp and pointing at the stamped counterparts of v's cells.
// It never writes to v or to anything reachable from v.
// A second result reports the walk's own FAILURE -- today only the value
// depth limit.  It cannot be carried in the first: an error is an ordinary
// first-class VALUE in ELPS, so a Go macro may splice one into its expansion
// as data, and sniffing the converted node's type for LError would truncate
// such an expansion at that node.
func (s *macroStamper) syntax(v *LVal, g cycleGuard) (*LVal, *LVal) {
	if v == nil || isSingleton(v) || v.sealed {
		return v, nil
	}
	if isValueNode(v) || len(v.Cells) == 0 {
		return s.value(v), nil
	}
	return s.syntaxContainer(v, g)
}

func (s *macroStamper) syntaxContainer(v *LVal, g cycleGuard) (*LVal, *LVal) {
	type frame struct {
		v, cp  *LVal
		cells  []*LVal
		i      int
		height int // container levels below v found so far
		start  int // the walk's work when it entered v
	}
	var buf [16]frame // stack-resident until a stamp walks deeper than 16
	stack := buf[:0]
	// The depth limit is read once for the whole walk rather than once per
	// container.  The boundary that makes that safe is the walk itself: a
	// stamp runs AFTER the macro body has returned, over a finished
	// expansion, and every function it calls -- isSingleton, isValueNode,
	// needsStamp, s.value/stampedCopy, cycleGuard.descend/ascend -- is pure
	// kernel code.  No Lisp evaluation, no native clone hook and no embedder
	// callback runs between this read and its uses, so nothing can adjust
	// the runtime's limit mid-walk.
	depthLimit := s.rt.ValueDepthLimit()
walk:
	for {
		var result *LVal
		// height is result's stampMemo.height when result is the
		// counterpart of a container the walk descended into (or answered
		// from the memo), and -1 for anything the walk did not descend
		// into: a leaf, a value, a singleton, a sealed subtree.
		height := -1
		switch {
		case v == nil || isSingleton(v) || v.sealed:
			result = v
		case isValueNode(v) || len(v.Cells) == 0:
			result = s.value(v)
		case g.abandoned():
			return v, nil
		default:
			depth := g.depth + len(stack)
			if m, ok := s.copies[v]; ok {
				if g.strict {
					result = m.cp
					break
				}
				// A finished container reached again through sharing.
				// Re-walking it would visit containers down to
				// depth+m.height and fail at the first one past the
				// limit; nothing else about the walk depends on where
				// the container is reached.
				if depth+m.height >= depthLimit {
					return nil, Error(ValueDepthError(depthLimit))
				}
				result, height = m.cp, m.height
				break
			}
			if depth >= depthLimit {
				return nil, Error(ValueDepthError(depthLimit))
			}
			_, cyclic := (cycleGuard{state: g.state, depth: depth, strict: g.strict}).descend(v)
			if cyclic {
				return v, nil // The caller discards this pass and restarts in strict mode.
			}
			f := frame{v: v, start: s.visits}
			if needsStamp(v) {
				f.cp = s.stampedCopy(v)
			}
			if g.strict && s.copies != nil {
				if f.cp == nil {
					cp := new(LVal)
					*cp = *v
					f.cp = cp
				}
				f.cells = make([]*LVal, len(v.Cells))
				f.cp.Cells = f.cells //elps:mutates private header allocated above or by stampedCopy before publication
				s.copies[v] = stampMemo{cp: f.cp}
			} else {
				s.visits += 1 + len(v.Cells)
				if s.copies == nil && s.visits > sharedWalkBudget {
					// Large enough to be a sharing bomb rather than an
					// expansion anyone wrote (lisp/sharing.go): memoise
					// the containers finished from here on.
					s.copies = make(map[*LVal]stampMemo)
				}
			}
			stack = append(stack, f)
			v = v.Cells[0]
			continue
		}
		for len(stack) > 0 {
			f := &stack[len(stack)-1]
			if height >= f.height {
				f.height = height + 1
			}
			if f.cells != nil {
				f.cells[f.i] = result
			} else if result != f.v.Cells[f.i] {
				f.cells = make([]*LVal, len(f.v.Cells))
				copy(f.cells, f.v.Cells[:f.i])
				f.cells[f.i] = result
			}
			f.i++
			if f.i < len(f.v.Cells) {
				v = f.v.Cells[f.i]
				continue walk
			}
			if !g.strict && g.depth+len(stack) >= cycleGuardDepth {
				g.ascend(f.v)
			}
			if f.cp == nil && f.cells == nil {
				result = f.v
			} else {
				cp := f.cp
				if cp == nil {
					cp = new(LVal)
					*cp = *f.v
				}
				if f.cells != nil {
					cp.Cells = f.cells
				} //elps:mutates private copy-on-write macro header allocated above or by stampedCopy
				result = cp
			}
			height = f.height
			if !g.strict && s.copies != nil && s.visits-f.start >= sharedMemoGrain {
				s.copies[f.v] = stampMemo{cp: result, height: height}
			}
			*f = frame{}
			stack = stack[:len(stack)-1]
		}
		return result, nil
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

// quasiquoteRebuildAllowance is how much duplicated work -- quote wrappers
// unwrapped again and cells rebuilt on re-entering a shared impure list --
// one quasiquote may do before it is charged in steps (see findAndUnquote).
// A million units is far past what a template that merely repeats forms
// does (a thousand-cell impure form repeated a hundred times re-enters a
// tenth of it), so such a template keeps its exact step count; and it bounds the uncharged work, and the
// allocation that comes with it (an unquote's value is re-quoted once per
// wrapper), to the order of what one builtin step may already do.  It is
// not tied to MaxAlloc, which counts elements of one allocation and which an
// embedder may raise freely.
const quasiquoteRebuildAllowance = 1 << 20

// quasiquoteMemo is one pure container's entry in findAndUnquote's
// sharing memo (see there).
type quasiquoteMemo struct {
	// result is what the walk built for the container.
	result *LVal
	// need is how far below the container's own value depth the walk
	// checked the value depth limit: a hit at value depth d fails exactly
	// where re-walking would, when d+need reaches the limit.
	need int
	// allocLimit and depthLimit are the limits the entry was built under.
	// An unquoted expression elsewhere in the template may change them,
	// and an entry built under other limits is not reused.
	allocLimit, depthLimit int
}

// findAndUnquote rebuilds the quasiquote template v, evaluating its unquoted
// expressions.
//
// SHARING.  A template is a graph, and one built at runtime -- (eval (list
// 'quasiquote x)), or a Go macro's output -- can share a subtree between
// many parents; the D-level chain (set! x (list x x)) has 2^D paths.  The
// walk counts its work (lists entered and the cells they hold) and, past
// sharedWalkBudget (lisp/sharing.go), memoises every PURE list it finishes
// -- one with no unquote anywhere below it -- by identity, so a pure
// subtree reached again is not rebuilt once per path.  Only pure lists: the
// result of a pure list is a function of the list alone (and of the two
// limits, which the entry records), while a list holding an unquote must
// evaluate it once per occurrence, as it always has.
//
// Work counts quote wrappers as well as lists and cells: prepareUnquote
// unwraps a node's LQuote chain on every visit, and a program can build an
// L-deep chain in L steps.  A pure node is memoised keyed on the node as
// reached (its outermost wrapper), and a pure list also keyed on the list
// itself, so the same list behind a fresh wrapper -- (quasiquote '(unquote
// big)) makes a new one on every call -- shares its rebuild and pays only
// for its own quoting.
//
// Two kinds of work are DUPLICATED and cannot be memoised away, and those
// are what the unquotes' own steps do not pay for.  An impure list -- one
// holding an unquote -- must be rebuilt, its unquotes evaluated, every time
// it is reached: a list of width k shared along 2^D paths costs one step per
// occurrence but k cells of work and allocation.  And chains of quote
// wrappers that share a suffix -- the prefixes of one chain, each reached as
// its own node -- are unwrapped again down to the shared part, and an
// unquoted value under them is re-quoted as deep.  So past the budget the
// walk records the impure lists it finishes (keyed on the list) and the
// wrappers it unwraps, and totals the duplicated work: the cells of an
// impure list rebuilt again, and the wrappers unwrapped again.  That work is
// free up to quasiquoteRebuildAllowance units, and beyond it is charged one
// step per unit (LEnv.ChargeSteps, which also observes the context), so the
// step budget bounds it rather than a memo.
//
// A tree never revisits a list: its result, errors and step count are
// exactly the tree walk's (the memos only switch on).  A DAG past the
// budget gets one shared result per shared pure list where the tree walk
// built one copy per path; only a Go embedder comparing pointers can see
// that.  Steps change only once the duplicated work in one quasiquote passes
// the allowance above -- work the tree walk did without charging for it at
// all.
func findAndUnquote(env *LEnv, v *LVal, depth int) *LVal {
	type frame struct {
		v, orig                             *LVal
		cells                               []*LVal
		depth, valueDepth, quotes, i, total int
		// quoteEdges is the number of quote wrappers between orig and v;
		// need is how far below orig's value depth the walk has checked
		// the limit so far (see quasiquoteMemo.need).
		quoteEdges, need int
		splices          bool
		// impure reports that an unquote was evaluated below v, so its
		// result may not be memoised.
		impure bool
		// start is the walk's work when it entered v.
		start int
	}
	// The cursor stack lives on the Go stack for the depths a real template
	// reaches; append spills to the heap only past that.  Growing a nil
	// slice cost a heap allocation per doubling on EVERY quasiquote
	// evaluation, which is per macro call (substrate#504).
	var buf [8]frame
	stack := buf[:0]
	valueDepth := depth
	// memo is nil until the walk has done sharedWalkBudget work.  From then
	// on memo holds each pure node's result keyed on the node as reached
	// (outermost wrapper included), core each pure list's unquoted rebuild
	// keyed on the list itself (so a fresh wrapper around it still hits),
	// impure the lists holding an unquote, keyed on the list, and seen the
	// quote wrappers unwrapped.  rebuilt totals the duplicated work --
	// wrappers unwrapped again, impure lists rebuilt again -- charged past
	// quasiquoteRebuildAllowance.
	var memo, core map[*LVal]quasiquoteMemo
	var impure, seen map[*LVal]struct{}
	work, rebuilt := 0, 0
	for {
		var (
			result, list        *LVal
			quotes, quoteEdges  int
			evaluated, memoised bool
			need, start         int
		)
		if memo != nil {
			if e, ok := memo[v]; ok && e.allocLimit == env.Runtime.MaxAllocBytes() && e.depthLimit == env.Runtime.ValueDepthLimit() {
				if valueDepth+e.need >= e.depthLimit {
					return env.Error(ValueDepthError(e.depthLimit))
				}
				result, need, memoised = e.result, e.need, true
			}
		}
		if !memoised {
			var dup int
			result, list, quotes, quoteEdges, dup, evaluated = prepareUnquote(env, v, depth, valueDepth, seen)
			need = quoteEdges
			// The work this visit did: the quote wrappers it unwrapped, and
			// a list's cells.  start is the work before it, for the grain.
			start = work
			work += quoteEdges
			if list != nil && len(list.Cells) > 0 {
				work += 1 + len(list.Cells)
			}
			if memo == nil && work > sharedWalkBudget {
				memo = make(map[*LVal]quasiquoteMemo)
				core = make(map[*LVal]quasiquoteMemo)
				impure = make(map[*LVal]struct{})
				seen = make(map[*LVal]struct{})
			}
			if memo != nil && (list != nil || result.Type != LError) {
				// Wrappers unwrapped again -- a chain sharing a suffix with
				// one walked before -- are duplicated work.
				if dup > 0 {
					if lerr := chargeDuplicated(env, &rebuilt, dup); lerr != nil {
						return lerr
					}
				}
				switch {
				case list == nil && !evaluated && quoteEdges > 0:
					// A leaf behind quote wrappers: pure, and as costly to
					// reach again as its wrappers are deep.
					memo[v] = quasiquoteMemo{result: result, need: quoteEdges, allocLimit: env.Runtime.MaxAllocBytes(), depthLimit: env.Runtime.ValueDepthLimit()}
				case list != nil && len(list.Cells) > 0:
					if e, ok := core[list]; ok && quoteEdges > 0 && e.allocLimit == env.Runtime.MaxAllocBytes() && e.depthLimit == env.Runtime.ValueDepthLimit() {
						// A pure list reached again behind other wrappers:
						// its rebuild is shared, only the quoting is new.
						if valueDepth+quoteEdges+e.need >= e.depthLimit {
							return env.Error(ValueDepthError(e.depthLimit))
						}
						result, need, list = requote(e.result, quotes), quoteEdges+e.need, nil
					} else if _, again := impure[list]; again {
						// A shared impure list, rebuilt once more.
						if lerr := chargeDuplicated(env, &rebuilt, 1+len(list.Cells)); lerr != nil {
							return lerr
						}
					}
				}
			}
		}
		if list != nil {
			f := frame{v: list, orig: v, cells: make([]*LVal, len(list.Cells)), depth: depth, valueDepth: valueDepth + quoteEdges, quotes: quotes, quoteEdges: quoteEdges, need: quoteEdges, start: start}
			if len(list.Cells) > 0 {
				stack = append(stack, f)
				v = list.Cells[0]
				depth++
				valueDepth = f.valueDepth + 1
				continue
			}
			result = finishUnquote(list, f.cells, quotes, false, 0)
			if memo != nil && quoteEdges > 0 {
				// An empty list behind quote wrappers: pure, like a leaf.
				memo[v] = quasiquoteMemo{result: result, need: quoteEdges, allocLimit: env.Runtime.MaxAllocBytes(), depthLimit: env.Runtime.ValueDepthLimit()}
			}
		}
		// Read the allocation cap once per unquote step rather than once per
		// frame the loop below unwinds.  The boundary is that loop: it only
		// fills cells and calls finishUnquote, both pure, and every path that
		// can evaluate user code leaves it first (the `break` returns to the
		// outer loop, which calls prepareUnquote again and re-reads).
		allocLimit := env.Runtime.MaxAllocBytes()
		for {
			if result.Type == LError {
				return result
			}
			if len(stack) == 0 {
				return result
			}
			f := &stack[len(stack)-1]
			f.cells[f.i] = result
			f.impure = f.impure || evaluated
			if n := f.quoteEdges + 1 + need; n > f.need {
				f.need = n
			}
			added := 1
			if result.spliced {
				if result.Type != LSExpr {
					return env.Errorf("unquote-splicing: cannot splice non-list: %s", result.Type)
				}
				f.splices = true
				added = len(result.Cells)
			}
			if added > allocLimit-f.total {
				if added > math.MaxInt-f.total {
					return env.Errorf("allocation size exceeds maximum (%d): element count overflows int", allocLimit)
				}
				return env.Errorf("allocation size %d exceeds maximum (%d)", f.total+added, allocLimit)
			}
			f.total += added
			f.i++
			if f.i < len(f.cells) {
				v = f.v.Cells[f.i]
				depth = f.depth + 1
				valueDepth = f.valueDepth + 1
				break
			}
			rebuiltList := finishUnquote(f.v, f.cells, 0, f.splices, f.total)
			result = requote(rebuiltList, f.quotes)
			evaluated, need = f.impure, f.need
			if memo != nil {
				if f.impure {
					impure[f.v] = struct{}{}
				} else if work-f.start >= sharedMemoGrain {
					depthLimit := env.Runtime.ValueDepthLimit()
					memo[f.orig] = quasiquoteMemo{result: result, need: need, allocLimit: allocLimit, depthLimit: depthLimit}
					if f.quoteEdges > 0 {
						// Reached behind wrappers: also keyed on the list,
						// for the same list behind other wrappers.
						core[f.v] = quasiquoteMemo{result: rebuiltList, need: need - f.quoteEdges, allocLimit: allocLimit, depthLimit: depthLimit}
					}
				}
			}
			stack = stack[:len(stack)-1]
		}
	}
}

// prepareUnquote examines one node of a quasiquote template.
//
// result is the node's value when it needs no walk: the node itself (a
// leaf), an unquoted expression's value, or an error.  list is the list to
// walk when the node is one, behind its wrappers; quotes is the quote level
// to restore on its rebuilt copy.  quoteEdges is the number of LQuote
// wrappers unwrapped to reach the list, leaf or unquote, except on an
// error.  seen, when non-nil, is the set of wrappers the walk has unwrapped
// before; dup counts those among this node's.  evaluated reports that
// result is the value of an unquoted expression.
func prepareUnquote(env *LEnv, v *LVal, depth, valueDepth int, seen map[*LVal]struct{}) (result, list *LVal, quotes, quoteEdges, dup int, evaluated bool) {
	// Read the depth limit once per call.  The boundary is this function:
	// the quote-unwrapping loop below runs no user code (getUnquoteType is a
	// pure shape test), and the only evaluation this function reaches --
	// doUnquoteValue/doUnquoteSpliced -- happens after the last use, on a
	// path that returns immediately.  Hoisting any further, into
	// findAndUnquote's loop, would span those evaluations, and a host that
	// adjusts the limit from an unquoted expression must still be obeyed on
	// the next element.
	depthLimit := env.Runtime.ValueDepthLimit()
	if valueDepth >= depthLimit {
		return env.Error(ValueDepthError(depthLimit)), nil, 0, 0, 0, false
	}
	// Traverse nested quasiquote/quote wrappers too; they do not delay an
	// unquote in ELPS. See docs/lang.md#quasiquote-traversal. depth tracks
	// list-element position for splicing. valueDepth counts every value edge,
	// including ancestor quote wrappers; the quoted flag itself is not an edge.
	inner := v
	quoteLevel := 0
	if inner.quoted {
		quoteLevel += 1
	}
	for inner.Type == LQuote {
		quoteEdges++
		if valueDepth+quoteEdges >= depthLimit {
			return env.Error(ValueDepthError(depthLimit)), nil, 0, 0, 0, false
		}
		if seen != nil {
			if _, again := seen[inner]; again {
				dup++
			} else {
				seen[inner] = struct{}{}
			}
		}
		quoteLevel += 1
		inner = inner.Cells[0]
	}
	if inner.Type != LSExpr {
		// back out of the entire quote chain and return v to leave the value
		// unchanged in the quasiquote.
		return v, nil, 0, quoteEdges, dup, false
	}
	v = inner

	unquote, err := getUnquoteType(v)
	if err != nil {
		env.loc = v.source
		return env.Error(err), nil, 0, 0, 0, false
	}
	if unquote == unquoteSpliced {
		// v looks like ``(unquote-splicing expr)''
		expr := v.Cells[1]
		if depth == 0 || quoteLevel > 0 {
			env.loc = v.source
			return env.Errorf("unquote-splicing used in an invalid context"), nil, 0, 0, 0, false
		}
		return doUnquoteSpliced(env, expr), nil, 0, quoteEdges, dup, true
	}
	if unquote == unquoteValue {
		// v looks like ``(unquote expr)''
		return doUnquoteValue(env, v.Cells[1], quoteLevel), nil, 0, quoteEdges, dup, true
	}
	return nil, v, quoteLevel, quoteEdges, dup, false
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

func finishUnquote(v *LVal, cells []*LVal, quoteLevel int, hasSplices bool, newlen int) *LVal {
	// splice in children of children that were unquoted with
	// ``unquote-splicing''
	if hasSplices {
		newcells := make([]*LVal, 0, newlen)
		for _, v := range cells {
			if v.spliced {
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
	return requote(expr, quoteLevel)
}

// chargeDuplicated adds n units of duplicated work to *rebuilt and charges
// the part past quasiquoteRebuildAllowance as evaluation steps, returning
// the step-limit or cancellation condition if the charge ends the
// evaluation (see findAndUnquote).
func chargeDuplicated(env *LEnv, rebuilt *int, n int) *LVal {
	before := *rebuilt
	*rebuilt += n
	if charge := *rebuilt - max(before, quasiquoteRebuildAllowance); charge > 0 {
		if lerr := env.ChargeSteps(int64(charge)); lerr.Type == LError {
			return lerr
		}
	}
	return nil
}

// requote restores quoteLevel levels of quoting on v.
func requote(v *LVal, quoteLevel int) *LVal {
	for range quoteLevel {
		v = Quote(v)
	}
	return v
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
