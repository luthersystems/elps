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
	got := s.syntax(v, cycleGuard{state: &st})
	if st.cyclic {
		// The walk above stopped as soon as it knew the expansion contains
		// itself, and whatever it built on the way is discarded -- copies
		// and expansion IDs alike, which is why the IDs are minted on the
		// stamper and only committed below.  The rerun visits each node
		// once and memoises its copies, so a back-edge lands on the copy
		// and the returned tree contains itself exactly where the
		// expansion did.
		s.copies = make(map[*LVal]*LVal)
		s.nextID = s.firstID
		got = s.syntax(v, strictCycleGuard())
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

func locateGuarded(v *LVal, callSite *token.Location, boundary map[*LVal]struct{}, g cycleGuard) {
	if v == nil || isSingleton(v) || v.sealed || isValueNode(v) {
		return
	}
	if _, isArg := boundary[v]; isArg {
		return
	}
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
	if needsStamp(v) {
		v.source = callSite //elps:mutates locates a Go macro's fresh expansion node (fresh by contract, see LEnv.AddMacros); arguments, sealed and value nodes are skipped above
	}
	for _, c := range v.Cells {
		locateGuarded(c, callSite, boundary, g)
	}
	if nested && g.tracking() {
		g.ascend(v)
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
	// copies memoises the strict rerun over a cyclic expansion: a nested
	// node maps to its copy BEFORE its cells are walked, so the walk's
	// second arrival at it -- through the cycle -- lands on the copy.  It
	// is nil on the ordinary walk, which never revisits a node.
	copies map[*LVal]*LVal
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
func (s *macroStamper) syntax(v *LVal, g cycleGuard) *LVal {
	if v == nil {
		return nil
	}
	// Identity-based guard: a type-based check would catch only the empty-
	// LSExpr singletonNil and miss singletonTrue/singletonFalse (which are
	// LSymbol with Source.Pos == -1). See issue #274.
	if isSingleton(v) {
		return v
	}
	// Sealed subtrees are parsed program nodes spliced into the expansion
	// (macros receive their arguments unevaluated, so argument expressions
	// arrive as shared parse-tree nodes).  They are shared, not copied: the
	// same node may be under evaluation in every environment sharing the
	// parse, its location is the real one, and a sealed node's descendants
	// are all sealed, so there is nothing below it to stamp.  Most parser
	// nodes carry a real location (Pos >= 0), but the parser CAN emit
	// synthetic Pos < 0 locations (a funref's lisp:function head symbol, a
	// #^ head symbol mirroring a location-less operand), so without this
	// guard such a node would be replaced by a stamped copy -- harmless
	// now, but a pointless allocation on a shared tree.
	if v.sealed {
		return v
	}
	// Values never reach this walk: the root is diverted by
	// stampMacroExpansion and children by the loop below.  Keep the guard
	// anyway -- it is the ownership rule's last line of defence.
	if isValueNode(v) {
		return s.value(v)
	}
	if len(v.Cells) == 0 {
		// A leaf stamps itself and reaches nothing.
		if !needsStamp(v) {
			return v
		}
		return s.stampedCopy(v)
	}
	// Only a node with children is entered on the guard's path: a leaf
	// reaches nothing, and stamping runs on every macro expansion.
	if g.abandoned() {
		return v
	}
	if s.copies != nil {
		if cp, ok := s.copies[v]; ok {
			return cp
		}
	}
	var cyclic bool
	g, cyclic = g.descend(v)
	if cyclic {
		return v
	}
	var (
		cp    *LVal
		cells []*LVal
	)
	if needsStamp(v) {
		// Stamped BEFORE the cells are walked, so expansion IDs are
		// assigned in pre-order -- a parent before its children -- exactly
		// as the in-place stamp assigned them.
		cp = s.stampedCopy(v)
	}
	if s.copies != nil {
		// The strict rerun over a cyclic expansion: copy unconditionally,
		// and memo the copy before descending so a back-edge lands on it.
		// The cell slice is filled below; the copy holds it from the start
		// so the cycle closes onto the finished storage.
		if cp == nil {
			cp = new(LVal)
			*cp = *v
		}
		cells = make([]*LVal, len(v.Cells))
		cp.Cells = cells
		s.copies[v] = cp
	}
	for i, c := range v.Cells {
		var sc *LVal
		switch {
		case c == nil:
		case isValueNode(c):
			sc = s.value(c)
		default:
			sc = s.syntax(c, g)
		}
		if cells != nil {
			cells[i] = sc
			continue
		}
		if sc != c {
			// The first cell to change: from here on the copy has its own
			// cell slice, seeded with the unchanged cells before it.  v's
			// backing array is never written.
			cells = make([]*LVal, len(v.Cells))
			copy(cells, v.Cells[:i])
			cells[i] = sc
		}
	}
	if g.tracking() {
		g.ascend(v)
	}
	if cp == nil {
		if cells == nil {
			// Nothing under v changed and v itself is located: shared.
			return v
		}
		cp = new(LVal)
		*cp = *v
	}
	if cells != nil {
		cp.Cells = cells
	}
	return cp
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
