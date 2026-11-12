// Copyright © 2026 The ELPS authors

package libelpspath

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib/internal/libutil"
)

// DefaultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "elpspath"

const packageDoc = `Path-based access and mutation for nested data structures.

Paths are given as positional ELPS values — one argument per path
step, no string DSL. Path step types:

  string    map key         "foo"
  int       array index     0, -1
  symbol    iterate all     '*
  list      array slice     '(range 1 3), '(range 1)

? reads without copying. ?set, ?del and ?nil copy the document's maps,
lists and vectors; their "!" forms mutate in place. In-place edits of
list elements are refused; use a copying form or a vector instead.
An out-of-range integer index leaves the document unchanged on a write
(and returns nil on a read). Negative indexes count from the end.

Tagged values and quote wrappers are rebuilt recursively by copying
writes and traversed by cycle checks, but cannot be indexed into.
Other value types are opaque leaves, including symbols, bytes,
functions and natives. Reads may return them and writes
may store them. A further path step into a leaf raises an error naming
its type and location. Copies share opaque leaves by reference; mutation
of their storage outside elpspath is visible through both documents.
Writes need containers that are acyclic and arrays with exactly one
dimension, and check this throughout the document, including off-path
containers and the contents of tagged values and quote wrappers, because a
write may copy or rebuild any of them. New values pass the same check.
A read (?) with a path checks only what that path steps into: it never
walks beyond its path or into the value it returns, so a fault elsewhere
cannot change its answer, and its cost follows the path rather than the
document. A read with no path checks the whole document, as writes do.
Function bodies and captured environments remain opaque and shared; they
are not walked.

An iterator retains its per-element error handling: reads substitute nil
for a failed element path, and writes leave that element unchanged.

Iterators count their work: one unit per element visited and per path
step taken below it, plus the containers copied or shifted under them. Past 2^20 units in one call,
each further unit is charged as one evaluation step, and the call checks
the evaluation's deadline as it goes. Running out of steps or time stops
the whole call with the evaluator's own condition (step-limit-exceeded,
step-budget-exceeded or context-cancelled); a mutating form keeps the
changes it already made. Calls below the allowance cost the steps they
always did.

For ?set! and ?set, the last argument is the new value; all preceding
arguments are path steps.

parse-path is the one function that takes a string: it converts a
jq-style path into a list of the steps above, for a path that
arrives as a string and is used more than once.

  (apply ? (cons obj (parse-path ".items[0].id")))`

// LoadPackage adds the elpspath package to env.
//
// DefinePackage is get-or-create, and AddBuiltins refuses only exact
// symbol-name collisions, so an embedder may compose additional builtins
// into the same lisp-visible "elpspath" package with its own loader (as
// substrate does with its legacy jq-string operations).
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	prevPkg := env.Runtime.Package.Name
	defer env.InPackage(lisp.Symbol(prevPkg))
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.SetPackageDoc(packageDoc)
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// builtins are functions exported to elps.
//
// IMPORTANT: the formal lists below are read-only templates built once at
// package initialisation and handed to AddBuiltins by every environment that
// loads this package, exactly as every other lisplib package here does.
// Nothing may mutate them after construction.
//
// Two mechanisms hold that, and the annotation below rests on both.  libutil
// SEALS each formals list at construction, so the shared template is under
// the kernel's sealed-write guards; and AddBuiltins gives every environment
// a PRIVATE copy of it (formalsCopier, lisp/defformals.go, issue #513), so no
// two Runtimes hold one list in the first place.  See the long comment above
// AddMacros in lisp/env.go for why both exist.
//
//elpsvet:allow read-only formal templates; sealed by libutil at construction, and copied per environment by formalsCopier (lisp/env.go AddBuiltins)
var builtins = []lisp.LBuiltinDef{
	lisp.DropsArgList(libutil.FunctionDoc("?", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryGet,
		`Get value at a path specified by positional args.

		Each arg is a path step: string for map key, int for array index,
		'* to iterate all elements, '(range from to) for array slice.
		The slice end is optional: '(range from) runs to the end.

		Values of any leaf type may be read. Descending into a leaf raises
		an error naming its type and path. A missing key or out-of-range
		integer index returns nil. Results may share storage with obj.

		(? obj "foo" 0 "bar")     => value at foo[0].bar
		(? users '* "name")       => list of all user names
		(? scores '(range 1 3))   => elements [1,3)
		(? scores '(range 1))     => elements [1,end)
		(? scores '(range -2))    => the last two elements
		(? obj)                   => obj itself (no path steps)

		Accepts at most 1024 nested iterator steps; deeper traversal raises a depth error.`)),
	lisp.DropsArgList(libutil.FunctionDoc("parse-path", lisp.Formals("selector"), BuiltinParsePath,
		`Convert a jq-style path string into a list of positional path steps.

		The steps are what the ? family takes, so the result applies
		straight into any of them:

		(parse-path ".items[0].id")   => '("items" 0 "id")
		(parse-path ".items[].id")    => '("items" * "id")
		(parse-path ".items[1:3]")    => '("items" '(range 1 3))
		(parse-path ".items[1:]")     => '("items" '(range 1))
		(parse-path ".")              => '()

		(apply ? (cons obj (parse-path sel)))
		(apply ?set (concat 'list (list obj) (parse-path sel) (list v)))

		This is for a path that ARRIVES AS A STRING and is used more than
		once -- convert it once, keep the steps, and every later operation
		skips the parse.

		KEY SYNTAX. A bare .key is the classic identifier rule,
		[A-Za-z_][A-Za-z_0-9]*, which is jq's rule too. Underscores and
		digits are fine, so the snake_case keys these paths usually
		address need nothing special:

		(parse-path ".field_mask.paths")  => '("field_mask" "paths")

		Anything else -- a hyphen, a leading digit, "$", non-ASCII --
		MUST be bracketed and quoted, as it must in jq:

		(parse-path ".my-key")        => error: failed to parse: -key
		                                 (the error explains this rule)
		(parse-path ".[\"my-key\"]")   => '("my-key")
		(parse-path ".[\"\"]")         => '("")

		The jq optional-selector suffix "?" is accepted and DISCARDED --
		".a?" is exactly ".a" -- because nothing in the engine suppresses
		errors per step.

		A malformed selector RAISES. It does not return an empty list:
		no steps is the identity path, so a swallowed error would make a
		bad selector address the whole document.

		A selector may not SPAN LINES, and that raises for the same
		reason:

		(parse-path ".[0]
		.password")   => error: selector may not span lines

		The jq-string grammar this shares cuts a bracket-led selector at
		its first newline and DISCARDS the rest, which would convert the
		selector above to the single step 0 -- a live path to the whole
		element. (apply ?set ...) through it would overwrite the record
		instead of its "password" field, silently.

		Converting on each call is slower than one string-path operation,
		since it parses AND builds a list. How much the caching is worth
		depends on the operation: a write first walks the whole document to
		validate it, so on a large document the parse is noise, but a read
		(?) walks only its path, so for reads the parse can dominate at any
		document size.`)),
	lisp.DropsArgList(libutil.FunctionDoc("?set!", lisp.Formals("val", lisp.VarArgSymbol, "steps-and-value"), BuiltinQuerySetMutate,
		`Set value at a path specified by positional args, mutating the original.

		The last argument is the new value; all preceding arguments are path
		steps. Returns the mutated original. Any leaf type may be stored.
		Out-of-range integer indexes are no-ops. List element mutation is
		refused; use ?set or a vector instead.

		The new value is stored BY REFERENCE, not copied: after the call it is
		reachable and mutable through the result, and a later write through
		either name is visible through the other.

		(?set! obj "foo" "bar" "new")   => obj with foo.bar="new" (mutated)
		(?set! obj "items" 0 "x")       => obj with items[0]="x" (mutated)
		(?set! data '* "active" true)   => set active=true on all elements
		(?set! scores '* 0)             => every element of scores is 0

		A path that ends in '* stores the value in each element. The
		sequence must be a vector.

		Accepts at most 1024 nested iterator steps; deeper traversal raises a depth error.`)),
	lisp.DropsArgList(libutil.FunctionDoc("?set", lisp.Formals("val", lisp.VarArgSymbol, "steps-and-value"), BuiltinQuerySet,
		`Set value at a path specified by positional args, returning a copy.

		The last argument is the new value; all preceding arguments are path
		steps. The original is not modified. Any leaf type may be stored.
		Out-of-range integer indexes return an unchanged copy.
		Quoted lists stay quoted on an out-of-range integer no-op but become
		unquoted after an in-range integer edit.

		Maps, lists, vectors and tagged/quote wrappers in the SOURCE document
		are copied recursively; opaque leaves (including bytes, functions
		and native values) are shared. The new value is
		stored BY REFERENCE, not copied: the value you supply becomes
		reachable and mutable through the result, so a later in-place write
		through the result (?set!, ?del!, append!) reaches the caller's
		value, and vice versa. Pass a copy if the result must not alias it.

		(?set obj "foo" "bar" "new")   => new obj with foo.bar="new"
		(?set obj "items" 0 "x")       => new obj with items[0]="x"

		Iterative copying beyond 1000000 levels raises an ordinary depth error.

		Accepts at most 1024 nested iterator steps; deeper traversal raises a depth error.
		Copying path chains longer than 1024 steps also raise a depth error.`)),
	lisp.DropsArgList(libutil.FunctionDoc("?del!", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryDeleteMutate,
		`Delete value at a path specified by positional args, mutating the original.

		Out-of-range integer indexes are no-ops. List element mutation is refused.

		(?del! obj "foo")              => obj with foo removed (mutated)
		(?del! obj "items" 1)          => obj with items[1] removed (mutated)
		(?del! records '* "cache")     => remove cache key from all elements
		(?del! scores '*)              => every element of scores is nil

		A path that ends in '* stores nil in each element and keeps the
		length, as ?del does. The sequence must be a vector.

		Accepts at most 1024 nested iterator steps; deeper traversal raises a depth error.`)),
	lisp.DropsArgList(libutil.FunctionDoc("?del", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryDelete,
		`Delete value at a path specified by positional args, returning a copy.

		Out-of-range integer indexes return an unchanged copy. Containers are
		copied recursively through tagged/quote wrappers; opaque leaves,
		including functions, are shared by reference.
		Quoted lists stay quoted on an out-of-range integer no-op but become
		unquoted after an in-range integer edit.

		(?del obj "foo")               => new obj with foo removed
		(?del obj "items" 1)           => new obj with items[1] removed

		Iterative copying beyond 1000000 levels raises an ordinary depth error.

		Accepts at most 1024 nested iterator steps; deeper traversal raises a depth error.
		Copying path chains longer than 1024 steps also raise a depth error.`)),
	lisp.DropsArgList(libutil.FunctionDoc("?nil!", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryNilMutate,
		`Set value at a path to nil, mutating the original. The key is kept.

		Out-of-range integer indexes are no-ops. List element mutation is refused.

		(?nil! obj "foo")              => obj with foo=nil (mutated)
		(?nil! rows '* "cached")       => nil out cached on all elements
		(?nil! scores '*)              => every element of scores is nil

		A path that ends in '* stores nil in each element. The sequence
		must be a vector.

		Accepts at most 1024 nested iterator steps; deeper traversal raises a depth error.`)),
	lisp.DropsArgList(libutil.FunctionDoc("?nil", lisp.Formals("val", lisp.VarArgSymbol, "steps"), BuiltinQueryNil,
		`Set value at a path to nil, returning a copy. The key is kept.

		Out-of-range integer indexes return an unchanged copy. Containers are
		copied recursively through tagged/quote wrappers; opaque leaves,
		including functions, are shared by reference.
		Quoted lists stay quoted on an out-of-range integer no-op but become
		unquoted after an in-range integer edit.

		(?nil obj "foo")               => new obj with foo=nil
		(?nil patient "ssn")           => new obj with ssn=nil

		Iterative copying beyond 1000000 levels raises an ordinary depth error.

		Accepts at most 1024 nested iterator steps; deeper traversal raises a depth error.
		Copying path chains longer than 1024 steps also raise a depth error.`)),
}

// errCyclicValue reports a value that contains itself.  append! and assoc!
// mutate a container in place, so a program can put a container inside
// itself.  A copy of one has no finite representation, so the write gate and
// the copy walk refuse it.  A read is different (#960): its path has finitely
// many steps and it never recurses into what it returns, so it answers on a
// cyclic value instead.  The walkers below refuse it with an ordinary error,
// which the builtins turn
// into a condition handler-bind can catch, rather than recursing until the
// goroutine stack overflows and the runtime kills the process -- a failure
// recover() cannot intercept.  See cycle.go and issue #393.
var errCyclicValue = errors.New("cannot operate on a value that contains itself")

// errNilCell refuses a Go nil *LVal met on a read's path. Only host code can
// build one; the whole-document gate stopped it before reads were scoped to
// their path (#960), so the read accessors now refuse it themselves rather
// than return a value no builtin may return.
var errNilCell = errors.New("invalid document: nil cell")

// okSimpleContainerTypeGuarded validates container shapes and detects cycles.
// Non-container values are opaque leaves and impose no type restriction.
//
// It continues the walk g is already on rather than starting a fresh one.
// Every nested check must pass g down; starting a new walk per level resets
// the bound on every lap and it never fires.  okSimpleType is the entry
// point that begins a walk.
//
// With okSimpleType this is the gate every write builtin runs before
// touching a value; ? with a path does not, because such a read only visits
// its path (#960).
// The copy walk refuses a multi-dimensional array itself as well, since it
// cannot construct a copy of one and an embedder can reach it through the
// Path API without this gate.  A cycle
// is refused here for the same reason, and the copy walk is guarded too
// because the exported Path interface lets a Go embedder reach a copy
// without coming through this gate.
func okSimpleContainerTypeGuarded(in *lisp.LVal, g cycleGuard) error {
	if in.IsNil() {
		return errors.New("nil container type invalid")
	}
	switch in.Type {
	case lisp.LSortMap, lisp.LArray, lisp.LSExpr, lisp.LTaggedVal, lisp.LQuote:
		// Containers and wrappers reach other values; opaque leaves do not.
		// Handled below.
	default:
		return fmt.Errorf("invalid container type: %v", in.Type)
	}
	if _, ok := g.state.valid[in]; ok {
		// Already validated, reached again through sharing: a container
		// that passed once passes again, and one that finished cannot be
		// on a cycle, or its walk would not have finished.
		return nil
	}
	g, cyclic := g.descend(in)
	if cyclic {
		return errCyclicValue
	}
	var err error
	if g.state.unordered && in.Type == lisp.LSortMap {
		// In place of okSimpleContainerContents, not under it: the walk
		// recurses once for each level of the document, and a frame added
		// to a level lowers the depth the goroutine stack can hold.
		err = okSimpleMapUnordered(in, g)
	} else {
		err = okSimpleContainerContents(in, g)
	}
	if g.tracking() {
		g.ascend(in)
	}
	if err == nil {
		g.state.noteValid(in, containerWidth(in))
	}
	return err
}

// containerWidth is the number of values a validated container holds, for
// the sharing budget.
func containerWidth(in *lisp.LVal) int {
	switch in.Type {
	case lisp.LSortMap:
		if m, err := mapData(in); err == nil {
			return 2 * m.Len()
		}
		return 0
	case lisp.LArray:
		if _, data, err := arrayParts(in); err == nil {
			return len(data.Cells)
		}
		return 0
	default:
		return len(in.Cells)
	}
}

// okSimpleContainerContents checks the values a container reaches.  It is
// only ever called through okSimpleContainerTypeGuarded, which has already
// established that in is a container and put it on g's path.
func okSimpleContainerContents(in *lisp.LVal, g cycleGuard) error {
	switch in.Type {
	case lisp.LTaggedVal, lisp.LQuote:
		wrapped, err := wrapperValue(in)
		if err != nil {
			return err
		}
		return okSimpleTypeGuarded(wrapped, g)
	case lisp.LSortMap:
		m0, err := mapData(in)
		if err != nil {
			return err
		}
		entries := sortedMapEntries(m0)
		if entries.IsError() {
			return lisp.GoError(entries)
		}
		for _, ent := range entries.Cells {
			v := ent.Cells[1]
			err := okSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LArray:
		// Exactly one dimension, matching toCells -- see the reason there.
		// The gate and the accessor have to agree about what "indexable"
		// means, or a shape this admits still fails downstream.
		dims, data, err := arrayParts(in)
		if err != nil {
			return err
		}
		if n := dims.Len(); n != 1 {
			if n > 1 {
				return errors.New("cannot index multi-dimensional array")
			}
			return errors.New("cannot index zero-dimensional array")
		}
		cells := data.Cells
		for _, v := range cells {
			err := okSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	case lisp.LSExpr:
		cells := in.Cells
		for _, v := range cells {
			err := okSimpleTypeGuarded(v, g)
			if err != nil {
				return err
			}
		}
		return nil
	default:
		return fmt.Errorf("invalid container type: %v", in.Type)
	}
}

// unorderedPairsCap is the first capacity of an unordered validation walk's
// stack of map entries.  The stack holds the entries of the maps on the
// walk's current path, so 16 covers a few levels of small records in one
// allocation.  An entry is 40 bytes.
const unorderedPairsCap = 16

// errOrderedWalk ends an unordered validation walk that met a map it cannot
// read without the map's own Entries method.  It never leaves okSimpleType.
var errOrderedWalk = errors.New("elpspath: map needs an ordered walk")

// okSimpleMapUnordered checks the values of the map in, in no particular
// order, for the first pass of okSimpleType.
//
// The ordered walk asks the map for its entries, which builds a pair, a key
// and two slots for each entry and sorts them, only to read the values and
// drop the rest.  On a document of many records that is almost all the time
// and memory of a validation (luthersystems/elps#944).  AppendMapKeyPairs
// hands out the same values with nothing built and nothing sorted.  The
// pairs of every map on the walk's current path share one slice, used as a
// stack, so the walk makes that slice, grows it a few times, and allocates
// nothing else for a map.
//
// A backing other than the interpreter's own is not read here.  Its Entries
// method is host code: it can fail, and it can change the document.  The
// walk stops before it calls any method of such a map, so the ordered pass
// is the only caller, once for each visit.
func okSimpleMapUnordered(in *lisp.LVal, g cycleGuard) error {
	st := g.state
	if st.pairs == nil {
		st.pairs = make([]lisp.MapKeyPair, 0, unorderedPairsCap)
	}
	// A map with no map data would panic AppendMapKeyPairs; the ordered
	// pass reports it (errMalformedMap, #963).
	if _, err := mapData(in); err != nil {
		return errOrderedWalk
	}
	base := len(st.pairs)
	var ok bool
	st.pairs, ok = in.AppendMapKeyPairs(st.pairs)
	if !ok {
		return errOrderedWalk
	}
	// A nested map appends to st.pairs and can move it, so each pair is
	// read through the state by index.  Nothing below truncates past end.
	end := len(st.pairs)
	var err error
	for i := base; i < end && err == nil; i++ {
		err = okSimpleTypeGuarded(st.pairs[i].Val, g)
	}
	// Drop the values so the slice does not keep them reachable.
	clear(st.pairs[base:end])
	st.pairs = st.pairs[:base]
	return err
}

// ValidateDocument is the whole-document gate the write builtins (?set,
// ?set!, ?del, ?del!, ?nil, ?nil!) run before they touch a document: it
// refuses cycles, arrays that are not one-dimensional, and malformed
// tagged or quote wrappers anywhere in v. A write can copy or rebuild any
// part of the document, so it needs the whole graph to be sound; a read
// with a path does not, and ? runs it only when given no path (#960).
//
// It is exported so an embedder (luthersystems/substrate's v1 builtins) can
// gate its own writes directly instead of calling (elpspath:? v) for the
// side effect. That no-step read still runs this gate, so existing
// embedders stay safe, but it is the only read that does, and saying what
// is meant is clearer than relying on it.
func ValidateDocument(v *lisp.LVal) error { return okSimpleType(v) }

// okSimpleType validates the container graph, accepting arbitrary opaque leaves.
// It is linear in the DISTINCT containers of a value with nested sharing: past
// sharedWalkBudget work it skips a container it has already validated (see
// cycleState.valid), so a document holding a D-level sharing chain costs D,
// not 2^D, on every elpspath write.
// Every write builtin retains this guard: copying supports lists, maps,
// one-dimensional arrays, and tagged/quote wrappers. These walks reject cycles.
// ? with a path does not run it: such a read visits only its path and never
// copies, so the whole-document walk would make every read cost O(document)
// for nothing (#960). ? with no path still runs it (see BuiltinQueryGet).
// Opaque leaf storage (bytes, native payloads, function bodies/environments)
// is neither traversed nor copied by elpspath.
//
// It walks in two passes, because every call pays for the first and almost
// none needs the second.
//
// The first pass reads each map's values in no particular order
// (okSimpleMapUnordered).  It answers one question: is the document valid.
// The order of a walk does not change that answer.  Either walk reaches the
// same containers and makes the same check on each one, and a container on
// a cycle fails in any order, because no walk into it can finish.
//
// The second pass runs only when the first found a fault, or a map it could
// not read.  It is the walk in sorted key order, and its result is the
// function's result.  So a document with more than one fault reports the
// first one in key order, whatever order the first pass took, and a valid
// document costs one cheap walk.  The error is consensus-visible to an
// embedder such as substrate, which is why the ordered walk decides it.
// TestValidatorMatchesOrderedWalk compares the two passes with the ordered
// walk alone.
func okSimpleType(in *lisp.LVal) error {
	if okSimpleTypeUnordered(in) {
		return nil
	}
	var st cycleState
	return okSimpleTypeGuarded(in, newCycleGuard(&st))
}

// okSimpleTypeUnordered is the first pass of okSimpleType.  It reports
// whether the unordered walk found in valid.
//
// A panic counts as a fault.  Only a value that host code built wrongly can
// panic the walk; the walk checks the ones it knows (a Go nil cell, a
// malformed array or map header), and the recover is the backstop.  The
// ordered walk can meet another fault before that value and return an
// error, so the ordered pass decides:
// it returns that error, or panics on the value itself.
func okSimpleTypeUnordered(in *lisp.LVal) (valid bool) { //nolint:nonamedreturns // the deferred recover sets the result in this frame
	defer func() {
		if recover() != nil {
			valid = false
		}
	}()
	var st cycleState
	st.unordered = true
	return okSimpleTypeGuarded(in, newCycleGuard(&st)) == nil
}

// okSimpleTypeGuarded is okSimpleType continuing a walk already in progress.
// The guard is threaded through rather than re-created because the recursion
// that overflows the stack is okSimpleType <-> okSimpleContainerType, so a
// bound either survives the round trip or does nothing.
func okSimpleTypeGuarded(in *lisp.LVal, g cycleGuard) error {
	// A host-built Go nil cell is a fault to report, not a panic: the
	// ordered pass decides the gate's answer, so it must return one.
	if err := checkCell(in); err != nil {
		return err
	}
	if in.IsNil() {
		// allow nil as a placeholder for removed elements
		return nil
	}
	switch in.Type {
	case lisp.LSortMap, lisp.LArray, lisp.LSExpr, lisp.LTaggedVal, lisp.LQuote:
		return okSimpleContainerTypeGuarded(in, g)
	default:
		// Everything else is an opaque leaf, regardless of its ELPS type.
		return nil
	}
}
