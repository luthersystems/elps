// Copyright © 2026 The ELPS authors

package elpsutil_test

import (
	"context"
	"fmt"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"github.com/luthersystems/elps/elpsutil"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser"
)

// Fuzzing the EMBEDDING SURFACE: the path a Go program takes to install its own
// packages, builtins, special operators and macros into an interpreter, and
// then to run lisp against them.
//
// # Why this package
//
// Issue #318 lists elpsutil/ as unfuzzed. It is 193 lines and looks trivial,
// which is exactly why it was left last -- and it is the wrong one to leave
// last. luthersystems/substrate imports github.com/luthersystems/elps/elpsutil
// directly and wraps it in internal/substrate/elpsutil/elpsutil.go, so this is
// the code that installs every chaincode-visible Go builtin. Nothing else in
// this repository stands between a substrate package definition and the
// interpreter's symbol table.
//
// The measured starting point was worse than "unfuzzed". Running the WHOLE
// repository test suite with -coverpkg=./elpsutil reported 6.2% of statements:
//
//	Function            100.0%     <- incidentally, from debugger + FuzzEval
//	Name/Formals/Eval   100.0%     <- same
//	nopLoader             0.0%
//	packageInit           0.0%
//	packageBuiltins       0.0%
//	packageSpecialOps     0.0%
//	packageMacros         0.0%
//	Load                  0.0%
//	LoadAll               0.0%
//	LibraryLoader         0.0%
//	PackageLoader         0.0%
//
// The only part of elpsutil the suite touched was the one-line Function
// constructor, used by three tests that want a throwaway builtin. The entire
// package-loading surface -- the part substrate actually calls, as
// `elpsutil.Load(env, elpsutil.PackageLoader(&substrate.CorePackage{}))` --
// had never executed under test.
//
// This target's SEED CORPUS ALONE, with no fuzzing at all, takes that to 96.9%
// (measured: go test -run FuzzElpsutilEmbed -coverpkg=./elpsutil). Twelve of
// the thirteen functions reach 100%. What remains is two blocks in
// PackageLoader:
//
//	e := env.DefinePackage(name);  if !e.IsNil() { return e }
//	e = env.InPackage(name);       if !e.IsNil() { return e }
//
// Neither is reachable. DefinePackage errors only when its argument is neither
// LSymbol nor LString, and the argument is lisp.Symbol(p.PackageName()), which
// is an LSymbol by construction. InPackage errors only when the package does
// not exist, and the line above just defined it. They are defensive branches
// against arguments elpsutil builds itself, so no input can reach them and no
// seed pretends to -- stated here rather than papered over with a stub that
// would cover the lines without testing anything.
//
// # What is fuzzer-controlled, and why each thing is
//
// Two inputs, mutated independently so any package shape can be paired with
// any program:
//
//	spec -- a byte program describing what to INSTALL: how many packages, what
//	        they are named, which of the four optional interfaces each one
//	        implements, what its PackageInit does, and the name/formals/body of
//	        every builtin, special operator and macro it registers. Also which
//	        of the four loader entry points combines them.
//
//	src  -- ordinary elps source, evaluated after the install, so the
//	        registered functions are CALLED with arguments the fuzzer chose.
//
// Justification, item by item:
//
//   - PACKAGE NAMES are fuzzer-controlled because a name is not inert. It is
//     the registry key (DefinePackage/InPackage), it is embedded in every FID
//     string, it is the left half of qualified symbol resolution `pkg:sym`,
//     and it is what GetFunName renders into error messages. An embedder picks
//     it, so a defect keyed on a name with a ':' in it, an empty name, or a
//     name that is not valid UTF-8 is an embedder-reachable defect. Names are
//     drawn EITHER from a table of interesting constants OR straight from
//     spec bytes; see specReader.name.
//
//   - WHICH OPTIONAL INTERFACES A PACKAGE IMPLEMENTS is fuzzer-controlled
//     because that is the whole of packageInit, packageBuiltins,
//     packageSpecialOps and packageMacros: each is a two-branch type
//     assertion, and the !ok branch of all four (plus nopLoader, which only
//     the init one can reach) is dead code unless some package declines to
//     implement them. Four concrete Go types cover the lattice; see pkgTypes.
//
//   - FORMALS are fuzzer-controlled because the formals list is data the
//     embedder constructs and can get wrong. elpsutil.Function stores
//     whatever it is given; PackageLoader now checks the shape at install
//     time (checkFormals), while lisp.FunInPackage still performs no
//     validation at all. The table therefore carries both kinds of shape:
//     well-formed ones, which register and reach argument binding deep inside
//     LEnv.bind, and malformed ones an embedder can plausibly produce by
//     accident -- an LError (which is what lisp.Formals("&rest") returns), a
//     non-QExpr scalar, a list whose cells are not symbols -- which exercise
//     the install-time refusal AND are still routed into bind through the
//     validation bypass; see "Formals that elpsutil now refuses" below.
//
//   - ONE SHARED FORMALS VALUE, reused across several registrations, is a
//     formals shape of its own (formalsShared). elpsutil.Builtin.Formals
//     returns fun.formals by POINTER, and env.builtin hands that same pointer
//     to FunInPackage, so registering one *Builtin into two packages gives two
//     LFuns aliasing one formals LVal. lisp.bind documents that it "does not
//     modify fun or args"; this is the input that would notice if that ever
//     stopped being true.
//
//   - BUILTIN BODIES are fuzzer-controlled, from a table, because the body is
//     the part substrate writes. The interesting ones re-enter the
//     interpreter (env.Eval, env.LoadString) or mutate interpreter state
//     (env.InPackage) from inside a call -- which is what a real Go builtin
//     does, and which no other fuzz target in this repository does at all.
//     FuzzEval registers exactly one builtin, `fuzz-panic`, whose body is a
//     bare panic.
//
//   - THE LOADER STRATEGY is fuzzer-controlled because Load, LoadAll,
//     LibraryLoader and PackageLoader are four separate functions with four
//     separate error-propagation paths, and LoadAll and LibraryLoader nest.
//
//   - src IS ORDINARY ELPS because installing a builtin proves nothing until
//     something calls it. This is also the only way the fuzzer supplies the
//     ARGUMENTS a builtin binds, which is where malformed formals bite.
//
// # The panic that is designed in, and where it went
//
// lisp.AddBuiltins ends with
//
//	if exist.Type != LError { panic("symbol already defined: " + f.Name()) }
//
// and AddMacros/AddSpecialOps have the same shape. That is a deliberate
// programming-error panic, it is not elpsutil's, and it used to be trivially
// reachable through elpsutil -- `Function("true", ...)` alone did it, because
// Package.get answers the reserved symbols `true` and `false` without
// consulting the symbol table, so they are "already defined" in a package that
// is brand new and empty.
//
// It is no longer reachable through elpsutil (#351): PackageLoader now
// refuses a colliding definition with an *LVal error, mirroring each Add*
// function's own rule, before lisp ever sees the definition. The harness
// therefore no longer filters names. Earlier versions pre-checked every name
// against the package it was about to be bound in and dropped the ones that
// would collide (nameAvailable), which was the only way to fuzz at all while
// the panic was reachable. Now colliding names are handed straight to
// elpsutil and the refusal is part of what is fuzzed: markRejectEvidence
// attributes "already defined" and "reserved constant" load errors to the
// reject/collision and reject/reserved-name labels, and the guard corpus is
// required to produce both.
//
// lisp's panic itself is still real, and this harness still depends on it
// being real: elpsutil's checkPackageName exists only to mirror it, so if the
// panic changed shape, elpsutil could refuse definitions lisp accepts (or
// stop refusing ones it rejects) without any test noticing.
// TestCollisionPanicIsReal pins the panic at its source -- env.AddBuiltins,
// env.AddSpecialOps and env.AddMacros, called directly -- so that account
// cannot silently expire.
//
// The payoff is that the assertion needs no qualification and no longer rests
// on a filter: NO panic is tolerated, and nothing is dropped from the input
// space to keep one at bay. There is no recover in elpsutil, none between it
// and the fuzz function, so a panic during install is an ordinary crash that
// `go test -fuzz` attributes to its input and writes to testdata.
//
// # Go nil, which used to be excluded and is now fuzzer-controlled
//
// Earlier versions of this target never generated a Go nil, in two places,
// and recorded both exclusions as findings: a builtin registered with nil
// formals (`elpsutil.Function(name, nil, fn)`, the plausible mis-spelling of
// lisp.Formals()) registered cleanly and then nil-derefed inside LEnv.bind on
// the first call, and a Loader or PackageInit returning a Go nil *LVal
// nil-derefed immediately inside elpsutil itself, as an uncaught panic in the
// embedder's process. Both were contract violations only a public-API change
// could fix, and this header used to say that decision was not this target's
// to take.
//
// That decision has since been taken (#351): elpsutil now validates at the
// boundary. PackageLoader refuses nil formals at INSTALL time with an error
// naming the package and definition, and every loader entry point reports a
// nil loader result as an error naming the loader. Both shapes are therefore
// generated like any other input: formalsTable carries a go-nil entry and
// initTable carries init/go-nil, and the resulting loads must fail with
// ordinary errors -- never a panic, never an internal-panic LVal.
// TestNilContractMistakesAreErrors pins the new behaviour in both directions,
// so a regression toward the old panics is named there before the fuzzer
// rediscovers it as a crash.
//
// # Formals that elpsutil now refuses, and how bind still sees them
//
// Five formals shapes in formalsTable -- error-value, scalar-int,
// scalar-string, non-symbol-cells and nested -- used to flow through elpsutil
// into LEnv.bind as data, which is why they are in the table at all: bind is
// where a malformed formals list finally gets examined, and nothing else in
// this repository's fuzz corpus reaches it with shapes like these.
// PackageLoader now refuses them at install time (checkFormals), which is the
// right boundary behaviour but would, left alone, quietly end that bind
// coverage while the coverage labels kept passing on decode-time marks.
//
// The harness keeps both halves, honestly labelled:
//
//   - the def is handed to elpsutil anyway, and the refusal is proved rather
//     than presumed: markRejectEvidence marks formals-reject/<shape> only
//     when a load error names the def and carries the shape's problem text;
//
//   - buildDefs registers a copy of the def through env.AddBuiltins(true,
//     ...) -- the documented route that bypasses elpsutil's validation,
//     available to any embedder that skips PackageLoader -- under a fresh
//     fz-bind-N name in the user package, so autoCall still drives the shape
//     into bind, and formals/<shape> is marked at exactly that registration.
//
// go-nil is the exception: it takes the refusal half only. Calling a builtin
// whose formals are Go nil still nil-derefs inside bind -- that panic is
// lisp's, deliberately untouched by #351 -- and the harness asserts
// IsInternalPanic on every call result, so a bypass copy would turn the known
// finding into a permanent red. TestFormalsTableRejectionFlags pins each
// entry's rejected flag and problem text against elpsutil.Validate, so the
// table cannot drift from the rule elpsutil actually applies.
//
// # What is deliberately NOT fuzzer-controlled
//
// NO FILESYSTEM. Runtime.Library is left nil for the same reason FuzzEval
// leaves it nil: it is what makes load-file return an error rather than
// reading the disk, so fuzzer-generated source cannot reach the test machine.
//
// SINGLE GOROUTINE. Install and evaluation both run on one worker goroutine
// under the watchdog, and nothing else runs concurrently. A crasher that does
// not reproduce from its own testdata file is worthless.
const (
	// Budgets for the interpreter. Same shape and same reasoning as FuzzEval's
	// and FuzzDebugEval's -- src is arbitrary elps and nothing else bounds it.
	fuzzMaxSteps          = 200_000
	fuzzMaxTailIterations = 10_000
	fuzzMaxPhysicalHeight = 2_000
	fuzzMaxAlloc          = 1_000_000
	fuzzMacroDepth        = 100

	// fuzzDeadline bounds the interpreter itself; watchdogTimeout is the
	// harness's backstop and is measured in SCHEDULED time (internal/fuzzwatch),
	// because a wall-clock bound on a contended runner fails inputs that are
	// not to blame.
	fuzzDeadline    = 2 * time.Second
	watchdogTimeout = 30 * time.Second

	// Shape of one install. Both caps exist so a single input cannot spend the
	// whole deadline in the harness: every package is a full DefinePackage /
	// InPackage / register cycle, and every def is an LFun allocation plus a
	// symbol-table write.
	//
	// maxPackages is >1 on purpose. One package cannot reach LibraryLoader's
	// loop, cannot produce a second registration into the SAME package name
	// from a different Package value, and cannot alias one *Builtin across two
	// packages -- which is the formalsShared case above.
	maxPackages = 4
	maxDefs     = 6

	// maxAutoCalls bounds how many registered symbols autoCall invokes. An
	// install can register up to 3 x maxDefs per package across maxPackages;
	// calling all of them at two arities each would let one input spend the
	// whole deadline in the harness. The interesting ones are the first few.
	maxAutoCalls = 8

	// maxNameBytes caps a fuzzer-derived name. Long enough to carry a
	// multi-byte rune, a ':' and a NUL; short enough that the mutator spends
	// its budget on shapes rather than on length.
	maxNameBytes = 16

	// initSwitchPackage is where the "switch package during init" body goes.
	// It is a fixed name rather than a fuzzer-chosen one so the init body can
	// define and enter it without consuming spec bytes, and so the package a
	// switching init leaves behind is one the corpus can name.
	initSwitchPackage = "elpsutil-fuzz-init-switch"
)

// ---------------------------------------------------------------------------
// the four package types
// ---------------------------------------------------------------------------

// pkgData is everything a generated package knows. The four types below wrap
// it and differ ONLY in which optional interfaces they satisfy, which is what
// selects the ok/!ok branch of packageInit, packageBuiltins, packageSpecialOps
// and packageMacros.
type pkgData struct {
	name     string
	doc      string
	init     func(*lisp.LEnv) *lisp.LVal
	builtins []lisp.LBuiltinDef
	ops      []lisp.LBuiltinDef
	macros   []lisp.LBuiltinDef
}

// plainPkg implements Package and nothing else: every type assertion in
// elpsutil misses, and packageInit falls through to nopLoader.
type plainPkg struct{ d *pkgData }

func (p plainPkg) PackageName() string { return p.d.name }

// defsPkg registers functions but has no init and no doc.
type defsPkg struct{ d *pkgData }

func (p defsPkg) PackageName() string            { return p.d.name }
func (p defsPkg) Builtins() []lisp.LBuiltinDef   { return p.d.builtins }
func (p defsPkg) SpecialOps() []lisp.LBuiltinDef { return p.d.ops }
func (p defsPkg) Macros() []lisp.LBuiltinDef     { return p.d.macros }

// initPkg documents and initialises itself but registers nothing.
type initPkg struct{ d *pkgData }

func (p initPkg) PackageName() string                   { return p.d.name }
func (p initPkg) PackageDoc() string                    { return p.d.doc }
func (p initPkg) PackageInit(env *lisp.LEnv) *lisp.LVal { return p.d.init(env) }

// fullPkg implements every optional interface.
type fullPkg struct{ d *pkgData }

func (p fullPkg) PackageName() string                   { return p.d.name }
func (p fullPkg) PackageDoc() string                    { return p.d.doc }
func (p fullPkg) PackageInit(env *lisp.LEnv) *lisp.LVal { return p.d.init(env) }
func (p fullPkg) Builtins() []lisp.LBuiltinDef          { return p.d.builtins }
func (p fullPkg) SpecialOps() []lisp.LBuiltinDef        { return p.d.ops }
func (p fullPkg) Macros() []lisp.LBuiltinDef            { return p.d.macros }

// pkgTypes is indexed by a spec byte. Order is load-bearing only in that
// TestElpsutilFuzzReachesEveryFeature reads the labels back, not the indices.
var pkgTypes = []struct {
	label string
	// registers reports whether this type can carry defs, which the harness
	// needs to know before it reserves names.
	registers bool
	wrap      func(*pkgData) elpsutil.Package
}{
	{"type/plain", false, func(d *pkgData) elpsutil.Package { return plainPkg{d} }},
	{"type/defs", true, func(d *pkgData) elpsutil.Package { return defsPkg{d} }},
	{"type/init", false, func(d *pkgData) elpsutil.Package { return initPkg{d} }},
	{"type/full", true, func(d *pkgData) elpsutil.Package { return fullPkg{d} }},
}

// ---------------------------------------------------------------------------
// tables the spec indexes into
// ---------------------------------------------------------------------------

// pkgNames are package names worth starting from. Each is here for a reason
// the fuzzer would take a long time to rediscover by mutating bytes.
var pkgNames = []string{
	"fuzzpkg",  // the ordinary case
	"fuzzpkg2", // a second ordinary one, so two packages can coexist
	"user",     // THE package Load switches back into; registering into it is
	//             what substrate does for its top-level builtins
	"lisp",  // the Lang package: symbols added here resolve from everywhere
	"",      // empty; DefinePackage accepts it and it becomes a registry key
	"a:b",   // ':' is the qualified-symbol separator
	"\x00",  // NUL in a registry key and in every FID built from it
	"é",     // multi-byte, so any byte-indexing of a package name shows up
	" ",     // whitespace-only
	"fuzz-", // trailing separator-ish
}

// defNames are symbol names. Names that collide are deliberately listed:
// PackageLoader now refuses them with an install-time error where lisp would
// have panicked, and having them in the table is how those refusal paths
// (reject/collision, reject/reserved-name) stay exercised.
var defNames = []string{
	"fz-a", "fz-b", "fz-c", "fz-d",
	"true",   // reserved: Package.get answers it without a symbol-table lookup
	"+",      // collides in the lisp package only
	"car",    // likewise, and a name an embedder might plausibly want back
	"",       // empty symbol name
	"a:b",    // embedded package separator
	"fz\x00", // NUL
	"fz é",   // multi-byte and a space
	"&rest",  // a control symbol as a function name
	"'",      // the quote character
}

// formalsShared is ONE LVal reused by the formalsShared shape below. It is a
// package-level var rather than a per-input allocation precisely so that two
// registrations in the same input alias it; see the comment on Formals
// aliasing in the header.
var formalsShared = lisp.Formals("shared-x", "shared-y")

// formalsTable is every formals shape the fuzzer can attach to a def,
// including the Go nil that used to be excluded (see "Go nil, which used to
// be excluded" in the header).
//
// Shapes with rejected set are the ones PackageLoader refuses at install time
// (checkFormals). They still matter twice over: handed to elpsutil they fuzz
// the refusal itself (buildDefs records the expectation; markRejectEvidence
// marks formals-reject/<label> only when a load error proves it), and --
// except go-nil -- a copy is registered through the validation bypass so
// LEnv.bind still meets the shape as data; see "Formals that elpsutil now
// refuses" in the header. TestFormalsTableRejectionFlags pins every rejected
// flag and problem string against elpsutil.Validate so this table cannot
// drift from the real rule.
var formalsTable = []struct {
	label string
	// problem is a substring of checkFormals' report for this shape, distinct
	// per shape, used to attribute a load error to this entry before its
	// formals-reject/<label> may be marked. Empty for accepted shapes.
	problem string
	make    func() *lisp.LVal
	// rejected marks shapes PackageLoader refuses at install time.
	rejected bool
	// bindBypass routes a copy of the def into LEnv.bind through
	// env.AddBuiltins(true, ...). False only for go-nil: calling a builtin
	// whose formals are Go nil still nil-derefs inside bind, and the harness
	// asserts IsInternalPanic on every call result.
	bindBypass bool
}{
	{label: "nullary", make: func() *lisp.LVal { return lisp.Formals() }},
	{label: "unary", make: func() *lisp.LVal { return lisp.Formals("x") }},
	{label: "binary", make: func() *lisp.LVal { return lisp.Formals("x", "y") }},
	{label: "rest", make: func() *lisp.LVal { return lisp.Formals("&rest", "xs") }},
	{label: "opt", make: func() *lisp.LVal { return lisp.Formals("x", "&optional", "y") }},
	{label: "key", make: func() *lisp.LVal { return lisp.Formals("&key", "k") }},
	// lisp.Formals returns an LError for a misplaced control symbol. An
	// embedder that ignores the return value stores the ERROR as its formals.
	{label: "error-value", rejected: true, bindBypass: true,
		problem: "formals are an error value",
		make:    func() *lisp.LVal { return lisp.Formals("&rest") }},
	// A control symbol with nothing after it: accepted at registration,
	// rejected by bindFormalNext at call time.
	{label: "dangling-opt", make: func() *lisp.LVal { return lisp.Formals("&optional") }},
	{label: "dangling-key", make: func() *lisp.LVal { return lisp.Formals("&key") }},
	// Duplicate formal names: no error anywhere, second binding wins.
	{label: "duplicate", make: func() *lisp.LVal { return lisp.Formals("x", "x") }},
	// Not a list at all. bind reads fun.Cells[0].Cells, which is empty for
	// these, so through the bypass they silently become nullary.
	{label: "scalar-int", rejected: true, bindBypass: true,
		problem: "formals are a int, not a list",
		make:    func() *lisp.LVal { return lisp.Int(3) }},
	{label: "scalar-string", rejected: true, bindBypass: true,
		problem: "formals are a string, not a list",
		make:    func() *lisp.LVal { return lisp.String("x") }},
	// A list whose cells are not symbols.
	{label: "non-symbol-cells", rejected: true, bindBypass: true,
		problem: "formal argument 0 is a int, not a symbol",
		make: func() *lisp.LVal {
			return lisp.QExpr([]*lisp.LVal{lisp.Int(1), lisp.String("s")})
		}},
	// An SExpr where a QExpr is expected. checkFormals accepts it (same LType,
	// symbol cells), so it reaches bind on the ordinary path.
	{label: "sexpr", make: func() *lisp.LVal {
		return lisp.SExpr([]*lisp.LVal{lisp.Symbol("x")})
	}},
	// Nested list as a formal.
	{label: "nested", rejected: true, bindBypass: true,
		problem: "formal argument 0 is a list, not a symbol",
		make: func() *lisp.LVal {
			return lisp.QExpr([]*lisp.LVal{lisp.QExpr([]*lisp.LVal{lisp.Symbol("x")})})
		}},
	{label: "shared", make: func() *lisp.LVal { return formalsShared }},
	// Go nil where lisp.Formals() belongs: the plausible mis-spelling of
	// "takes no arguments". PackageLoader refuses it at install time; see
	// TestNilContractMistakesAreErrors. No bindBypass -- see that field.
	{label: "go-nil", rejected: true,
		problem: "formals are nil",
		make:    func() *lisp.LVal { return nil }},
}

// bodyTable is every builtin body the fuzzer can attach to a def.
//
// Every one of these returns a non-nil *LVal and none of them panics
// deliberately: a body that panics would make the IsInternalPanic assertion
// meaningless, and FuzzEval already owns that experiment with its `fuzz-panic`
// builtin. What these bodies DO exercise is the thing a real embedded builtin
// does and nothing else in this repository's fuzz corpus does -- re-entering
// the interpreter, and mutating interpreter state, from inside a call.
var bodyTable = []struct {
	label string
	fn    lisp.LBuiltin
}{
	{"nil", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return lisp.Nil()
	}},
	// Returns the argument list the binder built. That LVal is handed straight
	// back into evaluation, so anything the interpreter then does to it is
	// done to a value the binder still considers its own.
	{"echo", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return args
	}},
	{"copy", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return args.Copy()
	}},
	{"quote", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return lisp.Quote(args)
	}},
	{"first", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		if args.Len() == 0 {
			return lisp.Nil()
		}
		return args.Cells[0]
	}},
	{"error", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return env.Errorf("fuzz builtin refused %d argument(s)", args.Len())
	}},
	// Re-entrant evaluation of caller-supplied data. This is the shape of
	// every substrate builtin that interprets its arguments.
	{"eval-args", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return env.Eval(args)
	}},
	{"eval-first", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		if args.Len() == 0 {
			return lisp.Nil()
		}
		return env.Eval(args.Cells[0])
	}},
	// Nested interpretation from inside a call. The source is fixed, so it
	// cannot recurse into the fuzzer's own functions; the point is the nesting
	// itself, not what is nested.
	{"load-string", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return env.LoadString("elpsutil-fuzz-builtin", "(+ 1 2)")
	}},
	// Mutates the interpreter's current package from inside a call. Legal --
	// the in-package builtin does it -- and a builtin that does it without
	// restoring is a realistic embedder bug whose blast radius is everything
	// evaluated afterwards.
	{"switch-package", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return env.InPackage(lisp.String(lisp.DefaultUserPackage))
	}},
	// Renders caller-controlled values. LVal.String walks the whole value.
	{"render", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return lisp.String(args.String())
	}},
	{"symbol", func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return lisp.Symbol("fz-returned-symbol")
	}},
}

// initTable is every PackageInit body. `init/error` is the only way to reach
// PackageLoader's `if e.Type == lisp.LError { return e }` after the init call,
// and `init/switch` is the only way to reach the state where the package the
// builtins land in is NOT the package PackageLoader selected -- PackageLoader
// does not re-establish the package after init returns.
var initTable = []struct {
	label string
	fn    func(*lisp.LEnv) *lisp.LVal
}{
	{"init/nil", func(env *lisp.LEnv) *lisp.LVal { return lisp.Nil() }},
	{"init/error", func(env *lisp.LEnv) *lisp.LVal { return env.Errorf("fuzz package init refused") }},
	{"init/eval", func(env *lisp.LEnv) *lisp.LVal { return env.LoadString("elpsutil-fuzz-init", "(+ 1 2)") }},
	{"init/switch", func(env *lisp.LEnv) *lisp.LVal {
		env.Runtime.Registry.DefinePackage(initSwitchPackage)
		return env.InPackage(lisp.String(initSwitchPackage))
	}},
	{"init/doc", func(env *lisp.LEnv) *lisp.LVal {
		env.SetPackageDoc("set from init")
		return lisp.Nil()
	}},
	// Removes the user package from the registry. This is the ONLY way to
	// reach the `if lerr.Type == lisp.LError { return lerr }` guarding
	// InPackage(user) in Load and LoadAll -- every other argument elpsutil
	// passes to DefinePackage/InPackage it constructed itself and cannot be
	// wrong.
	//
	// It used to be a one-line `delete(Registry.Packages, ...)`. #382
	// unexported the registry's map and added no removal method, so that
	// write is gone and an embedder can no longer reach this state by
	// mutating the map. The state itself is still reachable -- Runtime.Registry
	// is an assignable field -- so the case is rebuilt rather than dropped:
	// swapping in a registry that carries every package EXCEPT user leaves
	// Load facing exactly the condition it is being asked to report.
	{"init/drop-user", func(env *lisp.LEnv) *lisp.LVal {
		old := env.Runtime.Registry
		fresh := lisp.NewRegistry()
		fresh.Lang = old.Lang
		for _, name := range old.PackageNames() {
			if name == lisp.DefaultUserPackage {
				continue
			}
			fresh.AddPackage(old.Package(name))
		}
		env.Runtime.Registry = fresh
		return lisp.Nil()
	}},
	// A PackageInit that returns a Go nil *LVal: the loader-side nil mistake,
	// and the only fuzzer-reachable route to loaderResult's nil branch inside
	// PackageLoader. Reported as an error naming the package and PackageInit;
	// see TestNilContractMistakesAreErrors.
	{"init/go-nil", func(env *lisp.LEnv) *lisp.LVal { return nil }},
}

// strategyLabels are the four ways the generated packages get installed. Every
// one goes through elpsutil.Load, because PackageLoader on its own leaves the
// interpreter inside the package it just defined -- returning to `user` is
// Load's job, and asserting that postcondition (see install) requires it.
var strategyLabels = []string{
	"strategy/load-each",
	"strategy/library-loader",
	"strategy/load-all",
	"strategy/nested",
}

// ---------------------------------------------------------------------------
// decoding the spec
// ---------------------------------------------------------------------------

// specReader hands out the spec's bytes. Past the end it yields zeroes rather
// than wrapping: wrapping makes a short spec describe an unboundedly long
// install, and zeroes make truncation a fixed point, which is what lets the
// fuzzer's own minimiser converge.
type specReader struct {
	b []byte
	i int
}

func (r *specReader) next() byte {
	if r.i >= len(r.b) {
		r.i++
		return 0
	}
	b := r.b[r.i]
	r.i++
	return b
}

func (r *specReader) take(n int) []byte {
	out := make([]byte, n)
	for i := range out {
		out[i] = r.next()
	}
	return out
}

// name reads either a table entry or a run of raw spec bytes. The high bit
// selects: raw names are how a defect keyed on a byte no table author thought
// of still gets found, and the table is how the fuzzer reaches the interesting
// constants without having to spell them.
func (r *specReader) name(tbl []string) string {
	b := r.next()
	if b&0x80 == 0 {
		return tbl[int(b)%len(tbl)]
	}
	n := int(r.next()) % (maxNameBytes + 1)
	return string(r.take(n))
}

// ---------------------------------------------------------------------------
// the harness
// ---------------------------------------------------------------------------

// install is one fuzzer-described embedding: a set of generated packages, the
// loader strategy that installs them, and the environment they land in.
type install struct {
	env *lisp.LEnv

	// reached records which labelled features this input exercised, for
	// TestElpsutilFuzzReachesEveryFeature.
	reached map[string]bool

	// ndefs counts defs decoded from the spec and handed to elpsutil. The
	// reach guard asserts it is non-zero across the seed corpus, so a decoder
	// change that quietly stopped describing defs would be caught. A counted
	// def may still be refused at install time -- that refusal is part of
	// what the target fuzzes.
	ndefs int

	// callable is every symbol the spec attached to a def, qualified, plus
	// the bind-bypass names, so the guards can call what the install
	// described. A name whose package failed to load never registered, and
	// calling it yields an unbound-symbol error, which is itself a fine
	// input.
	callable []string

	// expectRejects records each def the spec gave an install-rejected
	// formals shape, with the evidence markRejectEvidence must find in a load
	// error before the shape's formals-reject/<label> may be marked.
	expectRejects []expectReject

	// nbypass numbers the bind-bypass registrations so their names are unique
	// within this install; see buildDefs.
	nbypass int
}

// expectReject is one predicted install-time refusal: the def's name as the
// spec chose it, the formals shape's label, and the shape's problem text as
// checkFormals reports it.
type expectReject struct {
	defName string
	label   string
	problem string
}

func (in *install) mark(label string) { in.reached[label] = true }

// plainSymbol reports whether s can appear verbatim in source text and read
// back as the same symbol. Deliberately strict: the point of qualify is to
// produce a call that PARSES, and a name that needs escaping teaches the
// evaluator nothing that the parser targets do not already cover.
func plainSymbol(s string) bool {
	if s == "" || (s[0] >= '0' && s[0] <= '9') {
		return false
	}
	for _, r := range s {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9':
		case r == '-', r == '_':
		default:
			return false
		}
	}
	return true
}

// qualify renders the symbol that calls a registered function. Names too
// exotic to spell are still registered and still reachable through the
// package's symbol table -- they are simply not called by autoCall.
func qualify(pkgName, name string) (string, bool) {
	if !plainSymbol(name) {
		return "", false
	}
	if pkgName == lisp.DefaultUserPackage || pkgName == "lisp" {
		return name, true
	}
	if !plainSymbol(pkgName) {
		return "", false
	}
	return pkgName + ":" + name, true
}

// buildDefs decodes n defs for one package.
func (in *install) buildDefs(r *specReader, pkgName string, n int) []lisp.LBuiltinDef {
	if n == 0 {
		return nil
	}
	defs := make([]lisp.LBuiltinDef, 0, n)
	for range n {
		name := r.name(defNames)
		fsel := formalsTable[int(r.next())%len(formalsTable)]
		bsel := bodyTable[int(r.next())%len(bodyTable)]
		if fsel.rejected {
			// The def still goes to elpsutil below -- the refusal is part of
			// what this target fuzzes -- but formals/<label> may NOT be
			// marked here: the shape will not reach LEnv.bind through
			// elpsutil, and a coverage label must not claim more than the
			// harness does. markRejectEvidence marks formals-reject/<label>
			// instead, when a load error proves the refusal happened.
			in.expectRejects = append(in.expectRejects, expectReject{
				defName: name, label: fsel.label, problem: fsel.problem,
			})
			if fsel.bindBypass {
				// Register a copy through the documented validation bypass so
				// bind still meets the shape as data (see the header). This
				// runs at decode time, before any loader: the env is fresh,
				// the current package is `user`, and fz-bind-N cannot be
				// bound yet, so AddBuiltins -- which panics on collision, see
				// TestCollisionPanicIsReal -- cannot be handed one. A
				// fuzzer-chosen def that later collides with this name is
				// refused by PackageLoader like any other collision.
				bname := fmt.Sprintf("fz-bind-%d", in.nbypass)
				in.nbypass++
				in.env.AddBuiltins(true, elpsutil.Function(bname, fsel.make(), bsel.fn))
				in.mark("formals/" + fsel.label)
				in.mark("body/" + bsel.label)
				in.callable = append(in.callable, bname)
			}
		} else {
			in.mark("formals/" + fsel.label)
			in.mark("body/" + bsel.label)
		}
		// Nothing is filtered: a def whose formals are malformed, or whose
		// name collides -- with another def in this input, with a symbol
		// already bound in the target package, or with the reserved constants
		// -- goes to elpsutil anyway, and the install-time refusal it
		// produces is attributed to its label by markRejectEvidence.
		defs = append(defs, elpsutil.Function(name, fsel.make(), bsel.fn))
		in.ndefs++
		if sym, ok := qualify(pkgName, name); ok {
			in.callable = append(in.callable, sym)
		}
	}
	return defs
}

// build decodes the whole spec into loaders, ready to be run.
func (in *install) build(spec []byte) []elpsutil.Loader {
	r := &specReader{b: spec}
	npkg := 1 + int(r.next())%maxPackages

	loaders := make([]elpsutil.Loader, 0, npkg)
	pkgs := make([]elpsutil.Package, 0, npkg)
	for range npkg {
		ty := pkgTypes[int(r.next())%len(pkgTypes)]
		name := r.name(pkgNames)
		initSel := initTable[int(r.next())%len(initTable)]
		nb := int(r.next()) % (maxDefs + 1)
		no := int(r.next()) % (maxDefs + 1)
		nm := int(r.next()) % (maxDefs + 1)

		d := &pkgData{
			name: name,
			doc:  string(r.take(int(r.next()) % 8)),
			init: initSel.fn,
		}
		if ty.registers {
			d.builtins = in.buildDefs(r, name, nb)
			d.ops = in.buildDefs(r, name, no)
			d.macros = in.buildDefs(r, name, nm)
		}
		in.mark(ty.label)
		if ty.label == "type/init" || ty.label == "type/full" {
			in.mark(initSel.label)
		}
		p := ty.wrap(d)
		pkgs = append(pkgs, p)
		loaders = append(loaders, elpsutil.PackageLoader(p))
	}

	// Which entry point installs them. Every branch funnels through
	// elpsutil.Load so the "back in the user package" postcondition applies.
	switch int(r.next()) % len(strategyLabels) {
	case 0:
		in.mark(strategyLabels[0])
		return loaders
	case 1:
		in.mark(strategyLabels[1])
		return []elpsutil.Loader{elpsutil.LibraryLoader(pkgs...)}
	case 2:
		in.mark(strategyLabels[2])
		return []elpsutil.Loader{elpsutil.LoadAll(loaders...)}
	default:
		in.mark(strategyLabels[3])
		// LoadAll over a LibraryLoader: both aggregate loaders on one path,
		// which is how substrate composes a library out of several Go
		// packages and then loads it alongside others.
		//
		// The packages are SPLIT between the two rather than passed to both.
		// Passing both would install every package twice, and the second pass
		// re-registers names the first pass just bound -- a collision
		// PackageLoader now refuses with an error, so no nested input could
		// ever load cleanly. The split keeps every registration unique so
		// this strategy exercises the success path too; the fuzzer is free to
		// mutate specs into colliding shapes on its own.
		k := max(1, npkg/2)
		return []elpsutil.Loader{elpsutil.LoadAll(
			elpsutil.LibraryLoader(pkgs[:k]...),
			elpsutil.LoadAll(loaders[k:]...),
		)}
	}
}

// newInstall builds a fully loaded environment. Runtime.Library is left nil so
// load-file errors instead of reading the filesystem.
func newInstall(env *lisp.LEnv) *install {
	return &install{
		env:     env,
		reached: map[string]bool{},
	}
}

func newEnv() (*lisp.LEnv, *lisp.LVal) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	if rc := lisp.InitializeUserEnv(env,
		lisp.WithMaxSteps(fuzzMaxSteps),
		lisp.WithMaxTailIterations(fuzzMaxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(fuzzMaxPhysicalHeight),
		lisp.WithMaxAlloc(fuzzMaxAlloc),
		lisp.WithMaxMacroExpansionDepth(fuzzMacroDepth),
	); rc.Type == lisp.LError {
		return nil, rc
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, rc
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, rc
	}
	return env, nil
}

// outcome is what one input produced, carried back off the worker goroutine.
type outcome struct {
	// loadResults is one *LVal per elpsutil.Load call.
	loadResults []*lisp.LVal
	// pkgAfterLoad is the interpreter's current package name once every load
	// that SUCCEEDED has run. Load's contract is that it switches back into
	// the user package; see the assertion in check.
	pkgAfterLoad string
	// allLoadsOK records whether every load returned a non-error, which is the
	// precondition for the postcondition above.
	allLoadsOK bool
	// evalResult is what src evaluated to, or nil if src was never evaluated.
	evalResult *lisp.LVal
	// autoResults is what each autoCall expression evaluated to, paired with
	// the expression, so a failure names the call that produced it.
	autoResults []autoResult
}

type autoResult struct {
	expr string
	res  *lisp.LVal
}

// autoCall invokes every function the install registered.
//
// Without this the target has a hole that the seed corpus HIDES. spec and src
// are mutated independently, so after a few generations the program in src has
// almost no chance of naming a symbol the spec happened to register: the seeds
// pair them by hand, the mutator does not, and the half of this target that
// exercises argument binding against fuzzer-chosen formals would quietly stop
// running while the corpus kept growing on install-side coverage alone. That
// is the shape of the #348 failure -- whole functions starved by how the seeds
// were arranged, invisible without measuring -- so the calls are generated
// from what was actually installed rather than hoped for from src.
//
// Each call is its own evaluation, so an arity error (the common case for a
// deliberately malformed formals list) ends that call rather than the rest.
// Two arities per symbol: zero arguments and two, which straddles every
// formals shape in the table.
func (in *install) autoCall(ctx context.Context) []autoResult {
	out := make([]autoResult, 0, 2*maxAutoCalls)
	for i, sym := range in.callable {
		if i >= maxAutoCalls {
			break
		}
		for _, expr := range []string{"(" + sym + ")", "(" + sym + " 1 2)"} {
			out = append(out, autoResult{expr, in.env.LoadStringContext(ctx, "autocall", expr)})
		}
	}
	return out
}

// run performs the install and then evaluates src. It runs on its own
// goroutine under the watchdog; it must not touch *testing.T.
func (in *install) run(ctx context.Context, loaders []elpsutil.Loader, src []byte) *outcome {
	out := &outcome{allLoadsOK: true}
	for _, l := range loaders {
		rc := elpsutil.Load(in.env, l)
		out.loadResults = append(out.loadResults, rc)
		if rc == nil || rc.Type == lisp.LError {
			out.allLoadsOK = false
		}
	}
	out.pkgAfterLoad = in.env.Runtime.Package.Name
	if out.allLoadsOK {
		in.mark("load/ok")
	} else {
		in.mark("load/error")
	}
	out.evalResult = in.env.LoadStringContext(ctx, "fuzz", string(src))
	// After src, not before: src may redefine, shadow or package-switch, and
	// calling the installed functions in whatever state it left behind is
	// strictly more interesting than calling them in a pristine one.
	out.autoResults = in.autoCall(ctx)
	if len(out.autoResults) > 0 {
		in.mark("autocall/ran")
	}
	return out
}

// fatalf is the subset of *testing.T the harness needs, so the same code
// serves the fuzz target and the ordinary corpus tests.
type fatalf interface {
	Helper()
	Fatalf(format string, args ...any)
	Skipf(format string, args ...any)
}

// runBudgeted installs and evaluates under the watchdog, and applies every
// assertion. Returns the install so callers can read `reached` back.
func runBudgeted(t fatalf, spec, src []byte) *install {
	t.Helper()

	env, rc := newEnv()
	if rc != nil {
		t.Fatalf("could not build the elpsutil fuzz environment: %v", rc)
		return nil
	}
	in := newInstall(env)

	// The spec is decoded on THIS goroutine, before the worker starts:
	// buildDefs registers the bind-bypass builtins straight into the
	// environment, and nothing else may be touching the registry at the time.
	loaders := in.build(spec)

	ctx, cancel := context.WithTimeout(context.Background(), fuzzDeadline)
	defer cancel()

	ch := make(chan *outcome, 1)
	go func() { ch <- in.run(ctx, loaders, src) }()

	budget := fuzzwatch.New(watchdogTimeout)
	wait := budget.Total()
	var out *outcome
	for out == nil {
		select {
		case out = <-ch:
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				t.Skipf("no verdict: the process was starved throughout (%s)", report)
				return nil
			default:
				t.Fatalf("the install did not terminate within %s of SCHEDULED time"+
					" despite a %s context deadline and a %d-step budget (%s)"+
					"\n--- spec (%d bytes) ---\n%q\n--- src (%d bytes) ---\n%q",
					budget.Total(), fuzzDeadline, int64(fuzzMaxSteps), report,
					len(spec), spec, len(src), src)
				return nil
			}
		}
	}

	in.check(t, out, spec, src)
	return in
}

// markRejectEvidence turns install-time refusals into coverage marks, so that
// a reject/* label can only ever claim a refusal that demonstrably happened:
// each mark requires the refusal's own error text in a load result. Nothing
// here asserts -- an input with no refusals simply marks nothing.
func (in *install) markRejectEvidence(results []*lisp.LVal) {
	for _, rc := range results {
		if rc == nil || rc.Type != lisp.LError {
			continue
		}
		msg := rc.String()
		// The two refusals that replaced lisp's "symbol already defined"
		// panic on the elpsutil path; the strings are checkPackageName's.
		if strings.Contains(msg, "already defined in package") {
			in.mark("reject/collision")
		}
		if strings.Contains(msg, "is a reserved constant") {
			in.mark("reject/reserved-name")
		}
		// loaderResult's report of a loader (here: a PackageInit) returning a
		// Go nil *LVal instead of lisp.Nil(); the string is loaderResult's.
		if strings.Contains(msg, "returned a nil *LVal") {
			in.mark("loader-nil/reported")
		}
		// Formals refusals are attributed to the table entry that predicted
		// them: the error must name the def AND carry the shape's own problem
		// text. Problem strings are distinct per shape, so a mark can only be
		// earned by the refusal it claims.
		for _, er := range in.expectRejects {
			if strings.Contains(msg, strconv.Quote(er.defName)) &&
				strings.Contains(msg, er.problem) {
				in.mark("formals-reject/" + er.label)
			}
		}
	}
}

// check applies every assertion this target makes.
func (in *install) check(t fatalf, out *outcome, spec, src []byte) {
	t.Helper()

	in.markRejectEvidence(out.loadResults)

	for i, rc := range out.loadResults {
		if rc == nil {
			t.Fatalf("elpsutil.Load returned a nil *LVal (call %d)"+
				"\n--- spec (%d bytes) ---\n%q", i, len(spec), spec)
			return
		}
		if lisp.IsInternalPanic(rc) {
			t.Fatalf("elpsutil.Load recovered a Go panic (a host-code defect, not a lisp error)"+
				"\n--- error ---\n%v\n--- spec (%d bytes) ---\n%q", rc, len(spec), spec)
			return
		}
		// A load either succeeds (nil) or reports an error. Anything else
		// means a loader's return value reached the caller unexamined.
		if rc.Type != lisp.LError && !rc.IsNil() {
			t.Fatalf("elpsutil.Load returned neither nil nor an error: %v (type %v)"+
				"\n--- spec (%d bytes) ---\n%q", rc, rc.Type, len(spec), spec)
			return
		}
		_ = rc.String()
	}

	// Load's documented postcondition: "after a Loader executes
	// LEnv.InPackage() is executed to switch back into the user package", so
	// symbols defined next land in `user` rather than in whichever package the
	// last loader happened to leave selected. Only claimed on success --
	// Load returns early on an error and makes no promise then.
	if out.allLoadsOK && out.pkgAfterLoad != lisp.DefaultUserPackage {
		t.Fatalf("every elpsutil.Load succeeded but the interpreter was left in package %q,"+
			" not %q. Load exists to restore the user package; code loaded after this"+
			" would define its symbols in the wrong place"+
			"\n--- spec (%d bytes) ---\n%q", out.pkgAfterLoad, lisp.DefaultUserPackage, len(spec), spec)
		return
	}

	res := out.evalResult
	if res == nil {
		t.Fatalf("evaluation returned a nil LVal\n--- spec (%d bytes) ---\n%q"+
			"\n--- src (%d bytes) ---\n%q", len(spec), spec, len(src), src)
		return
	}
	if lisp.IsInternalPanic(res) {
		t.Fatalf("evaluation against fuzzer-installed builtins recovered a Go panic"+
			" (a host-code defect, not a lisp error)"+
			"\n--- error ---\n%v\n--- spec (%d bytes) ---\n%q\n--- src (%d bytes) ---\n%q",
			res, len(spec), spec, len(src), src)
		return
	}
	// String() walks the whole value; a result that cannot be rendered is a
	// defect in its own right.
	_ = res.String()

	// The same three properties for every call autoCall made. These are the
	// calls that bind fuzzer-chosen arguments to fuzzer-chosen formals, so
	// this is where a malformed formals list that survived registration
	// finally gets examined.
	for _, ar := range out.autoResults {
		if ar.res == nil {
			t.Fatalf("calling an installed function returned a nil LVal"+
				"\n--- call ---\n%s\n--- spec (%d bytes) ---\n%q", ar.expr, len(spec), spec)
			return
		}
		if lisp.IsInternalPanic(ar.res) {
			t.Fatalf("calling an installed function recovered a Go panic"+
				" (a host-code defect, not a lisp error)"+
				"\n--- call ---\n%s\n--- error ---\n%v"+
				"\n--- spec (%d bytes) ---\n%q\n--- src (%d bytes) ---\n%q",
				ar.expr, ar.res, len(spec), spec, len(src), src)
			return
		}
		_ = ar.res.String()
	}
}

// ---------------------------------------------------------------------------
// seeds
// ---------------------------------------------------------------------------

// spec assembles a package descriptor byte-for-byte, so a seed says what it
// means instead of hoping a byte string decodes the way its comment claims.
// The field order here MUST match specReader's consumption order in build.
type seedDef struct {
	name    string // index into defNames, or "" for raw
	formals string
	body    string
}

type seedPkg struct {
	typ      string
	name     string
	init     string
	builtins []seedDef
	ops      []seedDef
	macros   []seedDef
	doc      string
}

func idx(tbl []string, s string) byte {
	for i, v := range tbl {
		if v == s {
			return byte(i)
		}
	}
	panic("elpsutil fuzz seed: no such table entry: " + s)
}

func idxPkgType(label string) byte {
	for i, t := range pkgTypes {
		if t.label == label {
			return byte(i)
		}
	}
	panic("elpsutil fuzz seed: no such package type: " + label)
}

func idxFormals(label string) byte {
	for i, f := range formalsTable {
		if f.label == label {
			return byte(i)
		}
	}
	panic("elpsutil fuzz seed: no such formals shape: " + label)
}

func idxBody(label string) byte {
	for i, b := range bodyTable {
		if b.label == label {
			return byte(i)
		}
	}
	panic("elpsutil fuzz seed: no such body: " + label)
}

func idxInit(label string) byte {
	for i, v := range initTable {
		if v.label == label {
			return byte(i)
		}
	}
	panic("elpsutil fuzz seed: no such init: " + label)
}

func encodeDefs(out []byte, defs []seedDef) []byte {
	for _, d := range defs {
		out = append(out, idx(defNames, d.name), idxFormals(d.formals), idxBody(d.body))
	}
	return out
}

// buildSpec encodes a whole spec. strategy is an index into strategyLabels.
func buildSpec(strategy int, pkgs ...seedPkg) []byte {
	if len(pkgs) == 0 || len(pkgs) > maxPackages {
		panic(fmt.Sprintf("elpsutil fuzz seed: %d packages, want 1..%d", len(pkgs), maxPackages))
	}
	out := []byte{byte(len(pkgs) - 1)}
	for _, p := range pkgs {
		// The decoder reads the doc length modulo 8, so a longer doc would
		// desynchronise every field after it and the seed would silently
		// describe something other than what it says.
		if len(p.doc) >= 8 {
			panic("elpsutil fuzz seed: package doc must be under 8 bytes: " + p.doc)
		}
		if len(p.builtins) > maxDefs || len(p.ops) > maxDefs || len(p.macros) > maxDefs {
			panic(fmt.Sprintf("elpsutil fuzz seed: more than %d defs of one kind", maxDefs))
		}
		out = append(out,
			idxPkgType(p.typ),
			idx(pkgNames, p.name),
			idxInit(p.init),
			byte(len(p.builtins)),
			byte(len(p.ops)),
			byte(len(p.macros)),
			byte(len(p.doc)),
		)
		out = append(out, []byte(p.doc)...)
		out = encodeDefs(out, p.builtins)
		out = encodeDefs(out, p.ops)
		out = encodeDefs(out, p.macros)
	}
	return append(out, byte(strategy))
}

// FuzzElpsutilEmbed installs fuzzer-described Go packages through elpsutil and
// then runs fuzzer-chosen lisp against them.
//
// Asserted, and only this:
//
//  1. Nothing panics. There is no recover in elpsutil and none between it and
//     this function, so an install panic is an ordinary crash that `go test
//     -fuzz` attributes to its input. The one panic that IS designed in --
//     lisp's "symbol already defined" -- is answered rather than avoided:
//     PackageLoader refuses the colliding definition with an error before
//     lisp can panic, and no input is filtered to keep clear of it; see the
//     header, and TestCollisionPanicIsReal for the panic itself.
//  2. Every elpsutil.Load returns a non-nil *LVal that is either nil or an
//     LError, and never an internal-panic error.
//  3. When every load succeeded, the interpreter is back in the `user`
//     package. This is Load's entire reason to exist over PackageLoader.
//  4. Evaluating src against the installed builtins terminates (the watchdog),
//     returns a non-nil *LVal, never recovers a Go panic (IsInternalPanic),
//     and produces a renderable result.
//  5. The same three properties for every call autoCall makes, which is where
//     fuzzer-chosen arguments meet fuzzer-chosen formals.
//
// NOT asserted: that any package installs successfully, or that src evaluates
// without error. An LError is the right answer for almost all mutator output.
func FuzzElpsutilEmbed(f *testing.F) {
	add := func(spec []byte, src string) { f.Add(spec, []byte(src)) }

	// The workhorse: one ordinary package with one builtin of each kind, and a
	// program that calls all three.
	ordinary := buildSpec(0, seedPkg{
		typ: "type/full", name: "fuzzpkg", init: "init/nil", doc: "d",
		builtins: []seedDef{{"fz-a", "binary", "echo"}},
		ops:      []seedDef{{"fz-b", "rest", "quote"}},
		macros:   []seedDef{{"fz-c", "unary", "first"}},
	})
	add(ordinary, `(fuzzpkg:fz-a 1 2)`)
	add(ordinary, `(fuzzpkg:fz-b 1 2 3)`)
	add(ordinary, `(fuzzpkg:fz-c (+ 1 2))`)
	add(ordinary, `(list (fuzzpkg:fz-a 1 2) (fuzzpkg:fz-b) (fuzzpkg:fz-c 'x))`)
	// Wrong arity, in both directions, against every formals shape's binder.
	add(ordinary, `(fuzzpkg:fz-a)`)
	add(ordinary, `(fuzzpkg:fz-a 1 2 3 4 5)`)
	// The registered function as a VALUE rather than a call: funcall, apply
	// and map all route through funCall with a builtin LFun.
	add(ordinary, `(funcall fuzzpkg:fz-a 1 2)`)
	add(ordinary, `(apply fuzzpkg:fz-a '(1 2))`)
	add(ordinary, `(map 'list (lambda (x) (fuzzpkg:fz-c x)) '(1 2 3))`)
	// Render and inspect the installed function itself.
	add(ordinary, `(debug-print fuzzpkg:fz-a)`)
	add(ordinary, ``)

	// Every formals shape, one seed each, called with zero, one and three
	// arguments. The malformed shapes are the point: they are accepted at
	// registration and only examined by LEnv.bind at call time, so a seed that
	// never calls them covers nothing.
	for _, fs := range formalsTable {
		sp := buildSpec(0, seedPkg{
			typ: "type/defs", name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{"fz-a", fs.label, "echo"}},
		})
		add(sp, `(fuzzpkg:fz-a)`)
		add(sp, `(fuzzpkg:fz-a 1)`)
		add(sp, `(fuzzpkg:fz-a 1 2 3)`)
		add(sp, `(fuzzpkg:fz-a :k 1)`)
	}

	// Every body, called once, with an argument that is itself a call so the
	// re-entrant bodies have something to re-enter with.
	for _, b := range bodyTable {
		sp := buildSpec(0, seedPkg{
			typ: "type/defs", name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{"fz-a", "rest", b.label}},
		})
		add(sp, `(fuzzpkg:fz-a (+ 1 2))`)
		add(sp, `(fuzzpkg:fz-a 'x '(1 2 3))`)
	}

	// Every package type and every init body.
	for _, ty := range pkgTypes {
		for _, it := range initTable {
			add(buildSpec(0, seedPkg{
				typ: ty.label, name: "fuzzpkg", init: it.label,
				builtins: []seedDef{{"fz-a", "unary", "echo"}},
			}), `(fuzzpkg:fz-a 1)`)
		}
	}

	// Every loader strategy, over two packages, so LibraryLoader and LoadAll
	// have more than one thing to iterate.
	twoPkgs := []seedPkg{
		{typ: "type/full", name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{"fz-a", "unary", "echo"}}},
		{typ: "type/defs", name: "fuzzpkg2", init: "init/nil",
			builtins: []seedDef{{"fz-b", "unary", "eval-first"}}},
	}
	for i := range strategyLabels {
		add(buildSpec(i, twoPkgs...), `(list (fuzzpkg:fz-a 1) (fuzzpkg2:fz-b '(+ 1 2)))`)
	}

	// An init that fails, under each strategy: this is the only way into the
	// error-propagation branches of Load, LoadAll, LibraryLoader and
	// PackageLoader, and the only input for which the "back in user" assertion
	// is correctly NOT claimed.
	for i := range strategyLabels {
		add(buildSpec(i,
			seedPkg{typ: "type/full", name: "fuzzpkg", init: "init/error",
				builtins: []seedDef{{"fz-a", "unary", "echo"}}},
			seedPkg{typ: "type/defs", name: "fuzzpkg2", init: "init/nil",
				builtins: []seedDef{{"fz-b", "unary", "echo"}}},
		), `(fuzzpkg2:fz-b 1)`)
	}

	// An init that removes the user package, under each strategy. Load and
	// LoadAll each guard their own InPackage(user) call, and only the strategy
	// that routes through LoadAll reaches LoadAll's copy of that guard.
	for i := range strategyLabels {
		add(buildSpec(i,
			seedPkg{typ: "type/full", name: "fuzzpkg", init: "init/drop-user",
				builtins: []seedDef{{"fz-a", "unary", "echo"}}},
			seedPkg{typ: "type/defs", name: "fuzzpkg2", init: "init/nil",
				builtins: []seedDef{{"fz-b", "unary", "echo"}}},
		), `(fuzzpkg:fz-a 1)`)
	}

	// The aliasing case: ONE formals LVal shared by two registrations in two
	// different packages, both then called.
	add(buildSpec(1,
		seedPkg{typ: "type/defs", name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{"fz-a", "shared", "echo"}}},
		seedPkg{typ: "type/defs", name: "fuzzpkg2", init: "init/nil",
			builtins: []seedDef{{"fz-b", "shared", "echo"}}},
	), `(list (fuzzpkg:fz-a 1 2) (fuzzpkg2:fz-b 3 4) (fuzzpkg:fz-a 5 6))`)

	// Registering into packages that already exist, which is what an embedder
	// adding to `user` or extending `lisp` does. The colliding second def
	// makes each load fail with PackageLoader's install-time refusal (where
	// lisp used to panic); the first def is already registered by then, so
	// the seed covers both halves: a def landing in a populated package, and
	// the refusal naming the collision.
	add(buildSpec(0, seedPkg{
		typ: "type/defs", name: "user", init: "init/nil",
		builtins: []seedDef{{"fz-a", "unary", "echo"}, {"+", "unary", "echo"}},
	}), `(fz-a 1)`)
	add(buildSpec(0, seedPkg{
		typ: "type/defs", name: "lisp", init: "init/nil",
		builtins: []seedDef{{"fz-b", "unary", "echo"}, {"car", "unary", "echo"}},
	}), `(fz-b 1)`)
	// A duplicate within a single package: the second fz-a arrives after the
	// first is already in the symbol table, and the load is refused.
	add(buildSpec(0, seedPkg{
		typ: "type/defs", name: "fuzzpkg", init: "init/nil",
		builtins: []seedDef{{"fz-a", "unary", "echo"}, {"fz-a", "binary", "echo"}},
	}), `(fuzzpkg:fz-a 1)`)

	// Names that are legal registry keys but not legal source text. These
	// register and are never callable from src, which is the point: the value
	// still has to render, and the package still has to load.
	for _, n := range []string{"", "a:b", "\x00", "é", " ", "fuzz-"} {
		add(buildSpec(0, seedPkg{
			typ: "type/defs", name: n, init: "init/nil",
			builtins: []seedDef{{"fz-a", "unary", "echo"}, {"a:b", "unary", "echo"},
				{"", "nullary", "nil"}, {"fz\x00", "unary", "render"}},
		}), `(debug-print "loaded")`)
	}

	// An init that leaves the interpreter in another package: the builtins
	// that follow land there rather than in the package PackageLoader chose.
	add(buildSpec(0, seedPkg{
		typ: "type/full", name: "fuzzpkg", init: "init/switch",
		builtins: []seedDef{{"fz-a", "unary", "echo"}},
	}), `(fuzzpkg:fz-a 1)`)

	// Four packages sharing one name, so the same registry entry is written
	// four times by four different Package values.
	same := make([]seedPkg, 0, 4)
	for i, n := range []string{"fz-a", "fz-b", "fz-c", "fz-d"} {
		same = append(same, seedPkg{
			typ: pkgTypes[i%len(pkgTypes)].label, name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{n, "unary", "echo"}},
		})
	}
	add(buildSpec(1, same...), `(list (fuzzpkg:fz-b 1) (fuzzpkg:fz-d 2))`)

	// Raw (non-table) names, exercised through the high-bit branch of
	// specReader.name: package name "\xff\xfe" (invalid UTF-8) with a
	// two-byte symbol.
	add([]byte{
		0,                       // one package
		idxPkgType("type/defs"), // type
		0x80, 2, 0xff, 0xfe,     // raw package name
		idxInit("init/nil"), // init (unused for type/defs)
		1, 0, 0,             // 1 builtin, 0 ops, 0 macros
		0,                   // no doc
		0x80, 2, 0x41, 0x42, // raw symbol name "AB"
		idxFormals("unary"), idxBody("echo"),
		0, // strategy
	}, `(debug-print "raw")`)

	// Parser-adversarial src against a live install: small by construction
	// (see fuzzseed.Adversarial's own note on seed size), and what they cover
	// here is the evaluator meeting a malformed program in an environment
	// whose symbol table the fuzzer wrote.
	for _, seed := range fuzzseed.Adversarial() {
		add(ordinary, string(seed))
	}
	// Terminating, deterministic-error and adversarial elps programs,
	// likewise.
	for _, src := range fuzzseed.EvalTerminating() {
		add(ordinary, src)
	}
	for _, src := range fuzzseed.EvalErroring() {
		add(ordinary, src)
	}
	for _, src := range fuzzseed.EvalAdversarial() {
		add(ordinary, src)
	}
	// An empty spec: every field reads as zero, which must still describe a
	// loadable package rather than trip the decoder.
	add([]byte{}, `(+ 1 2)`)
	add(nil, ``)

	f.Fuzz(func(t *testing.T, spec, src []byte) {
		runBudgeted(t, spec, src)
	})
}

// ---------------------------------------------------------------------------
// guards
// ---------------------------------------------------------------------------

// requiredFeatures is every labelled path the seed corpus must reach on its
// own, before the fuzzer mutates anything.
//
// This is the guard that keeps the target honest, and the analogue of
// TestLSPFuzzScriptReachesEveryOp. Each label corresponds to a branch in
// elpsutil or to an input shape the header claims is covered:
//
//	strategy/*  -> Load, LoadAll, LibraryLoader, PackageLoader
//	type/plain  -> the !ok branch of packageInit (hence nopLoader),
//	               packageBuiltins, packageSpecialOps and packageMacros
//	type/full   -> the ok branch of all four
//	init/error  -> PackageLoader's `if e.Type == lisp.LError` after init
//	load/error  -> the error returns in Load, LoadAll and LibraryLoader
//	reject/*    -> PackageLoader refused a definition -- a name collision or
//	               a reserved constant -- with an error where lisp would have
//	               panicked; marked only on the refusal's own error text
//	loader-nil/reported -> loaderResult reported a loader returning a Go nil
//	               *LVal (via init/go-nil), likewise marked on the report's
//	               own error text
//
// #348 is why this exists: there, seed-script ordering silently starved whole
// functions, and measuring was the only way to see it.
var requiredFeatures = []string{
	"strategy/load-each", "strategy/library-loader", "strategy/load-all", "strategy/nested",
	"type/plain", "type/defs", "type/init", "type/full",
	"init/nil", "init/error", "init/eval", "init/switch", "init/doc", "init/drop-user",
	"init/go-nil", "loader-nil/reported",
	"load/ok", "load/error", "autocall/ran",
	"reject/collision", "reject/reserved-name",
}

// TestElpsutilFuzzReachesEveryFeature runs the seed corpus through the harness
// and asserts every entry in requiredFeatures was reached.
//
// The seeds are rebuilt here rather than read back out of *testing.F, which
// exposes no way to enumerate them. Keeping the two lists in one file is what
// makes that safe: a seed added to FuzzElpsutilEmbed that is meant to unlock a
// feature must also be added here, and the failure message says so.
func TestElpsutilFuzzReachesEveryFeature(t *testing.T) {
	t.Parallel()

	reached := map[string]bool{}
	total := 0
	for _, c := range guardCorpus() {
		in := runBudgeted(t, c.spec, []byte(c.src))
		if in == nil {
			continue
		}
		total += in.ndefs
		for k := range in.reached {
			reached[k] = true
		}
	}

	if total == 0 {
		t.Fatal("the guard corpus decoded NO defs at all: the spec decoder is describing" +
			" empty installs, so this target installs nothing and asserts nothing")
	}

	var missing []string
	for _, f := range requiredFeatures {
		if !reached[f] {
			missing = append(missing, f)
		}
	}
	if len(missing) > 0 {
		sort.Strings(missing)
		t.Fatalf("the guard corpus never reached: %v"+
			"\nEach of those labels is a branch in elpsutil (or an input shape the target's"+
			" header claims to cover) that no seed exercises. Left alone the fuzzer has to"+
			" rediscover it from random bytes, which is what silently starved whole functions"+
			" in #348.", missing)
	}

	// Every formals shape and every body must be reachable too, or the table
	// entry is dead weight. What "reachable" means depends on the shape:
	// accepted shapes must register through elpsutil (formals/<label>);
	// install-rejected shapes must demonstrably be refused by PackageLoader
	// (formals-reject/<label>, marked only on the refusal's own error text)
	// and, where the bypass applies, must also reach LEnv.bind through it
	// (formals/<label>, marked at the bypass registration).
	for _, fs := range formalsTable {
		if fs.rejected {
			if !reached["formals-reject/"+fs.label] {
				t.Errorf("no guard-corpus load error proved PackageLoader refuses formals shape %q", fs.label)
			}
			if fs.bindBypass && !reached["formals/"+fs.label] {
				t.Errorf("no guard-corpus input carried formals shape %q into LEnv.bind through the bypass", fs.label)
			}
		} else if !reached["formals/"+fs.label] {
			t.Errorf("no guard-corpus input registered a builtin with formals shape %q", fs.label)
		}
	}
	for _, b := range bodyTable {
		if !reached["body/"+b.label] {
			t.Errorf("no guard-corpus input registered a builtin with body %q", b.label)
		}
	}
}

// TestFormalsTableRejectionFlags pins each formalsTable entry's rejected flag
// and problem text against elpsutil's real validation rule, through the
// exported Validate. A shape the table calls accepted that elpsutil refuses
// would silently lose its bind coverage; a shape the table calls rejected
// that elpsutil accepts would make its formals-reject label unreachable and
// the guard above permanently red. Either way the drift is named here, at the
// table entry, rather than discovered from a coverage failure.
func TestFormalsTableRejectionFlags(t *testing.T) {
	t.Parallel()
	for _, fs := range formalsTable {
		t.Run(fs.label, func(t *testing.T) {
			t.Parallel()
			p := &pkgData{name: "probe", builtins: []lisp.LBuiltinDef{
				elpsutil.Function("probe-fn", fs.make(), bodyTable[0].fn),
			}}
			err := elpsutil.Validate(defsPkg{p})
			switch {
			case fs.rejected && err == nil:
				t.Fatalf("formalsTable calls shape %q install-rejected but elpsutil.Validate"+
					" accepts it; if checkFormals relaxed, clear the entry's rejected flag so"+
					" the shape goes back to reaching bind through elpsutil", fs.label)
			case fs.rejected && !strings.Contains(err.Error(), fs.problem):
				t.Fatalf("shape %q is refused but not with the recorded problem text %q: %v."+
					" markRejectEvidence matches on that text, so the entry's formals-reject"+
					" label could never be earned; update the entry's problem string", fs.label, fs.problem, err)
			case !fs.rejected && err != nil:
				t.Fatalf("formalsTable calls shape %q accepted but elpsutil.Validate refuses"+
					" it: %v. Set the entry's rejected flag (and problem text) or the shape's"+
					" coverage label claims a registration that can no longer happen", fs.label, err)
			}
			if fs.bindBypass && !fs.rejected {
				t.Fatalf("shape %q sets bindBypass without rejected; the bypass exists only"+
					" for shapes elpsutil refuses", fs.label)
			}
		})
	}
}

// guardCorpus is the subset of FuzzElpsutilEmbed's seeds the guards run. It is
// a function rather than a copy of the seed list so the two cannot drift on
// the axes the guards actually check: it enumerates every table entry and
// every strategy programmatically.
type guardCase struct {
	spec []byte
	src  string
}

func guardCorpus() []guardCase {
	var out []guardCase
	addSpec := func(spec []byte, src string) { out = append(out, guardCase{spec, src}) }

	for _, fs := range formalsTable {
		addSpec(buildSpec(0, seedPkg{
			typ: "type/defs", name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{"fz-a", fs.label, "echo"}},
		}), `(fuzzpkg:fz-a 1 2)`)
	}
	for _, b := range bodyTable {
		addSpec(buildSpec(0, seedPkg{
			typ: "type/defs", name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{"fz-a", "rest", b.label}},
		}), `(fuzzpkg:fz-a (+ 1 2))`)
	}
	for _, ty := range pkgTypes {
		for _, it := range initTable {
			addSpec(buildSpec(0, seedPkg{
				typ: ty.label, name: "fuzzpkg", init: it.label,
				builtins: []seedDef{{"fz-a", "unary", "echo"}},
			}), `(fuzzpkg:fz-a 1)`)
		}
	}
	twoPkgs := []seedPkg{
		{typ: "type/full", name: "fuzzpkg", init: "init/nil",
			builtins: []seedDef{{"fz-a", "unary", "echo"}}},
		{typ: "type/defs", name: "fuzzpkg2", init: "init/nil",
			builtins: []seedDef{{"fz-b", "unary", "eval-first"}}},
	}
	for i := range strategyLabels {
		addSpec(buildSpec(i, twoPkgs...), `(list (fuzzpkg:fz-a 1) (fuzzpkg2:fz-b '(+ 1 2)))`)
		addSpec(buildSpec(i,
			seedPkg{typ: "type/full", name: "fuzzpkg", init: "init/error",
				builtins: []seedDef{{"fz-a", "unary", "echo"}}},
			seedPkg{typ: "type/defs", name: "fuzzpkg2", init: "init/nil",
				builtins: []seedDef{{"fz-b", "unary", "echo"}}},
		), `(fuzzpkg2:fz-b 1)`)
		addSpec(buildSpec(i,
			seedPkg{typ: "type/full", name: "fuzzpkg", init: "init/drop-user",
				builtins: []seedDef{{"fz-a", "unary", "echo"}}},
			seedPkg{typ: "type/defs", name: "fuzzpkg2", init: "init/nil",
				builtins: []seedDef{{"fz-b", "unary", "echo"}}},
		), `(fuzzpkg:fz-a 1)`)
	}
	// Guarantees reject/reserved-name: `true` can never be bound, and
	// PackageLoader refuses it with an error where lisp would have panicked.
	addSpec(buildSpec(0, seedPkg{
		typ: "type/defs", name: "fuzzpkg", init: "init/nil",
		builtins: []seedDef{{"true", "unary", "echo"}, {"fz-a", "unary", "echo"}},
	}), `(fuzzpkg:fz-a 1)`)
	// Guarantees reject/collision: the second fz-a arrives after the first is
	// already in the package's symbol table.
	addSpec(buildSpec(0, seedPkg{
		typ: "type/defs", name: "fuzzpkg", init: "init/nil",
		builtins: []seedDef{{"fz-a", "unary", "echo"}, {"fz-a", "binary", "echo"}},
	}), `(fuzzpkg:fz-a 1)`)
	return out
}

// TestElpsutilSeedsTerminate is the false-positive guard: every guard-corpus
// input must reach a verdict. A seed that hangs would not fail loudly -- it
// would make every generation descended from it useless.
func TestElpsutilSeedsTerminate(t *testing.T) {
	t.Parallel()
	for i, c := range guardCorpus() {
		t.Run(fmt.Sprintf("case/%d", i), func(t *testing.T) {
			t.Parallel()
			runBudgeted(t, c.spec, []byte(c.src))
		})
	}
}

// TestCollisionPanicIsReal pins lisp's designed-in duplicate-registration
// panic at its source: env.AddBuiltins, env.AddSpecialOps and env.AddMacros,
// called directly.
//
// The panic is deliberately untouched by #351 but is no longer reachable
// through elpsutil -- PackageLoader refuses the colliding definition with an
// error first -- so going through elpsutil.Load here would test the refusal,
// not the panic. Two things still rest on the panic being real: elpsutil's
// checkPackageName exists only to mirror these functions' collision rules,
// and the harness registers its bind-bypass builtins through env.AddBuiltins
// on the assumption that a fresh name in a fresh environment cannot collide.
// If lisp's rule ever changes, update checkPackageName and this test
// together; a divergence between them means elpsutil refuses definitions lisp
// accepts, or stops refusing ones that panic.
func TestCollisionPanicIsReal(t *testing.T) {
	t.Parallel()

	kinds := []struct {
		name string
		add  func(env *lisp.LEnv, def lisp.LBuiltinDef)
	}{
		{"builtin", func(env *lisp.LEnv, def lisp.LBuiltinDef) { env.AddBuiltins(true, def) }},
		{"special-op", func(env *lisp.LEnv, def lisp.LBuiltinDef) { env.AddSpecialOps(true, def) }},
		{"macro", func(env *lisp.LEnv, def lisp.LBuiltinDef) { env.AddMacros(true, def) }},
	}
	cases := []struct {
		name string
		// pkg is the package registered into; defined first if absent.
		pkg string
		// names are registered in order and the LAST one must panic.
		names []string
	}{
		{"duplicate-symbol", "collide1", []string{"dup", "dup"}},
		{"reserved-symbol", "collide2", []string{lisp.TrueSymbol}},
		{"existing-lang-symbol", "lisp", []string{"car"}},
	}
	for _, kind := range kinds {
		for _, tc := range cases {
			t.Run(kind.name+"/"+tc.name, func(t *testing.T) {
				t.Parallel()
				env, rc := newEnv()
				if rc != nil {
					t.Fatalf("env: %v", rc)
				}
				env.Runtime.Registry.DefinePackage(tc.pkg)
				if lrc := env.InPackage(lisp.String(tc.pkg)); lrc.Type == lisp.LError {
					t.Fatalf("in-package %q: %v", tc.pkg, lrc)
				}
				for _, name := range tc.names[:len(tc.names)-1] {
					kind.add(env, elpsutil.Function(name, lisp.Formals("x"), bodyTable[0].fn))
				}
				last := tc.names[len(tc.names)-1]
				defer func() {
					if r := recover(); r == nil {
						t.Fatalf("registering %s %q in package %q did NOT panic."+
							" lisp's duplicate-symbol panic is what elpsutil's checkPackageName"+
							" mirrors and what the fuzz harness's bind-bypass registrations"+
							" assume; if lisp's rule changed, change checkPackageName and this"+
							" test together.", kind.name, last, tc.pkg)
					}
				}()
				kind.add(env, elpsutil.Function(last, lisp.Formals("x"), bodyTable[0].fn))
			})
		}
	}
}

// TestNilContractMistakesAreErrors pins the boundary validation that lets the
// fuzzer generate Go nil at all: formalsTable's go-nil shape and initTable's
// init/go-nil body exist on the strength of the behaviour asserted here.
//
// This test replaces TestKnownNilContractViolations, which pinned the
// opposite -- nil formals deferring to a call-time internal panic, and a nil
// loader result panicking uncaught inside elpsutil.Load -- back when the
// fuzzer was deliberately kept away from both inputs. #351 moved the check to
// the install boundary, so the tripwire now points the other way: if either
// behaviour regresses toward a panic, this test names the change before
// FuzzElpsutilEmbed rediscovers it as a crash, and the two table entries must
// be pulled back out of the fuzzer's reach.
func TestNilContractMistakesAreErrors(t *testing.T) {
	t.Parallel()

	t.Run("nil-formals-rejected-at-install", func(t *testing.T) {
		t.Parallel()
		env, rc := newEnv()
		if rc != nil {
			t.Fatalf("env: %v", rc)
		}
		p := &pkgData{name: "nilformals", builtins: []lisp.LBuiltinDef{
			// The plausible mistake: nil where lisp.Formals() belongs.
			elpsutil.Function("boom", nil, bodyTable[0].fn),
		}}
		lrc := elpsutil.Load(env, elpsutil.PackageLoader(defsPkg{p}))
		if lrc == nil || lrc.Type != lisp.LError {
			t.Fatalf("registering a builtin with nil formals did not fail at install time"+
				" (got %v). The old behaviour -- registering cleanly and nil-derefing inside"+
				" LEnv.bind at first call -- is exactly what formalsTable's go-nil entry would"+
				" turn into a permanent fuzz crash; restore the install-time check or pull the"+
				" entry.", lrc)
		}
		msg := lrc.String()
		for _, want := range []string{`"nilformals"`, `"boom"`, "formals are nil"} {
			if !strings.Contains(msg, want) {
				t.Fatalf("the install error does not name the mistake: missing %q in %q", want, msg)
			}
		}
		// Nothing may have been half-installed: the call must fail as an
		// ordinary unbound-symbol error, never as a recovered Go panic.
		res := env.LoadString("t", "(nilformals:boom)")
		if res == nil || res.Type != lisp.LError {
			t.Fatalf("the rejected builtin is still callable: %v", res)
		}
		if lisp.IsInternalPanic(res) {
			t.Fatalf("calling the rejected builtin recovered a Go panic: %v", res)
		}
	})

	t.Run("nil-loader-return-reported", func(t *testing.T) {
		t.Parallel()
		env, rc := newEnv()
		if rc != nil {
			t.Fatalf("env: %v", rc)
		}
		var lrc *lisp.LVal
		func() {
			defer func() {
				if r := recover(); r != nil {
					t.Fatalf("a Loader returning a Go nil *LVal panics inside elpsutil.Load"+
						" again: %v. initTable's init/go-nil entry generates exactly this input,"+
						" so the fuzzer would report the panic as a crash on its first pass;"+
						" restore the nil check or pull the entry.", r)
				}
			}()
			lrc = elpsutil.Load(env, func(env *lisp.LEnv) *lisp.LVal { return nil })
		}()
		if lrc == nil || lrc.Type != lisp.LError {
			t.Fatalf("a nil loader result was not reported as an error: %v", lrc)
		}
		if !strings.Contains(lrc.String(), "returned a nil *LVal") {
			t.Fatalf("the error does not say what the loader did wrong: %v", lrc)
		}
	})
}
