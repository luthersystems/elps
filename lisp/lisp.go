// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"github.com/luthersystems/elps/internal/fmtmeta"
	"github.com/luthersystems/elps/parser/token"
)

// LType is the type of an LVal
type LType uint

// TODO(elps2): Consider turning the following types into tagged-values:
//	LBytes
//	LSortMap
//	LArray
// Maybe allow for "tagged native values" that use LVal.Native to store the
// tagged data as an alternative to LVal.Cells[0].

// Possible LValType values
const (
	// LInvalid (0) is not a valid lisp type.
	LInvalid LType = iota
	// LInt values store an int in the LVal.Int field.
	LInt
	// LFloat values store a float64 in the LVal.Float field.
	LFloat
	// LError values use the LVal.Cells slice to store the following items:
	//		[0] a symbol representing the error "condition" (class name)
	//		[1:] error data (of any type)
	//
	// In addition, LError values store a copy of the function call stack at
	// the time of their creation in the LVal.Native field.
	//
	// TODO:  Make the stack a first class type (or some composite type) so
	// that it could be inspected during a condition handler.
	LError
	// LSymbol values store a string representation of the symbol in the
	// LVal.Str field.
	LSymbol
	LQSymbol // TODO:  Remove this... I can't believe it actually has usages
	// LSExpr values are "list" values in lisp and store their values in
	// LVal.Cells.
	LSExpr
	// LFun values use the following fields in an LVal:
	// 		LVal.Str      The local name used to reference the function (if any)
	// 		LVal.Native   A funData object
	//
	// In addition to these fields, a function defined in lisp (with defun,
	// lambda, defmacro, etc) uses the LVal.Cells field to store the following
	// items:
	//		[0]  a list describing the function's arguments
	//		[1:] body expressions of the function (potentially no expressions)
	//
	// NOTE:  Native go functions (LBuiltin) don't have a lexical environment
	// by default.  If a native function needs a lexical environment in order
	// to evaluate further expressions it is expected to create one.  See the
	// implementation of the builtin ``let''.
	//
	// NOTE: Cells[1] in an LFun may contain a string literal which contains a
	// docstring.  To match common-lisp semantics and maintain backwards
	// compatibility a function with a body consisting of only a string literal
	// returns the string constant and is considered to have no documentation.
	// A builtin function may also include a docstring in Cells[1].
	LFun
	// LQuote values are special values only used to represents two or more
	// levels of quoting (e.g. ''3 or '''''''()).  The quoted value is stored
	// in LVals.Cells[0].  The first level of quoting takes places by setting
	// the LVal.quoted field on a value with a normal value in LVal.Type.
	// LQuote values must always have a true LVal.quoted field.
	LQuote
	// LString values store a string in the LVal.Str field.
	LString
	// LBytes values store a *[]byte in the LVal.Native field.  LVal.Native,
	// and the contained pointer, must must never be nil (the slice being
	// pointed to may be nil though).
	LBytes
	// LSortMap value uses the LVal.Map field to store a map.
	//
	// TODO:  Use a tree-based map (that is potentially stored in Cells).  A
	// tree based map would be capable of supporting integer keys.
	LSortMap
	// LArray values use the LVal.Cells slice to store the following items:
	//		[0] a list containing dimension cardinalities in index 0
	//  	[1] a list containing row-major ordered array values
	LArray
	// LNative values store a Go value in the LVal.Native field and can be used
	// by builtin functions to store values of any type.
	LNative
	// LTaggedVal is a user-defined type that uses the following fields in an
	// LVal:
	// 		LVal.Str      The user-defined type name
	// 		LVal.Cells[0] The user-data for the typed-value
	LTaggedVal
	// Mark LVals are used to trasmit information down the stack through return
	// values.  Because the LEnv does not evaluate expressions using a stack
	// based virtual machine these Mark values, which often wrap other LVal
	// data in their Cells, are passed back from functions.  Typically the
	// environment is solely responsible for managing mark values and
	// applications should never see them during calls to builtin functions.
	LMarkTerminal  // LEnv marks the frame as terminal and evaluates tho contained expr
	LMarkTailRec   // LEnv resumes a call a set number of frames down the stack.
	LMarkMacExpand // LEnv will evaluate the returned LVal a subsequent time.
	// LTypeMax is not a real type but represents a value numerically greater
	// than all valid LType values.  It also can be used to determine the
	// number of valid LType values.
	LTypeMax
)

var lvalTypeStrings = []string{
	LInvalid:       "INVALID",
	LInt:           "int",
	LFloat:         "float",
	LError:         "error",
	LSymbol:        "symbol",
	LQSymbol:       "qsymbol",
	LSExpr:         "list",
	LFun:           "function",
	LQuote:         "quote",
	LString:        "string",
	LBytes:         "bytes",
	LSortMap:       "sorted-map",
	LArray:         "array",
	LNative:        "native",
	LTaggedVal:     "tagged-value",
	LMarkTailRec:   "marker-tail-recursion",
	LMarkMacExpand: "marker-macro-expansion",
}

func (t LType) String() string {
	if t >= LType(len(lvalTypeStrings)) {
		return lvalTypeStrings[LInvalid]
	}
	return lvalTypeStrings[t]
}

// LFunType denotes special functions, either macros or special operators.
type LFunType uint8

// LFunType constants.  LFunNone indicates a normal function.
const (
	LFunNone LFunType = iota
	LFunMacro
	LFunSpecialOp
)

var lfunTypeStrings = []string{
	LFunNone:      "function",
	LFunMacro:     "macro",
	LFunSpecialOp: "operator",
}

func (ft LFunType) String() string {
	if ft >= LFunType(len(lfunTypeStrings)) { //nolint:gosec // bounded by iota constants
		return "invalid-function-type"
	}
	return lfunTypeStrings[ft]
}

// funData is the Native payload of every LFun value: the builtin
// implementation or the captured environment, plus the function's identity
// (FID, package).  Unexported (issue #382): the captured environment was
// the deepest aliasing channel left in the exported API — handing an
// embedder the *LEnv exposes the live Scope of every closure sharing it,
// invisible to the runtime seal.  External readers keep the narrow
// identity accessors (FID, Package, Builtin); in-repo tooling reaches the
// captured environment through internal/funraw.
//
// The FIELDS are unexported too, not just the type.  An unexported type
// reached through an exported field is not sealed: LFun values carry their
// funData in the exported LVal.Native, and reflect can read an EXPORTED
// field of an unexported struct and hand back a usable value
// (reflect.Value.Interface does not set the read-only flag for it).  With
// `Env` exported, an embedder could recover a closure's captured *LEnv
// with plain reflection and rebind inside it through the public Put — the
// exact channel this comment claims is closed.  Unexported fields make
// reflect.Value.Interface panic instead, so every other privatized field
// (LVal.source/meta/macroExpansion, LEnv.scope/parent/loc, MapData's
// backing) was already unreachable; these are now too.
type funData struct {
	builtin LBuiltin
	env     *LEnv

	// loc is the captured environment's location register as it stood when
	// the function was defined.  bind gives the call environment this
	// snapshot rather than the captured environment's live register,
	// because eval READS env.loc before it rebinds it: the nesting-depth
	// guard and checkLimits both raise through env.ErrorConditionf, which
	// stamps env.loc into the error's rendered text and Source().  A
	// step-limit, nesting-limit or context cancellation that trips exactly
	// at a function-body entry therefore reports the definition site, which
	// is what the per-function child environment reported before functions
	// captured their defining environment directly.  Builtins leave it nil:
	// they carry no environment, so bind never reads it.
	loc *token.Location

	fid string
	pkg string
}

// macroExpansionContext is shared by all nodes in a single macro expansion.
// It records the macro call site, name, definition site, and unevaluated
// arguments for debugger inspection.  Unexported (issue #382): #370's stamp
// wrote expansion metadata onto shared parser nodes, so the only write path
// is the in-kernel stamp; external tooling reads a snapshot through the
// MacroExpansion accessor.
type macroExpansionContext struct {
	CallSite *token.Location // where the macro was invoked
	Name     string          // qualified macro name (e.g. "lisp:defun")
	DefSite  *token.Location // macro definition location (nil for builtins)
	Args     []*LVal         // unevaluated call-site arguments (for debugger scope)
}

// macroExpansionInfo is attached to LVal nodes produced by macro expansion.
// It is only allocated when a debugger is attached (Runtime.Debugger != nil),
// so production code pays zero allocation cost.
//
// The embedded *macroExpansionContext describes the macro CALL and is shared
// by every node of one expansion, by design.  This struct is the per-node
// half.  (*LVal).Copy does not carry either half across: the context's
// Args point at the tree the copy was made from, so a copy drops the
// record exactly as Fork and detach do (lisp/copier.go).
type macroExpansionInfo struct {
	*macroExpansionContext // shared across all nodes in one expansion

	// ID distinguishes one expansion node from another.  stampMacroExpansion
	// assigns it from Runtime.nextMacroExpID, monotonically increasing, so no
	// two nodes an expansion stamps share a value.
	//
	// It is unique per stamped node.  (*LVal).Copy used to duplicate it
	// -- Copy takes no *Runtime and had no counter to draw a fresh value
	// from -- so two distinct nodes could carry one ID, which the debugger's
	// stepper (exprStepLocation reads this into StepLocation.MacroID and
	// stepper.go steps on `loc.MacroID != s.start.MacroID`) would read as
	// one node and not pause between.  Copy now drops the whole record
	// (lisp/copier.go), as Fork and detach do, so a copy carries no ID and
	// that hazard (issue #466) has no path.
	ID int64
}

// MacroExpansionMeta is a read-only snapshot of the debug metadata attached
// to values produced by macro expansion while a debugger is attached.  It is
// returned by (*LVal).MacroExpansion; the metadata itself lives in
// unexported storage (issue #382) because the historical corruption in #370
// was a write of expansion metadata onto shared parser nodes — reads get a
// copy, and the in-kernel stamp is the only writer.
type MacroExpansionMeta struct {
	// CallSite is a copy of the location where the macro was invoked.
	CallSite *token.Location
	// DefSite is a copy of the macro definition location (nil for builtins).
	DefSite *token.Location
	// Name is the qualified macro name (e.g. "lisp:defun").
	Name string
	// Args holds the unevaluated call-site arguments.  The slice is a copy
	// but the nodes are the shared originals — read-only by contract (they
	// are typically sealed parse-tree nodes).
	Args []*LVal
	// ID is unique per stamped node, monotonically increasing within a
	// runtime.
	ID int64
}

// MacroExpansion returns a snapshot of v's macro-expansion debug metadata
// and reports whether v carries any.  Metadata exists only on nodes stamped
// during macro expansion while a debugger is attached (Runtime.Debugger !=
// nil); in production runs every value reports false.  The snapshot is a
// copy: mutating it does not touch v.
//
// MacroExpansion is nil-receiver safe: a nil LVal reports false.
func (v *LVal) MacroExpansion() (MacroExpansionMeta, bool) {
	if v == nil || v.macroExpansion == nil || v.macroExpansion.macroExpansionContext == nil {
		return MacroExpansionMeta{}, false
	}
	ctx := v.macroExpansion.macroExpansionContext
	m := MacroExpansionMeta{
		Name: ctx.Name,
		ID:   v.macroExpansion.ID,
	}
	if ctx.CallSite != nil {
		loc := *ctx.CallSite
		m.CallSite = &loc
	}
	if ctx.DefSite != nil {
		loc := *ctx.DefSite
		m.DefSite = &loc
	}
	if len(ctx.Args) > 0 {
		m.Args = append([]*LVal(nil), ctx.Args...)
	}
	return m, true
}

// LVal is a lisp value
//
// Field order is chosen so that every pointer-bearing word sits in the leading
// 64 bytes: the GC only scans up to the last pointer word, so grouping the
// pointers first and letting Str/Cells contribute their pointer word last
// leaves their len/cap tails (and all the scalars) outside the scan range.
// This cuts the GC scan extent from 112 bytes to 64 without changing the
// struct's overall size. LVal is allocated for every value in the
// interpreter, so keep the pointers first when adding fields — `govet`'s
// fieldalignment check (see .golangci.yml) enforces this.
type LVal struct {
	// Native is generic storage for data which cannot be represented as an
	// LVal (and thus can't be stored in Cells).
	Native interface{}

	// source is the value's originating location in source code.  The
	// reference may be shared by multiple LVals (and with scanner tokens),
	// which is why the field is unexported: external packages read it
	// through Source(), which returns a copy, and write it through
	// SetSource().  See issue #362.
	source *token.Location

	// meta holds formatting metadata, only populated in format-preserving
	// mode.  Unexported (issue #382), typed by an internal package: only
	// this module's format tooling (parser/rdparser writes, formatter
	// reads) can touch it, through internal/fmtraw.  Format-preserving
	// trees are never sealed, evaluated, or shared, so nothing outside
	// that tooling has ever had a legitimate use.
	meta *fmtmeta.Meta

	// macroExpansion holds debug metadata for nodes produced by macro
	// expansion. Only populated when a debugger is attached — nil in
	// production (zero overhead: 8-byte nil pointer).  Unexported (issue
	// #382): external packages read a snapshot through the MacroExpansion
	// accessor; the in-kernel stamp (stampMacroExpansion) is the only
	// writer.
	macroExpansion *macroExpansionInfo

	// Str used by LSymbol and LString values
	Str string

	// Cells used by many values as a storage space for lisp objects.
	//
	// TODO: Consider making Cells' type []LVal instead of []*LVal to reduce
	// the burden on the allocator/gc.
	Cells []*LVal

	// Type is the native type for a value in lisp.
	Type LType

	// Fields used for numeric types.
	Int   int
	Float float64

	// FunType used to further classify LFun values.
	FunType LFunType

	// quoted is a flag indicating a single level of quoting.  It is
	// unexported (issue #382): external packages read it through IsQuoted;
	// the only write paths are construction-time (Quote, Splice,
	// shallowUnquote) because the #333/#334 singleton race was an external
	// in-place write to this field.
	quoted bool

	// spliced denotes the value as needing to be spliced into a parent
	// value.  Unexported (issue #382): the flag is pure evaluator plumbing
	// between Splice and quasiquote expansion — no external package has
	// ever had a legitimate read or write.
	spliced bool

	// sealed marks a node of a parsed program: the value (and, for
	// containers, its Cells backing array) may be shared by every
	// environment that evaluates the same parse — substrate's parse cache
	// shares one tree process-wide — so kernel code must never mutate it in
	// place.  Guarded mutation sites refuse with the catchable
	// modify-literal-error condition (empty inputs excepted); see
	// lisp/seal.go for the design and the full list of guarded sites.
	//
	// The field occupies an existing padding byte: LVal is 112 bytes with
	// or without it (TestLValSizeUnchanged pins this).
	//
	// The flag is monotone: it is set (only) by SealAST after parsing
	// completes, propagated by header copies (Quote, Splice,
	// shallowUnquote — `*cp = *v` — which share the Cells backing array and
	// therefore inherit the constraint) and by the kernel sites that create
	// new headers over shared backing (cdr, rest, slice), and cleared only
	// on fresh storage (Copy, detach).  It is never written after a tree
	// becomes shared, so concurrent readers are race-free.
	sealed bool
}

// Source returns a copy of v's originating location in source code.  The
// boolean result reports whether v has a location at all — a false return
// means v carries no location (and the returned zero Location is
// meaningless), which is distinct from a real location whose fields happen
// to be zero.
//
// The returned Location is a value copy: mutating it never affects v or any
// other LVal.  The stored reference may be shared by many LVals, which is
// why no pointer accessor exists (issue #362).
//
// Source is nil-receiver safe: a nil LVal reports no recorded location.
//
// When v has no recorded location the boolean is false and the returned
// value is the synthetic "<native code>" location (File "<native code>",
// Pos -1) — the same location that values constructed by Go code have
// always reported — so the result is printable either way.
func (v *LVal) Source() (token.Location, bool) {
	if v == nil || v.source == nil {
		return nativeLocation(), false
	}
	return *v.source, true
}

// SetSource sets v's originating location in source code.  A nil loc clears
// the location.  The LVal stores the provided pointer, so a producer (e.g. a
// parser) may retain loc and continue to fix up its fields after the call —
// but once an LVal escapes to consumers the location must be treated as
// frozen, because the reference may be shared by many LVals (issue #362).
//
// A sealed value (a parsed program node — see lisp/seal.go) keeps its
// parse-time location forever: SetSource on a sealed value is a no-op,
// because the node may be shared by every environment evaluating the same
// parse and restamping it would be a cross-environment write.  The parser
// itself always stamps locations before sealing, so no in-repo caller is
// affected.
func (v *LVal) SetSource(loc *token.Location) {
	if v.sealed {
		return
	}
	v.source = loc //elps:mutates the audited setter for source metadata; sealed (shared) nodes are skipped above
}

// setSynthesizedSource gives every node in nodes that carries no location of
// its own the location loc.
//
// It is for the nodes a builtin SYNTHESIZES -- the formals, call and body
// forms that compose, flip and the `expr` operator build for the function
// they return.  Those come out of SExpr/Symbol/Formals with source == nil,
// and a nil source renders as "unknown" in a stack note, so a stack running
// through a composed function named no location at all.
//
// The macro stamp used to hide that for the composed functions a macro
// returned: stampMacroExpansion walked INTO the returned function value and
// wrote the macro CALL site onto its body.  That was the wrong location (the
// function was built where compose ran, not where the macro was called) and
// a write into storage the expansion did not own; the stamp no longer
// descends into a value, so the location has to be established here, where
// the node is constructed.  See the warning above stampMacroExpansion.
//
// loc must be a Location the constructed function may own -- env.loc.Copy(),
// ONE copy shared by the nodes of one constructed function, never env.loc
// itself (issue #431), whose pointee moves with the evaluator.
//
// Nodes that already carry a location are skipped, but that is a
// convenience, not a safety net: the caller must pass only nodes it
// constructed.  A function value the caller was HANDED (compose's f and g,
// flip's fun) has no location when it is a builtin, so passing one would
// stamp a live global binding -- exactly the corruption the ownership rule
// above stampMacroExpansion exists to prevent.
func setSynthesizedSource(loc *token.Location, nodes ...*LVal) {
	for _, v := range nodes {
		if v == nil || v.source != nil {
			continue
		}
		v.SetSource(loc)
	}
}

// IsQuoted reports whether v carries a single level of quoting — the flag
// behind the LQuote wrapper and the ['(...)/[...]] display forms.  The
// underlying field is unexported (issue #382): quoting is established at
// construction time (Quote, Splice, QExpr, QSymbol, the parser) and removed
// only by the evaluator's own unquote step, so external packages get a read
// but never a write — an in-place external write to the flag on a shared
// value was exactly the #333/#334 singleton corruption.
//
// IsQuoted is nil-receiver safe: a nil LVal reports false.
func (v *LVal) IsQuoted() bool {
	return v != nil && v.quoted
}

// GetType returns a quoted symbol denoting v's type.
func GetType(v *LVal) *LVal {
	t := Symbol(v.Str)
	t.quoted = true
	if v.Type != LTaggedVal {
		t.Str = v.Type.String()
	}
	return t
}

// Value conveniently converts v to an LVal.  Types which can be represented
// directly in lisp will be converted to the appropriate LVal.  All other types
// will be turned into a Native LVal.  Value is the inverse of the GoValue
// function.
func Value(v interface{}) *LVal {
	switch v := v.(type) {
	case bool:
		return Bool(v)
	case string:
		return String(v)
	case []byte:
		return Bytes(v)
	case int:
		return Int(v)
	case float64:
		return Float(v)
	case []*LVal:
		return QExpr(v)
	default:
		return Native(v)
	}
}

// Singleton LVals (singletonNil, singletonTrue, singletonFalse) and
// their helpers (isSingleton, assertNotSingleton, SingletonSnapshot)
// live in singleton.go.

// Bool returns an LVal with truthiness identical to b.
//
// The returned value is a shared singleton — callers MUST NOT mutate it.
func Bool(b bool) *LVal {
	if b {
		checkSingleton(singletonTrue)
		return singletonTrue
	}
	checkSingleton(singletonFalse)
	return singletonFalse
}

// Int returns an LVal representing the number x.
func Int(x int) *LVal {
	return &LVal{
		Type: LInt,
		Int:  x,
	}
}

// Float returns an LVal representation of the number x
func Float(x float64) *LVal {
	return &LVal{
		Type:  LFloat,
		Float: x,
	}
}

// String returns an LVal representing the string str.
func String(str string) *LVal {
	return &LVal{
		Type: LString,
		Str:  str,
	}
}

// Bytes returns an LVal representing binary data b.
func Bytes(b []byte) *LVal {
	return &LVal{
		Type:   LBytes,
		Native: &b,
	}
}

func SplitSymbol(sym *LVal) *LVal {
	if sym.Type != LSymbol {
		return Errorf("not a symbol")
	}
	pieces := strings.Split(sym.Str, ":")
	switch len(pieces) {
	case 1:
		return QExpr([]*LVal{sym})
	case 2:
		return QExpr([]*LVal{Symbol(pieces[0]), Symbol(pieces[1])})
	default:
		return Errorf("illegal symbol: %q", sym.Str)
	}
}

// Symbol returns an LVal representing the symbol s
func Symbol(s string) *LVal {
	return &LVal{
		Type: LSymbol,
		Str:  s,
	}
}

// QSymbol returns an LVal representing the quoted symbol
func QSymbol(s string) *LVal {
	return &LVal{
		Type: LQSymbol,
		Str:  s,
	}
}

// Nil returns an LVal representing nil, an empty list, an absent value.
//
// The returned value is a shared singleton — callers MUST NOT mutate it.
// If you need a mutable empty list (e.g., to append children), use
// SExpr(nil) directly.
func Nil() *LVal {
	checkSingleton(singletonNil)
	return singletonNil
}

// Native returns an LVal containng a native Go value.
func Native(v interface{}) *LVal {
	return &LVal{
		Type:   LNative,
		Native: v,
	}
}

// SExpr returns an LVal representing an S-expression, a symbolic expression.
// Provided cells are used as backing storage for the returned expression and
// are not copied.
func SExpr(cells []*LVal) *LVal {
	return &LVal{
		Type:  LSExpr,
		Cells: cells,
	}
}

// QExpr returns an LVal representing an Q-expression, a quoted expression, a
// list.  Provided cells are used as backing storage for the returned list and
// are not copied.
func QExpr(cells []*LVal) *LVal {
	return &LVal{
		Type:   LSExpr,
		quoted: true,
		Cells:  cells,
	}
}

// Vector returns an LVal representing a vector, a 1-dimensional array.
// Provided cells are used as backing storage for the returned vector and are
// not copied.
func Vector(cells []*LVal) *LVal {
	return Array(nil, cells)
}

// MakeVector returns a vector with n cells initialized to Nil.
func MakeVector(n int) *LVal {
	cells := make([]*LVal, n)
	for i := range cells {
		cells[i] = Nil()
	}
	return Vector(cells)
}

// Array returns an LVal representing an array reference.  The dims argument is
// be a list of integers sizes for each dimension of the array.  If non-empty,
// cells provides the backing storage for the array.  The dims argument may be
// nil, in which case a vector (one dimensional array) is returned.  If dims is
// non-nil then cells must either be nil or have one element for every array
// element, in row-major order.
//
// When no backing storage is supplied every element is initialized to Nil,
// the same value MakeVector gives an unset element.  It used to be left as
// the zero value of the slice, a Go nil *LVal, which is not a value the rest
// of the interpreter can hold: reading one dereferences it.  In-tree that was
// latent -- every caller here fills the cells it asked for before the array
// escapes -- but Array is exported and its documentation says cells may be
// nil, so an embedder following it built a value that panicked the host the
// first time lisp touched it:
//
//	env.PutGlobal(Symbol("a"), Array(QExpr([]*LVal{Int(3)}), nil))
//	(aref a 0)    ; internal-panic: nil pointer dereference
//
// A panic is the wrong answer twice over: the read is not an error at all
// (an unset element is nil, which is what the array now holds), and an
// internal-panic is deliberately not catchable by handler-bind, so the host
// had no way to contain it.  See issue #367.
func Array(dims *LVal, cells []*LVal) *LVal {
	// stored is the dims list the array keeps.  Caller-supplied dims are
	// copied because the array writes its cardinality in place later --
	// builtinAppendMutate grows a vector's dims as append! adds cells --
	// so sharing the caller's list would let that write land in a value
	// the caller still holds.  No in-tree production caller passes dims
	// any more (the vector-building builtins all derive them; only the
	// fuzz harness's multi-dimensional constructor remains), so this
	// branch now chiefly serves embedders.  Dims this function constructs
	// are reachable from nothing else, so that path stores them directly.  The parameter is
	// deliberately never assigned to stored: that flow would escape a
	// caller's dims into the returned value as far as the compiler can tell,
	// and heap-allocate the literal every builtin call site passes.
	var stored *LVal
	totalSize := 1
	if dims == nil {
		// A self-built dims list is exactly [len(cells)], so its product
		// needs no loop and cannot overflow.
		totalSize = len(cells)
		stored = QExpr([]*LVal{Int(len(cells))})
	} else if dims.Type != LSExpr {
		return Errorf("array dimensions are not a list: %v", dims.Type)
	} else {
		for _, n := range dims.Cells {
			if n.Type != LInt {
				return Errorf("array dimension is not an integer: %v", n.Type)
			}
		}
		for _, n := range dims.Cells {
			totalSize *= n.Int
			if totalSize < 0 {
				return Errorf("integer overflow")
			}
		}
	}
	if len(cells) > 0 && len(cells) != totalSize {
		return Errorf("array contents do not match size")
	} else if len(cells) == 0 {
		cells = make([]*LVal, totalSize)
		for i := range cells {
			cells[i] = Nil()
		}
	}
	if stored == nil {
		// Deferred past every check above so the error paths stay copy-free.
		stored = dims.Copy()
	}

	return &LVal{
		Type: LArray,
		Cells: []*LVal{
			stored,
			QExpr(cells),
		},
	}
}

// SortedMap returns an LVal representing a sorted map
func SortedMap() *LVal {
	return SortedMapFromData(&MapData{newmap()})
}

// SortedMapSized returns an empty sorted-map whose backing is sized to
// receive n entries, as make(map, n) sizes a Go map.  n is a capacity hint:
// the map is empty and it grows past n as SortedMap's would.  A negative n
// is clamped to zero rather than passed to make, because the language
// requires make's size argument to be non-negative and gc's tolerance of a
// negative map hint is not something an exported constructor should oblige
// its callers to rely on.  Use it where the entry count is known before the
// first Set, such as when copying a map.
func SortedMapSized(n int) *LVal {
	if n < 0 {
		n = 0
	}
	return SortedMapFromData(&MapData{newmapSized(n)})
}

// SortedMapFromData returns sorted-map with the given backing implementation.
// Applications calling this function must make ensure the Map implementation
// provided satisfies the semantics of Map methods.
func SortedMapFromData(data *MapData) *LVal {
	return &LVal{
		Type:   LSortMap,
		Native: data,
	}
}

// FunRef returns a reference to fun that uses the local name symbol.
func FunRef(symbol, fun *LVal) *LVal {
	if symbol.Type != LSymbol {
		return Errorf("argument is not a symbol: %v", symbol.Type)
	}
	if fun.Type != LFun {
		return Errorf("argument is not a function: %v", fun.Type)
	}
	cp := &LVal{}
	*cp = *fun
	cp.Str = symbol.Str
	return cp
}

// FunInPackage returns an LFun bound to the named package. Prefer this
// over Fun for code embedding ELPS: Fun leaves Package empty, and a
// package-less LFun reaching funCall / MacroCall / SpecialOpCall
// produces "BUG: GetFunName" log spam (issue #271).
func FunInPackage(pkg, fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Type: LFun,
		Native: &funData{
			fid:     fid,
			builtin: fn,
			pkg:     pkg,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// Fun returns an LVal representing a function. Package is left empty;
// callers MUST set the funData Package before the value is invoked, or
// GetFunName will log "BUG: ..." at every call site that observes a
// package-less LFun.
//
// Deprecated: use FunInPackage, which sets Package atomically. See
// issue #271.
func Fun(fid string, formals *LVal, fn LBuiltin) *LVal {
	return FunInPackage("", fid, formals, fn)
}

// MacroInPackage returns a macro LFun bound to the named package.
// Prefer this over Macro for the same reasons FunInPackage is preferred
// over Fun. See issue #271.
func MacroInPackage(pkg, fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		FunType: LFunMacro,
		Native: &funData{
			fid:     fid,
			builtin: fn,
			pkg:     pkg,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// Macro returns an LVal representing a macro. Package is left empty;
// callers MUST set the funData Package before the value is invoked, or
// GetFunName will log "BUG: ..." at every call site that observes a
// package-less LFun.
//
// Deprecated: use MacroInPackage, which sets Package atomically. See
// issue #271.
func Macro(fid string, formals *LVal, fn LBuiltin) *LVal {
	return MacroInPackage("", fid, formals, fn)
}

// SpecialOpInPackage returns a special-operator LFun bound to the named
// package. Prefer this over SpecialOp for the same reasons FunInPackage
// is preferred over Fun. See issue #271.
func SpecialOpInPackage(pkg, fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Type:    LFun,
		FunType: LFunSpecialOp,
		Native: &funData{
			fid:     fid,
			builtin: fn,
			pkg:     pkg,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// SpecialOp returns an LVal representing a special operator.  Special
// operators are function which receive unevaluated results, like macros.
// However values returned by special operations do not require further
// evaluation, unlike macros.
//
// Package is left empty; callers MUST set the funData Package before the
// value is invoked, or GetFunName will log "BUG: ..." at every call site
// that observes a package-less LFun.
//
// Deprecated: use SpecialOpInPackage, which sets Package atomically.
// See issue #271.
func SpecialOp(fid string, formals *LVal, fn LBuiltin) *LVal {
	return SpecialOpInPackage("", fid, formals, fn)
}

// Error returns an LError representing err.  Errors store their message in
// Cells and their condition type in Str.  The error condition type must be a
// valid lisp symbol.
//
// Errors generated during expression evaluation typically have a non-nil Stack
// field.  The Env.Error() method is typically the preferred method for
// creating error LVal objects because it initializes Stack with an appropriate
// value.
func Error(err error) *LVal {
	return ErrorCondition("error", err)
}

// ErrorCondition returns an LError representing err and having the given
// condition type.  Errors store their message/data in Cells and their
// condition type in Str.  The condition type must be a valid lisp symbol.
//
// Errors generated during expression evaluation typically have a non-nil Stack
// field.  The Env.Error() method is typically the preferred method for
// creating error LVal objects because it initializes Stack with an appropriate
// value.
func ErrorCondition(condition string, err error) *LVal {
	return &LVal{
		Type:  LError,
		Str:   condition,
		Cells: []*LVal{Native(err)},
	}
}

// Errorf returns an LError with a formatted error message. Errors store their
// message in Cells and their condition type in Str. The condition type must be
// a valid symbol.
//
// Errors generated during expression evaluation typically have a non-nil Stack
// field.  The Env.Errorf() method is typically the preferred method for
// creating error LVal objects because it initializes Stack with an appropriate
// value.
func Errorf(format string, v ...interface{}) *LVal {
	return ErrorConditionf("error", format, v...)
}

// ErrorConditionf returns an LError with a formatted error message. Errors
// store their message in Cells and their condition type in Str. The condition
// type must be a valid symbol.
//
// Errors generated during expression evaluation typically have a non-nil Stack
// field.  The Env.ErrorConditionf() method is typically the preferred method
// for creating error LVal objects because it initializes Stack with an
// appropriate value.
func ErrorConditionf(condition string, format string, v ...interface{}) *LVal {
	return &LVal{
		Type:  LError,
		Str:   condition,
		Cells: []*LVal{String(fmt.Sprintf(format, v...))},
	}
}

// Quote quotes v and returns the quoted value.  The LVal v is modified.
func Quote(v *LVal) *LVal {
	if !v.quoted {
		cp := &LVal{}
		*cp = *v
		cp.quoted = true
		return cp
	}
	quote := &LVal{
		Type:   LQuote,
		quoted: true,
		Cells:  []*LVal{v},
	}
	return quote
}

// Splice is used in the implementation of quasiquote to insert a list into an
// outer slist.
func Splice(v *LVal) *LVal {
	cp := &LVal{}
	*cp = *v
	cp.spliced = true
	return cp
}

// shallowUnquote is an artifact from when functions could freely modify LVals
// It may be worth trying to unify all quoting under the LQuote type.
func shallowUnquote(v *LVal) *LVal {
	cp := &LVal{}
	*cp = *v
	cp.quoted = false
	return cp
}

// Formals returns an LVal reprsenting a function's formal argument list
// containing symbols with the given names.
func Formals(argSymbols ...string) *LVal {
	s := QExpr(make([]*LVal, len(argSymbols)))
	for i, name := range argSymbols {
		if name == VarArgSymbol {
			if i != len(argSymbols)-2 {
				return Errorf("invalid formal arguments: misplaced %s", VarArgSymbol)
			}
		}
		s.Cells[i] = Symbol(name)
	}
	return s
}

func markTailRec(npop int, fun *LVal, args *LVal) *LVal {
	return &LVal{
		Type:  LMarkTailRec,
		Cells: []*LVal{Int(npop), Int(npop), fun, args},
	}
}

// tailRecElided, tailRecFun and tailRecArgs read a tail-recursion mark.
//
// NOT LISP-REACHABLE (#367): the marks are produced only by markTailRec and
// consumed only inside funCall, which reaches these three accessors from
// behind its own `r.Type == LMarkTailRec` test.  LMarkTailRec has no reader
// syntax, no constructor a builtin can call and no path into a lisp binding,
// so no program can present a value of any other type here.  A panic marks a
// missing type test in the evaluator.
func (v *LVal) tailRecElided() int {
	if v.Type != LMarkTailRec {
		panic("not marker-tail-recursion")
	}
	return v.Cells[1].Int
}

func (v *LVal) tailRecFun() *LVal {
	if v.Type != LMarkTailRec {
		panic("not marker-tail-recursion")
	}
	return v.Cells[2]
}

func (v *LVal) tailRecArgs() *LVal {
	if v.Type != LMarkTailRec {
		panic("not marker-tail-recursion")
	}
	return v.Cells[3]
}

func markMacExpand(expr *LVal) *LVal {
	return &LVal{
		Type:  LMarkMacExpand,
		Cells: []*LVal{expr},
	}
}

// IsInternalPanic reports whether v is an error produced by recovering a Go
// panic that escaped host code during evaluation.
//
// This is the check `ignore-errors`, `handler-bind` and `with-cleanup`
// use -- the last to refuse to let an error raised by a cleanup form mask
// an in-flight panic, rather than to catch one -- and embedders
// should use it too, rather than comparing the condition name against
// CondInternalPanic.  The condition name alone is forgeable: lisp code can
// write (error 'internal-panic "...") and, if the name were the only test,
// would produce an error that no catch-all handler could contain.
//
// The marker is the Go stack snapshot the recover handler attaches to the
// error's CallStack copy.  Nothing reachable from lisp can populate it — the
// live Runtime stack's GoStack is always nil, so an error raised by the
// `error` builtin always copies a nil GoStack.  A forged 'internal-panic is
// therefore treated as an ordinary condition and stays containable.
func IsInternalPanic(v *LVal) bool {
	if v == nil || v.Type != LError || v.Str != CondInternalPanic {
		return false
	}
	stack, ok := v.Native.(*CallStack)
	return ok && stack != nil && len(stack.GoStack) > 0
}

// CallStack returns the call stack attached to the error v.  CallStack panics
// if v.Type is not LError.
//
// NOT LISP-REACHABLE (#367): the panic guards a Go type assertion, not a
// program's data.  Every in-tree caller -- builtinLoad*/builtinIsKey,
// env.Error*, macroDefun/macroDefmacro, opLambda, libjson's attachStack, and
// the two diagnostic renderers -- tests v.Type == LError first, so lisp source
// has no way to route a non-error here.  Reaching it means embedder Go code
// called the accessor on a value it had not classified, and that is the
// #351/#355 shape rather than this one.  A caller that cannot classify the
// value should test v.Type == LError itself: the type IS the check, so an
// accessor that answered nil would be reporting "no stack recorded" for a
// value that can never have one.
func (v *LVal) CallStack() *CallStack {
	if v.Type != LError {
		panic("not an error: " + v.Type.String())
	}
	stack, ok := v.Native.(*CallStack)
	if !ok {
		return nil
	}
	return stack
}

// SetCallStack attaches a copy of stack to the error v.  SetCallStack panics
// if v.Type is not LError.
//
// NOT LISP-REACHABLE (#367): same argument as CallStack above -- every
// in-tree caller guards on v.Type == LError.
func (v *LVal) SetCallStack(stack *CallStack) {
	if v.Type != LError {
		panic("not an error: " + v.Type.String())
	}
	v.Native = stack.Copy() //elps:mutates the audited setter stamping a copied stack onto an in-flight error at its capture point
}

// funData returns the function payload of an LFun value.  It panics on
// non-function values.  Unexported (issue #382): external packages read
// function identity through FID, Package, and Builtin, and in-repo tooling
// reaches the captured environment through internal/funraw.
//
// NOT LISP-REACHABLE (#367): calling a non-function is rejected before this
// is consulted -- evalSExpr answers "unbound symbol"/"not a function", and
// the builtins that take a function argument (map, foldl/foldr, sort, apply,
// funcall) run it through GetFunGlobal and return an error for anything that
// is not an LFun.  The remaining callers reach it from inside a
// `Type == LFun` branch: Docstring, str, Package.put, funCall/MacroCall,
// libschema's isValidator, and the debugger and profiler annotators.  Its
// thin readers (Package, Builtin, FID, funEnv) inherit the same argument.
func (v *LVal) funData() *funData {
	if v.Type != LFun {
		panic("not a function: " + v.Type.String())
	}
	return v.Native.(*funData)
}

// Package returns the name of the package a function value was defined in,
// or "" for a function value carrying no function data.  It panics on
// non-function values.
func (v *LVal) Package() string {
	if fd := v.funData(); fd != nil {
		return fd.pkg
	}
	return ""
}

// Builtin returns the native implementation of a builtin function value,
// or nil for user-defined functions and function values carrying no
// function data.  It panics on non-function values.
func (v *LVal) Builtin() LBuiltin {
	if fd := v.funData(); fd != nil {
		return fd.builtin
	}
	return nil
}

// FID returns the function value's unique identifier, or "" for a function
// value carrying no function data.  It panics on non-function values.
func (v *LVal) FID() string {
	if fd := v.funData(); fd != nil {
		return fd.fid
	}
	return ""
}

// funEnv returns the environment captured by a function value (nil for
// builtins).  Unexported (issue #382): the captured environment is the
// deepest aliasing channel into shared interpreter state, so external
// packages cannot reach it at all; in-repo tooling goes through
// internal/funraw.
func (v *LVal) funEnv() *LEnv {
	if fd := v.funData(); fd != nil {
		return fd.env
	}
	return nil
}

// Len returns the length of the list v.
func (v *LVal) Len() int {
	switch v.Type {
	case LString:
		return len(v.Str)
	case LBytes:
		return len(v.Bytes())
	case LSExpr:
		return len(v.Cells)
	case LSortMap:
		return v.Map().Len()
	case LArray:
		if v.Cells[0].Len() == 1 {
			return v.Cells[0].Cells[0].Int
		}
		fallthrough
	default:
		return -1
	}
}

// KeyArg returns the i'th cell of a builtin's argument list, or Nil if the
// list is shorter than that.
//
// The evaluator always passes one cell per declared formal, so for a builtin
// reached from lisp this is just Cells[i]. It differs for the builtins this
// package exports for embedding: an embedder binds the Go function to formals
// of its own, and one that declares fewer formals than the builtin reads --
// easily done for an &key argument, which is invisible at the call site --
// would otherwise index past the end of Cells and panic on every call. The Go
// signature is identical either way, so nothing catches it at compile time.
//
// An absent cell is reported as Nil, which is what the evaluator itself passes
// for an unsupplied &optional or &key argument. Builtins must therefore read
// those cells through KeyArg rather than indexing Cells directly.
//
// Use it ONLY for optional and &key cells, never for a required argument.
// Reporting an absent cell as Nil conflates "the caller supplied nothing" with
// "the caller supplied nil", and that is only safe where nil already means
// "not supplied". For a required argument whose valid domain includes Nil it
// is actively harmful: json:dump-string would answer "null" for a binding that
// supplied no argument at all, turning a panic into a silent wrong answer.
// Required arguments use ReqArg instead.
func (v *LVal) KeyArg(i int) *LVal {
	if i < 0 || i >= len(v.Cells) {
		return Nil()
	}
	return v.Cells[i]
}

// ReqArg returns the i'th cell of a builtin's argument list, or an error if the
// list is shorter than that.
//
// This is the required-argument counterpart to KeyArg. Indexing Cells directly
// panics when an embedder binds the builtin to formals declaring fewer
// arguments than it reads, and a panic in an embedder's process is a far worse
// outcome than an error value -- the evaluator can only report it as an opaque
// internal-panic, and a host embedding elps has no way to recover context from
// it. Reporting the absent cell as Nil is not an option either, for the reason
// given on KeyArg. So: an error, naming the mismatch.
func (v *LVal) ReqArg(env *LEnv, i int) *LVal {
	if i < 0 || i >= len(v.Cells) {
		return env.ErrorConditionf(CondMissingArgument,
			"missing required argument %d: this builtin reads at least %d argument(s)"+
				" but was bound to formals declaring only %d", i, i+1, len(v.Cells))
	}
	return v.Cells[i]
}

// UserData returns the user-data associated with an LTaggedVal.
// UserData returns an error if v is not an LTaggedVal.
func (v *LVal) UserData() *LVal {
	if v.Type != LTaggedVal {
		return Errorf("not tagged: %v", v.Type)
	}
	return v.Cells[0]
}

// Bytes returns the []byte stored in v.  Bytes panics if v.Type is not LBytes.
//
// NOT LISP-REACHABLE (#367), and the guard belongs to the CALLER: every
// in-tree caller tests v.Type == LBytes (or reaches this from inside a
// `case LBytes`) before calling.  `(append 'bytes 0)` was the counterexample
// -- the type SPECIFIER was validated and the sequence was not -- and it is
// now a seed in the eval fuzz corpus, replayed on every run.
//
// Deliberately not softened to "return nil for a non-LBytes": nil is a
// perfectly good empty byte string, so a caller that skipped its type check
// would get a silent wrong answer instead of a loud one.  That is the trade
// KeyArg's doc comment rejects.  A caller that cannot vouch for the type must
// test it and raise its own error; TestBuiltinRegistryNeverPanics
// (lisp/lisplib) is what finds the caller that forgot.
func (v *LVal) Bytes() []byte {
	if v.Type != LBytes {
		panic("not bytes: " + v.Type.String())
	}
	// NOTE:  Bytes are stored as a pointer to a slice to allow for effecient
	// appending in the same style as normal vectors.
	return *v.Native.(*[]byte)
}

// Map returns the map data stored in v.  Map panics if v.Type is not
// LSortMap.
//
// NOT LISP-REACHABLE (#367), on the same terms as Bytes above: every in-tree
// caller -- the sorted-map builtins, Copy's LSortMap case, equal, libschema
// and libelpspath's path walkers -- tests LSortMap first.  Returning nil for
// a non-map would only move the crash to the caller's next method call on the
// nil *MapData, so the type test stays the caller's job.
func (v *LVal) Map() *MapData {
	if v.Type != LSortMap {
		panic("not sorted-map: " + v.Type.String())
	}
	return v.Native.(*MapData)
}

// MapKeys returns a list of keys in the map.  MapKeys panics if v.Type is not
// LSortMap.  The type of each map key is retained from the first type a value
// was set for that key.  For example, if the MapSet(Symbol("a"), Int(1)) is
// called before MapSet(String("a"), Int(2)) then MapKey() will contain the
// symbol and not the string.
func (v *LVal) MapKeys() *LVal {
	return v.Map().Keys()
}

// MapEntries returns a list of key-value pairs in the map.  MapEntries
func (v *LVal) MapEntries() *LVal {
	return sortedMapEntries(v.Map())
}

// ArrayDims returns the dimensions of an array.  ArrayDims returns an error if
// v.Type is not LArray.
func (v *LVal) ArrayDims() *LVal {
	if v.Type != LArray {
		return Errorf("not an array: %v", v.Type)
	}
	return v.Cells[0].Copy()
}

// ArrayIndex returns the value at the given index in an array.
func (v *LVal) ArrayIndex(index ...*LVal) *LVal {
	if v.Type != LArray {
		return Errorf("not an array: %v", v.Type)
	}
	dims := v.Cells[0]
	if len(index) != dims.Len() {
		// %v, not %#v.  %#v prints the Go struct literal for the *LVal,
		// which embeds the Source and Cells POINTERS -- so this message
		// carried heap addresses and differed on every evaluation of the
		// same program (elps#427).  Downstream a phylum runs as chaincode,
		// where every endorsing peer must produce identical output for a
		// transaction, and this condition is reachable from lisp with
		// `(aref (vector))`.  An error message is program output.
		return Errorf("invalid index into array with dimensions %v: got %d index(es), want %d",
			dims, len(index), dims.Len())
	}
	if len(index) == 0 {
		return v.Cells[1]
	}
	for i, j := range index {
		n := dims.Cells[i]
		if j.Type != LInt {
			return Errorf("index is not an integer: %v", j.Type)
		}
		if j.Int < 0 {
			return Errorf("index is negative: %v", j)
		}
		if j.Int >= n.Int {
			return Errorf("index %d out of bounds for array dimenion %d of %v", j.Int, i, dims)
		}
	}
	i := 0
	stride := 1
	for len(index) > 0 {
		i += index[len(index)-1].Int * stride
		stride *= dims.Cells[len(index)-1].Int
		index = index[:len(index)-1]
	}

	return v.Cells[1].Cells[i]
}

// MapGet returns the value corresponding to k in v or an LError if k is not
// present in v.  MapGet panics if v.Type is not LSortMap.
func (v *LVal) MapGet(k interface{}) *LVal {
	switch k := k.(type) {
	case *LVal:
		x, _ := v.Map().Get(k)
		return x
	case string:
		x, _ := v.Map().Get(String(k))
		return x
	// numerics unsupported
	default:
		return Errorf("invalid key type: %T", k)
	}
}

// MapSet sets k to val in v.  MapSet returns an error if v.Type is not
// LSortMap.  String and symbol keys are coerced to avoid programming errors
// causing symbol and string keys with equal string values from existing in the
// same map.
func (v *LVal) MapSet(k interface{}, val *LVal) *LVal {
	if v.Type != LSortMap {
		return Errorf("not sorted-map: %v", v.Type)
	}
	switch k := k.(type) {
	case *LVal:
		return v.Map().Set(k, val)
	case string:
		return v.Map().Set(String(k), val)
	// numerics unsupported
	default:
		return Errorf("invalid key type: %T", k)
	}
}

// IsSpecialFun returns true if v is a special function.  IsSpecialFun doesn't
// actually check v.Type, only v.FunType.
func (v *LVal) IsSpecialFun() bool {
	return v.FunType != LFunNone
}

// IsMacro returns true if v is a macro function.  IsMacro doesn't
// actually check v.Type, only v.FunType.
func (v *LVal) IsMacro() bool {
	return v.FunType == LFunMacro
}

// IsSpecialOp returns true if v is a special operator.  IsMacro doesn't
// actually check v.Type, only v.FunType.
func (v *LVal) IsSpecialOp() bool {
	return v.FunType == LFunSpecialOp
}

// IsNil returns true if v represents a nil value.
//
// Only the empty list is nil.  Written as an expression rather than a switch:
// there is exactly one interesting type, so a switch would have to name the
// other seventeen LTypes to say nothing about them.
func (v *LVal) IsNil() bool {
	return v.Type == LSExpr && len(v.Cells) == 0
}

// mayNest reports whether a walk over v can reach another value through it.
//
// Cells is where every nested value lives except a sorted-map's, which lives
// in a MapData behind Native.  Anything else -- an int, a string, a symbol, a
// byte slice, a native Go value, the empty list -- is a leaf: a walk that
// reaches it stops there, so it never needs a place on a cycle guard's path.
//
// This is what keeps the guard off the common path.  Rendering and comparing
// leaves is most of what those walks do, and this check is a length test.
func (v *LVal) mayNest() bool {
	return len(v.Cells) > 0 || v.Type == LSortMap
}

// IsNumeric returns true if v has a primitive numeric type (int, float64).
//
// See IsNil for why this is an expression and not a switch.
func (v *LVal) IsNumeric() bool {
	return v.Type == LInt || v.Type == LFloat
}

// Equal returns a non-nil value if v and other are logically equal, under the
// rules used by the "equal?" function.
//
// Cyclic operands terminate.  Comparison is co-inductive: when the walk
// reaches a pair of values it is already comparing further up the current
// path, it takes that pair to be equal and moves on.  That is the greatest
// fixed point -- equality of the two values' infinite unfoldings, the same
// answer R7RS requires of equal? on circular structure -- and it is an answer
// rather than a guess: false is only ever returned for a difference actually
// found at a finite depth, so no equality is claimed that a longer walk could
// refute.  See lisp/cycle.go and issue #390.
func (v *LVal) Equal(other *LVal) *LVal {
	var st pairState
	eq := v.equal(other, pairGuard{state: &st})
	if !st.cyclic {
		return eq
	}
	// Both operands reach a pair that is already under comparison.  The walk
	// above stopped as soon as it knew that, because unrolling a cycle to
	// cycleGuardDepth levels is exponential in the width of the cycle; the
	// rerun compares each pair once.
	return v.equal(other, strictPairGuard())
}

// equal is Equal, with g bounding the walk.  Every nested comparison must pass
// g down rather than calling Equal, or the bound is lost.
//
// The guard sits inside the cases that recurse rather than at the top of the
// function, so that comparing two ints or two strings runs exactly the code it
// ran before the guard existed.
func (v *LVal) equal(other *LVal, g pairGuard) *LVal {
	if v.Type != other.Type {
		if v.IsNumeric() && other.IsNumeric() {
			return v.equalNum(other)
		}
		return Bool(false)
	}
	if v.IsNumeric() {
		return v.equalNum(other)
	}
	switch v.Type {
	case LString, LSymbol:
		return Bool(v.Str == other.Str)
	case LSExpr:
		if v.Len() != other.Len() {
			return Bool(false)
		}
		g, stop := g.descend(v, other)
		if stop {
			return Bool(true)
		}
		for i := range v.Cells {
			if !True(v.Cells[i].equal(other.Cells[i], g)) {
				return Bool(false)
			}
		}
		return Bool(true)
	case LArray:
		g, stop := g.descend(v, other)
		if stop {
			return Bool(true)
		}
		// NOTE:  This is a pretty cheeky for loop.  The first comparison it
		// does will compare array dimensions, which will ensure that we don't
		// hit an index out of bounds while comparing later indices.
		for i := range v.Cells {
			if Not(v.Cells[i].equal(other.Cells[i], g)) {
				return Bool(false)
			}
		}
		return Bool(true)
	case LTaggedVal:
		if v.Str != other.Str {
			return Bool(false)
		}
		g, stop := g.descend(v, other)
		if stop {
			return Bool(true)
		}
		return v.Cells[0].equal(other.Cells[0], g)
	case LSortMap:
		if v.Map().Len() != other.Map().Len() {
			return Bool(false)
		}
		g, stop := g.descend(v, other)
		if stop {
			return Bool(true)
		}
		vEntries := sortedMapEntries(v.Map())
		oEntries := sortedMapEntries(other.Map())
		for i := range vEntries.Cells {
			vPair := vEntries.Cells[i]
			oPair := oEntries.Cells[i]
			if !True(equalMapKey(vPair.Cells[0], oPair.Cells[0], g)) {
				return Bool(false)
			}
			if !True(vPair.Cells[1].equal(oPair.Cells[1], g)) {
				return Bool(false)
			}
		}
		return Bool(true)
	case LInvalid, LInt, LFloat, LError, LQSymbol, LFun, LQuote, LBytes,
		LNative, LMarkTerminal, LMarkTailRec, LMarkMacExpand, LTypeMax:
		// No structural equality is defined for these types, so equal? reports
		// false even when both operands are the same object.  Enumerated
		// rather than left to fall through so that a new LType has to make
		// this choice explicitly.
		//
		// LInt and LFloat are unreachable: the IsNumeric shortcut above
		// diverts every numeric comparison to equalNum.  LInvalid, the LMark*
		// sentinels and LTypeMax are not values an application can hold.
		return Bool(false)
	}
	return Bool(false)
}

// equalMapKey compares two sorted-map keys under the map's own notion of key
// identity.
//
// For the string-like keys the stock sortedmap accepts, identity is the key
// *name*: get, key?, assoc and dissoc all take either 'a or "a" for the same
// entry (docs/lang.md), so equality must too.  The string/symbol distinction
// is cosmetic — it reaches keys and printing, and nothing else.
//
// Every other key type falls back to Equal.  Map is an exported interface and
// SortedMapFromData an exported extension point, so an embedder may back a
// sorted-map with a store keyed by integers, tuples or anything else.  Those
// keys carry no name at all: comparing Str would make every one of them equal
// to every other, silently reporting structurally different maps as equal.
// The name rule was reasoned about for string-like keys only, and it is
// deliberately not extended past them.
func equalMapKey(a, b *LVal, g pairGuard) *LVal {
	if isStringLike(a) && isStringLike(b) {
		return Bool(a.Str == b.Str)
	}
	return a.equal(b, g)
}

// isStringLike reports whether v is one of the name-carrying key types the
// stock sortedmap accepts.
func isStringLike(v *LVal) bool {
	return v.Type == LString || v.Type == LSymbol
}

func (v *LVal) EqualNum(other *LVal) *LVal {
	if !v.IsNumeric() {
		return Errorf("receiver is not a number: %v", v.Type)
	}
	if !other.IsNumeric() {
		return Errorf("argument is not a number: %s", other.Type)
	}
	return v.equalNum(other)
}

func (v *LVal) equalNum(other *LVal) *LVal {
	if bothInt(v, other) {
		return Bool(v.Int == other.Int)
	}

	// This may not be correct
	return Bool(toFloat(v) == toFloat(other))
}

// Copy creates a deep copy of the receiver.
//
// Copy has within-runtime semantics — an LArray's backing storage is shared
// with the receiver, so it is not a tool for transferring values between
// Runtimes; the in-kernel detach (lisp/detach.go, unexported until a real
// consumer appears) covers that.
//
// The copy owns its positions.  Every node Copy reaches gets its own
// *token.Location, so a write through the copy cannot move what the original
// reports, and the reverse.  That holds for the Locations reachable THROUGH
// meta's comment tokens as well as for source on the node.  Values Go
// constructed carry no location at all (source is nil and the accessor
// synthesizes one by value), so there is nothing left to share -- the
// process-wide "<native code>" singleton this used to except is deleted, see
// nativeLocation.
//
// The copy also owns its per-node metadata: meta and macroExpansion.  The one
// thing deliberately still shared is macroExpansionInfo's embedded
// *macroExpansionContext, which describes the macro CALL rather than the node
// -- see macroExpansionInfo.Copy.
//
// The copy owns its payloads, once each.  A sorted map, a bytes buffer or a
// NativeCloner payload reachable through several headers is rebuilt ONCE
// and shared by those headers in the copy exactly as it was in the source;
// a value reachable twice is copied once and a cycle closes onto the copy.
// A map's values are walked like a list's cells.  What stays shared: an
// LArray's backing, a closure's environment, an LError's call stack, and a
// native payload that is not a NativeCloner.  See copier in lisp/copier.go.
func (v *LVal) Copy() *LVal {
	if v == nil {
		return nil
	}
	// One walk, one set of memos: see copier in lisp/copier.go for what is
	// memoised and why.  Stack-resident; nothing captures it, and a leaf
	// costs its header alone.
	var c copier
	return c.copy(v)
}

// copyMapData returns a fresh *MapData holding v's entries with the value
// pointers shared: the map structure is private to the copy, the values are
// not.  This is what assoc and dissoc build on every call, so the stock
// sorted map is cloned structurally (sortedmap.clone) rather than by
// enumerating its entries in sorted order and re-inserting them; an
// embedder map implementation keeps the entries path.
func (v *LVal) copyMapData() (*MapData, error) {
	m0 := v.Map()
	if m0 == nil {
		return nil, nil
	}
	if sm, ok := m0.mapBacking.(sortedmap); ok {
		return &MapData{sm.clone(nil)}, nil
	}
	if r, ok := m0.mapBacking.(StringKeyRanger); ok {
		// Same stock map the entries path builds for a string-keyed
		// embedder map, without boxing the entries first.
		nm := emptyForStringKeys(m0.Len())
		if err := r.RangeStringKeys(func(k string, v *LVal) {
			nm.m[k] = v
		}); err != nil {
			return nil, fmt.Errorf("failed to copy map: %w", err)
		}
		return &MapData{nm}, nil
	}
	m := &MapData{newmap()}
	for _, pair := range sortedMapEntries(m0).Cells {
		lerr := m.Set(pair.Cells[0], pair.Cells[1])
		if lerr.Type == LError {
			return nil, fmt.Errorf("failed to copy map: %v", lerr)
		}
	}
	return m, nil
}

// String renders v as lisp source.
//
// A value that contains itself renders the marker "#<cycle>" at the point the
// walk reaches it a second time, so the result is finite and the walk cannot
// overflow the goroutine stack and kill the process.  Rendering is otherwise
// unchanged: an acyclic value renders in full, at any nesting depth, exactly
// as it always did.  See lisp/cycle.go and issue #390.
func (v *LVal) String() string {
	var st cycleState
	s := v.stringGuard(cycleGuard{state: &st})
	if !st.cyclic {
		return s
	}
	// v contains itself.  The walk above stopped as soon as it knew that,
	// because unrolling a cycle to cycleGuardDepth levels is exponential in
	// the width of the cycle; the rerun visits each node once.
	return v.stringGuard(strictCycleGuard())
}

func (v *LVal) stringGuard(g cycleGuard) string {
	const QUOTE = `'`
	if v.Type == LQuote {
		return QUOTE + v.Cells[0].str(true, g)
	}
	return v.str(false, g)
}

// JoinDocStrings joins multiple doc string parts into a single string.
// Non-empty strings are joined with spaces.  Empty strings produce blank
// lines, acting as paragraph separators.
func JoinDocStrings(parts []string) string {
	var b strings.Builder
	for i, p := range parts {
		if p == "" {
			b.WriteString("\n\n")
		} else {
			if i > 0 && parts[i-1] != "" {
				b.WriteByte(' ')
			}
			b.WriteString(p)
		}
	}
	return b.String()
}

// Docstring returns the docstring of the function reference v.  If v is not
// a function Docstring returns the empty string.  For user-defined functions,
// consecutive leading string expressions in the body are concatenated to form
// the docstring (the body must contain at least one non-string expression
// after the doc strings).  Empty strings produce paragraph breaks.
func (v *LVal) Docstring() string {
	if v.Type != LFun {
		return ""
	}
	if v.Builtin() != nil {
		if len(v.Cells) > 1 {
			return v.Cells[1].Str
		}
		return ""
	}
	// Functions of the form (lambda (x) "abc") are considered constant string
	// functions without documentation so there must be a length check on the
	// function body.
	if len(v.Cells) > 2 && v.Cells[1].Type == LString {
		var parts []string
		for i := 1; i < len(v.Cells); i++ {
			if v.Cells[i].Type != LString {
				break
			}
			parts = append(parts, v.Cells[i].Str)
		}
		// Only treat as docstring if there's at least one non-string
		// body expression after the strings.
		if len(parts) < len(v.Cells)-1 {
			return JoinDocStrings(parts)
		}
	}
	return ""
}

// str renders v, with g bounding the walk so that a value containing itself
// renders cycleMark instead of recursing until the process dies.  Every
// nested render must pass g down rather than starting a fresh walk with
// String, or the bound is lost.  See lisp/cycle.go.
//
// The types that render from their own fields and reach nothing are handled
// here, ahead of the guard and running exactly the code they ran before it
// existed.  Rendering leaves is most of what this walk does, and none of them
// can be part of a cycle.
func (v *LVal) str(onTheRecord bool, g cycleGuard) string {
	const QUOTE = `'`
	// All types which may evaluate to things other than themselves must check
	// v.quoted.
	quote := ""
	if onTheRecord {
		quote = QUOTE
	}
	switch v.Type {
	case LInt:
		return quote + strconv.Itoa(v.Int)
	case LFloat:
		// NOTE:  The 'g' format can render a floating point number such that
		// it appears as an integer (2.0 renders as 2) which can be confusing
		// for those interested in the type of each numeric value.
		return quote + strconv.FormatFloat(v.Float, 'g', -1, 64)
	case LString:
		return quote + fmt.Sprintf("%q", v.Str)
	case LBytes:
		b := v.Bytes()
		if len(b) == 0 {
			return quote + "#<bytes>"
		}
		return quote + "#<bytes " + strings.Trim(fmt.Sprint(b), "[]") + ">" //nolint:staticcheck // fmt.Sprint gives byte slice repr, not string conversion
	case LSymbol:
		if v.quoted {
			quote = QUOTE
		}
		return quote + v.Str
	case LNative:
		return fmt.Sprintf("#<native value: %T>", v.Native)
	default:
		// Every remaining type renders values reachable from v, and is
		// handled by strNested below.  Enumerated as a default rather than
		// left implicit so that a new LType has to decide which half of this
		// function it belongs in.
	}
	// Everything left renders values reachable from v, so it is entered on the
	// guard's path.
	if g.abandoned() {
		return ""
	}
	g, cyclic := g.descend(v)
	if cyclic {
		return cycleMark
	}
	s := v.strNested(onTheRecord, g)
	if g.tracking() {
		g.ascend(v)
	}
	return s
}

// strNested renders the types that reach other values.  It is only ever
// reached through str, which has already put v on g's path.
func (v *LVal) strNested(onTheRecord bool, g cycleGuard) string {
	const QUOTE = `'`
	quote := ""
	if onTheRecord {
		quote = QUOTE
	}
	switch v.Type {
	case LError:
		if v.quoted {
			quote = QUOTE
			return quote + fmt.Sprintf("(error '%s %s)", v.Str, v.Cells[0].str(false, g))
		}
		return (*ErrorVal)(v).errorString(g)
	case LSExpr:
		if v.quoted {
			quote = QUOTE
		}
		return exprString(v, 0, quote+"(", ")", g)
	case LFun:
		if v.quoted {
			quote = QUOTE
		}
		if v.Builtin() != nil {
			return quote + "#<builtin>"
		}
		// The formals render directly.  There is no second list to
		// concatenate them with: what used to follow them was the
		// function's own environment scope, which was always empty (see
		// the note on funData.env), so this prints exactly what the
		// concatenation printed.
		return fmt.Sprintf("%s(lambda %s%s)", quote, exprString(v.Cells[0], 0, "(", ")", g), bodyStr(v.Cells[1:], g))
	case LQuote:
		// TODO: make more efficient
		return QUOTE + v.Cells[0].str(true, g)
	case LSortMap:
		return quote + sortedMapString(v, g)
	case LArray:
		if v.Cells[0].Len() == 1 {
			if v.Len() > 0 {
				return exprString(v.Cells[1], 0, quote+"(vector ", ")", g)
			} else {
				return quote + "(vector)"
			}
		}
		return fmt.Sprintf("#<array dims=%s>", v.Cells[0].str(false, g))
	case LTaggedVal:
		return fmt.Sprintf("#{%s %s}", v.Str, v.Cells[0].str(false, g))
	case LMarkTerminal:
		return quote + fmt.Sprintf("#<terminal-expression %s>", v.Cells[0].str(false, g))
	case LMarkTailRec:
		return quote + fmt.Sprintf("#<tail-recursion frames=%d (%s %s)>", v.Cells[0].Int, v.Cells[1].str(false, g), v.Cells[2].str(false, g))
	case LMarkMacExpand:
		return quote + fmt.Sprintf("#<macro-expansion %s)>", v.Cells[0].str(false, g))
	default:
		return quote + fmt.Sprintf("#<%s %#v>", v.Type, v)
	}
}

func bodyStr(exprs []*LVal, g cycleGuard) string {
	var buf bytes.Buffer
	for i := range exprs {
		buf.WriteString(" ")
		buf.WriteString(exprs[i].str(false, g))
	}
	return buf.String()
}

func exprString(v *LVal, offset int, left string, right string, g cycleGuard) string {
	if len(v.Cells[offset:]) == 0 {
		return left + right
	}
	var buf bytes.Buffer
	buf.WriteString(left)
	for i, c := range v.Cells[offset:] {
		if i > 0 {
			buf.WriteString(" ")
		}
		buf.WriteString(c.str(false, g))
	}
	buf.WriteString(right)
	return buf.String()
}

func isVec(v *LVal) bool {
	return v.Type == LArray && v.Cells[0].Len() == 1
}

func isSeq(v *LVal) bool {
	return v.Type == LSExpr || isVec(v)
}

func seqCells(v *LVal) []*LVal {
	// Callers must guard with isSeq.  The panic is the assertion for that
	// contract -- it is in a default clause so that reaching seqCells with a
	// type nobody thought about is loud rather than silent.
	//
	// NOT LISP-REACHABLE (#367): every caller either tests isSeq(v) and
	// returns "argument is not a proper sequence" first, or passes a
	// one-dimensional array this function just built.  isSeq is what makes
	// the multi-dimensional case unreachable too: it requires
	// v.Cells[0].Len() == 1, so an array with more than one dimension is
	// rejected as a sequence before it gets here.  A caller that forgets the
	// guard is caught by TestBuiltinRegistryNeverPanics (lisp/lisplib), which
	// offers a multi-dimensional array to every registered builtin in every
	// argument position.
	switch v.Type {
	case LSExpr:
		return v.Cells
	case LArray:
		if v.Cells[0].Len() > 1 {
			panic("multi-dimensional array is not a sequence")
		}
		return v.Cells[1].Cells
	default:
		panic("type is not a sequence")
	}
}

func makeByteSeq(v *LVal) *LVal {
	switch v.Type {
	case LString:
		v = Bytes([]byte(v.Str))
		fallthrough
	case LBytes:
		b := v.Bytes()
		cells := make([]*LVal, len(b))
		for i := range b {
			cells[i] = Int(int(b[i]))
		}
		return QExpr(cells)
	default:
		return Errorf("type is not a native byte sequence: %v", v.Type)
	}
}

// nativeLocation returns the synthetic location reported for values that
// were constructed by Go code rather than read from a source file.  It is
// produced by value on demand: there is no shared "<native code>" Location
// object anymore, so issue #362's shared-singleton corruption vector no
// longer exists — constructors simply leave LVal.source nil and the
// accessor/print paths synthesize this location.
func nativeLocation() token.Location {
	return token.NativeLocation()
}
