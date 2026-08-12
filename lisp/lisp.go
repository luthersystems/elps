// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
	"strings"

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
	// 		LVal.Native   An LFunData object
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
	// the LVal.Quoted field on a value with a normal value in LVal.Type.
	// LQuote values must always have a true LVal.Quoted field.
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

type LFunData struct {
	Builtin LBuiltin
	Env     *LEnv
	FID     string
	Package string
}

func (fd *LFunData) Copy() *LFunData {
	cp := &LFunData{}
	*cp = *fd
	cp.Env = fd.Env.Copy()
	return cp
}

// MacroExpansionContext is shared by all nodes in a single macro expansion.
// It records the macro call site, name, definition site, and unevaluated
// arguments for debugger inspection.
type MacroExpansionContext struct {
	CallSite *token.Location // where the macro was invoked
	Name     string          // qualified macro name (e.g. "lisp:defun")
	DefSite  *token.Location // macro definition location (nil for builtins)
	Args     []*LVal         // unevaluated call-site arguments (for debugger scope)
}

// MacroExpansionInfo is attached to LVal nodes produced by macro expansion.
// It is only allocated when a debugger is attached (Runtime.Debugger != nil),
// so production code pays zero allocation cost.
type MacroExpansionInfo struct {
	*MacroExpansionContext       // shared across all nodes in one expansion
	ID                     int64 // unique per node, monotonically increasing
}

// SourceMeta holds formatting metadata for an LVal, populated only when
// parsing in format-preserving mode. Nil in normal parsing — zero cost.
type SourceMeta struct {
	TrailingComment         *token.Token   // inline comment on same line after this node
	OriginalText            string         // original token text for literals (preserves escapes, numeric bases)
	LeadingComments         []*token.Token // comment tokens preceding this node
	InnerTrailingComments   []*token.Token // comments between last child and closing bracket
	BlankLinesBefore        int            // blank lines (newline count - 1) before this node (or before its leading comments)
	BlankLinesAfterComments int            // blank lines between last leading comment and the expression
	PrecedingSpaces         int            // spaces before this token on the same line (for column alignment)
	BracketType             rune           // '(' or '[' for LSExpr nodes
	NewlineBefore           bool           // true if at least one newline preceded this node in source
	ClosingBracketNewline   bool           // true if closing bracket was on its own line in source
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

	// Source is the values originating location in source code.  Programs
	// should not modify the contents of Source as the reference may be shared
	// by multiple LVals.
	Source *token.Location

	// Meta holds formatting metadata, only populated in format-preserving mode.
	Meta *SourceMeta

	// MacroExpansion holds debug metadata for nodes produced by macro
	// expansion. Only populated when a debugger is attached — nil in
	// production (zero overhead: 8-byte nil pointer).
	MacroExpansion *MacroExpansionInfo

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

	// Quoted is a flag indicating a single level of quoting.
	Quoted bool

	// Spliced denotes the value as needing to be spliced into a parent value.
	Spliced bool
}

// GetType returns a quoted symbol denoting v's type.
func GetType(v *LVal) *LVal {
	t := Symbol(v.Str)
	t.Quoted = true
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
		Source: nativeSource(),
		Type:   LInt,
		Int:    x,
	}
}

// Float returns an LVal representation of the number x
func Float(x float64) *LVal {
	return &LVal{
		Source: nativeSource(),
		Type:   LFloat,
		Float:  x,
	}
}

// String returns an LVal representing the string str.
func String(str string) *LVal {
	return &LVal{
		Source: nativeSource(),
		Type:   LString,
		Str:    str,
	}
}

// Bytes returns an LVal representing binary data b.
func Bytes(b []byte) *LVal {
	return &LVal{
		Source: nativeSource(),
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
		Source: nativeSource(),
		Type:   LSymbol,
		Str:    s,
	}
}

// QSymbol returns an LVal representing the quoted symbol
func QSymbol(s string) *LVal {
	return &LVal{
		Source: nativeSource(),
		Type:   LQSymbol,
		Str:    s,
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
		Source: nativeSource(),
		Type:   LNative,
		Native: v,
	}
}

// SExpr returns an LVal representing an S-expression, a symbolic expression.
// Provided cells are used as backing storage for the returned expression and
// are not copied.
func SExpr(cells []*LVal) *LVal {
	return &LVal{
		Source: nativeSource(),
		Type:   LSExpr,
		Cells:  cells,
	}
}

// QExpr returns an LVal representing an Q-expression, a quoted expression, a
// list.  Provided cells are used as backing storage for the returned list and
// are not copied.
func QExpr(cells []*LVal) *LVal {
	return &LVal{
		Source: nativeSource(),
		Type:   LSExpr,
		Quoted: true,
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
func Array(dims *LVal, cells []*LVal) *LVal {
	if dims == nil {
		dims = QExpr([]*LVal{Int(len(cells))})
	} else if dims.Type != LSExpr {
		return Errorf("array dimensions are not a list: %v", dims.Type)
	} else {
		for _, n := range dims.Cells {
			if n.Type != LInt {
				return Errorf("array dimension is not an integer: %v", n.Type)
			}
		}
	}
	totalSize := 1
	for _, n := range dims.Cells {
		totalSize *= n.Int
		if totalSize < 0 {
			return Errorf("integer overflow")
		}
	}
	if len(cells) > 0 && len(cells) != totalSize {
		return Errorf("array contents do not match size")
	} else if len(cells) == 0 {
		cells = make([]*LVal, totalSize)
	}

	return &LVal{
		Source: nativeSource(),
		Type:   LArray,
		Cells: []*LVal{
			dims.Copy(),
			QExpr(cells),
		},
	}
}

// SortedMap returns an LVal representing a sorted map
func SortedMap() *LVal {
	return SortedMapFromData(&MapData{newmap()})
}

// SortedMapFromData returns sorted-map with the given backing implementation.
// Applications calling this function must make ensure the Map implementation
// provided satisfies the semantics of Map methods.
func SortedMapFromData(data *MapData) *LVal {
	return &LVal{
		Source: nativeSource(),
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
		Source: nativeSource(),
		Type:   LFun,
		Native: &LFunData{
			FID:     fid,
			Builtin: fn,
			Package: pkg,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// Fun returns an LVal representing a function. Package is left empty;
// callers MUST set v.FunData().Package before the value is invoked, or
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
		Source:  nativeSource(),
		Type:    LFun,
		FunType: LFunMacro,
		Native: &LFunData{
			FID:     fid,
			Builtin: fn,
			Package: pkg,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// Macro returns an LVal representing a macro. Package is left empty;
// callers MUST set v.FunData().Package before the value is invoked, or
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
		Source:  nativeSource(),
		Type:    LFun,
		FunType: LFunSpecialOp,
		Native: &LFunData{
			FID:     fid,
			Builtin: fn,
			Package: pkg,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// SpecialOp returns an LVal representing a special operator.  Special
// operators are function which receive unevaluated results, like macros.
// However values returned by special operations do not require further
// evaluation, unlike macros.
//
// Package is left empty; callers MUST set v.FunData().Package before the
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
		Source: nativeSource(),
		Type:   LError,
		Str:    condition,
		Cells:  []*LVal{Native(err)},
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
		Source: nativeSource(),
		Type:   LError,
		Str:    condition,
		Cells:  []*LVal{String(fmt.Sprintf(format, v...))},
	}
}

// Quote quotes v and returns the quoted value.  The LVal v is modified.
func Quote(v *LVal) *LVal {
	if !v.Quoted {
		cp := &LVal{}
		*cp = *v
		cp.Quoted = true
		return cp
	}
	quote := &LVal{
		Source: nativeSource(),
		Type:   LQuote,
		Quoted: true,
		Cells:  []*LVal{v},
	}
	return quote
}

// Splice is used in the implementation of quasiquote to insert a list into an
// outer slist.
func Splice(v *LVal) *LVal {
	cp := &LVal{}
	*cp = *v
	cp.Spliced = true
	return cp
}

// shallowUnquote is an artifact from when functions could freely modify LVals
// It may be worth trying to unify all quoting under the LQuote type.
func shallowUnquote(v *LVal) *LVal {
	cp := &LVal{}
	*cp = *v
	cp.Quoted = false
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
// This is the check `ignore-errors` and `handler-bind` use, and embedders
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

func (v *LVal) SetCallStack(stack *CallStack) {
	if v.Type != LError {
		panic("not an error: " + v.Type.String())
	}
	v.Native = stack.Copy()
}

func (v *LVal) FunData() *LFunData {
	if v.Type != LFun {
		panic("not a function: " + v.Type.String())
	}
	return v.Native.(*LFunData)
}

func (v *LVal) Package() string {
	return v.FunData().Package
}

func (v *LVal) Builtin() LBuiltin {
	return v.FunData().Builtin
}

func (v *LVal) FID() string {
	return v.FunData().FID
}

func (v *LVal) Env() *LEnv {
	return v.FunData().Env
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
func (v *LVal) Bytes() []byte {
	if v.Type != LBytes {
		panic("not bytes: " + v.Type.String())
	}
	// NOTE:  Bytes are stored as a pointer to a slice to allow for effecient
	// appending in the same style as normal vectors.
	return *v.Native.(*[]byte)
}

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
		return Errorf("invalid index into array with dimenions %#v", v.Cells[0])
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
func (v *LVal) Copy() *LVal {
	if v == nil {
		return nil
	}
	cp := &LVal{}
	*cp = *v // shallow copy of all fields including Map and Bytes
	switch v.Type {
	case LArray:
		// Arrays are memory references but use Cells as backing storage.
		// We preserve the shared backing array (reference semantics).
	case LSortMap:
		// Sorted-maps store data in Native (*MapData) which contains Go
		// maps. A shallow struct copy would alias the underlying maps,
		// causing assoc!/dissoc! on the copy to mutate the original.
		// Copy the map structure while sharing value pointers.
		mdata, err := v.copyMapData()
		if err != nil {
			return Errorf("copy sorted-map: %v", err)
		}
		cp.Native = mdata
	default:
		cp.Cells = v.copyCells()
	}
	return cp
}

func (v *LVal) copyMapData() (*MapData, error) {
	m0 := v.Map()
	if m0 == nil {
		return nil, nil
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

func (v *LVal) copyCells() []*LVal {
	if len(v.Cells) == 0 {
		return nil
	}
	cells := make([]*LVal, len(v.Cells))
	for i := range cells {
		cells[i] = v.Cells[i].Copy()
	}
	return cells
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
	// v.Quoted.
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
		if v.Quoted {
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
		if v.Quoted {
			quote = QUOTE
			return quote + fmt.Sprintf("(error '%s %s)", v.Str, v.Cells[0].str(false, g))
		}
		return (*ErrorVal)(v).errorString(g)
	case LSExpr:
		if v.Quoted {
			quote = QUOTE
		}
		return exprString(v, 0, quote+"(", ")", g)
	case LFun:
		if v.Quoted {
			quote = QUOTE
		}
		if v.Builtin() != nil {
			return quote + "#<builtin>"
		}
		vars := lambdaVars(v.Cells[0], boundVars(v))
		return fmt.Sprintf("%s(lambda %s%s)", quote, vars.str(false, g), bodyStr(v.Cells[1:], g))
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

// lambdaVars renders a function's formals and closure bindings as a
// single unquoted list, for (*LVal).str.
//
// builtinConcat does NOT always hand back a freshly allocated list --
// the comment that used to sit on the unquote here claimed it did, and
// that was the bug in issue #333. builtinConcatSeq short-circuits the
// empty case with `return Nil()`, so a function with no formals AND no
// bound vars made this write `Quoted = false` straight into the
// process-wide singletonNil. It stored the value the field already
// held, which made it invisible to SingletonSnapshot.Verify (see the
// note on checkSingleton), but it was still a write to shared memory
// and raced with every concurrent (*LEnv).eval reading `v.Quoted` off a
// Nil().
//
// shallowUnquote copies before clearing Quoted, so lambdaVars mutates
// only a value it owns. It copies unconditionally rather than only when
// builtinConcat happens to share, which makes the contract the simple
// one -- lambdaVars returns a value the caller owns -- instead of the
// fragile one, "the result is usually fresh, but don't write to it."
// That contract is checkable without the race detector; see
// TestLambdaVarsDoesNotReturnSingleton. The extra LVal is noise next to
// the fmt.Sprintf this feeds, and only printing a function reaches it.
func lambdaVars(formals *LVal, bound *LVal) *LVal {
	s := SExpr([]*LVal{Quote(Symbol("list")), formals, bound})
	return shallowUnquote(builtinConcat(nil, s))
}

func boundVars(v *LVal) *LVal {
	env := v.Env()
	if env == nil {
		return Nil()
	}
	keys := make([]string, 0, len(env.Scope))
	for k := range env.Scope {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	bound := SExpr(nil)
	for i := range keys {
		q := SExpr([]*LVal{
			Symbol(keys[i]),
			env.Get(Symbol(keys[i])),
		})
		bound.Cells = append(bound.Cells, q)
	}
	return bound
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

var defaultSourceLocation = &token.Location{
	File: "<native code>",
	Pos:  -1,
}

// TODO(elps2): make the LVal.Source "immutable" (possibly an interface or a
// string) so it won't matter that nativeSource returns a shared reference.
func nativeSource() *token.Location {
	return defaultSourceLocation
}
