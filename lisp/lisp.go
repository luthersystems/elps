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

// SourceMeta holds formatting metadata for an LVal, populated only when
// parsing in format-preserving mode. Nil in normal parsing — zero cost.
type SourceMeta struct {
	OriginalText            string         // original token text for literals (preserves escapes, numeric bases)
	BracketType             rune           // '(' or '[' for LSExpr nodes
	LeadingComments         []*token.Token // comment tokens preceding this node
	TrailingComment         *token.Token   // inline comment on same line after this node
	InnerTrailingComments   []*token.Token // comments between last child and closing bracket
	BlankLinesBefore        int            // blank lines (newline count - 1) before this node (or before its leading comments)
	BlankLinesAfterComments int            // blank lines between last leading comment and the expression
	PrecedingSpaces         int            // spaces before this token on the same line (for column alignment)
	NewlineBefore           bool           // true if at least one newline preceded this node in source
	ClosingBracketNewline   bool           // true if closing bracket was on its own line in source
}

// LVal is a lisp value
type LVal struct {
	// Native is generic storage for data which cannot be represented as an
	// LVal (and thus can't be stored in Cells).

	Native interface{}

	// Source is the values originating location in source code.  Programs
	// should not modify the contents of Source as the reference may be shared
	// by multiple LVals.

	Source *token.Location

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

	// Meta holds formatting metadata, only populated in format-preserving mode.
	Meta *SourceMeta
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

// Singleton LVals for Nil, true, and false.
//
// These are pre-allocated, shared, immutable values returned by Nil(),
// Bool(true), and Bool(false). They exist because these three values are
// constructed on nearly every evaluation cycle — Nil() is returned by
// ~130 call sites (every builtin/op that "returns nothing"), and Bool()
// is returned by every comparison, predicate, and type check.
//
// SAFETY: A full audit of all 127 Nil() and 63 Bool() call sites
// confirmed that no call site mutates the returned *LVal. The only
// mutation vector was stampMacroExpansion in macro.go, which has been
// guarded to skip empty SExprs (nil singletons). The ELPS language has
// no destructive list operations (no nconc, rplaca, etc.) — append!
// only works on vectors (LArray), not lists (LSExpr), and cons/append
// create new lists without mutating their inputs.
//
// INVARIANT: Code that receives a Nil(), Bool(true), or Bool(false)
// return value MUST NOT mutate any field on the returned *LVal. If you
// need a mutable nil value (e.g., to build up a list), use SExpr(nil)
// directly instead of Nil().
var (
	singletonNil   = &LVal{Source: nativeSource(), Type: LSExpr}
	singletonTrue  = &LVal{Source: nativeSource(), Type: LSymbol, Str: TrueSymbol}
	singletonFalse = &LVal{Source: nativeSource(), Type: LSymbol, Str: FalseSymbol}
)

// Bool returns an LVal with truthiness identical to b.
//
// The returned value is a shared singleton — callers MUST NOT mutate it.
func Bool(b bool) *LVal {
	if b {
		return singletonTrue
	}
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

// Fun returns an LVal representing a function
func Fun(fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Source: nativeSource(),
		Type:   LFun,
		Native: &LFunData{
			FID:     fid,
			Builtin: fn,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// Macro returns an LVal representing a macro
func Macro(fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Source:  nativeSource(),
		Type:    LFun,
		FunType: LFunMacro,
		Native: &LFunData{
			FID:     fid,
			Builtin: fn,
		},
		Cells: []*LVal{formals, String("")},
	}
}

// SpecialOp returns an LVal representing a special operator.  Special
// operators are function which receive unevaluated results, like macros.
// However values returned by special operations do not require further
// evaluation, unlike macros.
func SpecialOp(fid string, formals *LVal, fn LBuiltin) *LVal {
	return &LVal{
		Source:  nativeSource(),
		Type:    LFun,
		FunType: LFunSpecialOp,
		Native: &LFunData{
			FID:     fid,
			Builtin: fn,
		},
		Cells: []*LVal{formals, String("")},
	}
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
func (v *LVal) IsNil() bool {
	switch v.Type {
	case LSExpr:
		return len(v.Cells) == 0
	}
	return false
}

// IsNumeric returns true if v has a primitive numeric type (int, float64).
func (v *LVal) IsNumeric() bool {
	switch v.Type {
	case LInt:
		return true
	case LFloat:
		return true
	}
	return false
}

// Equal returns a non-nil value if v and other are logically equal, under the
// rules used by the "equal?" function.
func (v *LVal) Equal(other *LVal) *LVal {
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
		for i := range v.Cells {
			if !True(v.Cells[i].Equal(other.Cells[i])) {
				return Bool(false)
			}
		}
		return Bool(true)
	case LArray:
		// NOTE:  This is a pretty cheeky for loop.  The first comparison it
		// does will compare array dimensions, which will ensure that we don't
		// hit an index out of bounds while comparing later indices.
		for i := range v.Cells {
			if Not(v.Cells[i].Equal(other.Cells[i])) {
				return Bool(false)
			}
		}
		return Bool(true)
	case LTaggedVal:
		if v.Str != other.Str {
			return Bool(false)
		}
		return v.Cells[0].Equal(other.Cells[0])
	case LSortMap:
		if v.Map().Len() != other.Map().Len() {
			return Bool(false)
		}
		vEntries := sortedMapEntries(v.Map())
		oEntries := sortedMapEntries(other.Map())
		for i := range vEntries.Cells {
			vPair := vEntries.Cells[i]
			oPair := oEntries.Cells[i]
			if !True(vPair.Cells[0].Equal(oPair.Cells[0])) {
				return Bool(false)
			}
			if !True(vPair.Cells[1].Equal(oPair.Cells[1])) {
				return Bool(false)
			}
		}
		return Bool(true)
	}
	return Bool(false)
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

func (v *LVal) String() string {
	const QUOTE = `'`
	if v.Type == LQuote {
		return QUOTE + v.Cells[0].str(true)
	}
	return v.str(false)
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

func (v *LVal) str(onTheRecord bool) string {
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
	case LError:
		if v.Quoted {
			quote = QUOTE
			return quote + fmt.Sprintf("(error '%s %v)", v.Str, v.Cells[0])
		}
		return GoError(v).Error()
	case LSymbol:
		if v.Quoted {
			quote = QUOTE
		}
		return quote + v.Str
	case LSExpr:
		if v.Quoted {
			quote = QUOTE
		}
		return exprString(v, 0, quote+"(", ")")
	case LFun:
		if v.Quoted {
			quote = QUOTE
		}
		if v.Builtin() != nil {
			return fmt.Sprintf("%s#<builtin>", quote)
		}
		vars := lambdaVars(v.Cells[0], boundVars(v))
		return fmt.Sprintf("%s(lambda %v%v)", quote, vars, bodyStr(v.Cells[1:]))
	case LQuote:
		// TODO: make more efficient
		return QUOTE + v.Cells[0].str(true)
	case LSortMap:
		return quote + sortedMapString(v)
	case LArray:
		if v.Cells[0].Len() == 1 {
			if v.Len() > 0 {
				return exprString(v.Cells[1], 0, quote+"(vector ", ")")
			} else {
				return quote + "(vector)"
			}
		}
		return fmt.Sprintf("#<array dims=%s>", v.Cells[0])
	case LNative:
		return fmt.Sprintf("#<native value: %T>", v.Native)
	case LTaggedVal:
		return fmt.Sprintf("#{%s %v}", v.Str, v.Cells[0])
	case LMarkTerminal:
		return quote + fmt.Sprintf("#<terminal-expression %s>", v.Cells[0])
	case LMarkTailRec:
		return quote + fmt.Sprintf("#<tail-recursion frames=%d (%s %s)>", v.Cells[0].Int, v.Cells[1], v.Cells[2])
	case LMarkMacExpand:
		return quote + fmt.Sprintf("#<macro-expansion %s)>", v.Cells[0])
	default:
		return quote + fmt.Sprintf("#<%s %#v>", v.Type, v)
	}
}

func bodyStr(exprs []*LVal) string {
	var buf bytes.Buffer
	for i := range exprs {
		buf.WriteString(" ")
		buf.WriteString(exprs[i].String())
	}
	return buf.String()
}

func lambdaVars(formals *LVal, bound *LVal) *LVal {
	s := SExpr([]*LVal{Quote(Symbol("list")), formals, bound})
	s = builtinConcat(nil, s)
	s.Quoted = false // This is fine because builtinConcat returns a new list
	return s
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

func exprString(v *LVal, offset int, left string, right string) string {
	if len(v.Cells[offset:]) == 0 {
		return left + right
	}
	var buf bytes.Buffer
	buf.WriteString(left)
	for i, c := range v.Cells[offset:] {
		if i > 0 {
			buf.WriteString(" ")
		}
		buf.WriteString(c.String())
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
	switch v.Type {
	case LSExpr:
		return v.Cells
	case LArray:
		if v.Cells[0].Len() > 1 {
			panic("multi-dimensional array is not a sequence")
		}
		return v.Cells[1].Cells
	}
	panic("type is not a sequence")
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
