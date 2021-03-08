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
	// by builtin functions store references to values of any type.
	LNative
	// Mark LVals are used to trasmit information down the stack through return
	// values.  Because the LEnv does not evaluate expressions using a stack
	// based virtual machine these Mark values, which often wrap other LVal
	// data in their Cells, are passed back from functions.  Typically the
	// environment is solely responsible for managing mark values and
	// applications should never see them during calls to builtin functions.
	LMarkTerminal  // LEnv marks the frame as terminal and evaluates tho contained expr
	LMarkTailRec   // LEnv resumes a call a set number of frames down the stack.
	LMarkMacExpand // LEnv will evaluate the returned LVal a subsequent time.
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
	LMarkTailRec:   "marker-tail-recursion",
	LMarkMacExpand: "marker-macro-expansion",
}

func (t LType) String() string {
	if int(t) >= len(lvalTypeStrings) {
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

type LFunData struct {
	FID     string
	Package string
	Builtin LBuiltin
	Env     *LEnv
}

func (fd *LFunData) Copy() *LFunData {
	cp := &LFunData{}
	*cp = *fd
	cp.Env = fd.Env.Copy()
	return cp
}

// LVal is a lisp value
type LVal struct {
	// Type is the native type for a value in lisp.
	Type LType

	// Source is the values originating location in source code.  Programs
	// should not modify the contents of Source as the reference may be shared
	// by multiple LVals.
	Source *token.Location

	// Fields used for numeric types
	Int   int
	Float float64

	// Str used by LSymbol and LString values
	Str string

	// Cells used by many values as a storage space for lisp objects.
	//
	// TODO: Consider making Cells' type []LVal instead of []*LVal to reduce
	// the burden on the allocator/gc.
	Cells []*LVal

	// Native is generic storage for data which cannot be represented as an
	// LVal (and thus can't be stored in Cells).
	Native interface{}

	// FunType used to further classify LFun values
	FunType LFunType

	Quoted  bool // flag indicating a single level of quoting
	Spliced bool // denote the value as needing to be spliced into a parent value
}

// Value conveniently converts v to an LVal.  Types which can be represented
// directly in lisp will be converted to the appropriate LVal.  All other types
// will be turned into a Native LVal.  Value is the GoValue function.
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

// Bool returns an LVal with truthiness identical to b.
func Bool(b bool) *LVal {
	if b {
		return Symbol(TrueSymbol)
	}
	return Symbol(FalseSymbol)
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
func Nil() *LVal {
	return SExpr(nil)
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

// SortedMap returns an LVal represented a sorted map
func SortedMap() *LVal {
	return &LVal{
		Source: nativeSource(),
		Type:   LSortMap,
		Native: make(map[interface{}]*LVal),
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
	fun.Str = symbol.Str
	return fun
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
		Cells: []*LVal{formals},
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
		Cells: []*LVal{formals},
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
		Cells: []*LVal{formals},
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
		return len(v.Map())
	case LArray:
		if v.Cells[0].Len() == 1 {
			return v.Cells[0].Cells[0].Int
		}
		fallthrough
	default:
		return -1
	}
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

func (v *LVal) Map() map[interface{}]*LVal {
	if v.Type != LSortMap {
		panic("not sorted-map: " + v.Type.String())
	}
	return v.Native.(map[interface{}]*LVal)
}

// MapKeys returns a list of keys in the map.  MapKeys panics if v.Type is not
// LSortMap.  The type of each map key is retained from the first type a value
// was set for that key.  For example, if the MapSet(Symbol("a"), Int(1)) is
// called before MapSet(String("a"), Int(2)) then MapKey() will contain the
// symbol and not the string.
func (v *LVal) MapKeys() *LVal {
	if v.Type != LSortMap {
		panic("not sortmap: " + v.Type.String())
	}
	list := QExpr(sortedMapKeys(v))
	return list
}

// ArrayDims returns the dimensions of an array.  ArrayDims panics if v.Type is
// not LArray
func (v *LVal) ArrayDims() *LVal {
	if v.Type != LArray {
		panic("not an array: " + v.Type.String())
	}
	return v.Cells[0].Copy()
}

// ArrayIndex returns the value at
func (v *LVal) ArrayIndex(index ...*LVal) *LVal {
	if v.Type != LArray {
		panic("not an array: " + v.Type.String())
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
	if v.Type != LSortMap {
		panic("not sortmap: " + v.Type.String())
	}
	switch k := k.(type) {
	case *LVal:
		return mapGet(v, k, nil)
	case string:
		return mapGet(v, String(k), nil)
	// numerics unsupported
	default:
		return Errorf("invalid key type: %T", k)
	}
}

// MapSet sets k to val in v.  MapSet panics if v.Type is not LSortMap.
// String and symbol keys are coerced to avoid programming errors causing
// symbol and string keys with equal string values from existing in the same
// map.
func (v *LVal) MapSet(k interface{}, val *LVal) *LVal {
	if v.Type != LSortMap {
		panic("not sortmap: " + v.Type.String())
	}
	switch k := k.(type) {
	case *LVal:
		return mapSet(v, k, val, true)
	case string:
		return mapSet(v, String(k), val, true)
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
// rules used by the ``equal?'' function.
//
// BUG:  sorted-map comparison is not implemented
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
	case LSortMap:
		if len(v.Map()) != len(other.Map()) {
			return Bool(false)
		}

		return Bool(false)
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
	if v.Type != LArray {
		// Arrays are memory references but use Cells as backing storage.  So
		// we can only copy the cells when the type is not an array.
		cp.Cells = v.copyCells()
	}
	return cp
}

func (v *LVal) copyMap() map[interface{}]*LVal {
	m0 := v.Map()
	if m0 == nil {
		return nil
	}
	m := make(map[interface{}]*LVal, len(m0))
	for k, v := range m0 {
		// Is v.Copy() really necessary here? It seems like things get copied
		// on mapGet anyway..
		m[k] = v.Copy()
	}
	return m
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
		return quote + "#<bytes " + strings.Trim(fmt.Sprint(b), "[]") + ">"
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
		panic("type is not a native byte sequence")
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
