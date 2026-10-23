// Copyright © 2026 The ELPS authors

package lisp

// GoHelper names the Go code that returns what a language builtin returns,
// for a Go builtin that would otherwise call the builtin with CallBuiltin.
//
// Method is the LEnv method, when there is one.  It checks the evaluation's
// context first, as CallBuiltin does, and then runs the builtin's own body,
// so the value, the error and the allocation checks are the same.  Note is
// the Go to write instead when there is no method, such as a type test for
// a predicate; it makes no context check.
type GoHelper struct {
	Method string
	Note   string
}

// builtinGoHelpers maps a builtin's name to its GoHelper.  Every LEnv
// method whose doc comment says it "returns what the X builtin returns" is
// listed (TestBuiltinGoHelpersListEveryHelper), and each method is checked
// against its builtin on inputs of every type
// (TestBuiltinGoHelpersMatchBuiltins).  elpsvet's elpsidiom analyzer reads
// it to report a lisp.BuiltinFunc that has a helper.
var builtinGoHelpers = map[string]GoHelper{
	"to-string":     {Method: "ToString"},
	"to-bytes":      {Method: "ToBytes"},
	"to-int":        {Method: "ToInt"},
	"car":           {Method: "Car"},
	"cdr":           {Method: "Cdr"},
	"rest":          {Method: "Rest"},
	"first":         {Method: "First"},
	"second":        {Method: "Second"},
	"nth":           {Method: "Nth"},
	"map":           {Method: "MapSeq"},
	"assoc":         {Method: "MapAssoc"},
	"assoc!":        {Method: "MapPut"},
	"dissoc":        {Method: "MapDissoc"},
	"dissoc!":       {Method: "MapDelete"},
	"get":           {Method: "MapLookup"},
	"keys":          {Method: "MapKeys"},
	"key?":          {Method: "MapHasKey"},
	"sorted-map":    {Method: "SortedMapOf"},
	"concat":        {Method: "Concat"},
	"zip":           {Method: "Zip"},
	"slice":         {Method: "Slice"},
	"append":        {Method: "Append"},
	"append!":       {Method: "AppendInPlace"},
	"make-sequence": {Method: "MakeSequence"},
	"length":        {Method: "Length"},
	"empty?":        {Method: "IsEmpty"},
	"string=":       {Method: "StringEqual"},
	"string<":       {Method: "StringLess"},
	"string>":       {Method: "StringGreater"},
	"=":             {Method: "NumEqual"},
	"<":             {Method: "NumLess"},
	"<=":            {Method: "NumLessEqual"},
	">":             {Method: "NumGreater"},
	">=":            {Method: "NumGreaterEqual"},
	"+":             {Method: "Add"},
	"-":             {Method: "Sub"},
	"*":             {Method: "Mul"},
	"mod":           {Method: "Mod"},
	"format-string": {Method: "FormatString"},
	"copy":          {Method: "DeepCopy"},
	"funcall":       {Method: "CallFunction"},
	"apply":         {Method: "ApplyFunction"},
	"vector":        {Note: "lisp.Cells{...}.Vector()"},
	"list":          {Note: "lisp.Cells{...}.List()"},
	// The type predicates.  TestTypePredicateNotes checks that every type
	// predicate is listed, and checks the Go each Note names (a copy in the
	// test, held to the Note's text) against its builtin.
	"nil?":          {Note: "v.IsNil()"},
	"true?":         {Note: "lisp.True(v)"},
	"list?":         {Note: "v.Type == lisp.LSExpr"},
	"sorted-map?":   {Note: "v.Type == lisp.LSortMap"},
	"array?":        {Note: "v.Type == lisp.LArray"},
	"vector?":       {Note: "v.IsVector()"},
	"bool?":         {Note: "v.IsSymbol(lisp.TrueSymbol) || v.IsSymbol(lisp.FalseSymbol)"},
	"number?":       {Note: "v.IsNumeric()"},
	"int?":          {Note: "v.Type == lisp.LInt"},
	"float?":        {Note: "v.Type == lisp.LFloat"},
	"symbol?":       {Note: "v.Type == lisp.LSymbol"},
	"string?":       {Note: "v.Type == lisp.LString"},
	"bytes?":        {Note: "v.Type == lisp.LBytes"},
	"tagged-value?": {Note: "v.Type == lisp.LTaggedVal"},
}

// BuiltinGoHelper returns the GoHelper for the language builtin name, and
// false when there is none.
func BuiltinGoHelper(name string) (GoHelper, bool) {
	h, ok := builtinGoHelpers[name]
	return h, ok
}

// Mul returns what the * builtin returns for (* xs...): the product of xs,
// 1 for none, or the same error from the same check.  It checks the
// evaluation's context first, as CallBuiltin does, and charges no step.
func (env *LEnv) Mul(xs ...*LVal) *LVal {
	if lerr := env.CheckContext(); lerr.IsError() {
		return lerr
	}
	return builtinMul(env, QExpr(xs))
}

// Mod returns what the mod builtin returns for (mod a b): the remainder of
// the ints a and b, or the same error from the same check.  It checks the
// evaluation's context first, as CallBuiltin does, and charges no step.
func (env *LEnv) Mod(a, b *LVal) *LVal {
	if lerr := env.CheckContext(); lerr.IsError() {
		return lerr
	}
	return builtinMod(env, Cells{a, b}.List())
}

// Append returns what the append builtin returns for (append typespec seq
// vals...), where typespec is the type specifier kind stands for: a new
// sequence of that type that holds seq's elements and then vals, or the same
// error from the same check, the allocation check included.  It does not
// change seq; AppendInPlace does, as append! does.  It checks the
// evaluation's context first, as CallBuiltin does, and charges no step.
func (env *LEnv) Append(kind SeqKind, seq *LVal, vals ...*LVal) *LVal {
	if lerr := env.CheckContext(); lerr.IsError() {
		return lerr
	}
	return appendValues(env, goSeqSpec(kind), seq, vals)
}

// MakeSequence returns what the make-sequence builtin returns for
// (make-sequence start stop step...): the list of numbers from start up to
// stop, or the same error from the same check, the allocation check
// included.  It checks the evaluation's context first, as CallBuiltin does,
// and charges no step.
func (env *LEnv) MakeSequence(start, stop *LVal, step ...*LVal) *LVal {
	if lerr := env.CheckContext(); lerr.IsError() {
		return lerr
	}
	args := make([]*LVal, 0, 2+len(step))
	args = append(args, start, stop)
	return builtinMakeSequence(env, QExpr(append(args, step...)))
}

// StringLess returns what the string< builtin returns for (string< a b):
// whether the string a sorts before b, or the same error from the same check.
// The error is the builtin's own *ErrorVal, and nil when there is none.  It
// checks the evaluation's context first, as CallBuiltin does, and charges no
// step.
func (env *LEnv) StringLess(a, b *LVal) (bool, error) {
	if lerr := env.CheckContext(); lerr.IsError() {
		return false, (*ErrorVal)(lerr)
	}
	return goError(boolResult(builtinStringLT(env, Cells{a, b}.List())))
}

// StringGreater returns what the string> builtin returns for (string> a b),
// as StringLess does for string<.
func (env *LEnv) StringGreater(a, b *LVal) (bool, error) {
	if lerr := env.CheckContext(); lerr.IsError() {
		return false, (*ErrorVal)(lerr)
	}
	return goError(boolResult(builtinStringGT(env, Cells{a, b}.List())))
}

// boolResult reads a predicate builtin's result as a Go bool, or its error.
func boolResult(v *LVal) (bool, *LVal) {
	if v.IsError() {
		return false, v
	}
	return True(v), nil
}

// goError returns the (value, error) pair of a Go form from the (value,
// *LVal) pair of a builtin body: v and a nil error when lerr is nil, else
// the zero value and lerr as a *ErrorVal.  The nil error is untyped, so a
// caller's err != nil test is false on success.
func goError[T any](v T, lerr *LVal) (T, error) {
	if lerr != nil {
		var zero T
		return zero, (*ErrorVal)(lerr)
	}
	return v, nil
}
