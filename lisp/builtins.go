// Copyright © 2018 The ELPS authors

package lisp

import (
	"bytes"
	"fmt"
	"math"
	"sort"
	"strconv"
	"strings"
)

// LBuiltin is a function that performs executes a lisp function.
type LBuiltin func(env *LEnv, args *LVal) *LVal

// LBuiltinDef is a built-in function
type LBuiltinDef interface {
	Name() string
	Formals() *LVal
	Eval(env *LEnv, args *LVal) *LVal
}

type builtinDocumented interface {
	Docstring() string
}

func builtinDocstring(defn LBuiltinDef) string {
	if doc, ok := defn.(builtinDocumented); ok {
		return doc.Docstring()
	}
	return ""
}

type langBuiltin struct {
	name    string
	formals *LVal
	fun     LBuiltin
	docs    string
}

func (fun *langBuiltin) Name() string {
	return fun.name
}

func (fun *langBuiltin) Formals() *LVal {
	return fun.formals
}

func (fun *langBuiltin) Eval(env *LEnv, args *LVal) *LVal {
	return fun.fun(env, args)
}

func (fun *langBuiltin) Docstring() string {
	return fun.docs
}

var (
	//elpsvet:allow user-registered builtin table; formals are sealed at registration (RegisterDefaultBuiltin) and shared via registrationFormals (env.go AddBuiltins)
	userBuiltins []*langBuiltin
	//elpsvet:allow default builtin table; formals are sealed at package init (sealDefaultFormals) and shared via registrationFormals (env.go AddBuiltins)
	langBuiltins = []*langBuiltin{
		{"load-string", Formals("source-code", KeyArgSymbol, "name"), builtinLoadString,
			`Parses and evaluates source-code (a string) as ELPS source. The
			optional :name keyword provides a label for error messages.
			Returns the result of the last evaluated expression.`},
		{"load-bytes", Formals("source-code", KeyArgSymbol, "name"), builtinLoadBytes,
			`Parses and evaluates source-code (a bytes value) as ELPS source.
			The optional :name keyword provides a label for error messages.
			Returns the result of the last evaluated expression.`},
		{"load-file", Formals("source-location"), builtinLoadFile,
			`Loads and evaluates the ELPS source file at source-location.
			Returns the result of the last evaluated expression.`},
		{"in-package", Formals("package-name", VarArgSymbol, "docstring"), builtinInPackage,
			`Switches the current package to package-name, creating it if it
			does not exist. Optional trailing strings set the package
			documentation (concatenated with spaces if multiple are given).
			Returns nil.`},
		{"use-package", Formals(VarArgSymbol, "package-name"), builtinUsePackage,
			`Imports all exported symbols from the named packages into the
			current package. Returns nil.`},
		{"export", Formals(VarArgSymbol, "symbol"), builtinExport,
			`Marks symbols as exported from the current package, making them
			available to other packages via use-package. Returns nil.`},
		{"set", Formals("sym", "val", VarArgSymbol, "docstring"), builtinSet,
			`Binds val to the quoted symbol sym in the current package scope,
			creating or overwriting the binding. Optional trailing strings set
			the symbol's documentation (concatenated; empty strings produce
			paragraph breaks). Returns the bound value. This is the primary
			way to create top-level bindings. Use set! to mutate an existing
			binding without risk of accidental creation.`},
		{"gensym", Formals(), builtinGensym,
			`Returns a new unique, uninterned symbol. Useful in macros to
			avoid variable name collisions.`},
		{"identity", Formals("value"), builtinIdentity,
			`Returns its argument unchanged.`},
		{"macroexpand", Formals("quoted-form"), builtinMacroExpand,
			`Repeatedly expands the macro call in quoted-form until the head
			is no longer a macro. Returns the fully expanded form.`},
		{"macroexpand-1", Formals("quoted-form"), builtinMacroExpand1,
			`Performs a single macro expansion step on quoted-form and returns
			the result. Useful for debugging macro expansion.`},
		{"funcall", Formals("fun", VarArgSymbol, "args"), builtinFunCall,
			`Calls fun with the given args and returns the result. Cannot be
			used with special operators or macros.`},
		{"apply", Formals("fun", VarArgSymbol, "args"), builtinApply,
			`Calls fun with arguments formed by appending the last argument
			(which must be a list) to any preceding arguments. For example,
			(apply f 1 2 '(3 4)) calls f with arguments 1 2 3 4.`},
		{"to-string", Formals("value"), builtinToString,
			`Converts value to its string representation. Accepts strings,
			symbols, bytes, integers, and floats.`},
		{"to-bytes", Formals("value"), builtinToBytes,
			`Converts value to a bytes value. Accepts strings (encoded as
			UTF-8) and bytes (returned as-is).`},
		{"to-int", Formals("value"), builtinToInt,
			`Converts value to an integer. Accepts strings (parsed as
			decimal), integers (returned as-is), and floats (truncated).`},
		{"to-float", Formals("value"), builtinToFloat,
			`Converts value to a float. Accepts strings (parsed), floats
			(returned as-is), and integers (widened).`},
		{"eval", Formals("expr"), builtinEval,
			`Evaluates expr in the current environment and returns the result.
			Quoted values are unquoted one level before evaluation.`},
		{"error", Formals("condition", VarArgSymbol, "args"), builtinError,
			`Signals an error with the given condition name (a symbol or
			string) and optional data arguments. The condition can be caught
			by handler-bind.`},
		{"rethrow", Formals(), builtinRethrow,
			`Re-throws the current error being handled by handler-bind,
			preserving the original stack trace. Can only be called from
			within a handler-bind handler. Use this instead of (apply error
			condition args) when you want to perform side effects (such as
			logging) but still propagate the original error unchanged.`},
		{"car", Formals("lis"), builtinCAR,
			`Returns the first element of a list, or nil if empty.`},
		{"cdr", Formals("lis"), builtinCDR,
			`Returns a list of all elements except the first, or nil if the
			list has fewer than two elements. Like slice, the result is a
			view sharing elements with lis.`},
		{"rest", Formals("lis"), builtinRest,
			`Returns a list of all elements except the first. Accepts lists
			and vectors. Returns nil if fewer than two elements. Like slice,
			the result is a view sharing elements with lis.`},
		{"first", Formals("seq"), builtinFirst,
			`Returns the first element of a sequence (list or vector), or nil
			if empty.`},
		{"second", Formals("seq"), builtinSecond,
			`Returns the second element of a sequence (list or vector), or nil
			if fewer than two elements.`},
		{"nth", Formals("seq", "n"), builtinNth,
			`Returns the element at zero-based index n in a sequence, or nil
			if n is out of bounds.`},
		{
			"map", Formals("type-specifier", "fn", "seq"), builtinMap,
			`Returns a sequence containing values returned by applying unary
			function fn to elements of seq in order. The type-specifier
			('list or 'vector) determines the return type.`,
		},
		{"foldl", Formals("fn", "z", "seq"), builtinFoldLeft,
			`Left-folds seq with binary function fn starting from accumulator
			z. Calls (fn acc elem) for each element left to right, threading
			the result. Returns z if seq is empty.`},
		{"foldr", Formals("fn", "z", "seq"), builtinFoldRight,
			`Right-folds seq with binary function fn starting from accumulator
			z. Calls (fn elem acc) for each element right to left, threading
			the result. Returns z if seq is empty.`},
		{"compose", Formals("f", "g"), builtinCompose,
			`Returns a new function that applies g to its arguments, then
			applies f to the result. (compose f g) is equivalent to
			(lambda (...) (f (g ...))).`},
		{"unpack", Formals("f", "lis"), builtinUnpack,
			`Calls f with the elements of lis as individual arguments.
			Equivalent to (apply f lis).`},
		{"flip", Formals("binary-function"), builtinFlip,
			`Returns a new function that calls binary-function with its two
			arguments swapped.`},
		{"assoc", Formals("map", "key", "value"), builtinAssoc,
			`Returns a new sorted-map with key set to value, without
			modifying the original. If map is nil, creates a new map.`},
		{"assoc!", Formals("map", "key", "value"), builtinAssocMutate,
			`Sets key to value in map, mutating it in place, and returns the
			modified map.`},
		{"dissoc", Formals("map", "key"), builtinDissoc,
			`Returns a new sorted-map with key removed, without modifying the
			original.`},
		{"dissoc!", Formals("map", "key"), builtinDissocMutate,
			`Removes key from map, mutating it in place, and returns the
			modified map.`},
		{"get", Formals("map", "key"), builtinGet,
			`Returns the value associated with key in a sorted-map, or nil if
			the key is not present or map is nil.`},
		{"keys", Formals("map"), builtinKeys,
			`Returns a list of all keys in a sorted-map in sorted order.`},
		{"key?", Formals("map", "key"), builtinIsKey,
			`Returns true if key exists in the sorted-map, false otherwise.`},
		{"sorted-map", Formals(VarArgSymbol, "args"), builtinSortedMap,
			`Creates a new sorted-map from alternating key-value pairs. For
			example, (sorted-map :a 1 :b 2). Keys are maintained in sorted
			order.`},
		{"concat", Formals("type-specifier", VarArgSymbol, "args"), builtinConcat,
			`Concatenates sequences into one of the specified type. Accepts
			'list, 'vector, 'string, or 'bytes as type-specifier.`},
		{"copy", Formals("value"), builtinCopy,
			`Returns a deep copy of value that shares no storage with it:
			lists, vectors, sorted-maps and bytes are rebuilt with fresh
			backing, recursively, so mutating the copy at any depth cannot
			be observed through the original. Function and native values
			are shared by reference, not copied: a lambda carried into the
			copy still reads and writes the bindings it captured, so
			calling one can be observed through the original. Strings and
			numbers are immutable values. Sharing between values inside the
			input is preserved in the copy, including cycles; sharing of a
			backing array between distinct values -- what cdr, rest and
			slice produce -- is not. Use copy to take ownership of data
			whose provenance you do not control:
			the result is always mutable, even when the input is (or came
			from) a quoted program literal.`},
		{"insert-index", Formals("type-specifier", "seq", "index", "item"), builtinInsertIndex,
			`Returns a new sequence with item inserted at the given index.
			The type-specifier ('list or 'vector) determines the return type.`},
		{"stable-sort", Formals("less-predicate", "list", VarArgSymbol, "key-fun"), builtinSortStable,
			`Sorts list using the binary less-predicate and returns the
			sorted list. The sort is stable. An optional key-fun extracts
			comparison keys from elements. A mutable list is sorted in
			place, so sorting a slice view also sorts that region of its
			source; sort (concat 'list x) when x is a view you do not own.
			A quoted program literal is never modified: sorting a non-empty
			literal (or a view sharing its storage) raises the catchable
			modify-literal-error condition -- sort (copy lit) instead.`},
		{"insert-sorted", Formals("type-specifier", "list", "predicate", "item", VarArgSymbol, "key-fun"), builtinInsertSorted,
			`Returns a new sequence with item inserted at its sorted position
			according to predicate. An optional key-fun extracts comparison
			keys from elements.`},
		{"search-sorted", Formals("n", "predicate"), builtinSearchSorted,
			`Returns the smallest index i in [0, n) for which predicate
			returns true, using binary search. Equivalent to Go's
			sort.Search.`},
		{"select", Formals("type-specifier", "predicate", "seq"), builtinSelect,
			`Returns a new sequence containing only elements for which
			predicate returns truthy. The type-specifier ('list or 'vector)
			determines the return type.`},
		{"reject", Formals("type-specifier", "predicate", "seq"), builtinReject,
			`Returns a new sequence containing only elements for which
			predicate returns falsey. The type-specifier ('list or 'vector)
			determines the return type.`},
		{"zip", Formals("type-specifier", "list", VarArgSymbol, "lists"), builtinZip,
			`Returns a sequence of sequences, where the i-th inner sequence
			contains the i-th element from each input list. Truncated to the
			shortest input.`},
		{"make-sequence", Formals("start", "stop", VarArgSymbol, "step"), builtinMakeSequence,
			`Returns a list of numbers from start (inclusive) to stop
			(exclusive), incrementing by step (default 1).`},
		{"format-string", Formals("format", VarArgSymbol, "values"), builtinFormatString,
			`Returns a string by replacing placeholders in the format string
			with corresponding values. Use {} for sequential substitution or
			{0}, {1}, etc. for positional. Strings are interpolated without
			quotes. Use {{ and }} for literal braces. Cannot mix sequential
			and positional styles.`},
		{"reverse", Formals("type-specifier", "seq"), builtinReverse,
			`Returns a new sequence with elements in reverse order. The
			type-specifier ('list or 'vector) determines the return type.`},
		{"slice", Formals("type-specifier", "seq", "start", "end"), builtinSlice,
			`Returns a subsequence from index start (inclusive) to end
			(exclusive). Accepts lists, vectors, strings, and bytes. For
			lists, vectors and bytes the result is a view sharing elements
			with seq: appending to it cannot disturb seq, but sorting it in
			place also sorts that region of seq. Use concat to take a copy.
			String slices are always independent. A vector is mutable, so
			(slice 'vector ...) over a non-empty quoted program literal
			raises the catchable modify-literal-error condition -- slice
			(copy lit) instead.`},
		{"list", Formals(VarArgSymbol, "args"), builtinList,
			`Returns a new list containing all the given arguments.`},
		{"vector", Formals(VarArgSymbol, "args"), builtinVector,
			`Returns a new one-dimensional array (vector) containing all the
			given arguments.`},
		{"append!", Formals("vec", VarArgSymbol, "values"), builtinAppendMutate,
			`Appends values to vec, mutating it in place. Returns the
			modified vector.`},
		{"append", Formals("type-specifier", "vec", VarArgSymbol, "values"), builtinAppend,
			`Returns a new sequence with values appended to vec. The
			type-specifier ('list, 'vector, or 'bytes) determines the return
			type. Does not mutate vec and never shares storage with it, so
			appending to the same source twice yields independent results.
			This costs a copy, making append O(n); use append! to accumulate
			in a loop. (append 'vector ...) over a non-empty quoted program
			literal raises the catchable modify-literal-error condition --
			append to (copy lit) instead.`},
		{"append-bytes!", Formals("bytes", "values"), builtinAppendBytesMutate,
			`Appends values to bytes, mutating it in place. Values can be a
			string, bytes, or list of integers. Returns the modified bytes.`},
		// NOTE:  ``append-bytes'' does not accept a type-specifier argument
		// because 'string would be equivalent to (concat 'string ...)
		{"append-bytes", Formals("bytes", "byte-sequence"), builtinAppendBytes,
			`Returns new bytes with byte-sequence appended. The byte-sequence
			can be a string, bytes, or list of integers. Does not mutate
			bytes and never shares storage with it; use append-bytes! to
			accumulate in a loop.`},
		{"aref", Formals("a", VarArgSymbol, "indices"), builtinARef,
			`Returns the element at the given indices in an array. Multiple
			indices access multi-dimensional arrays.`},
		{"length", Formals("seq"), builtinLength,
			`Returns the number of elements in a sequence. Accepts lists,
			vectors, sorted-maps, strings, and bytes.`},
		{"empty?", Formals("seq"), builtinIsEmpty,
			`Returns true if the sequence has zero elements, false otherwise.`},
		{"cons", Formals("head", "tail"), builtinCons,
			`Returns a new list with head prepended to the list tail.`},
		{"not", Formals("expr"), builtinNot,
			`Returns true if expr is falsey (nil or false), false otherwise.`},
		// true? is potentially needed as a boolean conversion function (for json)
		{"true?", Formals("expr"), builtinIsTrue,
			`Returns true if expr is truthy (not nil and not false), false
			otherwise. Useful as an explicit boolean conversion.`},
		{"type", Formals("value"), builtinType,
			`Returns a symbol naming the type of value (e.g. 'int, 'float,
			'string, 'list, 'sorted-map, 'array, 'bytes, 'fun).`},
		{"type?", Formals("type-specifier", "value"), builtinIsType,
			`Returns true if value matches the type-specifier symbol. Also
			accepts a tagged typedef for user-defined types.`},
		{"tagged-value?", Formals("value"), builtinIsTaggedVal,
			`Returns true if value is a tagged value (user-defined type
			instance), false otherwise.`},
		{"user-data", Formals("value"), builtinUserData,
			`Returns the user data associated with a tagged value.`},
		{"new", Formals("type-specifier", VarArgSymbol, "arguments"), builtinNew,
			`Creates a new instance of the type named by type-specifier,
			passing additional arguments to the type's constructor.`},
		{"nil?", Formals("expr"), builtinIsNil,
			`Returns true if expr is nil (the empty list), false otherwise.`},
		{"list?", Formals("expr"), builtinIsList,
			`Returns true if expr is a list (including nil), false otherwise.`},
		{"sorted-map?", Formals("expr"), builtinIsSortedMap,
			`Returns true if expr is a sorted-map, false otherwise.`},
		{"array?", Formals("expr"), builtinIsArray,
			`Returns true if expr is an array, false otherwise.`},
		{"vector?", Formals("expr"), builtinIsVector,
			`Returns true if expr is a one-dimensional array (vector), false
			otherwise.`},
		{"bool?", Formals("expr"), builtinIsBool,
			`Returns true if expr is a boolean (true or false), false
			otherwise.`},
		{"number?", Formals("expr"), builtinIsNumber,
			`Returns true if expr is numeric (int or float), false otherwise.`},
		{"int?", Formals("expr"), builtinIsInt,
			`Returns true if expr is an integer, false otherwise.`},
		{"float?", Formals("expr"), builtinIsFloat,
			`Returns true if expr is a float, false otherwise.`},
		{"symbol?", Formals("expr"), builtinIsSymbol,
			`Returns true if expr is a symbol, false otherwise.`},
		{"string?", Formals("expr"), builtinIsString,
			`Returns true if expr is a string, false otherwise.`},
		{"bytes?", Formals("expr"), builtinIsBytes,
			`Returns true if expr is a bytes value, false otherwise.`},
		{"equal?", Formals("a", "b"), builtinEqual,
			`Returns true if a and b are structurally equal, performing deep
			comparison across all value types. Sorted-map keys compare by
			name, matching how get and key? identify them, so a map keyed
			by 'a is equal? to a map keyed by "a".`},
		{"all?", Formals("predicate", "seq"), builtinAllP,
			`Returns true if predicate returns truthy for every element in
			seq. Returns true for an empty sequence. Short-circuits on the
			first falsey result.`},
		{"any?", Formals("predicate", "seq"), builtinAnyP,
			`Returns the first truthy result of applying predicate to
			elements of seq, or false if none match. Short-circuits on the
			first truthy result.`},
		{"max", Formals("real", VarArgSymbol, "rest"), builtinMax,
			`Returns the largest of the given numeric arguments.`},
		{"min", Formals("real", VarArgSymbol, "rest"), builtinMin,
			`Returns the smallest of the given numeric arguments.`},
		{"string>=", Formals("a", "b"), builtinStringGEq,
			`Returns true if string a is lexicographically >= string b.`},
		{"string>", Formals("a", "b"), builtinStringGT,
			`Returns true if string a is lexicographically > string b.`},
		{"string<=", Formals("a", "b"), builtinStringLEq,
			`Returns true if string a is lexicographically <= string b.`},
		{"string<", Formals("a", "b"), builtinStringLT,
			`Returns true if string a is lexicographically < string b.`},
		{"string=", Formals("a", "b"), builtinStringEq,
			`Returns true if strings a and b are equal.`},
		{"symbol=", Formals("a", "b"), builtinSymbolEq,
			`Returns true if symbols a and b are equal.`},
		{">=", Formals("a", "b"), builtinGEq,
			`Returns true if a >= b. Accepts ints and floats.`},
		{">", Formals("a", "b"), builtinGT,
			`Returns true if a > b. Accepts ints and floats.`},
		{"<=", Formals("a", "b"), builtinLEq,
			`Returns true if a <= b. Accepts ints and floats.`},
		{"<", Formals("a", "b"), builtinLT,
			`Returns true if a < b. Accepts ints and floats.`},
		{"=", Formals("a", "b"), builtinEqNum,
			`Returns true if numeric values a and b are equal.`},
		{"pow", Formals("a", "b"), builtinPow,
			`Returns a raised to the power b. Returns int when both args are
			ints and b >= 0; otherwise returns float.`},
		{"mod", Formals("a", "b"), builtinMod,
			`Returns the remainder of integer division a mod b. Both
			arguments must be ints and b must be non-zero.`},
		{"+", Formals(VarArgSymbol, "x"), builtinAdd,
			`Returns the sum of all arguments, or 0 with no arguments.
			Returns int if all args are ints; otherwise float.`},
		{"-", Formals(VarArgSymbol, "x"), builtinSub,
			`With one argument, returns its negation. With two or more,
			subtracts subsequent arguments from the first. Returns 0 with
			no arguments.`},
		{"/", Formals(VarArgSymbol, "x"), builtinDiv,
			`With one argument, returns 1/x. With two or more, divides
			the first by all subsequent. Returns int when all divisions are
			exact; otherwise float.`},
		{"*", Formals(VarArgSymbol, "x"), builtinMul,
			`Returns the product of all arguments, or 1 with no arguments.
			Returns int if all args are ints; otherwise float.`},
		{"debug-print", Formals(VarArgSymbol, "args"), builtinDebugPrint,
			`Prints all arguments to stderr followed by a newline. Returns
			nil.`},
		{"debug-stack", Formals(), builtinDebugStack,
			`Prints the current call stack to stderr for debugging. Returns
			nil.`},
	}
)

// sealDefaultFormals seals the formals of every definition in the package's
// shared builtin, macro and special-op tables.  The tables are constructed
// once at Go program initialization and consulted by every Runtime in the
// process; sealing their formals here — package init is single-threaded and
// runs before any environment can exist — makes that sharing explicit and
// safe.  Registration (registrationFormals in env.go) aliases a sealed list
// instead of deep-copying it per environment, and the seal's write guards
// (lisp/seal.go) catch any code path that would mutate the shared
// cells, exactly as they do for the sealed parser output that lisp-defined
// functions alias as their formals.
func init() { sealDefaultFormals() }

func sealDefaultFormals() {
	for _, table := range [][]*langBuiltin{langBuiltins, langMacros, langSpecialOps} {
		for _, def := range table {
			def.formals.SealAST()
		}
	}
}

// RegisterDefaultBuiltin adds the given function to the list returned by
// DefaultBuiltins.
func RegisterDefaultBuiltin(name string, formals *LVal, fn LBuiltin) {
	userBuiltins = append(userBuiltins, &langBuiltin{name, sealedFormalsCopy(formals), fn, ""})
}

// sealedFormalsCopy deep-copies a caller-supplied formals list for entry
// into one of the RegisterDefault* tables and seals the copy.  The copy
// keeps the caller's own value mutable and unaliased (the caller may reuse
// or modify it after registration); the seal marks the table's private copy
// as shared-immutable so registrationFormals (env.go) can alias it into
// every environment without a further per-env copy.
func sealedFormalsCopy(formals *LVal) *LVal {
	c := formals.Copy()
	c.SealAST()
	return c
}

// DefaultBuiltins returns the default set of LBuiltinDefs added to LEnv
// objects when LEnv.AddBuiltins is called without arguments.
func DefaultBuiltins() []LBuiltinDef {
	ops := make([]LBuiltinDef, len(langBuiltins)+len(userBuiltins))
	for i := range langBuiltins {
		ops[i] = langBuiltins[i]
	}
	offset := len(langBuiltins)
	for i := range userBuiltins {
		ops[offset+i] = userBuiltins[i]
	}
	return ops
}

func builtinLoadString(env *LEnv, args *LVal) *LVal {
	source, name := args.ReqArg(env, 0), args.KeyArg(1)
	if source.Type == LError {
		return source
	}
	if source.Type != LString {
		return env.Errorf("first argument is not a string: %v", source.Type)
	}
	if !name.IsNil() && name.Type != LString {
		return env.Errorf("name is not a string: %v", name.Type)
	}
	_name := "load-string"
	if name.Str != "" {
		_name = name.Str
	}

	// Load the source in the root environment so the loaded code does not
	// share the current lexical environment.  The loaded source will share a
	// stack but the stack frame TROBlock will prevent tail recursion
	// optimization from unwinding the stack to/beyond this point.
	env.Runtime.Stack.Top().TROBlock = true
	v := env.root().LoadString(_name, source.Str)
	if v.Type == LError && v.CallStack() == nil {
		v.SetCallStack(env.Runtime.Stack.Copy())
	}
	return v
}

func builtinLoadBytes(env *LEnv, args *LVal) *LVal {
	source, name := args.ReqArg(env, 0), args.KeyArg(1)
	if source.Type == LError {
		return source
	}
	if source.Type != LBytes {
		return env.Errorf("first argument is not bytes: %v", source.Type)
	}
	if !name.IsNil() && name.Type != LString {
		return env.Errorf("name is not a string: %v", name.Type)
	}
	_name := "load-bytes"
	if name.Str != "" {
		_name = name.Str
	}

	// Load the source in the root environment so the loaded code does not
	// share the current lexical environment.  The loaded source will share a
	// stack but the stack frame TROBlock will prevent tail recursion
	// optimization from unwinding the stack to/beyond this point.
	env.Runtime.Stack.Top().TROBlock = true
	v := env.root().Load(_name, bytes.NewReader(source.Bytes()))
	if v.Type == LError && v.CallStack() == nil {
		v.SetCallStack(env.Runtime.Stack.Copy())
	}
	return v
}

func builtinLoadFile(env *LEnv, args *LVal) *LVal {
	loc := args.Cells[0]
	if loc.Type != LString {
		return env.Errorf("first argument is not a string: %v", loc.Type)
	}

	// Load the source in the root environment so the loaded code does not
	// share the current lexical environment.  The loaded source will share a
	// stack but the stack frame TROBlock will prevent tail recursion
	// optimization from unwinding the stack to/beyond this point.
	env.Runtime.Stack.Top().TROBlock = true
	v := env.root().LoadFile(loc.Str)
	if v.Type == LError && v.CallStack() == nil {
		v.SetCallStack(env.Runtime.Stack.Copy())
	}
	return v
}

func builtinInPackage(env *LEnv, args *LVal) *LVal {
	if args.Cells[0].Type != LSymbol && args.Cells[0].Type != LString {
		return env.Errorf("first argument is not a symbol or a string: %v", args.Cells[0].Type)
	}
	name := args.Cells[0].Str
	pkg := env.Runtime.Registry.packages[name]
	newpkg := false
	if pkg == nil {
		newpkg = true
		env.Runtime.Registry.DefinePackage(name)
		pkg = env.Runtime.Registry.packages[name]
	}
	env.Runtime.Package = pkg
	if newpkg && env.Runtime.Registry.Lang != "" {
		// For now, all packages use the lisp package.  The ``in-package''
		// builtin doesn't provide syntax to simply use lisp (calling
		// ``lisp:use-package'' from a package that doesn't use lisp hard to
		// remember).
		env.UsePackage(Symbol(env.Runtime.Registry.Lang))
	}
	// Optional trailing doc strings
	if len(args.Cells) > 1 {
		var parts []string
		for _, arg := range args.Cells[1:] {
			if arg.Type != LString {
				return env.Errorf("docstring argument is not a string: %v", arg.Type)
			}
			parts = append(parts, arg.Str)
		}
		pkg.Doc = JoinDocStrings(parts)
	}
	return Nil()
}

func builtinUsePackage(env *LEnv, args *LVal) *LVal {
	for i, pkg := range args.Cells {
		if pkg.Type != LSymbol && pkg.Type != LString {
			return env.Errorf("argument %d is not a symbol or a string: %v", i+1, pkg.Type)
		}
		lerr := env.UsePackage(pkg)
		if lerr.Type == LError {
			return lerr
		}
	}
	return Nil()
}

func builtinExport(env *LEnv, args *LVal) *LVal {
	for _, arg := range args.Cells {
		switch arg.Type {
		case LSymbol, LString:
			env.Runtime.Package.Exports(arg.Str)
		case LSExpr:
			builtinExport(env, arg)
		default:
			return env.Errorf("argument is not a symbol, a string, or a list of valid types: %v", arg.Type)
		}
	}
	return Nil()
}

func builtinSet(env *LEnv, v *LVal) *LVal {
	if v.Cells[0].Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %v", v.Cells[0].Type)
	}

	lerr := env.PutGlobal(v.Cells[0], v.Cells[1])
	if lerr.Type == LError {
		return lerr
	}
	// Optional trailing doc strings
	if len(v.Cells) > 2 {
		var parts []string
		for _, arg := range v.Cells[2:] {
			if arg.Type != LString {
				return env.Errorf("docstring argument is not a string: %v", arg.Type)
			}
			parts = append(parts, arg.Str)
		}
		env.Runtime.Package.symbolDocs[v.Cells[0].Str] = JoinDocStrings(parts)
	}
	return env.GetGlobal(v.Cells[0])
}

func builtinGensym(env *LEnv, args *LVal) *LVal {
	return env.GenSym()
}

func builtinIdentity(env *LEnv, args *LVal) *LVal {
	return args.Cells[0]
}

func builtinMacroExpand(env *LEnv, args *LVal) *LVal {
	form := args.Cells[0]
	if form.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", form.Type)
	}
	maxDepth := env.Runtime.MaxMacroExpansions()
	for depth := 0; ; depth++ {
		if depth > maxDepth {
			return env.Errorf("macro expansion depth exceeded (%d expansions)", depth)
		}
		if form.IsNil() {
			return form
		}
		macsym := form.Cells[0]
		if macsym.Type != LSymbol {
			return form
		}
		mac := env.Get(macsym)
		r, ok := macroExpand1(env, mac, macroArgList(form))
		if !ok {
			return form
		}
		if r.Type != LSExpr {
			return r
		}
		form = r
	}
}

func builtinMacroExpand1(env *LEnv, args *LVal) *LVal {
	form := args.Cells[0]
	if form.Type != LSExpr {
		return env.Errorf("first argument is not a list: %v", form.Type)
	}
	if form.IsNil() {
		return form
	}
	macsym := form.Cells[0]
	if macsym.Type != LSymbol {
		return form
	}
	mac := env.Get(macsym)
	r, ok := macroExpand1(env, mac, macroArgList(form))
	if !ok {
		return form
	}
	return r
}

// macroArgList builds the argument list for expanding the macro call `form`,
// over storage this call owns rather than over form's own backing array
// (elps#396).
//
// SExpr does not copy — it wraps the slice it is handed — so SExpr(form.Cells[1:])
// produces a header carrying the CALLER'S backing array. Macro arguments are
// not evaluated, so that array arrives at the macro's parameters untouched:
// LEnv.bindFormalNext binds a variadic parameter to QExpr(args.Rest()), and
// argParser.Rest returns p.args[p.i:], one more window onto the same storage.
// Any destructive builtin in the macro body (stable-sort, append!) then
// writes through into `form`. For (macroexpand form) called from lisp, `form`
// is the evaluated first argument — typically the caller's own quoted literal,
// a node of the source program — so expanding a form silently rewrote it.
//
// This is the discipline the evaluator already has. LEnv.evalSExprCells is the
// runtime's normal route to a macro, and on its IsSpecialFun branch it builds
// the argument list over a FRESH array (`make([]*LVal, 1, len(s.Cells))` then
// `append(newCells, cells...)`) holding the caller's element pointers. This
// reproduces that exactly, so (macroexpand form) now perturbs form neither more
// nor less than evaluating it does. Before the fix the introspective operation
// was strictly MORE destructive than the one that actually runs the macro.
//
// The copy is shallow for the same reason the runtime's is. The ELEMENTS must
// stay the caller's nodes: the expansion splices them into its result and
// callers read source positions off exactly those nodes, so deep-copying would
// both diverge from eval and change what the language means. What must not be
// shared is the ARRAY — the only thing an in-place mutator applied to the &rest
// list itself can reach.
//
// Sealing raises the stakes rather than changing the argument. `form` is
// typically a SEALED program literal, so the aliased array above was the
// literal's own backing, handed to a macro's &rest parameter as an UNSEALED
// header — the one path that laundered a sealed node into mutable-looking
// storage and so slipped past every sealed-write guard in lisp/seal.go. The
// fresh array here is what closes it (commit 8d18071 on the sealing line;
// TestMacroexpandRestDoesNotLaunderSealedBacking in lisp/cow_seal_test.go).
func macroArgList(form *LVal) *LVal {
	margs := make([]*LVal, len(form.Cells)-1)
	copy(margs, form.Cells[1:])
	return SExpr(margs)
}

func macroExpand1(env *LEnv, mac *LVal, args *LVal) (*LVal, bool) {
	if mac.Type != LFun {
		return nil, false
	}
	if !mac.IsMacro() {
		return nil, false
	}
	// NOTE:  Any return from this point on should return a true second value
	// because mac is a macro.

	mark := env.MacroCall(mac, args)
	if mark.Type == LError {
		return mark, true
	}
	if mark.Type != LMarkMacExpand {
		return env.Errorf("internal error: macro did not return expansion marker: %v", mark.Type), true
	}
	// MacroCall unquotes its result so that it can work properly in normal
	// evaluation cycle.  So we need to re-quote the value here.
	return Quote(mark.Cells[0]), true
}

func builtinFunCall(env *LEnv, args *LVal) *LVal {
	fun, fargs := args.Cells[0], args.Cells[1:]
	fun = env.GetFunGlobal(fun)
	if fun.Type == LError {
		return fun
	}
	if fun.IsSpecialFun() {
		return env.Errorf("not a regular function: %v", fun.FunType)
	}
	// Because funcall and apply do not actually call env.Eval they cannot use
	// the standard method of signaling a terminal expression to the LEnv.  We
	// need to set the flag explicitly before env.funCall is invoked
	env.Runtime.Stack.Top().Terminal = true
	return env.FunCall(fun, SExpr(fargs))
}

func builtinApply(env *LEnv, args *LVal) *LVal {
	fun, fargs := args.Cells[0], args.Cells[1:]
	if len(fargs) == 0 {
		return env.Errorf("last argument must be a list")
	}
	argtail := fargs[len(fargs)-1]
	fargs = fargs[:len(fargs)-1]

	fun = env.GetFunGlobal(fun)
	if fun.Type == LError {
		return fun
	}
	if argtail.Type != LSExpr {
		return env.Errorf("last argument is not a list: %v", argtail.Type)
	}
	if fun.Type != LFun {
		return env.Errorf("first argument is not a function: %v", fun.Type)
	}
	if fun.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", fun.FunType)
	}

	// Because funcall and apply do not actually call env.Eval they cannot use
	// the standard method of signaling a terminal expression to the LEnv.  We
	// need to set the flag explicitly before FunCall is invoked.
	env.Runtime.Stack.Top().Terminal = true

	argcells := make([]*LVal, 0, len(fargs)+argtail.Len())
	argcells = append(argcells, fargs...)
	argcells = append(argcells, argtail.Cells...)
	return env.FunCall(fun, SExpr(argcells))
}

func builtinToString(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	s, err := toString(val)
	if err != nil {
		return env.Error(err)
	}
	return String(s)
}

func builtinToBytes(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	if val.Type == LBytes {
		return val
	}
	if val.Type == LString {
		return Bytes([]byte(val.Str))
	}
	// TODO:  Allow sequences of integers to be turned into bytes?
	return env.Errorf("cannot convert type to bytes: %v", val.Type)
}

func toString(val *LVal) (string, error) {
	switch val.Type {
	case LString:
		return val.Str, nil
	case LSymbol:
		return val.Str, nil
	case LBytes:
		return string(val.Bytes()), nil
	case LInt:
		i := strconv.Itoa(val.Int)
		return i, nil
	case LFloat:
		f := strconv.FormatFloat(val.Float, 'g', -1, 64)
		return f, nil
	default:
		return "", fmt.Errorf("cannot convert type to string: %v", val.Type)
	}
}

func builtinToInt(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	switch val.Type {
	case LString:
		x, err := strconv.Atoi(val.Str)
		if err != nil {
			return env.Error(err)
		}
		return Int(x)
	case LInt:
		return val
	case LFloat:
		return Int(int(val.Float))
	default:
		return env.Errorf("cannot convert type to int: %v", val.Type)
	}
}

func builtinToFloat(env *LEnv, args *LVal) *LVal {
	val := args.Cells[0]
	switch val.Type {
	case LString:
		x, err := strconv.ParseFloat(val.Str, 64)
		if err != nil {
			return env.Error(err)
		}
		return Float(x)
	case LInt:
		return Float(float64(val.Int))
	case LFloat:
		return val
	default:
		return env.Errorf("cannot convert type to float: %v", val.Type)
	}
}

func builtinEval(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type == LQuote {
		return v.Cells[0]
	}
	return env.Eval(shallowUnquote(v))
}

func builtinError(env *LEnv, args *LVal) *LVal {
	condition, rest := args.Cells[0], args.Cells[1:]
	if condition.Type != LSymbol {
		return env.Errorf("condition type is not a symbol: %v", condition.Type)
	}

	iargs := make([]interface{}, len(rest))
	for i, arg := range rest {
		iargs[i] = arg
	}
	return env.ErrorCondition(condition.Str, iargs...)
}

func builtinRethrow(env *LEnv, args *LVal) *LVal {
	cond := env.Runtime.CurrentCondition()
	if cond == nil {
		return env.Errorf("rethrow: not inside a handler-bind handler")
	}
	return cond
}

func builtinCAR(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type != LSExpr {
		return env.Errorf("argument is not a list %v", v.Type)
	}
	if len(v.Cells) == 0 {
		return Nil()
	}

	return v.Cells[0]
}

func builtinCDR(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type != LSExpr {
		return env.Errorf("argument is not a list %v", v.Type)
	}
	if len(v.Cells) < 2 {
		return Nil()
	}
	// The reslice is three-index so the tail's capacity stops at its length
	// (issue #373).  A two-index `v.Cells[1:]` inherits whatever spare
	// capacity v carries, and a later append to the tail grows into it,
	// writing through this view into memory the caller never named.  The
	// tail still SHARES its elements with v -- that is what a view is -- but
	// it can no longer reach past its own end.
	//
	// NOTE:  The elements are still shared, so this is not a copy.  See the
	// note on builtinSlice.
	r := QExpr(clampCap(v.Cells[1:]))
	// The clamp stops an append reaching PAST the view; it does not stop a
	// write WITHIN it, which for a sealed v is still a write to shared
	// program storage.  So the constraint travels with the backing array the
	// view still shares (lisp/seal.go), and the guarded mutation sites
	// (stable-sort, slice 'vector) see it on the view exactly as they would
	// on v itself.
	r.sealed = v.sealed
	return r
}

// clampCap returns cells with its capacity cut down to its length, so that a
// subsequent append is forced to reallocate rather than write into whatever
// spare capacity the backing array carries.
//
// This is the core of the issue #373 fix.  Every sequence view that escapes
// into lisp is clamped where it is produced, and every non-mutating append
// clamps its input where it is read; together those make it impossible for an
// append to write through one value into another.  Clamping is free -- no
// allocation, no copy -- and is a no-op for the exact-capacity slices that
// make up the common case.  What it costs is that an append which previously
// grew into a source's spare capacity must now reallocate.  That is the point
// of the change, not an accident of it.
func clampCap(cells []*LVal) []*LVal {
	return cells[:len(cells):len(cells)]
}

// clampCapBytes is clampCap for byte slices.
func clampCapBytes(b []byte) []byte {
	return b[:len(b):len(b)]
}

func builtinRest(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if !isSeq(v) {
		return env.Errorf("argument is not a proper sequence %v", v.Type)
	}
	cells := seqCells(v)
	if len(cells) < 2 {
		return Nil()
	}
	// Three-index reslice -- see builtinCDR (issue #373).
	r := QExpr(clampCap(cells[1:]))
	// The sealed constraint travels with the shared backing -- see builtinCDR.
	r.sealed = v.sealed
	return r
}

func builtinFirst(env *LEnv, args *LVal) *LVal {
	list := args.Cells[0]
	if !isSeq(list) {
		return env.Errorf("argument is not a proper sequence: %v", list.Type)
	}
	cells := seqCells(list)
	if len(cells) == 0 {
		return Nil()
	}
	return cells[0]
}

func builtinSecond(env *LEnv, args *LVal) *LVal {
	list := args.Cells[0]
	if !isSeq(list) {
		return env.Errorf("argument is not a proper sequence: %v", list.Type)
	}
	cells := seqCells(list)
	if len(cells) < 2 {
		return Nil()
	}
	return cells[1]
}

func builtinNth(env *LEnv, args *LVal) *LVal {
	list, n := args.Cells[0], args.Cells[1]
	if !isSeq(list) {
		return env.Errorf("first argument is not a proper sequence: %v", list.Type)
	}
	if n.Type != LInt {
		return env.Errorf("second argument is not an integer: %v", n.Type)
	}
	if n.Int < 0 {
		return env.Errorf("index cannot be negative: %d", n.Int)
	}
	cells := seqCells(list)
	if len(cells) <= n.Int {
		return Nil()
	}
	return cells[n.Int]
}

func builtinMap(env *LEnv, args *LVal) *LVal {
	typespec, f, lis := args.Cells[0], args.Cells[1], args.Cells[2]
	nilReturn := false
	if typespec.IsNil() {
		nilReturn = true
	} else if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("second argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	if !nilReturn {
		if msg := env.Runtime.CheckAlloc(lis.Len()); msg != "" {
			return env.Errorf("%s", msg)
		}
	}
	var v *LVal
	var cells []*LVal
	if nilReturn {
		v = Nil()
	} else {
		switch typespec.Str {
		case "vector":
			v = Array(QExpr([]*LVal{Int(lis.Len())}), nil)
			cells = seqCells(v)
		case "list":
			cells = make([]*LVal, lis.Len())
			v = QExpr(cells)
		default:
			return env.Errorf("type specifier is invalid: %v", typespec)
		}
	}
	for i, c := range seqCells(lis) {
		fargs := QExpr([]*LVal{c})
		fret := env.FunCall(f, fargs)
		if fret.Type == LError {
			return fret
		}
		if !nilReturn {
			cells[i] = fret
		}
	}
	return v
}

func builtinFoldLeft(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	for _, c := range seqCells(lis) {
		fargs := QExpr([]*LVal{
			// args reversed from foldr function invocation
			acc,
			c,
		})
		fret := env.FunCall(f, fargs)
		if fret.Type == LError {
			return fret
		}
		acc = fret
	}
	return acc
}

func builtinFoldRight(env *LEnv, args *LVal) *LVal {
	f := args.Cells[0]
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	acc := args.Cells[1]
	lis := args.Cells[2]
	if !isSeq(lis) {
		return env.Errorf("third argument is not a proper sequence: %s", lis.Type)
	}
	cells := seqCells(lis)
	for i := len(cells) - 1; i >= 0; i-- {
		c := cells[i]
		fargs := QExpr([]*LVal{
			// args reversed from foldl function invocation
			c,
			acc,
		})
		fret := env.FunCall(f, fargs)
		if fret.Type == LError {
			return fret
		}
		acc = fret
	}
	return acc
}

// NOTE: Compose requires concat and unpack in order to work with varargs.
func builtinCompose(env *LEnv, args *LVal) *LVal {
	f, g := args.Cells[0], args.Cells[1]
	f = env.GetFunGlobal(f)
	if f.Type == LError {
		return f
	}
	if f.Type != LFun {
		return env.Errorf("first argument is not a function: %s", f.Type)
	}
	if f.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", f.FunType)
	}
	g = env.GetFunGlobal(g)
	if g.Type == LError {
		return f
	}
	if g.Type != LFun {
		return env.Errorf("second argument is not a function: %s", g.Type)
	}
	if g.IsSpecialFun() {
		return env.Errorf("second argument is not a regular function: %v", g.FunType)
	}
	formals := g.Cells[0].Copy()
	gcall := SExpr(make([]*LVal, 0, len(formals.Cells)+1))
	body := SExpr([]*LVal{Symbol("lisp:funcall"), f, gcall})
	gcall.Cells = append(gcall.Cells, Symbol("lisp:apply"), g)
	var restSym *LVal
	for i, argSym := range formals.Cells {
		if argSym.Type != LSymbol {
			// This should not happen.  The list of formals should be checked
			// when the g function was created.
			return env.Errorf("invalid list of formals: %s", formals)
		}
		if argSym.Str == OptArgSymbol {
			continue
		}
		if argSym.Str == KeyArgSymbol {
			continue
		}
		if argSym.Str == VarArgSymbol {
			if len(formals.Cells) != i+2 {
				// This should not happen.  The list of formals should be checked
				// when the g function was created.
				return env.Errorf("invalid list of formals: %s", formals)
			}
			restSym = formals.Cells[i+1]
			break
		}
		gcall.Cells = append(gcall.Cells, argSym)
	}
	if restSym != nil {
		gcall.Cells = append(gcall.Cells, restSym)
	} else {
		gcall.Cells = append(gcall.Cells, Nil())
	}
	newfun := env.Lambda(formals, []*LVal{body})
	return newfun
}

func builtinUnpack(env *LEnv, args *LVal) *LVal {
	return builtinApply(env, args)
}

func builtinFlip(env *LEnv, args *LVal) *LVal {
	fun := args.Cells[0]
	fun = env.GetFunGlobal(fun)
	if fun.Type == LError {
		return fun
	}
	if fun.Type != LFun {
		return env.Errorf("argument is not a function: %s", fun.Type)
	}
	if fun.IsSpecialFun() {
		return env.Errorf("first argument is not a regular function: %v", fun.FunType)
	}
	formals := fun.Cells[0]
	if len(formals.Cells) < 2 {
		return env.Errorf("argument is not a function of two arguments: %s", formals)
	}
	call := SExpr([]*LVal{fun, Symbol("y"), Symbol("x")})
	return env.Lambda(Formals("x", "y"), []*LVal{call})
}

func builtinAssoc(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	v := args.Cells[2]
	if m.IsNil() {
		m = SortedMap()
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	} else {
		mdata, err := m.copyMapData()
		if err != nil {
			return env.Error(err)
		}
		m = SortedMapFromData(mdata)
	}
	err := m.Map().Set(k, v)
	if !err.IsNil() {
		return env.Errorf("%s", err)
	}
	return m
}

func builtinAssocMutate(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	v := args.Cells[2]
	if m.IsNil() {
		return env.Errorf("first argument is nil: %v", m.Type)
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	err := m.Map().Set(k, v)
	if !err.IsNil() {
		return env.Error(err.String())
	}
	return m
}

func builtinDissoc(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	if m.IsNil() {
		m = SortedMap()
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	} else {
		mdata, err := m.copyMapData()
		if err != nil {
			return env.Error(err)
		}
		m = SortedMapFromData(mdata)
	}
	err := m.Map().Del(k)
	if !err.IsNil() {
		return env.Errorf("%s", err)
	}
	return m
}

func builtinDissocMutate(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	k := args.Cells[1]
	if m.IsNil() {
		return env.Errorf("first argument is nil: %v", m.Type)
	} else if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	err := m.Map().Del(k)
	if !err.IsNil() {
		return env.Error(err.String())
	}
	return m
}

func builtinGet(env *LEnv, args *LVal) *LVal {
	m, k := args.Cells[0], args.Cells[1]
	if m.IsNil() {
		return Nil()
	}
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	v, _ := m.Map().Get(k)
	return v
}

func builtinKeys(env *LEnv, args *LVal) *LVal {
	m := args.Cells[0]
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	return m.Map().Keys()
}

func builtinIsKey(env *LEnv, args *LVal) *LVal {
	m, k := args.Cells[0], args.Cells[1]
	if m.Type != LSortMap {
		return env.Errorf("first argument is not a map: %s", m.Type)
	}
	v, ok := m.Map().Get(k)
	if v.Type == LError {
		v.SetCallStack(env.Runtime.Stack.Copy())
		return v
	}
	return Bool(ok)
}

func builtinSortedMap(env *LEnv, args *LVal) *LVal {
	m := SortedMap()
	if len(args.Cells)%2 != 0 {
		return env.Errorf("uneven number of arguments: %d", len(args.Cells))
	}
	data := m.Map()
	for len(args.Cells) >= 2 {
		k := args.Cells[0]
		v := args.Cells[1]
		err := data.Set(k, v)
		if !err.IsNil() {
			return err
		}
		args.Cells = args.Cells[2:] //elps:mutates decap of the per-call arglist header (evalSExprCells builds fresh backing per call) to walk the key/value pairs
	}
	return m
}

func builtinConcat(env *LEnv, args *LVal) *LVal {
	typespec := args.Cells[0]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	switch typespec.Str {
	case "vector", "list":
		return builtinConcatSeq(env, args)
	case "string":
		return builtinConcatString(env, args)
	case "bytes":
		return builtinConcatBytes(env, args)
	default:
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
}

func builtinConcatString(env *LEnv, args *LVal) *LVal {
	// typespec is already known to be 'string here
	_, rest := args.Cells[0], args.Cells[1:]
	size := 0
	for _, v := range rest {
		n := v.Len()
		if n < 0 {
			// A type with no length cannot be a byte-sequence.
			return env.Errorf("argument is not sequence of bytes: %v", v.Type)
		}
		size += n
	}
	if msg := env.Runtime.CheckAlloc(size); msg != "" {
		return env.Errorf("%s", msg)
	}
	buf := bytes.NewBuffer(make([]byte, 0, size))
	for _, v := range rest {
		switch v.Type {
		case LBytes:
			buf.Write(v.Bytes())
		case LString:
			buf.WriteString(v.Str)
		default:
			err := appendBytes(env, v, func(x byte) {
				buf.WriteByte(x)
			})
			if err != nil {
				return env.Error(err)
			}
		}
	}
	return String(buf.String())
}

func builtinConcatBytes(env *LEnv, args *LVal) *LVal {
	// typespec is already known to be 'bytes here
	_, rest := args.Cells[0], args.Cells[1:]
	size := 0
	for _, v := range rest {
		n := v.Len()
		if n < 0 {
			// A type with no length cannot be a byte-sequence.
			return env.Errorf("argument is not sequence of bytes: %v", v.Type)
		}
		size += n
	}
	if msg := env.Runtime.CheckAlloc(size); msg != "" {
		return env.Errorf("%s", msg)
	}
	buf := make([]byte, 0, size)
	for _, v := range rest {
		switch v.Type {
		case LBytes:
			buf = append(buf, v.Bytes()...)
		case LString:
			buf = append(buf, v.Str...)
		default:
			err := appendBytes(env, v, func(x byte) {
				buf = append(buf, x)
			})
			if err != nil {
				return env.Error(err)
			}
		}
	}
	return Bytes(buf)
}

func appendBytes(_ *LEnv, seq *LVal, fn func(x byte)) error {
	if !isSeq(seq) {
		return fmt.Errorf("argument is not a sequence of bytes: %v", seq.Type)
	}
	cells := seqCells(seq)
	// Check all cells before appending any bytes.
	for _, v := range cells {
		if v.Type != LInt {
			return fmt.Errorf("value not a byte: %v", v.Type)
		}
		if v.Int < 0 || v.Int > 0xFF {
			return fmt.Errorf("value overflows byte: %v", v)
		}
	}
	for _, v := range cells {
		fn(byte(v.Int))
	}
	return nil
}

func builtinConcatSeq(env *LEnv, args *LVal) *LVal {
	typespec, rest := args.Cells[0], args.Cells[1:]
	size := 0
	for _, v := range rest {
		if !isSeq(v) {
			return env.Errorf("argument is not a proper sequence: %v", v.Type)
		}
		size += v.Len()
	}
	if env != nil {
		if msg := env.Runtime.CheckAlloc(size); msg != "" {
			return env.Errorf("%s", msg)
		}
	}
	if size == 0 {
		switch typespec.Str {
		case "vector":
			return Array(QExpr([]*LVal{Int(0)}), nil)
		case "list":
			return Nil()
		}
	}
	if typespec.Str != "vector" && typespec.Str != "list" {
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
	// The result's storage is filled BEFORE it is wrapped in an LVal, rather
	// than allocating the container first and writing through it.  Both orders
	// cost one allocation, but this one never observes the container in a
	// half-built state -- the previous order left the cells holding the slice's
	// zero value, a Go nil *LVal, until the appends below overwrote them, and a
	// Go nil is not a value the interpreter can hold (issue #367).  It also
	// avoids paying for Array's nil-initialization of storage this function is
	// about to overwrite in full (measured: ~8% of BenchmarkAliasConcatCopy).
	//
	// Capacity is exactly size, so the appends cannot outgrow the slice and the
	// result is clamped in the sense clampCap means: appending to the returned
	// value allocates rather than writing into a neighbour (issue #373).
	cells := make([]*LVal, 0, size)
	for _, v := range rest {
		cells = append(cells, seqCells(v)...)
	}
	if typespec.Str == "vector" {
		return Array(QExpr([]*LVal{Int(size)}), cells)
	}
	return QExpr(cells)
}

func builtinSortStable(env *LEnv, args *LVal) *LVal {
	less, list, optArgs := args.Cells[0], args.Cells[1], args.Cells[2:]
	var keyFun *LVal
	less = env.GetFunGlobal(less)
	if less.Type == LError {
		return less
	}
	if less.Type != LFun {
		return env.Errorf("first argument is not a function: %v", less.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second arument is not a proper list: %v", list.Type)
	}
	if len(optArgs) > 1 {
		return env.Errorf("too many optional arguments provided")
	}
	if len(optArgs) > 0 {
		keyFun = optArgs[0]
		keyFun = env.GetFunGlobal(keyFun)
		if keyFun.Type == LError {
			return less
		}
		if keyFun.Type != LFun {
			return env.Errorf("third argument is not a function: %v", keyFun.Type)
		}
	}
	if list.sealed {
		// list is (or shares storage with) a parsed program literal —
		// (stable-sort < '(3 1 2)) — and sorting it in place would rewrite
		// the program for every environment sharing the parse (the
		// substrate#378 class; see lisp/seal.go).  Raise the catchable
		// modify-literal-error condition instead of the silent
		// copy-on-write this site used to perform: a census over this
		// repository and the production phylum corpus found zero
		// non-test programs reaching this path (issue #378), and a silent
		// copy masks code that believed it owned the value.  The remedy is
		// (stable-sort < (copy lit)).
		if len(seqCells(list)) > 0 {
			return errModifyLiteral(env)
		}
		// The empty carve-out (see CondModifyLiteral): cdr, rest, keys and
		// friends return the shared sealed empty list, so erroring here
		// would make (stable-sort < (rest xs)) fail only for short xs.
		// There are no cells to sort and no storage to protect; return a
		// fresh, mutable header so the caller never holds sealed storage
		// as a sort result (in particular, never the Nil() singleton).
		cp := &LVal{}
		*cp = *list
		cp.sealed = false
		cp.Cells = nil
		list = cp
	}
	cells := seqCells(list)
	sortCells := &lvalByFun{
		env:    env,
		fun:    less,
		keyfun: keyFun,
		cells:  cells,
	}
	sort.Stable(sortCells)
	if sortCells.err != nil {
		return sortCells.err
	}
	return list
}

type lvalByFun struct {
	env    *LEnv
	fun    *LVal
	keyfun *LVal
	err    *LVal
	cells  []*LVal
}

func (s *lvalByFun) Len() int {
	return len(s.cells)
}

func (s *lvalByFun) Swap(i, j int) {
	if s.err != nil {
		return
	}
	s.cells[i], s.cells[j] = s.cells[j], s.cells[i]
}

func (s *lvalByFun) Less(i, j int) bool {
	if s.err != nil {
		return false
	}
	a, b := s.cells[i], s.cells[j]
	// Functions are always copied when being invoked. But the arguments
	// are not copied in general.
	var expr *LVal
	if s.keyfun == nil {
		expr = SExpr([]*LVal{s.fun, a.Copy(), b.Copy()})
	} else {
		expr = SExpr([]*LVal{
			s.fun,
			SExpr([]*LVal{s.keyfun, a.Copy()}),
			SExpr([]*LVal{s.keyfun, b.Copy()}),
		})
	}
	ok := s.env.Eval(expr)
	if ok.Type == LError {
		s.err = ok
		return false
	}
	return True(ok)
}

func builtinInsertIndex(env *LEnv, args *LVal) *LVal {
	typespec, list, index, item := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if index.Type != LInt {
		return env.Errorf("third arument is not a integer: %v", index.Type)
	}
	if index.Int < 0 || index.Int > list.Len() {
		return env.Errorf("index out of bounds")
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(1 + list.Len())}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, 1+list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	inCells := seqCells(list)
	copy(cells[:index.Int], inCells[:index.Int])
	copy(cells[index.Int+1:], inCells[index.Int:])
	cells[index.Int] = item
	return v
}

func builtinInsertSorted(env *LEnv, args *LVal) *LVal {
	typespec, list, p, item, optArgs := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3], args.Cells[4:]
	var keyFun *LVal
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if p.Type != LFun {
		return env.Errorf("third arument is not a function: %v", p.Type)
	}
	if len(optArgs) > 1 {
		return env.Errorf("too many optional arguments provided")
	}
	if len(optArgs) > 0 {
		keyFun = optArgs[0]
		keyFun = env.GetFunGlobal(keyFun)
		if keyFun.Type == LError {
			return keyFun
		}
		if keyFun.Type != LFun {
			return env.Errorf("last argument is not a function: %v", keyFun.Type)
		}
	}
	sortErr := Nil()
	inCells := seqCells(list)
	i := sort.Search(len(inCells), func(i int) bool {
		var expr *LVal
		if keyFun == nil {
			expr = SExpr([]*LVal{p, item.Copy(), inCells[i].Copy()})
		} else {
			expr = SExpr([]*LVal{
				p,
				SExpr([]*LVal{
					keyFun,
					item.Copy(),
				}),
				SExpr([]*LVal{
					keyFun,
					inCells[i].Copy(),
				}),
			})
		}
		ok := env.Eval(expr)
		if ok.Type == LError {
			sortErr = ok
			return false
		}
		return True(ok)
	})
	if !sortErr.IsNil() {
		return sortErr
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(1 + list.Len())}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, 1+list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	copy(cells[:i], inCells[:i])
	copy(cells[i+1:], inCells[i:])
	cells[i] = item
	return v
}

func builtinSearchSorted(env *LEnv, args *LVal) *LVal {
	n, p := args.Cells[0], args.Cells[1]
	if n.Type != LInt {
		return env.Errorf("first argument is not an integer: %v", n.Type)
	}
	p = env.GetFunGlobal(p)
	if p.Type == LError {
		return p
	}
	if p.Type != LFun {
		return env.Errorf("second arument is not a function: %v", p.Type)
	}
	sortErr := Nil()
	i := sort.Search(n.Int, func(i int) bool {
		expr := SExpr([]*LVal{p, Int(i)})
		ok := env.Eval(expr)
		if ok.Type == LError {
			sortErr = ok
			return false
		}
		return True(ok)
	})
	if !sortErr.IsNil() {
		return sortErr
	}
	return Int(i)
}

func builtinSelect(env *LEnv, args *LVal) *LVal {
	typespec, pred, list := args.Cells[0], args.Cells[1], args.Cells[2]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("second argument is not a function: %v", pred.Type)
	}
	if pred.IsSpecialFun() {
		return env.Errorf("second argument is not a regular function")
	}
	if !isSeq(list) {
		return env.Errorf("third argument is not a proper sequence: %v", list.Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = seqCells(v)
		cells = cells[0:0:list.Len()]
	case "list":
		v = QExpr(nil)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for _, v := range seqCells(list) {
		ok := env.FunCall(pred, SExpr([]*LVal{v}))
		if ok.Type == LError {
			return ok
		}
		if True(ok) {
			cells = append(cells, v)
		}
	}
	switch typespec.Str {
	case "list":
		v.Cells = cells
	case "vector":
		v.Cells[0].Cells[0].Int = len(cells)
		v.Cells[1].Cells = cells
	}
	return v
}

func builtinReject(env *LEnv, args *LVal) *LVal {
	typespec, pred, list := args.Cells[0], args.Cells[1], args.Cells[2]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("second argument is not a function: %v", pred.Type)
	}
	if pred.IsSpecialFun() {
		return env.Errorf("second argument is not a regular function")
	}
	if !isSeq(list) {
		return env.Errorf("third argument is not a proper sequence: %v", list.Type)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = seqCells(v)
		cells = cells[0:0:list.Len()]
	case "list":
		v = QExpr(nil)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for _, v := range seqCells(list) {
		ok := env.FunCall(pred, SExpr([]*LVal{v}))
		if ok.Type == LError {
			return ok
		}
		if !True(ok) {
			cells = append(cells, v)
		}
	}
	switch typespec.Str {
	case "list":
		v.Cells = cells
	case "vector":
		v.Cells[0].Cells[0].Int = len(cells)
		v.Cells[1].Cells = cells
	}
	return v
}

func builtinZip(env *LEnv, args *LVal) *LVal {
	typespec, lists := args.Cells[0], args.Cells[1:]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	n := lists[0].Len()
	for _, list := range lists {
		if !isSeq(list) {
			return env.Errorf("argument is not a proper list: %v", list.Type)
		}
		m := list.Len()
		if m < n {
			n = m
		}
	}
	if msg := env.Runtime.CheckAlloc(n * len(lists)); msg != "" {
		return env.Errorf("%s", msg)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(n)}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, n)
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for i := range cells {
		var elem *LVal
		var elemCells []*LVal
		switch typespec.Str {
		case "vector":
			elem = Array(QExpr([]*LVal{Int(len(lists))}), nil)
			elemCells = seqCells(elem)
		case "list":
			elemCells = make([]*LVal, len(lists))
			elem = QExpr(elemCells)
		default:
			return env.Errorf("type specifier is invalid: %v", typespec)
		}
		for j, list := range lists {
			elemCells[j] = seqCells(list)[i]
		}
		cells[i] = elem
	}
	return v
}

func builtinMakeSequence(env *LEnv, args *LVal) *LVal {
	start, stop := args.Cells[0], args.Cells[1]
	if !start.IsNumeric() {
		return env.Errorf("first argument is not numeric: %v", start.Type)
	}
	if !stop.IsNumeric() {
		return env.Errorf("second argument is not numeric: %v", stop.Type)
	}
	var step *LVal
	if len(args.Cells) == 2 {
		if start.Type == LInt {
			step = Int(1)
		} else {
			step = Float(1.0)
		}
	} else {
		if len(args.Cells) > 3 {
			return env.Errorf("too many arguments provided")
		}
		step = args.Cells[2]
		if !step.IsNumeric() {
			return env.Errorf("third argument is not numeric: %v", step.Type)
		}
		if !lessNumeric(Float(0), step) {
			return env.Errorf("third argument is not positive")
		}
	}
	list := QExpr(nil)
	for x := start; lessNumeric(x, stop); x = addNumeric(x, step) {
		if msg := env.Runtime.CheckAlloc(len(list.Cells) + 1); msg != "" {
			return env.Errorf("%s", msg)
		}
		list.Cells = append(list.Cells, x.Copy())
	}
	return list
}

func builtinReverse(env *LEnv, args *LVal) *LVal {
	typespec, list := args.Cells[0], args.Cells[1]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) {
		return env.Errorf("first argument is not a proper sequence: %v", args.Cells[0].Type)
	}
	if msg := env.Runtime.CheckAlloc(list.Len()); msg != "" {
		return env.Errorf("%s", msg)
	}
	var v *LVal
	var cells []*LVal
	switch typespec.Str {
	case "vector":
		v = Array(QExpr([]*LVal{Int(list.Len())}), nil)
		cells = seqCells(v)
	case "list":
		cells = make([]*LVal, list.Len())
		v = QExpr(cells)
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
	for i, v := range seqCells(list) {
		cells[len(cells)-1-i] = v
	}
	return v
}

func builtinSlice(env *LEnv, args *LVal) *LVal {
	typespec, list, start, end := args.Cells[0], args.Cells[1], args.Cells[2], args.Cells[3]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	if !isSeq(list) && list.Type != LString && list.Type != LBytes {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	if start.Type != LInt {
		return env.Errorf("third argument is not an integer: %v", start.Type)
	}
	if end.Type != LInt {
		return env.Errorf("forth argument is not an integer: %v", end.Type)
	}
	n := list.Len()
	i := start.Int
	j := end.Int
	if i < 0 {
		return env.Errorf("index out of range")
	}
	if i > n {
		return env.Errorf("index out of range")
	}
	if j < 0 {
		return env.Errorf("index out of range")
	}
	if j > n {
		return env.Errorf("index out of range")
	}
	if i > j {
		return env.Errorf("end before start")
	}

	// Create an intermediate sliced list with a similar type.
	//
	// The bytes and sequence reslices are THREE-index (issue #373).  A
	// two-index `cells[i:j]` keeps the source's capacity, so a later append
	// to the returned view had spare room and grew into it -- writing through
	// the view into source elements at index >= j, which the caller never
	// named and cannot see in the value they were handed:
	//
	//	(set 'v (vector 10 20 30 40))
	//	(append! (slice 'vector v 0 2) 999)
	//	v  ; => (vector 10 20 999 40) before the fix
	//
	// Clamping the capacity to the view's length forces that append to
	// reallocate instead.  The view still SHARES the elements in [i,j) with
	// the source -- slice returns a view, not a copy, and an in-place
	// mutation such as stable-sort is still visible through the source.  Use
	// (concat 'vector (slice ...)) when a snapshot is wanted.
	//
	// The string case needs no clamp: Go strings are immutable, so a string
	// view cannot be written through at all.
	switch list.Type {
	case LString:
		list = String(list.Str[i:j])
	case LBytes:
		list = Bytes(clampCapBytes(list.Bytes()[i:j]))
	default: // isSeq(list)
		sealed := list.sealed
		list = QExpr(clampCap(seqCells(list)[i:j]))
		// The clamp stops an append growing past the view (issue #373); the
		// view still shares the original backing WITHIN its own bounds, so a
		// sealed input's constraint travels with the intermediate value
		// (lisp/seal.go) and the 'vector arm below copies on it.
		list.sealed = sealed
	}

	// Convert the intermediate sliced value into the desired type
	switch typespec.Str {
	case "string":
		switch list.Type {
		case LString:
			return list
		case LBytes:
			return String(string(list.Bytes()))
		default: // LSExpr
			var b []byte
			err := appendBytes(env, list, func(x byte) {
				b = append(b, x)
			})
			if err != nil {
				return env.Error(err)
			}
			return String(string(b))
		}
	case "bytes":
		switch list.Type {
		case LString:
			return Bytes([]byte(list.Str))
		case LBytes:
			return list
		default: // LSExpr
			var b []byte
			err := appendBytes(env, list, func(x byte) {
				b = append(b, x)
			})
			if err != nil {
				return env.Error(err)
			}
			return Bytes(b)
		}
	case "list":
		if list.Type == LString || list.Type == LBytes {
			list = makeByteSeq(list)
		}
		// list is now known to be LSExpr.  The intermediate above already
		// carries the sealed flag when the backing is shared with a sealed
		// input, so returning it directly keeps the constraint attached.
		return list
	case "vector":
		if list.Type == LString || list.Type == LBytes {
			list = makeByteSeq(list)
		}
		// list is now known to be LSExpr
		cells := list.Cells
		if list.sealed {
			// Vectors are mutable (append! writes their backing in place)
			// and are never sealed, so wrapping a sealed list's backing
			// array in a vector would hand the value domain a mutable
			// window onto the shared program (lisp/seal.go).  Raise the
			// catchable modify-literal-error condition instead of the
			// silent copy-on-write this site used to perform (issue #378);
			// the remedy is (slice 'vector (copy lit) ...).
			if len(cells) > 0 {
				return errModifyLiteral(env)
			}
			// The empty carve-out (see CondModifyLiteral): a zero-width
			// window over sealed backing — an empty slice of a literal, or
			// a slice of the shared sealed empty list cdr/rest return —
			// can neither write nor alias any sealed storage.  Drop the
			// window entirely so the vector owns (empty) fresh backing.
			cells = nil
		}
		return Array(QExpr([]*LVal{Int(len(cells))}), cells)
	default:
		return env.Errorf("type specifier is not valid: %v", typespec)
	}
}

// NOTE (issue #373):  builtinList and builtinVector are deliberately NOT
// capacity-clamped.  They hand out the call's own argument slice, which the
// evaluator has already discarded, so its spare capacity is not reachable from
// any other live value -- there is nothing for an append to corrupt.  Clamping
// them would not be free either: it would force the first (append! (vector
// ...) x) to reallocate for no benefit.
func builtinList(env *LEnv, v *LVal) *LVal {
	return QExpr(v.Cells)
}

func builtinVector(env *LEnv, args *LVal) *LVal {
	return Array(nil, args.Cells)
}

// builtinAppendMutate implements the append! builtin.
//
// NOTE (issue #373):  This is deliberately NOT capacity-clamped.  append! is
// the in-place accumulator -- it is documented to mutate its argument and
// return it -- so it keeps Go's amortised growth and stays O(1) per element.
// That is safe because the target's spare capacity is not reachable from any
// other value: every producer of a view (slice, cdr, rest) clamps what it
// hands out, and every non-mutating append clamps what it reads, so nothing
// else can be looking at the bytes past vec's length.
//
// The caller who reaches for append! has already said they want mutation.
// The bug in #373 was mutation nobody asked for.
func builtinAppendMutate(env *LEnv, args *LVal) *LVal {
	vec, vals := args.Cells[0], args.Cells[1:]
	if vec.Type == LBytes {
		return appendMutateBytes(env, args)
	}
	if !isVec(vec) {
		return env.Errorf("first argument is not a vector: %v", vec.Type)
	}
	if msg := env.Runtime.CheckAlloc(len(vec.Cells[1].Cells) + len(vals)); msg != "" {
		return env.Errorf("%s", msg)
	}
	dims := vec.Cells[0]
	dims.Cells[0].Int += len(vals)                           //elps:mutates append! is the documented mutating variant: it extends its vector argument in place
	vec.Cells[1].Cells = append(vec.Cells[1].Cells, vals...) //elps:mutates append! is the documented mutating variant: it extends its vector argument in place
	return vec
}

// appendMutateBytes is the bytes path of append!.  Not capacity-clamped,
// for the reason given on builtinAppendMutate (issue #373).
func appendMutateBytes(env *LEnv, args *LVal) *LVal {
	lbytes, xs := args.Cells[0], args.Cells[1:]
	b := lbytes.Bytes()
	xsVal := QExpr(xs)
	if msg := env.Runtime.CheckAlloc(len(b) + xsVal.Len()); msg != "" {
		return env.Errorf("%s", msg)
	}
	err := appendBytes(env, xsVal, func(x byte) {
		b = append(b, x) //elps:mutates append! 'bytes is the documented mutating variant: b is written back into lbytes' backing below
	})
	if err != nil {
		return env.Error(err)
	}
	*lbytes.Native.(*[]byte) = b
	return lbytes
}

// builtinAppendBytesMutate implements append-bytes!.  Not capacity-clamped,
// for the reason given on builtinAppendMutate (issue #373).
func builtinAppendBytesMutate(env *LEnv, args *LVal) *LVal {
	lbytes, byteseq := args.Cells[0], args.Cells[1]
	if lbytes.Type != LBytes {
		return env.Errorf("first argument is not bytes: %v", lbytes.Type)
	}
	b := lbytes.Bytes()
	if msg := env.Runtime.CheckAlloc(len(b) + byteseq.Len()); msg != "" {
		return env.Errorf("%s", msg)
	}
	switch byteseq.Type {
	case LString:
		b = append(b, byteseq.Str...) //elps:mutates append-bytes! is the documented mutating variant: b is written back into lbytes' backing below
	case LBytes:
		b = append(b, byteseq.Bytes()...) //elps:mutates append-bytes! is the documented mutating variant: b is written back into lbytes' backing below
	default:
		err := appendBytes(env, byteseq, func(x byte) {
			b = append(b, x) //elps:mutates append-bytes! is the documented mutating variant: b is written back into lbytes' backing below
		})
		if err != nil {
			return env.Error(err)
		}
	}
	*lbytes.Native.(*[]byte) = b
	return lbytes
}

func builtinAppend(env *LEnv, args *LVal) *LVal {
	typespec, seq, vals := args.Cells[0], args.Cells[1], args.Cells[2:]
	if typespec.Type != LSymbol {
		return env.Errorf("first argument is not a valid type specification: %v", typespec.Type)
	}
	if typespec.Str == "bytes" {
		return builtinAppend_Bytes(env, args)
	}
	if !isSeq(seq) {
		return env.Errorf("second argument is not a proper sequence: %v", seq.Type)
	}
	cells := seqCells(seq)
	if msg := env.Runtime.CheckAlloc(len(cells) + len(vals)); msg != "" {
		return env.Errorf("%s", msg)
	}
	switch typespec.Str {
	case "list":
		// The Cells of the returned list must not intersect with seqCells(seq)
		// (in regards to slice memory regions, not referenced LVals).  This is
		// particularly relevent if seq is a vector.  This makes the following
		// code more verbose.
		list := make([]*LVal, 0, len(cells)+len(vals))
		list = append(list, cells...)
		list = append(list, vals...)
		return QExpr(list)
	case "vector":
		// The input is clamped so this append can never write into seq's
		// spare capacity (issue #373).
		//
		// This reverses a deliberate old decision.  The comment here used to
		// say the result "may intersect with seqCells(seq) when chaining
		// calls to ``append'' so that vectors may be used in a manner akin
		// to go slices", and array_test.go pinned that as intended: after
		//
		//	(set 'v123 (append 'vector v12 3))
		//	(set 'v1234 (append 'vector v123 4))
		//	(set 'v1235 (append 'vector v123 5))
		//
		// v1234 read (vector 1 2 3 5) -- the second append had overwritten
		// the first one's result through their shared backing array.  The
		// value that changed was one `append` had already handed back and
		// promised not to touch, so no caller could have defended against
		// it.  `append` is the non-mutating constructor; its docstring says
		// so.  Sharing that can rewrite an already-returned value is not a
		// contract anyone can use correctly, so it is gone.
		//
		// The cost is real and is the point: an append that could previously
		// grow into spare capacity now reallocates and copies, so building a
		// vector with repeated (set 'v (append 'vector v x)) is quadratic.
		// `append!` is the in-place accumulator and keeps its amortised
		// growth -- the docs now say to reach for it.
		//
		// The clamp is NOT sufficient on its own when seq is sealed, and the
		// gap is narrow enough to be worth writing down.  `values` is &rest,
		// so `(append 'vector lit)` with no values appends nothing:
		// append(clampCap(cells)) then returns the input slice unchanged and
		// Array wraps the SEALED literal's own backing in a fresh, UNSEALED
		// vector -- a mutable window onto the shared program, which is the
		// substrate#378 class arriving through the one input the clamp does
		// not reallocate.  So the seal is answered where the seal is: a
		// sealed, non-empty input raises the catchable modify-literal-error
		// condition (issue #378; this site used to copy-on-write silently),
		// and the remedy is (append 'vector (copy lit) ...).
		//
		// The empty case falls through deliberately (see CondModifyLiteral):
		// cdr, rest and friends return the shared sealed empty list, so
		// erroring on it would make (append 'vector (rest xs) v) fail only
		// for short xs.  An empty sealed input is safe on the ordinary path
		// below: clampCap leaves it zero-capacity, so the append reallocates
		// and no window onto sealed storage can survive into the result.
		if seq.sealed && len(cells) > 0 {
			return errModifyLiteral(env)
		}
		// clampCap makes this append PROVABLY non-aliasing: it returns a
		// three-index reslice whose cap equals its len, so append cannot
		// write into seq's backing and must reallocate.  That is the whole
		// content of the issue #373 fix.  elpsvet's alias rule does not
		// follow the capacity through the helper call, so the proof is
		// recorded here rather than the rule weakened.
		//elps:mutates appends into a cap==len reslice (clampCap), which forces a reallocation; seq's backing is unreachable from the result
		return Array(nil, append(clampCap(cells), vals...))
	default:
		return env.Errorf("type specifier is invalid: %v", typespec)
	}
}

// NOTE:  The name of this function is funky because the append-bytes function
// will have other behavior, and there is another func in this file called
// appendBytes.
func builtinAppend_Bytes(env *LEnv, args *LVal) *LVal {
	// The comment that used to sit here claimed the sequence "has already been
	// validated".  It had not been: builtinAppend validates the type
	// SPECIFIER (args.Cells[0] == 'bytes) and then dispatches here without
	// looking at args.Cells[1], so `(append 'bytes 0)` reached LVal.Bytes()
	// with an int and panicked ("not bytes: int").  Under an embedded
	// interpreter that surfaces as an internal-panic condition -- an error no
	// ordinary handler can contain -- and in a phylum it is a chaincode
	// crash.  This is the same guard append-bytes has always had.
	_, lbytes, xs := args.Cells[0], args.Cells[1], args.Cells[2:]
	if lbytes.Type != LBytes {
		return env.Errorf("second argument is not bytes: %v", lbytes.Type)
	}
	// Clamped so this append cannot write into lbytes' spare capacity
	// (issue #373) -- `append 'bytes` is non-mutating and must not disturb
	// the source or any result it handed back earlier.
	b := clampCapBytes(lbytes.Bytes())
	xsVal := QExpr(xs)
	resultLen := len(b) + xsVal.Len()
	if msg := env.Runtime.CheckAlloc(resultLen); msg != "" {
		return env.Errorf("%s", msg)
	}
	err := appendBytes(env, xsVal, func(x byte) {
		b = append(b, x) //elps:mutates deliberate go-slice-style append: the result may share lbytes' backing so chained appends amortize, mirroring append 'vector (see #371 for the slice-retained-capacity caveat)
	})
	if err != nil {
		return env.Error(err)
	}
	return Bytes(b)
}

func builtinAppendBytes(env *LEnv, args *LVal) *LVal {
	lbytes, byteseq := args.Cells[0], args.Cells[1]
	if lbytes.Type != LBytes {
		return env.Errorf("first argument is not bytes: %v", lbytes.Type)
	}
	// Clamped so this append cannot write into lbytes' spare capacity
	// (issue #373).  append-bytes! is the mutating variant.
	b := clampCapBytes(lbytes.Bytes())
	switch byteseq.Type {
	case LString:
		b = append(b, byteseq.Str...) //elps:mutates deliberate go-slice-style append: the result may share lbytes' backing so chained appends amortize, mirroring append 'vector (see #371 for the slice-retained-capacity caveat)
	case LBytes:
		b = append(b, byteseq.Bytes()...) //elps:mutates deliberate go-slice-style append: the result may share lbytes' backing so chained appends amortize, mirroring append 'vector (see #371 for the slice-retained-capacity caveat)
	default:
		err := appendBytes(env, byteseq, func(x byte) {
			b = append(b, x) //elps:mutates deliberate go-slice-style append: the result may share lbytes' backing so chained appends amortize, mirroring append 'vector (see #371 for the slice-retained-capacity caveat)
		})
		if err != nil {
			return env.Error(err)
		}
	}
	return Bytes(b)
}

func builtinARef(env *LEnv, args *LVal) *LVal {
	array, indices := args.Cells[0], args.Cells[1:]
	if array.Type != LArray {
		return env.Errorf("first argument is not an array: %v", array.Type)
	}
	v := array.ArrayIndex(indices...)
	if v.Type == LError {
		return env.Error(v)
	}
	return v
}

func builtinLength(env *LEnv, args *LVal) *LVal {
	seq := args.Cells[0]
	n := seq.Len()
	if n < 0 {
		return env.Errorf("first argument is not a list, map, vector, bytes, or a string: %v", seq.Type)
	}
	return Int(n)
}

func builtinIsEmpty(env *LEnv, args *LVal) *LVal {
	seq := args.Cells[0]
	n := seq.Len()
	if n < 0 {
		return env.Errorf("first argument is not a list, map, vector, bytes, or a string: %v", seq.Type)
	}
	return Bool(n == 0)
}

func builtinCons(env *LEnv, args *LVal) *LVal {
	head, tail := args.Cells[0], args.Cells[1]
	if tail.Type != LSExpr {
		return env.Errorf("second argument is not a list: %s", tail.Type)
	}
	cells := make([]*LVal, 0, 1+args.Len())
	cells = append(cells, head)
	cells = append(cells, tail.Cells...)
	return QExpr(cells)
}

func builtinNot(env *LEnv, args *LVal) *LVal {
	return Bool(Not(args.Cells[0]))
}

func builtinIsTrue(env *LEnv, args *LVal) *LVal {
	return Bool(True(args.Cells[0]))
}

func builtinType(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return GetType(v)
}

func builtinIsType(env *LEnv, args *LVal) *LVal {
	typespec := args.Cells[0]
	v := args.Cells[1]
	if typespec.Type != LSymbol && typespec.Type != LTaggedVal {
		return env.Errorf("first argument is not a valid type specifier: %v", typespec.Type)
	}
	typesym := typespec.Str
	if typespec.Type == LTaggedVal {
		if typesym != env.Runtime.Registry.Lang+":typedef" {
			return env.Errorf("first argument is not a valid type specifier: %v", typesym)
		}
		typesym = typespec.Cells[0].Cells[0].Str
	}
	t := GetType(v)
	return Bool(t.Str == typesym)
}

func builtinIsTaggedVal(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LTaggedVal)
}

func builtinUserData(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.Type != LTaggedVal {
		return env.Errorf("argument is not a tagged value: %v", GetType(v))
	}
	return v.UserData()
}

func builtinNew(env *LEnv, args *LVal) *LVal {
	typespec := args.Cells[0]
	cargs := args.Cells[1:]
	if typespec.Type == LSymbol {
		typespec = env.GetGlobal(typespec)
		if typespec.Type == LError {
			return typespec
		}
	}
	return env.New(typespec, SExpr(cargs))
}

func builtinIsNil(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	if v.IsNil() {
		return Bool(true)
	}
	return Bool(false)
}

func builtinIsList(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSExpr)
}

func builtinIsSortedMap(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSortMap)
}

func builtinIsArray(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LArray)
}

func builtinIsVector(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LArray && v.Cells[0].Len() == 1)
}

func builtinIsBool(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSymbol && (v.Str == TrueSymbol || v.Str == FalseSymbol))
}

func builtinIsNumber(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.IsNumeric())
}

func builtinIsInt(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LInt)
}

func builtinIsFloat(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LFloat)
}

func builtinIsSymbol(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LSymbol)
}

func builtinIsString(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LString)
}

func builtinIsBytes(env *LEnv, args *LVal) *LVal {
	v := args.Cells[0]
	return Bool(v.Type == LBytes)
}

func builtinEqual(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	return a.Equal(b)
}

func builtinAllP(env *LEnv, args *LVal) *LVal {
	pred, list := args.Cells[0], args.Cells[1]
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("first argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a proper sequence: %v", list.Type)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if !True(ok) {
			return Bool(false)
		}
	}
	return Bool(true)
}

func builtinAnyP(env *LEnv, args *LVal) *LVal {
	pred, list := args.Cells[0], args.Cells[1]
	pred = env.GetFunGlobal(pred)
	if pred.Type == LError {
		return pred
	}
	if pred.Type != LFun {
		return env.Errorf("first argument is not a function: %v", pred.Type)
	}
	if !isSeq(list) {
		return env.Errorf("second argument is not a list: %v", list.Type)
	}
	for _, v := range seqCells(list) {
		expr := SExpr([]*LVal{pred, v})
		ok := env.Eval(expr)
		if ok.Type == LError {
			return ok
		}
		if True(ok) {
			return ok
		}
	}
	return Bool(false)
}

func builtinMax(env *LEnv, args *LVal) *LVal {
	maxVal := args.Cells[0]
	if !maxVal.IsNumeric() {
		return env.Errorf("argument is not a number: %s", maxVal.Type)
	}
	for _, x := range args.Cells[1:] {
		if !x.IsNumeric() {
			return env.Errorf("argument is not a number: %s", x.Type)
		}
		if lessNumeric(maxVal, x) {
			maxVal = x
		}
	}
	return maxVal
}

func builtinMin(env *LEnv, args *LVal) *LVal {
	minVal := args.Cells[0]
	if !minVal.IsNumeric() {
		return env.Errorf("argument is not a number: %s", minVal.Type)
	}
	for _, x := range args.Cells[1:] {
		if !x.IsNumeric() {
			return env.Errorf("argument is not a number: %s", x.Type)
		}
		if lessNumeric(x, minVal) {
			minVal = x
		}
	}
	return minVal
}

func builtinStringLEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str <= b.Str)
}

func builtinStringLT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str < b.Str)
}

func builtinStringGEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str >= b.Str)
}

func builtinStringGT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str > b.Str)
}

// BUG:  Symbol equality is not well defined and so this function's
// implemantion may need to change in an incompatible way.
func builtinSymbolEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LSymbol {
		return env.Errorf("first argument is not a symbol: %s", a.Type)
	}
	if b.Type != LSymbol {
		return env.Errorf("second argument is not a symbol: %s", b.Type)
	}
	return Bool(a.Str == b.Str)
}

func builtinStringEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LString {
		return env.Errorf("first argument is not a string: %s", a.Type)
	}
	if b.Type != LString {
		return env.Errorf("second argument is not a string: %s", b.Type)
	}
	return Bool(a.Str == b.Str)
}

func builtinLEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int <= b.Int)
	}
	return Bool(toFloat(a) <= toFloat(b))
}

func builtinLT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int < b.Int)
	}
	return Bool(toFloat(a) < toFloat(b))
}

func builtinGEq(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int >= b.Int)
	}
	return Bool(toFloat(a) >= toFloat(b))
}

func builtinGT(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return Bool(a.Int > b.Int)
	}
	return Bool(toFloat(a) > toFloat(b))
}

func builtinEqNum(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	return a.equalNum(b)
}

func builtinPow(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if !a.IsNumeric() {
		return env.Errorf("first argument is not a number: %s", a.Type)
	}
	if !b.IsNumeric() {
		return env.Errorf("second argument is not a number: %s", b.Type)
	}
	if bothInt(a, b) {
		return powInt(a.Int, b.Int)
	}
	return Float(math.Pow(toFloat(a), toFloat(b)))
}

// powInt raises a to the b-th power in int arithmetic, wrapping on overflow
// exactly as Go's `*` does.
//
// Binary exponentiation: at most 63 iterations for any b, because b is
// halved every turn.  The previous implementation doubled an exponent
// accumulator instead --
//
//	n := 1; atob := a
//	for 2*n < b { atob *= atob; n *= 2 }
//	for n < b   { atob *= a;    n++ }
//
// -- and for b > 2^62 that loop never terminates.  n doubles 1, 2, ..., 2^62,
// then 2^63 wraps to MinInt, then MinInt*2 wraps to exactly 0, and 2*0 < b is
// true forever.  Zero is a true fixed point: the loop allocates nothing and
// evaluates nothing, so it is invisible to MaxAlloc, to MaxSteps AND to a
// context deadline -- none of those are consulted inside a builtin.
// (pow -128 9223372036854775807) hung the process until something killed it.
// Found by FuzzApplyStdlib as (pow -9223372036854775808 9223372036854775806).
//
// The result is unchanged wherever the old loop terminated.  Go's int
// multiplication is arithmetic mod 2^64, which is a commutative ring, so the
// order in which the b factors are associated cannot change the product --
// squaring-and-multiplying and multiplying b times agree bit for bit,
// including on the wrapped answers.  (pow 2 64) still returns 0.
func powInt(a, b int) *LVal {
	if b == 0 {
		return Int(1)
	}
	if b < 0 {
		return Float(math.Pow(float64(a), float64(b)))
	}
	atob := 1
	base := a
	for b > 0 {
		if b&1 == 1 {
			atob *= base
		}
		b >>= 1
		if b > 0 {
			base *= base
		}
	}
	return Int(atob)
}

func builtinMod(env *LEnv, args *LVal) *LVal {
	a, b := args.Cells[0], args.Cells[1]
	if a.Type != LInt {
		return env.Errorf("first argument is not an int: %s", a.Type)
	}
	if b.Type != LInt {
		return env.Errorf("second argument is not an int: %s", b.Type)
	}
	if b.Int == 0 {
		return env.Errorf("second argument is zero")
	}
	return Int(a.Int % b.Int)
}

func builtinAdd(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(0)
	}
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}
	elemt := numericListType(v.Cells)
	if elemt == LInt {
		sum := 0
		for _, c := range v.Cells {
			sum += c.Int
		}
		return Int(sum)
	}
	sum := 0.0
	for _, c := range v.Cells {
		sum += toFloat(c)
	}
	return Float(sum)
}

func builtinSub(env *LEnv, v *LVal) *LVal {
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}

	if len(v.Cells) == 0 {
		return Int(0)
	}

	if len(v.Cells) == 1 {
		x := v.Cells[0]
		switch v.Cells[0].Type {
		case LInt:
			return Int(-x.Int)
		case LFloat:
			return Float(-x.Float)
		default:
			// indicates some bug in IsNumeric or this switch statement
			return env.Errorf("invalid numeric type: %v", x.Type)
		}
	}

	elemt := numericListType(v.Cells)
	if elemt == LInt {
		diff := v.Cells[0].Int
		for _, c := range v.Cells[1:] {
			diff -= c.Int
		}
		return Int(diff)
	}
	diff := toFloat(v.Cells[0])
	for _, c := range v.Cells[1:] {
		diff -= toFloat(c)
	}
	return Float(diff)
}

func builtinDiv(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(1)
	}

	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}

	if len(v.Cells) == 1 {
		// This is kind of dumb but it catches the case 1/1 which should return
		// an int.
		return divInt(Int(1), v)
	}

	// Attempt to perform integer division when all operands are integers and
	// division is exact.
	if v.Cells[0].Type == LInt {
		return divInt(v.Cells[0], SExpr(v.Cells[1:]))
	}
	return divFloat(v.Cells[0], SExpr(v.Cells[1:]))
}

// divInt tries to perform division as int if all quotients divide the previous
// result.
func divInt(x, args *LVal) *LVal {
	if x.Type != LInt {
		return Errorf("internal error: divInt called with non-integer: %v", x.Type)
	}
	ys := args.Cells
	for i := range ys {
		y := ys[i]
		switch {
		case y.Type != LInt:
			return divFloat(x, SExpr(ys[i:]))
		case y.Int == 0:
			// The result should be NaN, +Inf, or -Inf depending on the
			// remaining ys.  These computations should be carried out in
			// IEEE754 floating point for accuracy.
			return divFloat(x, SExpr(ys[i:]))
		case x.Int%y.Int != 0:
			return divFloat(x, SExpr(ys[i:]))
		default:
			// y divides x
			x = Int(x.Int / y.Int)
		}
	}
	return x
}

// divFloat performs float point division (the default).  divFloat assumes that
// there is at least one argument.
func divFloat(x, args *LVal) *LVal {
	div := x.Float
	if x.Type == LInt {
		div = float64(x.Int)
	}
	for _, c := range args.Cells {
		if c.Type == LInt {
			div /= float64(c.Int)
		} else {
			div /= c.Float
		}
	}
	return Float(div)
}

func builtinMul(env *LEnv, v *LVal) *LVal {
	if len(v.Cells) == 0 {
		return Int(1)
	}
	for _, c := range v.Cells {
		if !c.IsNumeric() {
			return env.Errorf("argument is not a number: %v", c.Type)
		}
	}
	return mulInt(Int(1), v)
}

// mulInt tries to perform multiplication as int if all arguments are int.
func mulInt(x, args *LVal) *LVal {
	if x.Type != LInt {
		return Errorf("internal error: mulInt called with non-integer: %v", x.Type)
	}
	ys := args.Cells
	for i := range ys {
		y := ys[i]
		switch {
		case y.Type != LInt:
			return mulFloat(x, SExpr(ys[i:]))
		default:
			x = Int(x.Int * y.Int)
		}
	}
	return x
}

// mulFloat performs floating point multiplication.
func mulFloat(x, args *LVal) *LVal {
	// NOTE:  We can't short-circuit upon seeing the value 0 because of
	// interactions when multplying 0 by Inf.  It is possible that
	// short-circuiting on NaN is viable, though this is probably not common
	// and the benefits are unclear.
	prod := x.Float
	if x.Type == LInt {
		prod = float64(x.Int)
	}
	for _, c := range args.Cells {
		if c.Type == LInt {
			prod *= float64(c.Int)
		} else {
			prod *= c.Float
		}
	}
	return Float(prod)
}

func builtinDebugPrint(env *LEnv, args *LVal) *LVal {
	// There is deliberately no zero-argument special case here.  There used to
	// be one, and it called fmt.Println -- writing the newline to os.Stdout,
	// ignoring Runtime.Stderr entirely.  That contradicted both the builtin's
	// documentation ("Prints all arguments to stderr") and its own non-empty
	// branch, and it meant an embedder who had redirected debug output still
	// got a stray newline on the host process's stdout, which for a
	// stdout-is-the-protocol host (an LSP server, a JSON-RPC chaincode
	// process) is a corrupted stream rather than cosmetic noise.
	//
	// fmt.Fprintln with no variadic arguments already writes exactly the
	// newline, so removing the branch is the whole fix.
	fmtargs := make([]interface{}, len(args.Cells))
	for i := range args.Cells {
		fmtargs[i] = args.Cells[i]
	}
	fmt.Fprintln(env.Runtime.getStderr(), fmtargs...) //nolint:errcheck // best-effort debug output
	return Nil()
}

func builtinDebugStack(env *LEnv, args *LVal) *LVal {
	_, err := env.Runtime.Stack.DebugPrint(env.Runtime.getStderr())
	if err != nil {
		return env.Error(err)
	}
	return Nil()
}

func addNumeric(a, b *LVal) *LVal {
	if bothInt(a, b) {
		return Int(a.Int + b.Int)
	}
	return Float(toFloat(a) + toFloat(b))
}

func lessNumeric(a, b *LVal) bool {
	if bothInt(a, b) {
		return a.Int < b.Int
	}
	return toFloat(a) < toFloat(b)
}

func bothInt(a, b *LVal) bool {
	if a.Type == LInt && b.Type == LInt {
		return true
	}
	return false
}

func numericListType(cells []*LVal) LType {
	if len(cells) == 0 {
		return LInvalid
	}
	if !cells[0].IsNumeric() {
		return cells[0].Type
	}
	t := cells[0].Type
	for _, c := range cells[1:] {
		if t == c.Type {
			continue
		}
		if c.IsNumeric() {
			t = LFloat
		}
	}
	return t
}

func toFloat(x *LVal) float64 {
	if !x.IsNumeric() {
		// NOT LISP-REACHABLE (#367): toFloat is unexported and every caller --
		// builtinLEq/LT/GEq/GT/Pow, builtinAdd/Sub, addNumeric/lessNumeric and
		// equalNum -- rejects a non-numeric operand with an error before
		// reaching here, so a program cannot present one.  The panic marks a
		// hole in one of those guards, not bad input.
		panic("toFloat called with non-numeric argument: " + x.String())
	}
	if x.Type == LInt {
		return float64(x.Int)
	}
	return x.Float
}

func builtinFormatString(env *LEnv, args *LVal) *LVal {
	format := args.Cells[0]
	fvals := args.Cells[1:]
	if format.Type != LString {
		return env.Errorf("first argument is not a string")
	}
	f := format.Str

	// Fast path: no braces at all.
	if strings.IndexByte(f, '{') < 0 && strings.IndexByte(f, '}') < 0 {
		return String(f)
	}

	var buf strings.Builder
	buf.Grow(len(f) + len(fvals)*16)

	seqIndex := 0
	// mode: 0 = undecided, 1 = sequential, 2 = positional
	mode := 0
	i := 0
	n := len(f)

	for i < n {
		// Find the next '{' or '}'.
		j := i
		for j < n && f[j] != '{' && f[j] != '}' {
			j++
		}
		// Write any literal text before the brace.
		if j > i {
			buf.WriteString(f[i:j])
		}
		if j >= n {
			break
		}

		ch := f[j]
		if ch == '}' {
			// Must be a '}}' escape.
			if j+1 < n && f[j+1] == '}' {
				buf.WriteByte('}')
				i = j + 2
				continue
			}
			return env.Errorf("unexpected closing brace '}' outside of formatting directive")
		}

		// ch == '{'
		// Check for '{{' escape.
		if j+1 >= n {
			return env.Errorf("unclosed '{' in format string")
		}
		if f[j+1] == '{' {
			buf.WriteByte('{')
			i = j + 2
			continue
		}

		// Find the closing '}'.
		closeIdx := strings.IndexByte(f[j+1:], '}')
		if closeIdx < 0 {
			return env.Errorf("unclosed '{' in format string")
		}
		closeIdx += j + 1 // absolute index of '}'

		inner := f[j+1 : closeIdx]

		// Determine argument index.
		var argIdx int
		if len(inner) == 0 || isAllSpaces(inner) {
			// Sequential placeholder: {} or {  }
			if mode == 2 {
				return env.Errorf("cannot mix positional {N} and sequential {} placeholders")
			}
			mode = 1
			argIdx = seqIndex
			seqIndex++
		} else {
			// Positional placeholder: {0}, { 1 }, etc.
			inner = strings.TrimSpace(inner)
			idx, err := strconv.Atoi(inner)
			if err != nil {
				return env.Errorf("invalid format directive: {%s}", inner)
			}
			if idx < 0 {
				return env.Errorf("negative positional index: {%s}", inner)
			}
			if mode == 1 {
				return env.Errorf("cannot mix positional {N} and sequential {} placeholders")
			}
			mode = 2
			argIdx = idx
		}

		if argIdx >= len(fvals) {
			if mode == 2 {
				return env.Errorf("positional index %d out of range for %d supplied values", argIdx, len(fvals))
			}
			return env.Errorf("too many formatting directives for supplied values")
		}

		val := fvals[argIdx]
		if val.Type == LString && !val.quoted {
			buf.WriteString(val.Str)
		} else {
			buf.WriteString(val.String())
		}

		i = closeIdx + 1
	}

	return String(buf.String())
}

// isAllSpaces returns true if s contains only spaces and tabs.
func isAllSpaces(s string) bool {
	for i := range len(s) {
		if s[i] != ' ' && s[i] != '\t' {
			return false
		}
	}
	return true
}
