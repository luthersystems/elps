// Copyright © 2018 The ELPS authors

package lisp

// TrueSymbol is the language's defacto true boolean value, though anything
// other than nil and 'false are considered true by functions and operators
// expecting a boolean.
const TrueSymbol = "true"

// FalseSymbol is the language's defacto false boolean value, though nil is
// also considered false by functions and operators expecting a boolean.
const FalseSymbol = "false"

// MetadaArgPrefix is a disallowed prefix for formal argument symbols.  Any
// symbol beginning with MetaArgPrefix in a formal argument list will be
// treated with special meaning an unrecognized symbols will cause a runtime
// error to occur.
const MetaArgPrefix = "&"

// VarArgSymbol is the symbol that indicates a variadic function argument in a
// function's list of formal arguments.  Functions may have at most one
// variadic argument.  Variadic arguments must be defined following optional
// arguments and are bound after all optional arguments are bound.
//
// Currently, it is an error to declare a function which has both variadic and
// keyword arguments.  While this may change it will always be discouraged due
// to the difficulty handling such mixtures of argument types.  It would be
// better to define separate functions, one with keyward args and the other
// with variadic args.
const VarArgSymbol = "&rest"

// OptArgSymbol is the symbol used to indicate optional arguments to a
// function.  Optional arguments are bound to given arguments if there are
// arguments left over following the binding of required arguments, otherwise
// they are bound to nil.
const OptArgSymbol = "&optional"

// KeyArgSymbol is the symbol used to indicate keyword arguments to a function.
// Keyword arguments are declared following optional arguments.  Keyword
// arguments may be supplied in any order and must specify the symbol they wish
// should be bound to by preceding the argument value with a keyward symbol.
const KeyArgSymbol = "&key"

// AnonArgSymbolPrefix is used to indicate unnamed arguments in the anonymous
// function shorthand ``(expr ...)''.
const AnonArgSymbolPrefix = "%"
