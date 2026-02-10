// Copyright © 2024 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// SymbolKind classifies a symbol definition.
type SymbolKind int

const (
	SymVariable  SymbolKind = iota // set, let binding
	SymFunction                    // defun, flet/labels binding
	SymMacro                       // defmacro
	SymParameter                   // function/lambda parameter
	SymSpecialOp                   // special operator (if, cond, etc.)
	SymBuiltin                     // builtin function
	SymType                        // deftype
)

func (k SymbolKind) String() string {
	switch k {
	case SymVariable:
		return "variable"
	case SymFunction:
		return "function"
	case SymMacro:
		return "macro"
	case SymParameter:
		return "parameter"
	case SymSpecialOp:
		return "special-op"
	case SymBuiltin:
		return "builtin"
	case SymType:
		return "type"
	default:
		return "unknown"
	}
}

// Symbol represents a defined name in a scope.
type Symbol struct {
	Name       string
	Kind       SymbolKind
	Source     *token.Location // nil for builtins
	Scope      *Scope
	Signature  *Signature // non-nil for callables
	DocString  string
	References int
	Exported   bool
}

// Signature describes the parameter signature of a callable symbol.
type Signature struct {
	Params []lisp.ParamInfo
}

// MinArity returns the minimum number of arguments required.
func (sig *Signature) MinArity() int {
	if sig == nil {
		return 0
	}
	count := 0
	for _, p := range sig.Params {
		if p.Kind == lisp.ParamRequired {
			count++
		}
	}
	return count
}

// MaxArity returns the maximum number of arguments accepted.
// Returns -1 for variadic functions (those with &rest or &key params).
func (sig *Signature) MaxArity() int {
	if sig == nil {
		return -1
	}
	for _, p := range sig.Params {
		if p.Kind == lisp.ParamRest || p.Kind == lisp.ParamKey {
			return -1
		}
	}
	return len(sig.Params)
}
