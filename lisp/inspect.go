// Copyright © 2024 The ELPS authors

package lisp

import "github.com/luthersystems/elps/parser/token"

// ParamKind classifies a function parameter.
type ParamKind int

const (
	ParamRequired ParamKind = iota
	ParamOptional
	ParamRest
	ParamKey
)

func (k ParamKind) String() string {
	switch k {
	case ParamRequired:
		return "required"
	case ParamOptional:
		return "optional"
	case ParamRest:
		return "rest"
	case ParamKey:
		return "key"
	default:
		return "unknown"
	}
}

// ParamInfo describes a single parameter in a function signature.
type ParamInfo struct {
	Name string
	Kind ParamKind
}

// FunctionInfo holds metadata extracted from a defun, defmacro, or lambda
// s-expression.
type FunctionInfo struct {
	Name      string           // empty for lambda
	Kind      string           // "defun", "defmacro", or "lambda"
	Params    []ParamInfo
	DocString string
	Source    *token.Location
}

// InspectFunction extracts metadata from a defun, defmacro, or lambda
// s-expression. Returns nil if node is not a recognized form.
func InspectFunction(node *LVal) *FunctionInfo {
	if node == nil || node.Type != LSExpr || node.Quoted || len(node.Cells) == 0 {
		return nil
	}

	head := node.Cells[0]
	if head.Type != LSymbol {
		return nil
	}

	info := &FunctionInfo{
		Kind:   head.Str,
		Source: node.Source,
	}

	switch head.Str {
	case "defun", "defmacro":
		// (defun name (formals) [docstring] body...)
		if len(node.Cells) < 3 {
			return nil
		}
		nameVal := node.Cells[1]
		if nameVal.Type != LSymbol {
			return nil
		}
		info.Name = nameVal.Str

		formalsVal := node.Cells[2]
		if formalsVal.Type != LSExpr {
			return nil
		}
		info.Params = ParseFormals(formalsVal)

		// Check for docstring: next cell after formals, if it's a string
		// and there's at least one more cell after it (the body).
		if len(node.Cells) > 4 && node.Cells[3].Type == LString {
			info.DocString = node.Cells[3].Str
		}

	case "lambda":
		// (lambda (formals) body...)
		if len(node.Cells) < 2 {
			return nil
		}
		formalsVal := node.Cells[1]
		if formalsVal.Type != LSExpr {
			return nil
		}
		info.Params = ParseFormals(formalsVal)

		// Check for docstring: (lambda (formals) "doc" body...)
		if len(node.Cells) > 3 && node.Cells[2].Type == LString {
			info.DocString = node.Cells[2].Str
		}

	default:
		return nil
	}

	return info
}

// ParseFormals extracts parameter info from a formals LVal. The formals
// list uses &optional, &rest, and &key markers to separate parameter kinds.
func ParseFormals(formals *LVal) []ParamInfo {
	if formals == nil || formals.Type != LSExpr {
		return nil
	}

	var params []ParamInfo
	mode := ParamRequired
	expectRestName := false

	for _, sym := range formals.Cells {
		if sym.Type != LSymbol {
			continue
		}
		switch sym.Str {
		case "&optional":
			mode = ParamOptional
		case "&rest":
			expectRestName = true
		case "&key":
			mode = ParamKey
		default:
			if expectRestName {
				params = append(params, ParamInfo{Name: sym.Str, Kind: ParamRest})
				expectRestName = false
			} else {
				params = append(params, ParamInfo{Name: sym.Str, Kind: mode})
			}
		}
	}

	return params
}
