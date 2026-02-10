// Copyright © 2024 The ELPS authors

package analysis

import (
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// analyzer is the internal state for a single analysis run.
type analyzer struct {
	root   *Scope
	result *Result
}

// prescan walks top-level expressions to register forward-referenceable
// definitions (defun, defmacro, set, export).
func (a *analyzer) prescan(exprs []*lisp.LVal, scope *Scope) {
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		head := astutil.HeadSymbol(expr)
		switch head {
		case "defun":
			a.prescanDefun(expr, scope, SymFunction)
		case "defmacro":
			a.prescanDefun(expr, scope, SymMacro)
		case "set":
			a.prescanSet(expr, scope)
		case "export":
			a.prescanExport(expr, scope)
		}
	}
}

func (a *analyzer) prescanDefun(expr *lisp.LVal, scope *Scope, kind SymbolKind) {
	if astutil.ArgCount(expr) < 2 {
		return
	}
	nameVal := expr.Cells[1]
	if nameVal.Type != lisp.LSymbol {
		return
	}
	formalsVal := expr.Cells[2]
	if formalsVal.Type != lisp.LSExpr {
		return
	}

	// Extract docstring
	var docStr string
	if astutil.ArgCount(expr) >= 3 && expr.Cells[3].Type == lisp.LString {
		// Has body after docstring
		if astutil.ArgCount(expr) >= 4 {
			docStr = expr.Cells[3].Str
		}
	}

	sym := &Symbol{
		Name:      nameVal.Str,
		Kind:      kind,
		Source:    nameVal.Source,
		Signature: signatureFromFormals(formalsVal),
		DocString: docStr,
	}
	scope.Define(sym)
	a.result.Symbols = append(a.result.Symbols, sym)
}

func (a *analyzer) prescanSet(expr *lisp.LVal, scope *Scope) {
	if astutil.ArgCount(expr) < 1 {
		return
	}
	name := extractSetSymbolName(expr.Cells[1])
	if name == "" {
		return
	}
	// Only define if not already in scope (prescan doesn't overwrite)
	if scope.LookupLocal(name) != nil {
		return
	}
	sym := &Symbol{
		Name:   name,
		Kind:   SymVariable,
		Source: expr.Cells[1].Source,
	}
	scope.Define(sym)
	a.result.Symbols = append(a.result.Symbols, sym)
}

func (a *analyzer) prescanExport(expr *lisp.LVal, scope *Scope) {
	for _, arg := range expr.Cells[1:] {
		name := ""
		if arg.Type == lisp.LSymbol {
			name = arg.Str
		} else if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
			name = arg.Cells[0].Str
		}
		if name == "" {
			continue
		}
		if sym := scope.LookupLocal(name); sym != nil {
			sym.Exported = true
		}
	}
}

// extractSetSymbolName extracts the symbol name from the first arg of set.
// Handles both (set 'name value) and (set name value).
func extractSetSymbolName(arg *lisp.LVal) string {
	if arg.Type == lisp.LSymbol {
		return arg.Str
	}
	// Quoted symbol: 'name parses as LSExpr{Quoted: true, Cells: [LSymbol{name}]}
	if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		return arg.Cells[0].Str
	}
	return ""
}

// analyzeExpr recursively walks an expression, building scopes and
// tracking symbol references.
func (a *analyzer) analyzeExpr(node *lisp.LVal, scope *Scope) {
	if node == nil {
		return
	}

	switch node.Type {
	case lisp.LSymbol:
		if !node.Quoted {
			a.resolveSymbol(node, scope)
		}
		return
	case lisp.LSExpr:
		if node.Quoted {
			return // data, not code
		}
		if len(node.Cells) == 0 {
			return // nil
		}
	default:
		return // literals
	}

	head := astutil.HeadSymbol(node)
	switch head {
	case "defun":
		a.analyzeDefun(node, scope, SymFunction)
	case "defmacro":
		a.analyzeDefun(node, scope, SymMacro)
	case "lambda":
		a.analyzeLambda(node, scope)
	case "let":
		a.analyzeLet(node, scope, false)
	case "let*":
		a.analyzeLet(node, scope, true)
	case "flet":
		a.analyzeFlet(node, scope, false)
	case "labels":
		a.analyzeFlet(node, scope, true)
	case "dotimes":
		a.analyzeDotimes(node, scope)
	case "set":
		a.analyzeSet(node, scope)
	case "set!":
		a.analyzeSetBang(node, scope)
	case "quote":
		return // skip quoted data
	case "in-package", "use-package", "export":
		return // package management, skip
	case "function":
		a.analyzeFunction(node, scope)
	case "handler-bind":
		a.analyzeHandlerBind(node, scope)
	default:
		a.analyzeCall(node, scope)
	}
}

func (a *analyzer) analyzeDefun(node *lisp.LVal, scope *Scope, kind SymbolKind) {
	if astutil.ArgCount(node) < 2 {
		return
	}
	// Name was already registered in prescan; just record a reference for it.
	formalsVal := node.Cells[2]
	if formalsVal.Type != lisp.LSExpr {
		return
	}

	scopeKind := ScopeFunction
	if kind == SymMacro {
		scopeKind = ScopeFunction // macros also use function scope
	}
	bodyScope := NewScope(scopeKind, scope, node)

	// Add parameters to body scope
	a.addParams(formalsVal, bodyScope)

	// Walk body (skip head, name, formals, and optional docstring)
	bodyStart := 3
	if astutil.ArgCount(node) >= 3 && node.Cells[3].Type == lisp.LString {
		if astutil.ArgCount(node) >= 4 {
			bodyStart = 4
		}
	}
	for i := bodyStart; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], bodyScope)
	}
}

func (a *analyzer) analyzeLambda(node *lisp.LVal, scope *Scope) {
	if astutil.ArgCount(node) < 1 {
		return
	}
	formalsVal := node.Cells[1]
	if formalsVal.Type != lisp.LSExpr {
		return
	}

	bodyScope := NewScope(ScopeLambda, scope, node)
	a.addParams(formalsVal, bodyScope)

	// Walk body (skip head, formals, optional docstring)
	bodyStart := 2
	if astutil.ArgCount(node) >= 2 && node.Cells[2].Type == lisp.LString {
		if astutil.ArgCount(node) >= 3 {
			bodyStart = 3
		}
	}
	for i := bodyStart; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], bodyScope)
	}
}

func (a *analyzer) analyzeLet(node *lisp.LVal, scope *Scope, sequential bool) {
	if astutil.ArgCount(node) < 1 {
		return
	}
	bindings := node.Cells[1]
	if bindings.Type != lisp.LSExpr {
		return
	}

	letScope := NewScope(ScopeLet, scope, node)

	for _, binding := range bindings.Cells {
		if binding.Type != lisp.LSExpr || len(binding.Cells) < 2 {
			continue
		}
		nameVal := binding.Cells[0]
		valueVal := binding.Cells[1]

		// For let*, values see prior bindings; for let, values see parent scope
		if sequential {
			a.analyzeExpr(valueVal, letScope)
		} else {
			a.analyzeExpr(valueVal, scope)
		}

		if nameVal.Type == lisp.LSymbol {
			sym := &Symbol{
				Name:   nameVal.Str,
				Kind:   SymVariable,
				Source: nameVal.Source,
			}
			letScope.Define(sym)
			a.result.Symbols = append(a.result.Symbols, sym)
		}
	}

	// Walk body
	for i := 2; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], letScope)
	}
}

func (a *analyzer) analyzeFlet(node *lisp.LVal, scope *Scope, labels bool) {
	if astutil.ArgCount(node) < 1 {
		return
	}
	bindings := node.Cells[1]
	if bindings.Type != lisp.LSExpr {
		return
	}

	fletScope := NewScope(ScopeFlet, scope, node)

	// For labels: define all names first, then analyze bodies in shared scope
	if labels {
		for _, binding := range bindings.Cells {
			if binding.Type != lisp.LSExpr || len(binding.Cells) < 2 {
				continue
			}
			nameVal := binding.Cells[0]
			if nameVal.Type != lisp.LSymbol {
				continue
			}
			formalsVal := binding.Cells[1]
			sym := &Symbol{
				Name:      nameVal.Str,
				Kind:      SymFunction,
				Source:    nameVal.Source,
				Signature: signatureFromFormals(formalsVal),
			}
			fletScope.Define(sym)
			a.result.Symbols = append(a.result.Symbols, sym)
		}
		for _, binding := range bindings.Cells {
			if binding.Type != lisp.LSExpr || len(binding.Cells) < 2 {
				continue
			}
			formalsVal := binding.Cells[1]
			if formalsVal.Type != lisp.LSExpr {
				continue
			}
			fnScope := NewScope(ScopeFunction, fletScope, binding)
			a.addParams(formalsVal, fnScope)
			for i := 2; i < len(binding.Cells); i++ {
				a.analyzeExpr(binding.Cells[i], fnScope)
			}
		}
	} else {
		// flet: function bodies use the parent scope (not fletScope)
		for _, binding := range bindings.Cells {
			if binding.Type != lisp.LSExpr || len(binding.Cells) < 2 {
				continue
			}
			nameVal := binding.Cells[0]
			if nameVal.Type != lisp.LSymbol {
				continue
			}
			formalsVal := binding.Cells[1]
			sym := &Symbol{
				Name:      nameVal.Str,
				Kind:      SymFunction,
				Source:    nameVal.Source,
				Signature: signatureFromFormals(formalsVal),
			}
			fletScope.Define(sym)
			a.result.Symbols = append(a.result.Symbols, sym)

			if formalsVal.Type == lisp.LSExpr {
				fnScope := NewScope(ScopeFunction, scope, binding)
				a.addParams(formalsVal, fnScope)
				for i := 2; i < len(binding.Cells); i++ {
					a.analyzeExpr(binding.Cells[i], fnScope)
				}
			}
		}
	}

	// Walk body
	for i := 2; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], fletScope)
	}
}

func (a *analyzer) analyzeDotimes(node *lisp.LVal, scope *Scope) {
	if astutil.ArgCount(node) < 1 {
		return
	}
	bindingList := node.Cells[1]
	if bindingList.Type != lisp.LSExpr || len(bindingList.Cells) < 2 {
		return
	}

	// Analyze the count expression in the outer scope
	a.analyzeExpr(bindingList.Cells[1], scope)

	dotimesScope := NewScope(ScopeDotimes, scope, node)
	varVal := bindingList.Cells[0]
	if varVal.Type == lisp.LSymbol {
		sym := &Symbol{
			Name:   varVal.Str,
			Kind:   SymVariable,
			Source: varVal.Source,
		}
		dotimesScope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}

	// Walk body
	for i := 2; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], dotimesScope)
	}
}

func (a *analyzer) analyzeSet(node *lisp.LVal, scope *Scope) {
	if astutil.ArgCount(node) < 2 {
		return
	}
	// Analyze the value expression
	a.analyzeExpr(node.Cells[2], scope)

	// For set, the name might be a new binding or overwrite
	name := extractSetSymbolName(node.Cells[1])
	if name == "" {
		return
	}
	// If not already defined at top level, define it
	if scope.LookupLocal(name) == nil {
		sym := &Symbol{
			Name:   name,
			Kind:   SymVariable,
			Source: node.Cells[1].Source,
		}
		scope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}
}

func (a *analyzer) analyzeSetBang(node *lisp.LVal, scope *Scope) {
	if astutil.ArgCount(node) < 2 {
		return
	}
	// set! takes an unquoted symbol
	target := node.Cells[1]
	if target.Type == lisp.LSymbol {
		a.resolveSymbol(target, scope)
	}
	// Analyze the value expression
	a.analyzeExpr(node.Cells[2], scope)
}

func (a *analyzer) analyzeFunction(node *lisp.LVal, scope *Scope) {
	// (function name) or #'name — resolve as function reference
	if astutil.ArgCount(node) < 1 {
		return
	}
	arg := node.Cells[1]
	if arg.Type == lisp.LSymbol {
		a.resolveSymbol(arg, scope)
	}
}

func (a *analyzer) analyzeHandlerBind(node *lisp.LVal, scope *Scope) {
	// Not a scope creator — walk all children in current scope
	for _, child := range node.Cells[1:] {
		a.analyzeExpr(child, scope)
	}
}

func (a *analyzer) analyzeCall(node *lisp.LVal, scope *Scope) {
	// Head can be symbol or expression
	for _, child := range node.Cells {
		a.analyzeExpr(child, scope)
	}
}

// addParams adds function parameter symbols to a scope.
func (a *analyzer) addParams(formalsVal *lisp.LVal, scope *Scope) {
	params := lisp.ParseFormals(formalsVal)
	for _, p := range params {
		sym := &Symbol{
			Name: p.Name,
			Kind: SymParameter,
		}
		// Try to find source location from the formals cells
		for _, cell := range formalsVal.Cells {
			if cell.Type == lisp.LSymbol && cell.Str == p.Name {
				sym.Source = cell.Source
				break
			}
		}
		scope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}
}

// resolveSymbol attempts to resolve a symbol reference in the given scope.
func (a *analyzer) resolveSymbol(node *lisp.LVal, scope *Scope) {
	if node.Type != lisp.LSymbol {
		return
	}
	name := node.Str

	// Skip keywords (start with :)
	if len(name) > 0 && name[0] == ':' {
		return
	}

	// Skip qualified symbols (contain :)
	for i := 1; i < len(name); i++ {
		if name[i] == ':' {
			return
		}
	}

	sym := scope.Lookup(name)
	if sym != nil {
		sym.References++
		a.result.References = append(a.result.References, &Reference{
			Symbol: sym,
			Source: node.Source,
			Node:   node,
		})
	} else {
		a.result.Unresolved = append(a.result.Unresolved, &UnresolvedRef{
			Name:   name,
			Source: node.Source,
			Node:   node,
		})
	}
}
