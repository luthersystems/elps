// Copyright © 2024 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// analyzer is the internal state for a single analysis run.
type analyzer struct {
	root   *Scope
	result *Result
	cfg    *Config
}

// prescan walks top-level expressions to register forward-referenceable
// definitions (defun, defmacro, set, export). It runs in two phases so
// that (export 'name) works regardless of source order — a common ELPS
// convention is to place exports before the corresponding defun.
func (a *analyzer) prescan(exprs []*lisp.LVal, scope *Scope) {
	// Phase 1: Register all definitions.
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
		case "deftype":
			a.prescanDeftype(expr, scope)
		case "set":
			a.prescanSet(expr, scope)
		case "use-package":
			a.prescanUsePackage(expr, scope)
		case "in-package":
			a.prescanInPackage(expr, scope)
		}
	}
	// Phase 2: Apply exports (all definitions now exist in scope).
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		if astutil.HeadSymbol(expr) == "export" {
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

func (a *analyzer) prescanDeftype(expr *lisp.LVal, scope *Scope) {
	// (deftype name (constructor-formals) body...)
	if astutil.ArgCount(expr) < 2 {
		return
	}
	nameVal := expr.Cells[1]
	if nameVal.Type != lisp.LSymbol {
		return
	}
	if scope.LookupLocal(nameVal.Str) != nil {
		return
	}
	sym := &Symbol{
		Name:   nameVal.Str,
		Kind:   SymType,
		Source: nameVal.Source,
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

func (a *analyzer) prescanUsePackage(expr *lisp.LVal, scope *Scope) {
	if a.cfg == nil || a.cfg.PackageExports == nil {
		return
	}
	if astutil.ArgCount(expr) < 1 {
		return
	}
	pkgName := extractPackageName(expr.Cells[1])
	if pkgName == "" {
		return
	}
	syms, ok := a.cfg.PackageExports[pkgName]
	if !ok {
		return
	}
	for _, ext := range syms {
		// Don't overwrite locally defined symbols
		if scope.LookupLocal(ext.Name) != nil {
			continue
		}
		sym := &Symbol{
			Name:      ext.Name,
			Kind:      ext.Kind,
			Source:    ext.Source,
			Signature: ext.Signature,
			DocString: ext.DocString,
			Exported:  true,
			External:  true,
		}
		scope.Define(sym)
	}
}

func (a *analyzer) prescanInPackage(expr *lisp.LVal, scope *Scope) {
	// When switching to a package, the "lisp" package is auto-imported
	// (runtime behavior from builtins.go:488). Also import the package
	// itself if we have its exports.
	if a.cfg == nil || a.cfg.PackageExports == nil {
		return
	}
	if astutil.ArgCount(expr) < 1 {
		return
	}
	pkgName := extractPackageName(expr.Cells[1])
	if pkgName == "" {
		return
	}
	// The "lisp" package is auto-imported when switching packages at runtime.
	for _, importPkg := range []string{"lisp", pkgName} {
		syms, ok := a.cfg.PackageExports[importPkg]
		if !ok {
			continue
		}
		for _, ext := range syms {
			if scope.LookupLocal(ext.Name) != nil {
				continue
			}
			sym := &Symbol{
				Name:      ext.Name,
				Kind:      ext.Kind,
				Source:    ext.Source,
				Signature: ext.Signature,
				DocString: ext.DocString,
				Exported:  true,
				External:  true,
			}
			scope.Define(sym)
		}
	}
}

// extractPackageName gets the package name from the first arg of use-package
// or in-package. Handles both quoted symbols ('testing) and strings ("testing").
func extractPackageName(arg *lisp.LVal) string {
	if arg.Type == lisp.LString {
		return arg.Str
	}
	if arg.Type == lisp.LSymbol {
		return arg.Str
	}
	// Quoted symbol: 'testing → LSExpr{Quoted: true, Cells: [LSymbol{testing}]}
	if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		return arg.Cells[0].Str
	}
	return ""
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
	case "deftype":
		a.analyzeDeftype(node, scope)
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
	case "test-let":
		a.analyzeTestLet(node, scope, false)
	case "test-let*":
		a.analyzeTestLet(node, scope, true)
	case "set":
		a.analyzeSet(node, scope)
	case "set!":
		a.analyzeSetBang(node, scope)
	case "quote":
		return // skip quoted data
	case "quasiquote":
		a.analyzeQuasiquote(node, scope)
		return
	case "in-package", "use-package", "export":
		return // package management, skip
	case "function":
		a.analyzeFunction(node, scope)
	case "lisp:expr", "expr":
		a.analyzePrefixLambda(node, scope)
	case "handler-bind":
		a.analyzeHandlerBind(node, scope)
	case "test":
		a.analyzeTest(node, scope)
	default:
		// Detect qualified deftype calls like (s:deftype "name" ...) where
		// the first arg is a string that becomes a global symbol binding.
		if strings.HasSuffix(head, ":deftype") && astutil.ArgCount(node) >= 1 &&
			node.Cells[1].Type == lisp.LString {
			a.analyzeStringDeftype(node, scope)
		} else if strings.HasPrefix(head, "def") {
			a.analyzeDefLike(node, scope)
		} else {
			a.analyzeCall(node, scope)
		}
	}
}

func (a *analyzer) analyzeDefun(node *lisp.LVal, scope *Scope, kind SymbolKind) {
	if astutil.ArgCount(node) < 2 {
		return
	}
	// Register the name in the enclosing scope if not already defined.
	// Prescan handles top-level definitions; this covers nested defun/defmacro
	// (e.g. inside test bodies, progn, when, etc.).
	nameVal := node.Cells[1]
	if nameVal.Type == lisp.LSymbol && scope.LookupLocal(nameVal.Str) == nil {
		formalsForSig := node.Cells[2]
		sym := &Symbol{
			Name:   nameVal.Str,
			Kind:   kind,
			Source: nameVal.Source,
		}
		if formalsForSig.Type == lisp.LSExpr {
			sym.Signature = signatureFromFormals(formalsForSig)
		}
		scope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}

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

func (a *analyzer) analyzeDeftype(node *lisp.LVal, scope *Scope) {
	// (deftype name (constructor-formals) constructor-body...)
	if astutil.ArgCount(node) < 2 {
		return
	}
	nameVal := node.Cells[1]
	// Register the type name in scope (prescan handles top-level; this
	// covers non-top-level deftype like inside test bodies).
	if nameVal.Type == lisp.LSymbol && scope.LookupLocal(nameVal.Str) == nil {
		sym := &Symbol{
			Name:   nameVal.Str,
			Kind:   SymType,
			Source: nameVal.Source,
		}
		scope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}
	formalsVal := node.Cells[2]
	if formalsVal.Type != lisp.LSExpr {
		return
	}
	bodyScope := NewScope(ScopeFunction, scope, node)
	a.addParams(formalsVal, bodyScope)
	for i := 3; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], bodyScope)
	}
}

// analyzeStringDeftype handles calls like (s:deftype "name" type-expr ...)
// where the string literal first argument creates a global symbol binding.
func (a *analyzer) analyzeStringDeftype(node *lisp.LVal, scope *Scope) {
	if astutil.ArgCount(node) < 1 {
		return
	}
	nameVal := node.Cells[1]
	if nameVal.Type != lisp.LString || nameVal.Str == "" {
		return
	}
	// Register the string value as a variable in the enclosing scope.
	sym := &Symbol{
		Name:   nameVal.Str,
		Kind:   SymVariable,
		Source: nameVal.Source,
	}
	scope.Define(sym)
	a.result.Symbols = append(a.result.Symbols, sym)
	// Analyze remaining arguments normally.
	for i := 2; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], scope)
	}
}

// analyzeDefLike handles unknown (def* ...) forms that aren't explicitly
// recognized (e.g. user-defined macros like defmethod, def-app-route).
// It scans children for the first formals list (an LSExpr whose contents
// are all symbols), creates a scope with those as parameters, and analyzes
// the remaining children as body in that scope.
func (a *analyzer) analyzeDefLike(node *lisp.LVal, scope *Scope) {
	// Find the first child (after head) that looks like a formals list.
	formalsIdx := -1
	for i := 1; i < len(node.Cells); i++ {
		if isFormalsLike(node.Cells[i]) {
			formalsIdx = i
			break
		}
	}
	// Also check for empty formals () at the defun-like position (index 2).
	// Empty () could be nil in a regular call, so only treat it as formals
	// when preceded by a symbol name (defun-like pattern: def* name () body).
	if formalsIdx < 0 && len(node.Cells) > 2 &&
		node.Cells[1].Type == lisp.LSymbol && !node.Cells[1].Quoted &&
		isEmptyFormals(node.Cells[2]) {
		formalsIdx = 2
	}
	if formalsIdx < 0 {
		// No formals detected — fall back to normal call analysis.
		a.analyzeCall(node, scope)
		return
	}

	// Resolve the head symbol — unlike built-in defun/defmacro, def-like
	// forms are calls to user-defined macros/functions.
	a.analyzeExpr(node.Cells[0], scope)

	// When formals are at index 2, child[1] is the definition name (like
	// defun). Register it in scope rather than resolving it as a reference.
	// e.g. (defendpoint my-name (req) body...)
	nameIdx := -1
	if formalsIdx == 2 && node.Cells[1].Type == lisp.LSymbol && !node.Cells[1].Quoted {
		nameIdx = 1
		nameVal := node.Cells[1]
		if scope.LookupLocal(nameVal.Str) == nil {
			sym := &Symbol{
				Name:   nameVal.Str,
				Kind:   SymFunction,
				Source: nameVal.Source,
			}
			scope.Define(sym)
			a.result.Symbols = append(a.result.Symbols, sym)
		}
	}

	// Analyze children before the formals in the outer scope (skip head and name).
	for i := 1; i < formalsIdx; i++ {
		if i == nameIdx {
			continue
		}
		a.analyzeExpr(node.Cells[i], scope)
	}

	// Create a function scope with the formals as parameters.
	bodyScope := NewScope(ScopeFunction, scope, node)
	a.addParams(node.Cells[formalsIdx], bodyScope)

	// Analyze body (everything after formals) in the new scope.
	for i := formalsIdx + 1; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], bodyScope)
	}
}

// isFormalsLike returns true if the node looks like a parameter list:
// a non-empty, non-quoted LSExpr where every child is a symbol (including
// &optional, &rest, &key markers).
func isFormalsLike(node *lisp.LVal) bool {
	if node.Type != lisp.LSExpr || node.Quoted || len(node.Cells) == 0 {
		return false
	}
	for _, child := range node.Cells {
		if child.Type != lisp.LSymbol {
			return false
		}
	}
	return true
}

// isEmptyFormals returns true if the node is an empty, non-quoted
// parenthesized list (), representing a zero-argument formals list.
func isEmptyFormals(node *lisp.LVal) bool {
	return node.Type == lisp.LSExpr && !node.Quoted && len(node.Cells) == 0
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

func (a *analyzer) analyzeTest(node *lisp.LVal, scope *Scope) {
	// (test "name" body...)
	if astutil.ArgCount(node) < 1 {
		return
	}
	// Prescan body for forward-referenceable definitions (defun, defmacro, set).
	body := node.Cells[2:]
	a.prescan(body, scope)
	for _, expr := range body {
		a.analyzeExpr(expr, scope)
	}
}

func (a *analyzer) analyzeTestLet(node *lisp.LVal, scope *Scope, sequential bool) {
	// (test-let "name" ((x 1) (y 2)) body...)
	// arg[1] = test name string (skip), arg[2] = bindings, rest = body
	if astutil.ArgCount(node) < 2 {
		return
	}
	bindings := node.Cells[2]
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

	// Walk body (after name string and bindings)
	for i := 3; i < len(node.Cells); i++ {
		a.analyzeExpr(node.Cells[i], letScope)
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
	// handler-bind form: (handler-bind ((cond-type handler) ...) body...)
	// The bindings list (Cells[1]) contains pairs where the first element
	// is a condition type name (data, not a variable reference) and the
	// second is a handler (usually a lambda). We must skip resolving the
	// condition type names to avoid false "undefined symbol" reports.
	if astutil.ArgCount(node) < 1 {
		return
	}
	bindings := node.Cells[1]
	if bindings.Type == lisp.LSExpr && !bindings.Quoted {
		for _, clause := range bindings.Cells {
			if clause.Type != lisp.LSExpr || len(clause.Cells) < 2 {
				continue
			}
			// Skip clause.Cells[0] — it's a condition type name, not code.
			// Analyze the handler (clause.Cells[1]) and any remaining forms.
			for _, child := range clause.Cells[1:] {
				a.analyzeExpr(child, scope)
			}
		}
	}
	// Analyze body forms
	for _, child := range node.Cells[2:] {
		a.analyzeExpr(child, scope)
	}
}

func (a *analyzer) analyzePrefixLambda(node *lisp.LVal, scope *Scope) {
	// (lisp:expr body) — #^body parses to this form.
	// The body uses implicit % params: %, %1, %2, %&rest, %&optional.
	if astutil.ArgCount(node) < 1 {
		return
	}
	body := node.Cells[1]

	// Scan the body for %-prefixed symbols to determine which params exist.
	params := make(map[string]bool)
	collectPercentParams(body, params)

	lambdaScope := NewScope(ScopeLambda, scope, node)
	for name := range params {
		sym := &Symbol{
			Name: name,
			Kind: SymParameter,
		}
		lambdaScope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}

	a.analyzeExpr(body, lambdaScope)
}

// collectPercentParams walks an expression tree and collects all %-prefixed
// symbol names (implicit parameters for #^ prefix lambda forms).
func collectPercentParams(node *lisp.LVal, params map[string]bool) {
	if node == nil {
		return
	}
	if node.Type == lisp.LSymbol && !node.Quoted && strings.HasPrefix(node.Str, "%") {
		params[node.Str] = true
		return
	}
	if node.Type == lisp.LSExpr && node.Quoted {
		return // quoted data, skip
	}
	for _, child := range node.Cells {
		collectPercentParams(child, params)
	}
}

func (a *analyzer) analyzeQuasiquote(node *lisp.LVal, scope *Scope) {
	// Quasiquote bodies are template data. Only (unquote expr) and
	// (unquote-splicing expr) contain code that should be analyzed.
	if astutil.ArgCount(node) < 1 {
		return
	}
	a.walkQuasiquoteTemplate(node.Cells[1], scope)
}

// walkQuasiquoteTemplate walks a quasiquote template, only analyzing
// expressions inside unquote and unquote-splicing forms.
func (a *analyzer) walkQuasiquoteTemplate(node *lisp.LVal, scope *Scope) {
	if node == nil {
		return
	}
	if node.Type != lisp.LSExpr || len(node.Cells) == 0 {
		return
	}
	// Quoted lists (bracket expressions [...]) in quasiquote templates can
	// still contain (unquote ...) forms, so we must recurse into them.
	if node.Quoted {
		for _, child := range node.Cells {
			a.walkQuasiquoteTemplate(child, scope)
		}
		return
	}
	head := astutil.HeadSymbol(node)
	if head == "unquote" || head == "unquote-splicing" {
		// The children of unquote/unquote-splicing are code, not template data.
		for _, child := range node.Cells[1:] {
			a.analyzeExpr(child, scope)
		}
		return
	}
	// Otherwise this is template data — recurse looking for nested unquotes.
	for _, child := range node.Cells {
		a.walkQuasiquoteTemplate(child, scope)
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

	// Handle qualified symbols (contain :)
	for i := 1; i < len(name); i++ {
		if name[i] == ':' {
			a.resolveQualifiedSymbol(node, scope, name[:i], name[i+1:])
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

// resolveQualifiedSymbol handles a qualified symbol like "pkg:sym".
// It looks up the unqualified name in PackageExports for the given package,
// and if found, records a reference using the unqualified symbol name (matching
// the convention used by prescanUsePackage).
func (a *analyzer) resolveQualifiedSymbol(node *lisp.LVal, scope *Scope, pkgName, symName string) {
	if a.cfg == nil || a.cfg.PackageExports == nil {
		return
	}

	exports, ok := a.cfg.PackageExports[pkgName]
	if !ok {
		return
	}

	// Find the exported symbol definition.
	var ext *ExternalSymbol
	for i := range exports {
		if exports[i].Name == symName {
			ext = &exports[i]
			break
		}
	}
	if ext == nil {
		return
	}

	// Check if the unqualified name is already in scope (e.g. from use-package).
	sym := scope.Lookup(symName)
	if sym == nil {
		// Create a symbol from the external definition and define it at root
		// scope so all references share the same Symbol pointer.
		sym = &Symbol{
			Name:      ext.Name,
			Kind:      ext.Kind,
			Source:    ext.Source,
			Signature: ext.Signature,
			DocString: ext.DocString,
			Exported:  true,
			External:  true,
		}
		a.result.RootScope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}

	sym.References++
	a.result.References = append(a.result.References, &Reference{
		Symbol: sym,
		Source: node.Source,
		Node:   node,
	})
}
