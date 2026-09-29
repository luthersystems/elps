// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
)

// prescanDefinition reads declaration events rather than decoding builtin
// definition syntax. Definitions are registered before exports in prescan;
// nested package definitions have already been selected by PackageForms.
func (a *analyzer) prescanDefinition(expr *lisp.LVal, scope *Scope, pkg string) {
	w := &lisp.CodeWalker{SourceAnalysis: true, Visit: func(n *lisp.WalkNode) bool {
		if n.Event == lisp.WalkForm {
			return n.Depth == 0
		}
		if n.Event != lisp.WalkDefine || n.Owner != expr {
			return true
		}
		kind := SymFunction
		switch n.Op {
		case "defmacro":
			kind = SymMacro
		case "deftype":
			kind = SymType
		}
		if kind != SymType && n.Formals.Type != lisp.LSExpr {
			return true
		}
		if kind == SymType && scope.LookupLocalInPackage(n.Node.Str, pkg) != nil {
			return true
		}
		sym := &Symbol{Name: n.Node.Str, Package: pkg, Kind: kind, Source: astutil.SymbolLoc(n.Node), Node: n.Node}
		if kind != SymType {
			sym.Signature = signatureFromFormals(n.Formals)
			sym.DocString = DefunDocstring(expr)
		}
		scope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
		return true
	}}
	w.Walk(expr)
}

// analyzeExpr owns resolution policy; CodeWalker owns the grammar. The scope
// stack follows walker events, including temporary selections of the outer
// scope for parallel initializers and flet closures.
func (a *analyzer) analyzeExpr(node *lisp.LVal, scope *Scope, currentPkg string) {
	var scopes []*Scope
	var calls []bool
	w := &lisp.CodeWalker{SourceAnalysis: true, BindingForm: a.bindingForm}
	w.Visit = func(n *lisp.WalkNode) bool {
		switch n.Event {
		case lisp.WalkForm:
			calls = append(calls, false)
			if n.Op != "" {
				return true
			}
			head := astutil.HeadSymbol(n.Node)
			switch head {
			case "in-package", "use-package":
				return false
			case "set":
				// set is an ordinary function with package-writing semantics, not a
				// lexical binding form. Preserve its post-value declaration timing.
				a.analyzeSet(n.Node, scope, currentPkg)
				return false
			}
			if strings.HasSuffix(head, ":deftype") && astutil.ArgCount(n.Node) >= 1 && n.Node.Cells[1].Type == lisp.LString {
				// Schema-style string declarations are library calls, not ELPS forms.
				a.analyzeStringDeftype(n.Node, scope, currentPkg)
				return false
			}
			if a.bindingForm(n.Node) != nil {
				return true
			}
			descend, opaque := a.visitCall(n.Node, scope, currentPkg)
			calls[len(calls)-1] = opaque
			return descend
		case lisp.WalkEnd:
			if calls[len(calls)-1] {
				a.insideMacroCall--
			}
			calls = calls[:len(calls)-1]
		case lisp.WalkEnter:
			scopes = append(scopes, scope)
			if n.Outer {
				scope = scope.Parent
			}
			if n.Node == nil {
				return true
			}
			kind := ScopeFunction
			switch n.Op {
			case "lambda", "expr":
				kind = ScopeLambda
			case "let", "let*", "test-let", "test-let*":
				kind = ScopeLet
			case "dotimes":
				kind = ScopeDotimes
			case "flet", "labels":
				if !n.Function {
					kind = ScopeFlet
				}
			case "macrolet":
				kind = ScopeMacrolet
			case "test":
				return true // test bodies have no lexical analysis scope
			}
			scope = NewScope(kind, scope, n.Node)
		case lisp.WalkLeave:
			scope = scopes[len(scopes)-1]
			scopes = scopes[:len(scopes)-1]
		case lisp.WalkBind:
			sym := &Symbol{Name: n.Node.Str, Kind: SymVariable, Source: astutil.SymbolLoc(n.Node), Node: n.Node, Init: n.Init}
			if n.Function {
				sym.Kind = SymParameter
				if n.Op == "expr" {
					sym.Source = nil
					sym.Node = nil
				}
			} else if n.Formals != nil {
				sym.Kind = SymFunction
				if n.Op == "macrolet" {
					sym.Kind = SymMacro
				} else {
					sym.Signature = signatureFromFormals(n.Formals)
				}
			}
			scope.Define(sym)
			a.result.Symbols = append(a.result.Symbols, sym)
		case lisp.WalkDefine:
			a.defineWalkSymbol(n, scope, currentPkg)
		case lisp.WalkRef, lisp.WalkSet:
			a.resolveSymbol(n.Node, scope, currentPkg)
		case lisp.WalkData:
			if n.Template && !n.Node.IsQuoted() {
				a.resolveTemplateSymbol(n.Node, scope, currentPkg)
			}
		case lisp.WalkLiteral:
		}
		return true
	}
	w.Walk(node)
}

// bindingForm supplies only application-defined grammar. Builtin forms have
// already been classified by CodeWalker. Explicit call policies take priority
// over Config.DefForms, as they did in the analyzer's original dispatch.
func (a *analyzer) bindingForm(node *lisp.LVal) *lisp.CodeBinding {
	switch astutil.HeadSymbol(node) {
	case "set", "in-package", "use-package", "export", "lisp:export", "thread-first", "thread-last":
		return nil
	}
	if strings.HasSuffix(astutil.HeadSymbol(node), ":deftype") && astutil.ArgCount(node) >= 1 && node.Cells[1].Type == lisp.LString {
		return nil
	}
	match, ok := defLikeMatch(node, a.cfg)
	if !ok {
		return nil
	}
	nameIdx := -1
	if match.bindsName && match.nameIdx > 0 && match.nameIdx < len(node.Cells) && node.Cells[match.nameIdx].Type == lisp.LSymbol && !node.Cells[match.nameIdx].IsQuoted() {
		nameIdx = match.nameIdx
	}
	return &lisp.CodeBinding{NameIndex: nameIdx, FormalsIndex: match.formalsIdx}
}

func (a *analyzer) defineWalkSymbol(n *lisp.WalkNode, scope *Scope, pkg string) {
	kind := SymFunction
	signature := false
	switch n.Op {
	case "defun", "defmacro":
		scope = a.root
		signature = n.Formals.Type == lisp.LSExpr
		if n.Op == "defmacro" {
			kind = SymMacro
		}
	case "deftype":
		kind = SymType
	default:
		match, _ := defLikeMatch(n.Owner, a.cfg)
		kind = match.nameKind
		if kind != SymVariable && kind != SymFunction && kind != SymMacro && kind != SymType {
			kind = SymFunction
		}
	}
	defPkg := packageForScope(scope, pkg)
	if scope.LookupLocalInPackage(n.Node.Str, defPkg) != nil {
		return
	}
	sym := &Symbol{Name: n.Node.Str, Package: defPkg, Kind: kind, Source: astutil.SymbolLoc(n.Node), Node: n.Node}
	if signature {
		sym.Signature = signatureFromFormals(n.Formals)
	}
	scope.Define(sym)
	a.result.Symbols = append(a.result.Symbols, sym)
}

// visitCall keeps expansion and opacity as resolution policy. Expansions are
// walked with another CodeWalker so the depth cap counts all nested expansions,
// not just a chain of heads. This also preserves MacroExpander/PanicReporter
// call counts and the original double reference to an unexpanded user macro.
func (a *analyzer) visitCall(node *lisp.LVal, scope *Scope, currentPkg string) (descend, opaque bool) {
	if node.Cells[0].Type == lisp.LSymbol {
		sym := scope.Lookup(node.Cells[0].Str)
		isMacro := sym != nil && sym.Kind == SymMacro && isUserMacro(sym)
		if isMacro {
			sym.References++
			a.result.References = append(a.result.References, &Reference{Symbol: sym, Source: astutil.SymbolLoc(node.Cells[0]), Node: node.Cells[0]})
		}
		if a.cfg != nil && a.cfg.MacroExpander != nil && (isMacro || sym == nil) && a.expansionDepth < maxMacroExpansionDepth {
			if expanded := a.cfg.MacroExpander.ExpandMacro(node, currentPkg); expanded != nil {
				a.insideMacroCall++
				a.expansionDepth++
				a.analyzeExpr(expanded, scope, currentPkg)
				a.expansionDepth--
				a.insideMacroCall--
				return false, false
			}
		}
		if isMacro {
			a.insideMacroCall++
			return true, true
		}
	}
	return true, false
}
