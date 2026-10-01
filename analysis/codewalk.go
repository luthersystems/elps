// Copyright © 2026 The ELPS authors

package analysis

import (
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/codewalk"
	"github.com/luthersystems/elps/lisp"
)

// prescanDefinition reads declaration events rather than decoding builtin
// definition syntax. Definitions are registered before exports in prescan;
// nested package definitions have already been selected by PackageForms.
func (a *analyzer) prescanDefinition(expr *lisp.LVal, scope *Scope, pkg string) {
	d := &a.definitions
	if d.a == nil {
		d.a = a
		d.walker.Visit = d.visit
		d.walker.DeclarationsOnly = true
	}
	d.expr, d.scope, d.pkg = expr, scope, pkg
	d.walker.Walk(expr)
}

type definitionScanner struct {
	a      *analyzer
	expr   *lisp.LVal
	scope  *Scope
	pkg    string
	walker codewalk.Walker
}

func (d *definitionScanner) visit(n *codewalk.Node) bool {
	if n.Event == lisp.WalkForm {
		return n.Depth == 0
	}
	if n.Event != lisp.WalkDefine || n.Owner != d.expr {
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
	if kind == SymType && d.scope.LookupLocalInPackage(n.Node.Str, d.pkg) != nil {
		return true
	}
	sym := &Symbol{Name: n.Node.Str, Package: d.pkg, Kind: kind, Source: astutil.SymbolLoc(n.Node), Node: n.Node}
	if kind != SymType {
		sym.Signature = signatureFromFormals(n.Formals)
		sym.DocString = DefunDocstring(d.expr)
	}
	d.scope.Define(sym)
	d.a.result.Symbols = append(d.a.result.Symbols, sym)
	return true
}

// analyzeExpr owns resolution policy; CodeWalker owns the grammar. The scope
// stack follows walker events, including temporary selections of the outer
// scope for parallel initializers and flet closures. With an expander, it
// returns the package after top-level forms, including progn and expansions.
func (a *analyzer) analyzeExpr(node *lisp.LVal, scope *Scope, currentPkg string) string {
	// Atomic set values and expansion results need no walker or visitor closure.
	if node == nil || node.IsQuoted() {
		return currentPkg
	}
	if node.Type == lisp.LSymbol {
		a.resolveSymbol(node, scope, currentPkg)
		return currentPkg
	}
	if node.Type != lisp.LSExpr || len(node.Cells) == 0 {
		return currentPkg
	}
	// Reuse one resolver/walker at each expansion or set recursion depth.
	// Method visitors avoid a fresh escaping closure and scope stack per form.
	if a.resolverDepth == len(a.resolvers) {
		r := &sourceResolver{a: a}
		r.walker.Visit, r.walker.BindingForm = r.visit, a.bindingForm
		r.walker.Reference = r.reference
		r.walker.Form = r.form
		r.walker.End, r.walker.SkipLiterals = r.end, true
		r.walker.EndDepth = &r.endDepth
		a.resolvers = append(a.resolvers, r)
	}
	r := a.resolvers[a.resolverDepth]
	a.resolverDepth++
	r.scope, r.pkg = scope, currentPkg
	r.scopes, r.opaqueCalls = r.scopes[:0], r.opaqueCalls[:0]
	r.endDepth = -1
	r.packageDepth = 0
	if a.cfg != nil && a.cfg.MacroExpander != nil {
		// Balance top-level progns as well as opaque calls. The no-expander
		// path keeps its selective End callbacks and historical package policy.
		r.walker.EndDepth = nil
	}
	r.walker.Walk(node)
	a.resolverDepth--
	return r.pkg
}

type sourceResolver struct {
	a            *analyzer
	scope        *Scope
	pkg          string
	scopes       []*Scope
	opaqueCalls  []int
	endDepth     int
	packageDepth int // depth of top-level forms, through transparent progns
	walker       codewalk.Walker
}

func (r *sourceResolver) reference(node *lisp.LVal) {
	r.a.resolveSymbol(node, r.scope, r.pkg)
}

func (r *sourceResolver) end(depth int) {
	if depth == r.packageDepth-1 {
		r.packageDepth--
	}
	if len(r.opaqueCalls) > 0 && r.opaqueCalls[len(r.opaqueCalls)-1] == depth {
		r.a.insideMacroCall--
		r.opaqueCalls = r.opaqueCalls[:len(r.opaqueCalls)-1]
		r.endDepth = -1
		if len(r.opaqueCalls) > 0 {
			r.endDepth = r.opaqueCalls[len(r.opaqueCalls)-1]
		}
	}
}

func (r *sourceResolver) form(node *lisp.LVal, op string, depth int) bool {
	if op != "" {
		return true
	}
	head := r.a.packageFormHead(node)
	switch head {
	case "progn", "lisp:progn":
		if r.a.cfg != nil && r.a.cfg.MacroExpander != nil {
			if depth == r.packageDepth {
				r.packageDepth++
			}
			return true
		}
	case "in-package":
		if depth == r.packageDepth && r.a.cfg != nil && r.a.cfg.MacroExpander != nil && astutil.ArgCount(node) >= 1 {
			if pkg := extractPackageName(node.Cells[1]); pkg != "" {
				r.pkg = pkg
			}
		}
		return false
	case "use-package":
		return false
	case "set":
		// set is an ordinary function with package-writing semantics, not a
		// lexical binding form. Preserve its post-value declaration timing.
		r.a.analyzeSet(node, r.scope, r.pkg)
		return false
	}
	if strings.HasSuffix(head, ":deftype") && astutil.ArgCount(node) >= 1 && node.Cells[1].Type == lisp.LString {
		// Schema-style string declarations are library calls, not ELPS forms.
		r.a.analyzeStringDeftype(node, r.scope, r.pkg)
		return false
	}
	if r.a.bindingForm(node) != nil {
		// Explicit application grammar wins. A name-based guess is only a
		// fallback when the expander cannot describe the call's actual code.
		if _, custom := customDefLikeMatch(node, r.a.cfg); custom || r.a.expand(node, r.scope, r.pkg) == nil {
			return true
		}
	}
	descend, opaque, pkg := r.a.visitCall(node, r.scope, r.pkg)
	if depth == r.packageDepth {
		r.pkg = pkg
	}
	if opaque {
		r.opaqueCalls = append(r.opaqueCalls, depth)
		r.endDepth = depth
	}
	return descend
}

func (r *sourceResolver) visit(n *codewalk.Node) bool {
	switch n.Event {
	case lisp.WalkForm:
		return r.form(n.Node, n.Op, n.Depth)
	case codewalk.End:
		r.end(n.Depth)
	case lisp.WalkEnter:
		r.scopes = append(r.scopes, r.scope)
		if n.Outer {
			r.scope = r.scope.Parent
		}
		if n.Node == nil {
			return true
		}
		kind := ScopeFunction
		switch n.Scope {
		case codewalk.ScopeAnonymous:
			kind = ScopeLambda
		case codewalk.ScopeLocal:
			kind = ScopeLet
		case codewalk.ScopeLoop:
			kind = ScopeDotimes
		case codewalk.ScopeFunctions:
			kind = ScopeFlet
		case codewalk.ScopeMacros:
			kind = ScopeMacrolet
		case codewalk.ScopeTest:
			return true // test bodies have no lexical analysis scope
		case codewalk.ScopeFunction:
		}
		r.scope = NewScope(kind, r.scope, n.Node)
	case lisp.WalkLeave:
		r.scope = r.scopes[len(r.scopes)-1]
		r.scopes = r.scopes[:len(r.scopes)-1]
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
		r.scope.Define(sym)
		r.a.result.Symbols = append(r.a.result.Symbols, sym)
	case lisp.WalkDefine:
		r.a.defineWalkSymbol(n, r.scope, r.pkg)
	case lisp.WalkRef, lisp.WalkSet:
		r.a.resolveSymbol(n.Node, r.scope, r.pkg)
	case lisp.WalkData:
		if n.Template && !n.Node.IsQuoted() {
			r.a.resolveTemplateSymbol(n.Node, r.scope, r.pkg)
		}
	case lisp.WalkLiteral, codewalk.FormalsOccurrence:
	}
	return true
}

// bindingForm supplies only application-defined grammar. Builtin forms have
// already been classified by CodeWalker. Explicit call policies take priority
// over Config.DefForms, as they did in the analyzer's original dispatch.
func (a *analyzer) bindingForm(node *lisp.LVal) *codewalk.Binding {
	head := astutil.HeadSymbol(node)
	// Without application grammar only the def-like naming heuristic can
	// match. Ordinary calls need none of its validation or repeated head reads.
	if (a.cfg == nil || len(a.cfg.DefForms) == 0) && !strings.HasPrefix(head, "def") {
		return nil
	}
	switch head {
	case "set", "in-package", "use-package", "export", "lisp:export", "thread-first", "thread-last":
		return nil
	}
	if strings.HasSuffix(head, ":deftype") && astutil.ArgCount(node) >= 1 && node.Cells[1].Type == lisp.LString {
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
	return &codewalk.Binding{NameIndex: nameIdx, FormalsIndex: match.formalsIdx}
}

func (a *analyzer) defineWalkSymbol(n *codewalk.Node, scope *Scope, pkg string) {
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
// not just a chain of heads. Prescan shares the expansion cache, and opaque
// calls retain the original double reference to an unexpanded user macro.
func (a *analyzer) visitCall(node *lisp.LVal, scope *Scope, currentPkg string) (descend, opaque bool, pkg string) {
	if node.Cells[0].Type == lisp.LSymbol {
		sym := scope.Lookup(node.Cells[0].Str)
		isMacro := sym != nil && sym.Kind == SymMacro && isUserMacro(sym)
		// An imported macro without a declaration location is referenced
		// here only when it expands; otherwise the ordinary traversal
		// references its head, as it does without an expander.
		importedOnly := false
		if a.cfg != nil && a.cfg.MacroExpander != nil {
			sym = scope.LookupInPackage(node.Cells[0].Str, currentPkg)
			isMacro = sym != nil && sym.Kind == SymMacro && isUserMacro(sym)
			importedOnly = !isMacro && isExpansionMacro(sym)
		}
		if isMacro || importedOnly {
			sym.References++
			a.result.References = append(a.result.References, newReference(sym, node.Cells[0]))
		}
		if pkg, ok := a.analyzeExpansion(node, scope, currentPkg); ok {
			return false, false, pkg
		}
		if importedOnly {
			// analyzeExpansion analyzed nothing, so the reference just
			// added is still the last one.
			sym.References--
			a.result.References = a.result.References[:len(a.result.References)-1]
		}
		if isMacro {
			a.insideMacroCall++
			return true, true, currentPkg
		}
	}
	return true, false, currentPkg
}

// analyzeExpansion analyzes node's macro expansion in its place, recording
// node as the origin of the definitions the expansion makes. It reports
// the package after the expansion, and false when node does not expand.
func (a *analyzer) analyzeExpansion(node *lisp.LVal, scope *Scope, currentPkg string) (string, bool) {
	expanded := a.expand(node, scope, currentPkg)
	if expanded == nil {
		return currentPkg, false
	}
	pkg := currentPkg
	a.withOrigin(node, currentPkg, func() {
		a.insideMacroCall++
		a.expansionDepth++
		pkg = a.analyzeExpr(expanded, scope, currentPkg)
		a.expansionDepth--
		a.insideMacroCall--
	})
	return pkg, true
}

// analyzeSet keeps package declaration policy separate from lexical syntax.
// Analysis historically visits only the target/value, omitting the head and
// extra arguments; an ordinary CodeWalker call would change its references.
func (a *analyzer) analyzeSet(node *lisp.LVal, scope *Scope, currentPkg string) {
	if astutil.ArgCount(node) < 2 {
		return
	}
	// set evaluates both arguments in the current lexical scope. Only a
	// quoted symbol target also identifies a static package binding below.
	a.analyzeExpr(node.Cells[1], scope, currentPkg)
	a.analyzeExpr(node.Cells[2], scope, currentPkg)

	name := extractSetSymbolName(node.Cells[1])
	if name == "" {
		return
	}

	// At runtime, set always calls PutGlobal — it writes to the package-
	// level scope regardless of where the call appears. Model this by:
	// - In non-global scopes: treat as a reference to the existing global
	//   binding (or create one in the root scope if none exists).
	// - In global scope: create or overwrite in the current scope (existing
	//   behavior, matches prescan).
	if scope.Kind != ScopeGlobal {
		// Look up in the root (global) scope specifically — set always
		// targets the package scope via PutGlobal, so let/lambda-local
		// bindings with the same name should not be found here.
		if existing := a.root.LookupLocalInPackage(name, currentPkg); existing != nil {
			existing.References++
			a.result.References = append(a.result.References, &Reference{
				Symbol: existing,
				Source: setTargetLoc(node.Cells[1]),
				Node:   extractSetSymbolNode(node.Cells[1]),
			})
			return
		}
		// No existing global — set will create it at package scope.
		sym := &Symbol{
			Name:    name,
			Package: currentPkg,
			Kind:    SymVariable,
			Source:  setTargetLoc(node.Cells[1]),
			Node:    extractSetSymbolNode(node.Cells[1]),
		}
		a.root.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
		return
	}

	// Global scope: define locally if not already present.
	defPkg := packageForScope(scope, currentPkg)
	if scope.LookupLocalInPackage(name, defPkg) == nil {
		sym := &Symbol{
			Name:    name,
			Package: defPkg,
			Kind:    SymVariable,
			Source:  setTargetLoc(node.Cells[1]),
			Node:    extractSetSymbolNode(node.Cells[1]),
		}
		scope.Define(sym)
		a.result.Symbols = append(a.result.Symbols, sym)
	}
}
