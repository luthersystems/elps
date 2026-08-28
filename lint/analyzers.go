// Copyright © 2024 The ELPS authors

package lint

import (
	"fmt"
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// AnalyzerSetUsage warns when `set` is used to reassign a symbol that was
// already bound by a prior `set` in the same file. The first `set` creating
// a binding is fine (ELPS has no `defvar`), but subsequent mutations of the
// same symbol should use `set!` to signal intent.
var AnalyzerSetUsage = &Analyzer{
	Name:     "set-usage",
	Severity: SeverityWarning,
	Doc:      "Warn when `set` is used to reassign an already-bound symbol.\n\nThe first `set` creating a new binding is fine — ELPS has no `defvar`, so `set` is the standard way to create top-level bindings. However, subsequent `set` calls on the same symbol should use `set!` to clearly signal mutation intent.",
	Run: func(pass *Pass) error {
		seen := make(map[string]bool)
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			// Reset tracking when the package context changes.
			// Each package has its own namespace, so a `set` in a
			// new package is a first binding, not a reassignment.
			if HeadSymbol(sexpr) == "in-package" && depth == 0 {
				seen = make(map[string]bool)
				return
			}
			if HeadSymbol(sexpr) != "set" {
				return
			}
			if ArgCount(sexpr) < 1 {
				return
			}
			// Extract the symbol name from the first argument.
			// (set 'name value) — the arg is a quoted symbol.
			arg := sexpr.Cells[1]
			name := ""
			if arg.Type == lisp.LSymbol {
				name = arg.Str
			} else if arg.Type == lisp.LSExpr && arg.IsQuoted() && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
				name = arg.Cells[0].Str
			}
			if name == "" {
				return
			}
			if seen[name] {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("use set! instead of set to mutate '%s (already bound)", name),
					Pos:     posFromSource(astutil.SourceLoc(src)),
					EndPos:  endPosFromNode(src),
					Notes:   []string{"set creates a new binding; set! mutates an existing one"},
				})
			}
			seen[name] = true
		})
		return nil
	},
}

// AnalyzerInPackageToplevel warns when `in-package` is used inside nested
// expressions (function bodies, let forms, etc.) where it has no useful effect.
var AnalyzerInPackageToplevel = &Analyzer{
	Name:     "in-package-toplevel",
	Severity: SeverityWarning,
	Doc:      "Warn when `in-package` is used inside nested expressions.\n\n`in-package` only has meaningful effect at the top level of a file. Using it inside a `defun`, `let`, `lambda`, or other nested form is almost certainly a mistake.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) == "in-package" && depth > 0 {
				src := SourceOf(sexpr)
				pass.Reportf(astutil.SourceLoc(src), "in-package should only be used at the top level")
			}
		})
		return nil
	},
}

// AnalyzerIfArity checks that `if` has exactly 3 arguments (condition, then, else).
var AnalyzerIfArity = &Analyzer{
	Name:     "if-arity",
	Severity: SeverityError,
	Doc:      "Check that `if` has exactly 3 arguments: condition, then-branch, else-branch.\n\nA missing else branch is a common source of subtle nil-return bugs. Extra arguments are silently ignored at parse time but indicate a structural error.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) != "if" {
				return
			}
			argc := ArgCount(sexpr)
			if argc == 3 {
				return
			}
			head := sexpr.Cells[0]
			if argc < 3 {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("if requires 3 arguments (condition, then, else), got too few (%d)", argc),
					Pos:     posFromSource(astutil.SourceLoc(head)),
					EndPos:  endPosFromNode(head),
					Notes:   []string{"use cond for multi-branch conditionals, or provide an else branch"},
				})
			} else {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("if requires 3 arguments (condition, then, else), got too many (%d)", argc),
					Pos:     posFromSource(astutil.SourceLoc(head)),
					EndPos:  endPosFromNode(head),
					Notes:   []string{"if takes exactly (condition then-expr else-expr); use progn to group multiple expressions"},
				})
			}
		})
		return nil
	},
}

// AnalyzerLetBindings checks for malformed `let` and `let*` binding lists.
var AnalyzerLetBindings = &Analyzer{
	Name:     "let-bindings",
	Severity: SeverityError,
	Doc:      "Check for malformed `let`/`let*` binding lists.\n\nThe first argument to `let` or `let*` must be a list of (symbol value) pairs. Common mistakes include forgetting the outer list: `(let (x 1) ...)` instead of `(let ((x 1)) ...)`.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			if head != "let" && head != "let*" {
				return
			}
			headNode := sexpr.Cells[0]
			if ArgCount(sexpr) < 1 {
				pass.Report(Diagnostic{
					Message: head + " requires a binding list and body",
					Pos:     posFromSource(astutil.SourceLoc(headNode)),
					EndPos:  endPosFromNode(headNode),
				})
				return
			}
			bindings := sexpr.Cells[1]
			src := SourceOf(sexpr)

			// Bindings must be a list
			if bindings.Type != lisp.LSExpr {
				pass.Reportf(astutil.SourceLoc(src), "%s bindings must be a list, got %s", head, bindings.Type)
				return
			}

			// Each binding must be a 2-element list (symbol value)
			for i, binding := range bindings.Cells {
				if binding.Type != lisp.LSExpr {
					pass.Report(Diagnostic{
						Message: fmt.Sprintf("%s binding %d is not a list (did you forget the outer parentheses?)", head, i+1),
						Pos:     posFromSource(bindingSource(binding, src)),
						EndPos:  endPosFromNode(binding),
						Notes:   []string{"correct form: (let ((x 1) (y 2)) body...)"},
					})
					continue
				}
				if len(binding.Cells) == 0 {
					pass.Reportf(bindingSource(binding, src),
						"%s binding %d is empty", head, i+1)
					continue
				}
				// Accept (unquote sym) as a valid binding name — it expands to a symbol at macro-expansion time.
				if binding.Cells[0].Type != lisp.LSymbol && HeadSymbol(binding.Cells[0]) != "unquote" {
					pass.Reportf(bindingSource(binding, src),
						"%s binding %d: first element must be a symbol, got %s", head, i+1, binding.Cells[0].Type)
					continue
				}
				if len(binding.Cells) != 2 {
					pass.Reportf(bindingSource(binding, src),
						"%s binding %d (%s): expected 2 elements (symbol value), got %d", head, i+1, binding.Cells[0].Str, len(binding.Cells))
				}
			}
		})
		return nil
	},
}

// AnalyzerQuoteCall warns when set is called with an unquoted symbol
// as the first argument, which is almost always a mistake.
//
// NOTE: defconst is deliberately NOT checked. It is a macro that quotes its
// own name argument — (defconst x 42) expands to (set 'x 42) — so the
// correct spelling is the unquoted one. Flagging it reported every correct
// use, and the suggested "fix" of (defconst 'x 42) makes the program fail at
// runtime with "lisp:set: first argument is not a symbol: quote".
var AnalyzerQuoteCall = &Analyzer{
	Name:     "quote-call",
	Severity: SeverityWarning,
	Doc:      "Warn when set is called with an unquoted symbol argument.\n\nThe first argument to set should be a quoted symbol: (set 'x 42). Writing (set x 42) evaluates x first, which is rarely intended. This check does not flag set!, which takes an unquoted symbol by design, nor defconst, which quotes its own name argument.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			if head != "set" {
				return
			}
			if ArgCount(sexpr) < 1 {
				return
			}
			arg := sexpr.Cells[1]
			// Warn if the first argument is a bare (unquoted) symbol.
			// Quoted symbols have Quoted == true (e.g. 'x parses as
			// LSymbol{Quoted: true}). A bare LSymbol with Quoted == false
			// means the user forgot the quote.
			if arg.Type == lisp.LSymbol && !arg.IsQuoted() {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s first argument should be quoted: (set '%s ...) not (set %s ...)", head, arg.Str, arg.Str),
					Pos:     posFromSource(astutil.SourceLoc(src)),
					EndPos:  endPosFromNode(src),
					Notes:   []string{fmt.Sprintf("did you mean (%s '%s ...)?", head, arg.Str)},
				})
			}
		})
		return nil
	},
}

// AnalyzerCondMissingElse warns when a cond has no default (else or true) clause.
var AnalyzerCondMissingElse = &Analyzer{
	Name:     "cond-missing-else",
	Severity: SeverityInfo,
	Doc:      "Warn when a cond expression has no default clause.\n\nWithout an else or (true ...) clause, cond returns nil when no condition matches. This is a common source of unexpected nil values. Add (else ...) or (true ...) as the last clause to handle the default case.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) != "cond" {
				return
			}
			// Skip empty cond (no clauses)
			if ArgCount(sexpr) == 0 {
				return
			}
			// Check the last clause for else or true
			last := sexpr.Cells[len(sexpr.Cells)-1]
			if last.Type != lisp.LSExpr || len(last.Cells) == 0 {
				return // malformed clause, handled by cond-structure
			}
			head := last.Cells[0]
			if head.Type == lisp.LSymbol && isCondDefault(head.Str) {
				return // has default clause
			}
			src := SourceOf(sexpr)
			pass.Report(Diagnostic{
				Message: "cond has no default (else) clause",
				Pos:     posFromSource(astutil.SourceLoc(src)),
				EndPos:  endPosFromNode(src),
				Notes:   []string{"add (else ...) or (true ...) as the last clause to handle unmatched cases"},
			})
		})
		return nil
	},
}

// isCondDefault returns true if sym is a recognized default-clause head for cond.
// ELPS users commonly write (else ...), (true ...), (:else ...), or (:true ...).
func isCondDefault(sym string) bool {
	return sym == "else" || sym == "true" || sym == ":else" || sym == ":true"
}

// posFromSource converts a *token.Location to a Position, handling nil.
func posFromSource(src *token.Location) Position {
	if src == nil {
		return Position{}
	}
	return Position{File: src.File, Line: src.Line, Col: src.Col}
}

// endPosFromNode extracts an end position from a node. Uses EndLine/EndCol
// from the source location if available, otherwise estimates from the symbol
// name length. Returns zero Position when no end can be determined.
func endPosFromNode(node *lisp.LVal) Position {
	if node == nil {
		return Position{}
	}
	src, ok := node.Source()
	if !ok {
		return Position{}
	}
	if src.EndLine > 0 && src.EndCol > 0 {
		return Position{File: src.File, Line: src.EndLine, Col: src.EndCol}
	}
	if node.Type == lisp.LSymbol && len(node.Str) > 0 && src.Col > 0 {
		return Position{File: src.File, Line: src.Line, Col: src.Col + len(node.Str)}
	}
	return Position{}
}

func bindingSource(binding *lisp.LVal, fallback *lisp.LVal) *token.Location {
	if loc := astutil.SourceLoc(binding); loc != nil && loc.Line > 0 {
		return loc
	}
	return astutil.SourceLoc(fallback)
}

// AnalyzerDefunStructure checks for malformed `defun` and `defmacro` forms.
var AnalyzerDefunStructure = &Analyzer{
	Name:     "defun-structure",
	Severity: SeverityError,
	Doc:      "Check for malformed `defun`/`defmacro` definitions.\n\nA `defun` requires a symbol name and a formals list. An empty body (no-op) is valid. Common mistakes include non-symbol names or a non-list formals argument.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			if head != "defun" && head != "defmacro" {
				return
			}
			headNode := sexpr.Cells[0]
			argc := ArgCount(sexpr)
			if argc < 2 {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s requires at least a name and formals list (got %d argument(s))", head, argc),
					Pos:     posFromSource(astutil.SourceLoc(headNode)),
					EndPos:  endPosFromNode(headNode),
				})
				return
			}
			name := sexpr.Cells[1]
			if name.Type != lisp.LSymbol {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s name must be a symbol, got %s", head, name.Type),
					Pos:     posFromSource(astutil.SourceLoc(headNode)),
					EndPos:  endPosFromNode(headNode),
				})
			}
			formals := sexpr.Cells[2]
			if formals.Type != lisp.LSExpr {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s formals must be a list, got %s", head, formals.Type),
					Pos:     posFromSource(astutil.SourceLoc(headNode)),
					EndPos:  endPosFromNode(headNode),
				})
			}
		})
		return nil
	},
}

// AnalyzerCondStructure checks for malformed `cond` clauses.
var AnalyzerCondStructure = &Analyzer{
	Name:     "cond-structure",
	Severity: SeverityError,
	Doc:      "Check for malformed `cond` clauses.\n\nEach `cond` clause must be a non-empty list. The `else` clause, if present, must be last. Common mistakes include bare values instead of lists, or misplaced `else`.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) != "cond" {
				return
			}
			src := SourceOf(sexpr)
			last := len(sexpr.Cells) - 1

			for i := 1; i < len(sexpr.Cells); i++ {
				clause := sexpr.Cells[i]
				clauseSrc := SourceOf(clause)
				if loc := astutil.SourceLoc(clauseSrc); loc == nil || loc.Line == 0 {
					clauseSrc = src
				}

				if clause.Type != lisp.LSExpr {
					pass.Report(Diagnostic{
						Message: fmt.Sprintf("cond clause %d is not a list", i),
						Pos:     posFromSource(astutil.SourceLoc(clauseSrc)),
						EndPos:  endPosFromNode(clauseSrc),
						Notes:   []string{"cond clauses must be lists: (cond ((test1) body1) ((test2) body2) (else default))"},
					})
					continue
				}
				if len(clause.Cells) == 0 {
					pass.Reportf(astutil.SourceLoc(clauseSrc), "cond clause %d is empty", i)
					continue
				}

				// Check for misplaced else
				if clause.Cells[0].Type == lisp.LSymbol && isCondDefault(clause.Cells[0].Str) {
					if i != last {
						pass.Reportf(astutil.SourceLoc(clauseSrc), "cond else clause must be last (is clause %d of %d)", i, last)
					}
				}
			}
		})
		return nil
	},
}

// AnalyzerBuiltinArity checks for wrong argument counts to known builtin functions.
var AnalyzerBuiltinArity = &Analyzer{
	Name:     "builtin-arity",
	Severity: SeverityError,
	Doc:      "Check argument counts for calls to known builtin functions and special forms.\n\nELPS builtin functions have well-defined argument signatures. This check catches calls with too few or too many arguments before runtime. User-defined functions that shadow builtin names are automatically excluded, including names bound by let/let*/flet/labels/macrolet. Binding lists, formals lists and threading macro children are also excluded.",
	Run: func(pass *Pass) error {
		// Collect user-defined names so we don't flag shadowed builtins.
		userDefs := UserDefined(pass.Exprs)

		// Collect AST nodes where arity checking should be skipped.
		skipNodes := aritySkipNodes(pass.Exprs)

		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if skipNodes[sexpr] {
				return
			}
			head := HeadSymbol(sexpr)
			if head == "" {
				return
			}
			if userDefs[head] {
				return
			}
			spec, ok := builtinArityTable[head]
			if !ok {
				return
			}
			argc := ArgCount(sexpr)
			helpNote := fmt.Sprintf("see (help '%s) or `elps doc %s` for usage", head, head)
			headNode := sexpr.Cells[0]
			if argc < spec.min {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s requires at least %d argument(s), got %d", head, spec.min, argc),
					Pos:     posFromSource(astutil.SourceLoc(headNode)),
					EndPos:  endPosFromNode(headNode),
					Notes:   []string{helpNote},
				})
			}
			if spec.max >= 0 && argc > spec.max {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s accepts at most %d argument(s), got %d", head, spec.max, argc),
					Pos:     posFromSource(astutil.SourceLoc(headNode)),
					EndPos:  endPosFromNode(headNode),
					Notes:   []string{helpNote},
				})
			}
		})
		return nil
	},
}

// bindingForms are the special operators whose first argument is a list of
// bindings rather than an expression to evaluate. Their binding entries look
// like function calls to a naive walk — (let ((map (sorted-map))) ...)
// contains the s-expression (map (sorted-map)) — so they must be excluded
// from call-shaped checks.
//
// letBinding entries are (name value); funBinding entries are
// (name (formals...) body...).
var bindingForms = map[string]struct{ funBinding bool }{
	"let":      {funBinding: false},
	"let*":     {funBinding: false},
	"flet":     {funBinding: true},
	"labels":   {funBinding: true},
	"macrolet": {funBinding: true},
}

// bindingList returns the binding list of a binding form, or nil if sexpr is
// not a binding form or is malformed.
func bindingList(sexpr *lisp.LVal) (*lisp.LVal, bool) {
	kind, ok := bindingForms[HeadSymbol(sexpr)]
	if !ok || ArgCount(sexpr) < 1 {
		return nil, false
	}
	binds := sexpr.Cells[1]
	if binds == nil || binds.Type != lisp.LSExpr {
		return nil, false
	}
	return binds, kind.funBinding
}

// markLocallyShadowedCalls marks every call in form's subtree whose head is
// one of the names form binds, so those calls are not checked against the
// builtin arity table.
//
// The marking is scoped to the binding form's own subtree. A file-global name
// set would be simpler but silently disables the check for that name
// everywhere in the file: one unrelated (let ([map ...]) ...) in one function
// would suppress a genuine (map 'list) arity error in another. builtin-arity
// is a SeverityError check that gates the build, so it must not go quietly
// dark outside the scope that actually rebinds the name.
//
// Scoping to the whole form rather than to each binding's body is a
// deliberate over-approximation — it costs nothing in practice and avoids
// duplicating let/let*/flet/labels scope rules here.
func markLocallyShadowedCalls(form *lisp.LVal, binds *lisp.LVal, funBinding bool, skip map[*lisp.LVal]bool) {
	local := make(map[string]bool)
	for _, bind := range binds.Cells {
		if bind == nil || bind.Type != lisp.LSExpr || len(bind.Cells) == 0 {
			continue
		}
		if name := bind.Cells[0]; name.Type == lisp.LSymbol {
			local[name.Str] = true
		}
		if funBinding && len(bind.Cells) >= 2 {
			CollectFormals(bind.Cells[1], local)
		}
	}
	if len(local) == 0 {
		return
	}
	WalkSExprs([]*lisp.LVal{form}, func(sexpr *lisp.LVal, depth int) {
		if head := HeadSymbol(sexpr); head != "" && local[head] {
			skip[sexpr] = true
		}
	})
}

// aritySkipNodes returns a set of AST nodes that should be excluded from
// arity checking. This covers three cases:
//
//  1. Formals lists — (defun f (x y) ...) the (x y) is a parameter list,
//     not a function call.
//  2. Threading macro children — (thread-first v (get "key")) expands to
//     (get v "key"), so the static arg count is one less than the runtime count.
//  3. Binding-form entries — the (map (sorted-map)) inside
//     (let ((map (sorted-map))) ...) binds the name `map`; it is not a call
//     to the builtin `map`. Same for let*, flet, labels and macrolet, whose
//     entries additionally carry a formals list.
func aritySkipNodes(exprs []*lisp.LVal) map[*lisp.LVal]bool {
	skip := make(map[*lisp.LVal]bool)
	WalkSExprs(exprs, func(sexpr *lisp.LVal, depth int) {
		head := HeadSymbol(sexpr)
		switch head {
		case "defun", "defmacro":
			// Formals at position 2: (defun name (formals...) body...)
			if ArgCount(sexpr) >= 2 {
				skip[sexpr.Cells[2]] = true
			}
		case "lambda":
			// Formals at position 1: (lambda (formals...) body...)
			if ArgCount(sexpr) >= 1 {
				skip[sexpr.Cells[1]] = true
			}
		case "thread-first", "thread-last":
			// Children at positions 2+ are forms that get an extra arg
			// inserted by the macro: (thread-first val (f a)) => (f val a)
			for i := 2; i < len(sexpr.Cells); i++ {
				skip[sexpr.Cells[i]] = true
			}
		}
		if binds, funBinding := bindingList(sexpr); binds != nil {
			skip[binds] = true
			for _, bind := range binds.Cells {
				if bind == nil || bind.Type != lisp.LSExpr {
					continue
				}
				skip[bind] = true
				if funBinding && len(bind.Cells) >= 2 {
					skip[bind.Cells[1]] = true // formals list
				}
			}
			markLocallyShadowedCalls(sexpr, binds, funBinding, skip)
		}
	})
	return skip
}

// aritySpec defines the min/max argument count for a function.
// max == -1 means variadic (unlimited).
type aritySpec struct {
	min int
	max int
}

// builtinArityTable is the static arity table for all known builtins,
// special ops, and macros. Built from the Formals() definitions.
var builtinArityTable = buildArityTable()

func buildArityTable() map[string]aritySpec {
	table := make(map[string]aritySpec)

	// Parse a Formals() spec into min/max arity.
	parseFormals := func(name string, formals *lisp.LVal) {
		if formals == nil || formals.Type != lisp.LSExpr {
			return
		}
		minArity := 0
		maxArity := 0
		variadic := false
		inOptional := false
		inKey := false

		for _, sym := range formals.Cells {
			if sym.Type != lisp.LSymbol {
				continue
			}
			switch sym.Str {
			case "&rest":
				variadic = true
			case "&optional":
				inOptional = true
			case "&key":
				inKey = true
			default:
				if variadic {
					// The symbol after &rest is the variadic param name, skip
					continue
				}
				maxArity++
				if !inOptional && !inKey {
					minArity++
				}
			}
		}

		if variadic || inKey {
			table[name] = aritySpec{min: minArity, max: -1}
		} else {
			table[name] = aritySpec{min: minArity, max: maxArity}
		}
	}

	// Builtins
	for _, b := range lisp.DefaultBuiltins() {
		parseFormals(b.Name(), b.Formals())
	}

	// Special operators
	for _, op := range lisp.DefaultSpecialOps() {
		parseFormals(op.Name(), op.Formals())
	}

	// Macros
	for _, m := range lisp.DefaultMacros() {
		parseFormals(m.Name(), m.Formals())
	}

	// Remove entries that are checked by more specific analyzers to avoid
	// duplicate diagnostics.
	delete(table, "if")   // checked by if-arity
	delete(table, "cond") // checked by cond-structure

	return table
}

// AnalyzerRethrowContext warns when `rethrow` is used outside of a
// `handler-bind` form. At runtime, rethrow can only be called from within a
// handler-bind handler; calling it elsewhere always produces an error.
var AnalyzerRethrowContext = &Analyzer{
	Name:     "rethrow-context",
	Severity: SeverityError,
	Doc:      "Warn when `rethrow` is used outside a `handler-bind` form.\n\n`rethrow` re-raises the current error being handled by handler-bind, preserving the original stack trace. Calling it outside any handler-bind always produces an error at runtime.",
	Run: func(pass *Pass) error {
		walkRethrowContext(pass.Exprs, 0, func(sexpr *lisp.LVal) {
			src := SourceOf(sexpr)
			pass.Report(Diagnostic{
				Message: "rethrow used outside handler-bind",
				Pos:     posFromSource(astutil.SourceLoc(src)),
				EndPos:  endPosFromNode(src),
				Notes:   []string{"rethrow can only be called from within a handler-bind handler"},
			})
		})
		return nil
	},
}

// walkRethrowContext recursively walks the AST, tracking how many
// handler-bind forms are in scope. When it finds a (rethrow) call with
// handlerDepth == 0, it calls report.
func walkRethrowContext(exprs []*lisp.LVal, handlerDepth int, report func(*lisp.LVal)) {
	for _, expr := range exprs {
		walkRethrowNode(expr, handlerDepth, report)
	}
}

func walkRethrowNode(node *lisp.LVal, handlerDepth int, report func(*lisp.LVal)) {
	if node == nil {
		return
	}
	if node.Type != lisp.LSExpr || node.IsQuoted() || len(node.Cells) == 0 {
		for _, child := range node.Cells {
			walkRethrowNode(child, handlerDepth, report)
		}
		return
	}

	head := HeadSymbol(node)

	if head == "rethrow" && handlerDepth == 0 {
		report(node)
		return
	}

	if head == "handler-bind" {
		// Walk the bindings (first arg) and body forms with incremented depth.
		for _, child := range node.Cells[1:] {
			walkRethrowNode(child, handlerDepth+1, report)
		}
		return
	}

	for _, child := range node.Cells {
		walkRethrowNode(child, handlerDepth, report)
	}
}

// implicitPrognForms lists forms whose body already supports multiple
// expressions, making a wrapping progn redundant. The int value is the
// index of the first body argument (0-based from the s-expression head).
// For example, defun has name + formals before the body, so bodyStart = 3.
var implicitPrognForms = map[string]int{
	"lambda":        2, // (lambda (formals) body...)
	"defun":         3, // (defun name (formals) body...)
	"defmacro":      3, // (defmacro name (formals) body...)
	"let":           2, // (let (bindings) body...)
	"let*":          2, // (let* (bindings) body...)
	"flet":          2, // (flet (bindings) body...)
	"labels":        2, // (labels (bindings) body...)
	"macrolet":      2, // (macrolet (bindings) body...)
	"handler-bind":  2, // (handler-bind (bindings) body...)
	"ignore-errors": 1, // (ignore-errors body...)
	"with-cleanup":  2, // (with-cleanup (cleanup...) body...)
	"dotimes":       2, // (dotimes (var n) body...)
	"progn":         1, // (progn body...) — nested progn
}

// AnalyzerWithCleanupForms warns about two degenerate spellings of the
// with-cleanup spec list, both of which run without complaint.
//
// An EMPTY list makes the form a no-op wrapper around its body: it still
// runs and still returns the same value, so nothing at runtime
// distinguishes it from the body alone.
//
// A BARE SYMBOL in the list is the missing-paren mistake that let-bindings
// catches for let, and it is the more dangerous of the two:
//
//	(with-cleanup (release h) (work))
//
// parses as a spec list of two forms -- the symbol `release` and the symbol
// `h` -- neither of which does anything.  The release never happens, and
// the program behaves exactly as if the cleanup had been written correctly
// right up until the body signals.  A real cleanup form is a call; a bare
// symbol as one is always either this mistake or dead code.
var AnalyzerWithCleanupForms = &Analyzer{
	Name:     "with-cleanup-forms",
	Severity: SeverityWarning,
	Doc:      "Warn about a with-cleanup spec list that is empty or holds a bare symbol.\n\nAn empty list guarantees nothing, so the form is indistinguishable from its body alone. A bare symbol is the missing-paren mistake -- `(with-cleanup (release h) ...)` runs neither `release` nor `h`, so the cleanup silently never happens.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) != "with-cleanup" {
				return
			}
			// Zero arguments is an arity error that builtin-arity already
			// reports, so leave it alone rather than double-reporting.
			if ArgCount(sexpr) < 1 {
				return
			}
			spec := sexpr.Cells[1]
			if spec.Type != lisp.LSExpr {
				// A non-list spec is the operator's own runtime error.
				return
			}
			if len(spec.Cells) == 0 {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: "with-cleanup has no cleanup forms, so it guarantees nothing",
					Pos:     posFromSource(astutil.SourceLoc(src)),
					EndPos:  endPosFromNode(src),
					Notes: []string{
						"cleanup forms go in the first argument: (with-cleanup ((release h)) body...)",
					},
				})
				return
			}
			for _, form := range spec.Cells {
				if form.Type != lisp.LSymbol {
					continue
				}
				src := SourceOf(form)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("cleanup form %q is a bare symbol and does nothing"+
						" (missing parentheses?)", form.Str),
					Pos:    posFromSource(astutil.SourceLoc(src)),
					EndPos: endPosFromNode(src),
					Notes: []string{
						"the spec is a LIST of forms: (with-cleanup ((release h)) body...)",
						"written as (with-cleanup (release h) ...) the cleanup never runs",
					},
				})
			}
		})
		return nil
	},
}

// AnalyzerUnnecessaryProgn warns when progn is used as the sole body
// expression in a form that already supports multiple body expressions.
var AnalyzerUnnecessaryProgn = &Analyzer{
	Name:     "unnecessary-progn",
	Severity: SeverityInfo,
	Doc:      "Warn when `progn` wraps the body of a form that already supports multiple expressions.\n\nForms like `defun`, `defmacro`, `lambda`, `let`, and others evaluate their body as an implicit progn. Wrapping the body in an explicit `(progn ...)` is redundant. This does not flag `progn` inside `if` branches, where it is needed.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			bodyStart, ok := implicitPrognForms[head]
			if !ok {
				return
			}
			// Check if there is exactly one body expression and it's a progn
			bodyExprs := len(sexpr.Cells) - bodyStart
			if bodyExprs != 1 {
				return
			}
			body := sexpr.Cells[bodyStart]
			if HeadSymbol(body) != "progn" {
				return
			}
			src := SourceOf(body)
			var msg string
			if head == "progn" {
				msg = "nested progn is redundant"
			} else {
				msg = fmt.Sprintf("progn is unnecessary in %s body (it supports multiple expressions)", head)
			}
			pass.Report(Diagnostic{
				Message: msg,
				Pos:     posFromSource(astutil.SourceLoc(src)),
				EndPos:  endPosFromNode(src),
				Notes:   []string{fmt.Sprintf("remove the progn and move its contents directly into the %s body", head)},
			})
		})
		// Also check cond clause bodies
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) != "cond" {
				return
			}
			for i := 1; i < len(sexpr.Cells); i++ {
				clause := sexpr.Cells[i]
				if clause.Type != lisp.LSExpr || len(clause.Cells) < 2 {
					continue
				}
				// Clause body starts at index 1 (after the test)
				if len(clause.Cells) == 2 && HeadSymbol(clause.Cells[1]) == "progn" {
					src := SourceOf(clause.Cells[1])
					pass.Report(Diagnostic{
						Message: "progn is unnecessary in cond clause body (it supports multiple expressions)",
						Pos:     posFromSource(astutil.SourceLoc(src)),
						EndPos:  endPosFromNode(src),
						Notes:   []string{"remove the progn and move its contents directly into the cond clause"},
					})
				}
			}
		})
		return nil
	},
}

// AnalyzerUnusedVariable warns about variables and parameters that are defined
// but never referenced. Requires semantic analysis (pass.Semantics != nil).
var AnalyzerUnusedVariable = &Analyzer{
	Name:     "unused-variable",
	Severity: SeverityWarning,
	Semantic: true,
	Doc:      "Warn about variables and parameters that are defined but never referenced.\n\nRequires semantic analysis (--workspace flag). Skips variables with underscore prefix (conventional \"ignored\" marker) and top-level (global scope) variables.",
	Run: func(pass *Pass) error {
		if pass.Semantics == nil {
			return nil
		}
		for _, sym := range pass.Semantics.Symbols {
			if sym.References > 0 {
				continue
			}
			if sym.Kind != analysis.SymVariable && sym.Kind != analysis.SymParameter {
				continue
			}
			// Skip global scope variables (top-level set bindings)
			if sym.Scope == pass.Semantics.RootScope {
				continue
			}
			// Skip _ prefixed names (conventional "unused" marker)
			if len(sym.Name) > 0 && sym.Name[0] == '_' {
				continue
			}
			pass.Report(Diagnostic{
				Message:     fmt.Sprintf("unused %s: %s", sym.Kind, sym.Name),
				Pos:         posFromSource(sym.Source),
				EndPos:      endPosFromNode(sym.Node),
				Notes:       []string{fmt.Sprintf("if '%s' is intentionally unused, prefix it with '_'", sym.Name)},
				Unnecessary: true,
			})
		}
		return nil
	},
}

// AnalyzerUnusedFunction warns about functions and macros that are defined at
// the top level but never referenced. Requires semantic analysis.
var AnalyzerUnusedFunction = &Analyzer{
	Name:     "unused-function",
	Severity: SeverityWarning,
	Semantic: true,
	Doc:      "Warn about top-level functions and macros that are defined but never referenced.\n\nRequires semantic analysis (--workspace flag). Exported symbols and functions with underscore prefix are excluded.",
	Run: func(pass *Pass) error {
		if pass.Semantics == nil {
			return nil
		}
		for _, sym := range pass.Semantics.Symbols {
			if sym.References > 0 {
				continue
			}
			if sym.Kind != analysis.SymFunction && sym.Kind != analysis.SymMacro {
				continue
			}
			// Only check top-level definitions
			if sym.Scope != pass.Semantics.RootScope {
				continue
			}
			// Skip exported symbols
			if sym.Exported {
				continue
			}
			// Skip _ prefixed names
			if len(sym.Name) > 0 && sym.Name[0] == '_' {
				continue
			}
			// Check cross-file references from workspace scanning.
			// Per-file analysis can't see callers in other files.
			if pass.Semantics.WorkspaceRefs != nil {
				key := analysis.SymbolToKey(sym).String()
				if refs, ok := pass.Semantics.WorkspaceRefs[key]; ok {
					hasExternal := false
					for _, ref := range refs {
						if ref.File != pass.Filename {
							hasExternal = true
							break
						}
					}
					if hasExternal {
						continue
					}
				}
			}
			pass.Report(Diagnostic{
				Message:     fmt.Sprintf("unused %s: %s", sym.Kind, sym.Name),
				Pos:         posFromSource(sym.Source),
				EndPos:      endPosFromNode(sym.Node),
				Notes:       []string{"if this is a public API, add it to an (export ...) form"},
				Unnecessary: true,
			})
		}
		return nil
	},
}

// AnalyzerShadowing reports when a local binding shadows a name from an
// enclosing scope. This is informational — shadowing is valid but can cause
// confusion. Requires semantic analysis (pass.Semantics != nil).
var AnalyzerShadowing = &Analyzer{
	Name:     "shadowing",
	Severity: SeverityInfo,
	Semantic: true,
	Doc: "Report when a local binding shadows a name from an enclosing scope.\n\n" +
		"Requires semantic analysis (--workspace flag). Severity follows what is being " +
		"hidden: shadowing a builtin, special operator or macro is a WARNING, because a " +
		"later call to that name silently means something else; shadowing another local " +
		"is INFO.\n\n" +
		"A binding whose initialiser references the name it shadows is NOT reported \u2014 " +
		"(let* ([ctx (default ctx (sorted-map))]) refines one value rather than " +
		"introducing a second meaning, and ELPS offers no other way to default an " +
		"&optional argument (elps#559).",
	Run: func(pass *Pass) error {
		if pass.Semantics == nil {
			return nil
		}
		for _, sym := range pass.Semantics.Symbols {
			if sym.Scope == nil || sym.Scope == pass.Semantics.RootScope {
				continue // top-level definitions can't shadow
			}
			if sym.Scope.Parent == nil {
				continue
			}
			outer := sym.Scope.Parent.Lookup(sym.Name)
			if outer == nil {
				continue
			}
			// Don't report shadowing of external (workspace/package-imported)
			// symbols — these are injected globally and would produce noise
			// for common parameter names like x, y, v, etc.
			if outer.External {
				continue
			}
			// Don't report when a parameter shadows a builtin or special-op.
			// Names like expr, car, map are pervasive in formals and shadowing
			// them is idiomatic — the builtins themselves use these names.
			if sym.Kind == analysis.SymParameter &&
				(outer.Kind == analysis.SymSpecialOp || outer.Kind == analysis.SymBuiltin) {
				continue
			}
			// Don't report a binding that REFINES the thing it shadows.
			// (let* ([ctx (default ctx (sorted-map))]) narrows one value; it
			// does not give the name a second meaning, and it is the only way
			// to default an &optional argument. See elps#559.
			//
			// This does NOT apply when the shadowed name is callable. There,
			// (let ([car (car xs)]) (car xs)) narrows nothing -- the body's
			// (car xs) applies an element as a function and fails at runtime.
			// Refinement is only coherent for a value; silencing it here would
			// suppress exactly the hazard hidesCallable exists to promote.
			if !hidesCallable(outer.Kind) && refinesShadowed(sym) {
				continue
			}
			// Hiding a builtin, special-op or macro is a different category of
			// problem from shadowing a local: a later (min a b) in that scope
			// silently denotes the local instead of the builtin.
			severity := SeverityInfo
			note := fmt.Sprintf("rename '%s' to avoid confusion with the outer %s", sym.Name, outer.Kind)
			if hidesCallable(outer.Kind) {
				severity = SeverityWarning
				note = fmt.Sprintf("rename '%s': while this binding is in scope, a call to %s "+
					"resolves to it rather than to the %s", sym.Name, sym.Name, outer.Kind)
			}
			pass.Report(Diagnostic{
				Message:  fmt.Sprintf("%s '%s' shadows %s from enclosing scope", sym.Kind, sym.Name, outer.Kind),
				Severity: severity,
				Pos:      posFromSource(sym.Source),
				EndPos:   endPosFromNode(sym.Node),
				Notes:    []string{note},
			})
		}
		return nil
	},
}

// hidesCallable reports whether shadowing a symbol of this kind changes what a
// CALL means. While the binding is in scope, (min a b) silently denotes the
// local instead of the builtin, so these are reported at warning severity
// rather than info (elps#559).
//
// SymFunction counts for the same reason a builtin does, and leaving it out was
// a real gap: after (defun helper (x) x), a (let ([helper 2])) makes the body's
// (helper 1) apply the integer 2. Nothing about that is milder because the
// callee happened to be user-defined.
func hidesCallable(k analysis.SymbolKind) bool {
	switch k {
	case analysis.SymBuiltin, analysis.SymSpecialOp, analysis.SymMacro, analysis.SymFunction:
		return true
	case analysis.SymVariable, analysis.SymParameter, analysis.SymType:
		return false
	default:
		return false
	}
}

// refinesShadowed reports whether a binding's initialiser references the very
// name the binding shadows — (let* ([ctx (default ctx (sorted-map))]).
//
// That shape narrows a single value rather than introducing a second meaning
// for the name, and ELPS gives authors no other way to default an &optional
// argument, so reporting it is noise that buries the shadows that matter
// (elps#559). Only let/let* bindings carry an Init, so every other symbol kind
// falls through to being reported exactly as before.
func refinesShadowed(sym *analysis.Symbol) bool {
	if sym == nil || sym.Init == nil {
		return false
	}
	return mentionsUnquoted(sym.Init, sym.Name)
}

// mentionsUnquoted reports whether name occurs in node as live code, refusing
// to descend into quoted subtrees.
//
// A quoted occurrence is data, not a use: (let ([keys 'keys]) ...) rebinds keys
// to the SYMBOL keys, which narrows nothing. Skipping the whole quoted subtree
// rather than only a quoted leaf also covers '(keys foo), whose elements do not
// individually carry the flag.
//
// Known limitation: an occurrence that a nested binding form inside the
// initialiser rebinds -- (let ([v (lambda (v) ...)]) ...) -- still counts,
// because this is a syntactic walk with no scope of its own. That can only
// under-report an info-level shadow of a local; hidesCallable kinds never reach
// here.
func mentionsUnquoted(node *lisp.LVal, name string) bool {
	if node == nil || node.IsQuoted() {
		return false
	}
	if node.Type == lisp.LSymbol {
		return node.Str == name
	}
	for _, c := range node.Cells {
		if mentionsUnquoted(c, name) {
			return true
		}
	}
	return false
}

// AnalyzerUserArity checks argument counts for calls to user-defined functions
// and macros whose signatures are known from the same file. Requires semantic
// analysis (pass.Semantics != nil).
var AnalyzerUserArity = &Analyzer{
	Name:     "user-arity",
	Severity: SeverityError,
	Semantic: true,
	Doc:      "Check argument counts for calls to user-defined functions and macros.\n\nRequires semantic analysis (--workspace flag). Only checks calls to functions with known signatures (Source != nil). Complements builtin-arity which covers builtins.",
	Run: func(pass *Pass) error {
		if pass.Semantics == nil {
			return nil
		}
		// Reuse aritySkipNodes to exclude formals lists and threading macro children.
		skipNodes := aritySkipNodes(pass.Exprs)

		// Build a set of function/macro names that are locally shadowed
		// somewhere in the file (by let, let*, lambda params, etc.).
		// When a name is shadowed, some call sites may refer to the local
		// binding rather than the root-scope defun, so we conservatively
		// skip arity checks for those names.
		locallyShadowed := make(map[string]bool)
		for _, sym := range pass.Semantics.Symbols {
			if sym.Scope == nil || sym.Scope == pass.Semantics.RootScope {
				continue
			}
			// Check if a root-scope function/macro has the same name.
			rootSym := pass.Semantics.RootScope.LookupLocal(sym.Name)
			if rootSym != nil && rootSym.Signature != nil &&
				(rootSym.Kind == analysis.SymFunction || rootSym.Kind == analysis.SymMacro) {
				locallyShadowed[sym.Name] = true
			}
		}

		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if skipNodes[sexpr] {
				return
			}
			head := HeadSymbol(sexpr)
			if head == "" {
				return
			}
			sym := pass.Semantics.RootScope.Lookup(head)
			if sym == nil || sym.Signature == nil || sym.Source == nil {
				return // unknown or builtin — skip
			}
			if sym.External {
				return // imported from workspace/package — may shadow builtins
			}
			if sym.Kind != analysis.SymFunction && sym.Kind != analysis.SymMacro {
				return
			}
			if locallyShadowed[head] {
				return // name is shadowed by a local binding somewhere
			}
			argc := ArgCount(sexpr)
			minArity := sym.Signature.MinArity()
			maxArity := sym.Signature.MaxArity()
			if argc < minArity {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s requires at least %d argument(s), got %d", head, minArity, argc),
					Pos:     posFromSource(astutil.SourceLoc(src)),
					EndPos:  endPosFromNode(src),
					Notes:   []string{"defined at " + sourceString(sym.Source)},
					Related: relatedFromSource(sym.Source, "defined here"),
				})
			}
			if maxArity >= 0 && argc > maxArity {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s accepts at most %d argument(s), got %d", head, maxArity, argc),
					Pos:     posFromSource(astutil.SourceLoc(src)),
					EndPos:  endPosFromNode(src),
					Notes:   []string{"defined at " + sourceString(sym.Source)},
					Related: relatedFromSource(sym.Source, "defined here"),
				})
			}
		})
		return nil
	},
}

// sourceString formats a token.Location for use in diagnostic notes.
func sourceString(loc *token.Location) string {
	if loc == nil {
		return "<unknown>"
	}
	if loc.Col > 0 {
		return fmt.Sprintf("%s:%d:%d", loc.File, loc.Line, loc.Col)
	}
	return fmt.Sprintf("%s:%d", loc.File, loc.Line)
}

func relatedFromSource(loc *token.Location, message string) []RelatedInformation {
	if loc == nil {
		return nil
	}
	return []RelatedInformation{{
		Location: posFromSource(loc),
		Message:  message,
	}}
}

// AnalyzerUndefinedSymbol reports symbols that could not be resolved in any
// enclosing scope. Requires semantic analysis (pass.Semantics != nil).
var AnalyzerUndefinedSymbol = &Analyzer{
	Name:     "undefined-symbol",
	Severity: SeverityError,
	Semantic: true,
	Doc:      "Report symbols that cannot be resolved in any enclosing scope.\n\nRequires semantic analysis (--workspace flag). Keywords and qualified symbols are excluded. Builtins, special operators, and macros are pre-populated.",
	Run: func(pass *Pass) error {
		if pass.Semantics == nil {
			return nil
		}
		for _, u := range pass.Semantics.Unresolved {
			sev := SeverityError
			notes := []string{fmt.Sprintf("'%s' is not defined in any enclosing scope; did you mean a different name?", u.Name)}
			if u.InsideMacroCall {
				sev = SeverityWarning
				notes = append(notes, "inside a macro call — the macro may introduce this binding at expansion time")
			}
			pass.Report(Diagnostic{
				Severity: sev,
				Message:  "undefined symbol: " + u.Name,
				Pos:      posFromSource(u.Source),
				EndPos:   endPosFromNode(u.Node),
				Notes:    notes,
			})
		}
		return nil
	},
}

// AnalyzerDuplicateDefinition warns when the same symbol is defined more than
// once at the top level (e.g. two defun with the same name). Only flags
// defun/defmacro duplicates — repeated set is already covered by set-usage.
// Cross-file duplicates are detected when an External symbol matches a local
// definition. Requires semantic analysis.
var AnalyzerDuplicateDefinition = &Analyzer{
	Name:     "duplicate-definition",
	Severity: SeverityWarning,
	Semantic: true,
	Doc:      "Warn when a symbol is defined more than once at the top level.\n\nRequires semantic analysis (--workspace flag). Detects same-file duplicates (two defun with the same name) and cross-file duplicates (a local defun that shadows an imported definition). Only checks defun and defmacro — repeated set is handled by set-usage.",
	Run: func(pass *Pass) error {
		if pass.Semantics == nil {
			return nil
		}

		// definitionKinds are the symbol kinds we check for duplicates.
		isDefKind := func(k analysis.SymbolKind) bool {
			return k == analysis.SymFunction || k == analysis.SymMacro
		}

		// Collect local (non-external) root-scope definitions by (name, package).
		type defKey struct {
			name string
			pkg  string
		}
		groups := make(map[defKey][]*analysis.Symbol)

		for _, sym := range pass.Semantics.Symbols {
			if sym.Scope != pass.Semantics.RootScope {
				continue
			}
			if sym.External {
				continue
			}
			if !isDefKind(sym.Kind) {
				continue
			}
			if sym.Source == nil {
				continue // skip builtins
			}
			key := defKey{name: sym.Name, pkg: sym.Package}
			groups[key] = append(groups[key], sym)
		}

		// Build an index of ExtraGlobals for cross-file duplicate checking.
		// This avoids relying on scope lookups which may have been
		// overwritten by local definitions during prescan.
		type extKey struct {
			name string
			pkg  string
		}
		extIndex := make(map[extKey]*analysis.ExternalSymbol)
		for i := range pass.Semantics.ExtraGlobals {
			ext := &pass.Semantics.ExtraGlobals[i]
			if !isDefKind(ext.Kind) || ext.Source == nil {
				continue
			}
			pkg := ext.Package
			if pkg == "" {
				pkg = "user"
			}
			// Keep only the first external occurrence per key.
			ek := extKey{name: ext.Name, pkg: pkg}
			if _, exists := extIndex[ek]; !exists {
				extIndex[ek] = ext
			}
		}

		// Normalize the current filename for reliable self-file comparison.
		// Paths from workspace scanning may be absolute while lint filenames
		// may be relative (or vice versa).
		cleanFilename := analysis.NormalizePath(pass.Filename)

		for key, syms := range groups {
			first := syms[0]

			// Same-file duplicates: report on 2nd+ definitions.
			for _, sym := range syms[1:] {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("duplicate definition: %s '%s' already defined", sym.Kind, sym.Name),
					Pos:     posFromSource(sym.Source),
					EndPos:  endPosFromNode(sym.Node),
					Notes:   []string{"first defined at " + sourceString(first.Source)},
					Related: relatedFromSource(first.Source, "first defined here"),
				})
			}

			// Cross-file: check if an external symbol with the same name
			// and package exists in ExtraGlobals. The extIndex maps each
			// (name, pkg) to at most one external, so this produces at
			// most one cross-file warning per local symbol.
			localPkg := key.pkg
			if localPkg == "" {
				localPkg = "user"
			}
			if ext, ok := extIndex[extKey{name: key.name, pkg: localPkg}]; ok {
				// Skip if the external definition is from the same file
				// (self-reference from workspace scanning).
				if cleanFilename != "" && analysis.NormalizePath(ext.Source.File) == cleanFilename {
					continue
				}
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("duplicate definition: %s '%s' is also defined externally", first.Kind, first.Name),
					Pos:     posFromSource(first.Source),
					EndPos:  endPosFromNode(first.Node),
					Notes:   []string{"also defined at " + sourceString(ext.Source)},
					Related: relatedFromSource(ext.Source, "also defined here"),
				})
			}
		}
		return nil
	},
}

// AnalyzerDeprecated reports uses of symbols whose docstring marks them
// deprecated, following the convention Go doc comments use: a docstring
// paragraph beginning with "Deprecated:" (or "DEPRECATED:") deprecates the
// symbol and the rest of the paragraph says what to use instead.
// lisp.DeprecationNotice is the canonical detector.
//
// Every symbol kind semantic analysis records a docstring for is covered:
// same-file defun/defmacro, workspace-scanned definitions, the compiled-in
// builtins, special operators and macros, and the builtins an embedder
// registers through a lisp.PackageRegistry (LintConfig.Registry).
//
// Requires semantic analysis (pass.Semantics != nil).
var AnalyzerDeprecated = &Analyzer{
	Name:     "deprecated",
	Severity: SeverityWarning,
	Semantic: true,
	Doc:      "Report uses of symbols marked deprecated by their docstring.\n\nRequires semantic analysis (--workspace flag). A symbol is deprecated when a paragraph of its docstring begins with \"Deprecated:\", the same convention Go doc comments use; the rest of that paragraph is reported as the notice. Definitions are never flagged, only uses, and a use inside the body of a definition that is itself deprecated is not reported — deprecated code may call deprecated code.",
	Run: func(pass *Pass) error {
		if pass.Semantics == nil {
			return nil
		}
		// Source spans of the definitions that are themselves deprecated. Go's
		// rule is that deprecated code may use deprecated code, so references
		// from inside those bodies are exempt. Built lazily: almost every file
		// has no deprecated reference at all, and the LSP runs this on each
		// keystroke.
		var exempt byteSpans
		exemptBuilt := false
		passFile := analysis.NormalizePath(pass.Filename)

		// References is a slice, in resolution order. Never iterate a Go map
		// here: diagnostic order is part of the CLI's output contract and
		// FuzzLintSource asserts two runs over the same bytes agree.
		reported := make(map[int]bool)
		for _, ref := range pass.Semantics.References {
			if ref == nil || ref.Symbol == nil || ref.Source == nil {
				continue
			}
			notice, ok := lisp.DeprecationNotice(ref.Symbol.DocString)
			if !ok {
				continue
			}
			// A configured MacroExpander analyzes expanded forms whose nodes
			// come from the macro's defining file, not the file being linted.
			// Their byte offsets are meaningless against this file -- reporting
			// them misattributes the diagnostic, and testing them against this
			// file's exemption spans can silently drop real findings. A
			// deprecated use inside a macro template is reported when the
			// template's own file is linted.
			if analysis.NormalizePath(ref.Source.File) != passFile {
				continue
			}
			if !exemptBuilt {
				exempt = deprecatedBodySpans(pass.Exprs)
				exemptBuilt = true
			}
			if exempt.contains(ref.Source.Pos) {
				continue
			}
			// One use, one diagnostic. A call to a user macro is resolved
			// twice -- analyzeCall records the head before trying expansion
			// and resolveSymbol records it again -- and with an expander the
			// two References need not share a node, so the key is the byte
			// offset of the use, which they always share.
			if ref.Source.Pos >= 0 {
				if reported[ref.Source.Pos] {
					continue
				}
				reported[ref.Source.Pos] = true
			}
			// Name it as the reference site spells it, so a qualified use
			// reports 'pkg:name' rather than the bare symbol.
			name := ref.Symbol.Name
			if ref.Node != nil && ref.Node.Type == lisp.LSymbol && ref.Node.Str != "" {
				name = ref.Node.Str
			}
			msg := fmt.Sprintf("use of deprecated %s '%s'", deprecatedKind(ref.Symbol.Kind), name)
			if notice != "" {
				msg += ": " + notice
			}
			// A builtin has no declaration to point at, and neither does a
			// symbol whose location was synthesised rather than scanned
			// (Location.Pos < 0 is the "no position" spelling token.Location
			// itself uses). Both leave the diagnostic with only its message.
			decl := ref.Symbol.Source
			if decl != nil && decl.Pos < 0 {
				decl = nil
			}
			// No hand-written nolint note: the CLI appends the suppression
			// hint to every diagnostic (cmd/diagnostic.go), and no other
			// analyzer duplicates it.
			var notes []string
			if decl != nil {
				notes = []string{"deprecated at " + sourceString(decl)}
			}
			pass.Report(Diagnostic{
				Message:    msg,
				Pos:        posFromSource(ref.Source),
				EndPos:     endPosFromNode(ref.Node),
				Notes:      notes,
				Related:    relatedFromSource(decl, "deprecated declaration here"),
				Deprecated: true,
			})
		}
		return nil
	},
}

// deprecatedKind renders a symbol kind for the deprecated diagnostic. It is
// deliberately not analysis.SymbolKind.String(): that spells SymSpecialOp
// "special-op", which reads as an identifier rather than prose in a sentence.
func deprecatedKind(k analysis.SymbolKind) string {
	switch k {
	case analysis.SymFunction:
		return "function"
	case analysis.SymMacro:
		return "macro"
	case analysis.SymBuiltin:
		return "builtin"
	case analysis.SymSpecialOp:
		return "special operator"
	default:
		return "symbol"
	}
}

// byteSpan is a half-open [start, end) range of byte offsets into the file
// being linted.
type byteSpan struct {
	start int
	end   int
}

// byteSpans is a sorted, non-overlapping set of source spans.
type byteSpans []byteSpan

// contains reports whether the byte offset pos falls inside any span.
func (s byteSpans) contains(pos int) bool {
	if pos < 0 || len(s) == 0 {
		return false
	}
	// The first span that could contain pos is the last one starting at or
	// before it.
	i := sort.Search(len(s), func(i int) bool { return s[i].start > pos })
	if i == 0 {
		return false
	}
	return pos < s[i-1].end
}

// deprecatedBodySpans returns the source spans of the defun/defmacro forms in
// exprs whose own docstring is deprecated.
//
// Suppression is keyed on source position rather than on node identity because
// the two ASTs an analyzer sees are two separate parses of the same bytes:
// LintFileWithContext parses pass.Exprs with the format-preserving parser,
// while pass.Semantics was analyzed over a plain parse (LintFileWithAnalysis).
// No *lisp.LVal is ever shared between them, so a node-identity set would
// silently match nothing. Byte offsets are assigned by the shared scanner and
// do agree.
//
// The returned spans are sorted and non-overlapping: the walk stops descending
// at the first deprecated definition it finds, so a deprecated definition
// nested inside another contributes no span of its own.
func deprecatedBodySpans(exprs []*lisp.LVal) byteSpans {
	var spans byteSpans
	for _, expr := range exprs {
		spans = collectDeprecatedSpans(expr, spans)
	}
	// The end is part of the ordering only so that two spans sharing a start
	// -- which disjoint spans cannot produce, but a fault-tolerant parse of
	// malformed source might -- still sort into one fixed order. sort.Slice is
	// not stable, and contains() reads the last span starting at or before the
	// offset, so an ambiguous order would be an ambiguous answer.
	sort.Slice(spans, func(i, j int) bool {
		if spans[i].start != spans[j].start {
			return spans[i].start < spans[j].start
		}
		return spans[i].end < spans[j].end
	})
	return spans
}

// collectDeprecatedSpans appends the span of every deprecated definition at or
// below v to spans.
//
// The traversal is hand-rolled rather than astutil.Walk because Walk stops at
// a quasiquote — but analysis.resolveTemplateSymbol resolves symbols inside
// templates and records References for them, so a macro's template body does
// produce references. Suppression has to cover the same source a reference can
// come from, or a deprecated macro whose template calls a deprecated function
// would report against itself.
func collectDeprecatedSpans(v *lisp.LVal, spans byteSpans) byteSpans {
	if v == nil {
		return spans
	}
	if isDeprecatedDefinition(v) {
		// A definition with untracked offsets yields no span: the check then
		// reports uses inside it, which is the conservative direction.
		if loc := astutil.SourceLoc(v); loc != nil && loc.EndPos > loc.Pos {
			return append(spans, byteSpan{start: loc.Pos, end: loc.EndPos})
		}
		return spans
	}
	for _, cell := range v.Cells {
		spans = collectDeprecatedSpans(cell, spans)
	}
	return spans
}

// isDeprecatedDefinition reports whether v is a defun/defmacro whose own
// docstring is deprecated. The docstring is read through
// analysis.DefunDocstring, the same function that fills the DocString this
// check reads off a reference's symbol -- so the definition a use is
// suppressed inside is judged by exactly the rule that deprecated it.
func isDeprecatedDefinition(v *lisp.LVal) bool {
	if v == nil || v.Type != lisp.LSExpr || v.IsQuoted() {
		return false
	}
	switch astutil.HeadSymbol(v) {
	case "defun", "defmacro":
	default:
		return false
	}
	_, ok := lisp.DeprecationNotice(analysis.DefunDocstring(v))
	return ok
}

// AnalyzerNames returns a sorted list of all default analyzer names.
func AnalyzerNames() []string {
	analyzers := DefaultAnalyzers()
	names := make([]string, len(analyzers))
	for i, a := range analyzers {
		names[i] = a.Name
	}
	sort.Strings(names)
	return names
}

// AnalyzerDoc returns a formatted documentation string for all analyzers.
func AnalyzerDoc() string {
	var b strings.Builder
	for _, a := range DefaultAnalyzers() {
		fmt.Fprintf(&b, "  %s\n", a.Name)
		lines := strings.Split(a.Doc, "\n")
		fmt.Fprintf(&b, "    %s\n\n", lines[0])
	}
	return b.String()
}
