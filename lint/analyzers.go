// Copyright © 2024 The ELPS authors

package lint

import (
	"fmt"
	"sort"
	"strings"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
)

// AnalyzerSetUsage warns when `set` is used to reassign a symbol that was
// already bound by a prior `set` in the same file. The first `set` creating
// a binding is fine (ELPS has no `defvar`), but subsequent mutations of the
// same symbol should use `set!` to signal intent.
var AnalyzerSetUsage = &Analyzer{
	Name: "set-usage",
	Doc:  "Warn when `set` is used to reassign an already-bound symbol.\n\nThe first `set` creating a new binding is fine — ELPS has no `defvar`, so `set` is the standard way to create top-level bindings. However, subsequent `set` calls on the same symbol should use `set!` to clearly signal mutation intent.",
	Run: func(pass *Pass) error {
		seen := make(map[string]bool)
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
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
			} else if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
				name = arg.Cells[0].Str
			}
			if name == "" {
				return
			}
			if seen[name] {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("use set! instead of set to mutate '%s (already bound)", name),
					Pos:     posFromSource(src.Source),
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
	Name: "in-package-toplevel",
	Doc:  "Warn when `in-package` is used inside nested expressions.\n\n`in-package` only has meaningful effect at the top level of a file. Using it inside a `defun`, `let`, `lambda`, or other nested form is almost certainly a mistake.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) == "in-package" && depth > 0 {
				src := SourceOf(sexpr)
				pass.Reportf(src.Source, "in-package should only be used at the top level")
			}
		})
		return nil
	},
}

// AnalyzerIfArity checks that `if` has exactly 3 arguments (condition, then, else).
var AnalyzerIfArity = &Analyzer{
	Name: "if-arity",
	Doc:  "Check that `if` has exactly 3 arguments: condition, then-branch, else-branch.\n\nA missing else branch is a common source of subtle nil-return bugs. Extra arguments are silently ignored at parse time but indicate a structural error.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			if HeadSymbol(sexpr) != "if" {
				return
			}
			argc := ArgCount(sexpr)
			if argc == 3 {
				return
			}
			src := SourceOf(sexpr)
			if argc < 3 {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("if requires 3 arguments (condition, then, else), got too few (%d)", argc),
					Pos:     posFromSource(src.Source),
					Notes:   []string{"use cond for multi-branch conditionals, or provide an else branch"},
				})
			} else {
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("if requires 3 arguments (condition, then, else), got too many (%d)", argc),
					Pos:     posFromSource(src.Source),
					Notes:   []string{"if takes exactly (condition then-expr else-expr); use progn to group multiple expressions"},
				})
			}
		})
		return nil
	},
}

// AnalyzerLetBindings checks for malformed `let` and `let*` binding lists.
var AnalyzerLetBindings = &Analyzer{
	Name: "let-bindings",
	Doc:  "Check for malformed `let`/`let*` binding lists.\n\nThe first argument to `let` or `let*` must be a list of (symbol value) pairs. Common mistakes include forgetting the outer list: `(let (x 1) ...)` instead of `(let ((x 1)) ...)`.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			if head != "let" && head != "let*" {
				return
			}
			if ArgCount(sexpr) < 1 {
				src := SourceOf(sexpr)
				pass.Reportf(src.Source, "%s requires a binding list and body", head)
				return
			}
			bindings := sexpr.Cells[1]
			src := SourceOf(sexpr)

			// Bindings must be a list
			if bindings.Type != lisp.LSExpr {
				pass.Reportf(src.Source, "%s bindings must be a list, got %s", head, bindings.Type)
				return
			}

			// Each binding must be a 2-element list (symbol value)
			for i, binding := range bindings.Cells {
				if binding.Type != lisp.LSExpr {
					pass.Report(Diagnostic{
						Message: fmt.Sprintf("%s binding %d is not a list (did you forget the outer parentheses?)", head, i+1),
						Pos:     posFromSource(bindingSource(binding, src)),
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

// AnalyzerQuoteCall warns when set or defconst is called with an unquoted symbol
// as the first argument, which is almost always a mistake.
var AnalyzerQuoteCall = &Analyzer{
	Name: "quote-call",
	Doc:  "Warn when set is called with an unquoted symbol argument.\n\nThe first argument to set should be a quoted symbol: (set 'x 42). Writing (set x 42) evaluates x first, which is rarely intended. This check does not flag set!, which takes an unquoted symbol by design.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			if head != "set" && head != "defconst" {
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
			if arg.Type == lisp.LSymbol && !arg.Quoted {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s first argument should be quoted: (set '%s ...) not (set %s ...)", head, arg.Str, arg.Str),
					Pos:     posFromSource(src.Source),
					Notes:   []string{fmt.Sprintf("did you mean (%s '%s ...)?", head, arg.Str)},
				})
			}
		})
		return nil
	},
}

// AnalyzerCondMissingElse warns when a cond has no default (else or true) clause.
var AnalyzerCondMissingElse = &Analyzer{
	Name: "cond-missing-else",
	Doc:  "Warn when a cond expression has no default clause.\n\nWithout an else or (true ...) clause, cond returns nil when no condition matches. This is a common source of unexpected nil values. Add (else ...) or (true ...) as the last clause to handle the default case.",
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
				Pos:     posFromSource(src.Source),
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

func bindingSource(binding *lisp.LVal, fallback *lisp.LVal) *token.Location {
	if binding.Source != nil && binding.Source.Line > 0 {
		return binding.Source
	}
	return fallback.Source
}

// AnalyzerDefunStructure checks for malformed `defun` and `defmacro` forms.
var AnalyzerDefunStructure = &Analyzer{
	Name: "defun-structure",
	Doc:  "Check for malformed `defun`/`defmacro` definitions.\n\nA `defun` requires a symbol name and a formals list. An empty body (no-op) is valid. Common mistakes include non-symbol names or a non-list formals argument.",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			if head != "defun" && head != "defmacro" {
				return
			}
			src := SourceOf(sexpr)
			argc := ArgCount(sexpr)
			if argc < 2 {
				pass.Reportf(src.Source, "%s requires at least a name and formals list (got %d argument(s))", head, argc)
				return
			}
			name := sexpr.Cells[1]
			if name.Type != lisp.LSymbol {
				pass.Reportf(src.Source, "%s name must be a symbol, got %s", head, name.Type)
			}
			formals := sexpr.Cells[2]
			if formals.Type != lisp.LSExpr {
				pass.Reportf(src.Source, "%s formals must be a list, got %s", head, formals.Type)
			}
		})
		return nil
	},
}

// AnalyzerCondStructure checks for malformed `cond` clauses.
var AnalyzerCondStructure = &Analyzer{
	Name: "cond-structure",
	Doc:  "Check for malformed `cond` clauses.\n\nEach `cond` clause must be a non-empty list. The `else` clause, if present, must be last. Common mistakes include bare values instead of lists, or misplaced `else`.",
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
				if clauseSrc.Source == nil || clauseSrc.Source.Line == 0 {
					clauseSrc = src
				}

				if clause.Type != lisp.LSExpr {
					pass.Report(Diagnostic{
						Message: fmt.Sprintf("cond clause %d is not a list", i),
						Pos:     posFromSource(clauseSrc.Source),
						Notes:   []string{"cond clauses must be lists: (cond ((test1) body1) ((test2) body2) (else default))"},
					})
					continue
				}
				if len(clause.Cells) == 0 {
					pass.Reportf(clauseSrc.Source, "cond clause %d is empty", i)
					continue
				}

				// Check for misplaced else
				if clause.Cells[0].Type == lisp.LSymbol && isCondDefault(clause.Cells[0].Str) {
					if i != last {
						pass.Reportf(clauseSrc.Source, "cond else clause must be last (is clause %d of %d)", i, last)
					}
				}
			}
		})
		return nil
	},
}

// AnalyzerBuiltinArity checks for wrong argument counts to known builtin functions.
var AnalyzerBuiltinArity = &Analyzer{
	Name: "builtin-arity",
	Doc:  "Check argument counts for calls to known builtin functions and special forms.\n\nELPS builtin functions have well-defined argument signatures. This check catches calls with too few or too many arguments before runtime. User-defined functions that shadow builtin names are automatically excluded. Formals lists and threading macro children are also excluded.",
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
			if argc < spec.min {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s requires at least %d argument(s), got %d", head, spec.min, argc),
					Pos:     posFromSource(src.Source),
					Notes:   []string{helpNote},
				})
			}
			if spec.max >= 0 && argc > spec.max {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: fmt.Sprintf("%s accepts at most %d argument(s), got %d", head, spec.max, argc),
					Pos:     posFromSource(src.Source),
					Notes:   []string{helpNote},
				})
			}
		})
		return nil
	},
}

// aritySkipNodes returns a set of AST nodes that should be excluded from
// arity checking. This covers two cases:
//
//  1. Formals lists — (defun f (x y) ...) the (x y) is a parameter list,
//     not a function call.
//  2. Threading macro children — (thread-first v (get "key")) expands to
//     (get v "key"), so the static arg count is one less than the runtime count.
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
		min := 0
		max := 0
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
				max++
				if !inOptional && !inKey {
					min++
				}
			}
		}

		if variadic || inKey {
			table[name] = aritySpec{min: min, max: -1}
		} else {
			table[name] = aritySpec{min: min, max: max}
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
	Name: "rethrow-context",
	Doc:  "Warn when `rethrow` is used outside a `handler-bind` form.\n\n`rethrow` re-raises the current error being handled by handler-bind, preserving the original stack trace. Calling it outside any handler-bind always produces an error at runtime.",
	Run: func(pass *Pass) error {
		walkRethrowContext(pass.Exprs, 0, func(sexpr *lisp.LVal) {
			src := SourceOf(sexpr)
			pass.Report(Diagnostic{
				Message: "rethrow used outside handler-bind",
				Pos:     posFromSource(src.Source),
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
	if node.Type != lisp.LSExpr || node.Quoted || len(node.Cells) == 0 {
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
