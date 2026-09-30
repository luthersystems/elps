// Copyright © 2026 The ELPS authors

package astutil

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Entries identify one function (including its nested closures), or one
// package variable initializer, never an entire file. Every exception needs a
// reason, and unused exceptions fail the guard. Entries marked #766 are
// legacy walkers to move onto the code walker.
//
//nolint:gosec // G101: dispatch exception reasons, not credentials
var specialOpAllowlist = map[string]string{
	"analysis/analyzer.go:analyzer.prescanForm":                 "registers one selected package declaration using walker definition events",
	"analysis/analyzer.go:extractSetSymbolNode":                 "extracts the literal symbol operand of set, including explicit quote",
	"analysis/codewalk.go:analyzer.bindingForm":                 "excludes explicit call policies from application-defined grammar",
	"analysis/codewalk.go:analyzer.defineWalkSymbol":            "maps walker definition events to package symbol kinds",
	"analysis/codewalk.go:definitionScanner.visit":              "maps walker declaration events to symbol kinds",
	"analysis/codewalk.go:sourceResolver.visit":                 "maps walker scope and binding events to resolver scopes",
	"analysis/expander.go:evalPreambleForm":                     "selects definitions safe to evaluate when loading a macro preamble",
	"analysis/generated.go:analyzer.expandPackageForms":         "excludes file-defined non-macros and advances package context while expanding selected package forms",
	"analysis/generated.go:isPackageFormHead":                   "keeps declarations and package control calls out of prescan macro expansion",
	"elpsutil/template.go:templateCompiler.placeholderKind":     "reads (unquote name) and (unquote-splicing name) as template placeholder markers in a Go macro template; it never walks or evaluates code",
	"analysis/perf/local.go:ScanFile":                           "existing performance analyzer declaration scan; migrate separately (#766)",
	"analysis/perf/local.go:isCallable":                         "existing performance analyzer callable classification; migrate separately (#766)",
	"analysis/perf/local.go:scanExpr":                           "existing performance analyzer syntax traversal; migrate separately (#766)",
	"analysis/workspace.go:FindEnclosingFunction":               "finds the definition enclosing a cursor position",
	"analysis/workspace.go:SymbolKeyFromNameKind":               "translates declaration names to workspace symbol keys",
	"analysis/workspace.go:extractDefinitions":                  "extracts workspace declaration metadata, not expression traversal",
	"analysis/workspace.go:scanFileFull":                        "collects top-level workspace definitions",
	"astutil/exports.go:ExportNames":                            "extracts literal export operands, including explicit quote",
	"astutil/query.go:ClassifyNodes":                            "reads walker Op events and classifies quasiquote template holes",
	"astutil/walk.go:UserDefined":                               "legacy file-wide builtin-shadowing heuristic; migrate separately (#766)",
	"astutil/walk.go:quotedSymbolName":                          "extracts a quoted symbol operand, not expression dispatch",
	"astutil/walk.go:walkNode":                                  "legacy syntactic walk omits quasiquote templates; not scope resolution (#766)",
	"formatter/printer.go:printer.tryPrefixForm":                "prints reader prefixes for their explicit form spellings",
	"formatter/rules.go:DefaultRules":                           "form-specific indentation configuration",
	"internal/fuzzseed/evalseed.go:EvalTerminating":             "names test seeds after the forms they exercise",
	"lint/analyzers.go:aritySkipNodes":                          "legacy conservative file-local shadowing and threading exclusions (#766)",
	"lint/analyzers.go:checkLispBindings":                       "checks writes to the sealed lisp package",
	"lint/analyzers.go:isDeprecatedDefinition":                  "recognizes definition annotations",
	"lint/analyzers.go:lambdaCallback":                          "recognizes literal comparator/iteration callbacks",
	"lint/analyzers.go:letRecursionState.walk":                  "existing initializer recursion check; migrate separately (#766)",
	"lint/analyzers.go:mutationRun.indexNode":                   "indexes comparator definitions and quasiquote holes for mutation checks",
	"lint/analyzers.go:packageLiteralArg":                       "extracts literal package operands",
	"lint/analyzers.go:sameFileDefuns":                          "indexes named comparator definitions",
	"lint/analyzers.go:scanMacroTemplates":                      "reads walker events to find templates and enclosing handlers",
	"lint/analyzers.go:var AnalyzerBuiltinArity":                "reads quasiquote walker events to preserve the syntactic lint policy",
	"lint/analyzers.go:var AnalyzerCondMissingElse":             "checks cond clause exhaustiveness",
	"lint/analyzers.go:var AnalyzerCondStructure":               "validates malformed cond syntax before code walking",
	"lint/analyzers.go:var AnalyzerDefunStructure":              "validates malformed definition syntax before code walking",
	"lint/analyzers.go:var AnalyzerDuplicateBinding":            "validates duplicate lexical binding entries",
	"lint/analyzers.go:var AnalyzerIfArity":                     "checks if argument count",
	"lint/analyzers.go:var AnalyzerLambdaList":                  "validates lambda-list syntax",
	"lint/analyzers.go:var AnalyzerLetBindings":                 "validates malformed let binding syntax before code walking",
	"lint/analyzers.go:var AnalyzerPackageBuiltins":             "checks literal package builtin operands",
	"lint/analyzers.go:var AnalyzerUnnecessaryProgn":            "reports redundant progn inside implicit bodies",
	"lint/analyzers.go:var AnalyzerWithCleanupForms":            "validates cleanup-list syntax",
	"lint/analyzers.go:var bindingForms":                        "legacy arity shadowing exclusions; migrate separately (#766)",
	"lint/analyzers.go:var implicitPrognForms":                  "body offsets for redundant progn diagnostics",
	"lint/analyzers.go:var iterationMutatingTargets":            "callback argument configuration for mutating iteration calls",
	"lint/analyzers.go:var mutatingBuiltins":                    "mutation check classifies writes including set!",
	"lint/analyzers.go:var rethrowFunctionForms":                "function enclosure classification for generated macro templates",
	"lint/analyzers.go:var storingCalls":                        "classifies storage calls including set! for loop-capture diagnostics",
	"lint/analyzers.go:var testDefinitionForms":                 "test-registration vocabulary",
	"lint/analyzers.go:walkEvaluated":                           "legacy mutation check traversal; migrate separately (#766)",
	"lint/analyzers.go:walkLambdaListCalls":                     "legacy lambda-list validation traversal; migrate separately (#766)",
	"lint/analyzers.go:walkRethrowContext":                      "reads handler walker enclosures and definition events",
	"lint/analyzers.go:walkRethrowTemplate":                     "syntactic scan of generated code templates rather than evaluated code",
	"lint/analyzers.go:walkTemplate":                            "legacy mutation check template traversal; migrate separately (#766)",
	"lisp/codewalk.go:CodeWalker.let":                           "source initializer metadata in the shared syntax walker",
	"lisp/codewalk.go:CodeWalker.templateList":                  "template-hole grammar in the shared syntax walker",
	"lisp/codewalk.go:var formKinds":                            "the single registry of structural special forms",
	"lisp/inspect.go:InspectFunction":                           "runtime function introspection identifies expr values",
	"lisp/macro.go:getUnquoteType":                              "runtime quasiquote evaluator classifies its holes",
	"lisp/macro.go:var lateMacros":                              "special-form spellings in builtin macro registration",
	"lisp/op.go:var lateSpecialOps":                             "the runtime special-operator registry",
	"lisp/x/debugger/debugrepl/repl.go:debugHandler.handleLine": "debugger REPL command named help",
	"lsp/hover.go:lambdaCapturesHover":                          "recognizes a lambda walker event to display captures",
	"lsp/hover.go:lambdaHeadAt":                                 "recognizes the lambda token under the cursor",
	"lsp/hover.go:var keywordDocs":                              "documentation keyed by language keywords",
	"lsp/semantic_tokens.go:var readerPrefixHeads":              "semantic highlighting of reader prefixes",
	"lsp/semantic_tokens.go:var specialOps":                     "semantic token vocabulary, not structural traversal",
	"minifier/minifier.go:collectQuotedSymbols":                 "preserves quoted names across renaming",
	"minifier/minifier.go:firstDynamicEvaluation":               "recognizes runtime evaluation and quoted code",
	"minifier/minifier.go:firstGlobalFallback":                  "recognizes forms that can reference package globals",
	"minifier/minifier.go:preservePackageSurfaceSymbols":        "preserves names used by package-writing declarations",
	"minifier/minifier.go:scanProgramSymbols":                   "existing minifier binding analysis; migrate separately (#766)",
}

var specialFormNames = func() map[string]bool {
	names := map[string]bool{}
	for _, op := range lisp.DefaultSpecialOps() {
		names[op.Name()] = true
	}
	for _, name := range []string{"defun", "defmacro", "deftype", "test-let", "test-let*", "unquote", "unquote-splicing"} {
		names[name] = true
	}
	return names
}()

// specialFormDispatches finds decoded string literals used in comparisons,
// switch cases or map keys. KeyValueExpr also covers maps whose type is elided
// inside another composite literal. Go struct keys cannot be string literals.
func specialFormDispatches(node ast.Node) []token.Pos {
	seen := map[token.Pos]bool{}
	var found []token.Pos
	literals := func(n ast.Node) {
		ast.Inspect(n, func(n ast.Node) bool {
			lit, ok := n.(*ast.BasicLit)
			if !ok || lit.Kind != token.STRING {
				return true
			}
			value, err := strconv.Unquote(lit.Value)
			if err == nil && specialFormNames[strings.TrimPrefix(value, "lisp:")] && !seen[lit.Pos()] {
				seen[lit.Pos()] = true
				found = append(found, lit.Pos())
			}
			return true
		})
	}
	ast.Inspect(node, func(n ast.Node) bool {
		switch n := n.(type) {
		case *ast.BinaryExpr:
			if n.Op == token.EQL || n.Op == token.NEQ || n.Op == token.LSS || n.Op == token.LEQ || n.Op == token.GTR || n.Op == token.GEQ {
				literals(n.X)
				literals(n.Y)
			}
		case *ast.CaseClause:
			for _, expr := range n.List {
				literals(expr)
			}
		case *ast.KeyValueExpr:
			literals(n.Key)
		}
		return true
	})
	return found
}

func TestSpecialFormDispatchDetection(t *testing.T) {
	for _, expr := range []string{
		`x == "lambda"`, "x != `lisp:lambda`", `x == "\x6cambda"`,
		`func() { switch x { case "let", ` + "`lisp:let*`" + `: } }`,
		`map[string]int{"lambda": 1}`, "map[string]int{`lisp:lambda`: 1}",
		`map[string]map[string]int{"other": {"lambda": 1}}`,
	} {
		t.Run(expr, func(t *testing.T) {
			node, err := parser.ParseExpr(expr)
			require.NoError(t, err)
			require.NotEmpty(t, specialFormDispatches(node))
		})
	}
	for _, expr := range []string{`[]string{"lambda"}`, `f("lisp:lambda")`, `"lambda"`, `x == "example:lambda"`, `x == "(lambda (x) x)"`} {
		node, err := parser.ParseExpr(expr)
		require.NoError(t, err)
		require.Empty(t, specialFormDispatches(node), expr)
	}
	// An added dispatch inside an otherwise allowlisted file still has its own
	// function key; this is the regression the former whole-file guard missed.
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "analysis/codewalk.go", `package p; func newDispatch(x string) bool { return x == "lisp:lambda" }`, 0)
	require.NoError(t, err)
	require.Len(t, specialFormDispatches(file.Decls[0]), 1)
	require.NotContains(t, specialOpAllowlist, "analysis/codewalk.go:newDispatch")
}

func TestSpecialFormNamesStayInTheWalker(t *testing.T) {
	root, err := filepath.Abs("..")
	require.NoError(t, err)
	fset := token.NewFileSet()
	seen := map[string]bool{}
	check := func(path, name string, node ast.Node) {
		positions := specialFormDispatches(node)
		if len(positions) == 0 {
			return
		}
		key := path + ":" + name
		seen[key] = true
		reason, allowed := specialOpAllowlist[key]
		assert.True(t, allowed && reason != "", "%s dispatches special-form names; use CodeWalker or allowlist this function with a reason (first at %s)", key, fset.Position(positions[0]))
	}
	err = filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if strings.HasPrefix(d.Name(), ".") || d.Name() == "testdata" || d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, 0)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		rel = filepath.ToSlash(rel)
		for _, decl := range file.Decls {
			switch decl := decl.(type) {
			case *ast.FuncDecl:
				name := decl.Name.Name
				if decl.Recv != nil {
					typ := decl.Recv.List[0].Type
					if ptr, ok := typ.(*ast.StarExpr); ok {
						typ = ptr.X
					}
					if ident, ok := typ.(*ast.Ident); ok {
						name = ident.Name + "." + name
					}
				}
				check(rel, name, decl)
			case *ast.GenDecl:
				for _, spec := range decl.Specs {
					if v, ok := spec.(*ast.ValueSpec); ok {
						for i, value := range v.Values {
							index := min(i, len(v.Names)-1)
							check(rel, "var "+v.Names[index].Name, value)
						}
					}
				}
			}
		}
		return nil
	})
	require.NoError(t, err)
	for key := range specialOpAllowlist {
		assert.True(t, seen[key], "%s has no special-form dispatch; remove the exception", key)
	}
}
