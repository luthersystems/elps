// Copyright © 2026 The ELPS authors

package analysis

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/require"
)

// This snapshot was recorded before replacing the analyzer's recursive walk.
// It pins event order as well as scope ownership, source nodes and references;
// testing only unresolved names would miss changes in rename and unused checks.
func TestCodeWalkAnalysisParity(t *testing.T) {
	sources := []string{
		`(export 'f) (defun f (x &optional y &rest z &key k) "doc" "" "more" (+ x y z k)) (f 1)`,
		`(let ((x outer) (y (lambda (p) p))) (let* ((x x) (z y)) (+ x z)))`,
		`(flet ((f (x) (f x)) (g (y) (f y))) (f (g 1))) (labels ((f (x) (g x)) (g (y) (f y))) (g 2))`,
		`(macrolet ((m (x) unknown) (n (y) y)) (m unknown)) (defmacro m (x) (quasiquote (list x (unquote x) unknown))) (m unknown)`,
		`(dotimes (i limit ignored) (+ i unknown)) (handler-bind ((condition (lambda (c) c) extra) (short)) unknown)`,
		`(cond (true yes) (else no) [bracket omitted] malformed () ((pred x) y))`,
		`(quasiquote [f (unquote (f unknown)) (unquote-splicing missing) (quasiquote (unquote x)) '(unquote y)])`,
		`(function f ignored) (set! x value ignored) (qualified-symbol ignored extra) (quote omitted) #'f`,
		`(test "t" (set 'v 1) (deftype ty (x) x) v) (test-let "t" ((x 1) (y x)) (+ x y)) (test-let* "t" ((x 1) (y x)) y)`,
		`(deftype ty (x) x) (s:deftype "schema" unknown) (defendpoint ep (req) req) (custom name meta (arg) arg)`,
		`(set 'v 1) (lambda (v) (set 'v v)) (set target value ignored) (set '(a b) 1) (set 'fresh)`,
		`(in-package 'one) (export 'f) (defun f () 1) (in-package 'two) (defun f () 2) (f) (use-package 'lib) (external)`,
		`#^(+ % strange) (expr (+ %odd unknown))`,
		`(with-cleanup ((let ((x 1)) x)) unknown) (help f) (benchmark "b" (n) n) (thread-first x (lambda (p) p))`,
		`(let ((x (defun init () 1)) (y (set 'created 1))) (export 'created) (defun nested () x))`,
		`(defun) (defun f) (defun f invalid body) (lambda bad body) (let bad body) (flet bad body) (dotimes (i) body) (test-let "t" bad body)`,
		`(let ((x) (y 1 extra) bad)) (flet ((f) (g bad body) (1 () body))) (labels ((1 () body))) (macrolet ((m)))`,
	}
	// Qualification and malformed operands have historically been interpreted
	// differently by source analysis and the evaluator. Preserve those choices.
	for _, head := range []string{"defun", "defmacro", "lambda", "let", "let*", "flet", "labels", "dotimes", "function", "set!", "cond", "macrolet", "handler-bind", "qualified-symbol", "quasiquote", "expr", "test", "thread-first", "quote"} {
		for _, prefix := range []string{"", "lisp:"} {
			for _, args := range []string{"", " x", " x y", " (x) y z", " ((x y)) z"} {
				sources = append(sources, "("+prefix+head+args+")")
			}
		}
	}
	var snapshots []string
	for _, src := range sources {
		p := rdparser.New(token.NewScanner("parity.lisp", strings.NewReader(src)))
		exprs, err := p.ParseProgram()
		require.NoError(t, err, src)
		cfg := &Config{DefForms: []DefFormSpec{{Head: "custom", FormalsIndex: 3, BindsName: true, NameIndex: 1}}, PackageExports: map[string][]ExternalSymbol{"lib": {{Name: "external", Package: "lib", Kind: SymFunction}}}}
		snapshots = append(snapshots, src+"\n"+analysisSnapshot(Analyze(exprs, cfg)))
	}
	data, err := json.MarshalIndent(snapshots, "", "  ")
	require.NoError(t, err)
	data = append(data, '\n')
	path := "testdata/codewalk.golden.json"
	if os.Getenv("ELPS_UPDATE_CODEWALK_GOLDEN") == "1" {
		require.NoError(t, os.MkdirAll("testdata", 0o750))
		require.NoError(t, os.WriteFile(path, data, 0o600))
	}
	want, err := os.ReadFile(path)
	require.NoError(t, err)
	require.Equal(t, string(want), string(data))
}

func analysisSnapshot(r *Result) string {
	var out strings.Builder
	scopes := map[*Scope]int{}
	var visit func(*Scope)
	visit = func(s *Scope) {
		id := len(scopes)
		scopes[s] = id
		fmt.Fprintf(&out, "scope %d %s parent=%d node=%s\n", id, s.Kind, scopes[s.Parent], parityNode(s.Node))
		for _, child := range s.Children {
			visit(child)
		}
	}
	visit(r.RootScope)
	symbol := func(s *Symbol) string {
		sig := "-"
		if s.Signature != nil {
			sig = fmt.Sprint(s.Signature.Params)
		}
		return fmt.Sprintf("%s:%s %s scope=%d node=%s init=%s sig=%s doc=%q exported=%t external=%t refs=%d", s.Package, s.Name, s.Kind, scopes[s.Scope], parityNode(s.Node), parityNode(s.Init), sig, s.DocString, s.Exported, s.External, s.References)
	}
	for _, s := range r.Symbols {
		fmt.Fprintf(&out, "symbol %s\n", symbol(s))
	}
	// Scope maps also include imports and names overwritten by later bindings.
	var maps []string
	for s, id := range scopes {
		for key, v := range s.Symbols {
			if v.Source != nil || v.External {
				maps = append(maps, fmt.Sprintf("map %d %s %s", id, key, symbol(v)))
			}
		}
		for key, v := range s.PackageSymbols {
			maps = append(maps, fmt.Sprintf("map %d %s %s", id, key, symbol(v)))
		}
		for pkg, imports := range s.PackageImports {
			for key, v := range imports {
				maps = append(maps, fmt.Sprintf("import %d %s:%s %s", id, pkg, key, symbol(v)))
			}
		}
	}
	sort.Strings(maps)
	for _, m := range maps {
		fmt.Fprintln(&out, m)
	}
	for _, ref := range r.References {
		fmt.Fprintf(&out, "ref %s:%s scope=%d node=%s\n", ref.Symbol.Package, ref.Symbol.Name, scopes[ref.Symbol.Scope], parityNode(ref.Node))
	}
	for _, ref := range r.Unresolved {
		fmt.Fprintf(&out, "unresolved %s macro=%t node=%s\n", ref.Name, ref.InsideMacroCall, parityNode(ref.Node))
	}
	return out.String()
}

func parityNode(v *lisp.LVal) string {
	if v == nil {
		return "-"
	}
	loc, ok := v.Source()
	if !ok {
		return v.String()
	}
	return fmt.Sprintf("%s@%d:%d:%d", v, loc.Pos, loc.Line, loc.Col)
}

// Pin the expansion adapter's callback count and its full-call-tree depth cap.
// A chain-only cap would expand the nested-call case forever.
func TestCodeWalkMacroContext(t *testing.T) {
	for _, nested := range []bool{false, true} {
		t.Run(strconv.FormatBool(nested), func(t *testing.T) {
			exp := &parityMacroExpander{nested: nested}
			result := parseAndAnalyzeWithConfig(t, `(defmacro m (x) x) (m inside) outside`, &Config{MacroExpander: exp})
			require.Equal(t, maxMacroExpansionDepth, exp.calls)
			require.Equal(t, maxMacroExpansionDepth+2, result.RootScope.LookupInPackage("m", "user").References)
			require.Len(t, result.Unresolved, 2)
			require.Equal(t, "inside", result.Unresolved[0].Name)
			require.True(t, result.Unresolved[0].InsideMacroCall)
			require.Equal(t, "outside", result.Unresolved[1].Name)
			require.False(t, result.Unresolved[1].InsideMacroCall)
			for _, pkg := range exp.packages {
				require.Equal(t, "user", pkg)
			}
		})
	}
}

type parityMacroExpander struct {
	calls    int
	packages []string
	nested   bool
}

func (e *parityMacroExpander) ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal {
	e.calls++
	e.packages = append(e.packages, pkg)
	if e.nested {
		return lisp.SExpr([]*lisp.LVal{lisp.Symbol("progn"), form})
	}
	return form
}
