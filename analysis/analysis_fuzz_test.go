// Copyright © 2026 The ELPS authors

package analysis

import (
	"bytes"
	"errors"
	"fmt"
	"testing"
	"time"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Fuzzing the ANALYSER on its own API, rather than through the language server.
//
// # Why a dedicated target, when FuzzLSPSession already reaches this package
//
// Issue #318 lists analysis/ as unfuzzed. FuzzLSPSession (PR #348) softened
// that: driving didOpen runs parse -> analysis -> lint synchronously, and the
// seed corpus alone measured 33.7% mean per-function coverage of this package.
// That is a downgrade, not a close, for three separate reasons.
//
//  1. THE LSP IS NOT THE ONLY CALLER. luthersystems/substrate imports
//     github.com/luthersystems/elps/analysis directly, as does lint/, the
//     minifier and the MCP server. Whatever the language server happens not to
//     construct is still constructible by those callers.
//
//  2. THE LSP CONSTRUCTS ONLY ONE SHAPE OF Config. Every field of
//     analysis.Config is attacker-influenced in production -- ExtraGlobals and
//     PackageSymbols come off disk, DefForms are DERIVED FROM MACRO NAMES in
//     workspace source (extractDefFormSpecs), PackageImports and
//     DefaultPackage from cross-file scans. The LSP builds them all from one
//     workspace scan, so the fuzzer never varies them independently and never
//     produces the combinations a caller with its own index would. DefForms is
//     the sharpest of these: FormalsIndex and NameIndex are INTEGER INDICES
//     into a fuzzer-chosen form's Cells, which is where an out-of-range slice
//     lives, and no LSP-driven input can set them to an arbitrary value.
//
//  3. THE POSITION QUERIES ARE THIS PACKAGE'S, NOT THE SERVER'S.
//     ScopeAtPosition and FindEnclosingFunction do their own line/column
//     arithmetic on a scope tree, and are exported for embedders.
//
// # The assertion with teeth: ExpansionPanics
//
// The analyser evaluates code. When Config.MacroExpander is set -- which the
// LSP, lint and the MCP server all do -- analysing a document CALLS
// env.MacroCall on macros loaded from the workspace, so a macro body runs
// while a file is merely being looked at. EnvMacroExpander.ExpandMacro wraps
// that in a blanket recover() whose recovery sets result = nil.
//
// nil is also the ordinary answer for "not a macro", and the analyser asks the
// expander about EVERY unknown head symbol, so nil is overwhelmingly the common
// return. #348 called this out and left it open: a panic in analysis-time macro
// expansion was invisible to any fuzzer, because there was no marker to key
// off, and closing it needed a change in this package rather than in the
// harness.
//
// EnvMacroExpander.ExpansionPanics is that change. It counts calls that failed
// to complete, keyed off a local variable in ExpandMacro's own frame that only
// a normal return can set -- non-forgeable in the same sense lisp.IsInternalPanic
// and the LSP harness's checkIndexBuilt are non-forgeable. This target asserts
// the count is zero after every input, which is the whole reason the target can
// claim to cover analysis-time evaluation at all.
//
// # What is deliberately kept out of reach
//
// NO FILESYSTEM. Filename is a fixed constant and Runtime.Library stays nil, so
// load-file cannot reach the disk while the preamble is being evaluated -- the
// same principle FuzzEval and FuzzLSPSession apply. The workspace-scanning half
// of this package needs real files and is fuzzed separately by
// FuzzWorkspaceScan, which owns the containment argument for that.
//
// SINGLE GOROUTINE. Nothing here spawns one, so a crasher reproduces from its
// own testdata file.
const (
	// Budgets for the injected LEnv. Analysis-time macro expansion evaluates
	// macro bodies, so this is arbitrary evaluation reachable from a source
	// file and needs FuzzEval's budgets. Same values as FuzzLSPSession's.
	analysisEnvMaxSteps          = 200_000
	analysisEnvMaxTailIterations = 10_000
	analysisEnvMaxPhysHeight     = 2_000
	analysisEnvMaxAlloc          = 1_000_000
	analysisEnvMacroDepth        = 100
	analysisEnvMaxEvalNesting    = 20_000

	// Fixed filenames. Never fuzzer-derived: they end up in token.Location and
	// in SymbolKey strings, and a fuzzer-chosen path is a step towards a
	// filesystem primitive for no coverage gain.
	fuzzAnalysisFile  = "fuzz-a.lisp"
	fuzzWorkspaceFile = "fuzz-b.lisp"

	// maxProbes bounds the position queries run against one result. Each walks
	// the scope tree, so an unbounded count would let one input spend the
	// deadline in the harness rather than in the code under test.
	maxProbes = 32

	// maxDefForms bounds how many DefFormSpecs are synthesised per input.
	maxDefForms = 4

	// watchdogTimeout is scheduled time, not wall clock (see internal/fuzzwatch).
	analysisWatchdogTimeout = 30 * time.Second
)

// ---------------------------------------------------------------------------
// harness
// ---------------------------------------------------------------------------

// discardWriter drops everything. The analyser may evaluate a macro body that
// prints; that must not spam the fuzzing log.
type discardWriter struct{}

func (discardWriter) Write(p []byte) (int, error) { return len(p), nil }

// countingExpander wraps EnvMacroExpander so the harness can tell how many
// expansions actually SUCCEEDED.
//
// This exists because of the lesson #348 recorded the hard way: setting
// cfg.MacroExpander looks sufficient for the macro-evaluation path to be
// covered, and is not. The analyser consults the expander only for a head
// symbol it does not already know, and a macro defined in the file under
// analysis is not in the env while a stdlib macro is not unknown -- so the only
// macro that reaches env.MacroCall is one that came from ANOTHER file. Without
// a count, a harness that never reaches MacroCall passes every test and looks
// identical to one that does. TestAnalyzeFuzzSeedsExpandMacros asserts the
// seeds reach it.
//
// It also forwards ExpansionPanics, so the target's assertion goes through the
// exported PanicReporter interface exactly as an embedder's would.
type countingExpander struct {
	inner    *EnvMacroExpander
	expanded int
	asked    int
}

func (c *countingExpander) ExpandMacro(form *lisp.LVal, pkg string) *lisp.LVal {
	c.asked++
	v := c.inner.ExpandMacro(form, pkg)
	if v != nil {
		c.expanded++
	}
	return v
}

func (c *countingExpander) ExpansionPanics() uint64 { return c.inner.ExpansionPanics() }

// newFuzzEnv builds the environment the expander runs in and replays preamble
// into it, mirroring what buildWorkspaceIndex and lint.BuildAnalysisConfig do:
// LoadWorkspaceMacros first, then the expander over the resulting env.
//
// Runtime.Library stays nil so load-file cannot reach the disk while the
// fuzzer-derived preamble is being EVALUATED.
func newFuzzEnv(preamble []*lisp.LVal) (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = rdparser.NewReader()
	if rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(discardWriter{}),
		lisp.WithMaxSteps(analysisEnvMaxSteps),
		lisp.WithMaxTailIterations(analysisEnvMaxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(analysisEnvMaxPhysHeight),
		lisp.WithMaxAlloc(analysisEnvMaxAlloc),
		lisp.WithMaxMacroExpansionDepth(analysisEnvMacroDepth),
		lisp.WithMaxEvalNesting(analysisEnvMaxEvalNesting),
	); rc.Type == lisp.LError {
		return nil, fmt.Errorf("InitializeUserEnv: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, fmt.Errorf("LoadLibrary: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, fmt.Errorf("InPackage: %v", rc)
	}
	// Evaluates fuzzer-derived forms, which is what production does with a
	// workspace preamble and the reason the env is budgeted.
	_ = LoadWorkspaceMacros(env, preamble)
	return env, nil
}

// preambleHeads are the top-level forms scanFileFull collects into
// Prescan.Preamble and LoadWorkspaceMacros then evaluates. Kept in step with
// the switch in workspace.go; TestFuzzPreambleHeadsMatchScan asserts it.
var preambleHeads = map[string]bool{
	"in-package": true, "use-package": true, "export": true,
	"defmacro": true, "defun": true, "set": true,
}

// fuzzPreamble extracts from src the forms a workspace scan would have
// collected out of a neighbouring file.
func fuzzPreamble(src []byte) []*lisp.LVal {
	sc := token.NewScanner(fuzzWorkspaceFile, bytes.NewReader(src))
	res := rdparser.New(sc).ParseProgramFaultTolerant()
	var preamble []*lisp.LVal
	for _, expr := range res.Exprs {
		if expr == nil || expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		if preambleHeads[astutil.HeadSymbol(expr)] {
			preamble = append(preamble, expr)
		}
	}
	return preamble
}

// script is the fuzzer's third input, read as a byte stream so that the same
// input always produces the same configuration.
type script struct {
	b []byte
	i int
}

func (s *script) next() byte {
	if len(s.b) == 0 {
		return 0
	}
	v := s.b[s.i%len(s.b)]
	s.i++
	return v
}

func (s *script) intn(n int) int {
	if n <= 0 {
		return 0
	}
	return int(s.next()) % n
}

func (s *script) bit() bool { return s.next()&1 == 1 }

// headSymbols collects the head symbol of every s-expression in exprs, at any
// depth, so that a synthesised DefFormSpec can name a form that is ACTUALLY
// PRESENT in the input. Specs naming a head that never occurs are free to
// generate and worthless: defLikeMatch returns false and analyzeDefLike is
// never entered. Measured, not assumed -- with a fixed vocabulary of heads the
// whole def-like surface stayed at 0%.
func headSymbols(exprs []*lisp.LVal) []string {
	var out []string
	seen := map[string]bool{}
	var walk func(v *lisp.LVal, depth int)
	walk = func(v *lisp.LVal, depth int) {
		if v == nil || depth > 32 || len(out) > 64 {
			return
		}
		if v.Type == lisp.LSExpr && len(v.Cells) > 0 {
			if h := astutil.HeadSymbol(v); h != "" && !seen[h] {
				seen[h] = true
				out = append(out, h)
			}
		}
		for _, c := range v.Cells {
			walk(c, depth+1)
		}
	}
	for _, e := range exprs {
		walk(e, 0)
	}
	return out
}

// buildDefForms synthesises DefFormSpecs from the script.
//
// FormalsIndex and NameIndex are the point of this: they are indices into a
// form's Cells that reach validDefFormSpec and analyzeDefLike, and production
// derives them from macro FORMALS found in workspace source
// (extractDefFormSpecs), so an embedder's index is as untrusted as the source
// it came from. Negative and past-the-end values are generated deliberately.
func buildDefForms(sc *script, heads []string) []DefFormSpec {
	n := sc.intn(maxDefForms + 1)
	specs := make([]DefFormSpec, 0, n)
	for range n {
		var head string
		if len(heads) > 0 && !sc.bit() {
			head = heads[sc.intn(len(heads))]
		} else {
			// A head that is probably absent, so the "no match" path is
			// exercised too.
			head = fmt.Sprintf("defthing%d", sc.intn(4))
		}
		specs = append(specs, DefFormSpec{
			Head: head,
			// -2..4: below, at and above the guard boundaries.
			FormalsIndex: sc.intn(7) - 2,
			BindsName:    sc.bit(),
			NameIndex:    sc.intn(7) - 2,
			NameKind:     SymbolKind(sc.intn(8)),
		})
	}
	return specs
}

// groupByPackage turns a flat symbol list into the package-keyed maps
// Config.PackageExports and Config.PackageSymbols expect.
func groupByPackage(syms []ExternalSymbol) map[string][]ExternalSymbol {
	m := map[string][]ExternalSymbol{}
	for _, s := range syms {
		m[s.Package] = append(m[s.Package], s)
	}
	return m
}

// analyzeResult is what one harness run reports back for the guard tests.
type analyzeResult struct {
	asked    int
	expanded int
	symbols  int
	refs     int
}

// runAnalyze is the body of FuzzAnalyzeSource. It returns an error rather than
// calling t.Fatalf so the guard tests can drive it too.
func runAnalyze(src, wsSrc, scriptBytes []byte) (analyzeResult, error) {
	var stats analyzeResult
	sc := &script{b: scriptBytes}

	env, err := newFuzzEnv(fuzzPreamble(wsSrc))
	if err != nil {
		return stats, fmt.Errorf("harness: %w", err)
	}
	exp := &countingExpander{inner: &EnvMacroExpander{Env: env}}
	if sc.bit() {
		// What an embedder does after loading more macros into a reused env.
		// Also proves Reset does not clear the abort record.
		exp.inner.Reset()
	}

	// The neighbouring file supplies the cross-file half of the config, the
	// way a workspace scan would. ExtractFileDefinitions runs the same
	// scanFileFull path the scanner uses.
	wsDefs := ExtractFileDefinitions(wsSrc, fuzzWorkspaceFile)

	// Parse the file under analysis fault-tolerantly, so malformed input still
	// produces the partial and half-built ASTs the analyser has to survive.
	// (AnalyzeFile, exercised below, takes the strict parser instead and
	// returns nil on a parse error, so on its own it would never see them.)
	res := rdparser.New(token.NewScanner(fuzzAnalysisFile, bytes.NewReader(src))).ParseProgramFaultTolerant()
	exprs := res.Exprs
	heads := headSymbols(exprs)

	pkgExports := ExtractPackageExports(env.Runtime.Registry)
	for pkg, syms := range groupByPackage(wsDefs) {
		pkgExports[pkg] = append(pkgExports[pkg], syms...)
	}

	defaultPkg := ""
	switch sc.intn(4) {
	case 1:
		defaultPkg = lisp.DefaultUserPackage
	case 2:
		defaultPkg = "demo"
	case 3:
		if len(wsDefs) > 0 {
			defaultPkg = wsDefs[sc.intn(len(wsDefs))].Package
		}
	}

	pkgImports := map[string][]string{}
	if sc.bit() {
		for _, s := range wsDefs {
			pkgImports[s.Package] = appendUnique(pkgImports[s.Package], lisp.DefaultUserPackage, "demo")
		}
		pkgImports[lisp.DefaultUserPackage] = appendUnique(pkgImports[lisp.DefaultUserPackage], "demo")
	}

	cfg := &Config{
		ExtraGlobals:   wsDefs,
		PackageExports: pkgExports,
		PackageSymbols: groupByPackage(wsDefs),
		DefForms:       buildDefForms(sc, heads),
		PackageImports: pkgImports,
		DefaultPackage: defaultPkg,
		Filename:       fuzzAnalysisFile,
	}
	if sc.intn(8) != 0 {
		// Usually on -- 7 inputs in 8. This is the path that EVALUATES macro
		// bodies and the one the ExpansionPanics assertion exists for, so it
		// must not be a coin flip; the off case exists only to keep the
		// no-expander branch of analyzeCall alive.
		cfg.MacroExpander = exp
	}

	// Cross-file references, built from the neighbouring file the same way
	// ScanWorkspaceRefs builds them.
	if sc.bit() {
		wsRes := Analyze(fuzzPreambleExprs(wsSrc), &Config{
			ExtraGlobals:   wsDefs,
			PackageExports: pkgExports,
			Filename:       fuzzWorkspaceFile,
		})
		refs := ExtractFileRefs(wsRes, fuzzWorkspaceFile)
		wsRefs := map[string][]FileReference{}
		for _, r := range refs {
			key := r.SymbolKey.String()
			wsRefs[key] = append(wsRefs[key], r)
		}
		cfg.WorkspaceRefs = wsRefs
	}

	result := Analyze(exprs, cfg)
	if result == nil {
		return stats, errors.New("Analyze returned nil; it has a single return and always builds a Result")
	}
	if result.RootScope == nil {
		return stats, errors.New("Analyze returned a Result with no RootScope")
	}

	// AnalyzeFile on the same bytes: a different entry point with its own
	// config copy and the strict parser.
	_ = AnalyzeFile(src, fuzzAnalysisFile, cfg)

	stats.asked, stats.expanded = exp.asked, exp.expanded
	stats.symbols, stats.refs = len(result.Symbols), len(result.References)

	probeResult(sc, result, src)

	// THE ASSERTION THIS TARGET EXISTS FOR. Reached through the exported
	// PanicReporter interface, as an embedder would.
	var reporter PanicReporter = exp
	if n := reporter.ExpansionPanics(); n > 0 {
		rec := exp.inner.LastExpansionPanic()
		return stats, fmt.Errorf(
			"EnvMacroExpander swallowed %d panic(s) during analysis-time macro expansion."+
				" ExpandMacro's recover() returns nil, which is also the ordinary"+
				" \"not a macro\" answer, so this is a Go panic in host code that no"+
				" return value could have reported.\n  macro:   %s\n  package: %s\n"+
				"  value:   %v\n  stack:\n%s",
			n, rec.Macro, rec.Package, rec.Value, rec.GoStack)
	}
	return stats, nil
}

// fuzzPreambleExprs parses the neighbouring file in full (not just its
// preamble) for the cross-file reference pass.
func fuzzPreambleExprs(src []byte) []*lisp.LVal {
	return rdparser.New(token.NewScanner(fuzzWorkspaceFile, bytes.NewReader(src))).
		ParseProgramFaultTolerant().Exprs
}

// probeResult runs the position and lookup queries an embedder makes against a
// Result. These have their own arithmetic over the scope tree and are exported
// for callers that never touch the LSP.
func probeResult(sc *script, result *Result, src []byte) {
	lines := bytes.Split(src, []byte("\n"))
	width := 1
	for _, ln := range lines {
		if len(ln) > width {
			width = len(ln)
		}
	}

	for range maxProbes {
		var line, col int
		switch sc.intn(4) {
		case 0:
			// Absurd but legal: an embedder converting from a 32-bit client
			// position can hand these straight through.
			line, col = 1<<20+int(sc.next()), 1<<20+int(sc.next())
		case 1:
			// Zero and negative: 1-based APIs called with 0-based values, the
			// single most common embedder mistake.
			line, col = sc.intn(3)-1, sc.intn(3)-1
		case 2:
			line, col = int(sc.next()), int(sc.next())
		default:
			// Mostly inside the document, so the queries resolve to real
			// scopes and reach the interesting branches.
			line = sc.intn(len(lines)+2) + 1
			col = sc.intn(width+2) + 1
		}

		scope := ScopeAtPosition(result.RootScope, line, col)
		_ = FindEnclosingFunction(result.RootScope, line, col)
		if scope == nil {
			continue
		}
		_ = scope.Kind.String()

		name := ""
		if len(result.Symbols) > 0 {
			name = result.Symbols[sc.intn(len(result.Symbols))].Name
		}
		pkg := lisp.DefaultUserPackage
		if len(result.ExtraGlobals) > 0 {
			pkg = result.ExtraGlobals[sc.intn(len(result.ExtraGlobals))].Package
		}
		_ = scope.Lookup(name)
		_ = scope.LookupLocal(name)
		_ = scope.LookupAllLocal(name)
		_ = scope.LookupInPackage(name, pkg)
		_ = scope.LookupLocalInPackage(name, pkg)
		_ = scope.LookupLocalVisible(name, pkg)
	}

	// Result consumers: everything a caller does with what it got back.
	for _, ref := range ExtractFileRefs(result, fuzzAnalysisFile) {
		_ = ref.SymbolKey.String()
	}
	for _, sym := range result.Symbols {
		_ = SymbolToKey(sym).String()
		_ = SymbolKeyFromNameKind(sym.Name, sym.Kind.String()).String()
		if sym.Signature != nil {
			_ = sym.Signature.MinArity()
			_ = sym.Signature.MaxArity()
		}
	}
	for _, u := range result.Unresolved {
		_ = u.Name
	}
	if len(result.ExtraGlobals) > 0 {
		_ = PreferredDefinition(result.ExtraGlobals)
		dup := append([]ExternalSymbol(nil), result.ExtraGlobals...)
		SortDefinitions(dup)
		_ = FindExternalSymbol(groupByPackage(dup), dup[0].Package, dup[0].Name)
	}
}

// ---------------------------------------------------------------------------
// seeds
// ---------------------------------------------------------------------------

// Real lisp, so analysis runs on well-formed input as well as on noise.
const (
	fuzzSeedDefs = `(in-package 'demo)
(use-package 'lisp)
(defun add (a b) "add two" (+ a b))
(defmacro twice (x) (quasiquote (+ (unquote x) (unquote x))))
(defmacro defthing (name args &rest body)
  (quasiquote (defun (unquote name) (unquote args) (unquote-splicing body))))
(defun caller (n) (add n (twice n)))
(export 'add)
(export 'twice)`

	fuzzSeedUses = `(in-package 'user)
(use-package 'demo)
(defun main () (add 1 2))
(defthing helper (x) (+ x 1))
(let ([a 1] [b 2]) (+ a b))
(labels ([f (x) (g x)] [g (x) x]) (f 1))`
)

// macroSeeds are the inputs whose whole job is to reach env.MacroCall: a macro
// that is in the env (because the neighbouring file defined it and
// LoadWorkspaceMacros replayed it) but unknown to the analyser. Nothing else
// gets there. See countingExpander.
var macroSeeds = [][2]string{
	{"(in-package 'demo)\n(twice (twice 3))",
		"(in-package 'demo)\n(defmacro twice (x) (quasiquote (+ (unquote x) (unquote x))))"},
	// Expands to a call to itself; bounded by the env's macro-expansion depth.
	{"(in-package 'demo)\n(loopy 1)",
		"(in-package 'demo)\n(defmacro loopy (x) (quasiquote (loopy (unquote x))))"},
	// Macro body raises a lisp error while being expanded during analysis.
	{"(in-package 'demo)\n(boom 1)",
		"(in-package 'demo)\n(defmacro boom (x) (error 'macro-boom \"x\"))"},
	// Macro body loops; the env's step budget is what stops it.
	{"(in-package 'demo)\n(spin 1)",
		"(in-package 'demo)\n(defmacro spin (x) (foldl (lambda (a b) (+ a b)) 0 (range 0 1000000)))"},
	// Macro expanding to a binding form, so the analyser resolves symbols the
	// macro introduced rather than treating the call as opaque.
	{"(in-package 'demo)\n(with-x 5 (+ x 1))",
		"(in-package 'demo)\n(defmacro with-x (v &rest body) (quasiquote (let ([x (unquote v)]) (unquote-splicing body))))"},
	// A preamble whose defmacro FAILS to evaluate. LoadWorkspaceMacros then
	// takes its error branch and names the macro (preambleDefName), which no
	// well-formed preamble reaches.
	{"(in-package 'demo)\n(broken 1)",
		"(in-package 'demo)\n(defmacro broken 1 2)\n(defmacro)\n(set)\n(use-package 'no-such-package)"},
}

// analyzeScripts are the seed configurations. Chosen to hit distinct Config
// shapes rather than to be pretty: the first byte drives the DefForm count and
// later bytes the flags. Package-level so the guard tests can drive the same
// set the seeds use -- a guard that invents its own script proves nothing about
// the corpus.
var analyzeScripts = [][]byte{
	{0, 0, 0, 0, 0, 0, 0, 0},
	{4, 1, 2, 3, 1, 0, 1, 2, 3, 4, 1, 1, 0, 0, 1, 1},
	{2, 3, 0, 1, 255, 254, 7, 9, 3, 3, 3, 3},
	{1, 0, 3, 1, 1, 1, 1, 1, 2, 2, 2, 2, 128, 64, 32, 16},
	{255, 255, 255, 255, 255, 255, 255, 255},
}

// FuzzAnalyzeSource analyses fuzzer-chosen source against a fuzzer-chosen
// Config, with a neighbouring file supplying the cross-file symbols and the
// macros that analysis-time expansion evaluates.
//
// Asserted, and only this:
//
//  1. No Go panic escapes Analyze, AnalyzeFile, or any of the result queries.
//     Nothing recovers between them and the fuzz function, so this is an
//     ordinary crash the engine attributes to its input.
//  2. EnvMacroExpander did not swallow a panic (ExpansionPanics). This is the
//     one that needs a marker, because that recover's nil is indistinguishable
//     from the routine answer.
//  3. Analyze returns a Result with a RootScope. It has a single return
//     statement and always builds one, so a nil means control left another way.
//  4. The run terminates (the watchdog). Interpreter budgets bound the macro
//     bodies; nothing bounds a loop in the analyser itself.
//
// NOT asserted: that any symbol resolves, that Unresolved is empty, or that a
// position query finds anything. Malformed source with nothing resolvable is
// the normal case for a fuzzer and the right answer is an empty result.
func FuzzAnalyzeSource(f *testing.F) {
	add := func(src, ws string, sc []byte) { f.Add([]byte(src), []byte(ws), sc) }

	scripts := analyzeScripts
	for _, sc := range scripts {
		add(fuzzSeedUses, fuzzSeedDefs, sc)
		add(fuzzSeedDefs, fuzzSeedUses, sc)
	}

	for _, ms := range macroSeeds {
		for _, sc := range scripts {
			add(ms[0], ms[1], sc)
		}
	}

	// Position-arithmetic shapes: no trailing newline, CRLF, a lone CR, tabs
	// and multi-byte runes. ScopeAtPosition indexes by line and column, so a
	// rune boundary is where that arithmetic goes wrong.
	for _, src := range []string{
		"", "\n\n\n", "(defun f (x) x)", "(defun f (x) x)\r\n(f 1)\r\n",
		"(defun f (x) x)\r(f 1)", "(defun éèê (x) x)\n(éèê 1)",
		"\t\t(defun f (x)\n\t\tx)", "(defun f (x", ";; only a comment",
		"(deftype point (x y) x)", "(set 'g 1)\n(set! g 2)",
		"(cond ((true) 1) (:else 2))", "(flet ([h (x) x]) (h 1))",
		"(dotimes (i 3) i)", "(macrolet ([m (x) x]) (m 1))",
		"(handler-bind ((condition (lambda (c) c))) (error 'x \"y\"))",
		"(defun f (x) x) ; nolint:unused-variable",
		"a:b\nfoo:bar\n:kw\n", "(lisp:car '(1 2))",
		// One per remaining arm of analyzeSExpr's head switch. Every one of
		// these was added because MEASUREMENT said the arm was at 0% from the
		// seeds, not because reading the switch suggested it -- the same way
		// #348 found its three silent harness bugs. The arms are keyed on exact
		// head symbols, so a seed corpus without the literal spelling never
		// enters them however much lisp it contains.
		"(test \"name\" (assert true))",
		"(test-let ([a 1]) a)",
		"(test-let* ([a 1] [b a]) b)",
		"(function car)",
		"(qualified-symbol 'demo 'add)",
		"(s:deftype \"my-type\" (or 'a 'b))",
		"(expr (+ % 1))",
		"(thread-first 1 (+ 2) (* 3))",
		"(thread-last 1 (+ 2))",
		"(defthing empty () 1)", // empty formals, for isEmptyFormals
		"(let* ([a 1] [b a]) b)",
	} {
		add(src, fuzzSeedDefs, scripts[1])
	}

	// Parser-adversarial text: a fault-tolerant parse of these yields the
	// partial and malformed ASTs the analyser then has to survive. Small by
	// construction, so they do not throttle the generations descended from
	// them. LispSources() is deliberately not used, for the same reason.
	for _, seed := range fuzzseed.Adversarial() {
		add(string(seed), fuzzSeedDefs, scripts[0])
		add(fuzzSeedUses, string(seed), scripts[2])
	}

	f.Fuzz(func(t *testing.T, src, wsSrc, scriptBytes []byte) {
		runAnalyzeBudgeted(t, src, wsSrc, scriptBytes)
	})
}

// fatalf is the subset of *testing.T the budgeted runners use, so the guard
// tests can pass something else.
type fatalf interface {
	Helper()
	Fatalf(format string, args ...interface{})
	Skipf(format string, args ...interface{})
}

// runAnalyzeBudgeted runs one analysis on its own goroutine under the
// watchdog. A hang is a real possibility: the interpreter budgets bound macro
// bodies, but nothing bounds a loop in the analyser itself.
func runAnalyzeBudgeted(t fatalf, src, wsSrc, scriptBytes []byte) {
	t.Helper()

	type outcome struct {
		err error
	}
	done := make(chan outcome, 1)
	go func() {
		_, err := runAnalyze(src, wsSrc, scriptBytes)
		done <- outcome{err}
	}()

	budget := fuzzwatch.New(analysisWatchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case o := <-done:
			if o.err != nil {
				t.Fatalf("%v\n--- src (%d bytes) ---\n%q\n--- workspace (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					o.err, len(src), src, len(wsSrc), wsSrc, len(scriptBytes), scriptBytes)
			}
			return
		case <-time.After(wait):
			verdict, more, report := budget.Check()
			switch verdict {
			case fuzzwatch.Continue:
				wait = more
			case fuzzwatch.Inconclusive:
				t.Skipf("no verdict: the process was starved throughout (%s)", report)
				return
			default:
				t.Fatalf("analysis did not terminate within %s of SCHEDULED time (%s)."+
					" Macro bodies run under interpreter budgets, but nothing bounds a"+
					" loop in the analyser itself"+
					"\n--- src (%d bytes) ---\n%q\n--- workspace (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					budget.Total(), report, len(src), src, len(wsSrc), wsSrc,
					len(scriptBytes), scriptBytes)
				return
			}
		}
	}
}
