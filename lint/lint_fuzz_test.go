// Copyright © 2026 The ELPS authors

package lint

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"testing"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Fuzzing the LINTER on its own API, rather than through the language server.
//
// # Why a dedicated target, when FuzzLSPSession already reaches this package
//
// Issue #318 lists lint/ as unfuzzed. FuzzLSPSession (PR #348) reaches it
// transitively -- didOpen runs every registered analyzer synchronously -- and
// measured 38.7% mean per-function coverage of this package from its seed
// corpus. What that path does NOT reach is most of the package's public
// surface, and the gap is not incidental:
//
//   - THE LSP RUNS ONE FIXED ANALYZER SET. It builds a Linter from
//     DefaultAnalyzers() and never narrows it. But `elps lint --checks=a,b`
//     does, and narrowing turns on a whole block of logic in
//     LintFileWithContext: `narrowedRun`, the conditional staleness rules that
//     govern suppression directives, and the unknown-analyzer message. From
//     the LSP seeds that block is dead code.
//
//   - THE LSP NEVER CALLS THE CLI ENTRY POINTS. LintFiles, BuildAnalysisConfig
//     and defaultStdlibExports measured 0%, as did every output and severity
//     function -- FormatText, FormatJSON, ParseSeverity, MaxSeverity,
//     ShouldFail, Severity.MarshalJSON/UnmarshalJSON. Those decide the exit
//     code of `elps lint` and the bytes a CI job parses.
//
//   - luthersystems/substrate IMPORTS THIS PACKAGE DIRECTLY. The linter runs
//     over customer-supplied phylum source, not only over a file someone opened
//     in an editor.
//
// # The assertions
//
// Panics are the primary one and need no marker: nothing recovers between an
// analyzer's Run and the fuzz function. Two more are worth the cost.
//
// SERIALISABILITY. Diagnostics are on their way out as JSON -- `elps lint
// --format=json` is what a CI job reads, and the LSP converts the same structs
// into protocol.Diagnostic. A diagnostic that cannot be marshalled is a broken
// run reported as a clean one, so FormatJSON's error is checked rather than
// discarded.
//
// DETERMINISM. LintFileWithContext builds its unused-nolint diagnostics by
// RANGING OVER A MAP (nolintMap) and then sorts the combined slice with
// sort.Slice -- which is not stable -- on (file, line) alone. Two diagnostics
// on one line therefore have no defined order. Linting the same bytes twice and
// comparing is a cheap, honest test of a property the CLI's output contract
// depends on, and it is exactly the sort of thing no unit test with a
// hand-written fixture would notice.
const (
	// Budgets for the injected LEnv, from FuzzLSPSession. LintFileWithAnalysis
	// runs semantic analysis, which with a MacroExpander set EVALUATES macro
	// bodies loaded from the workspace.
	lintEnvMaxSteps          = 200_000
	lintEnvMaxTailIterations = 10_000
	lintEnvMaxPhysHeight     = 2_000
	lintEnvMaxAlloc          = 1_000_000
	lintEnvMacroDepth        = 100
	lintEnvMaxEvalNesting    = 20_000

	// Fixed names. Never fuzzer-derived: filenames reach os.ReadFile in
	// LintFiles, and a fuzzer-chosen path there is a file-read primitive
	// pointed at the machine running the tests.
	lintFileA = "fuzz-a.lisp"
	lintFileB = "fuzz-b.lisp"

	// Cap on bytes written into the temp workspace per input.
	lintMaxFileBytes = 64 << 10

	lintWatchdogTimeout = 30 * time.Second
)

// discardWriter drops everything a macro body prints during analysis.
type discardWriter struct{}

func (discardWriter) Write(p []byte) (int, error) { return len(p), nil }

// script reads the fuzzer's third input as a byte stream, so one input always
// produces one configuration.
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

// newLintEnv builds the environment semantic analysis expands macros in.
// Runtime.Library stays nil so load-file cannot reach the disk.
func newLintEnv() (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = rdparser.NewReader()
	if rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(discardWriter{}),
		lisp.WithMaxSteps(lintEnvMaxSteps),
		lisp.WithMaxTailIterations(lintEnvMaxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(lintEnvMaxPhysHeight),
		lisp.WithMaxAlloc(lintEnvMaxAlloc),
		lisp.WithMaxMacroExpansionDepth(lintEnvMacroDepth),
		lisp.WithMaxEvalNesting(lintEnvMaxEvalNesting),
	); rc.Type == lisp.LError {
		return nil, fmt.Errorf("InitializeUserEnv: %v", rc)
	}
	if rc := lisplib.LoadLibrary(env); rc.Type == lisp.LError {
		return nil, fmt.Errorf("LoadLibrary: %v", rc)
	}
	if rc := env.InPackage(lisp.String(lisp.DefaultUserPackage)); rc.Type == lisp.LError {
		return nil, fmt.Errorf("InPackage: %v", rc)
	}
	return env, nil
}

// buildLinter draws an analyzer subset from the script.
//
// The subset is the point: `elps lint --checks=...` narrows the set, and
// narrowing is what turns on LintFileWithContext's `narrowedRun` branch and the
// conditional nolint-staleness rules. The full set and the empty set are both
// reachable (an empty --checks is a user error the CLI passes straight
// through), and so is a Linter with a nil Run, which is what a
// half-constructed embedder Analyzer looks like.
func buildLinter(sc *script) *Linter {
	all := DefaultAnalyzers()
	var chosen []*Analyzer
	switch sc.intn(8) {
	case 0:
		chosen = nil
	case 1:
		chosen = all
	default:
		for _, a := range all {
			if sc.bit() {
				chosen = append(chosen, a)
			}
		}
		if len(chosen) == 0 {
			chosen = all[:1]
		}
	}
	// An EMBEDDER-REGISTERED analyzer, which is the case substrate is. It is
	// also the only way Pass.ReportNode and Pass.ReportWithNotes get called at
	// all: measurement said both sat at 0%, and grepping the repository
	// explained why -- nothing in elps calls either one. They are exported API
	// that exists solely for callers outside this module, so a target that
	// covers only elps' own analyzers leaves two public functions unfuzzed
	// while reporting a healthy number.
	//
	// Registering one also exercises the Linter's handling of a name that is
	// NOT in DefaultAnalyzers(): the knownAnalyzers merge, enabledAnalyzers,
	// and the narrowedRun computation all read both lists.
	switch sc.intn(4) {
	case 0:
		chosen = append(chosen, embedderAnalyzer(sc))
	case 1:
		chosen = append(chosen, failingAnalyzer())
	}
	return &Linter{Analyzers: chosen}
}

// embedderAnalyzer reports through every Pass.Report* method, at nodes the
// script chooses.
func embedderAnalyzer(sc *script) *Analyzer {
	stride := 1 + sc.intn(7)
	sev := []Severity{severityUnset, SeverityError, SeverityWarning, SeverityInfo}[sc.intn(4)]
	return &Analyzer{
		Name:     "fuzz-embedder",
		Doc:      "reports through every Pass method, so all four are fuzzed",
		Severity: SeverityWarning,
		Run: func(pass *Pass) error {
			n := 0
			WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
				n++
				if n%stride != 0 {
					return
				}
				// The message prefixes are load-bearing: they are how
				// TestLintFuzzEmbedderAnalyzerReports tells which Pass method
				// produced a diagnostic, and therefore how it can assert that
				// all four were actually used.
				switch n % 3 {
				case 0:
					pass.ReportNode(sexpr, msgReportNode+" on %s/%d", HeadSymbol(sexpr), depth)
				case 1:
					pass.ReportWithNotes(Diagnostic{
						Message:  msgReportWithNotes,
						Pos:      posFromSource(astutil.SourceLoc(SourceOf(sexpr))),
						Severity: sev,
					}, "first note", "")
				default:
					pass.Reportf(astutil.SourceLoc(SourceOf(sexpr)), msgReportf+" at depth %d", depth)
				}
			})
			// A node with no source at all, which is what a synthesised or
			// macro-expanded form looks like to an embedder.
			pass.ReportNode(nil, msgReportNode+" with no node")
			pass.ReportNode(lisp.Symbol("synthetic"), msgReportNode+" on a sourceless symbol")
			pass.Reportf(nil, msgReportf+" with no location")
			pass.Report(Diagnostic{Message: msgReport})
			return nil
		},
	}
}

// failingAnalyzer returns an error, which is what a half-configured embedder
// check does. LintFileWithContext wraps and returns it, abandoning the run.
func failingAnalyzer() *Analyzer {
	return &Analyzer{
		Name:     "fuzz-failing",
		Doc:      "always fails",
		Severity: SeverityError,
		Run:      func(*Pass) error { return errAnalyzerFailed },
	}
}

var errAnalyzerFailed = errors.New("deliberate analyzer failure")

// Message prefixes identifying which Pass method produced a diagnostic.
const (
	msgReport          = "embedder Report"
	msgReportWithNotes = "embedder ReportWithNotes"
	msgReportNode      = "embedder ReportNode"
	msgReportf         = "embedder Reportf"
)

// severityStrings feed ParseSeverity, which parses a --fail-on flag.
var severityStrings = []string{
	"error", "warning", "info", "hint", "ERROR", "Warning", "", "none",
	"warn", "0", "-1", "9999",
}

// lintStats is what one run reports back, for the guard tests.
type lintStats struct {
	syntactic int
	semantic  int
	workspace int
	analyzers int
}

func runLint(root string, src, wsSrc, scriptBytes []byte) (lintStats, error) {
	var stats lintStats
	sc := &script{b: scriptBytes}

	if len(src) > lintMaxFileBytes {
		src = src[:lintMaxFileBytes]
	}
	if len(wsSrc) > lintMaxFileBytes {
		wsSrc = wsSrc[:lintMaxFileBytes]
	}

	linter := buildLinter(sc)
	stats.analyzers = len(linter.Analyzers)

	// --- syntactic: what `elps lint` does without --workspace ---------------
	diags, err := linter.LintFile(src, lintFileA)
	if err == nil {
		stats.syntactic = len(diags)
		if err := checkDiagnostics(diags, "LintFile"); err != nil {
			return stats, err
		}
		// Determinism. Same bytes, same linter, same answer -- see the header.
		again, err2 := linter.LintFile(src, lintFileA)
		if err2 != nil {
			return stats, fmt.Errorf("LintFile succeeded then failed on the same input: %w", err2)
		}
		if !reflect.DeepEqual(diags, again) {
			return stats, fmt.Errorf(
				"LintFile is not deterministic: two runs over identical bytes with an"+
					" identical analyzer set produced different diagnostics.\n first: %v\nsecond: %v",
				diags, again)
		}
	}

	// --- semantic: what `elps lint --workspace` does per file ---------------
	env, err := newLintEnv()
	if err != nil {
		return stats, fmt.Errorf("harness: %w", err)
	}
	preamble := lintPreamble(wsSrc)
	_ = analysis.LoadWorkspaceMacros(env, preamble)
	exp := &analysis.EnvMacroExpander{Env: env}

	wsDefs := analysis.ExtractFileDefinitions(wsSrc, lintFileB)
	stdlibExports := analysis.ExtractPackageExports(env.Runtime.Registry)
	acfg := &analysis.Config{
		ExtraGlobals:   wsDefs,
		PackageExports: stdlibExports,
		PackageSymbols: groupByPackage(wsDefs),
		MacroExpander:  exp,
	}
	if sc.bit() {
		acfg.DefaultPackage = lisp.DefaultUserPackage
	}

	semDiags, err := linter.LintFileWithAnalysis(src, lintFileA, acfg)
	if err == nil {
		stats.semantic = len(semDiags)
		if err := checkDiagnostics(semDiags, "LintFileWithAnalysis"); err != nil {
			return stats, err
		}
	}

	// LintFileWithContext with an explicitly nil Result, which is the
	// "semantic analyzers are no-ops" contract, and with one built here.
	if _, err := linter.LintFileWithContext(src, lintFileA, nil); err != nil {
		_ = err // a parse failure is a legitimate outcome
	}

	// --- CLI entry points: LintFiles over a real (temp) workspace -----------
	//
	// Filenames are fixed constants; only the CONTENTS come from the fuzzer.
	// LintFiles calls os.ReadFile on every path it is handed and
	// BuildAnalysisConfig walks the workspace root, so a fuzzer-chosen path
	// here would be a file-read primitive.
	pathA := filepath.Join(root, lintFileA)
	pathB := filepath.Join(root, lintFileB)
	if err := os.WriteFile(pathA, src, 0o600); err != nil {
		return stats, fmt.Errorf("harness: %w", err)
	}
	if err := os.WriteFile(pathB, wsSrc, 0o600); err != nil {
		return stats, fmt.Errorf("harness: %w", err)
	}

	lcfg := &LintConfig{
		Workspace: root,
		Excludes:  []string{"*.txt", "[", "nothing.lisp"}[:sc.intn(4)],
	}
	if sc.bit() {
		lcfg.Registry = env.Runtime.Registry
	}
	switch sc.intn(3) {
	case 0:
		// Env set, MacroExpander not: BuildAnalysisConfig loads workspace
		// macros and constructs the expander itself.
		lcfg.Env = env
	case 1:
		lcfg.MacroExpander = exp
	}
	if sc.bit() {
		// Skips defaultStdlibExports' fresh env boot, which is the reason the
		// field exists.
		lcfg.StdlibExports = stdlibExports
	}

	files := []string{pathA, pathB}[:1+sc.intn(2)]
	wsDiags, err := linter.LintFiles(lcfg, files)
	if err == nil {
		stats.workspace = len(wsDiags)
		if err := checkDiagnostics(wsDiags, "LintFiles"); err != nil {
			return stats, err
		}
	}
	// The CLI's stdin path calls BuildAnalysisConfig directly rather than
	// through LintFiles. Only occasionally: LintFiles has already called it
	// once, and a second call re-walks the workspace and re-runs
	// ScanWorkspaceRefs over every file. Measured on the first campaign, that
	// doubled cost put this target at ~65 exec/sec against FuzzAnalyzeSource's
	// ~280, and the second call reaches no branch the first did not.
	if sc.intn(4) == 0 {
		if _, err := BuildAnalysisConfig(lcfg); err != nil {
			_ = err // a workspace that cannot be scanned is a legitimate outcome
		}
	}

	// --- output, severity and the AST helpers -------------------------------
	all := append(append(append([]Diagnostic{}, diags...), semDiags...), wsDiags...)
	if err := reportSurface(sc, all); err != nil {
		return stats, err
	}
	walkSurface(src)

	// The macro expander must not have swallowed a panic while semantic
	// analysis evaluated macro bodies. ExpandMacro's recover returns nil, which
	// is also its ordinary answer, so nothing in the diagnostics could ever
	// have shown this.
	if n := exp.ExpansionPanics(); n > 0 {
		rec := exp.LastExpansionPanic()
		return stats, fmt.Errorf(
			"EnvMacroExpander swallowed %d panic(s) while lint ran semantic analysis"+
				"\n  macro:   %s\n  package: %s\n  value:   %v\n  stack:\n%s",
			n, rec.Macro, rec.Package, rec.Value, rec.GoStack)
	}
	return stats, nil
}

// checkDiagnostics asserts the invariants every returned diagnostic must hold,
// whichever entry point produced it.
func checkDiagnostics(diags []Diagnostic, from string) error {
	for i, d := range diags {
		if d.Analyzer == "" {
			return fmt.Errorf("%s returned diagnostic %d with no Analyzer name: %+v", from, i, d)
		}
		if d.Pos.File == "" {
			return fmt.Errorf("%s returned diagnostic %d with no file; every entry point"+
				" backfills it from its filename argument: %+v", from, i, d)
		}
		if d.Severity < SeverityError || d.Severity > SeverityInfo {
			return fmt.Errorf("%s returned diagnostic %d with severity %d, outside"+
				" [%d,%d]. Report and ReportWithNotes fill an unset severity from the"+
				" analyzer's default, so a value outside the range means an analyzer set"+
				" one by hand: %+v",
				from, i, int(d.Severity), int(SeverityError), int(SeverityInfo), d)
		}
		if _, err := json.Marshal(d); err != nil {
			return fmt.Errorf("%s returned a diagnostic that will not marshal to JSON"+
				" (--format=json is what CI reads): %v: %+v", from, err, d)
		}
	}
	return nil
}

// reportSurface drives the severity and output functions -- the ones that
// decide `elps lint`'s exit code and the bytes a CI job parses.
func reportSurface(sc *script, diags []Diagnostic) error {
	_ = MaxSeverity(diags)
	for _, s := range []Severity{SeverityError, SeverityWarning, SeverityInfo} {
		_ = ShouldFail(diags, s)
		_ = s.String()
		b, err := s.MarshalJSON()
		if err != nil {
			return fmt.Errorf("Severity(%d).MarshalJSON: %w", int(s), err)
		}
		var back Severity
		if err := back.UnmarshalJSON(b); err != nil {
			return fmt.Errorf("Severity(%d) does not round-trip through JSON: %w", int(s), err)
		}
		if back != s {
			return fmt.Errorf("Severity round-trip changed the value: %d -> %s -> %d",
				int(s), b, int(back))
		}
	}
	for range 4 {
		_, _ = ParseSeverity(severityStrings[sc.intn(len(severityStrings))])
	}
	for _, d := range diags {
		_ = d.String()
		_ = d.Pos.String()
	}
	FormatText(io.Discard, diags)
	if err := FormatJSON(io.Discard, diags); err != nil {
		return fmt.Errorf("FormatJSON failed on diagnostics the linter itself produced"+
			" (--format=json is what CI reads): %v", err)
	}
	_ = AnalyzerNames()
	_ = AnalyzerDoc()
	return nil
}

// walkSurface drives the exported AST helpers, which embedders writing their
// own analyzers call directly.
func walkSurface(src []byte) {
	res := rdparser.New(token.NewScanner(lintFileA, bytes.NewReader(src))).ParseProgramFaultTolerant()
	Walk(res.Exprs, func(node, parent *lisp.LVal, depth int) {
		_ = SourceOf(node)
		// NOT SourceOf(parent). Walk documents "parent is nil for top-level
		// expressions" and SourceOf dereferences its argument unconditionally,
		// so the obvious pairing of the two segfaults. That is a real (if
		// minor) contract defect in astutil.SourceOf rather than something this
		// target should keep re-finding on every input; see the report
		// accompanying this branch. Guarded here so the finding is recorded
		// once instead of masking everything downstream of it.
		if parent != nil {
			_ = SourceOf(parent)
		}
	})
	WalkSExprs(res.Exprs, func(sexpr *lisp.LVal, depth int) {
		_ = HeadSymbol(sexpr)
		_ = ArgCount(sexpr)
		defs := map[string]bool{}
		if len(sexpr.Cells) > 1 {
			CollectFormals(sexpr.Cells[1], defs)
		}
	})
	_ = UserDefined(res.Exprs)
}

// lintPreamble extracts the forms a workspace scan would replay into the env.
func lintPreamble(src []byte) []*lisp.LVal {
	res := rdparser.New(token.NewScanner(lintFileB, bytes.NewReader(src))).ParseProgramFaultTolerant()
	heads := map[string]bool{
		"in-package": true, "use-package": true, "export": true,
		"defmacro": true, "defun": true, "set": true,
	}
	var out []*lisp.LVal
	for _, expr := range res.Exprs {
		if expr == nil || expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		if heads[HeadSymbol(expr)] {
			out = append(out, expr)
		}
	}
	return out
}

func groupByPackage(syms []analysis.ExternalSymbol) map[string][]analysis.ExternalSymbol {
	m := map[string][]analysis.ExternalSymbol{}
	for _, s := range syms {
		m[s.Package] = append(m[s.Package], s)
	}
	return m
}

// ---------------------------------------------------------------------------
// the target
// ---------------------------------------------------------------------------

const (
	lintSeedDefs = `(in-package 'demo)
(use-package 'lisp)
(defun add (a b) "add two" (+ a b))
(defmacro twice (x) (quasiquote (+ (unquote x) (unquote x))))
(defmacro defthing (name args &rest body)
  (quasiquote (defun (unquote name) (unquote args) (unquote-splicing body))))
(export 'add)
(export 'twice)`

	lintSeedUses = `(in-package 'user)
(use-package 'demo)
(defun main () (add 1 2))
(defun unused-arg (x y) x)
(defthing helper (q) (+ q 1))
(let ([shadow 1]) (let ([shadow 2]) shadow))
(set 'global 1)
(if true 1)`
)

// nolintSeeds exist because the nolint machinery -- filterSuppressed,
// walkForNolint, checkNolintToken and the whole unused-nolint block -- is
// driven entirely by COMMENTS, and a corpus of well-formed lisp contains none.
// The conditional-staleness rules also depend on whether the run was narrowed,
// which is what buildLinter's subset draw supplies.
var nolintSeeds = []string{
	"(defun f (x) x) ; nolint",
	"(defun f (x) x) ; nolint:unused-variable",
	"(defun f (x) x) ; nolint:no-such-analyzer",
	"(defun f (x) x) ; nolint:unused-variable,no-such-analyzer",
	"(defun f (x) x) ; nolint:unused-nolint",
	"; nolint\n(defun f (x) x)",
	"(defun f (x) x) ;nolint:undefined-symbol",
	"(defun f (x) x) ; nolint: ,, ,",
	"(defun f (x) x) ; nolint:" + "a,b,c,d,e,f",
	";; nolint\n;; nolint:x\n(defun f () 1)",
}

// lintScripts are the seed configurations. Package-level so the guard tests
// drive the same set the corpus does -- a guard that invents its own script
// proves nothing about the corpus.
var lintScripts = [][]byte{
	{1, 0, 0, 0, 0, 0, 0, 0},       // full analyzer set
	{0, 0, 0, 0, 0, 0, 0, 0},       // empty Linter
	{2, 1, 0, 1, 0, 1, 0, 1, 1, 1}, // narrowed: alternating subset
	{3, 255, 255, 0, 0, 1, 2, 3, 1, 0, 1},
	{7, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2},
	{5, 0, 1, 0, 1, 0, 1, 0, 3, 3, 3},
}

// FuzzLintSource lints fuzzer-chosen source with a fuzzer-chosen analyzer set,
// through all four entry points the CLI, the LSP and embedders use.
//
// Asserted, and only this:
//
//  1. No Go panic escapes an analyzer, the nolint machinery or the output
//     functions. Nothing recovers in between.
//  2. Every diagnostic returned by every entry point has an analyzer name, a
//     file, an in-range severity, and marshals to JSON.
//  3. LintFile is deterministic over identical bytes.
//  4. Semantic analysis' macro expander swallowed no panic.
//  5. The run terminates (the watchdog). Nothing bounds a loop in an analyzer.
//
// NOT asserted: that any particular diagnostic is or is not produced. A linter
// finding nothing in garbage is correct, and so is finding a hundred things.
func FuzzLintSource(f *testing.F) {
	add := func(src, ws string, sc []byte) { f.Add([]byte(src), []byte(ws), sc) }

	scripts := lintScripts
	for _, sc := range scripts {
		add(lintSeedUses, lintSeedDefs, sc)
		add(lintSeedDefs, lintSeedUses, sc)
		add("", "", sc)
	}
	for _, s := range nolintSeeds {
		add(s, lintSeedDefs, scripts[0])
		add(s, lintSeedDefs, scripts[2]) // narrowed run: different staleness rules
	}

	// Semantic findings that need a RESOLVED user symbol with a signature:
	// arity violations against a function defined in the same file, and
	// duplicate definitions. These reach the Notes/Related-building helpers,
	// which measurement put at 0% from everything above.
	add("(in-package 'user)\n(defun add (a b) (+ a b))\n(add 1)\n(add 1 2 3)\n(add)",
		lintSeedDefs, scripts[0])
	add("(in-package 'user)\n(defun dup () 1)\n(defun dup () 2)\n(defmacro dup (x) x)",
		lintSeedDefs, scripts[0])
	add("(in-package 'demo)\n(defun add (a b) 1)", lintSeedDefs, scripts[0])

	// Shapes chosen for what breaks the analyzers rather than the parser:
	// arity errors, shadowing, rethrow patterns, dead branches, deep nesting.
	for _, s := range []string{
		"(car)", "(car 1 2 3)", "(+)", "(lambda)", "(defun)", "(defun f)",
		"(handler-bind ((condition (lambda (c) (error c)))) 1)",
		"(cond)", "(cond (true))", "(if)", "(if true)",
		"(let ())", "(let)", "(let ([a]) a)", "(let ([] 1) 1)",
		"(deftype)", "(deftype t)", "(set)", "(set!)",
		"(defun f (x &optional y &rest z) (list x y z))",
		"(defun f (&rest) 1)", "(defun f (a a) a)",
		"(progn (progn (progn (progn 1))))",
		"(defun f () (f))",
		"(in-package)", "(use-package)", "(export)",
	} {
		add(s, lintSeedDefs, scripts[0])
	}

	// A macro defined next door and called here: the only shape that makes
	// semantic analysis EVALUATE a macro body, which is what the
	// ExpansionPanics assertion guards.
	add("(in-package 'demo)\n(twice (twice 3))",
		"(in-package 'demo)\n(defmacro twice (x) (quasiquote (+ (unquote x) (unquote x))))",
		scripts[0])
	add("(in-package 'demo)\n(boom 1)",
		"(in-package 'demo)\n(defmacro boom (x) (error 'macro-boom \"x\"))", scripts[0])

	for _, seed := range fuzzseed.Adversarial() {
		add(string(seed), lintSeedDefs, scripts[0])
		add(lintSeedUses, string(seed), scripts[2])
	}

	f.Fuzz(func(t *testing.T, src, wsSrc, scriptBytes []byte) {
		runLintBudgeted(t, t.TempDir(), src, wsSrc, scriptBytes)
	})
}

// fatalf is the subset of *testing.T the budgeted runner uses, so the guard
// tests can pass something else.
type fatalf interface {
	Helper()
	Fatalf(format string, args ...interface{})
	Skipf(format string, args ...interface{})
}

func runLintBudgeted(t fatalf, root string, src, wsSrc, scriptBytes []byte) {
	t.Helper()

	done := make(chan error, 1)
	go func() {
		_, err := runLint(root, src, wsSrc, scriptBytes)
		done <- err
	}()

	budget := fuzzwatch.New(lintWatchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case err := <-done:
			if err != nil {
				t.Fatalf("%v\n--- src (%d bytes) ---\n%q\n--- workspace (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					err, len(src), src, len(wsSrc), wsSrc, len(scriptBytes), scriptBytes)
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
				t.Fatalf("lint did not terminate within %s of SCHEDULED time (%s)."+
					" Macro bodies run under interpreter budgets; nothing bounds an"+
					" analyzer's own walk"+
					"\n--- src (%d bytes) ---\n%q\n--- workspace (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					budget.Total(), report, len(src), src, len(wsSrc), wsSrc,
					len(scriptBytes), scriptBytes)
				return
			}
		}
	}
}
