// Copyright © 2026 The ELPS authors

package mcpserver

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/parser/rdparser"
)

// FuzzToolAgreement is a DIFFERENTIAL target: it asserts that mcpserver's
// `lint` and `diagnostics` tools answer the same question about the same file
// with the same list of diagnostics.
//
// # Why this property, and why a fuzzer
//
// The two tools run the same analyzer set (s.linter) over the same bytes and
// build their diagnostic lists from the same two sources (parse errors plus
// lint diagnostics), so any difference between them comes from the
// analysis.Config each one hands the semantic analyzers. Issue #424 is exactly
// that: `lint` built a throwaway config with lint.BuildAnalysisConfig — no Env,
// no Registry, no MacroExpander — while `diagnostics` served from the cached
// workspace state, whose config carries the service-wide expander. A client had
// no way to tell which of the two contradictory answers to believe.
//
// The unit tests for #424 pin three specific macro shapes. What makes the
// divergence worth a fuzz target instead is that the config is a struct of
// eleven fields feeding analyzers that consult them in combination: the ways
// two configs can disagree are not enumerable by hand, and the next one to be
// introduced will not look like a macro at all. A differential assertion needs
// no oracle — it compares two implementations that are required to agree — so
// it stays true as analyzers are added.
//
// # What is and is not asserted
//
// Asserted: the two tools return equal diagnostics (code, message, range) in
// equal order, and neither panics. Both sort by range before returning, so
// order is part of the contract rather than an accident.
//
// NOT asserted: that any particular diagnostic is produced. Finding nothing in
// garbage is a correct answer, as is finding a hundred things — the target is
// blind to which, by design.
//
// # Cost
//
// This target is SLOW: measured at ~1.7 exec/sec against FuzzLintSource's ~65
// and FuzzAnalyzeSource's ~280. Each input boots an env, loads the whole
// standard library into it and builds a workspace index, because that is what
// makes the two tools share a cached state at all. The env is deliberately not
// reused across inputs — loading one input's macros into it would leak into the
// next and make a failure unreproducible from its corpus entry alone. A
// campaign here buys hundreds of execs, not millions; it is a differential
// property check, not a throughput target.
//
// The env is budgeted (semantic analysis EVALUATES macro bodies drawn from the
// fuzzer's workspace file) and its Runtime.Library is left nil so `load-file`
// cannot reach the disk. Filenames are fixed constants, never fuzzer-derived:
// both tools call os.ReadFile on the path they are given, and a fuzzer-chosen
// path there would be a file-read primitive aimed at the machine running the
// tests.
func FuzzToolAgreement(f *testing.F) {
	add := func(src, ws string) { f.Add([]byte(src), []byte(ws)) }

	// The #424 shapes: a macro that rebinds a caller-side name (expansion
	// REMOVES an index entry) and one that splices quoted caller forms into
	// code position (expansion ADDS one).
	add("(defun total () 42)\n"+
		"(defmacro with-binding (name value &rest body)\n"+
		"  (quasiquote (let ([(unquote name) (unquote value)]) (unquote-splicing body))))",
		"(defun f () (with-binding total 1 (+ total 1)))")
	add("(defun alpha () 1)\n(defmacro run-all (specs) (quasiquote (progn (unquote-splicing specs))))",
		"(defun f () (run-all '((alpha))))")

	// Ordinary shapes that reach the semantic analyzers without a macro:
	// arity, duplicate definitions, unresolved symbols, package boundaries.
	for _, s := range []string{
		"", "(in-package 'user)\n(defun add (a b) (+ a b))\n(add 1)\n(add 1 2 3)",
		"(in-package 'user)\n(defun dup () 1)\n(defun dup () 2)",
		"(in-package 'demo)\n(export 'shared)\n(defun shared () 1)",
		"(undefined-thing)", "(car)", "(let ([a]) a)", "(defun f () (f))",
		"(", ")", "(defun f (a a) a)",
	} {
		add(s, "(in-package 'user)\n(defun helper () 1)")
	}

	// A macro that errors during expansion, and one that expands forever:
	// both leave the expander's state where the two tools could diverge on it.
	add("(in-package 'demo)\n(boom 1)",
		"(in-package 'demo)\n(defmacro boom (x) (error 'macro-boom \"x\"))")
	add("(in-package 'demo)\n(loop 1)",
		"(in-package 'demo)\n(defmacro loop (x) (quasiquote (loop (unquote x))))")

	for _, seed := range fuzzseed.Adversarial() {
		add(string(seed), "(in-package 'user)\n(defun helper () 1)")
	}

	f.Fuzz(func(t *testing.T, src, wsSrc []byte) {
		runAgreementBudgeted(t, t.TempDir(), src, wsSrc)
	})
}

const (
	// Budgets for the injected LEnv. Analysis with a MacroExpander evaluates
	// macro bodies loaded from the fuzzer's workspace file.
	agreeEnvMaxSteps          = 200_000
	agreeEnvMaxTailIterations = 10_000
	agreeEnvMaxPhysHeight     = 2_000
	agreeEnvMaxAlloc          = 1_000_000
	agreeEnvMacroDepth        = 100
	agreeEnvMaxEvalNesting    = 20_000

	// Fixed names. Never fuzzer-derived — see the target's doc comment.
	agreeFileA = "fuzz-a.lisp"
	agreeFileB = "fuzz-b.lisp"

	agreeMaxFileBytes = 64 << 10

	// Denominated in SCHEDULED time (internal/fuzzwatch): wall clock during
	// which this process was not run by the OS is not charged to the tools.
	agreeWatchdogTimeout = 60 * time.Second
)

func runAgreementBudgeted(t *testing.T, root string, src, wsSrc []byte) {
	t.Helper()
	if len(src) > agreeMaxFileBytes || len(wsSrc) > agreeMaxFileBytes {
		t.Skip("input larger than the workspace byte cap")
	}

	done := make(chan error, 1)
	go func() { done <- runAgreement(root, src, wsSrc) }()

	budget := fuzzwatch.New(agreeWatchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case err := <-done:
			if err != nil {
				t.Fatalf("%v\n--- src (%d bytes) ---\n%q\n--- workspace (%d bytes) ---\n%q",
					err, len(src), src, len(wsSrc), wsSrc)
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
				t.Fatalf("the lint/diagnostics comparison did not terminate within %s"+
					" of SCHEDULED time (%s)"+
					"\n--- src (%d bytes) ---\n%q\n--- workspace (%d bytes) ---\n%q",
					budget.Total(), report, len(src), src, len(wsSrc), wsSrc)
				return
			}
		}
	}
}

// runAgreement writes the two files, serves both tools from one server, and
// compares their answers for fuzz-a.lisp.
func runAgreement(root string, src, wsSrc []byte) error {
	pathA := filepath.Join(root, agreeFileA)
	if err := os.WriteFile(pathA, src, 0o600); err != nil {
		return fmt.Errorf("harness: %w", err)
	}
	if err := os.WriteFile(filepath.Join(root, agreeFileB), wsSrc, 0o600); err != nil {
		return fmt.Errorf("harness: %w", err)
	}

	env, err := newAgreementEnv()
	if err != nil {
		return fmt.Errorf("harness: %w", err)
	}
	srv := New(WithWorkspaceRoot(root), WithEnv(env))
	// One server, so both tools see the same cached workspace state — which is
	// the point. A divergence here is a divergence in how the tools USE that
	// state, not in what two independent servers happened to index.
	svc := srv.service
	ctx := context.Background()

	fd, diagErr := svc.collectFileDiagnostics(pathA, nil, &root)
	_, lintResp, lintErr := svc.lintTool(ctx, nil, LintInput{Path: pathA})

	// Whether the file can be served at all is itself part of the agreement.
	// A workspace neither tool can serve is a legitimate outcome; one tool
	// refusing what the other answers is not. Only error-ness is compared —
	// lint wraps its failures in toolErr, so the messages differ by design.
	switch {
	case diagErr != nil && lintErr != nil:
		return nil
	case diagErr != nil:
		return fmt.Errorf("lint served %s but diagnostics returned: %w", agreeFileA, diagErr)
	case lintErr != nil:
		return fmt.Errorf("diagnostics served %s but lint returned: %w", agreeFileA, lintErr)
	}

	if len(fd.Diagnostics) != len(lintResp.Diagnostics) {
		return fmt.Errorf("lint and diagnostics disagree on %s: diagnostics returned %d, lint returned %d"+
			"\n  diagnostics: %s\n  lint:        %s",
			agreeFileA, len(fd.Diagnostics), len(lintResp.Diagnostics),
			describeDiagnostics(fd.Diagnostics), describeDiagnostics(lintResp.Diagnostics))
	}
	for i := range fd.Diagnostics {
		a, b := fd.Diagnostics[i], lintResp.Diagnostics[i]
		if a.Code != b.Code || a.Message != b.Message || a.Range != b.Range || a.Severity != b.Severity {
			return fmt.Errorf("lint and diagnostics disagree on %s at index %d"+
				"\n  diagnostics: %s\n  lint:        %s",
				agreeFileA, i, describeDiagnostic(a), describeDiagnostic(b))
		}
	}
	return nil
}

func describeDiagnostic(d Diagnostic) string {
	return fmt.Sprintf("%s %q [%d:%d-%d:%d] %s", d.Code, d.Message,
		d.Range.Start.Line, d.Range.Start.Character,
		d.Range.End.Line, d.Range.End.Character, d.Severity)
}

func describeDiagnostics(ds []Diagnostic) string {
	out := make([]string, 0, len(ds))
	for _, d := range ds {
		out = append(out, describeDiagnostic(d))
	}
	return fmt.Sprint(out)
}

// newAgreementEnv builds the environment analysis expands macros in.
// Runtime.Library stays nil so load-file cannot reach the disk.
func newAgreementEnv() (*lisp.LEnv, error) {
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = rdparser.NewReader()
	if rc := lisp.InitializeUserEnv(env,
		lisp.WithStderr(discardAgreementWriter{}),
		lisp.WithMaxSteps(agreeEnvMaxSteps),
		lisp.WithMaxTailIterations(agreeEnvMaxTailIterations),
		lisp.WithMaximumPhysicalStackHeight(agreeEnvMaxPhysHeight),
		lisp.WithMaxAlloc(agreeEnvMaxAlloc),
		lisp.WithMaxMacroExpansionDepth(agreeEnvMacroDepth),
		lisp.WithMaxEvalNesting(agreeEnvMaxEvalNesting),
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

type discardAgreementWriter struct{}

func (discardAgreementWriter) Write(p []byte) (int, error) { return len(p), nil }
