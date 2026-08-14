// Copyright © 2026 The ELPS authors

package analysis

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/internal/fuzzwatch"
)

// Fuzzing the WORKSPACE SCANNER: the half of analysis/ that reads a directory
// tree off disk.
//
// # Why this is separate from FuzzAnalyzeSource
//
// workspace.go is 1221 of this package's lines and roughly half its functions,
// and none of it is reachable without real files. FuzzLSPSession cannot reach
// any of it -- it initializes with RootURI and RootPath unset precisely so that
// a fuzz target does not walk the machine it is running on, and says so. From
// the LSP seed corpus every one of PrescanWorkspace, ScanWorkspaceRefs,
// scanFileFull, buildLoadTree, walkLoadFile, collectLispFilesWithConfig,
// extractDefFormSpecs and the six ScanWorkspace* entry points measured 0%.
//
// They are not obscure. lint.BuildAnalysisConfig calls PrescanWorkspace and
// ScanWorkspaceRefs on every `elps lint --workspace` run, the LSP calls them on
// every editor session that has a root, and luthersystems/substrate imports
// this package directly. The input is a checkout, which is to say: whatever
// source someone asked the tool to look at.
//
// # Containment, which is the whole design problem here
//
// A scanner that follows references between files is a file-read primitive if
// the fuzzer controls where it points. walkLoadFile is exactly that: it reads a
// file, finds `(load-file "...")`, joins the string to the file's directory and
// RECURSES. `filepath.Join(dir, "../../../etc/passwd")` escapes the workspace,
// and the recursion means one escape in one file is enough.
//
// So the harness draws a line that the fuzzer cannot cross:
//
//	the TREE SHAPE is the harness's        every path, every filename and every
//	                                       directory is a fixed constant below;
//	                                       nothing the fuzzer produces is ever
//	                                       used as a path.
//
//	the FILE CONTENTS are the fuzzer's     with one substitution: `load-file` is
//	                                       rewritten to `load-filx` in every
//	                                       fuzzer-derived file (same length, so
//	                                       every token.Location is unchanged).
//	                                       After that no fuzzer-derived file can
//	                                       introduce a load edge at all.
//
//	the LOAD GRAPH is the harness's        main.lisp gets harness-written
//	                                       load-file forms, selected by the
//	                                       fuzzer from a fixed table: a real
//	                                       file, a file in a subdirectory, a
//	                                       missing file, and main.lisp itself
//	                                       (the cycle that tests walkLoadFile's
//	                                       `visited` guard). Every target is
//	                                       inside the temp root.
//
// The result is that walkLoadFile is fully exercised -- recursion, cycles,
// missing files, package inheritance through the tree -- while the set of
// files it can possibly open is a fixed, four-element list under t.TempDir().
// Stripping one token is a smaller cost than the alternative, which is what
// FuzzLSPSession had to do: not fuzz this code at all.
//
// # What the fuzzer does get to steer
//
// ScanConfig is attacker-adjacent in its own right: Excludes and IncludeDirs
// come from a config file or CLI flags in production, and Excludes are
// filepath.Match patterns, where a malformed pattern (`[`) returns
// ErrBadPattern and MatchesExclude discards the error. MaxFiles and
// MaxFileBytes decide whether a scan is truncated, and Truncated is a
// user-visible warning in the LSP.
const (
	// The tree. Fixed, and the only paths this target can ever touch.
	wsMainFile   = "main.lisp"
	wsPlainFile  = "plain.lisp"
	wsSubFile    = "sub/nested.lisp"
	wsSkipFile   = "_skipped/hidden.lisp" // under a ShouldSkipDir directory
	wsDotFile    = ".dot/dotted.lisp"     // under a hidden directory
	wsNonLisp    = "notes.txt"            // wrong extension, never collected
	wsMissingRef = "absent.lisp"          // referenced by main, never created

	wsWatchdogTimeout = 30 * time.Second

	// Cap on total bytes written per input. A scan is O(bytes) across several
	// passes, so an unbounded corpus entry would spend the deadline in I/O.
	wsMaxFileBytes = 64 << 10
)

// loadFileToken is neutralised in every fuzzer-derived file. The replacement is
// the SAME LENGTH, so byte offsets, line lengths and every token.Location the
// scanner computes are identical to what the original bytes would have
// produced -- the substitution removes an edge from the load graph and changes
// nothing else.
var (
	loadFileToken = []byte("load-file")
	loadFileSafe  = []byte("load-filx")
)

func neutraliseLoadFile(src []byte) []byte {
	return bytes.ReplaceAll(src, loadFileToken, loadFileSafe)
}

// wsLoadTargets is the fixed set of load-file arguments main.lisp may be given.
// Every entry resolves inside the temp root; the last two are the missing-file
// and self-cycle cases.
var wsLoadTargets = []string{wsPlainFile, wsSubFile, wsMissingRef, wsMainFile}

// wsFileByteLimits straddle the sizes the harness actually writes, so that the
// skip-oversize branch is reachable without being universal.
var wsFileByteLimits = []int64{0, 0, 0, 16, 256, 4096, 1 << 20}

// wsExcludePatterns are the fixed exclude patterns, including two that
// filepath.Match rejects outright.
var wsExcludePatterns = []string{
	"*.lisp", "plain.lisp", "sub", "_*", "*", "[", "a[b", "**/*.lisp", "",
}

// writeWorkspace materialises the tree under root and returns the root.
func writeWorkspace(root string, sc *script, a, b []byte) error {
	if len(a) > wsMaxFileBytes {
		a = a[:wsMaxFileBytes]
	}
	if len(b) > wsMaxFileBytes {
		b = b[:wsMaxFileBytes]
	}

	// main.lisp: fuzzer-derived content (load-file neutralised) followed by
	// harness-written load edges the fuzzer only gets to SELECT.
	var main bytes.Buffer
	main.Write(neutraliseLoadFile(a))
	main.WriteByte('\n')
	if sc.bit() {
		main.WriteString("(in-package 'wsdemo)\n")
	}
	for range sc.intn(len(wsLoadTargets) + 1) {
		fmt.Fprintf(&main, "(load-file %q)\n", wsLoadTargets[sc.intn(len(wsLoadTargets))])
	}

	files := map[string][]byte{
		wsPlainFile: neutraliseLoadFile(b),
		wsSubFile:   neutraliseLoadFile(a),
		wsSkipFile:  neutraliseLoadFile(b),
		wsDotFile:   neutraliseLoadFile(a),
		wsNonLisp:   neutraliseLoadFile(b),
	}
	if sc.bit() {
		files[wsMainFile] = main.Bytes()
	}

	for name, content := range files {
		path := filepath.Join(root, filepath.FromSlash(name))
		if err := os.MkdirAll(filepath.Dir(path), 0o750); err != nil {
			return err
		}
		if err := os.WriteFile(path, content, 0o600); err != nil {
			return err
		}
	}
	return nil
}

// buildScanConfig draws a ScanConfig from the script.
func buildScanConfig(sc *script) *ScanConfig {
	if sc.intn(8) == 0 {
		// nil is a supported argument -- every effective* accessor has a nil
		// receiver branch, and the four-arg ScanWorkspace* wrappers pass nil.
		return nil
	}
	cfg := &ScanConfig{
		// 0 means "use the default"; small values make Truncated reachable.
		MaxFiles: sc.intn(5),
		// Drawn from a table rather than computed, because the first version
		// of this line was `intn(4) * 16` and every value it could produce was
		// smaller than every file the harness writes. The scan collected ZERO
		// files, every seed passed, and all of workspace.go past
		// collectLispFilesWithConfig sat at 0% -- caught by
		// TestWorkspaceFuzzSeedsCollectFiles, not by reading. The entries now
		// straddle the sizes actually written: 0 (default), tiny (everything
		// skipped), around the seed size (some skipped), and large.
		MaxFileBytes: wsFileByteLimits[sc.intn(len(wsFileByteLimits))],
	}
	for range sc.intn(3) {
		cfg.Excludes = append(cfg.Excludes, wsExcludePatterns[sc.intn(len(wsExcludePatterns))])
	}
	for _, dir := range []string{"_skipped", ".dot", "sub"} {
		if sc.bit() {
			cfg.IncludeDirs = append(cfg.IncludeDirs, dir)
		}
	}
	return cfg
}

// scanStats is what one workspace run reports back, for the guard tests.
type scanStats struct {
	files     int
	defs      int
	preamble  int
	defForms  int
	refKeys   int
	truncated bool
}

func runWorkspaceScan(root string, a, b, scriptBytes []byte) (scanStats, error) {
	var stats scanStats
	sc := &script{b: scriptBytes}

	if err := writeWorkspace(root, sc, a, b); err != nil {
		return stats, fmt.Errorf("harness: writing workspace: %w", err)
	}
	scanCfg := buildScanConfig(sc)

	prescan, err := PrescanWorkspace(root, scanCfg)
	if err != nil {
		// A walk error is a legitimate outcome, not a finding: the scanner is
		// meant to report an unreadable tree rather than panic on one.
		return stats, nil //nolint:nilerr // an error return here is the code under test behaving
	}
	if prescan == nil {
		return stats, errors.New("PrescanWorkspace returned (nil, nil);" +
			" it has a single success return and always builds a WorkspacePrescan")
	}
	stats.files = len(prescan.Files)
	stats.defs = len(prescan.AllDefs)
	stats.preamble = len(prescan.Preamble)
	stats.defForms = len(prescan.DefForms)
	stats.truncated = prescan.Truncated

	// Every file the scan reports must be one the harness wrote. This is the
	// containment claim as an ASSERTION rather than as a comment: if a future
	// change to the scanner (or to this harness) let a fuzzer-derived string
	// become a path, the escape shows up here rather than in someone's logs.
	for _, p := range prescan.Files {
		rel, relErr := filepath.Rel(root, p)
		if relErr != nil || rel == ".." || bytes.HasPrefix([]byte(rel), []byte(".."+string(filepath.Separator))) {
			return stats, fmt.Errorf("scan collected a path outside the workspace root: %q (root %q)", p, root)
		}
	}

	// The remaining entry points, all of which are exported and used by
	// different callers (the CLI, the LSP, substrate).
	_, _ = ScanWorkspace(root)
	_, _ = ScanWorkspacePackages(root)
	_, _ = ScanWorkspaceDefinitions(root)
	_, _, _ = ScanWorkspaceFull(root)
	_, _, _, _ = ScanWorkspaceAll(root)
	_, _, _, _, _ = ScanWorkspaceAllWithConfig(root, scanCfg)

	// The config a workspace-aware caller builds out of a prescan, then the
	// cross-file reference scan that reads every file again through
	// AnalyzeFile.
	env, err := newFuzzEnv(prescan.Preamble)
	if err != nil {
		return stats, fmt.Errorf("harness: %w", err)
	}
	exp := &countingExpander{inner: &EnvMacroExpander{Env: env}}
	cfg := &Config{
		ExtraGlobals:   prescan.AllDefs,
		PackageExports: prescan.PkgExports,
		PackageSymbols: prescan.PkgAllSymbols,
		DefForms:       prescan.DefForms,
		PackageImports: prescan.PackageImports,
		DefaultPackage: prescan.DefaultPackage,
		MacroExpander:  exp,
	}
	refs := ScanWorkspaceRefs(root, cfg, scanCfg)
	stats.refKeys = len(refs)

	// Path helpers, on the paths the scan produced rather than on invented
	// strings. absent.lisp is included on purpose: NormalizePath has a whole
	// fallback for a path that does not exist, and every path a successful scan
	// returns does exist, so nothing else reaches it. A load-file naming a
	// missing file is how production gets there.
	_ = NormalizePath(filepath.Join(root, wsMissingRef))
	_ = NormalizePath(filepath.Join(root, wsMissingRef, "deeper", "still"))
	for _, p := range prescan.Files {
		_ = NormalizePath(p)
		_ = MatchesExclude(p, wsExcludePatterns)
		_ = ShouldSkipDir(filepath.Base(filepath.Dir(p)))
		_ = ExtractFileDefinitions([]byte(filepath.Base(p)), p)
	}

	if n := exp.ExpansionPanics(); n > 0 {
		rec := exp.inner.LastExpansionPanic()
		return stats, fmt.Errorf(
			"EnvMacroExpander swallowed %d panic(s) during the workspace scan"+
				"\n  macro:   %s\n  package: %s\n  value:   %v\n  stack:\n%s",
			n, rec.Macro, rec.Package, rec.Value, rec.GoStack)
	}
	return stats, nil
}

// FuzzWorkspaceScan scans a fuzzer-written .lisp tree with a fuzzer-chosen
// ScanConfig.
//
// Asserted, and only this:
//
//  1. No Go panic escapes the scan or the reference pass. Nothing recovers
//     between them and the fuzz function.
//  2. PrescanWorkspace never returns (nil, nil).
//  3. Every collected path is inside the temp root -- the containment
//     invariant, asserted rather than assumed.
//  4. The macro expander swallowed no panic (ExpansionPanics).
//  5. The scan terminates (the watchdog). walkLoadFile is recursive over a
//     graph the load edges describe, and `visited` is the only thing stopping
//     a cycle.
//
// NOT asserted: which files are collected, how many definitions are found, or
// that anything resolves. A tree of garbage yielding nothing is correct.
func FuzzWorkspaceScan(f *testing.F) {
	add := func(a, b string, sc []byte) { f.Add([]byte(a), []byte(b), sc) }

	scripts := [][]byte{
		{0, 0, 0, 0, 0, 0, 0, 0},
		{1, 3, 1, 2, 0, 1, 1, 0, 1, 0, 1, 1, 1},
		{1, 4, 3, 3, 2, 5, 6, 1, 1, 1, 0, 0, 2, 2},
		{1, 2, 1, 1, 3, 1, 2, 3, 4, 5, 6, 7, 8},
		{255, 255, 255, 255, 255, 255, 255, 255},
		{1, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0}, // MaxFiles small -> Truncated
	}

	const defs = `(in-package 'wsdemo)
(use-package 'lisp)
(defun add (a b) "add two" (+ a b))
(defmacro defthing (name args &rest body)
  (quasiquote (defun (unquote name) (unquote args) (unquote-splicing body))))
(deftype point (x y) x)
(set 'answer 42)
(export 'add)
(export 'defthing)`

	const uses = `(in-package 'user)
(use-package 'wsdemo)
(defun main () (add 1 2))
(defthing helper (x) (+ x 1))`

	for _, sc := range scripts {
		add(defs, uses, sc)
		add(uses, defs, sc)
		add("", "", sc)
	}

	// A bare file (no in-package) is what makes the load tree load-bearing:
	// its package is inherited through main.lisp's load edges, which is the
	// only thing buildLoadTree exists for.
	add("(defun bare () 1)", defs, scripts[1])
	add(defs, "(defun bare () 1)\n(bare)", scripts[1])

	// The token the harness strips, present in the input, so the neutralising
	// substitution itself is on the seeded path.
	add("(load-file \"../../../etc/passwd\")\n(defun f () 1)", defs, scripts[1])

	for _, seed := range fuzzseed.Adversarial() {
		add(string(seed), defs, scripts[0])
		add(defs, string(seed), scripts[2])
	}

	f.Fuzz(func(t *testing.T, a, b, scriptBytes []byte) {
		runWorkspaceScanBudgeted(t, t.TempDir(), a, b, scriptBytes)
	})
}

func runWorkspaceScanBudgeted(t fatalf, root string, a, b, scriptBytes []byte) {
	t.Helper()

	done := make(chan error, 1)
	go func() {
		_, err := runWorkspaceScan(root, a, b, scriptBytes)
		done <- err
	}()

	budget := fuzzwatch.New(wsWatchdogTimeout)
	wait := budget.Total()
	for {
		select {
		case err := <-done:
			if err != nil {
				t.Fatalf("%v\n--- a (%d bytes) ---\n%q\n--- b (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					err, len(a), a, len(b), b, len(scriptBytes), scriptBytes)
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
				t.Fatalf("the workspace scan did not terminate within %s of SCHEDULED"+
					" time (%s). walkLoadFile recurses over the load graph and only"+
					" `visited` bounds it"+
					"\n--- a (%d bytes) ---\n%q\n--- b (%d bytes) ---\n%q"+
					"\n--- script (%d bytes) ---\n%q",
					budget.Total(), report, len(a), a, len(b), b,
					len(scriptBytes), scriptBytes)
				return
			}
		}
	}
}
