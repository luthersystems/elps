// Copyright © 2026 The ELPS authors

package analysis

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// Guards for the two analysis fuzz targets.
//
// These exist because of what PR #348 found by measuring rather than reading:
// three separate harness bugs where every operation "ran", every test passed,
// and whole functions sat at 0% because the harness was quietly starving them.
// A fuzz target cannot report that it is covering less than it thinks. These
// tests assert the specific reachability claims the harness comments make, so
// that a future change which breaks one fails CI instead of silently reducing
// what the nightly sweep explores.

// TestAnalyzeFuzzSeedsExpandMacros is the sharpest of these guards.
//
// Setting cfg.MacroExpander looks sufficient for the analysis-time evaluation
// path to be covered, and is not: the analyser consults the expander only for a
// head symbol it does not already know, so a stdlib macro (known) and a macro
// defined in the file under analysis (not in the env) both miss. Only a macro
// that came from ANOTHER file is both present in the env and unknown to the
// analyser. If macroSeeds ever stops producing that shape, env.MacroCall is
// never reached, ExpandMacro returns nil every time for the boring reason, and
// the target's ExpansionPanics assertion becomes vacuous while still passing.
func TestAnalyzeFuzzSeedsExpandMacros(t *testing.T) {
	// Exactly the cross product the seed corpus contains, so this guard is
	// about the CORPUS rather than about a script invented here.
	totalAsked, totalExpanded := 0, 0
	for i, ms := range macroSeeds {
		for j, sc := range analyzeScripts {
			stats, err := runAnalyze([]byte(ms[0]), []byte(ms[1]), sc)
			require.NoError(t, err, "macroSeeds[%d] with analyzeScripts[%d] failed", i, j)
			totalAsked += stats.asked
			totalExpanded += stats.expanded
		}
	}
	assert.Positive(t, totalAsked,
		"the analyser never consulted the macro expander at all")
	assert.Positive(t, totalExpanded,
		"no seed reached a SUCCESSFUL expansion, so env.MacroCall never ran and the"+
			" analysis-time evaluation path this target claims to cover is covered by"+
			" nothing. The shape that reaches it is a macro defined in the NEIGHBOURING"+
			" file (so LoadWorkspaceMacros put it in the env) and called from the file"+
			" under analysis (so the analyser does not already know it)")
}

// TestFuzzPreambleHeadsMatchScan pins the harness's copy of the preamble head
// list to the switch it was copied from. If workspace.go starts collecting a
// new form and this list does not, the harness replays a SUBSET of what
// production replays and the difference is invisible.
func TestFuzzPreambleHeadsMatchScan(t *testing.T) {
	src, err := os.ReadFile("workspace.go")
	require.NoError(t, err)

	// The `case "in-package", "use-package", ...:` arm inside scanFileFull.
	re := regexp.MustCompile(`(?s)switch astutil\.HeadSymbol\(expr\) \{\s*case ([^:]+):\s*preamble = append`)
	m := re.FindSubmatch(src)
	require.NotNil(t, m, "could not locate scanFileFull's preamble switch in workspace.go;"+
		" this guard has gone stale and must be repointed, not deleted")

	found := map[string]bool{}
	for _, lit := range strings.Split(string(m[1]), ",") {
		if s := strings.Trim(strings.TrimSpace(lit), `"`); s != "" {
			found[s] = true
		}
	}
	assert.Equal(t, found, preambleHeads,
		"the fuzz harness's preambleHeads no longer matches scanFileFull's switch;"+
			" the harness would replay a different preamble than production does")
}

// TestAnalyzeFuzzSeedsBuildScopes checks the seeds are not all degenerate: at
// least one produces a Result with symbols and references. A corpus that parses
// to nothing exercises the analyser's early returns and nothing else, which is
// exactly how #348's didChange bug hid.
func TestAnalyzeFuzzSeedsBuildScopes(t *testing.T) {
	stats, err := runAnalyze([]byte(fuzzSeedUses), []byte(fuzzSeedDefs), []byte{4, 1, 2, 3, 1, 0, 1, 2})
	require.NoError(t, err)
	assert.Positive(t, stats.symbols, "the primary seed produced no symbols")
	assert.Positive(t, stats.refs, "the primary seed produced no references")
}

// TestWorkspaceFuzzSeedsCollectFiles checks the workspace harness actually
// materialises and collects a tree. A bug in writeWorkspace would leave the
// scan looking at an empty directory: every call would return promptly, every
// seed would pass, and the whole of workspace.go past collectLispFiles would be
// unreached.
func TestWorkspaceFuzzSeedsCollectFiles(t *testing.T) {
	// Script with MaxFiles unconstrained and main.lisp written.
	sc := []byte{1, 3, 1, 2, 0, 1, 1, 0, 1, 0, 1, 1, 1}
	const defs = `(in-package 'wsdemo)
(defun add (a b) (+ a b))
(defmacro defthing (name args &rest body) (quasiquote (defun (unquote name) (unquote args) (unquote-splicing body))))
(export 'add)`
	stats, err := runWorkspaceScan(t.TempDir(), []byte(defs), []byte("(in-package 'user)\n(use-package 'wsdemo)\n(defun main () (add 1 2))"), sc)
	require.NoError(t, err)
	assert.Positive(t, stats.files, "the scan collected no files at all")
	assert.Positive(t, stats.defs, "the scan found no definitions")
	assert.Positive(t, stats.preamble,
		"the scan collected no preamble forms, so LoadWorkspaceMacros had nothing to"+
			" evaluate and the macro-loading path is unreached")
	assert.Positive(t, stats.defForms,
		"no DefFormSpec was derived from a def*-prefixed macro; extractDefFormSpecs"+
			" and the whole def-like analysis path are unreached")
}

// TestWorkspaceFuzzContainsLoadFile is the containment claim as a test.
//
// walkLoadFile joins a load-file argument to the containing directory and
// recurses, so `(load-file "../../../etc/passwd")` in a fuzzer-derived file
// would make this target a file-read primitive pointed at the machine running
// it. The harness strips the token; this proves the strip works, that it
// preserves byte offsets, and that the only load edges that survive are the
// harness's own.
func TestWorkspaceFuzzContainsLoadFile(t *testing.T) {
	hostile := []byte(`(in-package 'user)
(load-file "../../../etc/passwd")
(load-file "/etc/shadow")
(defun f () 1)`)

	safe := neutraliseLoadFile(hostile)
	assert.Len(t, safe, len(hostile),
		"the substitution changed the length, so every token.Location downstream"+
			" of it now differs from what the original bytes would have produced")
	assert.NotContains(t, string(safe), "load-file",
		"a load edge survived neutralisation")

	// End to end: run the harness on the hostile input and check every file the
	// scan reports is inside the temp root.
	root := t.TempDir()
	stats, err := runWorkspaceScan(root, hostile, hostile, []byte{1, 3, 1, 2, 0, 1, 1, 0})
	require.NoError(t, err)
	assert.Positive(t, stats.files)

	// And that the neutralised token is what is actually on disk.
	for _, name := range []string{wsPlainFile, wsSubFile} {
		b, readErr := os.ReadFile(filepath.Join(root, filepath.FromSlash(name))) //nolint:gosec // fixed name under t.TempDir()
		require.NoError(t, readErr)
		assert.NotContains(t, string(b), "load-file",
			"%s went to disk with a live load edge in it", name)
	}
}

// TestWorkspaceFuzzLoadTreeIsExercised checks the harness's own load edges
// actually build a load tree, including the self-cycle that is the only thing
// testing walkLoadFile's `visited` guard. Without this, main.lisp could quietly
// stop being written and buildLoadTree would go to 0% with every seed passing.
func TestWorkspaceFuzzLoadTreeIsExercised(t *testing.T) {
	root := t.TempDir()
	sc := &script{b: []byte{1, 3, 1, 2, 0, 1, 1, 0, 1, 0, 1, 1, 1}}
	require.NoError(t, writeWorkspace(root, sc, []byte("(defun a () 1)"), []byte("(defun b () 2)")))

	main, err := os.ReadFile(filepath.Join(root, wsMainFile)) //nolint:gosec // fixed name under t.TempDir()
	require.NoError(t, err, "main.lisp was not written, so buildLoadTree never runs")
	assert.Contains(t, string(main), "(load-file ",
		"main.lisp carries no load edge, so walkLoadFile's recursion is unreached")

	pkgMap, order := buildLoadTree(filepath.Join(root, wsMainFile))
	assert.NotEmpty(t, order, "the load walk visited nothing")
	for path := range pkgMap {
		rel, relErr := filepath.Rel(root, path)
		require.NoError(t, relErr)
		assert.False(t, rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)),
			"the load tree escaped the workspace root: %q", path)
	}
}
