// Copyright © 2026 The ELPS authors

package lisp_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/luthersystems/elps/lisp"
)

// TestCoWCounterInactiveByDefault pins the production behaviour of the
// copy-on-write census (issue #378): nothing is recorded and nothing is
// reported unless the elpscheck tag is on.
func TestCoWCounterInactiveByDefault(t *testing.T) {
	if elpscheckActive {
		t.Skip("elpscheck build: the census is active by design")
	}
	env := newCowTestEnv(t)
	for _, src := range []string{
		`(stable-sort > '(3 1 2))`,
		`(slice 'vector '(1 2 3) 0 2)`,
		`(append 'vector '(1 2 3) 4)`,
	} {
		if v := env.LoadString("cow.lisp", src); v.Type == lisp.LError {
			t.Fatalf("eval %q: %v", src, v)
		}
	}
	if counts := lisp.CoWOnSealedCounts(); counts != nil {
		t.Errorf("CoWOnSealedCounts() = %v in an untagged build, want nil", counts)
	}
	if report := lisp.CoWOnSealedReport(); report != "" {
		t.Errorf("CoWOnSealedReport() = %q in an untagged build, want empty", report)
	}
}

// TestNoCoWCounterSymbolsUntagged is the zero-cost proof.  The census is
// only defensible if a release binary carries none of it, so this builds
// the elps command the way a release does — no build tags — and asserts
// that neither the counter's symbols nor its site strings survive into
// the executable.  The whole mechanism must compile away, not merely be
// cheap.
//
// The anti-vacuity half builds the same command WITH the tag and requires
// the symbols to appear, so a test that silently stopped looking (a
// renamed symbol, a build that failed to produce anything) fails instead
// of passing.
func TestNoCoWCounterSymbolsUntagged(t *testing.T) {
	if testing.Short() {
		t.Skip("short mode: this test compiles the elps command twice")
	}
	if _, err := exec.LookPath("go"); err != nil {
		t.Skipf("go toolchain unavailable: %v", err)
	}
	root, err := filepath.Abs("..")
	if err != nil {
		t.Fatalf("module root: %v", err)
	}
	dir := t.TempDir()

	build := func(tags string) string {
		t.Helper()
		out := filepath.Join(dir, "elps-untagged")
		args := []string{"build", "-o", out}
		if tags != "" {
			args = append(args, "-tags", tags)
			out = filepath.Join(dir, "elps-"+tags)
			args[2] = out
		}
		args = append(args, ".")
		cmd := exec.Command("go", args...)
		cmd.Dir = root
		if b, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("go build -tags %q: %v\n%s", tags, err, b)
		}
		return out
	}

	symbols := func(bin string) string {
		t.Helper()
		cmd := exec.Command("go", "tool", "nm", bin)
		b, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("go tool nm: %v\n%s", err, b)
		}
		return string(b)
	}

	// The identifiers that exist only to serve the census, and the site
	// strings the instrumented call sites pass to it.
	wantAbsentSymbols := []string{"cowCheck", "cowLispLocation", "recordCoWOnSealed", "cowEventKey"}
	wantAbsentStrings := []string{"slice-vector", "append-vector", "elps-cow:"}

	untagged := build("")
	nm := symbols(untagged)
	for _, sym := range wantAbsentSymbols {
		if strings.Contains(nm, sym) {
			t.Errorf("untagged binary contains the counter symbol %q; the census must compile away entirely", sym)
		}
	}
	image, err := os.ReadFile(untagged)
	if err != nil {
		t.Fatalf("read binary: %v", err)
	}
	for _, s := range wantAbsentStrings {
		if strings.Contains(string(image), s) {
			t.Errorf("untagged binary contains the census string %q; the site names must die with the inlined call", s)
		}
	}

	// Anti-vacuity: with the tag on, the same names must be there.
	tagged := symbols(build("elpscheck"))
	for _, sym := range wantAbsentSymbols {
		if !strings.Contains(tagged, sym) {
			t.Errorf("elpscheck binary lacks %q: the untagged check above proves nothing", sym)
		}
	}
}
