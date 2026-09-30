package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// The -<GOMAXPROCS> suffix is stripped only when it is known to be there
// (issue #767). At GOMAXPROCS=1 `go test` appends nothing, so a trailing -N is
// part of the benchmark's own name and must survive.
func TestProcsRuleStrip(t *testing.T) {
	cases := []struct {
		procs      int
		name, want string
		suffixed   bool
	}{
		{0, "EnvGet-4", "EnvGet", true}, // legacy: any trailing -N
		{0, "Package/get-nested-2", "Package/get-nested", true},
		{1, "Package/get-nested-2", "Package/get-nested-2", false},
		{1, "EnvGet", "EnvGet", false},
		{4, "EnvGet-4", "EnvGet", true},
		{4, "Package/get-nested-2-4", "Package/get-nested-2", true},
		{4, "Package/get-nested-2", "Package/get-nested-2", false},
		{2, "Encode-2", "Encode", true},
	}
	for _, c := range cases {
		r := procsRule{n: c.procs}
		if got := r.strip(c.name); got != c.want {
			t.Errorf("procs=%d strip(%q) = %q, want %q", c.procs, c.name, got, c.want)
		}
		if got := r.suffixed(c.name); got != c.suffixed {
			t.Errorf("procs=%d suffixed(%q) = %v, want %v", c.procs, c.name, got, c.suffixed)
		}
	}
}

const procsTable = `goos: linux
goarch: arm64
pkg: github.com/luthersystems/elps/lisp/lisplib/libjson
                          │     base     │                 pr                  │
                          │    sec/op    │    sec/op     vs base               │
ROW     1.000µ ± 2%    1.300µ ± 3%  +30.00% (p=0.000 n=10)
`

func runProcs(t *testing.T, row, waiver string, args ...string) (int, string) {
	t.Helper()
	dir := t.TempDir()
	tbl := filepath.Join(dir, "t.txt")
	if err := os.WriteFile(tbl, []byte(strings.Replace(procsTable, "ROW", row, 1)), 0o600); err != nil {
		t.Fatal(err)
	}
	w := filepath.Join(dir, "w.txt")
	line := "github.com/luthersystems/elps/lisp/lisplib/libjson | " + waiver +
		" | sec/op | 50 | 2099-01-01 | elps#767 | fixture for the GOMAXPROCS suffix rule\n"
	if err := os.WriteFile(w, []byte(line), 0o600); err != nil {
		t.Fatal(err)
	}
	t.Setenv("BENCH_WAIVERS", w)
	t.Setenv("BENCH_GOMAXPROCS", "")
	var out, errb bytes.Buffer
	rc := run(append(args, tbl), &out, &errb)
	return rc, out.String() + errb.String()
}

func TestGomaxprocsSuffixEndToEnd(t *testing.T) {
	cases := []struct {
		name, row, waiver string
		args              []string
		want              int
		contains          string
	}{
		{"at 1, a real trailing -N is part of the name and a waiver binds it",
			"Package/get-nested-2", "Package/get-nested-2", []string{"-gomaxprocs", "1"}, 0, "WAIVED"},
		{"at 1, the trailing -N is NOT eaten, so a shortened waiver does not bind",
			"Package/get-nested-2", "Package/get-nested", []string{"-gomaxprocs", "1"}, 1, "REGRESSION"},
		{"at 4, the -4 suffix is stripped and the waiver binds (substrate)",
			"EnvGet-4", "EnvGet", []string{"-gomaxprocs", "4"}, 0, "WAIVED"},
		{"at 4, only -4 is stripped: a name ending -2 keeps it",
			"Package/get-nested-2-4", "Package/get-nested-2", []string{"-gomaxprocs", "4"}, 0, "WAIVED"},
		{"at 4, a waiver written with -4 is rejected",
			"EnvGet-4", "EnvGet-4", []string{"-gomaxprocs", "4"}, 2, "GOMAXPROCS"},
		{"legacy default (unset) still strips -4 exactly as before",
			"EnvGet-4", "EnvGet", nil, 0, "WAIVED"},
		{"legacy default still rejects a suffixed-looking waiver",
			"EnvGet-4", "EnvGet-4", nil, 2, "GOMAXPROCS"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			rc, out := runProcs(t, c.row, c.waiver, c.args...)
			if rc != c.want {
				t.Fatalf("exit %d, want %d\n%s", rc, c.want, out)
			}
			if !strings.Contains(out, c.contains) {
				t.Fatalf("output lacks %q\n%s", c.contains, out)
			}
		})
	}
}

func TestGomaxprocsFlagValidation(t *testing.T) {
	for _, v := range []string{"-1", "x"} {
		var out, errb bytes.Buffer
		t.Setenv("BENCH_GOMAXPROCS", "")
		if rc := run([]string{"-gomaxprocs", v, "nonexistent"}, &out, &errb); rc != 2 {
			t.Errorf("-gomaxprocs %s: exit %d, want 2", v, rc)
		}
	}
}
