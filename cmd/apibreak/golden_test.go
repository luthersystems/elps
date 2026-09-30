package main

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestGoldenGate(t *testing.T) {
	for _, tc := range []struct {
		name, overrides string
		rc              int
		want            []string
	}{
		{"unwaived", "overrides-empty.txt", 1, []string{
			"BREAK         golden canonical.txt:changed: bytes changed",
			"BREAK         golden canonical.txt:removed: entry removed",
			"BREAK         golden deleted.txt:record: entry removed",
			"3 breaking change(s), 0 waived, 3 unwaived",
			"golden | canonical.txt:changed | 2026-12-29 | <issue",
		}},
		{"waived", "overrides-golden.txt", 0, []string{"3 breaking change(s), 3 waived, 0 unwaived"}},
		{"expired", "overrides-golden-expired.txt", 1, []string{"EXPIRED 2000-01-01", "1 unwaived"}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			var out, errb bytes.Buffer
			rc := run([]string{
				"-today", "2026-09-29", "-overrides", "testdata/" + tc.overrides,
				"-golden-base", "testdata/golden-base", "-golden-head", "testdata/golden-head",
			}, &out, &errb)
			output := out.String() + errb.String()
			if rc != tc.rc {
				t.Fatalf("exit %d, want %d\n%s", rc, tc.rc, output)
			}
			for _, want := range tc.want {
				if !strings.Contains(output, want) {
					t.Errorf("output lacks %q\n%s", want, output)
				}
			}
			for _, name := range []string{"unchanged", "added"} {
				if strings.Contains(output, ":"+name) {
					t.Errorf("compatible entry %s reported\n%s", name, output)
				}
			}
		})
	}
}

func TestGoldenGateInputs(t *testing.T) {
	for _, tc := range []struct {
		name, base, head string
		rc               int
	}{
		{"same", "a\t1\n", "a\t1\n", 0},
		{"addition", "a\t1\n", "b\t2\na\t1\n", 0},
		{"new file", "", "a\t1\n", 0},
		{"removed last entry", "a\t1\n", "", 1},
		{"number spelling", "a\t1\n", "a\t1.0\n", 1},
		{"escape spelling", "a\t\"\\u003c\"\n", "a\t\"<\"\n", 1},
		{"whitespace", "a\t[1]\n", "a\t[1] \n", 1},
		{"CR bytes", "a\t1\n", "a\t1\r\n", 1},
		{"duplicate base", "a\t1\na\t2\n", "a\t1\n", 2},
		{"duplicate head", "a\t1\n", "a\t1\na\t2\n", 2},
		{"malformed base", "no tab\n", "a\t1\n", 2},
		{"malformed head", "a\t1\n", "no tab\n", 2},
		{"empty name", "a\t1\n", "\t1\n", 2},
		{"empty bytes", "a\t1\n", "a\t\n", 2},
	} {
		t.Run(tc.name, func(t *testing.T) {
			base, head := t.TempDir(), t.TempDir()
			for dir, data := range map[string]string{base: tc.base, head: tc.head} {
				if data == "" { // Missing directories/files represent additions or removals.
					continue
				}
				if err := os.WriteFile(filepath.Join(dir, "canonical.txt"), []byte(data), 0o600); err != nil {
					t.Fatal(err)
				}
			}
			var out, errb bytes.Buffer
			rc := run([]string{"-overrides", "testdata/overrides-empty.txt", "-golden-base", base, "-golden-head", head}, &out, &errb)
			if rc != tc.rc {
				t.Fatalf("exit %d, want %d\n%s%s", rc, tc.rc, &out, &errb)
			}
		})
	}
	// The base predating the corpus is allowed; deleting the entire head corpus is a break.
	for _, tc := range []struct {
		base, head string
		rc         int
	}{
		{"testdata/missing-golden", "testdata/golden-head", 0},
		{"testdata/golden-base", "testdata/missing-golden", 1},
		{"testdata/golden-base", "", 2},
		{"", "testdata/golden-head", 2},
	} {
		var out, errb bytes.Buffer
		rc := run([]string{"-overrides", "testdata/overrides-empty.txt", "-golden-base", tc.base, "-golden-head", tc.head}, &out, &errb)
		if rc != tc.rc {
			t.Errorf("base %q, head %q: exit %d, want %d\n%s%s", tc.base, tc.head, rc, tc.rc, &out, &errb)
		}
	}
}
