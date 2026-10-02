package main

import (
	"bytes"
	"strings"
	"testing"
)

func judge(t *testing.T, overrides string) (int, string) {
	t.Helper()
	var out, errb bytes.Buffer
	rc := run([]string{
		"-today", "2026-09-29",
		"-overrides", "testdata/" + overrides,
		"-go-report", "testdata/go-report.txt",
		"-lisp-base", "testdata/lisp-base.json",
		"-lisp-head", "testdata/lisp-head.json",
	}, &out, &errb)
	return rc, out.String() + errb.String()
}

func TestGate(t *testing.T) {
	for _, tc := range []struct {
		overrides string
		rc        int
		want      []string
	}{
		{"overrides-empty.txt", 1, []string{
			"BREAK         go lisp.QSymbol: removed",
			"BREAK         lisp time:sleep: removed",
			"BREAK         lisp gone:*: package removed",
			"3 breaking change(s), 0 waived, 3 unwaived",
			"lisp | time:sleep | 2026-12-29 | <issue",
		}},
		{"overrides-ok.txt", 0, []string{"3 breaking change(s), 3 waived, 0 unwaived"}},
		{"overrides-expired.txt", 1, []string{"EXPIRED 2000-01-01", "1 unwaived"}},
		{"overrides-bad.txt", 2, []string{"OVERRIDE-BAD", "not a YYYY-MM-DD date"}},
		// Two entries for one break would make the verdict depend on which
		// one the matcher meets first; refuse the file instead.
		{"overrides-dup.txt", 2, []string{"OVERRIDE-BAD", ":5", "duplicate override for lisp|time:sleep", "first at line 3"}},
	} {
		rc, out := judge(t, tc.overrides)
		if rc != tc.rc {
			t.Errorf("%s: exit %d, want %d\n%s", tc.overrides, rc, tc.rc, out)
		}
		for _, w := range tc.want {
			if !strings.Contains(out, w) {
				t.Errorf("%s: output lacks %q\n%s", tc.overrides, w, out)
			}
		}
	}
	// Compatible changes (an added optional, a renamed formal, a new macro)
	// are not breaks.
	_, out := judge(t, "overrides-empty.txt")
	for _, s := range []string{"time:now", "time:parse", "time:added"} {
		if strings.Contains(out, s) {
			t.Errorf("compatible change %s reported as a break\n%s", s, out)
		}
	}
}

func TestFormalsCompatible(t *testing.T) {
	f := func(req, opt []string, rest string, keys []string) *formalsDoc {
		return &formalsDoc{Required: req, Optional: opt, Rest: rest, Keys: keys}
	}
	a := []string{"a"}
	ab := []string{"a", "b"}
	for _, tc := range []struct {
		name       string
		base, head *formalsDoc
		ok         bool
	}{
		{"same", f(a, nil, "", nil), f(a, nil, "", nil), true},
		{"new required", f(a, nil, "", nil), f(ab, nil, "", nil), false},
		{"required to optional", f(ab, nil, "", nil), f(a, []string{"b"}, "", nil), true},
		{"dropped optional", f(a, []string{"b"}, "", nil), f(a, nil, "", nil), false},
		{"optional absorbed by rest", f(a, []string{"b"}, "", nil), f(a, nil, "r", nil), true},
		{"dropped rest", f(a, nil, "r", nil), f(ab, nil, "", nil), false},
		{"dropped key", f(nil, nil, "", ab), f(nil, nil, "", a), false},
		{"added key", f(nil, nil, "", a), f(nil, nil, "", ab), true},
	} {
		if got := formalsCompatible(tc.base, tc.head); got != tc.ok {
			t.Errorf("%s: got %v want %v", tc.name, got, tc.ok)
		}
	}
}

func TestGoReportRejectsUnknownLines(t *testing.T) {
	if _, err := parseGoReport(strings.NewReader("Compatible changes:\n")); err == nil {
		t.Fatal("an unrecognized apidiff line must be an error, not zero breaks")
	}
}

// apidiff prints a "! second, different message" note, with its "first:" and
// "second:" lines, when it reports one object twice (a generic type changed
// under several instantiations).  The note is skipped; the "-" line that
// follows it is the break.  A note line out of place is still an error.
func TestGoReportSkipsDuplicateMessageNotes(t *testing.T) {
	report := "! second, different message for obj type example.com/m/p.D[T any] func(i int) T, isNew false, part \"\"\n" +
		"  first:  changed from func(int) T to struct{f func(i int) T}\n" +
		"  second: changed from func(int) A to struct{f func(i int) A}\n" +
		"- ./p.D: changed from func(int) T to struct{f func(i int) T}\n"
	got, err := parseGoReport(strings.NewReader(report))
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].symbol != "p.D" || got[0].what != "changed from func(int) T to struct{f func(i int) T}" {
		t.Fatalf("got %+v, want the one p.D break", got)
	}
	for _, bad := range []string{
		"  first:  changed from func(int) T to struct{}\n",
		"! second, different message for obj x, isNew false, part \"\"\n  first:  a\n  second: b\n  second: c\n",
		"! second, different message for obj x, isNew false, part \"\"\n- ./p.D: changed\n",
	} {
		if _, err := parseGoReport(strings.NewReader(bad)); err == nil {
			t.Errorf("%q: a note line out of place must be an error", bad)
		}
	}
}
