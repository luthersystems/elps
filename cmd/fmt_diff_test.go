// Copyright © 2024 The ELPS authors

package cmd

import (
	"bytes"
	"fmt"
	"math/rand"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"
)

func unifiedDiff(t *testing.T, a, b string) string {
	t.Helper()
	var buf bytes.Buffer
	require.NoError(t, writeUnifiedDiff(&buf, "f.lisp", []byte(a), []byte(b)))
	return buf.String()
}

// TestFmtDiffResyncsAfterChange pins luthersystems/elps#943. `elps fmt -d`
// compared the files at equal line indexes, so one inserted or deleted line
// made every later line show as changed.
func TestFmtDiffResyncsAfterChange(t *testing.T) {
	var a, b strings.Builder
	a.WriteString("(a)\n\n\n")
	b.WriteString("(a)\n\n")
	for i := range 40 {
		fmt.Fprintf(&a, "(line %d)\n", i)
		fmt.Fprintf(&b, "(line %d)\n", i)
	}
	a.WriteString("(z   1)\n")
	b.WriteString("(z 1)\n")

	require.Equal(t, strings.Join([]string{
		"--- f.lisp",
		"+++ f.lisp",
		"@@ -1,6 +1,5 @@",
		" (a)",
		" ",
		"-",
		" (line 0)",
		" (line 1)",
		" (line 2)",
		"@@ -41,4 +40,4 @@",
		" (line 37)",
		" (line 38)",
		" (line 39)",
		"-(z   1)",
		"+(z 1)",
		"",
	}, "\n"), unifiedDiff(t, a.String(), b.String()))
}

func TestFmtDiffFormat(t *testing.T) {
	for _, tc := range []struct{ name, a, b, want string }{
		{"equal", "a\nb\n", "a\nb\n", ""},
		{"insert into empty", "", "a\nb\n", "@@ -0,0 +1,2 @@\n+a\n+b\n"},
		{"delete all", "a\n", "", "@@ -1 +0,0 @@\n-a\n"},
		{"insert at end", "a\n", "a\nb\n", "@@ -1 +1,2 @@\n a\n+b\n"},
		{"insert at start", "b\n", "a\nb\n", "@@ -1 +1,2 @@\n+a\n b\n"},
		{"add final newline", "a\nb", "a\nb\n", "@@ -1,2 +1,2 @@\n a\n-b\n\\ No newline at end of file\n+b\n"},
		{"drop final newline", "a\n", "a", "@@ -1 +1 @@\n-a\n+a\n\\ No newline at end of file\n"},
		{"changes six lines apart share a hunk",
			"x\n1\n2\n3\n4\n5\n6\ny\n", "X\n1\n2\n3\n4\n5\n6\nY\n",
			"@@ -1,8 +1,8 @@\n-x\n+X\n 1\n 2\n 3\n 4\n 5\n 6\n-y\n+Y\n"},
		{"changes seven lines apart get two hunks",
			"x\n1\n2\n3\n4\n5\n6\n7\ny\n", "X\n1\n2\n3\n4\n5\n6\n7\nY\n",
			"@@ -1,4 +1,4 @@\n-x\n+X\n 1\n 2\n 3\n@@ -6,4 +6,4 @@\n 5\n 6\n 7\n-y\n+Y\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			require.Equal(t, "--- f.lisp\n+++ f.lisp\n"+tc.want, unifiedDiff(t, tc.a, tc.b))
		})
	}
}

var hunkHeader = regexp.MustCompile(`^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@$`)

// applyUnifiedDiff applies a diff to a, as patch(1) does, and checks every
// hunk header against the hunk body.
func applyUnifiedDiff(t *testing.T, a, diff string) string {
	t.Helper()
	src := splitLines([]byte(a))
	lines := splitLines([]byte(diff))
	require.GreaterOrEqual(t, len(lines), 2)
	lines = lines[2:]
	var out strings.Builder
	pos := 0
	num := func(s string, def int) int {
		if s == "" {
			return def
		}
		n, err := strconv.Atoi(s)
		require.NoError(t, err)
		return n
	}
	for i := 0; i < len(lines); {
		m := hunkHeader.FindStringSubmatch(strings.TrimSuffix(lines[i], "\n"))
		require.NotNil(t, m, "hunk header %q", lines[i])
		i++
		aStart, aCount := num(m[1], 0), num(m[2], 1)
		bStart, bCount := num(m[3], 0), num(m[4], 1)
		if aCount > 0 {
			aStart--
		}
		if bCount > 0 {
			bStart--
		}
		require.GreaterOrEqual(t, aStart, pos, "hunks overlap or are out of order")
		for ; pos < aStart; pos++ {
			out.WriteString(src[pos])
		}
		require.Equal(t, bStart, strings.Count(out.String(), "\n"), "new-file line of the hunk")
		for aCount > 0 || bCount > 0 {
			require.Less(t, i, len(lines))
			kind, text := lines[i][0], lines[i][1:]
			i++
			if i < len(lines) && strings.HasPrefix(lines[i], `\`) {
				text = strings.TrimSuffix(text, "\n")
				i++
			}
			if kind != '+' {
				require.Equal(t, src[pos], text)
				pos++
				aCount--
			}
			if kind != '-' {
				out.WriteString(text)
				bCount--
			}
		}
		require.Zero(t, aCount)
		require.Zero(t, bCount)
	}
	for ; pos < len(src); pos++ {
		out.WriteString(src[pos])
	}
	return out.String()
}

// lcsLen is the textbook table. It is the oracle for the edit count.
func lcsLen(a, b []string) int {
	prev := make([]int, len(b)+1)
	cur := make([]int, len(b)+1)
	for i := range a {
		for j := range b {
			if a[i] == b[j] {
				cur[j+1] = prev[j] + 1
			} else {
				cur[j+1] = max(cur[j], prev[j+1])
			}
		}
		prev, cur = cur, prev
	}
	return prev[len(b)]
}

// TestFmtDiffRandom checks on random files that the diff applies to give the
// formatted file and that the edit script is a shortest one.
func TestFmtDiffRandom(t *testing.T) {
	rng := rand.New(rand.NewSource(1)) //nolint:gosec // test data, not a secret
	gen := func(n, alphabet int) string {
		var sb strings.Builder
		for range n {
			fmt.Fprintf(&sb, "l%d\n", rng.Intn(alphabet))
		}
		s := sb.String()
		if s != "" && rng.Intn(4) == 0 {
			s = s[:len(s)-1]
		}
		return s
	}
	for range 3000 {
		alphabet := 1 + rng.Intn(6)
		a := gen(rng.Intn(30), alphabet)
		b := gen(rng.Intn(30), alphabet)
		if rng.Intn(3) == 0 {
			// A near copy: the usual input of fmt -d.
			lines := splitLines([]byte(a))
			if len(lines) > 0 {
				lines[rng.Intn(len(lines))] = "changed\n"
			}
			b = strings.Join(lines, "")
		}
		diff := unifiedDiff(t, a, b)
		require.Equal(t, b, applyUnifiedDiff(t, a, diff), "a=%q b=%q diff=%q", a, b, diff)

		la, lb := splitLines([]byte(a)), splitLines([]byte(b))
		changed := 0
		for _, e := range diffLines(la, lb) {
			if e.kind != ' ' {
				changed++
			}
		}
		require.Equal(t, len(la)+len(lb)-2*lcsLen(la, lb), changed, "a=%q b=%q", a, b)
	}
}

// TestFmtDiffLargeFile checks that a large file with changes at both ends
// needs neither a quadratic table nor quadratic time.
func TestFmtDiffLargeFile(t *testing.T) {
	var a, b strings.Builder
	b.WriteString("new first\n")
	for i := range 200000 {
		fmt.Fprintf(&a, "(line %d)\n", i)
		fmt.Fprintf(&b, "(line %d)\n", i)
	}
	a.WriteString("old last\n")
	diff := unifiedDiff(t, a.String(), b.String())
	require.Equal(t, 2, strings.Count(diff, "@@ -"))
	require.Equal(t, b.String(), applyUnifiedDiff(t, a.String(), diff))
}
