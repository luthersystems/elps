// Copyright © 2024 The ELPS authors

package cmd

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// diffContext is the number of unchanged lines shown around each change. It
// is the default of diff -u, which patch and code review tools expect.
const diffContext = 3

// lineEdit is one line of an edit script. An equal line advances both files.
// A delete advances the original file only. An insert advances the formatted
// file only.
type lineEdit struct {
	kind byte // ' ', '-' or '+'
	a, b int  // index of the line in the original and in the formatted file
}

// writeUnifiedDiff writes the difference between original and formatted as a
// unified diff with hunks, the format of diff -u.
//
// `elps fmt -d` used to compare the two files line by line at the same index.
// After the first inserted or deleted line the indexes no longer matched, so
// every later line showed as changed (luthersystems/elps#943). A reader could
// not see what fmt changes. This function computes a shortest edit script, so
// an unchanged line is never reported, and patch(1) can apply the output.
func writeUnifiedDiff(w io.Writer, path string, original, formatted []byte) error {
	a := splitLines(original)
	b := splitLines(formatted)
	edits := diffLines(a, b)

	bw := bufio.NewWriter(w)
	fmt.Fprintf(bw, "--- %s\n", path) //nolint:errcheck // bufio keeps the first error for Flush
	fmt.Fprintf(bw, "+++ %s\n", path) //nolint:errcheck // bufio keeps the first error for Flush
	for start := 0; start < len(edits); {
		// A hunk starts diffContext lines before a change. It ends when more
		// than 2*diffContext equal lines follow a change, because the context
		// of two changes that are closer than that overlaps.
		for start < len(edits) && edits[start].kind == ' ' {
			start++
		}
		if start == len(edits) {
			break
		}
		lo := max(start-diffContext, 0)
		hi := start
		for i := start; i < len(edits) && i-hi <= 2*diffContext; i++ {
			if edits[i].kind != ' ' {
				hi = i + 1
			}
		}
		hi = min(hi+diffContext, len(edits))

		var aCount, bCount int
		for _, e := range edits[lo:hi] {
			if e.kind != '+' {
				aCount++
			}
			if e.kind != '-' {
				bCount++
			}
		}
		fmt.Fprintf(bw, "@@ -%s +%s @@\n", hunkRange(edits[lo].a, aCount), hunkRange(edits[lo].b, bCount)) //nolint:errcheck // bufio keeps the first error for Flush
		for _, e := range edits[lo:hi] {
			line := b
			idx := e.b
			if e.kind != '+' {
				line, idx = a, e.a
			}
			bw.WriteByte(e.kind)      //nolint:errcheck,gosec // bufio keeps the first error for Flush
			bw.WriteString(line[idx]) //nolint:errcheck,gosec // bufio keeps the first error for Flush
			if !strings.HasSuffix(line[idx], "\n") {
				// The marker diff -u uses. Without it patch cannot tell a
				// last line with a newline from one without.
				bw.WriteString("\n\\ No newline at end of file\n") //nolint:errcheck,gosec // bufio keeps the first error for Flush
			}
		}
		start = hi
	}
	return bw.Flush()
}

// hunkRange formats one side of a hunk header. first is the zero-based index
// of the first line of the range. An empty range names the line before it,
// and a range of one line has no count. Both are rules of the format.
func hunkRange(first, count int) string {
	switch count {
	case 0:
		return fmt.Sprintf("%d,0", first)
	case 1:
		return strconv.Itoa(first + 1)
	default:
		return fmt.Sprintf("%d,%d", first+1, count)
	}
}

// splitLines splits data into lines. Each line keeps its line feed, so a last
// line without one compares as different from the same text with one.
func splitLines(data []byte) []string {
	var lines []string
	for len(data) > 0 {
		end := bytes.IndexByte(data, '\n') + 1
		if end == 0 {
			end = len(data)
		}
		lines = append(lines, string(data[:end]))
		data = data[end:]
	}
	return lines
}

// diffLines returns a shortest edit script that turns a into b.
//
// It is the linear space variant of the algorithm of E. Myers, "An O(ND)
// Difference Algorithm and Its Variations" (1986). The time is O((N+M)D) for
// D changed lines and the memory is O(N+M). A table of longest common
// subsequences is simpler, but it needs N*M cells: 800 MB for two files of
// 10000 lines. No module that elps depends on outside its tests has a line
// diff, and a new dependency is not worth one function.
func diffLines(a, b []string) []lineEdit {
	// Compare line numbers and not strings: the search compares the same
	// lines many times.
	ids := make(map[string]int)
	intern := func(lines []string) []int {
		out := make([]int, len(lines))
		for i, line := range lines {
			id, ok := ids[line]
			if !ok {
				id = len(ids)
				ids[line] = id
			}
			out[i] = id
		}
		return out
	}
	d := differ{a: intern(a), b: intern(b)}
	size := len(a) + len(b) + 4
	d.fwd = make([]int, size)
	d.rev = make([]int, size)
	d.edits = make([]lineEdit, 0, max(len(a), len(b)))
	d.diff(0, len(a), 0, len(b))
	deletesFirst(d.edits)
	return d.edits
}

// deletesFirst reorders each run of changed lines so that its deleted lines
// come before its inserted lines, as diff -u prints them. The search can
// return them in either order, and a reader expects the old text first.
func deletesFirst(edits []lineEdit) {
	for i := 0; i < len(edits); i++ {
		if edits[i].kind == ' ' {
			continue
		}
		end := i
		var ins []lineEdit
		w := i
		for ; end < len(edits) && edits[end].kind != ' '; end++ {
			if edits[end].kind == '+' {
				ins = append(ins, edits[end])
				continue
			}
			edits[w] = edits[end]
			w++
		}
		if len(ins) != end-i && len(ins) != 0 {
			// Keep the positions consistent with the new order: no line is
			// inserted before the deletes, and all deletes are done before
			// the inserts.
			bStart, aEnd := ins[0].b, edits[w-1].a+1
			for j := i; j < w; j++ {
				edits[j].b = bStart
			}
			for j := range ins {
				ins[j].a = aEnd
			}
		}
		copy(edits[w:end], ins)
		i = end
	}
}

// differ holds the state of one diffLines call. fwd and rev are the furthest
// reaching positions of the forward and the reverse search, one per diagonal.
// Every recursion step reuses them, so the search allocates nothing.
type differ struct {
	a, b     []int
	fwd, rev []int
	edits    []lineEdit
}

func (d *differ) emit(kind byte, a, b, n int) {
	for i := range n {
		e := lineEdit{kind: kind, a: a, b: b}
		if kind != '+' {
			e.a += i
		}
		if kind != '-' {
			e.b += i
		}
		d.edits = append(d.edits, e)
	}
}

// diff appends the edit script for a[a0:a1] and b[b0:b1].
func (d *differ) diff(a0, a1, b0, b1 int) {
	// Common first and last lines need no search. Removing them also
	// guarantees that the middle snake below splits the problem into two
	// smaller ones: what remains needs two edits or more.
	start := 0
	for a0+start < a1 && b0+start < b1 && d.a[a0+start] == d.b[b0+start] {
		start++
	}
	d.emit(' ', a0, b0, start)
	a0, b0 = a0+start, b0+start
	end := 0
	for a1-end > a0 && b1-end > b0 && d.a[a1-end-1] == d.b[b1-end-1] {
		end++
	}
	a1, b1 = a1-end, b1-end

	switch {
	case a0 == a1:
		d.emit('+', a0, b0, b1-b0)
	case b0 == b1:
		d.emit('-', a0, b0, a1-a0)
	default:
		s := d.middleSnake(a0, a1, b0, b1)
		d.diff(a0, s.x, b0, s.y)
		d.emit(' ', s.x, s.y, s.u-s.x)
		d.diff(s.u, a1, s.v, b1)
	}
	d.emit(' ', a1, b1, end)
}

// snake is a run of equal lines from (x,y) to (u,v). It can be empty.
type snake struct{ x, y, u, v int }

// middleSnake finds the middle run of equal lines on a shortest edit path
// from (a0,b0) to (a1,b1).
//
// A forward search from the start and a reverse search from the end each
// extend by one edit in turn. They meet in the middle of a shortest path.
// The reverse search runs the forward recurrence on the reversed lines, so
// rev holds distances from the end.
func (d *differ) middleSnake(a0, a1, b0, b1 int) snake {
	n, m := a1-a0, b1-b0
	delta := n - m
	odd := delta&1 != 0
	// Diagonal k holds the points with x-y == k. off makes the index
	// non-negative: k stays in [-(n+m+1)/2-1, (n+m+1)/2+1].
	off := (n+m+1)/2 + 1
	fwd, rev := d.fwd, d.rev
	fwd[off+1], rev[off+1] = 0, 0
	for dist := 0; ; dist++ {
		for k := -dist; k <= dist; k += 2 {
			var px int
			if k == -dist || (k != dist && fwd[off+k-1] < fwd[off+k+1]) {
				px = fwd[off+k+1]
			} else {
				px = fwd[off+k-1] + 1
			}
			py := px - k
			sx, sy := px, py
			for px < n && py < m && d.a[a0+px] == d.b[b0+py] {
				px++
				py++
			}
			fwd[off+k] = px
			// The reverse search sees this diagonal as delta-k.
			if rk := delta - k; odd && rk >= -(dist-1) && rk <= dist-1 && px+rev[off+rk] >= n {
				return snake{a0 + sx, b0 + sy, a0 + px, b0 + py}
			}
		}
		for k := -dist; k <= dist; k += 2 {
			var px int
			if k == -dist || (k != dist && rev[off+k-1] < rev[off+k+1]) {
				px = rev[off+k+1]
			} else {
				px = rev[off+k-1] + 1
			}
			py := px - k
			sx, sy := px, py
			for px < n && py < m && d.a[a1-1-px] == d.b[b1-1-py] {
				px++
				py++
			}
			rev[off+k] = px
			if fk := delta - k; !odd && fk >= -dist && fk <= dist && px+fwd[off+fk] >= n {
				return snake{a1 - px, b1 - py, a1 - sx, b1 - sy}
			}
		}
	}
}
