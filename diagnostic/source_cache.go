// Copyright © 2026 The ELPS authors

package diagnostic

import (
	"bufio"
	"bytes"
	"os"
)

// sourceCache holds the files that one Render or RenderAll reads, so that
// each file is read once and each line is found by index. Without it every
// span read its whole file again and scanned from line 1
// (luthersystems/elps#981).
type sourceCache map[string]*sourceFile

type sourceFile struct {
	data []byte
	// starts holds the offset of each line's first byte.
	starts []int
	// scan is true when a line is too long for the index to match
	// bufio.Scanner's result; line then scans data as readSourceLine does.
	scan bool
	ok   bool
}

// maxIndexedLine is the longest line the index serves. bufio.Scanner fails
// on a token near bufio.MaxScanTokenSize, so a file with a longer line keeps
// the scanner's exact result.
const maxIndexedLine = bufio.MaxScanTokenSize / 2

// line returns what readSourceLine returns for file and line.
func (c sourceCache) line(r *Renderer, file string, line int) string {
	if line <= 0 || file == "" || file == "<native code>" {
		return ""
	}
	f, seen := c[file]
	if !seen {
		f = loadSourceFile(r, file)
		c[file] = f
	}
	if !f.ok {
		return ""
	}
	if f.scan {
		return scanSourceLine(f.data, line)
	}
	if line > len(f.starts) {
		return ""
	}
	start := f.starts[line-1]
	end := len(f.data)
	if line < len(f.starts) {
		end = f.starts[line] - 1
	} else if start == end {
		// The text ends with a newline: the scanner yields no empty last line.
		return ""
	}
	// bufio.ScanLines drops one carriage return before the newline.
	return string(bytes.TrimSuffix(f.data[start:end], []byte{'\r'}))
}

func loadSourceFile(r *Renderer, file string) *sourceFile {
	data, err := r.readSource(file)
	if err != nil {
		return &sourceFile{}
	}
	f := &sourceFile{data: data, ok: true, starts: []int{0}}
	for off := 0; ; {
		i := bytes.IndexByte(data[off:], '\n')
		if i < 0 {
			if len(data)-off > maxIndexedLine {
				f.scan = true
			}
			break
		}
		if i+1 > maxIndexedLine {
			f.scan = true
		}
		off += i + 1
		f.starts = append(f.starts, off)
	}
	return f
}

// readSource reads file with SourceReader, or with os.ReadFile when it is
// nil.
func (r *Renderer) readSource(file string) ([]byte, error) {
	if r.SourceReader != nil {
		return r.SourceReader(file)
	}
	return os.ReadFile(file) //nolint:gosec // reads user-specified source files for display
}

func scanSourceLine(data []byte, line int) string {
	scanner := bufio.NewScanner(bytes.NewReader(data))
	for i := 1; scanner.Scan(); i++ {
		if i == line {
			return scanner.Text()
		}
	}
	return ""
}
