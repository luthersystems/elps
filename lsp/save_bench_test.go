// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// BenchmarkSaveDefinitions measures the synchronous part of a didSave:
// updateFileDefinitions for one file of a workspace with many globals
// (luthersystems/elps#944). The editor waits for it before it gets the
// diagnostics of the saved file.
//
// The cost must follow the size of the saved file. A step that does work
// for each global of the workspace (a file system call, for example)
// makes each save slower as the workspace grows.
func BenchmarkSaveDefinitions(b *testing.B) {
	const defsPerFile = 40
	for _, files := range []int{20, 200} {
		b.Run(fmt.Sprintf("globals=%d", files*defsPerFile), func(b *testing.B) {
			dir := b.TempDir()
			var saved string
			for f := range files {
				var src strings.Builder
				fmt.Fprintf(&src, "(in-package 'pkg-%d)\n", f%10)
				for d := range defsPerFile {
					fmt.Fprintf(&src, "(export 'fn-%d-%d)\n(defun fn-%d-%d (a b) (+ a b %d))\n", f, d, f, d, d)
				}
				// Sub-directories, as in a real workspace: the path of
				// each file has several components.
				path := filepath.Join(dir, fmt.Sprintf("mod-%d", f%7), "src", fmt.Sprintf("file-%d.lisp", f))
				if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
					b.Fatal(err)
				}
				if err := os.WriteFile(path, []byte(src.String()), 0o600); err != nil {
					b.Fatal(err)
				}
				saved = path
			}
			s := testServer()
			s.rootPath = dir
			s.buildWorkspaceIndex()
			if got := len(s.analysisCfg.ExtraGlobals); got != files*defsPerFile {
				b.Fatalf("index has %d globals, want %d", got, files*defsPerFile)
			}
			uri := pathToURI(saved)
			b.ReportAllocs()
			b.ResetTimer()
			for b.Loop() {
				s.updateFileDefinitions(uri)
			}
			b.StopTimer()
			if got := len(s.analysisCfg.ExtraGlobals); got != files*defsPerFile {
				b.Fatalf("after the saves the index has %d globals, want %d", got, files*defsPerFile)
			}
		})
	}
}
