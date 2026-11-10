// Copyright © 2026 The ELPS authors

package lsp

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// BenchmarkReferencesClosedDefinitionFile measures textDocument/references
// for a symbol whose definition file is not open in the editor
// (luthersystems/elps#944).
//
// The request must find the export form of the symbol in that file. The
// workspace reference index holds it, so the request reads no file. A
// request that reads and analyzes the definition file costs time in
// proportion to the size of that file, on each request.
func BenchmarkReferencesClosedDefinitionFile(b *testing.B) {
	for _, defs := range []int{50, 500} {
		b.Run(fmt.Sprintf("defs=%d", defs), func(b *testing.B) {
			dir := b.TempDir()
			var lib strings.Builder
			lib.WriteString("(in-package 'helpers)\n(export 'target)\n(defun target (x) (+ x 1))\n")
			for i := range defs {
				fmt.Fprintf(&lib, "(defun fn-%d (a b) (if (> a b) (target a) (+ a b %d)))\n", i, i)
			}
			consumer := "(in-package 'consumer)\n(defun do-work () (helpers:target 42))\n"
			consumerPath := filepath.Join(dir, "consumer.lisp")
			for path, src := range map[string]string{
				filepath.Join(dir, "helpers.lisp"): lib.String(),
				consumerPath:                       consumer,
			} {
				if err := os.WriteFile(path, []byte(src), 0o600); err != nil {
					b.Fatal(err)
				}
			}
			s := testServer()
			s.rootPath = dir
			s.buildWorkspaceIndex()
			uri := pathToURI(consumerPath)
			s.ensureAnalysis(openDoc(s, uri, consumer))
			params := &protocol.ReferenceParams{
				TextDocumentPositionParams: protocol.TextDocumentPositionParams{
					TextDocument: protocol.TextDocumentIdentifier{URI: uri},
					Position:     protocol.Position{Line: 1, Character: 28},
				},
				Context: protocol.ReferenceContext{IncludeDeclaration: true},
			}
			ctx := mockContext()
			// The declaration, the export, the call in the consumer and
			// one call for each fn-N.
			want := 3 + defs
			b.ReportAllocs()
			b.ResetTimer()
			for b.Loop() {
				locs, err := s.textDocumentReferences(ctx, params)
				if err != nil || len(locs) != want {
					b.Fatalf("got %d locations (err %v), want %d", len(locs), err, want)
				}
			}
		})
	}
}
