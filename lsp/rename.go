// Copyright © 2024 The ELPS authors

package lsp

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// textDocumentPrepareRename validates that the symbol under the cursor
// is renameable and returns its range.
func (s *Server) textDocumentPrepareRename(_ *glsp.Context, params *protocol.PrepareRenameParams) (any, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, nil // no document — rename not applicable
	}
	s.ensureAnalysis(doc)

	// elps#464: the client counts Character in the negotiated encoding
	// (UTF-16 unless it asked for utf-8); everything below counts bytes.
	line, col := s.cursorAt(doc, params.Position)

	sym, ref := symbolAtPosition(doc, line, col)
	if sym == nil {
		return nil, nil // no symbol at position — rename not applicable
	}

	// Reject renaming builtins and special ops.
	// Per LSP spec, prepareRename returns null (not error) for non-renameable symbols.
	if sym.Kind == analysis.SymBuiltin || sym.Kind == analysis.SymSpecialOp {
		return nil, nil
	}
	// Reject external symbols unless they have a real source location
	// (workspace-defined symbols imported via use-package or ExtraGlobals).
	if sym.External && (sym.Source == nil || sym.Source.Pos < 0) {
		return nil, nil
	}

	// Determine the range to highlight.
	var loc = sym.Source
	if ref != nil && ref.Source != nil {
		loc = ref.Source
	}
	if loc == nil {
		return nil, nil
	}

	// elps#464: elpsToLSPRange returns BYTE columns; the client reads them in
	// the negotiated encoding. prepareRename's range is what the editor
	// pre-selects in its rename box, and checkRenameSpans in the fuzz harness
	// uses it as the oracle for the edit ranges below, so it has to move in
	// the same unit as they do.
	return &protocol.RangeWithPlaceholder{
		Range:       s.wireRange(doc.Content, elpsToLSPRange(loc, len(sym.Name))),
		Placeholder: sym.Name,
	}, nil
}

// textDocumentRename handles the textDocument/rename request.
func (s *Server) textDocumentRename(_ *glsp.Context, params *protocol.RenameParams) (*protocol.WorkspaceEdit, error) {
	doc := s.docs.Get(params.TextDocument.URI)
	if doc == nil {
		return nil, errors.New("document not found")
	}
	s.ensureAnalysis(doc)

	// elps#464: the client counts Character in the negotiated encoding
	// (UTF-16 unless it asked for utf-8); everything below counts bytes.
	line, col := s.cursorAt(doc, params.Position)

	sym, _ := symbolAtPosition(doc, line, col)
	if sym == nil {
		return nil, errors.New("no symbol at position")
	}

	if sym.Kind == analysis.SymBuiltin || sym.Kind == analysis.SymSpecialOp {
		return nil, fmt.Errorf("cannot rename %s: %s", symbolKindLabel(sym.Kind), sym.Name)
	}
	// Reject external symbols that lack a real source location (stdlib builtins).
	// Workspace-defined external symbols (with real source) can be renamed.
	if sym.External && (sym.Source == nil || sym.Source.Pos < 0) {
		return nil, fmt.Errorf("cannot rename external symbol: %s", sym.Name)
	}

	edits := make(map[protocol.DocumentUri][]protocol.TextEdit)
	docURI := params.TextDocument.URI

	// elps#464: elpsToLSPRange produces BYTE columns and the client applies
	// these edits in the negotiated encoding, so every range below is
	// converted against the text of the file it points into -- both ends
	// together, which is the invariant elps#470 restored and the reason a
	// rename edit is the one outbound range this PR converts: it is applied
	// to the user's file unread.
	texts := s.newDocumentTexts()

	// Rename at the definition site.
	if sym.Source != nil && sym.Source.Pos >= 0 && sym.Source.Line > 0 {
		defURI := s.resolveURI(docURI, sym.Source.File)
		edits[defURI] = append(edits[defURI], protocol.TextEdit{
			Range:   texts.rangeFor(defURI, elpsToLSPRange(sym.Source, len(sym.Name))),
			NewText: params.NewName,
		})
	}

	// Rename at all reference sites in the current file.
	if doc.analysis != nil {
		for _, ref := range doc.analysis.References {
			if ref.Symbol != sym || ref.Source == nil {
				continue
			}
			refURI := s.resolveURI(docURI, ref.Source.File)
			edits[refURI] = append(edits[refURI], protocol.TextEdit{
				Range:   texts.rangeFor(refURI, elpsToLSPRange(ref.Source, len(sym.Name))),
				NewText: params.NewName,
			})
		}
	}

	// Cross-file rename edits from workspace index.
	key := symbolToKey(sym)
	currentFile := uriToPath(docURI)
	for _, wref := range s.getWorkspaceRefs(key, currentFile) {
		refURI := s.resolveURI(docURI, wref.File)
		edits[refURI] = append(edits[refURI], protocol.TextEdit{
			Range:   texts.rangeFor(refURI, elpsToLSPRange(wref.Source, len(sym.Name))),
			NewText: params.NewName,
		})
	}

	return &protocol.WorkspaceEdit{Changes: edits}, nil
}
