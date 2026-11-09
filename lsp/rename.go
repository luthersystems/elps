// Copyright © 2024 The ELPS authors

package lsp

import (
	"errors"
	"fmt"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/parser/token"
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
		Range:       s.wireRange(doc.Content, nameRange(loc, sym.Name)),
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
	//
	// A conversion that CANNOT be performed fails the whole request. A rename
	// is one edit spanning many files, and there is no such thing as applying
	// most of it: a caller that shipped the ranges it could convert and byte
	// columns for the rest would corrupt the file it could not read while
	// renaming every other one correctly, leaving the user a broken workspace
	// and no diagnostic. See documentTexts.rangeFor for which failures are
	// fatal and which are the status quo.
	texts := s.newDocumentTexts()
	// The export of a symbol is in the result of more than one lookup below
	// when the rename starts in the file that has the definition.  Two edits
	// of one range would write the new name two times.
	type editKey struct {
		uri       protocol.DocumentUri
		line, col int
	}
	seen := make(map[editKey]bool)
	addEdit := func(uri protocol.DocumentUri, loc *token.Location) error {
		k := editKey{uri: uri, line: loc.Line, col: loc.Col}
		if seen[k] {
			return nil
		}
		seen[k] = true
		rng, err := texts.rangeFor(uri, nameRange(loc, sym.Name))
		if err != nil {
			return err
		}
		edits[uri] = append(edits[uri], protocol.TextEdit{Range: rng, NewText: params.NewName})
		return nil
	}

	// Rename at the definition site.
	if sym.Source != nil && sym.Source.Pos >= 0 && sym.Source.Line > 0 {
		if err := addEdit(s.resolveURI(docURI, sym.Source.File), sym.Source); err != nil {
			return nil, err
		}
	}

	// Rename at all reference sites in the current file.
	if doc.analysis != nil {
		for _, ref := range doc.analysis.References {
			if ref.Symbol != sym || ref.Source == nil {
				continue
			}
			if err := addEdit(s.resolveURI(docURI, ref.Source.File), ref.Source); err != nil {
				return nil, err
			}
		}
	}

	// Cross-file rename edits from workspace index.
	key := symbolToKey(sym)
	currentFile := uriToPath(docURI)
	for _, wref := range s.getWorkspaceRefs(key, currentFile) {
		if err := addEdit(s.resolveURI(docURI, wref.File), wref.Source); err != nil {
			return nil, err
		}
	}

	// Rename in the export forms.  An export that kept the old name would
	// export a name with no definition.
	for _, loc := range s.exportLocations(doc, docURI, sym) {
		if err := addEdit(s.resolveURI(docURI, loc.File), loc); err != nil {
			return nil, err
		}
	}

	return &protocol.WorkspaceEdit{Changes: edits}, nil
}

// nameRange is the byte-column range of the name of a symbol at loc, a
// definition or a reference of the symbol name.
//
// The location of a reference written pkg:name covers the qualifier too.  A
// rename replaces only the name: an edit of the whole token would remove the
// qualifier, and the call would resolve in the package of the file.  The
// name is the end of the token, so its range is measured back from the end.
func nameRange(loc *token.Location, name string) protocol.Range {
	rng := elpsToLSPRange(loc, len(name))
	if rng.Start.Line == rng.End.Line && rng.End.Character-rng.Start.Character > safeUint(len(name)) {
		rng.Start.Character = rng.End.Character - safeUint(len(name))
	}
	return rng
}

// exportLocations returns the location of each name in an export form that
// names sym, in the current document doc and in the file that defines sym.
// An export form is almost always in the file that has the definition; one
// in a third file is not found.
//
// A definition file that is not open is read and analysed here.  One that
// cannot be read, or is over the workspace size limit, gives no location:
// the other edits of the rename are still correct, as they were before
// exports got an edit at all.
func (s *Server) exportLocations(doc *Document, docURI protocol.DocumentUri, sym *analysis.Symbol) []*token.Location {
	key := symbolToKey(sym)
	var locs []*token.Location
	collect := func(result *analysis.Result) {
		if result == nil {
			return
		}
		for _, ref := range result.ExportRefs {
			if ref.Source != nil && symbolToKey(ref.Symbol) == key {
				locs = append(locs, ref.Source)
			}
		}
	}
	collect(doc.analysis)

	if sym.Source == nil || sym.Source.File == "" {
		return locs
	}
	defURI := s.resolveURI(docURI, sym.Source.File)
	if defURI == docURI {
		return locs
	}
	if defDoc := s.docs.Get(defURI); defDoc != nil {
		s.ensureAnalysis(defDoc)
		collect(defDoc.analysis)
		return locs
	}
	path := uriToPath(defURI)
	source, status := s.readWorkspaceFile(path)
	if status != workspaceReadOK {
		return locs
	}
	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()
	collect(analysis.AnalyzeFile(source, path, cfg))
	return locs
}
