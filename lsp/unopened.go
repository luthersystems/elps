// Copyright © 2026 The ELPS authors

package lsp

import (
	"path/filepath"
	"slices"
	"strings"

	"github.com/luthersystems/elps/analysis"
)

// requestDoc returns the document a position request names: the open
// document of uri or, when the editor has not opened it, the file on disk.
//
// A client sends position requests for files it has not opened. VS Code
// does when a references or call hierarchy view runs "find references" on
// a result in another file, and a client that runs a request from a
// command line, or an agent, does all the time. Before, each such request
// found no document and returned nothing (references from an unopened
// file: elps#952). The workspace index already reads every file from
// disk, so a request can too.
//
// It reads only a file the workspace scan would index (scannedFile): a
// .lisp file under the workspace root, not in a directory or under a
// pattern the scan skips, inside the per-file size limit (the read of
// readWorkspaceFile). A request about any other file is a request about a
// file the index does not know, and reading it would let a request parse
// a file of any size or anywhere. The document is not stored: it is the
// text on disk now, and the editor never sends a change that would keep a
// stored copy current. So each request reads and analyzes the file again;
// a request is about one file, and the read is bounded by the size limit.
func (s *Server) requestDoc(uri string) *Document {
	if doc := s.docs.Get(uri); doc != nil {
		return doc
	}
	if !strings.HasPrefix(uri, "file:") {
		return nil
	}
	path := uriToPath(uri)
	if !s.scannedFile(path) {
		return nil
	}
	src, status := s.readWorkspaceFile(path)
	if status != workspaceReadOK {
		return nil
	}
	doc := &Document{URI: uri, Content: string(src)}
	doc.parse(s.docs.maxBytes)
	return doc
}

// scannedFile reports whether the workspace scan indexes path, by the
// rules of analysis.ScanWorkspaceFiles: a .lisp file under the workspace
// root, in no directory that analysis.ShouldSkipDir skips (unless an
// include names it) and matching no exclude pattern. With no root (a
// client that opened no folder) there is no workspace, so it is false.
//
// The check is made on the path with its symlinks resolved, and on the
// root resolved the same way, so a link inside the root that points out
// of it is outside. The scan never follows a link out of the root either.
func (s *Server) scannedFile(path string) bool {
	if s.rootPath == "" || filepath.Ext(path) != ".lisp" {
		return false
	}
	root, err := filepath.EvalSymlinks(s.rootPath)
	if err != nil {
		return false
	}
	resolved, err := filepath.EvalSymlinks(path)
	if err != nil || filepath.Ext(resolved) != ".lisp" {
		return false
	}
	rel, err := filepath.Rel(root, resolved)
	if err != nil || rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		return false
	}
	dirs := strings.Split(filepath.Dir(rel), string(filepath.Separator))
	for _, dir := range dirs {
		if dir != "." && analysis.ShouldSkipDir(dir) && !slices.Contains(s.includeDirs, dir) {
			return false
		}
	}
	return !analysis.MatchesExclude(path, s.excludePatterns)
}
