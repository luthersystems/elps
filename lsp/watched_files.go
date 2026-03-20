// Copyright © 2024 The ELPS authors

package lsp

import (
	"strings"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/tliron/glsp"
	protocol "github.com/tliron/glsp/protocol_3_16"
)

// initialized handles the LSP initialized notification. It registers a
// file watcher for .lisp files and triggers the initial workspace index.
func (s *Server) initialized(ctx *glsp.Context, _ *protocol.InitializedParams) error {
	// Store call function for dynamic capability registration.
	s.callMu.Lock()
	s.callFn = ctx.Call
	s.callMu.Unlock()

	// Register a file watcher for .lisp files via dynamic capability.
	// Done in a goroutine because ctx.Call is synchronous and waits for
	// the client's response — this must not block the initialized handler.
	callFn := ctx.Call
	if callFn != nil {
		go func() {
			defer func() { _ = recover() }() // don't crash if the client doesn't respond
			callFn(protocol.ServerClientRegisterCapability, protocol.RegistrationParams{
				Registrations: []protocol.Registration{
					{
						ID:     "elps-watcher",
						Method: string(protocol.MethodWorkspaceDidChangeWatchedFiles),
						RegisterOptions: protocol.DidChangeWatchedFilesRegistrationOptions{
							Watchers: []protocol.FileSystemWatcher{
								{GlobPattern: "**/*.lisp"},
							},
						},
					},
				},
			}, nil)
		}()
	}

	// Trigger background workspace index.
	go s.ensureWorkspaceIndex()

	return nil
}

// workspaceDidChangeWatchedFiles handles workspace/didChangeWatchedFiles.
// It updates the workspace reference index when .lisp files are created,
// changed, or deleted externally.
func (s *Server) workspaceDidChangeWatchedFiles(_ *glsp.Context, params *protocol.DidChangeWatchedFilesParams) error {
	for _, event := range params.Changes {
		uri := string(event.URI)

		// Only process .lisp files.
		if !strings.HasSuffix(uri, ".lisp") {
			continue
		}

		switch event.Type {
		case protocol.FileChangeTypeCreated, protocol.FileChangeTypeChanged:
			s.updateFileDefinitions(uri)
			s.updateFileRefs(uri)
		case protocol.FileChangeTypeDeleted:
			filePath := uriToPath(uri)
			s.removeFileDefinitions(filePath)
			s.removeFileRefs(filePath)
		}
	}

	// Debounce re-analysis of open documents.
	s.debouncedReanalyze("__watched_files__", 500*time.Millisecond)

	return nil
}

// removeFileRefs removes all workspace reference entries for the given
// file path.
func (s *Server) removeFileRefs(filePath string) {
	s.workspaceRefsMu.Lock()
	defer s.workspaceRefsMu.Unlock()

	for key, refs := range s.workspaceRefs {
		var kept []analysis.FileReference
		for _, ref := range refs {
			if ref.File != filePath {
				kept = append(kept, ref)
			}
		}
		if len(kept) == 0 {
			delete(s.workspaceRefs, key)
		} else {
			s.workspaceRefs[key] = kept
		}
	}
}

// debouncedReanalyze debounces a call to reanalyzeOpenDocuments using the
// existing debounce map and the given key/duration.
func (s *Server) debouncedReanalyze(key string, delay time.Duration) {
	s.debounceMu.Lock()
	defer s.debounceMu.Unlock()

	if t, ok := s.debounce[key]; ok {
		t.Stop()
	}
	s.debounce[key] = time.AfterFunc(delay, func() {
		s.reanalyzeOpenDocuments()
	})
}
