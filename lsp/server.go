// Copyright © 2024 The ELPS authors

// Package lsp implements a Language Server Protocol server for ELPS.
// It provides diagnostics, hover, go-to-definition, references,
// completion, document symbols, and rename support.
package lsp

import (
	"os"
	"sync"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lint"
	"github.com/tliron/glsp"
	glspserver "github.com/tliron/glsp/server"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

const serverName = "elps-lsp"

// Server is the ELPS language server.
type Server struct {
	handler  protocol.Handler
	glspSrv  *glspserver.Server
	docs     *DocumentStore
	rootURI  string
	rootPath string

	// Workspace analysis configuration built during initialization.
	analysisCfg   *analysis.Config
	analysisCfgMu sync.RWMutex
	indexOnce     sync.Once

	// Linter instance shared across diagnostics runs.
	linter *lint.Linter

	// Workspace reference index: maps SymbolKey.String() to cross-file references.
	workspaceRefs   map[string][]analysis.FileReference
	workspaceRefsMu sync.RWMutex

	// Embedder-provided environment and registry.
	registry *lisp.PackageRegistry
	env      *lisp.LEnv

	// Debouncer for didChange notifications.
	debounceMu sync.Mutex
	debounce   map[string]*time.Timer

	// Context for sending notifications (captured from latest request).
	notifyMu sync.Mutex
	notify   glsp.NotifyFunc

	// Call function for dynamic capability registration (captured from initialized).
	callMu sync.Mutex
	callFn glsp.CallFunc

	// exitFn is called on the LSP exit notification. Defaults to os.Exit.
	// Overridable for testing.
	exitFn func(int)

	// maxDocumentBytes is the maximum document size (in bytes) for semantic
	// analysis. Documents exceeding this limit receive an informational
	// diagnostic instead of full analysis. 0 means no limit.
	maxDocumentBytes int

	// maxWorkspaceFiles is the maximum number of .lisp files to scan during
	// workspace indexing. 0 means use the default (5000).
	maxWorkspaceFiles int

	// excludePatterns are glob patterns for files to skip during workspace
	// scanning. Matched against full path, base name, and directory components.
	excludePatterns []string

	// includeDirs are directory names that override ShouldSkipDir during
	// workspace scanning. A directory matching any entry will be walked
	// even if it would normally be skipped (e.g. "_examples").
	includeDirs []string
}

// Option configures the LSP server.
type Option func(*Server)

// WithRegistry injects an embedder's package registry for stdlib exports.
func WithRegistry(reg *lisp.PackageRegistry) Option {
	return func(s *Server) { s.registry = reg }
}

// WithEnv injects a fully initialized ELPS environment.
func WithEnv(env *lisp.LEnv) Option {
	return func(s *Server) { s.env = env }
}

// WithMaxDocumentBytes sets the maximum document size for semantic analysis.
// Documents exceeding this limit receive an informational diagnostic instead.
// A value <= 0 disables the limit (default).
func WithMaxDocumentBytes(n int) Option {
	return func(s *Server) { s.maxDocumentBytes = max(n, 0) }
}

// WithMaxWorkspaceFiles sets the maximum number of .lisp files scanned
// during workspace indexing. A value <= 0 uses the default (5000).
func WithMaxWorkspaceFiles(n int) Option {
	return func(s *Server) { s.maxWorkspaceFiles = max(n, 0) }
}

// WithExcludes sets glob patterns for files to skip during workspace scanning.
// Patterns are matched against the full path, base name, and each directory
// component using filepath.Match semantics.
func WithExcludes(patterns []string) Option {
	return func(s *Server) { s.excludePatterns = patterns }
}

// WithIncludes sets directory names that override ShouldSkipDir during
// workspace scanning. Directories matching any entry will be walked even
// if they would normally be skipped (e.g. "_examples").
func WithIncludes(dirs []string) Option {
	return func(s *Server) { s.includeDirs = dirs }
}

// New creates a new ELPS LSP server.
func New(opts ...Option) *Server {
	s := &Server{
		docs:     NewDocumentStore(),
		linter:   &lint.Linter{Analyzers: lint.DefaultAnalyzers()},
		debounce: make(map[string]*time.Timer),
		exitFn:   os.Exit,
	}
	for _, o := range opts {
		o(s)
	}

	s.handler = protocol.Handler{
		Initialize:  s.initialize,
		Initialized: s.initialized,
		Shutdown:    s.shutdown,
		Exit:        s.exit,
		SetTrace:    s.setTrace,

		TextDocumentDidOpen:   s.textDocumentDidOpen,
		TextDocumentDidChange: s.textDocumentDidChange,
		TextDocumentDidSave:   s.textDocumentDidSave,
		TextDocumentDidClose:  s.textDocumentDidClose,

		TextDocumentHover:                s.textDocumentHover,
		TextDocumentDefinition:           s.textDocumentDefinition,
		TextDocumentCompletion:           s.textDocumentCompletion,
		TextDocumentReferences:           s.textDocumentReferences,
		TextDocumentDocumentHighlight:    s.textDocumentDocumentHighlight,
		TextDocumentDocumentSymbol:       s.textDocumentDocumentSymbol,
		TextDocumentRename:               s.textDocumentRename,
		TextDocumentPrepareRename:        s.textDocumentPrepareRename,
		TextDocumentFormatting:           s.textDocumentFormatting,
		TextDocumentSignatureHelp:        s.textDocumentSignatureHelp,
		TextDocumentCodeAction:           s.textDocumentCodeAction,
		TextDocumentFoldingRange:         s.textDocumentFoldingRange,
		TextDocumentSemanticTokensFull:   s.textDocumentSemanticTokensFull,
		TextDocumentSelectionRange:       s.textDocumentSelectionRange,
		TextDocumentLinkedEditingRange:   s.textDocumentLinkedEditingRange,
		TextDocumentPrepareCallHierarchy: s.textDocumentPrepareCallHierarchy,
		CallHierarchyIncomingCalls:       s.callHierarchyIncomingCalls,
		CallHierarchyOutgoingCalls:       s.callHierarchyOutgoingCalls,

		WorkspaceDidChangeWatchedFiles: s.workspaceDidChangeWatchedFiles,
		WorkspaceSymbol:                s.workspaceSymbol,
	}

	s.glspSrv = glspserver.NewServer(&handlerWrapper{
		inner:  &s.handler,
		server: s,
	}, serverName, false)
	return s
}

// RunStdio starts the server using stdio transport.
func (s *Server) RunStdio() error {
	return s.glspSrv.RunStdio()
}

// RunTCP starts the server listening on the given address.
func (s *Server) RunTCP(addr string) error {
	return s.glspSrv.RunTCP(addr)
}

// initialize handles the LSP initialize request.
func (s *Server) initialize(ctx *glsp.Context, params *protocol.InitializeParams) (any, error) {
	s.captureNotify(ctx)

	if params.RootURI != nil {
		s.rootURI = *params.RootURI
		s.rootPath = uriToPath(s.rootURI)
	} else if params.RootPath != nil {
		s.rootPath = *params.RootPath
		s.rootURI = pathToURI(s.rootPath)
	}

	capabilities := s.handler.CreateServerCapabilities()

	// Override text document sync to full.
	syncKind := protocol.TextDocumentSyncKindFull
	capabilities.TextDocumentSync = &protocol.TextDocumentSyncOptions{
		OpenClose: boolPtr(true),
		Change:    &syncKind,
		Save:      &protocol.SaveOptions{IncludeText: boolPtr(false)},
	}

	// Set up completion trigger characters.
	capabilities.CompletionProvider = &protocol.CompletionOptions{
		TriggerCharacters: []string{"(", ":"},
	}

	// Enable prepare rename.
	capabilities.RenameProvider = &protocol.RenameOptions{
		PrepareProvider: boolPtr(true),
	}

	// Set up signature help trigger characters.
	capabilities.SignatureHelpProvider = &protocol.SignatureHelpOptions{
		TriggerCharacters:   []string{" "},
		RetriggerCharacters: []string{" ", ")"},
	}

	// Set up semantic tokens with our legend.
	capabilities.SemanticTokensProvider = &protocol.SemanticTokensOptions{
		Legend: semanticTokenLegend(),
		Full:   true,
	}

	version := "0.1.0"

	// Wrap capabilities to advertise LSP 3.17 features (like
	// inlayHintProvider) that protocol_3_16 doesn't include.
	type extendedCapabilities struct {
		protocol.ServerCapabilities
		InlayHintProvider bool `json:"inlayHintProvider,omitempty"`
	}
	type extendedInitResult struct {
		Capabilities extendedCapabilities                 `json:"capabilities"`
		ServerInfo   *protocol.InitializeResultServerInfo `json:"serverInfo,omitempty"`
	}

	return extendedInitResult{
		Capabilities: extendedCapabilities{
			ServerCapabilities: capabilities,
			InlayHintProvider:  true,
		},
		ServerInfo: &protocol.InitializeResultServerInfo{
			Name:    serverName,
			Version: &version,
		},
	}, nil
}

// shutdown handles the LSP shutdown request.
func (s *Server) shutdown(ctx *glsp.Context) error {
	// Cancel any pending debounce timers.
	s.debounceMu.Lock()
	for _, t := range s.debounce {
		t.Stop()
	}
	s.debounce = make(map[string]*time.Timer)
	s.debounceMu.Unlock()

	return nil
}

// exit handles the LSP exit notification by terminating the process.
// Per the LSP spec, the server should exit with code 0 if shutdown was
// called first, or code 1 otherwise. We always exit with 0 since we
// handle shutdown gracefully.
func (s *Server) exit(_ *glsp.Context) error {
	s.exitFn(0)
	return nil
}

// setTrace handles the $/setTrace notification (required by some clients).
func (s *Server) setTrace(_ *glsp.Context, _ *protocol.SetTraceParams) error {
	return nil
}

// ensureWorkspaceIndex guarantees the workspace index is built at least
// once. It is safe to call from any goroutine. The index is built lazily
// on first demand — typically from the first hover, completion, or other
// request that requires analysis. After building, cached analysis results
// for all open documents are invalidated so they will be re-analyzed with
// the workspace config on next access.
func (s *Server) ensureWorkspaceIndex() {
	s.indexOnce.Do(func() {
		s.buildWorkspaceIndex()
		// Invalidate cached analysis for all open documents so they'll
		// be re-analyzed with the new workspace config on next access.
		for _, doc := range s.docs.All() {
			doc.mu.Lock()
			doc.analysis = nil
			doc.mu.Unlock()
		}
	})
}

// buildWorkspaceIndex scans the workspace root and builds the analysis config.
func (s *Server) buildWorkspaceIndex() {
	defer func() { _ = recover() }() // don't crash the server on scan panic

	var extraGlobals []analysis.ExternalSymbol
	var pkgExports map[string][]analysis.ExternalSymbol
	var defForms []analysis.DefFormSpec
	var packageImports map[string][]string

	// Build scan config from server options. MaxFileBytes uses its own
	// default (not maxDocumentBytes) since document analysis limits and
	// workspace scan limits serve different purposes.
	scanCfg := &analysis.ScanConfig{
		MaxFiles:    s.maxWorkspaceFiles,
		Excludes:    s.excludePatterns,
		IncludeDirs: s.includeDirs,
	}

	// Two-phase workspace scan: prescan extracts definitions AND
	// macro-derived DefFormSpecs for cross-file def-like form recognition.
	if s.rootPath != "" {
		if prescan, err := analysis.PrescanWorkspace(s.rootPath, scanCfg); err == nil {
			extraGlobals = prescan.AllDefs
			pkgExports = prescan.PkgExports
			defForms = prescan.DefForms
			packageImports = prescan.PackageImports
			if prescan.Truncated {
				s.sendNotification("window/showMessage", &protocol.ShowMessageParams{
					Type:    protocol.MessageTypeWarning,
					Message: "Workspace file limit reached; some files were not indexed for cross-file analysis.",
				})
			}
		}
	}

	// Extract stdlib exports from the embedder's registry.
	reg := s.registry
	if reg == nil && s.env != nil {
		reg = s.env.Runtime.Registry
	}
	if reg != nil {
		stdlib := analysis.ExtractPackageExports(reg)
		if pkgExports == nil {
			pkgExports = stdlib
		} else {
			for k, v := range stdlib {
				pkgExports[k] = append(pkgExports[k], v...)
			}
		}
	}

	// Deduplicate package exports by symbol name. Prefer registry entries
	// (richer type info) over workspace entries when both exist.
	// Sort after dedup for deterministic resolution order.
	for pkg, syms := range pkgExports {
		deduped := deduplicateExports(syms)
		analysis.SortDefinitions(deduped)
		pkgExports[pkg] = deduped
	}

	// Sort extraGlobals for deterministic duplicate resolution.
	analysis.SortDefinitions(extraGlobals)

	cfg := &analysis.Config{
		ExtraGlobals:   extraGlobals,
		PackageExports: pkgExports,
		DefForms:       defForms,
		PackageImports: packageImports,
	}

	s.analysisCfgMu.Lock()
	s.analysisCfg = cfg
	s.analysisCfgMu.Unlock()

	// Build workspace reference index using the populated config.
	if s.rootPath != "" {
		wsRefs := analysis.ScanWorkspaceRefs(s.rootPath, cfg, scanCfg)
		s.workspaceRefsMu.Lock()
		s.workspaceRefs = wsRefs
		s.workspaceRefsMu.Unlock()
	}
}

// reanalyzeOpenDocuments invalidates cached analysis for all open documents
// and re-publishes diagnostics with the current workspace config.
func (s *Server) reanalyzeOpenDocuments() {
	for _, doc := range s.docs.All() {
		doc.mu.Lock()
		doc.analysis = nil
		// Reset publishedVersion so the version guard allows republishing
		// with the updated workspace config.
		doc.publishedVersion = 0
		doc.mu.Unlock()
		s.analyzeAndPublish(doc)
	}
}

// deduplicateExports removes duplicate symbols by name from a list of
// exports. The last entry wins — since stdlib (registry) entries are
// appended after workspace entries, they take precedence.
func deduplicateExports(syms []analysis.ExternalSymbol) []analysis.ExternalSymbol {
	seen := make(map[string]int, len(syms))
	var result []analysis.ExternalSymbol
	for _, sym := range syms {
		if idx, ok := seen[sym.Name]; ok {
			// Replace existing entry (last wins = registry preferred).
			result[idx] = sym
		} else {
			seen[sym.Name] = len(result)
			result = append(result, sym)
		}
	}
	return result
}

// getAnalysisConfig returns a copy of the workspace analysis config with
// the filename set for the given document.
func (s *Server) getAnalysisConfig(uri string) *analysis.Config {
	s.analysisCfgMu.RLock()
	base := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	cfg := &analysis.Config{
		Filename: uriToPath(uri),
	}
	if base != nil {
		cfg.ExtraGlobals = base.ExtraGlobals
		cfg.PackageExports = base.PackageExports
		cfg.DefForms = base.DefForms
		cfg.PackageImports = base.PackageImports
	}
	return cfg
}

// ensureAnalysis ensures the document has a current analysis result.
// It lazily triggers the workspace index build if needed (fallback for
// issue #173 where the initialized notification may fail).
func (s *Server) ensureAnalysis(doc *Document) {
	// Build workspace index before locking doc — buildWorkspaceIndex may
	// call reanalyzeOpenDocuments which also locks documents.
	s.ensureWorkspaceIndex()

	doc.mu.Lock()
	defer doc.mu.Unlock()
	if doc.analysis != nil {
		return
	}
	cfg := s.getAnalysisConfig(doc.URI)
	doc.analyze(cfg)
}

// captureNotify stores the notification function from the context for
// async use (e.g., publishing diagnostics after a debounce).
func (s *Server) captureNotify(ctx *glsp.Context) {
	s.notifyMu.Lock()
	s.notify = ctx.Notify
	s.notifyMu.Unlock()
}

// sendNotification sends a notification to the client.
func (s *Server) sendNotification(method string, params any) {
	s.notifyMu.Lock()
	fn := s.notify
	s.notifyMu.Unlock()
	if fn != nil {
		fn(method, params)
	}
}

// getWorkspaceRefs returns cross-file references for the given symbol key,
// excluding references from the specified file to avoid double-counting.
func (s *Server) getWorkspaceRefs(key string, excludeFile string) []analysis.FileReference {
	s.workspaceRefsMu.RLock()
	refs := s.workspaceRefs[key]
	s.workspaceRefsMu.RUnlock()

	if excludeFile == "" {
		return refs
	}

	var filtered []analysis.FileReference
	for _, ref := range refs {
		if ref.File != excludeFile {
			filtered = append(filtered, ref)
		}
	}
	return filtered
}

// symbolToKey derives the workspace ref lookup key from an analysis symbol.
func symbolToKey(sym *analysis.Symbol) string {
	return analysis.SymbolToKey(sym).String()
}

// updateFileRefs re-analyzes a single file and updates the workspace ref index.
func (s *Server) updateFileRefs(uri string) {
	filePath := uriToPath(uri)
	source, err := os.ReadFile(filePath) //nolint:gosec // LSP server reads user files
	if err != nil {
		return
	}

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()

	result := analysis.AnalyzeFile(source, filePath, cfg)
	if result == nil {
		return
	}

	newRefs := analysis.ExtractFileRefs(result, filePath)

	// Build a set of new refs keyed by SymbolKey string.
	newByKey := make(map[string][]analysis.FileReference)
	for i := range newRefs {
		key := newRefs[i].SymbolKey.String()
		newByKey[key] = append(newByKey[key], newRefs[i])
	}

	s.workspaceRefsMu.Lock()
	defer s.workspaceRefsMu.Unlock()

	if s.workspaceRefs == nil {
		s.workspaceRefs = make(map[string][]analysis.FileReference)
	}

	// Remove old entries for this file from all keys.
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

	// Insert new entries.
	for key, refs := range newByKey {
		s.workspaceRefs[key] = append(s.workspaceRefs[key], refs...)
	}
}

// updateFileDefinitions re-extracts top-level definitions from a single file
// and updates ExtraGlobals in the analysis config. Old entries for this file
// are removed and replaced with the new definitions.
func (s *Server) updateFileDefinitions(uri string) {
	filePath := uriToPath(uri)
	source, err := os.ReadFile(filePath) //nolint:gosec // LSP server reads user files
	if err != nil {
		return
	}

	newDefs := analysis.ExtractFileDefinitions(source, filePath)

	s.analysisCfgMu.Lock()
	defer s.analysisCfgMu.Unlock()

	if s.analysisCfg == nil {
		return
	}

	s.analysisCfg.ExtraGlobals = append(filterExtraGlobalsByFile(s.analysisCfg.ExtraGlobals, filePath), newDefs...)
}

// removeFileDefinitions removes all ExtraGlobals entries for the given file path.
func (s *Server) removeFileDefinitions(filePath string) {
	s.analysisCfgMu.Lock()
	defer s.analysisCfgMu.Unlock()

	if s.analysisCfg == nil {
		return
	}

	s.analysisCfg.ExtraGlobals = filterExtraGlobalsByFile(s.analysisCfg.ExtraGlobals, filePath)
}

// filterExtraGlobalsByFile returns a new slice with all entries whose
// Source.File does not match excludeFile. Entries with nil Source are kept.
func filterExtraGlobalsByFile(globals []analysis.ExternalSymbol, excludeFile string) []analysis.ExternalSymbol {
	var kept []analysis.ExternalSymbol
	for _, sym := range globals {
		if sym.Source == nil || sym.Source.File != excludeFile {
			kept = append(kept, sym)
		}
	}
	return kept
}

// setTestWorkspaceRefs injects workspace refs directly for unit tests.
func (s *Server) setTestWorkspaceRefs(refs map[string][]analysis.FileReference) {
	s.workspaceRefsMu.Lock()
	s.workspaceRefs = refs
	s.workspaceRefsMu.Unlock()
}

func boolPtr(b bool) *bool {
	return &b
}
