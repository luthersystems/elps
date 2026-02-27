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

	// Embedder-provided environment and registry.
	registry *lisp.PackageRegistry
	env      *lisp.LEnv

	// Debouncer for didChange notifications.
	debounceMu sync.Mutex
	debounce   map[string]*time.Timer

	// Context for sending notifications (captured from latest request).
	notifyMu sync.Mutex
	notify   glsp.NotifyFunc

	// exitFn is called on the LSP exit notification. Defaults to os.Exit.
	// Overridable for testing.
	exitFn func(int)
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
		Initialize: s.initialize,
		Shutdown:   s.shutdown,
		Exit:       s.exit,
		SetTrace:   s.setTrace,

		TextDocumentDidOpen:   s.textDocumentDidOpen,
		TextDocumentDidChange: s.textDocumentDidChange,
		TextDocumentDidSave:   s.textDocumentDidSave,
		TextDocumentDidClose:  s.textDocumentDidClose,

		TextDocumentHover:          s.textDocumentHover,
		TextDocumentDefinition:     s.textDocumentDefinition,
		TextDocumentCompletion:     s.textDocumentCompletion,
		TextDocumentReferences:     s.textDocumentReferences,
		TextDocumentDocumentSymbol: s.textDocumentDocumentSymbol,
		TextDocumentRename:         s.textDocumentRename,
		TextDocumentPrepareRename:  s.textDocumentPrepareRename,
		TextDocumentFormatting:     s.textDocumentFormatting,
		TextDocumentSignatureHelp:  s.textDocumentSignatureHelp,
		TextDocumentCodeAction:     s.textDocumentCodeAction,
	}

	s.glspSrv = glspserver.NewServer(&s.handler, serverName, false)
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

	version := "0.1.0"
	return protocol.InitializeResult{
		Capabilities: capabilities,
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

	// Scan workspace files in a single pass (only if we have a root path).
	if s.rootPath != "" {
		if globals, pkgs, err := analysis.ScanWorkspaceFull(s.rootPath); err == nil {
			extraGlobals = globals
			pkgExports = pkgs
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
	for pkg, syms := range pkgExports {
		pkgExports[pkg] = deduplicateExports(syms)
	}

	cfg := &analysis.Config{
		ExtraGlobals:   extraGlobals,
		PackageExports: pkgExports,
	}

	s.analysisCfgMu.Lock()
	s.analysisCfg = cfg
	s.analysisCfgMu.Unlock()
}

// reanalyzeOpenDocuments invalidates cached analysis for all open documents
// and re-publishes diagnostics with the current workspace config.
func (s *Server) reanalyzeOpenDocuments() {
	for _, doc := range s.docs.All() {
		doc.mu.Lock()
		doc.analysis = nil
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

func boolPtr(b bool) *bool {
	return &b
}
