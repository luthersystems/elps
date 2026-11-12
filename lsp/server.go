// Copyright © 2024 The ELPS authors

// Package lsp implements a Language Server Protocol server for ELPS.
// It provides diagnostics, hover, go-to-definition, references,
// completion, document symbols, and rename support.
package lsp

import (
	"context"
	"errors"
	"io"
	"os"
	"reflect"
	"slices"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/token"
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
	// wsScan is the scan of each workspace file that the tables of
	// wsScanCfg were merged from, and stdlibExports the registry exports
	// merged with them. A save replaces the scan of one file and merges
	// again (see storeWorkspaceScan). All three are guarded by
	// analysisCfgMu. wsScanCfg differs from analysisCfg when the config
	// was set without a scan; the scan does not describe that config.
	wsScan        *analysis.WorkspaceScan
	wsScanCfg     *analysis.Config
	stdlibExports map[string][]analysis.ExternalSymbol
	indexOnce     sync.Once
	// macroExpander is the expander of the analysis config, with the
	// macros of the workspace loaded in s.env. Guarded by analysisCfgMu.
	// macroReloadMu serializes the saves that swap the workspace scan and
	// reload the macros of the saved file, so the env takes the reloads
	// in the order of the scans (see reloadFileMacros). It is taken
	// before analysisCfgMu.
	macroExpander *analysis.EnvMacroExpander
	macroReloadMu sync.Mutex
	// globalIndex is the GlobalIndex of globalIndexOf, the ExtraGlobals
	// of the last config an analysis ran with (see withGlobalIndex).
	// Both are guarded by globalIndexMu.
	globalIndex   *analysis.GlobalIndex
	globalIndexOf []analysis.ExternalSymbol
	globalIndexMu sync.Mutex
	// macroBase maps the name of each file whose macros in the env do not
	// follow the stored scan to the scan they do follow, or to nil when
	// none of them loaded. A reload that stopped at a limit keeps the old
	// macros, and the index build skips the files after its deadline. The
	// next save compares the file with this scan, not with the stored
	// one, so it loads the file again even when the text is the same.
	// Guarded by macroReloadMu.
	macroBase map[string]*analysis.WorkspaceScan
	// macroBuildTimeout and macroReloadTimeout bound the loads of the
	// preamble (see reloadFileMacros). Tests shorten them.
	macroBuildTimeout  time.Duration
	macroReloadTimeout time.Duration

	// posEncoding holds the positionEncoding negotiated during initialize
	// (elps#464). Zero -- the value before any handshake -- is encodingUTF16,
	// which is what LSP specifies and what a client that negotiates nothing
	// is owed. Written once from initialize and read from every request
	// handler, hence atomic.
	posEncoding atomic.Int32

	// Linter instance shared across diagnostics runs.
	linter *lint.Linter

	// Workspace reference index: maps SymbolKey.String() to cross-file references.
	workspaceRefs map[string][]analysis.FileReference
	// workspaceRefsBuilt reports that workspaceRefs holds the result of
	// a scan of the whole workspace. A save alone adds the entries of
	// one file.
	workspaceRefsBuilt bool
	workspaceRefsMu    sync.RWMutex

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

	// maxDocumentBytes is the maximum document size (in bytes) the server
	// will parse, analyze or format. Documents exceeding this limit are
	// stored unparsed and receive an informational diagnostic instead of
	// full analysis. 0 means no limit.
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

// WithMaxDocumentBytes sets the maximum document size the server will parse,
// analyze or format. Documents exceeding this limit are not parsed at all --
// so AST-based features (hover, definition, completion, symbols, folding,
// semantic tokens, selection ranges, highlights, call hierarchy) return
// nothing for them and formatting is a no-op -- and receive an informational
// diagnostic instead of analysis. The limit has to cover parsing to bound
// anything: the AST costs far more than the source text and is rebuilt on
// every edit (see Document.parse).
//
// The limit applies to document text the client sends; the didSave path, which
// re-reads the saved file from disk, and workspace scanning are both bounded by
// analysis.ScanConfig.MaxFileBytes instead.
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

		macroBuildTimeout:  analysis.PreambleLoadTimeout,
		macroReloadTimeout: defaultMacroReloadTimeout,
	}
	for _, o := range opts {
		o(s)
	}
	// The store parses on Open and Change, so it needs the limit too; an
	// over-limit document must be cheap from the moment it arrives, not only
	// once analyzeAndPublish looks at it.
	s.docs.maxBytes = s.maxDocumentBytes

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

	// Negotiate the position encoding before anything else, because every
	// column this server sends or receives for the rest of the session is
	// counted in the unit chosen here (elps#464).
	//
	// The client's list arrives in capabilities.general.positionEncodings, an
	// LSP 3.17 field that glsp's protocol_3_16 InitializeParams does not name
	// and encoding/json therefore discards -- so it is read from the raw
	// params glsp keeps on the Context. A client that offers nothing (every
	// 3.16 client) gets UTF-16, which is what LSP specifies.
	encoding := selectPositionEncoding(clientPositionEncodings(ctx.Params))
	s.posEncoding.Store(int32(encoding))

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
	// inlayHintProvider and positionEncoding) that protocol_3_16 doesn't
	// include.
	type extendedCapabilities struct {
		protocol.ServerCapabilities
		InlayHintProvider bool   `json:"inlayHintProvider,omitempty"`
		PositionEncoding  string `json:"positionEncoding,omitempty"`
	}
	type extendedInitResult struct {
		Capabilities extendedCapabilities                 `json:"capabilities"`
		ServerInfo   *protocol.InitializeResultServerInfo `json:"serverInfo,omitempty"`
	}

	// Declare the encoding only when it is utf-8. Omitting the field is how
	// LSP 3.17 spells "utf-16" (it is the documented default for an absent
	// positionEncoding), and a 3.16 client would not understand the field at
	// all -- so the omission is both correct for the new clients and silent
	// for the old ones.
	var encodingName string
	if encoding == encodingUTF8 {
		encodingName = positionEncodingName(encoding)
	}

	return extendedInitResult{
		Capabilities: extendedCapabilities{
			ServerCapabilities: capabilities,
			InlayHintProvider:  true,
			PositionEncoding:   encodingName,
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

	scanCfg := s.scanConfig()

	// Two-phase workspace scan: the scan extracts definitions AND
	// macro-derived DefFormSpecs for cross-file def-like form recognition.
	// The scan of each file is kept, so a save can replace one file and
	// merge the tables again.
	scan := analysis.NewWorkspaceScan(s.rootPath)
	if s.rootPath != "" {
		if scanned, err := analysis.ScanWorkspaceFiles(s.rootPath, scanCfg); err == nil {
			scan = scanned
		}
	}
	prescan := scan.Prescan()
	preamble := prescan.Preamble
	if prescan.Truncated {
		s.sendNotification("window/showMessage", &protocol.ShowMessageParams{
			Type:    protocol.MessageTypeWarning,
			Message: "Workspace file limit reached; some files were not indexed for cross-file analysis.",
		})
	}

	// Extract stdlib exports from the embedder's registry.
	var stdlib map[string][]analysis.ExternalSymbol
	reg := s.registry
	if reg == nil && s.env != nil {
		reg = s.env.Runtime.Registry
	}
	if reg != nil {
		stdlib = analysis.ExtractPackageExports(reg)
	}

	cfg := &analysis.Config{}
	setWorkspaceTables(cfg, prescan, stdlib)

	// Enable macro expansion at analysis time if an environment is available.
	// Load workspace-defined macros into the env so the expander can find
	// them. A save loads the macros of the saved file again
	// (reloadFileMacros). The load is bounded like a reload: each form
	// has a step budget and the whole load a deadline, because every
	// request waits for the build (ensureWorkspaceIndex).
	//
	// After the deadline no later form runs. The files of those forms
	// are named in a warning and load at their next save (macroBase).
	var exp *analysis.EnvMacroExpander
	var skipped []string
	if s.env != nil {
		exp = &analysis.EnvMacroExpander{Env: s.env}
		ctx, cancel := context.WithTimeout(context.Background(), s.macroBuildTimeout)
		load := exp.LoadWorkspaceMacrosContext(ctx, preamble)
		cancel()
		for _, err := range load.Errors {
			s.logWarning(err.Error())
		}
		if load.Unfinished < len(preamble) {
			skipped = scan.PreambleFilesFrom(load.Unfinished)
			s.logWarning("macro load stopped at its deadline; these files load their macros at their next save: " +
				strings.Join(skipped, ", "))
		}
		cfg.MacroExpander = exp
		// A call of a workspace macro, such as (defhelper foo), defines
		// foo only when it expands. Add what the macro calls of each
		// file define to the tables, so the other files resolve those
		// names too (luthersystems/elps#953). A save computes them again
		// (withGenerated).
		if gen, changed := scan.WithGeneratedDefinitions(cfg, nil, s.readGenerated(nil, "")); changed {
			scan = gen
			setWorkspaceTables(cfg, scan.Prescan(), stdlib)
		}
	}

	// Build workspace reference index using the populated config.
	// Set it on the config so per-file analysis (and lint analyzers)
	// can check cross-file references.
	if s.rootPath != "" {
		wsRefs := analysis.ScanWorkspaceRefs(s.rootPath, s.withGlobalIndex(cfg), scanCfg)
		cfg.WorkspaceRefs = wsRefs
		s.workspaceRefsMu.Lock()
		s.workspaceRefs = wsRefs
		s.workspaceRefsBuilt = true
		s.workspaceRefsMu.Unlock()
	}

	s.macroReloadMu.Lock()
	s.macroBase = nil
	for _, name := range skipped {
		s.markMacrosBehind(name, nil)
	}
	s.analysisCfgMu.Lock()
	s.analysisCfg = cfg
	s.wsScan, s.wsScanCfg, s.stdlibExports = scan, cfg, stdlib
	s.macroExpander = exp
	s.analysisCfgMu.Unlock()
	s.macroReloadMu.Unlock()
}

// logWarning sends a warning to the log of the client.
func (s *Server) logWarning(msg string) {
	s.sendNotification("window/logMessage", &protocol.LogMessageParams{
		Type:    protocol.MessageTypeWarning,
		Message: msg,
	})
}

// setWorkspaceTables sets the workspace tables of cfg from the merged
// scan of the workspace files and the exports of the package registry.
// The index build and each save use it, so the tables after a save are
// those of a new build over the same files. It writes no slice of
// stdlib, and the slices of prescan become those of cfg.
func setWorkspaceTables(cfg *analysis.Config, prescan *analysis.WorkspacePrescan, stdlib map[string][]analysis.ExternalSymbol) {
	pkgExports := prescan.PkgExports
	if pkgExports == nil {
		pkgExports = make(map[string][]analysis.ExternalSymbol, len(stdlib))
	}
	for k, v := range stdlib {
		pkgExports[k] = append(pkgExports[k], v...)
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
	analysis.SortDefinitions(prescan.AllDefs)

	cfg.ExtraGlobals = prescan.AllDefs
	cfg.PackageExports = pkgExports
	cfg.PackageSymbols = prescan.PkgAllSymbols
	cfg.DefForms = prescan.DefForms
	cfg.PackageImports = prescan.PackageImports
	cfg.DefaultPackage = prescan.DefaultPackage
	cfg.FilePackages = prescan.FilePackages
	cfg.RecordMethodsByFile = prescan.RecordMethodsByFile
}

// storeWorkspaceScan replaces the workspace scan and stores a Config
// with the tables merged from it. changed is the file whose scan
// differs. It reports whether the record methods of that file changed.
// The caller holds analysisCfgMu, and the server has a scan.
//
// It stores a new Config and new tables, and never writes the old ones.
// A request reads the Config pointer under analysisCfgMu and then reads
// its fields without the lock, and each analysis keeps the tables of the
// Config it started with. A write in place would be a data race with
// those readers.
func (s *Server) storeWorkspaceScan(scan *analysis.WorkspaceScan, changed string) bool {
	next := *s.analysisCfg
	old := next.RecordMethodsByFile
	setWorkspaceTables(&next, scan.Prescan(), s.stdlibExports)
	s.analysisCfg, s.wsScan, s.wsScanCfg = &next, scan, &next
	return !reflect.DeepEqual(old.File(changed), next.RecordMethodsByFile.File(changed))
}

// hasWorkspaceScan reports whether the tables of the analysis config
// were merged from the workspace scan. The caller holds analysisCfgMu.
func (s *Server) hasWorkspaceScan() bool {
	return s.wsScan != nil && s.analysisCfg != nil && s.analysisCfg == s.wsScanCfg
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

	cfg := analysis.ConfigForFile(base, uriToPath(uri))
	cfg.GlobalIndex = s.globalIndexFor(cfg.ExtraGlobals)
	return cfg
}

// withGlobalIndex returns a copy of cfg whose GlobalIndex indexes its
// ExtraGlobals, for an analysis. It never writes cfg: a request may be
// reading it.
func (s *Server) withGlobalIndex(cfg *analysis.Config) *analysis.Config {
	if cfg == nil {
		return nil
	}
	next := *cfg
	next.GlobalIndex = s.globalIndexFor(next.ExtraGlobals)
	return &next
}

// globalIndexFor returns the GlobalIndex of globals, the ExtraGlobals of
// a config. It builds the index on the first analysis that asks for it
// and gives the same one to every later analysis until ExtraGlobals
// changes.
//
// The configs do not carry the index. A save replaces ExtraGlobals, and
// an index stored with the new config would cost each save time and
// memory in proportion to all the globals of the workspace, where the
// rest of the save costs about the size of the saved file
// (BenchmarkSaveDefinitions; luthersystems/elps#944). Built here, a
// save costs nothing for it, and a run of analyses after the save, the
// open documents and the references of the saved file, builds it once.
func (s *Server) globalIndexFor(globals []analysis.ExternalSymbol) *analysis.GlobalIndex {
	if len(globals) == 0 {
		return nil
	}
	s.globalIndexMu.Lock()
	defer s.globalIndexMu.Unlock()
	// A save makes a new ExtraGlobals slice, so the first element tells
	// one slice from another.
	same := len(s.globalIndexOf) == len(globals) && &s.globalIndexOf[0] == &globals[0]
	if !same {
		s.globalIndex = analysis.NewGlobalIndex(globals)
		s.globalIndexOf = globals
	}
	return s.globalIndex
}

// docView is the state of a document that one request works from: the
// text, the AST of that text and the analysis of that AST.
//
// A handler reads these three from the view and not from the Document.
// The timers (the didChange debounce, the record method pass) and the
// didChange handler replace the fields of the Document under Document.mu
// while a request runs. A read without the lock is a data race, and two
// reads can return different values: a nil check followed by a nil
// dereference (luthersystems/elps#943). The view is one consistent copy.
// Its values are never written after they are stored, so the request can
// read them without a lock.
type docView struct {
	content  string
	ast      []*lisp.LVal
	analysis *analysis.Result
}

// ensureAnalysis ensures the document has a current analysis result and
// returns the view of the document that goes with it. It lazily triggers
// the workspace index build if needed (fallback for issue #173 where the
// initialized notification may fail).
//
// The analysis of the view is nil when the document has no AST (it is
// over the size limit).
func (s *Server) ensureAnalysis(doc *Document) docView {
	// Build workspace index before locking doc — buildWorkspaceIndex may
	// call reanalyzeOpenDocuments which also locks documents.
	s.ensureWorkspaceIndex()

	doc.mu.Lock()
	defer doc.mu.Unlock()
	if doc.analysis == nil {
		cfg := s.getAnalysisConfig(doc.URI)
		doc.analyze(cfg)
	}
	return docView{content: doc.Content, ast: doc.ast, analysis: doc.analysis}
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

// crossFileRefs returns the places that name sym in the files other than
// the document docURI: references and export forms.
//
// The entries of an open document come from the text the editor holds
// for it, with the positions of that text. The workspace index follows
// the files on disk, so its positions are wrong for a document with
// changes that are not saved: a rename edit at one of them lands on other
// text. The index gives the entries of the files that are not open.
//
// Without an index (no build yet, or a build that failed) the export
// forms of the file that defines sym are read from that file, so a rename
// does not leave an export of the old name.
func (s *Server) crossFileRefs(sym *analysis.Symbol, docURI protocol.DocumentUri) []analysis.FileReference {
	key := symbolToKey(sym)
	currentFile := uriToPath(docURI)

	open := make(map[string]*Document)
	var openPaths []string
	for _, doc := range s.docs.All() {
		path := uriToPath(doc.URI)
		if doc.URI == docURI || path == currentFile {
			continue
		}
		open[path] = doc
		openPaths = append(openPaths, path)
	}
	slices.Sort(openPaths)

	s.workspaceRefsMu.RLock()
	indexed := s.workspaceRefs[key]
	built := s.workspaceRefsBuilt
	s.workspaceRefsMu.RUnlock()

	var refs []analysis.FileReference
	fromBuffer := make(map[string]bool, len(open))
	for _, path := range openPaths {
		view := s.ensureAnalysis(open[path])
		if view.analysis == nil {
			// No analysis of the buffer (it is over the size limit).
			// The index is all there is for this file.
			continue
		}
		fromBuffer[path] = true
		for _, ref := range analysis.ExtractFileRefs(view.analysis, path) {
			if ref.SymbolKey.String() == key {
				refs = append(refs, ref)
			}
		}
	}
	for i := range indexed {
		if indexed[i].File != currentFile && !fromBuffer[indexed[i].File] {
			refs = append(refs, indexed[i])
		}
	}
	if built || sym.Source == nil || sym.Source.File == "" {
		return refs
	}

	defPath := uriToPath(s.resolveURI(docURI, sym.Source.File))
	if defPath == currentFile || fromBuffer[defPath] {
		return refs
	}
	source, status := s.readWorkspaceFile(defPath)
	if status != workspaceReadOK {
		return refs
	}
	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()
	cfg = s.withGlobalIndex(cfg)
	for _, ref := range analysis.ExtractFileRefs(analysis.AnalyzeFile(source, defPath, cfg), defPath) {
		if ref.Export && ref.SymbolKey.String() == key {
			refs = append(refs, ref)
		}
	}
	return refs
}

// definitionSource returns the location of the definition of sym for an
// edit or a location list. For a symbol that another open document
// defines, it is the location in the text of that document, which can
// differ from the location in the file on disk that sym.Source holds.
func (s *Server) definitionSource(sym *analysis.Symbol, docURI protocol.DocumentUri) *token.Location {
	if !sym.External || sym.Source == nil || sym.Source.File == "" {
		return sym.Source
	}
	defURI := s.resolveURI(docURI, sym.Source.File)
	if defURI == docURI {
		return sym.Source
	}
	doc := s.docs.Get(defURI)
	if doc == nil {
		return sym.Source
	}
	view := s.ensureAnalysis(doc)
	if view.analysis == nil {
		return sym.Source
	}
	key := analysis.SymbolToKey(sym)
	for _, def := range view.analysis.Symbols {
		if def == nil || def.External || def.Source == nil || def.Source.Pos < 0 {
			continue
		}
		if analysis.SymbolToKey(def) == key {
			loc := *def.Source
			loc.File = uriToPath(defURI)
			return &loc
		}
	}
	// The buffer no longer defines the symbol. The definition is then
	// in no text the editor holds.
	return nil
}

// symbolToKey derives the workspace ref lookup key from an analysis symbol.
func symbolToKey(sym *analysis.Symbol) string {
	return analysis.SymbolToKey(sym).String()
}

// scanConfig is the workspace scan configuration built from the server
// options. MaxFileBytes uses the scan's own default (not maxDocumentBytes):
// document analysis limits bound text the client sends, workspace scan limits
// bound files read from disk, and they serve different purposes. Everything
// that reads a workspace file from disk -- the index build and the didSave
// refresh of the same tables -- goes through this one config so they agree.
func (s *Server) scanConfig() *analysis.ScanConfig {
	return &analysis.ScanConfig{
		MaxFiles:    s.maxWorkspaceFiles,
		Excludes:    s.excludePatterns,
		IncludeDirs: s.includeDirs,
	}
}

// workspaceReadStatus is why readWorkspaceFile did not return a file, and it
// is the difference between "the index does not know about this file" and
// "the index must stop claiming to know about this file".
type workspaceReadStatus int

const (
	// workspaceReadOK: the source was read and is under the limit.
	workspaceReadOK workspaceReadStatus = iota
	// workspaceReadUnavailable: missing, a directory, or unreadable. The
	// caller leaves its tables as they were -- this is main's behaviour for
	// every failure, and a transient read error must not empty the index.
	workspaceReadUnavailable
	// workspaceReadOverLimit: the file exists and is over the scan's per-file
	// size limit, so the scan would not have indexed it. The caller PURGES
	// its entries for the file rather than leaving them, because a file that
	// was indexed while small and then saved over the limit would otherwise
	// keep definitions and references it no longer has -- go-to-definition
	// pointing at a symbol the file does not define any more.
	workspaceReadOverLimit
)

// readWorkspaceFile reads a .lisp file from disk for the workspace index,
// under the same per-file size limit the workspace scan applies
// (ScanConfig.MaxFileBytes, default analysis.DefaultMaxFileBytes). The scan
// skips an over-limit file by its Stat size before ever opening it; this does
// the same, then reads through a limit one byte past the bound so a file that
// grows between the Stat and the read is still rejected rather than parsed.
//
// The status tells the caller which of the two non-OK outcomes it has, and
// they are NOT the same: an over-limit file leaves the index exactly as if the
// scan had skipped it, which means purging what an earlier under-limit scan
// put there; a missing or unreadable one leaves the tables alone (elps#611).
func (s *Server) readWorkspaceFile(path string) ([]byte, workspaceReadStatus) {
	var source []byte
	maxBytes := s.scanConfig().EffectiveMaxFileBytes()
	info, err := os.Stat(path)
	if err != nil || info.IsDir() {
		return nil, workspaceReadUnavailable
	}
	if info.Size() > maxBytes {
		return nil, workspaceReadOverLimit
	}
	f, err := os.Open(path) //nolint:gosec // LSP server reads user files
	if err != nil {
		return nil, workspaceReadUnavailable
	}
	defer func() { _ = f.Close() }()
	source, err = io.ReadAll(io.LimitReader(f, maxBytes+1))
	if err != nil {
		return nil, workspaceReadUnavailable
	}
	if int64(len(source)) > maxBytes {
		// It grew past the bound between the Stat and the read.
		return nil, workspaceReadOverLimit
	}
	return source, workspaceReadOK
}

// updateFileRefs re-analyzes a single file and updates the workspace ref index.
func (s *Server) updateFileRefs(uri string) {
	filePath := uriToPath(uri)
	source, status := s.readWorkspaceFile(filePath)
	switch status {
	case workspaceReadOK:
	case workspaceReadOverLimit:
		// The scan would not have indexed this file, so neither may the index
		// keep what an earlier under-limit read put there.
		s.removeFileRefs(filePath)
		return
	default:
		return
	}

	s.analysisCfgMu.RLock()
	cfg := s.analysisCfg
	s.analysisCfgMu.RUnlock()
	// After the unlock: the first analysis after a save builds the index,
	// and a save must not wait for that.
	cfg = s.withGlobalIndex(cfg)

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

	s.replaceFileRefs(filePath, newByKey)
}

// replaceFileRefs replaces the entries of filePath in the workspace
// reference index with added (by symbol key; nil removes the file).
//
// It stores a new map and never writes the old one. The analysis config
// holds the index too (Config.WorkspaceRefs), and a lint pass of an open
// document reads it with no lock while a save updates it on another
// goroutine: a write in place is a data race with that pass. The Config
// that held the old map is replaced with one that holds the new map.
func (s *Server) replaceFileRefs(filePath string, added map[string][]analysis.FileReference) {
	s.workspaceRefsMu.Lock()
	old := s.workspaceRefs
	next := make(map[string][]analysis.FileReference, len(old)+len(added))
	for key, refs := range old {
		first := slices.IndexFunc(refs, func(ref analysis.FileReference) bool { return ref.File == filePath })
		if first < 0 {
			next[key] = refs
			continue
		}
		kept := slices.Clone(refs[:first])
		for _, ref := range refs[first+1:] {
			if ref.File != filePath {
				kept = append(kept, ref)
			}
		}
		if len(kept) > 0 {
			next[key] = kept
		}
	}
	for key, refs := range added {
		have := next[key]
		// The full slice expression gives append a new backing array,
		// so a slice of the old map is not written.
		next[key] = append(have[:len(have):len(have)], refs...)
	}
	s.workspaceRefs = next
	s.workspaceRefsMu.Unlock()

	s.analysisCfgMu.Lock()
	defer s.analysisCfgMu.Unlock()
	cfg := s.analysisCfg
	if cfg == nil || old == nil || !sameMap(cfg.WorkspaceRefs, old) {
		// The config does not share the index (a test set one of them).
		return
	}
	withRefs := *cfg
	withRefs.WorkspaceRefs = next
	if s.wsScanCfg == cfg {
		s.wsScanCfg = &withRefs
	}
	s.analysisCfg = &withRefs
}

// sameMap reports whether a and b are one map.
func sameMap(a, b map[string][]analysis.FileReference) bool {
	return reflect.ValueOf(a).Pointer() == reflect.ValueOf(b).Pointer()
}

// updateFileDefinitions scans a single file again and replaces what the
// file adds to the workspace tables of the analysis config. It reports
// whether the record methods the file adds changed.
func (s *Server) updateFileDefinitions(uri string) bool {
	return s.updateFileIndex(uri).records
}

// indexUpdate is what a save or a removal of one file changed.
type indexUpdate struct {
	// records: the record methods the file adds.
	records bool
	// tables: what the file gives to the workspace tables, the positions
	// of its definitions aside.
	tables bool
	// macros: the preamble of the file, so its macros were loaded again
	// into the env of the macro expander.
	macros bool
}

// updateFileIndex scans a single file again and merges the workspace
// tables of the analysis config again with the new scan of the file. It
// reports what changed (see indexUpdate).
//
// All the tables follow the save, not only ExtraGlobals. The analysis of
// another file resolves a use-package import through PackageExports, so
// an export that a save adds or removes must reach that table, or the
// diagnostics of the other files stay wrong until the server starts again
// (luthersystems/elps#943).
//
// The tables after the save are those of a new index build: the server
// keeps the scan of each file (analysis.WorkspaceScan) and merges them
// with the code of the build. So the order of the entries does not
// depend on the saves before, an import or a definition that the save
// removes goes away, and an entry that lost a tie to an entry of the
// saved file comes back when that entry goes.
//
// The macros that the index build loaded into the env of the macro
// expander follow the save too (luthersystems/elps#949). Without that,
// every analysis expands the macro as it was when the server started:
// a macro whose expansion defines another name leaves the uses of the
// new name unresolved. See reloadFileMacros.
func (s *Server) updateFileIndex(uri string) indexUpdate {
	filePath := analysis.NormalizePath(uriToPath(uri))
	source, status := s.readWorkspaceFile(filePath)
	switch status {
	case workspaceReadOK:
	case workspaceReadOverLimit:
		// As above: purge, do not keep stale definitions for a file the scan
		// would now skip.
		return s.removeFileIndex(filePath)
	default:
		return indexUpdate{}
	}

	s.macroReloadMu.Lock()
	defer s.macroReloadMu.Unlock()
	s.analysisCfgMu.Lock()
	if s.analysisCfg == nil {
		s.analysisCfgMu.Unlock()
		return indexUpdate{}
	}
	if s.hasWorkspaceScan() {
		old := s.wsScan
		scan, changed := old.WithFile(filePath, source)
		name, _ := scan.Path(filePath)
		exp := s.macroExpander
		s.analysisCfgMu.Unlock()
		// The macros load before the tables are stored: what the macro
		// calls define (withGenerated) follows the macros after the
		// save. macroReloadMu keeps every other writer of the scan out
		// meanwhile.
		up := indexUpdate{tables: changed}
		up.macros = s.reloadFileMacros(exp, old, scan, name)
		scan, changed = s.withGenerated(exp, scan, name, source, up.macros)
		up.tables = up.tables || changed
		s.analysisCfgMu.Lock()
		up.records = s.storeWorkspaceScan(scan, name)
		s.analysisCfgMu.Unlock()
		return up
	}
	cfg := s.analysisCfg
	s.analysisCfgMu.Unlock()
	// A config that was set without a scan (a test does it). Its tables
	// are updated entry by entry, with the limits replacePackageSymbols
	// and addPackageImports describe.
	// DefaultPackage and FilePackages are set once, so the scan can read
	// them from a config that a later save replaces.
	return indexUpdate{records: s.replaceFileIndex(filePath, analysis.IndexFile(source, filePath, cfg))}
}

// defaultMacroReloadTimeout bounds the time a save spends loading the
// preamble of one file, as analysis.PreambleLoadTimeout bounds the index
// build. Every expansion waits for a reload.
const defaultMacroReloadTimeout = analysis.PreambleReloadTimeout

// markMacrosBehind records that the macros of the file name in the env
// follow the scan base, not the stored scan (see macroBase). The caller
// holds macroReloadMu.
func (s *Server) markMacrosBehind(name string, base *analysis.WorkspaceScan) {
	if s.macroBase == nil {
		s.macroBase = make(map[string]*analysis.WorkspaceScan)
	}
	s.macroBase[name] = base
}

// reloadFileMacros loads the macros of the file name again into the env
// of exp, when the preamble of the file differs between the scans old
// and next. It reports whether it did.
//
// The caller holds macroReloadMu and not analysisCfgMu. The reload
// evaluates Lisp and waits for the expansions that run, and a request
// that waits for analysisCfgMu meanwhile would stall behind it.
//
// A save whose preamble is the same (most saves: a change inside a
// function body) compares the forms of one file and evaluates nothing,
// so the cost of a save still follows the size of the saved file
// (luthersystems/elps#944).
//
// The reload has a step budget for each form and the deadline
// macroReloadTimeout. A form that stops at a limit leaves the macros of
// the file as they were, and the warning names the form. The file is
// then marked in macroBase, so its next save loads it again even with
// the same text; old is replaced by the scan that the macros of the file
// follow when the file is marked.
//
// The workspace reference index of the other files is not scanned
// again. A reference that only a macro expansion makes is updated when
// its own file is saved, or at the next index build.
func (s *Server) reloadFileMacros(exp *analysis.EnvMacroExpander, old, next *analysis.WorkspaceScan, name string) bool {
	if exp == nil {
		return false
	}
	base, behind := s.macroBase[name]
	if behind {
		old = base
	}
	ctx, cancel := context.WithTimeout(context.Background(), s.macroReloadTimeout)
	defer cancel()
	reloaded, errs := exp.ReloadFileMacros(ctx, old, next, name)
	stopped := false
	for _, err := range errs {
		s.logWarning(err.Error())
		stopped = stopped || errors.Is(err, analysis.ErrPreambleLimit)
	}
	switch {
	case stopped && !behind:
		s.markMacrosBehind(name, old)
	case !stopped && behind:
		delete(s.macroBase, name)
	}
	return reloaded
}

// withGenerated returns scan with the definitions that the macro calls
// of the file name make, or of every file when allFiles, expanded by exp
// (see analysis.WorkspaceScan.WithGeneratedDefinitions). It reports
// whether they changed. A save computes them for the saved file only,
// unless it loaded macros again: then a call in any file can expand to
// other definitions. Without an expander it returns scan.
//
// The caller holds macroReloadMu and not analysisCfgMu: the expansion
// evaluates Lisp.
func (s *Server) withGenerated(exp *analysis.EnvMacroExpander, scan *analysis.WorkspaceScan, name string, source []byte, allFiles bool) (*analysis.WorkspaceScan, bool) {
	if exp == nil || !allFiles && !scan.MayGenerateDefinitions(name) {
		// A file of defun and set forms: no tables to merge for it, so
		// the save costs what it did before (luthersystems/elps#944).
		return scan, false
	}
	cfg := &analysis.Config{MacroExpander: exp}
	setWorkspaceTables(cfg, scan.Prescan(), s.stdlibExportsSnapshot())
	var paths []string
	if !allFiles {
		paths = []string{name}
	}
	return scan.WithGeneratedDefinitions(cfg, paths, s.readGenerated(source, name))
}

// readGenerated returns the read function of WithGeneratedDefinitions:
// the text source for the file name (the text the save scanned), and
// otherwise the file on disk within the size limit of the workspace scan.
func (s *Server) readGenerated(source []byte, name string) func(string) ([]byte, bool) {
	return func(path string) ([]byte, bool) {
		if source != nil && path == name {
			return source, true
		}
		src, status := s.readWorkspaceFile(path)
		return src, status == workspaceReadOK
	}
}

// stdlibExportsSnapshot returns the package exports of the registry that
// the index build read.
func (s *Server) stdlibExportsSnapshot() map[string][]analysis.ExternalSymbol {
	s.analysisCfgMu.Lock()
	defer s.analysisCfgMu.Unlock()
	return s.stdlibExports
}

// removeFileIndex removes what the file at filePath adds to the workspace
// tables of the analysis config. Its results are those of
// updateFileIndex.
func (s *Server) removeFileIndex(filePath string) indexUpdate {
	s.macroReloadMu.Lock()
	defer s.macroReloadMu.Unlock()
	s.analysisCfgMu.Lock()
	if s.analysisCfg == nil {
		s.analysisCfgMu.Unlock()
		return indexUpdate{}
	}
	if s.hasWorkspaceScan() {
		old := s.wsScan
		name, held := old.Path(filePath)
		if !held {
			s.analysisCfgMu.Unlock()
			return indexUpdate{}
		}
		scan, _ := old.WithoutFile(filePath)
		exp := s.macroExpander
		s.analysisCfgMu.Unlock()
		// The macros of a removed file go away, as at the next index build.
		up := indexUpdate{tables: true}
		up.macros = s.reloadFileMacros(exp, old, scan, name)
		if up.macros {
			// The removed file defined macros: what the calls of the other
			// files define can change with them.
			scan, _ = s.withGenerated(exp, scan, "", nil, true)
		}
		s.analysisCfgMu.Lock()
		up.records = s.storeWorkspaceScan(scan, name)
		s.analysisCfgMu.Unlock()
		return up
	}
	s.analysisCfgMu.Unlock()
	return indexUpdate{records: s.replaceFileIndex(analysis.NormalizePath(filePath), &analysis.FileIndex{})}
}

// replaceFileIndex replaces the entries of filePath (a normalized path)
// in the workspace tables of the analysis config with those of idx. It
// reports whether the record methods the file adds changed.
//
// It stores a new Config and new tables, and never writes the old ones.
// A request reads the Config pointer under analysisCfgMu and then reads
// its fields without the lock, and each analysis keeps the tables of the
// Config it started with. A write in place would be a data race with
// those readers.
func (s *Server) replaceFileIndex(filePath string, idx *analysis.FileIndex) bool {
	s.analysisCfgMu.Lock()
	defer s.analysisCfgMu.Unlock()

	if s.analysisCfg == nil {
		return false
	}
	next := *s.analysisCfg
	// One matcher for the three tables: they name the same files.
	from := newSourceFileMatcher(filePath)
	next.ExtraGlobals = append(from.without(next.ExtraGlobals, len(idx.Defs)), idx.Defs...)
	next.PackageExports = replacePackageSymbols(next.PackageExports, from, idx.PkgExports, true)
	next.PackageSymbols = replacePackageSymbols(next.PackageSymbols, from, idx.PkgAllSymbols, false)
	next.PackageImports = addPackageImports(next.PackageImports, idx.PackageImports)
	recordsChanged := setFileRecordMethods(&next, filePath, idx.RecordMethods)
	s.analysisCfg = &next
	return recordsChanged
}

// replacePackageSymbols returns a copy of a per-package table of the
// analysis config (PackageExports or PackageSymbols) in which the entries
// of the file of from are replaced with added. The slices of the packages it
// changes are new; table and its slices are not written.
//
// exports selects the rules of PackageExports, which the index build
// keeps with one entry per name and sorted (see buildWorkspaceIndex):
//
//   - An entry from the package registry (it has no source) wins over a
//     workspace entry of the same name.
//   - Of two workspace entries of the same name, the one whose file
//     sorts last wins. The index build keeps the last one in scan order.
//
// The index build drops the entry that loses, so the table cannot give it
// back when the winner goes away. A name that two files export in one
// package is then missing after one of them stops exporting it, until the
// next index build.
func replacePackageSymbols(table map[string][]analysis.ExternalSymbol, from *sourceFileMatcher, added map[string][]analysis.ExternalSymbol, exports bool) map[string][]analysis.ExternalSymbol {
	next := make(map[string][]analysis.ExternalSymbol, len(table)+len(added))
	for pkg, syms := range table {
		kept := from.without(syms, len(added[pkg]))
		if len(kept) == len(syms) && len(added[pkg]) == 0 {
			// Not changed: share the slice with the old table.
			next[pkg] = syms
			continue
		}
		if len(kept) > 0 || len(added[pkg]) > 0 {
			next[pkg] = kept
		}
	}
	for pkg, syms := range added {
		if len(syms) == 0 {
			continue
		}
		if !exports {
			next[pkg] = append(next[pkg], syms...)
			continue
		}
		// A copy: the slice may be the one of the old table, and the
		// loop and the sort below write it.
		merged := slices.Clone(next[pkg])
		byName := make(map[string]int, len(merged)+len(syms))
		for i := range merged {
			byName[merged[i].Name] = i
		}
		for _, sym := range syms {
			i, dup := byName[sym.Name]
			switch {
			case !dup:
				byName[sym.Name] = len(merged)
				merged = append(merged, sym)
			case merged[i].Source == nil:
				// The registry entry wins.
			case sym.Source != nil && sym.Source.File >= merged[i].Source.File:
				merged[i] = sym
			}
		}
		analysis.SortDefinitions(merged)
		next[pkg] = merged
	}
	return next
}

// addPackageImports returns imports with the use-package imports of one
// file added. It returns imports itself when the file adds none, and a
// copy otherwise.
//
// It only adds. PackageImports does not record which file an import came
// from, so an import that a save removes stays until the next index
// build. A stale import can hide an undefined symbol. A missing import
// would report each name of the package as undefined in the other files.
func addPackageImports(imports, added map[string][]string) map[string][]string {
	var next map[string][]string
	for pkg, names := range added {
		for _, name := range names {
			have := imports[pkg]
			if next != nil {
				have = next[pkg]
			}
			if slices.Contains(have, name) {
				continue
			}
			if next == nil {
				next = make(map[string][]string, len(imports)+1)
				for k, v := range imports {
					next[k] = v
				}
			}
			// The full slice expression gives append a new backing array,
			// so the slice of the old table is not written.
			next[pkg] = append(have[:len(have):len(have)], name)
		}
	}
	if next == nil {
		return imports
	}
	return next
}

// setFileRecordMethods replaces the record methods of a file in cfg, so a
// s:defmethod added, renamed or removed in it reaches the record lint
// checks of the other files. RecordMethodsByFile.With returns a copy: a
// Config copied from this one may be in use. It reports whether the
// methods changed.
func setFileRecordMethods(cfg *analysis.Config, filePath string, methods analysis.RecordMethods) bool {
	old := cfg.RecordMethodsByFile
	if len(methods) == 0 && len(old.File(filePath)) == 0 {
		return false
	}
	if reflect.DeepEqual(old.File(filePath), methods) {
		return false
	}
	cfg.RecordMethodsByFile = old.With(filePath, methods)
	return true
}

// removeFileDefinitions removes the entries of the given file path from
// the workspace tables of the analysis config (see updateFileDefinitions).
// It reports whether the file had record methods.
func (s *Server) removeFileDefinitions(filePath string) bool {
	return s.removeFileIndex(filePath).records
}

// recordReanalyzeDelay is how long a change of a file's record methods
// waits before the open documents that use record types are analyzed
// again. Saves in a row share one pass.
var recordReanalyzeDelay = 500 * time.Millisecond

// recordReanalyzeKey is the debounce key of that pass.
const recordReanalyzeKey = "__record_methods__"

// savedReanalyzeKey is the debounce key of the pass over every open
// document after a save that changed the workspace tables.
const savedReanalyzeKey = "__saved__"

// reanalyzeRecordDocumentsLater schedules reanalyzeRecordDocuments, so
// the record diagnostics of the other open documents follow a change of
// the workspace's s:defmethod methods. except is the URI of the document
// that changed, which its save already analyzed.
func (s *Server) reanalyzeRecordDocumentsLater(except string) {
	s.debounceMu.Lock()
	defer s.debounceMu.Unlock()

	if t, ok := s.debounce[recordReanalyzeKey]; ok {
		t.Stop()
	}
	s.debounce[recordReanalyzeKey] = time.AfterFunc(recordReanalyzeDelay, func() {
		defer func() { _ = recover() }() // don't crash the server on analysis panic
		s.reanalyzeRecordDocuments(except)
	})
}

// reanalyzeRecordDocuments analyzes again, and publishes, each open
// document but except whose text makes a record type: only those read the
// workspace methods (the record lint checks of s:make-record).
func (s *Server) reanalyzeRecordDocuments(except string) {
	for _, doc := range s.docs.All() {
		if doc.URI == except {
			continue
		}
		doc.mu.Lock()
		uses := strings.Contains(doc.Content, "make-record")
		if uses {
			doc.analysis = nil
			// Reset publishedVersion so the version guard allows
			// republishing with the new workspace methods.
			doc.publishedVersion = 0
		}
		doc.mu.Unlock()
		if uses {
			s.analyzeAndPublish(doc)
		}
	}
}

// sourceFileMatcher reports which symbols of the workspace tables come
// from one file.
//
// A symbol names its file as the scan read it, and the save names it as
// the editor sent it. Two spellings of one file (a symlink, a relative
// path) compare equal only as normalized paths, and
// analysis.NormalizePath resolves symlinks with file system calls. A save
// tests every symbol of the workspace. The symbols of one file share one
// spelling, so the matcher normalizes each spelling one time and keeps
// the answer. The cost of a save then follows the number of files, not
// the number of symbols: at 8000 symbols the per-symbol calls were most
// of the time of a save (luthersystems/elps#944).
//
// A matcher lives for one save. A cache kept between saves would also
// keep a spelling whose symlink changed.
type sourceFileMatcher struct {
	file  string          // normalized
	match map[string]bool // spelling of a file -> it is file

	// last and lastIs are the spelling and the answer of the last test.
	// The tables hold the symbols of a file next to each other, so most
	// tests repeat the last one and skip the map.
	last   string
	lastIs bool
	primed bool
}

// newSourceFileMatcher returns a matcher for file, a normalized path.
func newSourceFileMatcher(file string) *sourceFileMatcher {
	return &sourceFileMatcher{file: file, match: make(map[string]bool)}
}

// matches reports whether sym comes from the file. A symbol without a
// source (a registry entry) comes from no file.
func (m *sourceFileMatcher) matches(sym *analysis.ExternalSymbol) bool {
	if sym.Source == nil {
		return false
	}
	spelling := sym.Source.File
	if m.primed && spelling == m.last {
		return m.lastIs
	}
	is, ok := m.match[spelling]
	if !ok {
		is = analysis.NormalizePath(spelling) == m.file
		m.match[spelling] = is
	}
	m.last, m.lastIs, m.primed = spelling, is, true
	return is
}

// without returns the entries of syms that do not come from the file. It
// returns syms itself when all of them stay, so a table that the save
// does not change costs no copy. It never writes syms, and an append to
// the result does not write them either. room is the number of entries
// the caller appends: a new slice has the capacity for them, so the
// append does not copy the table a second time.
func (m *sourceFileMatcher) without(syms []analysis.ExternalSymbol, room int) []analysis.ExternalSymbol {
	first := -1
	for i := range syms {
		if m.matches(&syms[i]) {
			first = i
			break
		}
	}
	if first < 0 {
		return syms[:len(syms):len(syms)]
	}
	kept := make([]analysis.ExternalSymbol, first, len(syms)-1+room)
	copy(kept, syms[:first])
	for i := first + 1; i < len(syms); i++ {
		if !m.matches(&syms[i]) {
			kept = append(kept, syms[i])
		}
	}
	return kept
}

// setTestWorkspaceRefs injects workspace refs directly for unit tests.
func (s *Server) setTestWorkspaceRefs(refs map[string][]analysis.FileReference) {
	s.workspaceRefsMu.Lock()
	s.workspaceRefs = refs
	s.workspaceRefsBuilt = true
	s.workspaceRefsMu.Unlock()
}

func boolPtr(b bool) *bool {
	return &b
}
