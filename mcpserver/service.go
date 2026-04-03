package mcpserver

import (
	"context"
	"errors"
	"fmt"
	"hash/fnv"
	"io/fs"
	"log/slog"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/analysis/perf"
	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/lisp/lisplib"
	"github.com/luthersystems/elps/lisp/lisplib/libhelp"
	"github.com/luthersystems/elps/lisp/lisplib/libtesting"
	"github.com/luthersystems/elps/parser"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/modelcontextprotocol/go-sdk/mcp"
)

const builtinScheme = "elps-builtin"

type serviceConfig struct {
	registry        *lisp.PackageRegistry
	env             *lisp.LEnv
	envFactory      func() (*lisp.LEnv, error)
	workspaceRoot   string
	perfConfig      *perf.Config
	excludePatterns []string
	linter          *lint.Linter
	logger          *slog.Logger
}

type service struct {
	registry        *lisp.PackageRegistry
	env             *lisp.LEnv
	envFactory      func() (*lisp.LEnv, error)
	workspaceRoot   string
	perfConfig      *perf.Config
	excludePatterns []string
	linter          *lint.Linter
	logger          *slog.Logger

	mu         sync.RWMutex
	workspaces map[string]*workspaceState

	buildWorkspaceStateHook     func(string)
	workspaceFingerprintHook    func(string)
	workspaceValidationInterval time.Duration
}

type workspaceState struct {
	cfg         *analysis.Config
	symbols     []analysis.ExternalSymbol
	refs        map[string][]analysis.FileReference
	fingerprint string
	validatedAt atomic.Int64 // UnixNano timestamp; atomic to avoid races between readers and markWorkspaceValidated
}

type document struct {
	Path        string
	Content     string
	Analysis    *analysis.Result
	ParseErrors []error
}

func newService(cfg serviceConfig) *service {
	return &service{
		registry:                    cfg.registry,
		env:                         cfg.env,
		envFactory:                  cfg.envFactory,
		workspaceRoot:               cfg.workspaceRoot,
		perfConfig:                  clonePerfConfig(cfg.perfConfig),
		excludePatterns:             cfg.excludePatterns,
		linter:                      cfg.linter,
		logger:                      cfg.logger,
		workspaces:                  make(map[string]*workspaceState),
		workspaceValidationInterval: time.Second,
	}
}

func (s *service) hoverTool(ctx context.Context, _ *mcp.CallToolRequest, in FileQueryInput) (*mcp.CallToolResult, HoverResponse, error) {
	doc, state, err := s.loadDocument(in.Path, in.Content, in.WorkspaceRoot)
	if err != nil {
		return nil, HoverResponse{}, err
	}
	if err := validateCursor(in.Line, in.Character); err != nil {
		return nil, HoverResponse{}, err
	}
	sym, _ := symbolAtPosition(doc, in.Line, in.Character)
	if sym != nil {
		return nil, HoverResponse{
			SymbolName: sym.Name,
			Kind:       symbolKindLabel(sym.Kind),
			Signature:  formatSignature(sym.Signature),
			Doc:        sym.DocString,
			DefinedIn:  symbolLocation(sym, doc.Path, state),
			Markdown:   buildHoverContent(sym),
			Found:      true,
		}, nil
	}

	word := wordAtPosition(doc.Content, in.Line, in.Character)
	if content, ext := qualifiedSymbolHover(state, word); ext != nil {
		return nil, HoverResponse{
			SymbolName: ext.Name,
			Kind:       symbolKindLabel(ext.Kind),
			Signature:  formatSignature(ext.Signature),
			Doc:        ext.DocString,
			DefinedIn:  externalLocation(ext, state),
			Markdown:   content,
			Found:      true,
		}, nil
	}
	return nil, HoverResponse{Found: false}, nil
}

func (s *service) definitionTool(ctx context.Context, _ *mcp.CallToolRequest, in FileQueryInput) (*mcp.CallToolResult, DefinitionResponse, error) {
	doc, state, err := s.loadDocument(in.Path, in.Content, in.WorkspaceRoot)
	if err != nil {
		return nil, DefinitionResponse{}, err
	}
	if err := validateCursor(in.Line, in.Character); err != nil {
		return nil, DefinitionResponse{}, err
	}

	sym, _ := symbolAtPosition(doc, in.Line, in.Character)
	word := wordAtPosition(doc.Content, in.Line, in.Character)

	if sym == nil || sym.Source == nil {
		if sym != nil && isBuiltin(sym) {
			return nil, DefinitionResponse{Found: true, Location: builtinLocationForSymbolWord(sym, word)}, nil
		}
		if loc := qualifiedSymbolDefinition(state, word); loc != nil {
			return nil, DefinitionResponse{Found: true, Location: loc}, nil
		}
		return nil, DefinitionResponse{Found: false}, nil
	}

	if sym.Source.Pos < 0 {
		return nil, DefinitionResponse{Found: true, Location: builtinLocationForSymbolWord(sym, word)}, nil
	}

	loc := locationFromSource(resolvePathAgainstRoot(sym.Source.File, s.rootForState(in.WorkspaceRoot), doc.Path), sym.Source, len(sym.Name))
	return nil, DefinitionResponse{Found: true, Location: loc}, nil
}

func (s *service) referencesTool(ctx context.Context, _ *mcp.CallToolRequest, in ReferencesInput) (*mcp.CallToolResult, ReferencesResponse, error) {
	doc, state, err := s.loadDocument(in.Path, in.Content, in.WorkspaceRoot)
	if err != nil {
		return nil, ReferencesResponse{}, err
	}
	if err := validateCursor(in.Line, in.Character); err != nil {
		return nil, ReferencesResponse{}, err
	}

	sym, _ := symbolAtPosition(doc, in.Line, in.Character)
	if sym == nil || doc.Analysis == nil {
		return nil, ReferencesResponse{References: []Location{}}, nil
	}

	refs := []Location{}
	if in.IncludeDeclaration && sym.Source != nil && sym.Source.Pos >= 0 {
		refs = append(refs, *locationFromSource(resolvePathAgainstRoot(sym.Source.File, s.rootForState(in.WorkspaceRoot), doc.Path), sym.Source, len(sym.Name)))
	}

	for _, ref := range doc.Analysis.References {
		if ref.Symbol != sym || ref.Source == nil {
			continue
		}
		refs = append(refs, *locationFromSource(resolvePathAgainstRoot(ref.Source.File, s.rootForState(in.WorkspaceRoot), doc.Path), ref.Source, len(sym.Name)))
	}

	for _, wref := range getWorkspaceRefs(state, analysis.SymbolToKey(sym).String(), doc.Path) {
		refs = append(refs, *locationFromSource(wref.File, wref.Source, len(sym.Name)))
	}

	resp := ReferencesResponse{
		SymbolName: sym.Name,
		References: refs,
	}
	if in.Offset > 0 || in.Limit > 0 {
		total := len(resp.References)
		resp.References = paginateSlice(resp.References, in.Offset, in.Limit)
		if len(resp.References) < total {
			resp.Truncated = true
			resp.Total = total
		}
	}
	return nil, resp, nil
}

func (s *service) documentSymbolsTool(ctx context.Context, _ *mcp.CallToolRequest, in DocumentQueryInput) (*mcp.CallToolResult, DocumentSymbolsResponse, error) {
	doc, _, err := s.loadDocument(in.Path, in.Content, in.WorkspaceRoot)
	if err != nil {
		return nil, DocumentSymbolsResponse{}, err
	}
	symbols := []DocumentSymbol{}
	if doc.Analysis != nil {
		for _, sym := range doc.Analysis.Symbols {
			if sym.External || sym.Source == nil || sym.Source.Line == 0 {
				continue
			}
			if sym.Source.File != "" && sym.Source.File != doc.Path {
				continue
			}
			if sym.Scope != nil && sym.Scope.Kind != analysis.ScopeGlobal {
				continue
			}
			symbols = append(symbols, DocumentSymbol{
				Name:   sym.Name,
				Kind:   symbolKindLabel(sym.Kind),
				Detail: strings.Trim(formatSignature(sym.Signature), "()"),
				Path:   doc.Path,
				Range:  rangeFromSource(sym.Source, len(sym.Name)),
			})
		}
	}
	sort.Slice(symbols, func(i, j int) bool {
		if symbols[i].Path == symbols[j].Path {
			return compareRange(symbols[i].Range, symbols[j].Range)
		}
		return symbols[i].Path < symbols[j].Path
	})
	resp := DocumentSymbolsResponse{Symbols: symbols}
	if in.Offset > 0 || in.Limit > 0 {
		total := len(resp.Symbols)
		resp.Symbols = paginateSlice(resp.Symbols, in.Offset, in.Limit)
		if len(resp.Symbols) < total {
			resp.Truncated = true
			resp.Total = total
		}
	}
	return nil, resp, nil
}

func (s *service) workspaceSymbolsTool(ctx context.Context, _ *mcp.CallToolRequest, in WorkspaceSymbolsInput) (*mcp.CallToolResult, WorkspaceSymbolsResponse, error) {
	start := time.Now()
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, true)
	if err != nil {
		return nil, WorkspaceSymbolsResponse{}, err
	}
	state, err := s.workspace(root)
	if err != nil {
		return nil, WorkspaceSymbolsResponse{}, err
	}
	query := strings.ToLower(in.Query)
	out := []WorkspaceSymbol{}
	seen := make(map[string]bool)
	for _, sym := range state.symbols {
		if sym.Source == nil || sym.Source.Line == 0 {
			continue
		}
		if !matchesQuery(sym.Name, query) && !matchesQuery(sym.Package+":"+sym.Name, query) {
			continue
		}
		entry := WorkspaceSymbol{
			Name:    sym.Name,
			Kind:    symbolKindLabel(sym.Kind),
			Package: sym.Package,
			Path:    sym.Source.File,
			Range:   rangeFromSource(sym.Source, len(sym.Name)),
		}
		if !seenWorkspaceSymbol(seen, entry) {
			out = append(out, entry)
		}
	}
	sort.Slice(out, func(i, j int) bool {
		if out[i].Path == out[j].Path {
			if out[i].Name == out[j].Name {
				return compareRange(out[i].Range, out[j].Range)
			}
			return out[i].Name < out[j].Name
		}
		return out[i].Path < out[j].Path
	})
	resp := WorkspaceSymbolsResponse{Symbols: out}
	if in.Offset > 0 || in.Limit > 0 {
		total := len(resp.Symbols)
		resp.Symbols = paginateSlice(resp.Symbols, in.Offset, in.Limit)
		if len(resp.Symbols) < total {
			resp.Truncated = true
			resp.Total = total
		}
	}
	resp.Meta = makeMeta(root, time.Since(start), len(state.symbols))
	return nil, resp, nil
}

func (s *service) diagnosticsTool(ctx context.Context, _ *mcp.CallToolRequest, in DiagnosticsInput) (*mcp.CallToolResult, DiagnosticsResponse, error) {
	start := time.Now()
	if in.IncludeWorkspace {
		root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, true)
		if err != nil {
			return nil, DiagnosticsResponse{}, err
		}
		files, err := s.listWorkspaceFiles(root, true)
		if err != nil {
			return nil, DiagnosticsResponse{}, err
		}
		contentByPath := make(map[string]*string)
		var filterPath string
		if in.Path != nil {
			resolved, resolveErr := s.resolvePath(*in.Path, root)
			if resolveErr != nil {
				return nil, DiagnosticsResponse{}, resolveErr
			}
			filterPath = resolved
			if in.Content != nil {
				contentByPath[resolved] = in.Content
			}
		}
		result := []FileDiagnostics{}
		for _, path := range files {
			fd, diagErr := s.collectFileDiagnostics(path, contentByPath[path], &root)
			if diagErr != nil {
				return nil, DiagnosticsResponse{}, diagErr
			}
			if filterPath != "" && path != filterPath {
				continue
			}
			fd.Diagnostics = filterDiagnosticsBySeverity(fd.Diagnostics, in.Severity)
			if in.Severity != nil && len(fd.Diagnostics) == 0 {
				continue
			}
			result = append(result, fd)
		}
		resp := s.applyDiagnosticsLimits(result, in.MaxFiles, in.Offset)
		resp.Meta = makeMeta(root, time.Since(start), len(files))
		return nil, resp, nil
	}

	if in.Path == nil && in.Content == nil {
		return nil, DiagnosticsResponse{}, errors.New("path is required when include_workspace is false and no content is provided")
	}
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	if err != nil {
		return nil, DiagnosticsResponse{}, err
	}
	path := "<stdin>"
	if in.Path != nil {
		path = *in.Path
	}
	fd, err := s.collectFileDiagnostics(path, in.Content, optionalStringPtr(root))
	if err != nil {
		return nil, DiagnosticsResponse{}, err
	}
	fd.Diagnostics = filterDiagnosticsBySeverity(fd.Diagnostics, in.Severity)
	resp := DiagnosticsResponse{
		Files: []FileDiagnostics{fd},
		Meta:  makeMeta(root, time.Since(start), 1),
	}
	return nil, resp, nil
}

func (s *service) perfIssuesTool(ctx context.Context, _ *mcp.CallToolRequest, in PerfSelectionInput) (*mcp.CallToolResult, PerfIssuesResponse, error) {
	start := time.Now()
	result, err := s.runPerf(in)
	if err != nil {
		return nil, PerfIssuesResponse{}, err
	}
	issues := mapIssues(result.Issues)
	out := PerfIssuesResponse{Issues: issues, Solved: []SolvedFunctionSummary{}}
	if in.Top > 0 {
		if len(out.Issues) > in.Top {
			out.TotalIssues = len(out.Issues)
			out.Issues = out.Issues[:in.Top]
			out.Truncated = true
		}
		solved := result.Solved
		if len(in.Rules) > 0 {
			solved = filterSolvedByRules(solved, result.Issues, in.Rules)
		}
		out.Solved = topSolved(solved, in.Top)
	}
	root, _ := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	out.Meta = makeMeta(root, time.Since(start), 0)
	return nil, out, nil
}

func (s *service) callGraphTool(ctx context.Context, _ *mcp.CallToolRequest, in PerfSelectionInput) (*mcp.CallToolResult, CallGraphResponse, error) {
	start := time.Now()
	result, err := s.runPerf(in)
	if err != nil {
		return nil, CallGraphResponse{}, err
	}
	resp := mapCallGraph(result.Graph)
	if in.Top > 0 {
		resp = truncateCallGraph(resp, result.Solved, in.Top)
	}
	root, _ := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	resp.Meta = makeMeta(root, time.Since(start), 0)
	return nil, resp, nil
}

func (s *service) hotspotsTool(ctx context.Context, _ *mcp.CallToolRequest, in PerfSelectionInput) (*mcp.CallToolResult, HotspotsResponse, error) {
	start := time.Now()
	if in.Top <= 0 {
		return nil, HotspotsResponse{}, errors.New("top must be greater than zero")
	}
	result, err := s.runPerf(in)
	if err != nil {
		return nil, HotspotsResponse{}, err
	}
	root, _ := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	return nil, HotspotsResponse{
		Functions: topSolved(result.Solved, in.Top),
		Meta:      makeMeta(root, time.Since(start), 0),
	}, nil
}

func (s *service) runPerf(in PerfSelectionInput) (*perf.Result, error) {
	files, err := s.selectPerfFiles(in)
	if err != nil {
		return nil, err
	}
	cfg, err := s.mergePerfConfig(in)
	if err != nil {
		return nil, err
	}
	// Filter out files that can't be parsed — perf.AnalyzeFiles uses
	// strict parsing and fails on the first broken file.
	validFiles := filterParseableFiles(files, s.logger)
	return perf.AnalyzeFiles(validFiles, cfg)
}

func filterParseableFiles(files []string, logger *slog.Logger) []string {
	valid := make([]string, 0, len(files))
	for _, path := range files {
		data, err := os.ReadFile(path) //nolint:gosec // tool reads user-selected paths
		if err != nil {
			if logger != nil {
				logger.Warn("skipping unreadable file for perf analysis", "path", path, "error", err)
			}
			continue
		}
		scanner := token.NewScanner(path, strings.NewReader(string(data)))
		parser := rdparser.NewFormatting(scanner)
		if _, parseErr := parser.ParseProgram(); parseErr != nil {
			if logger != nil {
				logger.Warn("skipping unparseable file for perf analysis", "path", path, "error", parseErr)
			}
			continue
		}
		valid = append(valid, path)
	}
	return valid
}

func (s *service) mergePerfConfig(in PerfSelectionInput) (*perf.Config, error) {
	cfg := clonePerfConfig(s.perfConfig)
	if len(in.Rules) > 0 {
		for _, rule := range in.Rules {
			if !validPerfRule(rule) {
				return nil, fmt.Errorf("unsupported rule: %s", rule)
			}
		}
		cfg.Rules = append([]string(nil), in.Rules...)
	}
	if in.Config == nil {
		return cfg, nil
	}
	if in.Config.ExpensiveFunctions != nil {
		cfg.ExpensiveFunctions = append([]string(nil), in.Config.ExpensiveFunctions...)
	}
	if in.Config.LoopKeywords != nil {
		cfg.LoopKeywords = append([]string(nil), in.Config.LoopKeywords...)
	}
	if in.Config.FunctionCosts != nil {
		cfg.FunctionCosts = make(map[string]int, len(in.Config.FunctionCosts))
		for k, v := range in.Config.FunctionCosts {
			cfg.FunctionCosts[k] = v
		}
	}
	if in.Config.SuppressionPrefix != "" {
		cfg.SuppressionPrefix = in.Config.SuppressionPrefix
	}
	if in.Config.HotPathThreshold > 0 {
		cfg.MaxScore = in.Config.HotPathThreshold
	}
	if in.Config.ScalingWarningThreshold > 0 {
		cfg.MaxAcceptableOrder = in.Config.ScalingWarningThreshold
	}
	if in.Config.ScalingErrorThreshold > 0 {
		cfg.ScalingErrorThreshold = in.Config.ScalingErrorThreshold
	}
	return cfg, nil
}

func (s *service) selectPerfFiles(in PerfSelectionInput) ([]string, error) {
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, len(in.Paths) == 0)
	if err != nil {
		return nil, err
	}
	if len(in.Paths) > 0 {
		excludes := perfExcludePatterns(s.perfConfig, in.IncludeTests)
		files := make([]string, 0, len(in.Paths))
		for _, path := range in.Paths {
			resolved, resolveErr := s.resolvePath(path, root)
			if resolveErr != nil {
				return nil, resolveErr
			}
			if shouldExcludePerfPath(root, resolved, excludes) {
				continue
			}
			files = append(files, resolved)
		}
		sort.Strings(files)
		return files, nil
	}
	return s.listPerfWorkspaceFiles(root, s.perfConfig, in.IncludeTests)
}

func (s *service) collectFileDiagnostics(path string, content *string, workspaceRoot *string) (FileDiagnostics, error) {
	doc, _, err := s.loadDocument(path, content, workspaceRoot)
	if err != nil {
		return FileDiagnostics{}, err
	}
	diags := make([]Diagnostic, 0, len(doc.ParseErrors))
	for _, parseErr := range doc.ParseErrors {
		diags = append(diags, parseDiagnostic(parseErr, doc.Path))
	}
	lintDiags, err := s.linter.LintFileWithContext([]byte(doc.Content), doc.Path, doc.Analysis)
	if err == nil {
		for _, diag := range lintDiags {
			diags = append(diags, lintDiagnostic(diag))
		}
	}
	sort.Slice(diags, func(i, j int) bool {
		return compareRange(diags[i].Range, diags[j].Range)
	})
	return FileDiagnostics{Path: doc.Path, Diagnostics: diags}, nil
}

func (s *service) loadDocument(path string, content *string, workspaceRoot *string) (*document, *workspaceState, error) {
	if path == "" && content == nil {
		return nil, nil, errors.New("path is required")
	}
	if path == "" {
		path = "<stdin>"
	}
	root, err := s.resolveWorkspaceRoot(workspaceRoot, false)
	if err != nil {
		return nil, nil, err
	}
	resolvedPath, err := s.resolvePath(path, root)
	if err != nil {
		return nil, nil, err
	}
	_, contentString, err := s.readSource(resolvedPath, content)
	if err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil, newToolErr("file_not_found", fmt.Sprintf("file not found: %s", resolvedPath), resolvedPath)
		}
		return nil, nil, err
	}
	state, err := s.workspace(root)
	if err != nil {
		return nil, nil, err
	}

	scanner := token.NewScanner(resolvedPath, strings.NewReader(contentString))
	parser := rdparser.New(scanner)
	parsed := parser.ParseProgramFaultTolerant()

	cfg := &analysis.Config{
		Filename:       resolvedPath,
		ExtraGlobals:   state.cfg.ExtraGlobals,
		PackageExports: state.cfg.PackageExports,
		PackageSymbols: state.cfg.PackageSymbols,
		DefForms:       state.cfg.DefForms,
		PackageImports: state.cfg.PackageImports,
		DefaultPackage: state.cfg.DefaultPackage,
		WorkspaceRefs:  state.cfg.WorkspaceRefs,
		MacroExpander:  state.cfg.MacroExpander,
	}
	var result *analysis.Result
	if parsed.Exprs != nil {
		result = analysis.Analyze(parsed.Exprs, cfg)
	}

	return &document{
		Path:        resolvedPath,
		Content:     contentString,
		Analysis:    result,
		ParseErrors: parsed.Errors,
	}, state, nil
}

func (s *service) workspace(root string) (*workspaceState, error) {
	if root == "" {
		return s.buildWorkspaceState("", "", time.Now())
	}

	s.mu.RLock()
	cached, ok := s.workspaces[root]
	s.mu.RUnlock()
	if ok && !s.shouldValidateWorkspace(cached) {
		return cached, nil
	}

	fingerprint, err := s.fingerprintWorkspace(root)
	if err != nil {
		return nil, err
	}

	s.mu.RLock()
	if state, ok := s.workspaces[root]; ok && state.fingerprint == fingerprint {
		s.mu.RUnlock()
		s.markWorkspaceValidated(root)
		return state, nil
	}
	s.mu.RUnlock()

	// NOTE: Between the RUnlock above and the Lock below, another goroutine
	// may also build a workspace state for the same root. This is benign —
	// buildWorkspaceState is pure and the last writer wins with an identical
	// result. A full mutex around the build would serialize all workspace
	// loads, which is worse than occasional duplicate work.
	state, err := s.buildWorkspaceState(root, fingerprint, time.Now())
	if err != nil {
		return nil, err
	}
	s.mu.Lock()
	s.workspaces[root] = state
	s.mu.Unlock()
	return state, nil
}

func (s *service) buildWorkspaceState(root, fingerprint string, validatedAt time.Time) (*workspaceState, error) {
	if s.buildWorkspaceStateHook != nil {
		s.buildWorkspaceStateHook(root)
	}
	state := &workspaceState{
		cfg:         &analysis.Config{},
		fingerprint: fingerprint,
	}
	state.validatedAt.Store(validatedAt.UnixNano())
	scanCfg := &analysis.ScanConfig{
		Excludes: s.excludePatterns,
	}
	var preamble []*lisp.LVal
	if root != "" {
		prescan, err := analysis.PrescanWorkspace(root, scanCfg)
		if err != nil {
			return nil, err
		}
		state.cfg.ExtraGlobals = prescan.AllDefs
		state.cfg.PackageExports = prescan.PkgExports
		state.cfg.PackageSymbols = prescan.PkgAllSymbols
		state.cfg.DefForms = prescan.DefForms
		state.cfg.PackageImports = prescan.PackageImports
		state.cfg.DefaultPackage = prescan.DefaultPackage
		state.symbols = prescan.AllDefs
		preamble = prescan.Preamble
	}

	reg := s.registry
	if s.env != nil {
		reg = s.env.Runtime.Registry
	}
	if reg != nil {
		stdlib := analysis.ExtractPackageExports(reg)
		if state.cfg.PackageExports == nil {
			state.cfg.PackageExports = stdlib
		} else {
			for pkgName, syms := range stdlib {
				state.cfg.PackageExports[pkgName] = mergeExternalSymbols(state.cfg.PackageExports[pkgName], syms)
			}
		}
	}
	for pkgName, syms := range state.cfg.PackageExports {
		state.cfg.PackageExports[pkgName] = deduplicateExports(syms)
	}
	if root != "" {
		state.refs = analysis.ScanWorkspaceRefs(root, state.cfg, scanCfg)
		state.cfg.WorkspaceRefs = state.refs
	}
	if s.env != nil {
		if errs := analysis.LoadWorkspaceMacros(s.env, preamble); len(errs) > 0 {
			for _, err := range errs {
				slog.Warn("failed to load workspace macro", "error", err)
			}
		}
		state.cfg.MacroExpander = &analysis.EnvMacroExpander{Env: s.env}
	}
	return state, nil
}

func (s *service) readSource(path string, content *string) ([]byte, string, error) {
	if content != nil {
		return []byte(*content), *content, nil
	}
	source, err := os.ReadFile(path) //nolint:gosec // tool reads user-selected paths
	if err != nil {
		return nil, "", err
	}
	return source, string(source), nil
}

func (s *service) resolveWorkspaceRoot(root *string, require bool) (string, error) {
	if root != nil {
		if *root == "" {
			if require {
				return "", errors.New("workspace_root is required")
			}
			return "", nil
		}
		resolved, err := filepath.Abs(*root)
		if err != nil {
			return "", err
		}
		return resolved, nil
	}
	if s.workspaceRoot != "" {
		return filepath.Abs(s.workspaceRoot)
	}
	if require {
		return "", errors.New("workspace_root is required")
	}
	return "", nil
}

func (s *service) resolvePath(path string, root string) (string, error) {
	if path == "" {
		return "", errors.New("path is required")
	}
	if path == "<stdin>" {
		return path, nil
	}
	if filepath.IsAbs(path) {
		return filepath.Clean(path), nil
	}
	if root != "" {
		// Check if path already resolves under workspace root from CWD.
		if absPath, err := filepath.Abs(path); err == nil {
			if strings.HasPrefix(absPath, root+string(filepath.Separator)) || absPath == root {
				return absPath, nil
			}
		}
		resolved := filepath.Join(root, path)
		// Defense-in-depth: ensure the resolved path stays under the workspace root.
		if !strings.HasPrefix(resolved, root+string(filepath.Separator)) && resolved != root {
			return "", fmt.Errorf("path %q resolves outside workspace root", path)
		}
		return resolved, nil
	}
	return filepath.Abs(path)
}

func (s *service) rootForState(workspaceRoot *string) string {
	root, _ := s.resolveWorkspaceRoot(workspaceRoot, false)
	return root
}

func validateCursor(line, character int) error {
	if line < 0 {
		return newToolErr("invalid_position", "line must be non-negative", "")
	}
	if character < 0 {
		return newToolErr("invalid_position", "character must be non-negative", "")
	}
	return nil
}

func (s *service) listWorkspaceFiles(root string, includeTests bool) ([]string, error) {
	files, err := s.collectWorkspaceFiles(root, includeTests)
	if err != nil {
		return nil, err
	}
	paths := make([]string, 0, len(files))
	for _, file := range files {
		paths = append(paths, file.path)
	}
	return paths, nil
}

func (s *service) listPerfWorkspaceFiles(root string, cfg *perf.Config, includeTests bool) ([]string, error) {
	paths, err := s.listWorkspaceFiles(root, true)
	if err != nil {
		return nil, err
	}
	excludes := perfExcludePatterns(cfg, includeTests)
	filtered := make([]string, 0, len(paths))
	for _, path := range paths {
		if shouldExcludePerfPath(root, path, excludes) {
			continue
		}
		filtered = append(filtered, path)
	}
	return filtered, nil
}

func matchesQuery(name, query string) bool {
	return query == "" || strings.Contains(strings.ToLower(name), query)
}

func optionalStringPtr(s string) *string {
	if s == "" {
		return nil
	}
	return &s
}

func perfExcludePatterns(cfg *perf.Config, includeTests bool) []string {
	var excludes []string
	if cfg != nil {
		excludes = append(excludes, cfg.ExcludeFiles...)
	}
	if !perfIncludeTests(cfg, includeTests) {
		excludes = append(excludes, "*_test.lisp")
	}
	return excludes
}

func perfIncludeTests(cfg *perf.Config, includeTests bool) bool {
	return includeTests || (cfg != nil && cfg.IncludeTests)
}

func compareRange(a, b Range) bool {
	if a.Start.Line != b.Start.Line {
		return a.Start.Line < b.Start.Line
	}
	if a.Start.Character != b.Start.Character {
		return a.Start.Character < b.Start.Character
	}
	if a.End.Line != b.End.Line {
		return a.End.Line < b.End.Line
	}
	return a.End.Character < b.End.Character
}

func seenWorkspaceSymbol(seen map[string]bool, symbol WorkspaceSymbol) bool {
	key := symbol.Name + "|" + symbol.Package + "|" + symbol.Path + "|" + symbol.Kind + "|" +
		strconv.Itoa(symbol.Range.Start.Line) + "|" + strconv.Itoa(symbol.Range.Start.Character) + "|" +
		strconv.Itoa(symbol.Range.End.Line) + "|" + strconv.Itoa(symbol.Range.End.Character)
	if seen[key] {
		return true
	}
	seen[key] = true
	return false
}

func shouldExcludePerfPath(root, path string, patterns []string) bool {
	if len(patterns) == 0 {
		return false
	}
	rel := path
	if root != "" {
		if relPath, err := filepath.Rel(root, path); err == nil {
			rel = relPath
		}
	}
	for _, pattern := range patterns {
		if pathMatchesPattern(rel, pattern) || pathMatchesPattern(path, pattern) {
			return true
		}
	}
	return false
}

func pathMatchesPattern(path, pattern string) bool {
	normalizedPath := normalizeGlobPath(path)
	normalizedPattern := normalizeGlobPath(pattern)
	if matched, _ := filepath.Match(normalizedPattern, normalizedPath); matched {
		return true
	}
	if matched, _ := filepath.Match(normalizedPattern, filepath.Base(normalizedPath)); matched {
		return true
	}
	for _, component := range splitPath(normalizedPath) {
		if matched, _ := filepath.Match(normalizedPattern, component); matched {
			return true
		}
	}
	if strings.Contains(normalizedPattern, "**") && matchGlobstarPath(splitPath(normalizedPath), splitPath(normalizedPattern)) {
		return true
	}
	return false
}

func splitPath(path string) []string {
	path = normalizeGlobPath(path)
	if path == "." {
		return nil
	}
	parts := strings.Split(path, "/")
	out := make([]string, 0, len(parts))
	for _, part := range parts {
		if part == "" || part == "." {
			continue
		}
		out = append(out, part)
	}
	return out
}

func normalizeGlobPath(path string) string {
	return filepath.ToSlash(filepath.Clean(path))
}

func matchGlobstarPath(pathParts, patternParts []string) bool {
	if len(patternParts) == 0 {
		return len(pathParts) == 0
	}
	if patternParts[0] == "**" {
		if len(patternParts) == 1 {
			return true
		}
		for i := 0; i <= len(pathParts); i++ {
			if matchGlobstarPath(pathParts[i:], patternParts[1:]) {
				return true
			}
		}
		return false
	}
	if len(pathParts) == 0 {
		return false
	}
	matched, _ := filepath.Match(patternParts[0], pathParts[0])
	if !matched {
		return false
	}
	return matchGlobstarPath(pathParts[1:], patternParts[1:])
}

type workspaceFile struct {
	path    string
	size    int64
	modTime time.Time
}

func (s *service) collectWorkspaceFiles(root string, includeTests bool) ([]workspaceFile, error) {
	var files []workspaceFile
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			if s.logger != nil {
				s.logger.Warn("skipping unreadable workspace path", "path", path, "error", err)
			}
			if d != nil && d.IsDir() {
				return filepath.SkipDir
			}
			return nil
		}
		if d.IsDir() {
			// Never skip the root directory itself — only subdirectories.
			if path != root && analysis.ShouldSkipDir(d.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".lisp" {
			return nil
		}
		if !includeTests && strings.HasSuffix(path, "_test.lisp") {
			return nil
		}
		info, infoErr := d.Info()
		if infoErr != nil {
			if s.logger != nil {
				s.logger.Warn("skipping file with unreadable info", "path", path, "error", infoErr)
			}
			return nil
		}
		files = append(files, workspaceFile{
			path:    path,
			size:    info.Size(),
			modTime: info.ModTime(),
		})
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Slice(files, func(i, j int) bool { return files[i].path < files[j].path })
	return files, nil
}

func (s *service) workspaceFingerprint(root string) (string, error) {
	if root == "" {
		return "", nil
	}
	files, err := s.collectWorkspaceFiles(root, true)
	if err != nil {
		return "", err
	}
	h := fnv.New64a()
	for _, file := range files {
		_, _ = fmt.Fprintf(h, "%s\x00%d\x00%d\x00", file.path, file.size, file.modTime.UnixNano())
	}
	return fmt.Sprintf("%x", h.Sum(nil)), nil
}

func (s *service) shouldValidateWorkspace(state *workspaceState) bool {
	if state == nil {
		return true
	}
	interval := s.workspaceValidationInterval
	if interval <= 0 {
		return true
	}
	validated := time.Unix(0, state.validatedAt.Load())
	return time.Since(validated) >= interval
}

func (s *service) fingerprintWorkspace(root string) (string, error) {
	if s.workspaceFingerprintHook != nil {
		s.workspaceFingerprintHook(root)
	}
	return s.workspaceFingerprint(root)
}

func (s *service) markWorkspaceValidated(root string) {
	s.mu.RLock()
	state, ok := s.workspaces[root]
	s.mu.RUnlock()
	if ok {
		state.validatedAt.Store(time.Now().UnixNano())
	}
}

func deduplicateExports(syms []analysis.ExternalSymbol) []analysis.ExternalSymbol {
	seen := make(map[string]int, len(syms))
	var result []analysis.ExternalSymbol
	for _, sym := range syms {
		if idx, ok := seen[sym.Name]; ok {
			if preferExternalSymbol(sym, result[idx]) {
				result[idx] = sym
			}
			continue
		}
		seen[sym.Name] = len(result)
		result = append(result, sym)
	}
	return result
}

func mergeExternalSymbols(primary, secondary []analysis.ExternalSymbol) []analysis.ExternalSymbol {
	if len(primary) == 0 {
		return deduplicateExports(append([]analysis.ExternalSymbol(nil), secondary...))
	}
	result := append([]analysis.ExternalSymbol(nil), primary...)
	seen := make(map[string]bool, len(primary))
	for _, sym := range primary {
		seen[sym.Name] = true
	}
	for _, sym := range secondary {
		if seen[sym.Name] {
			continue
		}
		seen[sym.Name] = true
		result = append(result, sym)
	}
	return deduplicateExports(result)
}

func preferExternalSymbol(candidate, current analysis.ExternalSymbol) bool {
	if hasRealSource(candidate.Source) != hasRealSource(current.Source) {
		return hasRealSource(candidate.Source)
	}
	return false
}

func hasRealSource(loc *token.Location) bool {
	return loc != nil && loc.Line > 0
}

func getWorkspaceRefs(state *workspaceState, key string, excludeFile string) []analysis.FileReference {
	if state == nil || state.refs == nil {
		return nil
	}
	refs := state.refs[key]
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

func splitPackageQualified(prefix string) (pkg, partial string, ok bool) {
	if strings.HasPrefix(prefix, ":") {
		return "", "", false
	}
	idx := strings.LastIndex(prefix, ":")
	if idx < 0 {
		return "", "", false
	}
	return prefix[:idx], prefix[idx+1:], true
}

func symbolAtPosition(doc *document, line, character int) (*analysis.Symbol, *analysis.Reference) {
	if doc == nil || doc.Analysis == nil {
		return nil, nil
	}
	elpsLine := line + 1
	elpsCol := character + 1
	for _, ref := range doc.Analysis.References {
		if ref.Source == nil || ref.Source.Line == 0 {
			continue
		}
		if ref.Source.Line == elpsLine && locContainsCol(ref.Source, ref.Symbol.Name, elpsCol) {
			return ref.Symbol, ref
		}
	}
	for _, sym := range doc.Analysis.Symbols {
		if sym.Source == nil || sym.Source.Line == 0 {
			continue
		}
		if sym.Source.Line == elpsLine && locContainsCol(sym.Source, sym.Name, elpsCol) {
			return sym, nil
		}
	}
	return nil, nil
}

func locContainsCol(loc *token.Location, name string, col int) bool {
	start := loc.Col
	if start == 0 {
		return false
	}
	end := start + len(name)
	if loc.EndCol > 0 {
		end = loc.EndCol
	}
	return col >= start && col < end
}

// wordAtPosition extracts the symbol word at a 0-based line and column.
// It operates on byte offsets, which is correct for ELPS symbol names (ASCII
// only). Multi-byte UTF-8 in comments or strings could misalign the column,
// but symbol lookup would simply miss — no incorrect results.
func wordAtPosition(content string, line, col int) string {
	lines := strings.Split(content, "\n")
	if line < 0 || line >= len(lines) {
		return ""
	}
	ln := lines[line]
	if col < 0 || col > len(ln) {
		return ""
	}
	if col >= len(ln) {
		col = len(ln)
	}
	start := col
	for start > 0 && isSymbolChar(ln[start-1]) {
		start--
	}
	end := col
	for end < len(ln) && isSymbolChar(ln[end]) {
		end++
	}
	return ln[start:end]
}

func isSymbolChar(c byte) bool {
	if c >= 'a' && c <= 'z' {
		return true
	}
	if c >= 'A' && c <= 'Z' {
		return true
	}
	if c >= '0' && c <= '9' {
		return true
	}
	switch c {
	case '-', '_', '!', '?', '+', '*', '/', '<', '>', '=', ':', '.', '#', '^':
		return true
	}
	return false
}

func qualifiedSymbolHover(state *workspaceState, word string) (string, *analysis.ExternalSymbol) {
	pkgName, symName, ok := splitPackageQualified(word)
	if !ok || symName == "" || state == nil || state.cfg == nil || state.cfg.PackageExports == nil {
		return "", nil
	}
	for _, ext := range state.cfg.PackageExports[pkgName] {
		if ext.Name == symName {
			return buildHoverContent(externalToSymbol(&ext)), &ext
		}
	}
	return "", nil
}

func qualifiedSymbolDefinition(state *workspaceState, word string) *Location {
	pkgName, symName, ok := splitPackageQualified(word)
	if !ok || symName == "" || state == nil || state.cfg == nil || state.cfg.PackageExports == nil {
		return nil
	}
	for _, ext := range state.cfg.PackageExports[pkgName] {
		if ext.Name != symName {
			continue
		}
		sym := externalToSymbol(&ext)
		if isBuiltin(sym) {
			return builtinLocation(pkgName, symName)
		}
		if sym.Source != nil {
			return locationFromSource(sym.Source.File, sym.Source, len(sym.Name))
		}
	}
	return nil
}

func externalToSymbol(ext *analysis.ExternalSymbol) *analysis.Symbol {
	return &analysis.Symbol{
		Name:      ext.Name,
		Package:   ext.Package,
		Kind:      ext.Kind,
		Source:    ext.Source,
		Signature: ext.Signature,
		DocString: ext.DocString,
		External:  true,
	}
}

func buildHoverContent(sym *analysis.Symbol) string {
	var b strings.Builder
	b.WriteString("**")
	b.WriteString(symbolKindLabel(sym.Kind))
	b.WriteString("** `")
	b.WriteString(sym.Name)
	b.WriteString("`")
	if sym.Signature != nil {
		sig := formatSignature(sym.Signature)
		inner := strings.Trim(sig, "()")
		b.WriteString("\n\n```lisp\n(")
		b.WriteString(sym.Name)
		if inner != "" {
			b.WriteString(" ")
			b.WriteString(inner)
		}
		b.WriteString(")\n```")
	}
	if sym.DocString != "" {
		b.WriteString("\n\n")
		b.WriteString(sym.DocString)
	}
	if sym.Source != nil && sym.Source.File != "" {
		fmt.Fprintf(&b, "\n\n*Defined in %s:%d*", sym.Source.File, sym.Source.Line)
	} else if isBuiltin(sym) {
		b.WriteString("\n\n*Built-in*")
	}
	return b.String()
}

func symbolKindLabel(kind analysis.SymbolKind) string {
	switch kind {
	case analysis.SymFunction:
		return "function"
	case analysis.SymMacro:
		return "macro"
	case analysis.SymVariable:
		return "variable"
	case analysis.SymParameter:
		return "parameter"
	case analysis.SymSpecialOp:
		return "special operator"
	case analysis.SymBuiltin:
		return "builtin"
	case analysis.SymType:
		return "type"
	default:
		return "symbol"
	}
}

func formatSignature(sig *analysis.Signature) string {
	if sig == nil || len(sig.Params) == 0 {
		return ""
	}
	var parts []string
	for _, p := range sig.Params {
		part := p.Name
		switch p.Kind {
		case lisp.ParamRest:
			part = "&rest " + part
		case lisp.ParamOptional:
			part = "&optional " + part
		case lisp.ParamKey:
			part = "&key " + part
		}
		parts = append(parts, part)
	}
	return "(" + strings.Join(parts, " ") + ")"
}

func symbolLocation(sym *analysis.Symbol, currentPath string, state *workspaceState) *Location {
	if sym == nil {
		return nil
	}
	if isBuiltin(sym) {
		return builtinLocation(builtinPackage(sym), sym.Name)
	}
	if sym.Source == nil {
		return nil
	}
	path := sym.Source.File
	if path == "" {
		path = currentPath
	}
	return locationFromSource(path, sym.Source, len(sym.Name))
}

func externalLocation(sym *analysis.ExternalSymbol, _ *workspaceState) *Location {
	if sym == nil || sym.Source == nil {
		return nil
	}
	return locationFromSource(sym.Source.File, sym.Source, len(sym.Name))
}

func locationFromSource(path string, loc *token.Location, nameLen int) *Location {
	if loc == nil {
		return nil
	}
	rng := rangeFromSource(loc, nameLen)
	return &Location{
		Path:         path,
		Line:         rng.Start.Line,
		Character:    rng.Start.Character,
		EndLine:      rng.End.Line,
		EndCharacter: rng.End.Character,
	}
}

func rangeFromSource(loc *token.Location, nameLen int) Range {
	start := Position{Line: max(loc.Line-1, 0), Character: max(loc.Col-1, 0)}
	end := Position{Line: start.Line, Character: start.Character + nameLen}
	if loc.EndLine > 0 && loc.EndCol > 0 {
		end = Position{Line: max(loc.EndLine-1, 0), Character: max(loc.EndCol-1, 0)}
	}
	return Range{Start: start, End: end}
}

func parseDiagnostic(err error, path string) Diagnostic {
	rng := Range{}
	var errVal *lisp.ErrorVal
	if errors.As(err, &errVal) && errVal.Source != nil && errVal.Source.Line > 0 {
		rng = rangeFromSource(errVal.Source, 1)
	}
	var locErr *token.LocationError
	if errors.As(err, &locErr) && locErr.Source != nil && locErr.Source.Line > 0 {
		rng = rangeFromSource(locErr.Source, 1)
	}
	return Diagnostic{
		Source:   "elps",
		Severity: "error",
		Message:  err.Error(),
		Range:    rng,
	}
}

func lintDiagnostic(diag lint.Diagnostic) Diagnostic {
	start := Position{Line: max(diag.Pos.Line-1, 0), Character: max(diag.Pos.Col-1, 0)}
	end := start
	if diag.EndPos.Line > 0 {
		end = Position{Line: max(diag.EndPos.Line-1, 0), Character: max(diag.EndPos.Col-1, 0)}
	}
	return Diagnostic{
		Source:   "elps-lint",
		Code:     diag.Analyzer,
		Severity: diag.Severity.String(),
		Message:  diag.Message,
		Range:    Range{Start: start, End: end},
	}
}

func builtinLocationForSymbolWord(sym *analysis.Symbol, word string) *Location {
	pkg := builtinPackageForWord(word)
	if pkg == "lisp" && !strings.Contains(word, ":") {
		pkg = builtinPackage(sym)
	}
	return builtinLocation(pkg, sym.Name)
}

func builtinPackageForWord(word string) string {
	if pkgName, _, ok := splitPackageQualified(word); ok && pkgName != "" {
		return pkgName
	}
	return "lisp"
}

func builtinPackage(sym *analysis.Symbol) string {
	if sym != nil && sym.Package != "" {
		return sym.Package
	}
	return "lisp"
}

func builtinLocation(pkg, name string) *Location {
	return &Location{
		Line:         0,
		Character:    0,
		EndLine:      0,
		EndCharacter: 0,
		Virtual:      true,
		VirtualID:    builtinURI(pkg, name),
	}
}

func builtinURI(pkg, name string) string {
	return builtinScheme + "://" + pkg + "/" + name
}

func isBuiltin(sym *analysis.Symbol) bool {
	return sym.Source == nil || sym.Source.Pos < 0
}

func resolvePathAgainstRoot(path, root, fallback string) string {
	if path == "" {
		return fallback
	}
	if filepath.IsAbs(path) || root == "" {
		return path
	}
	return filepath.Join(root, path)
}

func validPerfRule(rule string) bool {
	switch rule {
	case string(perf.PERF001), string(perf.PERF002), string(perf.PERF003), string(perf.PERF004), string(perf.UNKNOWN001):
		return true
	default:
		return false
	}
}

func mapIssues(issues []perf.Issue) []PerfIssue {
	out := make([]PerfIssue, 0, len(issues))
	for _, issue := range issues {
		var trace []TraceEntry
		for _, entry := range issue.Trace {
			trace = append(trace, TraceEntry{
				Function: entry.Function,
				Location: locationFromSource(entry.Source.File, entry.Source, len(entry.Function)),
				Note:     entry.Note,
			})
		}
		out = append(out, PerfIssue{
			Rule:        string(issue.Rule),
			Severity:    issue.Severity.String(),
			Message:     issue.Message,
			Function:    issue.Function,
			Path:        issue.File,
			Location:    locationFromSource(issue.File, issue.Source, len(issue.Function)),
			Details:     append([]string(nil), issue.Details...),
			Fingerprint: issue.Fingerprint,
			Trace:       trace,
		})
	}
	return out
}

func topSolved(solved []*perf.SolvedFunction, top int) []SolvedFunctionSummary {
	if len(solved) == 0 || top <= 0 {
		return []SolvedFunctionSummary{}
	}
	items := append([]*perf.SolvedFunction(nil), solved...)
	sort.Slice(items, func(i, j int) bool {
		if items[i].TotalScore == items[j].TotalScore {
			return items[i].Name < items[j].Name
		}
		return items[i].TotalScore > items[j].TotalScore
	})
	if top < len(items) {
		items = items[:top]
	}
	out := make([]SolvedFunctionSummary, 0, len(items))
	for _, fn := range items {
		out = append(out, SolvedFunctionSummary{
			Name:         fn.Name,
			Path:         fn.File,
			Location:     locationFromSource(fn.File, fn.Source, len(fn.Name)),
			LocalCost:    fn.LocalCost,
			TotalScore:   fn.TotalScore,
			ScalingOrder: fn.ScalingOrder,
			InCycle:      fn.InCycle,
		})
	}
	return out
}

func mapCallGraph(graph *perf.CallGraph) CallGraphResponse {
	if graph == nil {
		return CallGraphResponse{}
	}
	functions := make([]CallGraphFunction, 0, len(graph.Functions))
	for _, fn := range graph.Functions {
		functions = append(functions, CallGraphFunction{
			Name:         fn.Name,
			Path:         fn.File,
			Location:     locationFromSource(fn.File, fn.Source, len(fn.Name)),
			LocalCost:    fn.LocalCost,
			MaxLoopDepth: fn.MaxLoopDepth,
		})
	}
	sort.Slice(functions, func(i, j int) bool { return functions[i].Name < functions[j].Name })
	edges := make([]CallGraphEdge, 0, len(graph.Edges))
	for _, edge := range graph.Edges {
		edges = append(edges, CallGraphEdge{
			Caller:      edge.Caller,
			Callee:      edge.Callee,
			Location:    locationFromSource(edge.Source.File, edge.Source, len(edge.Callee)),
			LoopDepth:   edge.Context.LoopDepth,
			InLoop:      edge.Context.InLoop,
			IsExpensive: edge.IsExpensive,
		})
	}
	sort.Slice(edges, func(i, j int) bool {
		if edges[i].Caller == edges[j].Caller {
			return edges[i].Callee < edges[j].Callee
		}
		return edges[i].Caller < edges[j].Caller
	})
	return CallGraphResponse{Functions: functions, Edges: edges}
}

func truncateCallGraph(resp CallGraphResponse, solved []*perf.SolvedFunction, top int) CallGraphResponse {
	if top <= 0 || len(resp.Functions) <= top {
		return resp
	}
	items := append([]*perf.SolvedFunction(nil), solved...)
	sort.Slice(items, func(i, j int) bool {
		if items[i].TotalScore == items[j].TotalScore {
			return items[i].Name < items[j].Name
		}
		return items[i].TotalScore > items[j].TotalScore
	})
	if top < len(items) {
		items = items[:top]
	}
	keep := make(map[string]bool, len(items))
	for _, fn := range items {
		keep[fn.Name] = true
	}
	totalFunctions := len(resp.Functions)
	totalEdges := len(resp.Edges)
	functions := make([]CallGraphFunction, 0, len(items))
	for _, fn := range resp.Functions {
		if keep[fn.Name] {
			functions = append(functions, fn)
		}
	}
	edges := make([]CallGraphEdge, 0, len(resp.Edges))
	for _, edge := range resp.Edges {
		if keep[edge.Caller] || keep[edge.Callee] {
			edges = append(edges, edge)
		}
	}
	return CallGraphResponse{
		Functions:      functions,
		Edges:          edges,
		Truncated:      true,
		TotalFunctions: totalFunctions,
		TotalEdges:     totalEdges,
	}
}

func filterSolvedByRules(solved []*perf.SolvedFunction, issues []perf.Issue, rules []string) []*perf.SolvedFunction {
	ruleSet := make(map[string]bool, len(rules))
	for _, r := range rules {
		ruleSet[r] = true
	}
	funcHasMatchingIssue := make(map[string]bool)
	for _, issue := range issues {
		if ruleSet[string(issue.Rule)] {
			funcHasMatchingIssue[issue.Function] = true
		}
	}
	var filtered []*perf.SolvedFunction
	for _, fn := range solved {
		if funcHasMatchingIssue[fn.Name] {
			filtered = append(filtered, fn)
		}
	}
	return filtered
}

func filterDiagnosticsBySeverity(diags []Diagnostic, severity *string) []Diagnostic {
	if severity == nil || *severity == "" {
		return diags
	}
	target := strings.ToLower(*severity)
	filtered := make([]Diagnostic, 0, len(diags))
	for _, d := range diags {
		if strings.ToLower(d.Severity) == target {
			filtered = append(filtered, d)
		}
	}
	return filtered
}

func (s *service) applyDiagnosticsLimits(result []FileDiagnostics, maxFiles, offset int) DiagnosticsResponse {
	resp := DiagnosticsResponse{Files: result}
	if offset > 0 || maxFiles > 0 {
		total := len(resp.Files)
		resp.Files = paginateSlice(resp.Files, offset, maxFiles)
		if len(resp.Files) < total {
			resp.Truncated = true
			resp.TotalFiles = total
		}
	}
	return resp
}

func paginateSlice[T any](items []T, offset, limit int) []T {
	if offset > 0 {
		if offset >= len(items) {
			return []T{}
		}
		items = items[offset:]
	}
	if limit > 0 && limit < len(items) {
		items = items[:limit]
	}
	return items
}

func (s *service) docTool(_ context.Context, _ *mcp.CallToolRequest, in DocInput) (*mcp.CallToolResult, DocResponse, error) {
	start := time.Now()
	if in.Query == "" && len(in.Queries) == 0 {
		return nil, DocResponse{}, newToolErr("invalid_input", "query or queries is required", "")
	}
	env, err := s.docEnv()
	if err != nil {
		return nil, DocResponse{}, err
	}

	// Batch mode: look up multiple symbols at once.
	if len(in.Queries) > 0 {
		batch := make([]DocResult, 0, len(in.Queries))
		for _, q := range in.Queries {
			dr := DocResult{Query: q}
			sd, lookupErr := libhelp.QuerySymbol(env, q)
			if lookupErr == nil {
				sym := docSymbolFromLib(*sd)
				dr.Found = true
				dr.Symbol = &sym
			}
			batch = append(batch, dr)
		}
		return nil, DocResponse{Batch: batch, Meta: makeMeta("", time.Since(start), 0)}, nil
	}

	if in.Package {
		pd, pkgErr := libhelp.QueryPackage(env, in.Query)
		if pkgErr != nil {
			return nil, DocResponse{Found: false, Meta: makeMeta("", time.Since(start), 0)}, nil
		}
		pkg := &DocPackage{
			Name:    pd.Name,
			Doc:     pd.Doc,
			Symbols: make([]DocSymbol, 0, len(pd.Symbols)),
		}
		for _, sym := range pd.Symbols {
			pkg.Symbols = append(pkg.Symbols, docSymbolFromLib(sym))
		}
		return nil, DocResponse{Found: true, Package: pkg, Meta: makeMeta("", time.Since(start), 0)}, nil
	}
	sd, lookupErr := libhelp.QuerySymbol(env, in.Query)
	if lookupErr != nil {
		return nil, DocResponse{Found: false, Meta: makeMeta("", time.Since(start), 0)}, nil
	}
	sym := docSymbolFromLib(*sd)
	return nil, DocResponse{Found: true, Symbol: &sym, Meta: makeMeta("", time.Since(start), 0)}, nil
}

func docSymbolFromLib(sd libhelp.SymbolDoc) DocSymbol {
	ds := DocSymbol{
		Name: sd.Name,
		Kind: sd.Kind,
		Doc:  sd.Doc,
	}
	if sd.Formals != nil {
		ds.Formals = &DocFormals{
			Required: sd.Formals.Required,
			Optional: sd.Formals.Optional,
			Rest:     sd.Formals.Rest,
			Keys:     sd.Formals.Keys,
		}
	}
	return ds
}

func (s *service) docEnv() (*lisp.LEnv, error) {
	if s.env != nil {
		return s.env, nil
	}
	if s.envFactory != nil {
		return s.envFactory()
	}
	env, err := lisplib.NewDocEnv()
	if err != nil {
		return nil, err
	}
	if s.registry != nil {
		for name, pkg := range s.registry.Packages {
			if _, exists := env.Runtime.Registry.Packages[name]; !exists {
				env.Runtime.Registry.Packages[name] = pkg
			}
		}
	}
	return env, nil
}

func (s *service) testTool(_ context.Context, _ *mcp.CallToolRequest, in TestInput) (*mcp.CallToolResult, TestResponse, error) {
	start := time.Now()
	if in.Path == "" && in.Content == nil {
		return nil, TestResponse{}, newToolErr("invalid_input", "path or content is required", "")
	}
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	if err != nil {
		return nil, TestResponse{}, newToolErr("invalid_input", err.Error(), "")
	}
	path := "<stdin>"
	if in.Path != "" {
		resolved, resolveErr := s.resolvePath(in.Path, root)
		if resolveErr != nil {
			return nil, TestResponse{}, newToolErr("invalid_input", resolveErr.Error(), in.Path)
		}
		path = resolved
	}
	var source []byte
	if in.Content != nil {
		source = []byte(*in.Content)
	} else {
		source, err = os.ReadFile(path) //nolint:gosec // tool reads user-selected paths
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return nil, TestResponse{}, newToolErr("file_not_found", fmt.Sprintf("file not found: %s", path), path)
			}
			return nil, TestResponse{}, newToolErr("invalid_input", err.Error(), path)
		}
	}

	env, testErr := s.newTestEnv()
	if testErr != nil {
		return nil, TestResponse{}, testErr
	}

	// Load and evaluate the test file.
	lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if lisp.GoError(lerr) != nil {
		return nil, TestResponse{}, fmt.Errorf("package error: %v", lisp.GoError(lerr))
	}
	autoImportTesting(env)
	exprs, parseErr := env.Runtime.Reader.Read(path, strings.NewReader(string(source)))
	if parseErr != nil {
		return nil, TestResponse{
			Path:  path,
			Tests: []TestResult{{Name: "<parse>", Passed: false, Error: parseErr.Error()}},
			Total: 1, Failed: 1,
			Meta: makeMeta(root, time.Since(start), 1),
		}, nil
	}
	for _, expr := range exprs {
		// When the file switches packages, auto-import testing into the
		// new package so test/assert macros are available immediately.
		if isInPackageExpr(expr) {
			env.Eval(expr)
			autoImportTesting(env)
			continue
		}
		result := env.Eval(expr)
		if result.Type == lisp.LError {
			return nil, TestResponse{
				Path:  path,
				Tests: []TestResult{{Name: "<load>", Passed: false, Error: lvalErrorString(result)}},
				Total: 1, Failed: 1,
				Meta: makeMeta(root, time.Since(start), 1),
			}, nil
		}
	}

	suite := libtesting.EnvTestSuite(env)
	if suite == nil || suite.Len() == 0 {
		return nil, TestResponse{
			Path: path, Tests: []TestResult{}, Meta: makeMeta(root, time.Since(start), 1),
		}, nil
	}

	results := make([]TestResult, 0, suite.Len())
	passed, failed := 0, 0
	for i := range suite.Len() {
		test := suite.Test(i)
		tr := TestResult{Name: test.Name}
		if test.Fun == nil {
			tr.Error = "test has no function body"
			failed++
			results = append(results, tr)
			continue
		}
		result := env.Eval(lisp.SExpr([]*lisp.LVal{test.Fun}))
		if result == nil || result.Type == lisp.LError {
			tr.Passed = false
			if result != nil {
				tr.Error = lvalErrorString(result)
			}
			failed++
		} else {
			tr.Passed = true
			passed++
		}
		results = append(results, tr)
	}

	return nil, TestResponse{
		Path:   path,
		Tests:  results,
		Passed: passed,
		Failed: failed,
		Total:  len(results),
		Meta:   makeMeta(root, time.Since(start), 1),
	}, nil
}

func (s *service) newTestEnv() (*lisp.LEnv, error) {
	if s.envFactory != nil {
		return s.envFactory()
	}
	env := lisp.NewEnv(nil)
	env.Runtime.Reader = parser.NewReader()
	env.Runtime.Library = &lisp.RelativeFileSystemLibrary{}
	rc := lisp.InitializeUserEnv(env)
	if lisp.GoError(rc) != nil {
		return nil, fmt.Errorf("env init: %v", lisp.GoError(rc))
	}
	rc = lisplib.LoadLibrary(env)
	if lisp.GoError(rc) != nil {
		return nil, fmt.Errorf("load library: %v", lisp.GoError(rc))
	}
	return env, nil
}

func (s *service) evalTool(_ context.Context, _ *mcp.CallToolRequest, in EvalInput) (*mcp.CallToolResult, EvalResponse, error) {
	start := time.Now()
	if in.Expression == "" && len(in.Expressions) == 0 {
		return nil, EvalResponse{}, newToolErr("invalid_input", "expression or expressions is required", "")
	}

	// Batch mode: each expression gets its own isolated env.
	if len(in.Expressions) > 0 {
		batch := make([]EvalResult, 0, len(in.Expressions))
		for _, expr := range in.Expressions {
			result := s.evalSingle(expr)
			batch = append(batch, result)
		}
		return nil, EvalResponse{
			Batch: batch,
			Meta:  makeMeta("", time.Since(start), 0),
		}, nil
	}

	// Single expression mode.
	env, err := s.newTestEnv()
	if err != nil {
		return nil, EvalResponse{}, err
	}
	lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if lisp.GoError(lerr) != nil {
		return nil, EvalResponse{}, fmt.Errorf("package error: %v", lisp.GoError(lerr))
	}

	exprs, parseErr := env.Runtime.Reader.Read("<eval>", strings.NewReader(in.Expression))
	if parseErr != nil {
		return nil, EvalResponse{
			Error: parseErr.Error(),
			Meta:  makeMeta("", time.Since(start), 0),
		}, nil
	}

	var lastResult *lisp.LVal
	var results []string
	for _, expr := range exprs {
		lastResult = env.Eval(expr)
		if lastResult == nil || lastResult.Type == lisp.LError {
			errMsg := "unknown error"
			if lastResult != nil {
				errMsg = lvalErrorString(lastResult)
			}
			return nil, EvalResponse{
				Error: errMsg,
				Meta:  makeMeta("", time.Since(start), 0),
			}, nil
		}
		results = append(results, lastResult.String())
	}

	value := ""
	if lastResult != nil {
		value = lastResult.String()
	}
	return nil, EvalResponse{
		Value:   value,
		Results: results,
		Meta:    makeMeta("", time.Since(start), 0),
	}, nil
}

func (s *service) evalSingle(expression string) EvalResult {
	env, err := s.newTestEnv()
	if err != nil {
		return EvalResult{Expression: expression, Error: err.Error()}
	}
	lerr := env.InPackage(lisp.String(lisp.DefaultUserPackage))
	if lisp.GoError(lerr) != nil {
		return EvalResult{Expression: expression, Error: lisp.GoError(lerr).Error()}
	}
	exprs, parseErr := env.Runtime.Reader.Read("<eval>", strings.NewReader(expression))
	if parseErr != nil {
		return EvalResult{Expression: expression, Error: parseErr.Error()}
	}
	var lastResult *lisp.LVal
	for _, expr := range exprs {
		lastResult = env.Eval(expr)
		if lastResult == nil || lastResult.Type == lisp.LError {
			errMsg := "unknown error"
			if lastResult != nil {
				errMsg = lvalErrorString(lastResult)
			}
			return EvalResult{Expression: expression, Error: errMsg}
		}
	}
	value := ""
	if lastResult != nil {
		value = lastResult.String()
	}
	return EvalResult{Expression: expression, Value: value}
}

func (s *service) helpTool(_ context.Context, _ *mcp.CallToolRequest, _ HelpInput) (*mcp.CallToolResult, HelpResponse, error) {
	return nil, HelpResponse{Content: helpContent}, nil
}

const helpContent = `# ELPS MCP Server — Usage Guide

## Coordinate System
Lines and characters are **0-indexed** (LSP convention). Line 1 in your editor is line 0 in MCP tool calls.

## Path Resolution
- Use **bare filenames** (` + "`utils.lisp`" + `) for files in the workspace root.
- Use **absolute paths** for files outside the workspace.
- Do NOT use project-relative paths that include the workspace root directory name — they get double-prefixed.

## Content Override
The ` + "`content`" + ` parameter lets you analyze unsaved buffer content without writing to disk.
- Works for ` + "`defun`" + `, ` + "`set`" + `, ` + "`deftype`" + `, ` + "`defmacro`" + ` within a single file.
- Cross-package ` + "`use-package`" + ` within a single content buffer does NOT resolve imported symbols (single-file prescan limitation).
- ` + "`path`" + ` is still needed for workspace context when using content override with navigation tools.
- For diagnostics, you may pass ` + "`content`" + ` without ` + "`path`" + ` — results use ` + "`<stdin>`" + ` as the file path.

## workspace_root
Each tool call can specify a different ` + "`workspace_root`" + `. A new root triggers a fresh directory scan.
This is NOT the same as the startup ` + "`--workspace-root`" + ` flag (which sets the default).

## Scoping (Performance Tools)
` + "`call_graph`" + `, ` + "`hotspots`" + `, ` + "`perf_issues`" + ` analyze the entire workspace by default.
**Always pass ` + "`paths`" + `** to avoid output overflow. Use ` + "`top`" + ` to further limit results.

### ` + "`top`" + ` Parameter Behavior
- **hotspots**: limits function count
- **perf_issues**: limits both issues and solved functions
- **call_graph**: limits functions to top-N by cost, edges filtered to match

### ` + "`rules`" + ` Filter
On ` + "`perf_issues`" + `: filters both ` + "`issues`" + ` AND ` + "`solved`" + ` results.
On ` + "`call_graph`" + `: the ` + "`rules`" + ` parameter has no effect — scope with ` + "`paths`" + ` instead.

## workspace_symbols
Only finds ` + "`.lisp`" + `-defined symbols. Go-registered builtins (from the embedder's registry) are NOT returned.
Use ` + "`hover`" + ` on a known position to get builtin info.

## include_tests
Test files (` + "`*_test.lisp`" + `) are excluded by default. Set ` + "`include_tests=true`" + ` to include them.
This applies to all paths, whether using explicit ` + "`paths`" + ` or workspace scan.

## Result Limiting and Pagination
All list-returning tools support a ` + "`limit`" + ` parameter (or ` + "`max_files`" + ` for diagnostics, ` + "`top`" + ` for perf tools).
When results are trimmed, the response includes ` + "`truncated: true`" + ` and a total count field.
Use ` + "`offset`" + ` to paginate: ` + "`{\"limit\": 10, \"offset\": 20}`" + ` skips the first 20 results and returns the next 10.

## Empty Results
All tools return empty arrays (` + "`[]`" + `), never ` + "`null`" + `, for list fields.

## Eval Batch Mode
Each expression in ` + "`expressions`" + ` is evaluated in an **isolated environment**.
Variables and functions defined in one expression are NOT visible to later expressions.
To share state across expressions, combine them into a single ` + "`expression`" + ` string
using ` + "`let`" + `, ` + "`progn`" + `, or multiple top-level forms separated by newlines.

## Test Tool
The test tool auto-imports the ` + "`testing`" + ` package into both the default user
package and whatever package the file switches to via ` + "`(in-package ...)`" + `.
So ` + "`test`" + `, ` + "`assert-equal`" + `, ` + "`assert=`" + `, ` + "`assert-nil`" + `, etc. are available without
explicit ` + "`(use-package 'testing)`" + ` in any package context.

## Common Workflows
1. **Find a symbol**: ` + "`workspace_symbols`" + ` → locate definition file and line
2. **Understand a symbol**: ` + "`hover`" + ` → get signature, docs, definition location
3. **Go to definition**: ` + "`definition`" + ` → jump to where a symbol is defined
4. **Find usages**: ` + "`references`" + ` → find all references across the workspace
5. **Check for errors**: ` + "`diagnostics`" + ` → parse errors and lint warnings
6. **Lint only**: ` + "`lint`" + ` → run specific analyzers with ` + "`checks`" + ` filter
7. **Format code**: ` + "`format`" + ` → format source; use ` + "`check_only=true`" + ` to just check without getting content back
8. **Look up docs**: ` + "`doc`" + ` → search by name (` + "`map`" + `, ` + "`math:sin`" + `); use ` + "`package=true`" + ` to list a package
9. **Run tests**: ` + "`test`" + ` → run test file, get structured pass/fail per test
10. **Quick eval**: ` + "`eval`" + ` → evaluate expressions; use ` + "`expressions`" + ` array for batch mode
11. **Performance audit**: ` + "`perf_issues`" + ` → find performance problems, then ` + "`call_graph`" + ` for context

## Diagnostics vs Lint
- **diagnostics**: returns parse errors AND lint warnings for files. Use for "is this file healthy?"
  Supports ` + "`include_workspace`" + ` for cross-file analysis.
- **lint**: runs specific analyzers only. Use ` + "`checks`" + ` to select which analyzers to run.
  Supports ` + "`severity`" + ` filter and pagination. Better for targeted analysis.

## Format check_only
Pass ` + "`check_only=true`" + ` to the format tool to check whether a file needs formatting
without receiving the formatted content. Returns ` + "`changed: true/false`" + ` only.
When ` + "`check_only`" + ` is false (default), content is only returned when ` + "`changed: true`" + `.

## Standard Library Packages
Use ` + "`doc`" + ` with ` + "`package=true`" + ` to list symbols in any package:
` + "`math`" + `, ` + "`string`" + `, ` + "`json`" + `, ` + "`regexp`" + `, ` + "`time`" + `, ` + "`base64`" + `, ` + "`testing`" + `, ` + "`schema`" + `, ` + "`help`" + `, ` + "`golang`" + `.
Embedders may register additional packages via ` + "`WithRegistry`" + `.

## Lint Analyzers
| Analyzer | Severity | Description |
|----------|----------|-------------|
| set-usage | warning | Flags repeated ` + "`set`" + ` on same symbol (use ` + "`set!`" + ` for mutation) |
| in-package-toplevel | error | ` + "`in-package`" + ` must be at top level |
| if-arity | error | ` + "`if`" + ` requires 2-3 arguments |
| let-bindings | error | ` + "`let`" + ` binding list must be well-formed |
| defun-structure | error | ` + "`defun`" + ` requires name, params, body |
| cond-structure | error | ` + "`cond`" + ` clauses must be lists |
| builtin-arity | error | Checks argument count for builtins |
| rethrow-context | error | ` + "`rethrow`" + ` only valid inside ` + "`handler-bind`" + ` |
| handler-bind-structure | error | ` + "`handler-bind`" + ` form must be well-formed |
| cond-else | warning | ` + "`cond`" + ` ` + "`else`" + ` must be last clause |
| test-structure | error | ` + "`test`" + ` form must have name and body |
| lambda-structure | error | ` + "`lambda`" + ` requires params and body |
| missing-package-qualifier | warning | Unexported symbol used without package qualifier |
| deftype-structure | error | ` + "`deftype`" + ` requires name and field list |
| export-form | warning | ` + "`export`" + ` argument must be a quoted symbol |
| sort-stable-comparator | warning | ` + "`sort-stable`" + ` requires a comparator function |
| assert-arity | warning | ` + "`assert-*`" + ` test macros have specific arity |

## Performance Rules
| Rule | Description |
|------|-------------|
| PERF001 | Expensive function call detected inside a loop |
| PERF002 | Hot path — function exceeds cost threshold |
| PERF003 | High scaling order (e.g., O(n²) or worse) |
| PERF004 | Cycle detected in call graph |
| UNKNOWN001 | Unresolved function call in analysis |

## Advanced: Performance Config
The ` + "`config`" + ` parameter on perf tools exposes low-level tuning (function costs, thresholds, loop keywords).
Defaults are tuned for typical ELPS workloads. You should NOT need to set this for normal usage.
Only use it for custom cost models (e.g., marking domain-specific functions as expensive).

## Response Metadata
All tool responses include a ` + "`_meta`" + ` object with:
- ` + "`workspace_root`" + `: which workspace was used
- ` + "`elapsed_ms`" + `: time taken in milliseconds
- ` + "`file_count`" + `: number of files processed (where applicable)

## Error Responses
Errors include structured JSON with:
- ` + "`code`" + `: machine-readable error code (` + "`file_not_found`" + `, ` + "`parse_error`" + `, ` + "`invalid_position`" + `, ` + "`workspace_not_configured`" + `, ` + "`invalid_input`" + `)
- ` + "`message`" + `: human-readable description
- ` + "`path`" + `: relevant file path (when applicable)
`

func (s *service) formatTool(_ context.Context, _ *mcp.CallToolRequest, in FormatInput) (*mcp.CallToolResult, FormatResponse, error) {
	start := time.Now()
	if in.Path == "" && in.Content == nil {
		return nil, FormatResponse{}, newToolErr("invalid_input", "path or content is required", "")
	}
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	if err != nil {
		return nil, FormatResponse{}, newToolErr("invalid_input", err.Error(), "")
	}
	path := "<stdin>"
	if in.Path != "" {
		resolved, resolveErr := s.resolvePath(in.Path, root)
		if resolveErr != nil {
			return nil, FormatResponse{}, newToolErr("invalid_input", resolveErr.Error(), in.Path)
		}
		path = resolved
	}
	var source []byte
	if in.Content != nil {
		source = []byte(*in.Content)
	} else {
		source, err = os.ReadFile(path) //nolint:gosec // tool reads user-selected paths
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return nil, FormatResponse{}, newToolErr("file_not_found", fmt.Sprintf("file not found: %s", path), path)
			}
			return nil, FormatResponse{}, newToolErr("invalid_input", err.Error(), path)
		}
	}
	var cfg *formatter.Config
	if in.IndentSize > 0 {
		cfg = formatter.DefaultConfig()
		cfg.IndentSize = in.IndentSize
	}
	formatted, err := formatter.FormatFile(source, path, cfg)
	if err != nil {
		return nil, FormatResponse{}, newToolErr("parse_error", err.Error(), path)
	}
	changed := string(formatted) != string(source)
	resp := FormatResponse{
		Changed: changed,
		Meta:    makeMeta(root, time.Since(start), 1),
	}
	// Only include formatted content when it actually changed and
	// check_only is not set. Avoids sending 100K+ of unchanged content.
	if changed && !in.CheckOnly {
		resp.Formatted = string(formatted)
	}
	return nil, resp, nil
}

func (s *service) lintTool(_ context.Context, _ *mcp.CallToolRequest, in LintInput) (*mcp.CallToolResult, LintResponse, error) {
	start := time.Now()
	if in.Path == "" && in.Content == nil {
		return nil, LintResponse{}, newToolErr("invalid_input", "path or content is required", "")
	}
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	if err != nil {
		return nil, LintResponse{}, newToolErr("invalid_input", err.Error(), "")
	}
	path := "<stdin>"
	if in.Path != "" {
		resolved, resolveErr := s.resolvePath(in.Path, root)
		if resolveErr != nil {
			return nil, LintResponse{}, newToolErr("invalid_input", resolveErr.Error(), in.Path)
		}
		path = resolved
	}
	var source []byte
	if in.Content != nil {
		source = []byte(*in.Content)
	} else {
		source, err = os.ReadFile(path) //nolint:gosec // tool reads user-selected paths
		if err != nil {
			if errors.Is(err, os.ErrNotExist) {
				return nil, LintResponse{}, newToolErr("file_not_found", fmt.Sprintf("file not found: %s", path), path)
			}
			return nil, LintResponse{}, newToolErr("invalid_input", err.Error(), path)
		}
	}

	linter := s.linter
	if len(in.Checks) > 0 {
		linter = filterLinter(s.linter, in.Checks)
	}

	// Parse first to collect parse errors.
	scanner := token.NewScanner(path, strings.NewReader(string(source)))
	parsed := rdparser.New(scanner).ParseProgramFaultTolerant()

	diags := make([]Diagnostic, 0)
	for _, parseErr := range parsed.Errors {
		diags = append(diags, parseDiagnostic(parseErr, path))
	}

	var semantics *analysis.Result
	if root != "" && parsed.Exprs != nil {
		lintCfg := &lint.LintConfig{Workspace: root}
		if analysisCfg, buildErr := lint.BuildAnalysisConfig(lintCfg); buildErr == nil {
			semantics = analysis.Analyze(parsed.Exprs, analysisCfg)
		}
	}
	lintDiags, _ := linter.LintFileWithContext(source, path, semantics)

	for _, d := range lintDiags {
		diags = append(diags, lintDiagnostic(d))
	}
	diags = filterDiagnosticsBySeverity(diags, in.Severity)
	sort.Slice(diags, func(i, j int) bool { return compareRange(diags[i].Range, diags[j].Range) })

	resp := LintResponse{
		Diagnostics: diags,
		Meta:        makeMeta(root, time.Since(start), 1),
	}
	if in.Offset > 0 || in.Limit > 0 {
		total := len(resp.Diagnostics)
		resp.Diagnostics = paginateSlice(resp.Diagnostics, in.Offset, in.Limit)
		if len(resp.Diagnostics) < total {
			resp.Truncated = true
			resp.Total = total
		}
	}
	return nil, resp, nil
}

func filterLinter(original *lint.Linter, checks []string) *lint.Linter {
	selected := make(map[string]bool, len(checks))
	for _, name := range checks {
		selected[strings.TrimSpace(name)] = true
	}
	var filtered []*lint.Analyzer
	for _, a := range original.Analyzers {
		if selected[a.Name] {
			filtered = append(filtered, a)
		}
	}
	return &lint.Linter{Analyzers: filtered}
}

func newToolErr(code, message, path string) error {
	return &toolErr{Code: code, Message: message, Path: path}
}

type toolErr struct {
	Code    string
	Message string
	Path    string
}

func (e *toolErr) Error() string {
	if e.Path != "" {
		return fmt.Sprintf("%s: %s (%s)", e.Code, e.Message, e.Path)
	}
	return fmt.Sprintf("%s: %s", e.Code, e.Message)
}

func isInPackageExpr(expr *lisp.LVal) bool {
	if expr.Type != lisp.LSExpr || len(expr.Cells) == 0 {
		return false
	}
	head := expr.Cells[0]
	return head.Type == lisp.LSymbol && head.Str == "in-package"
}

func autoImportTesting(env *lisp.LEnv) {
	env.Eval(lisp.SExpr([]*lisp.LVal{
		lisp.Symbol("use-package"),
		lisp.String("testing"),
	}))
}

func lvalErrorString(v *lisp.LVal) string {
	if v == nil {
		return "unknown error"
	}
	if err := lisp.GoError(v); err != nil {
		return err.Error()
	}
	return v.Str
}

func makeMeta(workspaceRoot string, elapsed time.Duration, fileCount int) *ResponseMeta {
	return &ResponseMeta{
		WorkspaceRoot: workspaceRoot,
		ElapsedMs:     elapsed.Milliseconds(),
		FileCount:     fileCount,
	}
}

