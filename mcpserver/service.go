package mcpserver

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/analysis/perf"
	"github.com/luthersystems/elps/lint"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/modelcontextprotocol/go-sdk/mcp"
)

const builtinScheme = "elps-builtin"

type serviceConfig struct {
	registry      *lisp.PackageRegistry
	env           *lisp.LEnv
	workspaceRoot string
	perfConfig    *perf.Config
	linter        *lint.Linter
}

type service struct {
	registry      *lisp.PackageRegistry
	env           *lisp.LEnv
	workspaceRoot string
	perfConfig    *perf.Config
	linter        *lint.Linter

	mu         sync.RWMutex
	workspaces map[string]*workspaceState
}

type workspaceState struct {
	cfg  *analysis.Config
	refs map[string][]analysis.FileReference
}

type document struct {
	Path        string
	Content     string
	Analysis    *analysis.Result
	ParseErrors []error
}

func newService(cfg serviceConfig) *service {
	return &service{
		registry:      cfg.registry,
		env:           cfg.env,
		workspaceRoot: cfg.workspaceRoot,
		perfConfig:    clonePerfConfig(cfg.perfConfig),
		linter:        cfg.linter,
		workspaces:    make(map[string]*workspaceState),
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
			return nil, DefinitionResponse{Found: true, Location: builtinLocationForWord(word, sym.Name)}, nil
		}
		if loc := qualifiedSymbolDefinition(state, word); loc != nil {
			return nil, DefinitionResponse{Found: true, Location: loc}, nil
		}
		return nil, DefinitionResponse{Found: false}, nil
	}

	if sym.Source.Pos < 0 {
		return nil, DefinitionResponse{Found: true, Location: builtinLocationForWord(word, sym.Name)}, nil
	}

	loc := locationFromSource(resolvePathAgainstRoot(sym.Source.File, s.rootForState(in.WorkspaceRoot, state), doc.Path), sym.Source, len(sym.Name))
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

	var refs []Location
	if in.IncludeDeclaration && sym.Source != nil && sym.Source.Pos >= 0 {
		refs = append(refs, *locationFromSource(resolvePathAgainstRoot(sym.Source.File, s.rootForState(in.WorkspaceRoot, state), doc.Path), sym.Source, len(sym.Name)))
	}

	for _, ref := range doc.Analysis.References {
		if ref.Symbol != sym || ref.Source == nil {
			continue
		}
		refs = append(refs, *locationFromSource(resolvePathAgainstRoot(ref.Source.File, s.rootForState(in.WorkspaceRoot, state), doc.Path), ref.Source, len(sym.Name)))
	}

	for _, wref := range getWorkspaceRefs(state, analysis.SymbolToKey(sym).String(), doc.Path) {
		refs = append(refs, *locationFromSource(wref.File, wref.Source, len(sym.Name)))
	}

	return nil, ReferencesResponse{
		SymbolName: sym.Name,
		References: refs,
	}, nil
}

func (s *service) documentSymbolsTool(ctx context.Context, _ *mcp.CallToolRequest, in DocumentQueryInput) (*mcp.CallToolResult, DocumentSymbolsResponse, error) {
	doc, _, err := s.loadDocument(in.Path, in.Content, in.WorkspaceRoot)
	if err != nil {
		return nil, DocumentSymbolsResponse{}, err
	}
	var symbols []DocumentSymbol
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
	return nil, DocumentSymbolsResponse{Symbols: symbols}, nil
}

func (s *service) workspaceSymbolsTool(ctx context.Context, _ *mcp.CallToolRequest, in WorkspaceSymbolsInput) (*mcp.CallToolResult, WorkspaceSymbolsResponse, error) {
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, true)
	if err != nil {
		return nil, WorkspaceSymbolsResponse{}, err
	}
	state, err := s.workspace(root)
	if err != nil {
		return nil, WorkspaceSymbolsResponse{}, err
	}
	query := strings.ToLower(in.Query)
	var out []WorkspaceSymbol
	seen := make(map[string]bool)
	for _, sym := range state.cfg.ExtraGlobals {
		if !matchesQuery(sym.Name, query) || sym.Source == nil || sym.Source.Line == 0 {
			continue
		}
		entry := WorkspaceSymbol{
			Name:  sym.Name,
			Kind:  symbolKindLabel(sym.Kind),
			Path:  sym.Source.File,
			Range: rangeFromSource(sym.Source, len(sym.Name)),
		}
		if !seenWorkspaceSymbol(seen, entry) {
			out = append(out, entry)
		}
	}
	for pkgName, syms := range state.cfg.PackageExports {
		for _, sym := range syms {
			if sym.Source == nil || sym.Source.Line == 0 {
				continue
			}
			if !matchesQuery(sym.Name, query) && !matchesQuery(pkgName+":"+sym.Name, query) {
				continue
			}
			entry := WorkspaceSymbol{
				Name:    sym.Name,
				Kind:    symbolKindLabel(sym.Kind),
				Package: pkgName,
				Path:    sym.Source.File,
				Range:   rangeFromSource(sym.Source, len(sym.Name)),
			}
			if !seenWorkspaceSymbol(seen, entry) {
				out = append(out, entry)
			}
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
	return nil, WorkspaceSymbolsResponse{Symbols: out}, nil
}

func (s *service) diagnosticsTool(ctx context.Context, _ *mcp.CallToolRequest, in DiagnosticsInput) (*mcp.CallToolResult, DiagnosticsResponse, error) {
	if in.IncludeWorkspace {
		root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, true)
		if err != nil {
			return nil, DiagnosticsResponse{}, err
		}
		files, err := listWorkspaceFiles(root, true)
		if err != nil {
			return nil, DiagnosticsResponse{}, err
		}
		contentByPath := make(map[string]*string)
		if in.Path != nil && in.Content != nil {
			resolved, resolveErr := s.resolvePath(*in.Path, root)
			if resolveErr != nil {
				return nil, DiagnosticsResponse{}, resolveErr
			}
			contentByPath[resolved] = in.Content
		}
		var result []FileDiagnostics
		for _, path := range files {
			fd, diagErr := s.collectFileDiagnostics(path, contentByPath[path], &root)
			if diagErr != nil {
				return nil, DiagnosticsResponse{}, diagErr
			}
			result = append(result, fd)
		}
		return nil, DiagnosticsResponse{Files: result}, nil
	}

	if in.Path == nil {
		return nil, DiagnosticsResponse{}, errors.New("path is required when include_workspace is false")
	}
	root, err := s.resolveWorkspaceRoot(in.WorkspaceRoot, false)
	if err != nil {
		return nil, DiagnosticsResponse{}, err
	}
	fd, err := s.collectFileDiagnostics(*in.Path, in.Content, optionalStringPtr(root))
	if err != nil {
		return nil, DiagnosticsResponse{}, err
	}
	return nil, DiagnosticsResponse{Files: []FileDiagnostics{fd}}, nil
}

func (s *service) perfIssuesTool(ctx context.Context, _ *mcp.CallToolRequest, in PerfSelectionInput) (*mcp.CallToolResult, PerfIssuesResponse, error) {
	result, err := s.runPerf(in)
	if err != nil {
		return nil, PerfIssuesResponse{}, err
	}
	out := PerfIssuesResponse{Issues: mapIssues(result.Issues)}
	if in.Top > 0 {
		out.Solved = topSolved(result.Solved, in.Top)
	}
	return nil, out, nil
}

func (s *service) callGraphTool(ctx context.Context, _ *mcp.CallToolRequest, in PerfSelectionInput) (*mcp.CallToolResult, CallGraphResponse, error) {
	result, err := s.runPerf(in)
	if err != nil {
		return nil, CallGraphResponse{}, err
	}
	return nil, mapCallGraph(result.Graph), nil
}

func (s *service) hotspotsTool(ctx context.Context, _ *mcp.CallToolRequest, in PerfSelectionInput) (*mcp.CallToolResult, HotspotsResponse, error) {
	if in.Top <= 0 {
		return nil, HotspotsResponse{}, errors.New("top must be greater than zero")
	}
	result, err := s.runPerf(in)
	if err != nil {
		return nil, HotspotsResponse{}, err
	}
	return nil, HotspotsResponse{Functions: topSolved(result.Solved, in.Top)}, nil
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
	return perf.AnalyzeFiles(files, cfg)
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
		files := make([]string, 0, len(in.Paths))
		for _, path := range in.Paths {
			resolved, resolveErr := s.resolvePath(path, root)
			if resolveErr != nil {
				return nil, resolveErr
			}
			files = append(files, resolved)
		}
		sort.Strings(files)
		return files, nil
	}
	return listWorkspaceFiles(root, in.IncludeTests)
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
	if path == "" {
		return nil, nil, errors.New("path is required")
	}
	root, err := s.resolveWorkspaceRoot(workspaceRoot, false)
	if err != nil {
		return nil, nil, err
	}
	resolvedPath, err := s.resolvePath(path, root)
	if err != nil {
		return nil, nil, err
	}
	source, contentString, err := s.readSource(resolvedPath, content)
	if err != nil {
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
	}
	var result *analysis.Result
	if parsed.Exprs != nil {
		result = analysis.Analyze(parsed.Exprs, cfg)
	}

	_ = source
	return &document{
		Path:        resolvedPath,
		Content:     contentString,
		Analysis:    result,
		ParseErrors: parsed.Errors,
	}, state, nil
}

func (s *service) workspace(root string) (*workspaceState, error) {
	state, err := s.buildWorkspaceState(root)
	if err != nil {
		return nil, err
	}
	s.mu.Lock()
	s.workspaces[root] = state
	s.mu.Unlock()
	return state, nil
}

func (s *service) buildWorkspaceState(root string) (*workspaceState, error) {
	state := &workspaceState{cfg: &analysis.Config{}}
	if root != "" {
		globals, pkgs, err := analysis.ScanWorkspaceFull(root)
		if err != nil {
			return nil, err
		}
		state.cfg.ExtraGlobals = globals
		state.cfg.PackageExports = pkgs
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
				state.cfg.PackageExports[pkgName] = append(state.cfg.PackageExports[pkgName], syms...)
			}
		}
	}
	for pkgName, syms := range state.cfg.PackageExports {
		state.cfg.PackageExports[pkgName] = deduplicateExports(syms)
	}
	if root != "" {
		state.refs = analysis.ScanWorkspaceRefs(root, state.cfg)
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
	if filepath.IsAbs(path) {
		return filepath.Clean(path), nil
	}
	if root != "" {
		return filepath.Join(root, path), nil
	}
	return filepath.Abs(path)
}

func (s *service) rootForState(workspaceRoot *string, _ *workspaceState) string {
	root, _ := s.resolveWorkspaceRoot(workspaceRoot, false)
	return root
}

func validateCursor(line, character int) error {
	if line < 0 {
		return errors.New("line must be non-negative")
	}
	if character < 0 {
		return errors.New("character must be non-negative")
	}
	return nil
}

func listWorkspaceFiles(root string, includeTests bool) ([]string, error) {
	var paths []string
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if info.IsDir() {
			if analysis.ShouldSkipDir(info.Name()) {
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
		paths = append(paths, path)
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Strings(paths)
	return paths, nil
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
	key := fmt.Sprintf("%s|%s|%s|%d|%d|%d|%d",
		symbol.Name,
		symbol.Path,
		symbol.Kind,
		symbol.Range.Start.Line,
		symbol.Range.Start.Character,
		symbol.Range.End.Line,
		symbol.Range.End.Character,
	)
	if seen[key] {
		return true
	}
	seen[key] = true
	return false
}

func deduplicateExports(syms []analysis.ExternalSymbol) []analysis.ExternalSymbol {
	seen := make(map[string]int, len(syms))
	var result []analysis.ExternalSymbol
	for _, sym := range syms {
		if idx, ok := seen[sym.Name]; ok {
			result[idx] = sym
			continue
		}
		seen[sym.Name] = len(result)
		result = append(result, sym)
	}
	return result
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
		return builtinLocation("lisp", sym.Name)
	}
	if sym.Source == nil {
		return nil
	}
	path := sym.Source.File
	if path == "" {
		path = currentPath
	}
	_ = state
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

func builtinLocationForWord(word, symName string) *Location {
	pkg := "lisp"
	if pkgName, _, ok := splitPackageQualified(word); ok && pkgName != "" {
		pkg = pkgName
	}
	return builtinLocation(pkg, symName)
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
		return nil
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

func max(v, floor int) int {
	if v < floor {
		return floor
	}
	return v
}
