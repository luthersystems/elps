// Copyright © 2024 The ELPS authors

package analysis

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"sync"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Default limits for workspace scanning.
const (
	// DefaultMaxWorkspaceFiles limits the number of .lisp files scanned.
	DefaultMaxWorkspaceFiles = 5000
	// DefaultMaxFileBytes limits individual file size (5 MB).
	DefaultMaxFileBytes = 5 * 1024 * 1024
)

// ScanConfig controls workspace file collection limits.
type ScanConfig struct {
	// MaxFiles is the maximum number of .lisp files to collect.
	// 0 means use DefaultMaxWorkspaceFiles.
	MaxFiles int
	// MaxFileBytes is the maximum size in bytes for a single file.
	// Files exceeding this are skipped. 0 means use DefaultMaxFileBytes.
	MaxFileBytes int64
	// Excludes are glob patterns for files to skip during collection.
	// Patterns are matched against the full path, base name, and each
	// directory component using filepath.Match semantics.
	Excludes []string
}

// effectiveMaxFiles returns the file limit, applying the default if zero.
func (c *ScanConfig) effectiveMaxFiles() int {
	if c == nil || c.MaxFiles <= 0 {
		return DefaultMaxWorkspaceFiles
	}
	return c.MaxFiles
}

// effectiveMaxFileBytes returns the file size limit, applying the default if zero.
func (c *ScanConfig) effectiveMaxFileBytes() int64 {
	if c == nil || c.MaxFileBytes <= 0 {
		return DefaultMaxFileBytes
	}
	return c.MaxFileBytes
}

// effectiveExcludes returns the exclude patterns, or nil if none configured.
func (c *ScanConfig) effectiveExcludes() []string {
	if c == nil {
		return nil
	}
	return c.Excludes
}

// SymbolKey identifies a symbol across files by name and kind.
type SymbolKey struct {
	Package string
	Name    string
	Kind    SymbolKind
}

// String returns a lookup key for use in maps.
func (k SymbolKey) String() string {
	if k.Package != "" {
		return fmt.Sprintf("%s:%s/%s", k.Package, k.Name, k.Kind)
	}
	return fmt.Sprintf("%s/%s", k.Name, k.Kind)
}

// FileReference represents a cross-file reference to a symbol.
type FileReference struct {
	SymbolKey       SymbolKey
	Source          *token.Location
	File            string          // absolute path
	Enclosing       string          // name of enclosing function ("" if top-level)
	EnclosingSource *token.Location // definition site of enclosing function
	EnclosingKind   SymbolKind      // kind of enclosing function
}

// ScanWorkspace walks a directory tree, parsing all .lisp files and
// extracting exported top-level definitions. The result can be used as
// Config.ExtraGlobals for cross-file symbol resolution.
//
// Files that fail to parse are silently skipped (fault tolerant).
func ScanWorkspace(root string) ([]ExternalSymbol, error) {
	globals, _, err := ScanWorkspaceFull(root)
	return globals, err
}

// ScanWorkspacePackages is like ScanWorkspace but returns a map of
// package name to exported symbols, suitable for use as Config.PackageExports.
func ScanWorkspacePackages(root string) (map[string][]ExternalSymbol, error) {
	_, pkgs, err := ScanWorkspaceFull(root)
	return pkgs, err
}

// ScanWorkspaceDefinitions walks a directory tree and returns all top-level
// definitions, including non-exported symbols. The result is intended for
// workspace symbol search, not cross-file resolution.
func ScanWorkspaceDefinitions(root string) ([]ExternalSymbol, error) {
	_, _, allDefs, err := ScanWorkspaceAll(root)
	return allDefs, err
}

// ScanWorkspaceFull walks a directory tree in a single pass, parsing all
// .lisp files and extracting both global symbols and package exports.
// It skips hidden directories (names starting with '.'), node_modules,
// and vendor directories. Stops collecting after maxWorkspaceFiles.
// Parsing is done concurrently using a bounded worker pool.
//
// Files that fail to parse are silently skipped (fault tolerant).
func ScanWorkspaceFull(root string) ([]ExternalSymbol, map[string][]ExternalSymbol, error) {
	globals, pkgs, _, err := ScanWorkspaceAll(root)
	return globals, pkgs, err
}

// ScanWorkspaceAll combines ScanWorkspaceFull and ScanWorkspaceDefinitions
// into a single pass: each file is parsed once and all three results are
// extracted from the same AST.
func ScanWorkspaceAll(root string) (globals []ExternalSymbol, pkgs map[string][]ExternalSymbol, allDefs []ExternalSymbol, err error) {
	globals, pkgs, allDefs, _, err = ScanWorkspaceAllWithConfig(root, nil)
	return
}

// ScanWorkspaceAllWithConfig is like ScanWorkspaceAll but accepts a
// ScanConfig for configurable limits. It also returns whether the file
// list was truncated due to hitting the MaxFiles limit.
//
// Delegates to PrescanWorkspace internally to avoid duplicating the
// concurrent worker pool logic.
func ScanWorkspaceAllWithConfig(root string, scanCfg *ScanConfig) (globals []ExternalSymbol, pkgs map[string][]ExternalSymbol, allDefs []ExternalSymbol, truncated bool, err error) {
	prescan, err := PrescanWorkspace(root, scanCfg)
	if err != nil {
		return nil, nil, nil, false, err
	}
	return prescan.ExportedGlobals, prescan.PkgExports, prescan.AllDefs, prescan.Truncated, nil
}

// collectLispFilesWithConfig walks the directory tree and collects .lisp
// file paths, respecting ScanConfig limits. Returns the collected paths,
// whether the MaxFiles limit was reached (truncated), and any walk error.
func collectLispFilesWithConfig(root string, scanCfg *ScanConfig) ([]string, bool, error) {
	maxFiles := scanCfg.effectiveMaxFiles()
	maxBytes := scanCfg.effectiveMaxFileBytes()

	excludes := scanCfg.effectiveExcludes()

	var paths []string
	var truncated bool
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if info.IsDir() {
			if ShouldSkipDir(info.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".lisp" {
			return nil
		}
		if len(excludes) > 0 && MatchesExclude(path, excludes) {
			return nil
		}
		if maxBytes > 0 && info.Size() > maxBytes {
			return nil
		}
		paths = append(paths, path)
		if len(paths) >= maxFiles {
			truncated = true
			return filepath.SkipAll
		}
		return nil
	})
	return paths, truncated, err
}

// ShouldSkipDir returns true for directories that should not be walked.
// It skips hidden directories (e.g. .git, .vscode) and node_modules,
// but not "." or ".." which represent the current/parent directory.
func ShouldSkipDir(name string) bool {
	if name == "." || name == ".." {
		return false
	}
	if len(name) > 0 && name[0] == '.' {
		return true
	}
	if name == "node_modules" || name == "vendor" {
		return true
	}
	return false
}

// scanFileFull parses a file once and extracts exported globals, package-grouped
// exports, and all definitions in a single pass.
func scanFileFull(source []byte, filename string) (globals []ExternalSymbol, pkgs map[string][]ExternalSymbol, allDefs []ExternalSymbol) {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil, nil, nil
	}

	defs := extractDefinitions(exprs)
	exported := scanExportedDefinitionKeys(exprs)

	pkgs = make(map[string][]ExternalSymbol)
	for _, sym := range defs {
		if exported[definitionKey(sym.Package, sym.Name)] {
			globals = append(globals, sym)
			pkgs[sym.Package] = append(pkgs[sym.Package], sym)
		}
	}
	return globals, pkgs, defs
}

// extractDefinitions collects top-level definitions from pre-parsed expressions.
func extractDefinitions(exprs []*lisp.LVal) []ExternalSymbol {
	// Collect top-level definitions keyed by package-qualified name so
	// multi-package files can define the same symbol in different packages.
	defs := make(map[string]*ExternalSymbol)
	currentPkg := "user" // default package

	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		head := astutil.HeadSymbol(expr)
		switch head {
		case "in-package":
			if name := scanPackageName(expr); name != "" {
				currentPkg = name
			}
		case "defun":
			if sym := scanDefun(expr, SymFunction); sym != nil {
				sym.Package = currentPkg
				defs[definitionKey(sym.Package, sym.Name)] = sym
			}
		case "defmacro":
			if sym := scanDefun(expr, SymMacro); sym != nil {
				sym.Package = currentPkg
				defs[definitionKey(sym.Package, sym.Name)] = sym
			}
		case "deftype":
			if sym := scanDeftype(expr); sym != nil {
				sym.Package = currentPkg
				defs[definitionKey(sym.Package, sym.Name)] = sym
			}
		case "set":
			if sym := scanSet(expr); sym != nil {
				sym.Package = currentPkg
				defs[definitionKey(sym.Package, sym.Name)] = sym
			}
		}
	}

	var result []ExternalSymbol
	for _, sym := range defs {
		result = append(result, *sym)
	}
	return result
}

func definitionKey(pkg, name string) string {
	return pkg + "\x00" + name
}

func scanExportedDefinitionKeys(exprs []*lisp.LVal) map[string]bool {
	exported := make(map[string]bool)
	currentPkg := "user"
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		switch astutil.HeadSymbol(expr) {
		case "in-package":
			if name := scanPackageName(expr); name != "" {
				currentPkg = name
			}
		case "export":
			for _, name := range scanExportNames(expr) {
				exported[definitionKey(currentPkg, name)] = true
			}
		}
	}
	return exported
}

// scanPackageName extracts the package name from an in-package expression.
func scanPackageName(expr *lisp.LVal) string {
	if astutil.ArgCount(expr) < 1 {
		return ""
	}
	return extractPkgNameArg(expr.Cells[1])
}

// extractPkgNameArg gets a package name from a use-package/in-package argument.
var extractPkgNameArg = astutil.PackageNameArg

func scanDefun(expr *lisp.LVal, kind SymbolKind) *ExternalSymbol {
	if astutil.ArgCount(expr) < 2 {
		return nil
	}
	nameVal := expr.Cells[1]
	if nameVal.Type != lisp.LSymbol {
		return nil
	}
	formalsVal := expr.Cells[2]
	if formalsVal.Type != lisp.LSExpr {
		return nil
	}

	// Extract docstring: (defun name (args) "docstring" body...)
	var docStr string
	if astutil.ArgCount(expr) >= 3 && expr.Cells[3].Type == lisp.LString {
		// Only treat as docstring if there's at least one body form after it.
		if astutil.ArgCount(expr) >= 4 {
			docStr = expr.Cells[3].Str
		}
	}

	return &ExternalSymbol{
		Name:      nameVal.Str,
		Kind:      kind,
		Signature: signatureFromFormals(formalsVal),
		Source:    nameVal.Source,
		DocString: docStr,
	}
}

func scanDeftype(expr *lisp.LVal) *ExternalSymbol {
	// (deftype name (constructor-formals) body...)
	if astutil.ArgCount(expr) < 1 {
		return nil
	}
	nameVal := expr.Cells[1]
	if nameVal.Type != lisp.LSymbol {
		return nil
	}
	return &ExternalSymbol{
		Name:   nameVal.Str,
		Kind:   SymType,
		Source: nameVal.Source,
	}
}

func scanSet(expr *lisp.LVal) *ExternalSymbol {
	if astutil.ArgCount(expr) < 1 {
		return nil
	}
	arg := expr.Cells[1]
	name := ""
	if arg.Type == lisp.LSymbol {
		name = arg.Str
	} else if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		name = arg.Cells[0].Str
	}
	if name == "" {
		return nil
	}
	return &ExternalSymbol{
		Name:   name,
		Kind:   SymVariable,
		Source: arg.Source,
	}
}

func scanExportNames(expr *lisp.LVal) []string {
	var names []string
	for _, arg := range expr.Cells[1:] {
		name := ""
		if arg.Type == lisp.LSymbol {
			name = arg.Str
		} else if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
			name = arg.Cells[0].Str
		}
		if name != "" {
			names = append(names, name)
		}
	}
	return names
}

// AnalyzeFile parses and performs full semantic analysis on a single file.
// Returns nil if the file fails to parse.
func AnalyzeFile(source []byte, filename string, cfg *Config) *Result {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil
	}

	fileCfg := cfg
	if fileCfg == nil {
		fileCfg = &Config{}
	}
	fileCfg = &Config{
		ExtraGlobals:   fileCfg.ExtraGlobals,
		PackageExports: fileCfg.PackageExports,
		Filename:       filename,
	}
	return Analyze(exprs, fileCfg)
}

// ExtractFileRefs extracts cross-file-trackable references from an
// analysis result. Only references to global-scope, non-builtin symbols
// are included (builtins, special ops, parameters, and locals are skipped).
func ExtractFileRefs(result *Result, filePath string) []FileReference {
	if result == nil {
		return nil
	}

	var refs []FileReference
	for _, ref := range result.References {
		if ref.Symbol == nil || ref.Source == nil {
			continue
		}
		// Skip builtins, special ops, parameters, and locals.
		if !isGlobalUserSymbol(ref.Symbol) {
			continue
		}

		key := SymbolKey{
			Package: ref.Symbol.Package,
			Name:    ref.Symbol.Name,
			Kind:    ref.Symbol.Kind,
		}
		fref := FileReference{
			SymbolKey: key,
			Source:    ref.Source,
			File:      filePath,
		}

		// Find enclosing function for call hierarchy support.
		enclosing := FindEnclosingFunction(result.RootScope, ref.Source.Line, ref.Source.Col)
		if enclosing != nil {
			fref.Enclosing = enclosing.Name
			fref.EnclosingSource = enclosing.Source
			fref.EnclosingKind = enclosing.Kind
		}

		refs = append(refs, fref)
	}
	return refs
}

// isGlobalUserSymbol returns true if a symbol is a user-defined global
// (not a builtin, special op, parameter, or lambda-scoped local).
func isGlobalUserSymbol(sym *Symbol) bool {
	switch sym.Kind {
	case SymBuiltin, SymSpecialOp, SymParameter:
		return false
	}
	// Skip symbols without a real source location (builtins registered
	// in populateBuiltins have nil Source).
	if sym.Source == nil || sym.Source.Pos < 0 {
		return false
	}
	// Only include symbols defined at global or function scope.
	if sym.Scope != nil {
		switch sym.Scope.Kind {
		case ScopeGlobal, ScopeFunction:
			return true
		}
		return false
	}
	return true
}

// FindEnclosingFunction finds the function symbol that contains the given
// 1-based position by walking the scope tree.
func FindEnclosingFunction(root *Scope, line, col int) *Symbol {
	scope := ScopeAtPosition(root, line, col)
	for scope != nil {
		if scope.Kind == ScopeFunction {
			if scope.Node != nil && scope.Node.Source != nil && scope.Parent != nil {
				nodeLoc := scope.Node.Source
				var found *Symbol
				scope.Parent.forEachSymbol(func(sym *Symbol) bool {
					if sym.Kind != SymFunction && sym.Kind != SymMacro {
						return true
					}
					// Skip external (imported) symbols — they belong to
					// other files and may coincidentally share a line number.
					if sym.External {
						return true
					}
					if sym.Source != nil && sym.Source.Line == nodeLoc.Line &&
						sym.Source.File == nodeLoc.File {
						found = sym
						return false
					}
					return true
				})
				if found != nil {
					return found
				}
			}
		}
		scope = scope.Parent
	}
	return nil
}

// forEachSymbol iterates over all unique symbols in this scope (both bare and
// package-qualified) without allocating a slice. The callback receives each
// symbol exactly once; return false to stop early.
func (s *Scope) forEachSymbol(fn func(*Symbol) bool) {
	if s == nil {
		return
	}
	seen := make(map[*Symbol]bool, len(s.Symbols)+len(s.PackageSymbols))
	for _, sym := range s.Symbols {
		if sym == nil || seen[sym] {
			continue
		}
		seen[sym] = true
		if !fn(sym) {
			return
		}
	}
	for _, sym := range s.PackageSymbols {
		if sym == nil || seen[sym] {
			continue
		}
		seen[sym] = true
		if !fn(sym) {
			return
		}
	}
}

// ScopeAtPosition returns the innermost scope that contains the given
// 1-based ELPS line and column. It walks the scope tree depth-first.
func ScopeAtPosition(root *Scope, line, col int) *Scope {
	if root == nil {
		return nil
	}
	best := root
	for _, child := range root.Children {
		if s := scopeContainingAnalysis(child, line, col); s != nil {
			best = s
		}
	}
	return best
}

// scopeContainingAnalysis checks if a scope's node contains the position
// and recursively checks children for the most specific match.
func scopeContainingAnalysis(scope *Scope, line, col int) *Scope {
	if scope.Node == nil || scope.Node.Source == nil {
		return nil
	}
	loc := scope.Node.Source
	if loc.Line == 0 {
		return nil
	}
	startLine := loc.Line
	endLine := loc.EndLine
	if endLine == 0 {
		endLine = startLine + 1000
	}
	if line < startLine || line > endLine {
		return nil
	}
	if line == startLine && col < loc.Col {
		return nil
	}
	if line == endLine && loc.EndCol > 0 && col >= loc.EndCol {
		return nil
	}
	best := scope
	for _, child := range scope.Children {
		if s := scopeContainingAnalysis(child, line, col); s != nil {
			best = s
		}
	}
	return best
}

// ScanWorkspaceRefs walks the workspace directory tree, performs full
// analysis on each .lisp file, and extracts cross-file references.
// The cfg should have ExtraGlobals and PackageExports populated from
// a prior ScanWorkspaceFull call. Parsing is done concurrently.
// The optional scanCfg controls file collection limits and excludes.
//
// Returns a map from SymbolKey.String() to FileReference slices.
func ScanWorkspaceRefs(root string, cfg *Config, scanCfg *ScanConfig) map[string][]FileReference {
	paths, _, err := collectLispFilesWithConfig(root, scanCfg)
	if err != nil || len(paths) == 0 {
		return nil
	}

	type fileResult struct {
		refs []FileReference
	}

	results := make([]fileResult, len(paths))
	numWorkers := runtime.NumCPU()
	if numWorkers > len(paths) {
		numWorkers = len(paths)
	}
	if numWorkers < 1 {
		numWorkers = 1
	}

	var wg sync.WaitGroup
	work := make(chan int, len(paths))
	for i := range paths {
		work <- i
	}
	close(work)

	wg.Add(numWorkers)
	for range numWorkers {
		go func() {
			defer wg.Done()
			for i := range work {
				fileSrc, readErr := os.ReadFile(paths[i]) //nolint:gosec // CLI tool reads user-specified files
				if readErr != nil {
					continue
				}
				result := AnalyzeFile(fileSrc, paths[i], cfg)
				if result == nil {
					continue
				}
				absPath := paths[i]
				if !filepath.IsAbs(absPath) {
					if abs, absErr := filepath.Abs(absPath); absErr == nil {
						absPath = abs
					}
				}
				results[i] = fileResult{refs: ExtractFileRefs(result, absPath)}
			}
		}()
	}
	wg.Wait()

	// Merge results.
	refs := make(map[string][]FileReference)
	for _, r := range results {
		for i := range r.refs {
			key := r.refs[i].SymbolKey.String()
			refs[key] = append(refs[key], r.refs[i])
		}
	}
	return refs
}

// WorkspacePrescan holds the results of a workspace prescan: file definitions,
// package exports, and DefFormSpecs extracted from defmacro definitions.
type WorkspacePrescan struct {
	// Files is the list of .lisp file paths that were scanned.
	Files []string
	// Truncated is true if the file limit was reached during collection.
	Truncated bool
	// ExportedGlobals are exported top-level definitions only.
	// Used by ScanWorkspaceAllWithConfig for backwards compatibility.
	ExportedGlobals []ExternalSymbol
	// PkgExports maps package name to exported symbols.
	PkgExports map[string][]ExternalSymbol
	// AllDefs are all definitions (exported and non-exported).
	// Typically used as Config.ExtraGlobals for cross-file resolution.
	AllDefs []ExternalSymbol
	// DefForms are DefFormSpecs derived from defmacro definitions whose
	// names start with "def". These can be injected into Config.DefForms
	// for per-file analysis.
	DefForms []DefFormSpec
}

// PrescanWorkspace performs a single-pass workspace scan that collects
// file definitions and extracts DefFormSpecs from def*-prefixed macros.
// The result provides everything needed to build a Config for per-file
// analysis, including macro-derived definition forms.
func PrescanWorkspace(root string, scanCfg *ScanConfig) (*WorkspacePrescan, error) {
	paths, truncated, err := collectLispFilesWithConfig(root, scanCfg)
	if err != nil {
		return nil, err
	}

	type fileResult struct {
		globals  []ExternalSymbol
		pkgs     map[string][]ExternalSymbol
		allDefs  []ExternalSymbol
		defForms []DefFormSpec
	}

	results := make([]fileResult, len(paths))
	numWorkers := runtime.NumCPU()
	if numWorkers > len(paths) {
		numWorkers = len(paths)
	}
	if numWorkers < 1 {
		numWorkers = 1
	}

	var wg sync.WaitGroup
	work := make(chan int, len(paths))
	for i := range paths {
		work <- i
	}
	close(work)

	wg.Add(numWorkers)
	for range numWorkers {
		go func() {
			defer wg.Done()
			for i := range work {
				fileSrc, readErr := os.ReadFile(paths[i]) //nolint:gosec // CLI tool reads user-specified files
				if readErr != nil {
					continue
				}
				g, p, d := scanFileFull(fileSrc, paths[i])
				df := extractDefFormSpecs(d)
				results[i] = fileResult{globals: g, pkgs: p, allDefs: d, defForms: df}
			}
		}()
	}
	wg.Wait()

	prescan := &WorkspacePrescan{
		Files:      paths,
		Truncated:  truncated,
		PkgExports: make(map[string][]ExternalSymbol),
	}
	for _, r := range results {
		prescan.ExportedGlobals = append(prescan.ExportedGlobals, r.globals...)
		prescan.AllDefs = append(prescan.AllDefs, r.allDefs...)
		prescan.DefForms = append(prescan.DefForms, r.defForms...)
		for pkg, syms := range r.pkgs {
			prescan.PkgExports[pkg] = append(prescan.PkgExports[pkg], syms...)
		}
	}
	return prescan, nil
}

// extractDefFormSpecs derives DefFormSpecs from defmacro definitions whose
// names start with "def". This enables the analyzer to recognize these macros
// as definition forms in other files during per-file analysis.
func extractDefFormSpecs(defs []ExternalSymbol) []DefFormSpec {
	var specs []DefFormSpec
	for _, sym := range defs {
		if sym.Kind != SymMacro || !strings.HasPrefix(sym.Name, "def") {
			continue
		}
		if sym.Signature == nil {
			continue
		}
		// Heuristic: the macro defines a name at index 1 and has formals
		// at the first parameter-list-like position. For macros like
		// (defmacro def-handler (name routes) ...), the formals for the
		// generated function are typically in the second argument position.
		formalsIdx := 2 // default: (def-xxx name (formals) body...)
		if sym.Signature.MinArity() < 2 {
			formalsIdx = -1 // can't have both name and formals
		}
		if formalsIdx < 0 {
			continue
		}
		specs = append(specs, DefFormSpec{
			Head:         sym.Name,
			FormalsIndex: formalsIdx,
			BindsName:    true,
			NameIndex:    1,
			NameKind:     SymFunction,
		})
	}
	return specs
}

// ExtractFileDefinitions parses source and returns all top-level definitions
// from a single file. This is the public counterpart of extractDefinitions,
// suitable for incremental workspace index updates.
func ExtractFileDefinitions(source []byte, filename string) []ExternalSymbol {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil
	}

	return extractDefinitions(exprs)
}

// SymbolToKey derives a SymbolKey from an analysis Symbol.
func SymbolToKey(sym *Symbol) SymbolKey {
	kind := sym.Kind
	return SymbolKey{Package: sym.Package, Name: sym.Name, Kind: kind}
}

// SymbolKeyFromNameKind creates a SymbolKey from raw name and kind strings.
// The kind string should match SymbolKind.String() output.
func SymbolKeyFromNameKind(name string, kind string) SymbolKey {
	var k SymbolKind
	switch strings.ToLower(kind) {
	case "function":
		k = SymFunction
	case "macro":
		k = SymMacro
	case "variable":
		k = SymVariable
	case "type":
		k = SymType
	default:
		k = SymVariable
	}
	return SymbolKey{Name: name, Kind: k}
}
