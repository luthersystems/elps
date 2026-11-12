// Copyright © 2024 The ELPS authors

package analysis

import (
	"bytes"
	"cmp"
	"fmt"
	"maps"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"slices"
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
	// IncludeDirs are directory names that override ShouldSkipDir.
	// If a directory name matches any entry, it will be walked even if
	// ShouldSkipDir would normally skip it (e.g. "_examples").
	IncludeDirs []string
}

// effectiveMaxFiles returns the file limit, applying the default if zero.
func (c *ScanConfig) effectiveMaxFiles() int {
	if c == nil || c.MaxFiles <= 0 {
		return DefaultMaxWorkspaceFiles
	}
	return c.MaxFiles
}

// EffectiveMaxFileBytes returns the per-file size limit the scan applies,
// with the default filled in when MaxFileBytes is unset. Exported so that
// callers refreshing the scan's tables one file at a time (the LSP's didSave
// path) skip exactly the files the scan would have skipped.
func (c *ScanConfig) EffectiveMaxFileBytes() int64 {
	return c.effectiveMaxFileBytes()
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

// effectiveIncludeDirs returns the directory include list, or nil if none configured.
func (c *ScanConfig) effectiveIncludeDirs() []string {
	if c == nil {
		return nil
	}
	return c.IncludeDirs
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

	// Export marks the name in an export form: (export 'f) gives one for
	// f. It is a place the name is written, so a rename must edit it and
	// a reference search lists it. It is not a use and not a call. A
	// reader that counts uses (the unused-function lint check) or calls
	// (the call hierarchy) must skip an entry with Export set.
	Export bool
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
// It skips directories matched by ShouldSkipDir (hidden, underscore-prefixed,
// node_modules, vendor, build). Stops collecting after maxWorkspaceFiles.
// Parsing is done concurrently using a bounded worker pool.
//
// Files that fail to parse are silently skipped (fault tolerant).
//
//nolint:revive // exported API; changing it breaks embedders
func ScanWorkspaceFull(root string) ([]ExternalSymbol, map[string][]ExternalSymbol, error) {
	globals, pkgs, _, err := ScanWorkspaceAll(root)
	return globals, pkgs, err
}

// ScanWorkspaceAll combines ScanWorkspaceFull and ScanWorkspaceDefinitions
// into a single pass: each file is parsed once and all three results are
// extracted from the same AST.
//
//nolint:revive // exported API; changing it breaks embedders
func ScanWorkspaceAll(root string) ([]ExternalSymbol, map[string][]ExternalSymbol, []ExternalSymbol, error) {
	var globals []ExternalSymbol
	var pkgs map[string][]ExternalSymbol
	var allDefs []ExternalSymbol
	var err error
	globals, pkgs, allDefs, _, err = ScanWorkspaceAllWithConfig(root, nil)
	return globals, pkgs, allDefs, err
}

// ScanWorkspaceAllWithConfig is like ScanWorkspaceAll but accepts a
// ScanConfig for configurable limits. It also returns whether the file
// list was truncated due to hitting the MaxFiles limit.
//
// Delegates to PrescanWorkspace internally to avoid duplicating the
// concurrent worker pool logic.
//
//nolint:revive // exported API; changing it breaks embedders
func ScanWorkspaceAllWithConfig(root string, scanCfg *ScanConfig) ([]ExternalSymbol, map[string][]ExternalSymbol, []ExternalSymbol, bool, error) {
	var err error
	prescan, err := PrescanWorkspace(root, scanCfg)
	if err != nil {
		return nil, nil, nil, false, err
	}
	return prescan.ExportedGlobals, prescan.PkgExports, prescan.AllDefs, prescan.Truncated, nil
}

// collectedLispFiles holds workspace files and the collection limit status.
type collectedLispFiles struct {
	// paths contains collected file paths.
	paths []string
	// truncated reports that the file limit was reached.
	truncated bool
}

// collectLispFilesWithConfig walks the directory tree and collects .lisp
// file paths, respecting ScanConfig limits. Returns the collected paths,
// whether the MaxFiles limit was reached (truncated), and any walk error.
func collectLispFilesWithConfig(root string, scanCfg *ScanConfig) (collectedLispFiles, error) {
	maxFiles := scanCfg.effectiveMaxFiles()
	maxBytes := scanCfg.effectiveMaxFileBytes()

	excludes := scanCfg.effectiveExcludes()
	includeDirs := scanCfg.effectiveIncludeDirs()

	var paths []string
	var truncated bool
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if info.IsDir() {
			// Never skip the root directory itself — only subdirectories.
			if path != root && ShouldSkipDir(info.Name()) && !matchesInclude(info.Name(), includeDirs) {
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
	return collectedLispFiles{paths: paths, truncated: truncated}, err
}

// ShouldSkipDir returns true for directories that should not be walked.
// It skips hidden directories (e.g. .git, .vscode), underscore-prefixed
// directories (e.g. _archive, _old), and common dependency/build output
// directories — but not "." or ".." which represent the current/parent
// directory.
func ShouldSkipDir(name string) bool {
	if name == "." || name == ".." {
		return false
	}
	if len(name) > 0 && name[0] == '.' {
		return true
	}
	if len(name) > 0 && name[0] == '_' {
		return true
	}
	if name == "node_modules" || name == "vendor" || name == "build" {
		return true
	}
	return false
}

// matchesInclude returns true if name matches any entry in the include list.
func matchesInclude(name string, includes []string) bool {
	for _, inc := range includes {
		if name == inc {
			return true
		}
	}
	return false
}

// appendUnique appends items to dst, skipping duplicates already present.
// Uses linear scan — expected cardinality is small (1-3 imports per package).
func appendUnique(dst []string, items ...string) []string {
	for _, s := range items {
		if !sliceContains(dst, s) {
			dst = append(dst, s)
		}
	}
	return dst
}

func sliceContains(ss []string, s string) bool {
	for _, v := range ss {
		if v == s {
			return true
		}
	}
	return false
}

// scanFilePackage returns the first in-package declaration in the expressions
// along with its 1-based line number. Returns ("", 0) for bare files.
func scanFilePackage(exprs []*lisp.LVal) (string, int) {
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		if astutil.HeadSymbol(expr) == "in-package" {
			if name := scanPackageName(expr); name != "" {
				line := 0
				if loc, ok := expr.Source(); ok {
					line = loc.Line
				}
				return name, line
			}
		}
	}
	return "", 0
}

// buildLoadTree parses mainPath and recursively walks load-file calls to build
// a map from absolute file path to the package context inherited from the load
// chain. This mirrors the runtime behavior where load-file executes in the
// caller's current package context.
//
// Also returns loadOrder — the DFS traversal order of files, matching the
// runtime's sequential load-file evaluation. Used to sort preamble forms
// so workspace macros see definitions in the same order as the runtime.
func buildLoadTree(mainPath string) (map[string]string, []string) {
	var pkgMap map[string]string
	var loadOrder []string
	pkgMap = make(map[string]string)
	visited := make(map[string]bool)
	walkLoadFile(mainPath, lisp.DefaultUserPackage, loadTreeState{result: pkgMap, visited: visited, order: &loadOrder})
	return pkgMap, loadOrder
}

// loadTreeState holds the package map, visited files and load order.
type loadTreeState struct {
	// result maps each loaded file to its package.
	result map[string]string
	// visited records files already traversed.
	visited map[string]bool
	// order records file traversal order.
	order *[]string
}

// walkLoadFile recursively parses a file and tracks package context through
// in-package and load-file calls. It only tracks package *context* (which
// package is active), not use-package imports — those are handled separately
// by scanUsePackages and PackageImports.
func walkLoadFile(filePath string, currentPkg string, opts loadTreeState) {
	result, visited, order := opts.result, opts.visited, opts.order

	absPath, err := filepath.Abs(filePath)
	if err != nil {
		return
	}
	if visited[absPath] {
		return
	}
	visited[absPath] = true
	*order = append(*order, absPath)

	src, err := os.ReadFile(absPath) //nolint:gosec // workspace file
	if err != nil {
		return
	}
	s := token.NewScanner(absPath, bytes.NewReader(src))
	p := rdparser.New(s)
	exprs, err := p.ParseProgram()
	if err != nil {
		return
	}

	dir := filepath.Dir(absPath)
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		head := astutil.HeadSymbol(expr)
		switch head {
		case "in-package":
			if name := scanPackageName(expr); name != "" {
				currentPkg = name
			}
		case "load-file":
			if len(expr.Cells) >= 2 && expr.Cells[1].Type == lisp.LString {
				loadPath := filepath.Join(dir, expr.Cells[1].Str)
				absLoad, absErr := filepath.Abs(loadPath)
				if absErr != nil {
					continue
				}
				result[absLoad] = currentPkg
				walkLoadFile(absLoad, currentPkg, loadTreeState{result: result, visited: visited, order: order})
			}
		}
	}
}

// fileScan holds definitions and package declarations from one file.
type fileScan struct {
	// globals contains global symbol bindings.
	globals []ExternalSymbol
	// pkgs groups exported symbols by package.
	pkgs map[string][]ExternalSymbol
	// defs contains all definitions.
	defs []ExternalSymbol
	// pkgAll groups all definitions by package.
	pkgAll map[string][]ExternalSymbol
	// usePackages records imported package names.
	usePackages map[string][]string
	// preamble contains package setup and definition forms.
	preamble []*lisp.LVal
	// macroCandidates: see hasMacroCandidates.
	macroCandidates bool
	// filePkg is the file's first declared package.
	filePkg string
	// firstInPkgLine is the first package declaration line.
	firstInPkgLine int
	// recordMethods are the methods the file's load-time s:defmethod
	// calls add; nil for a test file.
	recordMethods RecordMethods
}

// scanFileFull parses a file once and extracts exported globals, package-grouped
// exports, all definitions, use-package declarations, raw defmacro AST nodes,
// and the file's primary package ("" for bare files without in-package).
func scanFileFull(source []byte, filename string) fileScan {
	var globals []ExternalSymbol
	var pkgs map[string][]ExternalSymbol
	var pkgAll map[string][]ExternalSymbol
	var usePackages map[string][]string
	var preamble []*lisp.LVal
	var filePkg string
	var firstInPkgLine int
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return fileScan{globals: nil, pkgs: nil, defs: nil, pkgAll: nil, usePackages: nil, preamble: nil, filePkg: "", firstInPkgLine: 0, recordMethods: nil}
	}

	filePkg, firstInPkgLine = scanFilePackage(exprs)
	defs := extractDefinitions(exprs)
	exported := scanExportedDefinitionKeys(exprs)

	pkgs = make(map[string][]ExternalSymbol)
	pkgAll = make(map[string][]ExternalSymbol)
	for _, sym := range defs {
		pkgAll[sym.Package] = append(pkgAll[sym.Package], sym)
		if exported[definitionKey(sym.Package, sym.Name)] {
			globals = append(globals, sym)
			pkgs[sym.Package] = append(pkgs[sym.Package], sym)
		}
	}
	usePackages = scanUsePackages(exprs)

	// Collect preamble forms (package management + definitions) in source
	// order. LoadWorkspaceMacros evals these to replay the package setup
	// and register macros/functions/globals, mirroring (load) behavior.
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		switch astutil.HeadSymbol(expr) {
		case "in-package", "use-package", "export", "defmacro", "defun", "set":
			preamble = append(preamble, expr)
		}
	}

	return fileScan{globals: globals, pkgs: pkgs, defs: defs, pkgAll: pkgAll, usePackages: usePackages, preamble: preamble, macroCandidates: hasMacroCandidates(exprs), filePkg: filePkg, firstInPkgLine: firstInPkgLine, recordMethods: fileRecordMethods(source, func() ([]*lisp.LVal, error) { return exprs, nil })}
}

// extractDefinitions collects top-level definitions from pre-parsed expressions.
func extractDefinitions(exprs []*lisp.LVal) []ExternalSymbol {
	// Collect top-level definitions keyed by package-qualified name so
	// multi-package files can define the same symbol in different packages.
	defs := make(map[string]*ExternalSymbol)
	currentPkg := lisp.DefaultUserPackage

	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
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
	// Source order. The order of a map differs from run to run, and the
	// tables built from the definitions keep the order they are given:
	// two scans of one workspace must give the same tables.
	slices.SortFunc(result, func(a, b ExternalSymbol) int {
		if c := compareExternalSymbol(a, b); c != 0 {
			return c
		}
		return cmp.Or(cmp.Compare(a.Package, b.Package), cmp.Compare(a.Name, b.Name))
	})
	return result
}

func definitionKey(pkg, name string) string {
	return pkg + "\x00" + name
}

func scanExportedDefinitionKeys(exprs []*lisp.LVal) map[string]bool {
	exported := make(map[string]bool)
	currentPkg := lisp.DefaultUserPackage
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		switch astutil.HeadSymbol(expr) {
		case "in-package":
			if name := scanPackageName(expr); name != "" {
				currentPkg = name
			}
		case "export", "lisp:export":
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

	return &ExternalSymbol{
		Name:      nameVal.Str,
		Kind:      kind,
		Signature: signatureFromFormals(formalsVal),
		Source:    astutil.SymbolLoc(nameVal),
		DocString: DefunDocstring(expr),
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
		Source: astutil.SymbolLoc(nameVal),
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
	} else if arg.Type == lisp.LSExpr && arg.IsQuoted() && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		name = arg.Cells[0].Str
	}
	if name == "" {
		return nil
	}
	return &ExternalSymbol{
		Name:   name,
		Kind:   SymVariable,
		Source: astutil.SymbolLoc(arg),
	}
}

func scanExportNames(expr *lisp.LVal) []string {
	return astutil.ExportNames(expr.Cells[1:])
}

// scanUsePackages extracts use-package declarations grouped by their
// in-package context. Returns a map from package name to the list of
// packages it imports via use-package.
func scanUsePackages(exprs []*lisp.LVal) map[string][]string {
	result := make(map[string][]string)
	currentPkg := lisp.DefaultUserPackage
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
			continue
		}
		head := astutil.HeadSymbol(expr)
		switch head {
		case "in-package":
			if name := scanPackageName(expr); name != "" {
				currentPkg = name
			}
		case "use-package":
			if astutil.ArgCount(expr) >= 1 {
				if pkgName := extractPkgNameArg(expr.Cells[1]); pkgName != "" {
					result[currentPkg] = append(result[currentPkg], pkgName)
				}
			}
		}
	}
	return result
}

// ConfigForFile returns a copy of cfg with Filename set to filename, leaving
// the caller's Config untouched. A nil cfg yields an empty Config.
//
// This is a whole-struct copy rather than a field-by-field rebuild on purpose.
// The rebuild it replaces silently dropped fields twice: DefForms and
// PackageImports, and then MacroExpander, which did not exist when the
// rebuild was written and was never added to it (issue #353). Copying carries
// new Config fields through automatically, so the omission cannot recur.
// Use this anywhere a workspace-wide Config is specialised for one file.
//
// When cfg.FilePackages names the file, its package replaces DefaultPackage.
func ConfigForFile(cfg *Config, filename string) *Config {
	fileCfg := &Config{}
	if cfg != nil {
		*fileCfg = *cfg
	}
	fileCfg.Filename = filename
	if len(fileCfg.FilePackages) > 0 {
		if abs, err := filepath.Abs(filename); err == nil {
			if pkg, ok := fileCfg.FilePackages[abs]; ok {
				fileCfg.DefaultPackage = pkg
			}
		}
	}
	return fileCfg
}

// AnalyzeFile parses and performs full semantic analysis on a single file.
// Returns nil if the file fails to parse.
//
// Every field of cfg is honoured, including MacroExpander: analysis through
// AnalyzeFile resolves the same symbols as analysis through Analyze. Callers
// that scan many files and do not want to pay for macro expansion should leave
// cfg.MacroExpander nil.
func AnalyzeFile(source []byte, filename string, cfg *Config) *Result {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil
	}

	return Analyze(exprs, ConfigForFile(cfg, filename))
}

// ExtractFileRefs extracts cross-file-trackable references from an
// analysis result. Only references to global-scope, non-builtin symbols
// are included (builtins, special ops, parameters, and locals are skipped).
//
// File and Source.File of every returned FileReference denote the same file.
// The workspace index pairs the two when building document edits (lsp/rename.go
// takes the URI from File and the range from Source), so a reference whose text
// lives in a different file must not be recorded under filePath.
//
// The names in export forms (Result.ExportRefs) are returned too, each
// with FileReference.Export set. An export form can be in any file of the
// package, so only an index of every file finds them all: a rename that
// missed one would leave the package exporting a name with no definition.
// They are marked because an export is not a use (see FileReference.Export).
func ExtractFileRefs(result *Result, filePath string) []FileReference {
	if result == nil {
		return nil
	}

	var refs []FileReference
	finder := enclosingFinder{root: result.RootScope}
	for _, ref := range result.References {
		if ref.Symbol == nil || ref.Source == nil {
			continue
		}
		// Skip builtins, special ops, parameters, and locals.
		if !isGlobalUserSymbol(ref.Symbol) {
			continue
		}
		// Skip references whose text lives in another file. Macro expansion
		// (Config.MacroExpander) splices nodes from the macro's definition
		// site into this file's AST, and those nodes carry the *other*
		// file's location. Recording them under filePath would aim rename
		// at the wrong file; they are captured correctly when the defining
		// file is itself scanned.
		if !sameSourceFile(ref.Source.File, filePath) {
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
		enclosing := finder.find(ref.Source.Line, ref.Source.Col)
		if enclosing != nil {
			fref.Enclosing = enclosing.Name
			fref.EnclosingSource = enclosing.Source
			fref.EnclosingKind = enclosing.Kind
		}

		refs = append(refs, fref)
	}
	for _, ref := range result.ExportRefs {
		if ref.Symbol == nil || ref.Source == nil || !isGlobalUserSymbol(ref.Symbol) {
			continue
		}
		if !sameSourceFile(ref.Source.File, filePath) {
			continue
		}
		// No enclosing function: an export form takes effect at the top
		// level of a file.
		refs = append(refs, FileReference{
			SymbolKey: SymbolKey{
				Package: ref.Symbol.Package,
				Name:    ref.Symbol.Name,
				Kind:    ref.Symbol.Kind,
			},
			Source: ref.Source,
			File:   filePath,
			Export: true,
		})
	}
	return refs
}

// sameSourceFile reports whether two file paths denote the same file.
// Callers mix absolute and relative paths (ScanWorkspaceRefs analyses a
// possibly-relative path but indexes under the absolute one), and an empty
// source file is treated as "the file being analysed".
func sameSourceFile(sourceFile, filePath string) bool {
	if sourceFile == "" || sourceFile == filePath {
		return true
	}
	a, errA := filepath.Abs(sourceFile)
	b, errB := filepath.Abs(filePath)
	if errA != nil || errB != nil {
		return false
	}
	return a == b
}

// isGlobalUserSymbol returns true if a symbol is a user-defined global
// (not a builtin, special op, parameter, or lambda-scoped local).
func isGlobalUserSymbol(sym *Symbol) bool {
	switch sym.Kind {
	case SymBuiltin, SymSpecialOp, SymParameter:
		return false
	case SymVariable, SymFunction, SymMacro, SymType:
		// User-definable kinds -- fall through to the scope checks below.
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
		case ScopeLambda, ScopeLet, ScopeFlet, ScopeMacrolet, ScopeDotimes:
			// Locals: not visible to other files, so not workspace symbols.
			return false
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
			nodeLoc, nodeOK := token.Location{}, false
			if scope.Node != nil {
				nodeLoc, nodeOK = scope.Node.Source()
			}
			if nodeOK && scope.Parent != nil {
				var found *Symbol
				// Package definitions live at root even when their closure
				// body is nested. Local labels/flet still live in the parent.
				for bindingScope := scope.Parent; bindingScope != nil && found == nil; bindingScope = bindingScope.Parent {
					bindingScope.forEachSymbol(func(sym *Symbol) bool {
						if isEnclosingCandidate(sym) && sym.Source.Line == nodeLoc.Line &&
							definesFunctionScope(sym, scope, nodeLoc.File) {
							found = sym
							return false
						}
						return true
					})
				}
				if found != nil {
					return found
				}
			}
		}
		scope = scope.Parent
	}
	return nil
}

// isEnclosingCandidate reports whether sym can be the function that a
// function scope belongs to: a function or macro of this file with a
// source location. An external symbol belongs to another file and may
// share a line number by chance.
func isEnclosingCandidate(sym *Symbol) bool {
	return (sym.Kind == SymFunction || sym.Kind == SymMacro) && !sym.External && sym.Source != nil
}

// definesFunctionScope reports whether the candidate sym, which starts on
// the line of the node of the function scope, is the function of that
// scope. nodeFile is the source file of the node.
func definesFunctionScope(sym *Symbol, scope *Scope, nodeFile string) bool {
	if sym.Source.File != nodeFile {
		return false
	}
	head := astutil.HeadSymbol(scope.Node)
	if head != "defun" && head != "defmacro" {
		return true
	}
	return len(scope.Node.Cells) > 1 && sym.Node == scope.Node.Cells[1]
}

// enclosingFinder answers FindEnclosingFunction for many positions of one
// analysis result.
//
// FindEnclosingFunction visits every symbol of each binding scope for
// each position. The root scope holds every global of the workspace, so
// one file's references cost (references x workspace symbols), and a
// workspace scan grew with the square of the workspace
// (luthersystems/elps#944). The finder visits the symbols of a binding
// scope one time and keeps the candidates by line, and it keeps the
// answer of each function scope.
type enclosingFinder struct {
	root *Scope
	// candidates holds, for each binding scope, its candidate symbols by
	// source line.
	candidates map[*Scope]map[int][]*Symbol
	// functions holds the answer of each function scope. A nil entry
	// means the scope has no function symbol.
	functions map[*Scope]*Symbol
}

// find returns what FindEnclosingFunction(f.root, line, col) returns.
func (f *enclosingFinder) find(line, col int) *Symbol {
	for scope := ScopeAtPosition(f.root, line, col); scope != nil; scope = scope.Parent {
		if scope.Kind != ScopeFunction {
			continue
		}
		sym, ok := f.functions[scope]
		if !ok {
			sym = f.functionOf(scope)
			if f.functions == nil {
				f.functions = make(map[*Scope]*Symbol)
			}
			f.functions[scope] = sym
		}
		if sym != nil {
			return sym
		}
	}
	return nil
}

// functionOf returns the function symbol of a function scope, or nil.
func (f *enclosingFinder) functionOf(scope *Scope) *Symbol {
	if scope.Node == nil || scope.Parent == nil {
		return nil
	}
	nodeLoc, ok := scope.Node.Source()
	if !ok {
		return nil
	}
	// The order of the binding scopes is the order of
	// FindEnclosingFunction: the parent first, the root last.
	for binding := scope.Parent; binding != nil; binding = binding.Parent {
		for _, sym := range f.candidatesOf(binding)[nodeLoc.Line] {
			if definesFunctionScope(sym, scope, nodeLoc.File) {
				return sym
			}
		}
	}
	return nil
}

// candidatesOf returns the candidate symbols of a binding scope by line.
func (f *enclosingFinder) candidatesOf(binding *Scope) map[int][]*Symbol {
	byLine, ok := f.candidates[binding]
	if ok {
		return byLine
	}
	// This is forEachSymbol with the candidate test first. The root
	// scope holds every global of the workspace and few candidates, so
	// only the candidates go in the set that removes a symbol held by
	// both maps.
	var seen map[*Symbol]bool
	add := func(sym *Symbol) {
		if sym == nil || !isEnclosingCandidate(sym) || seen[sym] {
			return
		}
		if seen == nil {
			seen = make(map[*Symbol]bool)
			byLine = make(map[int][]*Symbol)
		}
		seen[sym] = true
		byLine[sym.Source.Line] = append(byLine[sym.Source.Line], sym)
	}
	for _, sym := range binding.Symbols {
		add(sym)
	}
	for _, sym := range binding.PackageSymbols {
		add(sym)
	}
	if f.candidates == nil {
		f.candidates = make(map[*Scope]map[int][]*Symbol)
	}
	f.candidates[binding] = byLine
	return byLine
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
	if scope.Node == nil {
		return nil
	}
	loc, ok := scope.Node.Source()
	if !ok {
		return nil
	}
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
//
// If cfg.MacroExpander is set it is used for every file, so the index agrees
// with per-document analysis about symbols reachable only through a macro
// expansion. Expansion is only attempted for heads that are user macros or
// unresolved, so the added cost is small; leave the field nil to skip it.
// The optional scanCfg controls file collection limits and excludes.
//
// Returns a map from SymbolKey.String() to FileReference slices.
func ScanWorkspaceRefs(root string, cfg *Config, scanCfg *ScanConfig) map[string][]FileReference {
	files, filesErr := collectLispFilesWithConfig(root, scanCfg)
	paths, _, err := files.paths, files.truncated, filesErr
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
				fileSrc, readErr := os.ReadFile(paths[i]) // CLI tool: reads user-specified files by design
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
	// PkgAllSymbols maps package name to ALL symbols (exported and
	// non-exported). Used for qualified symbol resolution since ELPS
	// runtime allows pkg:sym access to any symbol in a package.
	PkgAllSymbols map[string][]ExternalSymbol
	// DefForms are DefFormSpecs derived from defmacro definitions whose
	// names start with "def". These can be injected into Config.DefForms
	// for per-file analysis.
	DefForms []DefFormSpec
	// PackageImports maps package name to the list of packages imported
	// via use-package across all workspace files. This enables per-file
	// analysis to resolve symbols from cross-file use-package declarations.
	PackageImports map[string][]string
	// Preamble contains package-management and macro-definition forms
	// (in-package, use-package, export, defmacro) from all workspace files,
	// in source order per file. Pass these to LoadWorkspaceMacros to replay
	// the package setup and register macros in a runtime environment.
	Preamble []*lisp.LVal
	// DefaultPackage is the package declared in main.lisp (if present).
	// Used as the default for bare files (no in-package) so that cross-file
	// resolution works in projects where files inherit the package from
	// load order.
	DefaultPackage string
	// FilePackages maps the absolute path of each file in main.lisp's
	// load-file tree to the package active at its load-file call. Copy it
	// to Config.FilePackages. It is set only when DefaultPackage is set,
	// the same condition under which definitions are remapped.
	FilePackages map[string]string
	// RecordMethodsByFile holds, for each file, the methods its load-time
	// s:defmethod calls add to each record type. A test file's methods
	// are known only to the analysis of a test file. Copy it to
	// Config.RecordMethodsByFile, so the record lint checks know a method
	// that another file adds.
	RecordMethodsByFile *RecordMethodsByFile
}

// PrescanWorkspace performs a single-pass workspace scan that collects
// file definitions and extracts DefFormSpecs from def*-prefixed macros.
// The result provides everything needed to build a Config for per-file
// analysis, including macro-derived definition forms.
func PrescanWorkspace(root string, scanCfg *ScanConfig) (*WorkspacePrescan, error) {
	scan, err := ScanWorkspaceFiles(root, scanCfg)
	if err != nil {
		return nil, err
	}
	return scan.Prescan(), nil
}

// scannedFile is what the scan of one workspace file gives to a
// WorkspacePrescan.
type scannedFile struct {
	globals        []ExternalSymbol
	pkgs           map[string][]ExternalSymbol
	allDefs        []ExternalSymbol
	pkgAll         map[string][]ExternalSymbol
	defForms       []DefFormSpec
	preamble       []*lisp.LVal
	usePackages    map[string][]string
	filePkg        string // "" for bare files
	firstInPkgLine int    // 1-based line of first in-package, 0 if none
	recordMethods  RecordMethods
	// lastInPackage is the package of the last in-package of a literal
	// name in preamble, or "" (see WorkspaceScan.packageAfter).
	lastInPackage string
	// macroCandidates: a top-level form may be a macro call that
	// defines a name (hasMacroCandidates).
	macroCandidates bool
	// generated holds what the macro calls of the file define, once
	// WithGeneratedDefinitions has expanded them. WithFile keeps them
	// until they are expanded again.
	generated generatedDefs
}

// scanWorkspaceFile scans one file for a WorkspaceScan.
func scanWorkspaceFile(source []byte, path string) scannedFile {
	scanned := scanFileFull(source, path)
	return scannedFile{
		globals:         scanned.globals,
		pkgs:            scanned.pkgs,
		allDefs:         scanned.defs,
		pkgAll:          scanned.pkgAll,
		defForms:        extractDefFormSpecs(scanned.defs),
		preamble:        scanned.preamble,
		usePackages:     scanned.usePackages,
		filePkg:         scanned.filePkg,
		firstInPkgLine:  scanned.firstInPkgLine,
		recordMethods:   scanned.recordMethods,
		lastInPackage:   finalPackage(scanned.preamble, ""),
		macroCandidates: scanned.macroCandidates,
	}
}

// WorkspaceScan holds the scan of each file of a workspace.
// ScanWorkspaceFiles builds it, and Prescan merges the files into the
// tables of a WorkspacePrescan.
//
// An editor keeps the scan to follow a file that changes. WithFile and
// WithoutFile replace the scan of one file, and Prescan then gives the
// tables that a new scan of the whole workspace would give: the merge is
// the same code over the same per-file results. Tables that are updated
// entry by entry do not have that property. Their order follows the
// history of the changes, and they cannot give back an entry that lost a
// tie to an entry that is now gone.
//
// A WorkspaceScan is never written after it is built. WithFile and
// WithoutFile return a new one, so it is safe for concurrent use.
type WorkspaceScan struct {
	root      string
	truncated bool
	// paths are the files in the order of the walk of the workspace,
	// and results their scans.
	paths   []string
	results []scannedFile
	// byPath maps a path, and its absolute form, to its index in paths.
	byPath map[string]int
	// norm maps NormalizePath of each path to its index. It is built on
	// the first lookup that needs it: it costs file system calls for
	// each file.
	norm *normalizedPaths
	// abs holds filepath.Abs of each path ("" when it failed).
	abs []string
	// pkgs memoizes loadPackages. A reload asks for the packages of the
	// scan before a save and of the scan after it; the first is the scan
	// after the save before, so its packages are known.
	pkgs *loadPackagesMemo

	// The load tree of main.lisp. See buildLoadTree.
	mainPath   string
	loadTree   map[string]string
	loadOrder  []string
	defaultPkg string
}

// loadPackagesMemo holds the result of loadPackages for one start
// package.
type loadPackagesMemo struct {
	mu    sync.Mutex
	start string
	pkgs  []string
}

// normalizedPaths is the lazy map of a WorkspaceScan from normalized path
// to file index.
type normalizedPaths struct {
	once  sync.Once
	index map[string]int
}

// ScanWorkspaceFiles scans each lisp file under root. It is the first
// half of PrescanWorkspace; WorkspaceScan.Prescan is the second.
func ScanWorkspaceFiles(root string, scanCfg *ScanConfig) (*WorkspaceScan, error) {
	files, filesErr := collectLispFilesWithConfig(root, scanCfg)
	paths, truncated, err := files.paths, files.truncated, filesErr
	if err != nil {
		return nil, err
	}

	results := make([]scannedFile, len(paths))
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
				fileSrc, readErr := os.ReadFile(paths[i]) // CLI tool: reads user-specified files by design
				if readErr != nil {
					continue
				}
				results[i] = scanWorkspaceFile(fileSrc, paths[i])
			}
		}()
	}
	wg.Wait()

	scan := &WorkspaceScan{root: root, truncated: truncated, paths: paths, results: results}
	scan.indexPaths()
	scan.buildLoadTree()
	return scan, nil
}

// NewWorkspaceScan returns the scan of a workspace with no files. root is
// the directory a file added with WithFile is placed under; it can be
// empty.
func NewWorkspaceScan(root string) *WorkspaceScan {
	scan := &WorkspaceScan{root: root}
	scan.indexPaths()
	scan.buildLoadTree()
	return scan
}

// indexPaths builds the path lookup tables after paths changed.
func (w *WorkspaceScan) indexPaths() {
	w.byPath = make(map[string]int, 2*len(w.paths))
	w.abs = make([]string, len(w.paths))
	for i, p := range w.paths {
		if abs, err := filepath.Abs(p); err == nil {
			w.byPath[abs] = i
			w.abs[i] = abs
		}
		w.byPath[p] = i
	}
	w.norm = &normalizedPaths{}
	w.pkgs = &loadPackagesMemo{}
}

// buildLoadTree reads the load tree of main.lisp from the files on disk.
// It infers the package of the files that have no in-package form.
func (w *WorkspaceScan) buildLoadTree() {
	w.mainPath, w.defaultPkg, w.loadOrder = "", "", nil
	w.loadTree = make(map[string]string)
	for _, p := range w.paths {
		if filepath.Base(p) == "main.lisp" {
			w.mainPath = p
			break
		}
	}
	if w.mainPath == "" {
		return
	}
	w.loadTree, w.loadOrder = buildLoadTree(w.mainPath)
	// Find main.lisp's package from its scan.
	for i, r := range w.results {
		if w.paths[i] == w.mainPath && r.filePkg != "" {
			w.defaultPkg = r.filePkg
			break
		}
	}
}

// find returns the index of the file path names, or -1. It tries the
// path as given and as an absolute path before it resolves symlinks.
func (w *WorkspaceScan) find(path string) int {
	if i, ok := w.byPath[path]; ok {
		return i
	}
	if abs, err := filepath.Abs(path); err == nil {
		if i, ok := w.byPath[abs]; ok {
			return i
		}
	}
	w.norm.once.Do(func() {
		w.norm.index = make(map[string]int, len(w.paths))
		for i, p := range w.paths {
			w.norm.index[NormalizePath(p)] = i
		}
	})
	if i, ok := w.norm.index[NormalizePath(path)]; ok {
		return i
	}
	return -1
}

// Path returns the path under which the scan holds the file that path
// names, and whether it holds the file. The scan names a file as the
// walk of the workspace did, which can differ from the spelling of an
// editor (a symlink, a relative root).
func (w *WorkspaceScan) Path(path string) (string, bool) {
	if i := w.find(path); i >= 0 {
		return w.paths[i], true
	}
	return "", false
}

// inLoadTree reports whether the load tree of main.lisp reads the file,
// or would if the file was main.lisp.
func (w *WorkspaceScan) inLoadTree(path string) bool {
	if filepath.Base(path) == "main.lisp" {
		return true
	}
	abs, err := filepath.Abs(path)
	if err != nil {
		return true
	}
	if slices.Contains(w.loadOrder, abs) {
		return true
	}
	_, ok := w.loadTree[abs]
	return ok
}

// WithFile returns a scan in which the file at path has the given
// source: a new file, or new text of a file the scan holds. It reports
// whether what the file gives to the tables changed, the positions of
// its definitions aside. The diagnostics of the other files depend on
// that part only.
//
// The load tree is read again from the files on disk when the file is
// part of it, or is new. So source must be the text of the file on disk.
func (w *WorkspaceScan) WithFile(path string, source []byte) (*WorkspaceScan, bool) {
	next := *w
	i := w.find(path)
	if i < 0 {
		name := w.newFilePath(path)
		i = w.insertIndex(name)
		next.paths = slices.Insert(slices.Clone(w.paths), i, name)
		next.results = slices.Insert(slices.Clone(w.results), i, scanWorkspaceFile(source, name))
		next.indexPaths()
		next.buildLoadTree()
		return &next, true
	}
	scanned := scanWorkspaceFile(source, w.paths[i])
	// Keep what the macro calls of the file defined until
	// WithGeneratedDefinitions expands them again: the tables keep the
	// names meanwhile, and it compares the new definitions with these.
	scanned.generated = w.results[i].generated
	next.results = slices.Clone(w.results)
	next.results[i] = scanned
	next.pkgs = &loadPackagesMemo{}
	changed := !sameTableEntries(&w.results[i], &scanned)
	if w.inLoadTree(w.paths[i]) {
		next.buildLoadTree()
		changed = changed || next.defaultPkg != w.defaultPkg ||
			!maps.Equal(next.loadTree, w.loadTree) || !slices.Equal(next.loadOrder, w.loadOrder)
	}
	return &next, changed
}

// WithoutFile returns a scan without the file at path, and whether the
// scan held it.
func (w *WorkspaceScan) WithoutFile(path string) (*WorkspaceScan, bool) {
	i := w.find(path)
	if i < 0 {
		return w, false
	}
	next := *w
	next.paths = slices.Delete(slices.Clone(w.paths), i, i+1)
	next.results = slices.Delete(slices.Clone(w.results), i, i+1)
	next.indexPaths()
	next.buildLoadTree()
	return &next, true
}

// newFilePath returns the name the walk of the workspace would give to a
// new file: the root joined with the path below it.
func (w *WorkspaceScan) newFilePath(path string) string {
	if w.root == "" {
		return path
	}
	for _, root := range []string{w.root, NormalizePath(w.root)} {
		rel, err := filepath.Rel(root, path)
		if err == nil && filepath.IsLocal(rel) {
			return filepath.Join(w.root, rel)
		}
	}
	return path
}

// insertIndex returns the place of a new file in paths, which are in the
// order of filepath.Walk: the names of each directory in sorted order.
// That is the order of the paths compared one element at a time. A file
// that is not under the root goes last.
func (w *WorkspaceScan) insertIndex(name string) int {
	if w.root == "" {
		return len(w.paths)
	}
	rel, err := filepath.Rel(w.root, name)
	if err != nil || !filepath.IsLocal(rel) {
		return len(w.paths)
	}
	elems := strings.Split(rel, string(filepath.Separator))
	i, _ := slices.BinarySearchFunc(w.paths, elems, func(p string, elems []string) int {
		other, relErr := filepath.Rel(w.root, p)
		if relErr != nil || !filepath.IsLocal(other) {
			return 1
		}
		return slices.Compare(strings.Split(other, string(filepath.Separator)), elems)
	})
	return i
}

// sameTableEntries reports whether two scans of one file give the same
// entries to the workspace tables, the source positions aside.
func sameTableEntries(a, b *scannedFile) bool {
	sameSyms := func(x, y []ExternalSymbol) bool {
		return slices.EqualFunc(x, y, func(p, q ExternalSymbol) bool {
			p.Source, q.Source = nil, nil
			return reflect.DeepEqual(p, q)
		})
	}
	sameTable := func(x, y map[string][]ExternalSymbol) bool {
		return maps.EqualFunc(x, y, sameSyms)
	}
	return a.filePkg == b.filePkg &&
		(a.firstInPkgLine == 0) == (b.firstInPkgLine == 0) &&
		sameSyms(a.allDefs, b.allDefs) &&
		sameSyms(a.globals, b.globals) &&
		sameTable(a.pkgs, b.pkgs) &&
		sameTable(a.pkgAll, b.pkgAll) &&
		reflect.DeepEqual(a.defForms, b.defForms) &&
		reflect.DeepEqual(a.usePackages, b.usePackages) &&
		reflect.DeepEqual(a.recordMethods, b.recordMethods)
}

// inheritedInPackage returns the in-package form that Prescan puts
// before the preamble of file idx, or nil. A file with no in-package form
// takes the package of its load-file call (or of main.lisp), as the
// runtime does, so its macros are defined in that package.
func (w *WorkspaceScan) inheritedInPackage(idx int) *lisp.LVal {
	pkg := w.inheritedPackage(idx)
	if pkg == "" {
		return nil
	}
	return lisp.Cells{
		lisp.Symbol("in-package"),
		lisp.Quote(lisp.Symbol(pkg)),
	}.SExpr()
}

// inheritedPackage is the package of the in-package form of
// inheritedInPackage, or "".
func (w *WorkspaceScan) inheritedPackage(idx int) string {
	r := &w.results[idx]
	if r.filePkg != "" || len(r.preamble) == 0 {
		return ""
	}
	if pkg := w.loadTree[w.abs[idx]]; pkg != "" {
		return pkg
	}
	return w.defaultPkg
}

// FilePreamble returns the forms that Prescan adds to
// WorkspacePrescan.Preamble for the file at path, and whether the scan
// holds the file. Pass them to EnvMacroExpander.ReloadFileMacros, or
// compare them with those of another scan to learn whether a change of
// the file changed what LoadWorkspaceMacros loads. The caller must not
// write the slice.
func (w *WorkspaceScan) FilePreamble(path string) ([]*lisp.LVal, bool) {
	i := w.find(path)
	if i < 0 {
		return nil, false
	}
	return w.filePreamble(i), true
}

// PreambleFilesFrom returns the paths of the files whose forms are at
// index form or later in the Preamble of Prescan, in that order. A file
// with a form on each side of the index is included. After a load of the
// preamble that stopped at form (PreambleLoad.Unfinished), these are the
// files whose macros are not all loaded.
func (w *WorkspaceScan) PreambleFilesFrom(form int) []string {
	var files []string
	n := 0
	w.preambleOrder(func(idx int) {
		count := len(w.results[idx].preamble)
		if w.inheritedPackage(idx) != "" {
			count++
		}
		n += count
		if count > 0 && n > form {
			files = append(files, w.paths[idx])
		}
	})
	return files
}

// filePreamble is FilePreamble of file idx.
func (w *WorkspaceScan) filePreamble(idx int) []*lisp.LVal {
	forms := w.results[idx].preamble
	if inPkg := w.inheritedInPackage(idx); inPkg != nil {
		forms = lisp.Cells{inPkg}.Append(forms...)
	}
	return forms
}

// preambleOrder visits the files in the order Prescan puts their
// preambles: the load-tree DFS order of main.lisp (the order of the
// runtime's sequential load-file evaluation), then the files the load
// tree does not read. So macros see definitions in the same order as at
// runtime.
//
// It calls visit for each file. Each save runs Prescan, so the order is
// not collected into a slice. The load order holds absolute paths, which
// byPath maps to their files.
func (w *WorkspaceScan) preambleOrder(visit func(idx int)) {
	var loaded []bool
	if len(w.loadOrder) > 0 {
		loaded = make([]bool, len(w.paths))
		for _, absPath := range w.loadOrder {
			if idx, ok := w.byPath[absPath]; ok {
				visit(idx)
				loaded[idx] = true
			}
		}
	}
	for i := range w.results {
		if loaded == nil || !loaded[i] {
			visit(i)
		}
	}
}

// loadPackages returns, for each file, the package that is active when
// LoadWorkspaceMacros over Prescan().Preamble reaches the first form of
// the file. start is the package active before the first form of the
// whole preamble.
//
// The index build evaluates the preambles of all files in a row, so a
// file with no in-package form (and no package from the load tree) runs
// in the package the file before it left active. A reload of one file
// must start there too, or its macros land in another package.
//
// The result is memoized on the scan, which never changes, and the
// caller must not write it.
func (w *WorkspaceScan) loadPackages(start string) []string {
	w.pkgs.mu.Lock()
	defer w.pkgs.mu.Unlock()
	if w.pkgs.pkgs != nil && w.pkgs.start == start {
		return w.pkgs.pkgs
	}
	pkgs := make([]string, len(w.results))
	pkg := start
	w.preambleOrder(func(idx int) {
		pkgs[idx] = pkg
		pkg = w.packageAfter(idx, pkg)
	})
	w.pkgs.start, w.pkgs.pkgs = start, pkgs
	return pkgs
}

// packageAfter returns the package active after the preamble of file
// idx, which starts in pkg. It is finalPackage of filePreamble(idx),
// read from the scan of the file.
func (w *WorkspaceScan) packageAfter(idx int, pkg string) string {
	if last := w.results[idx].lastInPackage; last != "" {
		return last
	}
	if inherited := w.inheritedPackage(idx); inherited != "" {
		return inherited
	}
	return pkg
}

// loadPackageOf is loadPackages for the file at path, or start when the
// scan is nil or does not hold the file.
func (w *WorkspaceScan) loadPackageOf(path, start string) string {
	if w == nil {
		return start
	}
	i := w.find(path)
	if i < 0 {
		return start
	}
	return w.loadPackages(start)[i]
}

// Prescan merges the scans of the files into the tables of a
// WorkspacePrescan. It writes neither the scan nor a slice of it.
func (w *WorkspaceScan) Prescan() *WorkspacePrescan {
	paths, results, truncated := w.paths, w.results, w.truncated
	loadTree, defaultPkg := w.loadTree, w.defaultPkg

	prescan := &WorkspacePrescan{
		Files:          paths,
		Truncated:      truncated,
		PkgExports:     make(map[string][]ExternalSymbol),
		PkgAllSymbols:  make(map[string][]ExternalSymbol),
		PackageImports: make(map[string][]string),
		DefaultPackage: defaultPkg,
	}
	w.preambleOrder(func(idx int) {
		if inPkg := w.inheritedInPackage(idx); inPkg != nil {
			prescan.Preamble = append(prescan.Preamble, inPkg)
		}
		prescan.Preamble = append(prescan.Preamble, results[idx].preamble...)
	})

	// Make room for the merged tables one time. An editor merges them
	// again on each save, and a slice that grows entry by entry is
	// copied again and again.
	var numGlobals, numDefs int
	numExports := make(map[string]int)
	numSymbols := make(map[string]int)
	for i := range results {
		gen := &results[i].generated
		numGlobals += len(results[i].globals) + len(gen.globals)
		numDefs += len(results[i].allDefs) + len(gen.allDefs)
		for _, t := range []map[string][]ExternalSymbol{results[i].pkgs, gen.pkgs} {
			for pkg, syms := range t {
				numExports[pkg] += len(syms)
			}
		}
		for _, t := range []map[string][]ExternalSymbol{results[i].pkgAll, gen.pkgAll} {
			for pkg, syms := range t {
				numSymbols[pkg] += len(syms)
			}
		}
	}
	if numGlobals > 0 {
		prescan.ExportedGlobals = make([]ExternalSymbol, 0, numGlobals)
	}
	if numDefs > 0 {
		prescan.AllDefs = make([]ExternalSymbol, 0, numDefs)
	}
	for pkg, n := range numExports {
		if n > 0 {
			prescan.PkgExports[pkg] = make([]ExternalSymbol, 0, n)
		}
	}
	for pkg, n := range numSymbols {
		if n > 0 {
			prescan.PkgAllSymbols[pkg] = make([]ExternalSymbol, 0, n)
		}
	}

	var recordMethods map[string]RecordMethods
	for i, r := range results {
		if len(r.recordMethods) > 0 {
			if recordMethods == nil {
				recordMethods = make(map[string]RecordMethods)
			}
			recordMethods[paths[i]] = r.recordMethods
		}
		prescan.ExportedGlobals = append(prescan.ExportedGlobals, r.globals...)
		prescan.AllDefs = append(prescan.AllDefs, r.allDefs...)
		prescan.DefForms = append(prescan.DefForms, r.defForms...)
		for pkg, syms := range r.pkgs {
			prescan.PkgExports[pkg] = append(prescan.PkgExports[pkg], syms...)
		}
		for pkg, syms := range r.pkgAll {
			prescan.PkgAllSymbols[pkg] = append(prescan.PkgAllSymbols[pkg], syms...)
		}
		for pkg, imports := range r.usePackages {
			prescan.PackageImports[pkg] = appendUnique(prescan.PackageImports[pkg], imports...)
		}
	}
	prescan.RecordMethodsByFile = NewRecordMethodsByFile(recordMethods)

	// Remap user-package symbols to their correct package.
	//
	// extractDefinitions assigns all symbols to "user" by default because
	// it has no package context beyond in-package forms. At runtime, bare
	// files (no in-package) inherit the caller's package from load-file,
	// so their definitions belong in a different package.
	//
	// DefaultPackage (from main.lisp's in-package) is the workspace's
	// "real" default package — the package most bare files will inherit
	// at runtime, since main.lisp is typically the entry point that loads
	// everything else.
	//
	// Three remapping rules:
	// (1) Bare files in the load tree: remap to the load-tree's package
	//     context (the package active at the load-file call site).
	// (2) Bare files NOT in the load tree: remap to DefaultPackage.
	//     These files may be loaded at runtime from inside function bodies
	//     (e.g. template files loaded conditionally) — DefaultPackage is
	//     the best static approximation of the runtime package.
	// (3) Non-bare files: only remap user-package defs that appear before
	//     the first in-package (they inherit the load context, not "user").
	if defaultPkg != "" {
		prescan.FilePackages = loadTree
		bareFiles := make(map[string]bool)
		firstPkgLine := make(map[string]int) // file → line of first in-package
		for i, r := range results {
			abs, absErr := filepath.Abs(paths[i])
			if absErr != nil {
				continue
			}
			if r.filePkg == "" {
				bareFiles[abs] = true
			}
			if r.firstInPkgLine > 0 {
				firstPkgLine[abs] = r.firstInPkgLine
			}
		}

		userPkg := lisp.DefaultUserPackage
		shouldRemap := func(sym *ExternalSymbol) (string, bool) {
			if sym.Package != userPkg || sym.Source == nil {
				return "", false
			}
			// The maps are keyed by absolute path, but Source.File holds
			// the path as the caller gave it, which can be relative.
			file := sym.Source.File
			if abs, absErr := filepath.Abs(file); absErr == nil {
				file = abs
			}
			pkg, inTree := loadTree[file]
			// Bare file: remap to load-tree package, or defaultPkg if not
			// in the load tree. At runtime, bare files always inherit their
			// caller's package — defaultPkg is the best approximation when
			// the file isn't explicitly loaded via load-file.
			if bareFiles[file] {
				if !inTree {
					pkg = defaultPkg
				}
				return pkg, true
			}
			if !inTree {
				return "", false
			}
			// Non-bare file: remap user-package defs before the first in-package.
			if line, ok := firstPkgLine[file]; ok && sym.Source.Line < line {
				return pkg, true
			}
			return "", false
		}

		for i := range prescan.AllDefs {
			if pkg, ok := shouldRemap(&prescan.AllDefs[i]); ok {
				prescan.AllDefs[i].Package = pkg
			}
		}
		for i := range prescan.ExportedGlobals {
			if pkg, ok := shouldRemap(&prescan.ExportedGlobals[i]); ok {
				prescan.ExportedGlobals[i].Package = pkg
			}
		}
		remapPackageMap(prescan.PkgExports, userPkg, shouldRemap)
		remapPackageMap(prescan.PkgAllSymbols, userPkg, shouldRemap)
		// Merge use-package imports from "user" into default package.
		if userImports, ok := prescan.PackageImports[userPkg]; ok {
			prescan.PackageImports[defaultPkg] = appendUnique(prescan.PackageImports[defaultPkg], userImports...)
			delete(prescan.PackageImports, userPkg)
		}
	}

	// The definitions that macro calls make (WithGeneratedDefinitions).
	// They go in after the remap above: the analysis that found them
	// already gave them the package of their file.
	for i := range results {
		gen := &results[i].generated
		if gen.empty() {
			continue
		}
		prescan.AllDefs = append(prescan.AllDefs, gen.allDefs...)
		prescan.ExportedGlobals = append(prescan.ExportedGlobals, gen.globals...)
		for pkg, syms := range gen.pkgs {
			prescan.PkgExports[pkg] = append(prescan.PkgExports[pkg], syms...)
		}
		for pkg, syms := range gen.pkgAll {
			prescan.PkgAllSymbols[pkg] = append(prescan.PkgAllSymbols[pkg], syms...)
		}
	}

	return prescan
}

// remapPackageMap moves symbols from srcPkg to their correct package based
// on shouldRemap. Used to fix bare-file symbols that start in "user" but
// should be in the package inherited from the load tree.
func remapPackageMap(m map[string][]ExternalSymbol, srcPkg string, shouldRemap func(*ExternalSymbol) (string, bool)) {
	syms, ok := m[srcPkg]
	if !ok {
		return
	}
	var remaining []ExternalSymbol
	for i := range syms {
		if pkg, ok := shouldRemap(&syms[i]); ok {
			syms[i].Package = pkg
			m[pkg] = append(m[pkg], syms[i])
			continue
		}
		remaining = append(remaining, syms[i])
	}
	if len(remaining) > 0 {
		m[srcPkg] = remaining
	} else {
		delete(m, srcPkg)
	}
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

// FileIndex holds what one file adds to the workspace tables of a Config.
// IndexFile builds it.
type FileIndex struct {
	// Defs are all top-level definitions of the file, for
	// Config.ExtraGlobals.
	Defs []ExternalSymbol
	// PkgExports are the exported definitions by package, for
	// Config.PackageExports.
	PkgExports map[string][]ExternalSymbol
	// PkgAllSymbols are all definitions by package, for
	// Config.PackageSymbols.
	PkgAllSymbols map[string][]ExternalSymbol
	// PackageImports are the use-package imports by package, for
	// Config.PackageImports.
	PackageImports map[string][]string
	// RecordMethods are the methods the load-time s:defmethod calls add,
	// for Config.RecordMethodsByFile.
	RecordMethods RecordMethods
}

// IndexFile scans one file and returns what it adds to the workspace
// tables of a Config: the same entries PrescanWorkspace collects for the
// file. An editor calls it when a file is saved, to replace the file's
// entries without a scan of the whole workspace.
//
// cfg is the Config of the workspace the file is in. It gives the package
// of a definition that runs before any in-package form: IndexFile applies
// the rules of PrescanWorkspace with cfg.DefaultPackage and
// cfg.FilePackages. Without them a definition of a file with no
// in-package would move to the "user" package on each save, and the other
// files of its real package would lose it.
//
// The load tree itself (FilePackages, DefaultPackage) is not built again:
// a save that changes a load-file call or the package of main.lisp needs a
// new PrescanWorkspace. A file that does not parse gives an empty index.
func IndexFile(source []byte, filename string, cfg *Config) *FileIndex {
	scanned := scanFileFull(source, filename)
	idx := &FileIndex{
		Defs:           scanned.defs,
		PkgExports:     scanned.pkgs,
		PkgAllSymbols:  scanned.pkgAll,
		PackageImports: scanned.usePackages,
		RecordMethods:  scanned.recordMethods,
	}
	if cfg == nil || cfg.DefaultPackage == "" {
		return idx
	}

	abs := filename
	if a, err := filepath.Abs(filename); err == nil {
		abs = a
	}
	treePkg, inTree := cfg.FilePackages[abs]
	bare := scanned.filePkg == ""
	userPkg := lisp.DefaultUserPackage
	// The three rules of PrescanWorkspace, for one file.
	shouldRemap := func(sym *ExternalSymbol) (string, bool) {
		if sym.Package != userPkg || sym.Source == nil {
			return "", false
		}
		if bare {
			if !inTree {
				return cfg.DefaultPackage, true
			}
			return treePkg, true
		}
		if !inTree {
			return "", false
		}
		if scanned.firstInPkgLine > 0 && sym.Source.Line < scanned.firstInPkgLine {
			return treePkg, true
		}
		return "", false
	}
	for i := range idx.Defs {
		if pkg, ok := shouldRemap(&idx.Defs[i]); ok {
			idx.Defs[i].Package = pkg
		}
	}
	remapPackageMap(idx.PkgExports, userPkg, shouldRemap)
	remapPackageMap(idx.PkgAllSymbols, userPkg, shouldRemap)
	if userImports, ok := idx.PackageImports[userPkg]; ok {
		idx.PackageImports[cfg.DefaultPackage] = appendUnique(idx.PackageImports[cfg.DefaultPackage], userImports...)
		delete(idx.PackageImports, userPkg)
	}
	return idx
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
