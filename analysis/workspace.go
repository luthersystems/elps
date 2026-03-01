// Copyright © 2024 The ELPS authors

package analysis

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// SymbolKey identifies a symbol across files by name and kind.
type SymbolKey struct {
	Name string
	Kind SymbolKind
}

// String returns a lookup key for use in maps.
func (k SymbolKey) String() string {
	return fmt.Sprintf("%s/%s", k.Name, k.Kind)
}

// FileReference represents a cross-file reference to a symbol.
type FileReference struct {
	SymbolKey       SymbolKey
	Source          *token.Location
	File            string           // absolute path
	Enclosing       string           // name of enclosing function ("" if top-level)
	EnclosingSource *token.Location  // definition site of enclosing function
	EnclosingKind   SymbolKind       // kind of enclosing function
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

// ScanWorkspaceFull walks a directory tree in a single pass, parsing all
// .lisp files and extracting both global symbols and package exports.
// It skips hidden directories (names starting with '.') and node_modules.
//
// Files that fail to parse are silently skipped (fault tolerant).
func ScanWorkspaceFull(root string) ([]ExternalSymbol, map[string][]ExternalSymbol, error) {
	var globals []ExternalSymbol
	pkgs := make(map[string][]ExternalSymbol)

	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // skip unreadable dirs
		}
		if info.IsDir() {
			name := info.Name()
			if shouldSkipDir(name) {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".lisp" {
			return nil
		}
		fileSrc, readErr := os.ReadFile(path) //nolint:gosec // CLI tool reads user-specified files
		if readErr != nil {
			return nil // skip unreadable files
		}

		// Parse once and extract both globals and packages.
		fileGlobals := scanFile(fileSrc, path)
		globals = append(globals, fileGlobals...)

		filePkgs := scanFilePackages(fileSrc, path)
		for pkg, syms := range filePkgs {
			pkgs[pkg] = append(pkgs[pkg], syms...)
		}
		return nil
	})
	if err != nil {
		return nil, nil, err
	}
	return globals, pkgs, nil
}

// shouldSkipDir returns true for directories that should not be walked.
// It skips hidden directories (e.g. .git, .vscode) and node_modules,
// but not "." or ".." which represent the current/parent directory.
func shouldSkipDir(name string) bool {
	if name == "." || name == ".." {
		return false
	}
	if len(name) > 0 && name[0] == '.' {
		return true
	}
	if name == "node_modules" {
		return true
	}
	return false
}

// scanFile parses a single file and extracts top-level definitions and
// export information. Returns nil if the file fails to parse.
func scanFile(source []byte, filename string) []ExternalSymbol {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil // skip files that fail to parse
	}

	// Collect top-level definitions
	defs := make(map[string]*ExternalSymbol)
	exported := make(map[string]bool)
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
				defs[sym.Name] = sym
			}
		case "defmacro":
			if sym := scanDefun(expr, SymMacro); sym != nil {
				sym.Package = currentPkg
				defs[sym.Name] = sym
			}
		case "deftype":
			if sym := scanDeftype(expr); sym != nil {
				sym.Package = currentPkg
				defs[sym.Name] = sym
			}
		case "set":
			if sym := scanSet(expr); sym != nil {
				sym.Package = currentPkg
				defs[sym.Name] = sym
			}
		case "export":
			for _, name := range scanExportNames(expr) {
				exported[name] = true
			}
		}
	}

	// Only return definitions that appear in export forms
	var result []ExternalSymbol
	for name, sym := range defs {
		if exported[name] {
			result = append(result, *sym)
		}
	}
	return result
}

// scanFilePackages is like scanFile but returns symbols grouped by package.
func scanFilePackages(source []byte, filename string) map[string][]ExternalSymbol {
	s := token.NewScanner(filename, bytes.NewReader(source))
	p := rdparser.New(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return nil
	}

	defs := make(map[string]*ExternalSymbol)
	exported := make(map[string]bool)
	currentPkg := "user"

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
				defs[sym.Name] = sym
			}
		case "defmacro":
			if sym := scanDefun(expr, SymMacro); sym != nil {
				sym.Package = currentPkg
				defs[sym.Name] = sym
			}
		case "deftype":
			if sym := scanDeftype(expr); sym != nil {
				sym.Package = currentPkg
				defs[sym.Name] = sym
			}
		case "set":
			if sym := scanSet(expr); sym != nil {
				sym.Package = currentPkg
				defs[sym.Name] = sym
			}
		case "export":
			for _, name := range scanExportNames(expr) {
				exported[name] = true
			}
		}
	}

	result := make(map[string][]ExternalSymbol)
	for name, sym := range defs {
		if exported[name] {
			result[sym.Package] = append(result[sym.Package], *sym)
		}
	}
	return result
}

// scanPackageName extracts the package name from an in-package expression.
func scanPackageName(expr *lisp.LVal) string {
	if astutil.ArgCount(expr) < 1 {
		return ""
	}
	return extractPkgNameArg(expr.Cells[1])
}

// extractPkgNameArg gets a package name from a use-package/in-package argument.
func extractPkgNameArg(arg *lisp.LVal) string {
	if arg.Type == lisp.LString {
		return arg.Str
	}
	if arg.Type == lisp.LSymbol {
		return arg.Str
	}
	if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		return arg.Cells[0].Str
	}
	return ""
}

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

		key := SymbolKey{Name: ref.Symbol.Name, Kind: ref.Symbol.Kind}
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
				for _, sym := range scope.Parent.Symbols {
					if sym.Kind != SymFunction && sym.Kind != SymMacro {
						continue
					}
					// Skip external (imported) symbols — they belong to
					// other files and may coincidentally share a line number.
					if sym.External {
						continue
					}
					if sym.Source != nil && sym.Source.Line == nodeLoc.Line &&
						sym.Source.File == nodeLoc.File {
						return sym
					}
				}
			}
		}
		scope = scope.Parent
	}
	return nil
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
// a prior ScanWorkspaceFull call.
//
// Returns a map from SymbolKey.String() to FileReference slices.
func ScanWorkspaceRefs(root string, cfg *Config) map[string][]FileReference {
	refs := make(map[string][]FileReference)

	_ = filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if info.IsDir() {
			if shouldSkipDir(info.Name()) {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".lisp" {
			return nil
		}
		fileSrc, readErr := os.ReadFile(path) //nolint:gosec // CLI tool reads user-specified files
		if readErr != nil {
			return nil
		}

		result := AnalyzeFile(fileSrc, path, cfg)
		if result == nil {
			return nil
		}

		absPath := path
		if !filepath.IsAbs(absPath) {
			if abs, err := filepath.Abs(absPath); err == nil {
				absPath = abs
			}
		}

		fileRefs := ExtractFileRefs(result, absPath)
		for i := range fileRefs {
			key := fileRefs[i].SymbolKey.String()
			refs[key] = append(refs[key], fileRefs[i])
		}
		return nil
	})

	return refs
}

// SymbolToKey derives a SymbolKey from an analysis Symbol.
func SymbolToKey(sym *Symbol) SymbolKey {
	kind := sym.Kind
	// Normalize macro kind to function for cross-file matching,
	// since a defmacro in one file is referenced as a "function call" in another.
	// Actually, keep them distinct — the kind is part of the identity.
	return SymbolKey{Name: sym.Name, Kind: kind}
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
