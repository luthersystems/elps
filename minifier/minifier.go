// Copyright © 2026 The ELPS authors

// Package minifier provides deterministic, scope-aware identifier minification
// for ELPS source files and parsed programs.
package minifier

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/formatter"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// InputFile is a source unit to minify.
type InputFile struct {
	Path   string
	Source []byte
}

// Config controls minification behavior.
type Config struct {
	Analysis      *analysis.Config
	Exclusions    map[string]bool
	RenameExports bool
	Formatter     *formatter.Config
}

// FileResult contains the minified output for one source unit.
type FileResult struct {
	Path    string
	Source  []byte
	Output  []byte
	Program []*lisp.LVal
}

// SymbolMapEntry records one deterministic symbol assignment.
type SymbolMapEntry struct {
	Minified string `json:"minified"`
	Original string `json:"original"`
	Kind     string `json:"kind"`
	File     string `json:"file,omitempty"`
	Line     int    `json:"line,omitempty"`
	Col      int    `json:"col,omitempty"`
}

// SymbolMap is the machine-readable symbol mapping emitted by a minify run.
type SymbolMap struct {
	Entries            []SymbolMapEntry    `json:"entries"`
	MinifiedToOriginal map[string]string   `json:"minified_to_original"`
	OriginalToMinified map[string][]string `json:"original_to_minified,omitempty"`
}

// Result contains the outputs of a minify run.
type Result struct {
	Files     []FileResult
	SymbolMap SymbolMap
}

type parsedFile struct {
	path             string
	source           []byte
	exprs            []*lisp.LVal
	trailingComments []*token.Token
	analysis         *analysis.Result
}

type fileSymbols struct {
	globals []analysis.ExternalSymbol
}

// Minify rewrites one or more source units using a single deterministic
// symbol-assignment session.
func Minify(inputs []InputFile, cfg *Config) (*Result, error) {
	if cfg == nil {
		cfg = &Config{}
	}
	if cfg.Formatter == nil {
		cfg.Formatter = formatter.DefaultConfig()
		cfg.Formatter.Compact = true
		cfg.Formatter.StripComments = true
	}
	if len(inputs) == 0 {
		return &Result{}, nil
	}

	files := make([]parsedFile, 0, len(inputs))
	for _, input := range inputs {
		file, err := parseFile(input)
		if err != nil {
			return nil, err
		}
		files = append(files, file)
	}

	perFile, pkgExports := scanInputSymbols(files)
	for i := range files {
		fileCfg := mergeAnalysisConfig(cfg.Analysis, files[i].path, perFile, pkgExports)
		files[i].analysis = analysis.Analyze(files[i].exprs, fileCfg)
	}

	protected := protectedMacroTemplateSymbols(files, cfg.Exclusions)
	assignments, assignmentKeys, symMap := buildAssignments(files, cfg, protected)
	for i := range files {
		applyAssignments(&files[i], assignments, assignmentKeys, cfg)
	}

	result := &Result{
		Files:     make([]FileResult, 0, len(files)),
		SymbolMap: symMap,
	}
	for _, file := range files {
		result.Files = append(result.Files, FileResult{
			Path:    file.path,
			Source:  file.source,
			Output:  formatter.FormatProgram(file.exprs, file.trailingComments, cfg.Formatter),
			Program: file.exprs,
		})
	}
	return result, nil
}

// MinifySource is a convenience wrapper for a single source unit.
func MinifySource(source []byte, filename string, cfg *Config) ([]byte, SymbolMap, error) {
	result, err := Minify([]InputFile{{Path: filename, Source: source}}, cfg)
	if err != nil {
		return nil, SymbolMap{}, err
	}
	if len(result.Files) == 0 {
		return nil, result.SymbolMap, nil
	}
	return result.Files[0].Output, result.SymbolMap, nil
}

// ReadExcludeFile loads one symbol per line, ignoring blank lines and comment
// lines that start with ';'.
func ReadExcludeFile(path string) ([]string, error) {
	f, err := os.Open(path) //nolint:gosec // CLI helper reads a user-specified file
	if err != nil {
		return nil, err
	}
	defer func() {
		_ = f.Close()
	}()

	var out []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" || strings.HasPrefix(line, ";") {
			continue
		}
		out = append(out, line)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return out, nil
}

// JSON returns the symbol map encoded as indented JSON.
func (m SymbolMap) JSON() ([]byte, error) {
	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	enc.SetIndent("", "  ")
	if err := enc.Encode(m); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
}

func parseFile(input InputFile) (parsedFile, error) {
	s := token.NewScanner(input.Path, bytes.NewReader(input.Source))
	p := rdparser.NewFormatting(s)

	exprs, err := p.ParseProgram()
	if err != nil {
		return parsedFile{}, fmt.Errorf("%s: %w", input.Path, err)
	}

	return parsedFile{
		path:             input.Path,
		source:           input.Source,
		exprs:            exprs,
		trailingComments: p.PendingComments(),
	}, nil
}

func mergeAnalysisConfig(base *analysis.Config, filename string, perFile map[string]fileSymbols, pkgExports map[string][]analysis.ExternalSymbol) *analysis.Config {
	cfg := &analysis.Config{Filename: filename}
	if base != nil {
		cfg.ExtraGlobals = append(cfg.ExtraGlobals, base.ExtraGlobals...)
		cfg.PackageExports = copyPackageExports(base.PackageExports)
	}
	if cfg.PackageExports == nil {
		cfg.PackageExports = make(map[string][]analysis.ExternalSymbol)
	}
	for pkg, exports := range pkgExports {
		cfg.PackageExports[pkg] = append(cfg.PackageExports[pkg], exports...)
	}
	for path, symbols := range perFile {
		if path == filename {
			continue
		}
		cfg.ExtraGlobals = append(cfg.ExtraGlobals, symbols.globals...)
	}
	return cfg
}

func copyPackageExports(in map[string][]analysis.ExternalSymbol) map[string][]analysis.ExternalSymbol {
	if in == nil {
		return nil
	}
	out := make(map[string][]analysis.ExternalSymbol, len(in))
	for pkg, symbols := range in {
		out[pkg] = append([]analysis.ExternalSymbol(nil), symbols...)
	}
	return out
}

func scanInputSymbols(files []parsedFile) (map[string]fileSymbols, map[string][]analysis.ExternalSymbol) {
	perFile := make(map[string]fileSymbols, len(files))
	pkgExports := make(map[string][]analysis.ExternalSymbol)
	for _, file := range files {
		globals, exports := scanProgramSymbols(file.exprs)
		perFile[file.path] = fileSymbols{globals: globals}
		for pkg, syms := range exports {
			pkgExports[pkg] = append(pkgExports[pkg], syms...)
		}
	}
	return perFile, pkgExports
}

func scanProgramSymbols(exprs []*lisp.LVal) ([]analysis.ExternalSymbol, map[string][]analysis.ExternalSymbol) {
	defs := make(map[string]analysis.ExternalSymbol)
	exported := make(map[string]map[string]bool)
	currentPkg := "user"

	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 || expr.Cells[0].Type != lisp.LSymbol {
			continue
		}
		switch expr.Cells[0].Str {
		case "in-package":
			if pkg := packageName(expr.Cells[1:]); pkg != "" {
				currentPkg = pkg
			}
		case "defun":
			if sym := topLevelDef(expr, analysis.SymFunction, currentPkg); sym != nil {
				defs[currentPkg+"/"+sym.Name] = *sym
			}
		case "defmacro":
			if sym := topLevelDef(expr, analysis.SymMacro, currentPkg); sym != nil {
				defs[currentPkg+"/"+sym.Name] = *sym
			}
		case "deftype":
			if sym := topLevelDef(expr, analysis.SymType, currentPkg); sym != nil {
				defs[currentPkg+"/"+sym.Name] = *sym
			}
		case "set":
			if sym := topLevelSet(expr, currentPkg); sym != nil {
				defs[currentPkg+"/"+sym.Name] = *sym
			}
		case "export":
			names := exportNames(expr.Cells[1:])
			if len(names) == 0 {
				continue
			}
			if exported[currentPkg] == nil {
				exported[currentPkg] = make(map[string]bool)
			}
			for _, name := range names {
				exported[currentPkg][name] = true
			}
		}
	}

	globals := make([]analysis.ExternalSymbol, 0, len(defs))
	pkgExports := make(map[string][]analysis.ExternalSymbol)
	for key, sym := range defs {
		globals = append(globals, sym)
		pkg, name, _ := strings.Cut(key, "/")
		if exported[pkg][name] {
			pkgExports[pkg] = append(pkgExports[pkg], sym)
		}
	}
	return globals, pkgExports
}

func topLevelDef(expr *lisp.LVal, kind analysis.SymbolKind, pkg string) *analysis.ExternalSymbol {
	if len(expr.Cells) < 2 || expr.Cells[1].Type != lisp.LSymbol {
		return nil
	}
	return &analysis.ExternalSymbol{
		Name:    expr.Cells[1].Str,
		Kind:    kind,
		Package: pkg,
		Source:  expr.Cells[1].Source,
	}
}

func topLevelSet(expr *lisp.LVal, pkg string) *analysis.ExternalSymbol {
	if len(expr.Cells) < 2 {
		return nil
	}
	name := setName(expr.Cells[1])
	if name == "" {
		return nil
	}
	return &analysis.ExternalSymbol{
		Name:    name,
		Kind:    analysis.SymVariable,
		Package: pkg,
		Source:  expr.Cells[1].Source,
	}
}

func packageName(args []*lisp.LVal) string {
	if len(args) == 0 {
		return ""
	}
	arg := args[0]
	if arg.Type == lisp.LString || arg.Type == lisp.LSymbol {
		return arg.Str
	}
	if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		return arg.Cells[0].Str
	}
	return ""
}

func exportNames(args []*lisp.LVal) []string {
	out := make([]string, 0, len(args))
	for _, arg := range args {
		switch {
		case arg.Type == lisp.LSymbol:
			out = append(out, arg.Str)
		case arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol:
			out = append(out, arg.Cells[0].Str)
		}
	}
	return out
}

func setName(arg *lisp.LVal) string {
	if arg.Type == lisp.LSymbol {
		return arg.Str
	}
	if arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		return arg.Cells[0].Str
	}
	return ""
}

func buildAssignments(files []parsedFile, cfg *Config, exclusions map[string]bool) (map[*analysis.Symbol]string, map[string]string, SymbolMap) {
	type symbolRecord struct {
		sym *analysis.Symbol
	}

	var records []symbolRecord
	for _, file := range files {
		for _, sym := range file.analysis.Symbols {
			if !renameable(sym, cfg, exclusions) {
				continue
			}
			records = append(records, symbolRecord{sym: sym})
		}
	}

	sort.Slice(records, func(i, j int) bool {
		return compareSymbols(records[i].sym, records[j].sym) < 0
	})

	assignments := make(map[*analysis.Symbol]string, len(records))
	assignmentKeys := make(map[string]string, len(records))
	entries := make([]SymbolMapEntry, 0, len(records))
	minToOrig := make(map[string]string, len(records))
	origToMin := make(map[string][]string)

	for i, record := range records {
		newName := fmt.Sprintf("x%d", i+1)
		assignments[record.sym] = newName
		assignmentKeys[symbolLookupKey(record.sym)] = newName

		entry := SymbolMapEntry{
			Minified: newName,
			Original: record.sym.Name,
			Kind:     record.sym.Kind.String(),
		}
		if record.sym.Source != nil {
			entry.File = record.sym.Source.File
			entry.Line = record.sym.Source.Line
			entry.Col = record.sym.Source.Col
		}
		entries = append(entries, entry)
		minToOrig[newName] = record.sym.Name
		origToMin[record.sym.Name] = append(origToMin[record.sym.Name], newName)
	}

	for name := range origToMin {
		sort.Strings(origToMin[name])
	}

	return assignments, assignmentKeys, SymbolMap{
		Entries:            entries,
		MinifiedToOriginal: minToOrig,
		OriginalToMinified: origToMin,
	}
}

func applyAssignments(file *parsedFile, assignments map[*analysis.Symbol]string, assignmentKeys map[string]string, cfg *Config) {
	for sym, newName := range assignments {
		if sym.Node != nil && sym.Node.Type == lisp.LSymbol {
			sym.Node.Str = newName
		}
	}

	for _, ref := range file.analysis.References {
		newName, ok := assignments[ref.Symbol]
		if !ok {
			newName, ok = assignmentKeys[symbolLookupKey(ref.Symbol)]
		}
		if ok && ref.Node != nil && ref.Node.Type == lisp.LSymbol {
			ref.Node.Str = newName
		}
	}

	if cfg.RenameExports {
		rewriteExports(file.exprs, file.analysis.RootScope, assignments)
	}
}

func symbolLookupKey(sym *analysis.Symbol) string {
	if sym == nil {
		return ""
	}
	file := ""
	line := 0
	col := 0
	if sym.Source != nil {
		file = sym.Source.File
		line = sym.Source.Line
		col = sym.Source.Col
	}
	return fmt.Sprintf("%s|%s|%s|%d|%d", sym.Name, sym.Kind.String(), file, line, col)
}

func rewriteExports(exprs []*lisp.LVal, scope *analysis.Scope, assignments map[*analysis.Symbol]string) {
	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		if expr.Cells[0].Type != lisp.LSymbol || expr.Cells[0].Str != "export" {
			continue
		}
		for _, arg := range expr.Cells[1:] {
			switch {
			case arg.Type == lisp.LSymbol:
				if sym := scope.LookupLocal(arg.Str); sym != nil {
					if newName, ok := assignments[sym]; ok {
						arg.Str = newName
					}
				}
			case arg.Type == lisp.LSExpr && arg.Quoted && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol:
				if sym := scope.LookupLocal(arg.Cells[0].Str); sym != nil {
					if newName, ok := assignments[sym]; ok {
						arg.Cells[0].Str = newName
					}
				}
			}
		}
	}
}

func renameable(sym *analysis.Symbol, cfg *Config, exclusions map[string]bool) bool {
	if sym == nil || sym.Node == nil || sym.Node.Type != lisp.LSymbol {
		return false
	}
	if sym.External {
		return false
	}
	if sym.Kind == analysis.SymBuiltin || sym.Kind == analysis.SymSpecialOp {
		return false
	}
	if sym.Exported && !cfg.RenameExports {
		return false
	}
	name := sym.Name
	if name == "" || strings.Contains(name, ":") || strings.HasPrefix(name, ":") || strings.HasPrefix(name, "%") {
		return false
	}
	if exclusions != nil && exclusions[name] {
		return false
	}
	return true
}

func protectedMacroTemplateSymbols(files []parsedFile, base map[string]bool) map[string]bool {
	protected := make(map[string]bool, len(base))
	for name := range base {
		protected[name] = true
	}
	for i := range files {
		for _, expr := range files[i].exprs {
			collectProtectedMacroTemplateSymbols(expr, files[i].analysis.RootScope, protected)
		}
	}
	return protected
}

func collectProtectedMacroTemplateSymbols(expr *lisp.LVal, root *analysis.Scope, protected map[string]bool) {
	if expr == nil || root == nil || expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) < 4 {
		return
	}
	if expr.Cells[0].Type != lisp.LSymbol || expr.Cells[0].Str != "defmacro" {
		return
	}
	macroScope := findScopeForNode(root, expr)
	if macroScope == nil {
		return
	}
	for _, body := range expr.Cells[3:] {
		walkMacroBodyForQuasiquote(body, macroScope, protected)
	}
}

func walkMacroBodyForQuasiquote(node *lisp.LVal, scope *analysis.Scope, protected map[string]bool) {
	if node == nil || node.Type != lisp.LSExpr {
		return
	}
	if !node.Quoted && len(node.Cells) > 0 && node.Cells[0].Type == lisp.LSymbol && node.Cells[0].Str == "quasiquote" {
		if len(node.Cells) > 1 {
			collectTemplateSymbols(node.Cells[1], scope, protected)
		}
		return
	}
	for _, child := range node.Cells {
		walkMacroBodyForQuasiquote(child, scope, protected)
	}
}

func collectTemplateSymbols(node *lisp.LVal, scope *analysis.Scope, protected map[string]bool) {
	if node == nil {
		return
	}
	if node.Type == lisp.LSymbol {
		if preserveMacroTemplateSymbol(scope, node.Str) {
			protected[node.Str] = true
		}
		return
	}
	if node.Type != lisp.LSExpr {
		return
	}
	if !node.Quoted && len(node.Cells) > 0 && node.Cells[0].Type == lisp.LSymbol {
		switch node.Cells[0].Str {
		case "unquote", "unquote-splicing":
			return
		}
	}
	for _, child := range node.Cells {
		collectTemplateSymbols(child, scope, protected)
	}
}

func preserveMacroTemplateSymbol(scope *analysis.Scope, name string) bool {
	if name == "" ||
		strings.Contains(name, ":") ||
		strings.HasPrefix(name, ":") ||
		strings.HasPrefix(name, "%") {
		return false
	}
	sym := scope.Lookup(name)
	return sym != nil && sym.Scope != nil && sym.Scope.Kind == analysis.ScopeGlobal
}

func findScopeForNode(scope *analysis.Scope, node *lisp.LVal) *analysis.Scope {
	if scope == nil {
		return nil
	}
	if scope.Node == node {
		return scope
	}
	for _, child := range scope.Children {
		if found := findScopeForNode(child, node); found != nil {
			return found
		}
	}
	return nil
}

func compareSymbols(a, b *analysis.Symbol) int {
	if a == nil || b == nil {
		switch {
		case a == nil && b == nil:
			return 0
		case a == nil:
			return -1
		default:
			return 1
		}
	}

	if diff := compareLocations(a.Source, b.Source); diff != 0 {
		return diff
	}
	if a.Name != b.Name {
		return strings.Compare(a.Name, b.Name)
	}
	if a.Kind != b.Kind {
		return strings.Compare(a.Kind.String(), b.Kind.String())
	}
	return 0
}

func compareLocations(a, b *token.Location) int {
	switch {
	case a == nil && b == nil:
		return 0
	case a == nil:
		return -1
	case b == nil:
		return 1
	}

	if a.File != b.File {
		return strings.Compare(a.File, b.File)
	}
	if a.Line != b.Line {
		if a.Line < b.Line {
			return -1
		}
		return 1
	}
	if a.Col != b.Col {
		if a.Col < b.Col {
			return -1
		}
		return 1
	}
	return 0
}
