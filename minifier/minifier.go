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
	"slices"
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/astutil"
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
	Analysis            *analysis.Config
	Exclusions          map[string]bool
	RenameExports       bool
	PreserveParams      bool
	PackageSurfaceForms []PackageSurfaceFormSpec
	Formatter           *formatter.Config
	// Warn receives at most one global-name preservation warning per Minify call.
	Warn func(string)
}

// PackageSurfaceFormSpec declares a top-level form that creates a stable
// package-visible binding that should be preserved by default.
type PackageSurfaceFormSpec struct {
	Head       string
	NameIndex  int
	QuotedName bool
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

// SymbolExclusion records a name that cannot be shortened safely.
type SymbolExclusion struct {
	Original string `json:"original"`
	Reason   string `json:"reason"`
}

// SymbolMap is the machine-readable symbol mapping emitted by a minify run.
type SymbolMap struct {
	Excluded           []SymbolExclusion   `json:"excluded,omitempty"`
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
	globals  []analysis.ExternalSymbol
	packages map[string]bool
}

type preservationSet struct {
	globalFallback    *lisp.LVal
	dynamicEvaluation *lisp.LVal
	quoted            map[string]bool
	names             map[string]bool
	symbols           map[*analysis.Symbol]bool
	symbolKeys        map[string]bool
}

// Minify rewrites one or more source units using a single deterministic
// symbol-assignment session. Quoted names (including list and quasiquote data)
// are preserved across all inputs and recorded as quoted-reference exclusions.
// Package definitions retain package scope even inside lexical forms. Runtime
// evaluation or symbol creation disables all renaming across inputs. Unproven
// package flow preserves package-level names; lexical locals can shorten only
// when dynamic evaluation is absent. Both fallbacks record their reason.
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

	protected := buildPreservationSet(files, cfg)
	perFile, pkgExports := scanInputSymbols(files, cfg)
	for i := range files {
		fileCfg := mergeAnalysisConfig(cfg.Analysis, files[i].path, perFile, pkgExports)
		files[i].analysis = analysis.Analyze(files[i].exprs, fileCfg)
	}

	if cfg.Warn != nil {
		if node := protected.dynamicEvaluation; node != nil {
			cfg.Warn(fmt.Sprintf("%s: %s may evaluate runtime data or create symbols; preserving all binding names, including lexical locals (dynamic-evaluation)", astutil.SourceLoc(node), node.Str))
		} else if node := protected.globalFallback; node != nil {
			cfg.Warn(fmt.Sprintf("%s: %s prevents static proof of package flow and exported names; preserving all package-level binding names (unproven-package-flow)", astutil.SourceLoc(node), node.Str))
		}
	}
	preservePackageSurfaceSymbols(files, cfg, protected)
	assignments, assignmentKeys, symMap := buildAssignments(files, cfg, protected)
	for i := range files {
		applyAssignments(&files[i], assignments, assignmentKeys)
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
		cfg.DefForms = append(cfg.DefForms, base.DefForms...)
	}
	if cfg.PackageExports == nil {
		cfg.PackageExports = make(map[string][]analysis.ExternalSymbol)
	}
	for pkg, exports := range pkgExports {
		cfg.PackageExports[pkg] = append(cfg.PackageExports[pkg], exports...)
	}
	currentPackages := map[string]bool{"user": true}
	if symbols, ok := perFile[filename]; ok && len(symbols.packages) > 0 {
		currentPackages = symbols.packages
	}
	for path, symbols := range perFile {
		if path == filename {
			continue
		}
		for _, sym := range symbols.globals {
			if currentPackages[sym.Package] {
				cfg.ExtraGlobals = append(cfg.ExtraGlobals, sym)
			}
		}
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

func scanInputSymbols(files []parsedFile, cfg *Config) (map[string]fileSymbols, map[string][]analysis.ExternalSymbol) {
	perFile := make(map[string]fileSymbols, len(files))
	pkgExports := make(map[string][]analysis.ExternalSymbol)
	for _, file := range files {
		globals, exports, packages := scanProgramSymbols(file.exprs, cfg)
		perFile[file.path] = fileSymbols{globals: globals, packages: packages}
		for pkg, syms := range exports {
			pkgExports[pkg] = append(pkgExports[pkg], syms...)
		}
	}
	return perFile, pkgExports
}

func scanProgramSymbols(exprs []*lisp.LVal, cfg *Config) ([]analysis.ExternalSymbol, map[string][]analysis.ExternalSymbol, map[string]bool) {
	exprs = astutil.PackageForms(exprs)
	defs := make(map[string]analysis.ExternalSymbol)
	exported := make(map[string]map[string]bool)
	currentPkg := "user"
	packages := map[string]bool{"user": true}

	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 || expr.Cells[0].Type != lisp.LSymbol {
			continue
		}
		switch expr.Cells[0].Str {
		case "in-package":
			if pkg := packageName(expr.Cells[1:]); pkg != "" {
				currentPkg = pkg
				packages[pkg] = true
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
		case "export", "lisp:export":
			names := astutil.ExportNames(expr.Cells[1:])
			if len(names) == 0 {
				continue
			}
			if exported[currentPkg] == nil {
				exported[currentPkg] = make(map[string]bool)
			}
			for _, name := range names {
				exported[currentPkg][name] = true
			}
		default:
			if sym := topLevelCustomDef(expr, currentPkg, cfg); sym != nil {
				defs[currentPkg+"/"+sym.Name] = *sym
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
	return globals, pkgExports, packages
}

func topLevelDef(expr *lisp.LVal, kind analysis.SymbolKind, pkg string) *analysis.ExternalSymbol {
	if len(expr.Cells) < 2 || expr.Cells[1].Type != lisp.LSymbol {
		return nil
	}
	return &analysis.ExternalSymbol{
		Name:    expr.Cells[1].Str,
		Kind:    kind,
		Package: pkg,
		Source:  astutil.SymbolLoc(expr.Cells[1]),
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
		Source:  astutil.SymbolLoc(expr.Cells[1]),
	}
}

func topLevelCustomDef(expr *lisp.LVal, pkg string, cfg *Config) *analysis.ExternalSymbol {
	if cfg == nil || cfg.Analysis == nil || len(expr.Cells) == 0 {
		return nil
	}
	head := astutil.HeadSymbol(expr)
	for _, spec := range cfg.Analysis.DefForms {
		if spec.Head != head || !spec.BindsName || spec.NameIndex <= 0 || spec.NameIndex >= len(expr.Cells) {
			continue
		}
		if expr.Cells[spec.NameIndex].Type != lisp.LSymbol {
			continue
		}
		kind := spec.NameKind
		if kind != analysis.SymVariable &&
			kind != analysis.SymFunction &&
			kind != analysis.SymMacro &&
			kind != analysis.SymType {
			kind = analysis.SymFunction
		}
		return &analysis.ExternalSymbol{
			Name:    expr.Cells[spec.NameIndex].Str,
			Kind:    kind,
			Package: pkg,
			Source:  astutil.SymbolLoc(expr.Cells[spec.NameIndex]),
		}
	}
	return nil
}

func packageName(args []*lisp.LVal) string {
	if len(args) == 0 {
		return ""
	}
	arg := args[0]
	if arg.Type == lisp.LString || arg.Type == lisp.LSymbol {
		return arg.Str
	}
	if arg.Type == lisp.LSExpr && arg.IsQuoted() && len(arg.Cells) > 0 && arg.Cells[0].Type == lisp.LSymbol {
		return arg.Cells[0].Str
	}
	return ""
}

// setName is the name (set ...) binds, or "" when its first arg names nothing.
func setName(arg *lisp.LVal) string {
	if node := setSymbolNode(arg); node != nil {
		return node.Str
	}
	return ""
}

// setSymbolNode returns the node naming the binding in the first arg of set.
// Only a quoted symbol names a static binding; bare symbols and compound
// targets are evaluated. Keep this aligned with analysis so the scanner
// never publishes a computed target as a cross-file definition.
func setSymbolNode(arg *lisp.LVal) *lisp.LVal {
	if arg.Type == lisp.LSymbol && arg.IsQuoted() {
		return arg
	}
	return nil
}

func buildAssignments(files []parsedFile, cfg *Config, preserved *preservationSet) (map[*analysis.Symbol]string, map[string]string, SymbolMap) {
	type symbolRecord struct {
		sym *analysis.Symbol
	}

	// Every definition of a package binding must make the same preservation
	// decision, even when redefinitions have different kinds or source files.
	preservedBindings := make(map[string]bool)
	for _, file := range files {
		for _, sym := range file.analysis.Symbols {
			if key := packageBindingKey(sym); key != "" && !sym.External && !renameable(sym, cfg, preserved) {
				preservedBindings[key] = true
			}
		}
	}
	// Reserve every surviving name before allocating any replacement. This
	// includes kind- and option-based preservation and package redefinitions,
	// and prevents new lexical bindings from capturing preserved references.
	reserved := make(map[string]bool, len(preserved.names))
	for name := range preserved.names {
		reserved[name] = true
	}
	var records []symbolRecord
	for _, file := range files {
		for _, sym := range file.analysis.Symbols {
			if !renameable(sym, cfg, preserved) || preservedBindings[packageBindingKey(sym)] {
				reserved[sym.Name] = true
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

	// Definition locations identify source occurrences, but redefinitions still
	// write one runtime package binding. Keep their assignments identical.
	packageNames := make(map[string]string)
	next := 1
	for _, record := range records {
		binding := packageBindingKey(record.sym)
		if newName, ok := packageNames[binding]; ok {
			assignments[record.sym] = newName
			assignmentKeys[symbolLookupKey(record.sym)] = newName
			continue
		}
		newName := fmt.Sprintf("x%d", next)
		for reserved[newName] {
			next++
			newName = fmt.Sprintf("x%d", next)
		}
		next++
		assignments[record.sym] = newName
		assignmentKeys[symbolLookupKey(record.sym)] = newName
		if binding != "" {
			packageNames[binding] = newName
		}

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
		slices.Sort(origToMin[name])
	}

	exclusionReasons := make(map[string]string)
	for name := range preserved.quoted {
		exclusionReasons[name] = "quoted-reference"
	}
	if preserved.globalFallback != nil || preserved.dynamicEvaluation != nil {
		for _, file := range files {
			for _, sym := range file.analysis.Symbols {
				if !sym.External && (preserved.dynamicEvaluation != nil || packageBindingKey(sym) != "") {
					reason := "unproven-package-flow"
					if preserved.dynamicEvaluation != nil {
						reason = "dynamic-evaluation"
					}
					exclusionReasons[sym.Name] = reason
				}
			}
		}
	}
	var excluded []SymbolExclusion
	for name, reason := range exclusionReasons {
		excluded = append(excluded, SymbolExclusion{Original: name, Reason: reason})
	}
	sort.Slice(excluded, func(i, j int) bool { return excluded[i].Original < excluded[j].Original })
	return assignments, assignmentKeys, SymbolMap{
		Excluded:           excluded,
		Entries:            entries,
		MinifiedToOriginal: minToOrig,
		OriginalToMinified: origToMin,
	}
}

func packageBindingKey(sym *analysis.Symbol) string {
	if sym == nil || sym.Scope == nil || sym.Scope.Kind != analysis.ScopeGlobal || sym.Package == "" {
		return ""
	}
	return sym.Package + ":" + sym.Name
}

func applyAssignments(file *parsedFile, assignments map[*analysis.Symbol]string, assignmentKeys map[string]string) {
	// Iterate the file's symbols in analysis order rather than ranging over
	// the assignments MAP.  Two distinct *analysis.Symbol records can share
	// one AST node -- `(defun f (e 'e))` registers the parameter twice at the
	// same location -- so these writes are order dependent, and Go randomises
	// map iteration: the same source minified to `(defun x1 (x2 'e))` on one
	// run and `(defun x1 (x3 'e))` on the next.
	//
	// That breaks the determinism guarantee TestMinifySource_DeterministicAnd-
	// ScopeAware pins, and determinism is what makes a phylum build
	// reproducible and its symbol map usable for decoding a stack trace from
	// a deployed chaincode.  Found by FuzzMinifySource on "(())(defun 2(e'e))".
	//
	// Ranging over this file's symbols is also strictly less work: the caller
	// invokes applyAssignments once per file, so the old loop re-applied every
	// other file's assignments on every pass.
	for _, sym := range file.analysis.Symbols {
		newName, ok := assignments[sym]
		if !ok {
			continue
		}
		if sym.Node != nil && sym.Node.Type == lisp.LSymbol {
			sym.Node.Str = newName //elps:mutates the minifier renames symbols in the AST it parsed for this run; the tree is tool-owned and never shared with an evaluator
		}
	}

	for _, ref := range file.analysis.References {
		newName, ok := assignments[ref.Symbol]
		if !ok {
			newName, ok = assignmentKeys[symbolLookupKey(ref.Symbol)]
		}
		if ok && ref.Node != nil && ref.Node.Type == lisp.LSymbol {
			rewriteReferenceNode(ref.Node, newName)
		}
	}
}

// symbolLookupKey is a byte-POSITION identity for a definition: two *Symbol
// values that came from different analyses of the same file are the same
// symbol iff this key matches.  It is what lets applyAssignments resolve a
// reference in one file to a definition scanned out of another.
//
// It therefore has TWO independent producers that have to measure a name's
// span identically: scanProgramSymbols in this package (topLevelDef /
// topLevelSet / topLevelCustomDef, which fill ExternalSymbol.Source) and
// analysis, which fills Symbol.Source while analysing a file.  When they
// diverge the lookup does not error -- it simply misses, and the minifier
// renames a definition while leaving every cross-file caller on the old name.
// That is elps#577's cross-file half, where analysis moved to
// astutil.SymbolLoc (name only) and this package still used astutil.SourceLoc
// (which includes a reader quote), so the two differed by one byte for every
// quoted name.
//
// TestScannerAndAnalysisAgreeOnDefinitionSource pins the two producers
// together; keep it covering any new definition shape either side learns.
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

func renameable(sym *analysis.Symbol, cfg *Config, preserved *preservationSet) bool {
	if preserved != nil && preserved.dynamicEvaluation != nil {
		return false
	}
	if sym == nil || sym.Node == nil || sym.Node.Type != lisp.LSymbol {
		return false
	}
	if sym.External {
		return false
	}
	if sym.Kind == analysis.SymBuiltin || sym.Kind == analysis.SymSpecialOp {
		return false
	}
	// Type names become runtime tags, visible in debug-print and serialized
	// values even when all source references belong to this minification run.
	if sym.Kind == analysis.SymType {
		return false
	}
	if sym.Exported && !cfg.RenameExports {
		return false
	}
	if cfg.PreserveParams && sym.Kind == analysis.SymParameter {
		return false
	}
	name := sym.Name
	if name == "" || strings.Contains(name, ":") || strings.HasPrefix(name, ":") || strings.HasPrefix(name, "%") {
		return false
	}
	if sym.Scope != nil && sym.Scope.Kind == analysis.ScopeGlobal {
		if preserved != nil && preserved.globalFallback != nil {
			return false
		}
		switch sym.Kind {
		case analysis.SymMacro, analysis.SymVariable:
			return false
		case analysis.SymFunction, analysis.SymParameter,
			analysis.SymSpecialOp, analysis.SymBuiltin, analysis.SymType:
			// Renameable at global scope. SymBuiltin, SymSpecialOp and SymType
			// are rejected above; SymParameter cannot be global.
		}
	}
	if preserved != nil && preserved.names[name] {
		return false
	}
	if preserved != nil && (preserved.symbols[sym] || preserved.symbolKeys[symbolLookupKey(sym)]) {
		return false
	}
	return true
}

func buildPreservationSet(files []parsedFile, cfg *Config) *preservationSet {
	protected := &preservationSet{
		names:      make(map[string]bool),
		quoted:     make(map[string]bool),
		symbols:    make(map[*analysis.Symbol]bool),
		symbolKeys: make(map[string]bool),
	}
	if cfg != nil {
		for name := range cfg.Exclusions {
			protected.names[name] = true
		}
	}
	for i := range files {
		for _, expr := range files[i].exprs {
			collectQuotedSymbols(expr, false, protected)
			if protected.dynamicEvaluation == nil {
				protected.dynamicEvaluation = firstDynamicEvaluation(expr)
			}
			if protected.globalFallback == nil {
				protected.globalFallback = firstGlobalFallback(expr, true, false)
			}
		}
	}
	return protected
}

func preservePackageSurfaceSymbols(files []parsedFile, cfg *Config, protected *preservationSet) {
	qualifiedRefs := make(map[string]bool)
	for i := range files {
		recordFileQualifiedReferences(files[i].exprs, qualifiedRefs)
	}
	for i := range files {
		currentPkg := "user"
		for _, expr := range astutil.PackageForms(files[i].exprs) {
			if expr.Type != lisp.LSExpr || expr.IsQuoted() || len(expr.Cells) == 0 {
				continue
			}
			if expr.Cells[0].Type == lisp.LSymbol && expr.Cells[0].Str == "in-package" {
				if pkg := packageName(expr.Cells[1:]); pkg != "" {
					currentPkg = pkg
				}
			}
			if expr.Cells[0].Type != lisp.LSymbol {
				continue
			}
			switch expr.Cells[0].Str {
			case "export", "lisp:export":
				// Literal export names are runtime data, including strings and
				// nested lists. Preserve them even with RenameExports enabled.
				for _, name := range astutil.ExportNames(expr.Cells[1:]) {
					protected.names[name] = true
				}
			case "defmacro":
				if len(expr.Cells) > 1 {
					preserveNodeSymbol(files[i].analysis, expr.Cells[1], protected)
				}
			case "set":
				if len(expr.Cells) > 1 {
					preserveNodeSymbol(files[i].analysis, setSymbolNode(expr.Cells[1]), protected)
				}
			case "defun", "deftype":
				if len(expr.Cells) > 1 && expr.Cells[1].Type == lisp.LSymbol {
					if qualifiedRefs[currentPkg+"/"+expr.Cells[1].Str] {
						preserveQualifiedDefinitionNode(files[i].analysis, expr.Cells[1], cfg, protected)
					}
				}
			default:
				if node := configuredTopLevelNameNode(expr, cfg); node != nil && qualifiedRefs[currentPkg+"/"+node.Str] {
					preserveQualifiedDefinitionNode(files[i].analysis, node, cfg, protected)
				}
			}
			if expr.Cells[0].Str == "set" && len(expr.Cells) > 1 {
				if name := setName(expr.Cells[1]); name != "" && qualifiedRefs[currentPkg+"/"+name] {
					preserveNodeSymbol(files[i].analysis, setSymbolNode(expr.Cells[1]), protected)
				}
			}
			preserveConfiguredPackageSurfaceSymbol(files[i].analysis, expr, cfg, protected)
		}
	}
}

func recordFileQualifiedReferences(exprs []*lisp.LVal, refs map[string]bool) {
	for _, expr := range exprs {
		recordQualifiedReferences(expr, refs)
	}
}

func preserveConfiguredPackageSurfaceSymbol(result *analysis.Result, expr *lisp.LVal, cfg *Config, protected *preservationSet) {
	if result == nil || expr == nil || protected == nil || len(expr.Cells) == 0 {
		return
	}
	head := astutil.HeadSymbol(expr)
	for _, spec := range packageSurfaceForms(cfg) {
		if spec.Head != head {
			continue
		}
		node := packageSurfaceSymbolNode(expr, spec)
		if node == nil {
			continue
		}
		preserveNodeSymbol(result, node, protected)
	}
}

func packageSurfaceForms(cfg *Config) []PackageSurfaceFormSpec {
	if cfg == nil {
		return nil
	}
	return cfg.PackageSurfaceForms
}

func packageSurfaceSymbolNode(expr *lisp.LVal, spec PackageSurfaceFormSpec) *lisp.LVal {
	if expr == nil || spec.NameIndex <= 0 || spec.NameIndex >= len(expr.Cells) {
		return nil
	}
	arg := expr.Cells[spec.NameIndex]
	if spec.QuotedName {
		return setSymbolNode(arg)
	}
	if arg.Type == lisp.LSymbol && !arg.IsQuoted() {
		return arg
	}
	return nil
}

func configuredTopLevelNameNode(expr *lisp.LVal, cfg *Config) *lisp.LVal {
	if expr == nil || len(expr.Cells) == 0 || cfg == nil || cfg.Analysis == nil {
		return nil
	}
	head := astutil.HeadSymbol(expr)
	for _, spec := range cfg.Analysis.DefForms {
		if spec.Head != head || !spec.BindsName || spec.NameIndex <= 0 || spec.NameIndex >= len(expr.Cells) {
			continue
		}
		node := expr.Cells[spec.NameIndex]
		if node.Type == lisp.LSymbol && !node.IsQuoted() {
			return node
		}
	}
	return nil
}

func preserveNodeSymbol(result *analysis.Result, node *lisp.LVal, protected *preservationSet) {
	if sym := nodeSymbol(result, node); sym != nil {
		protected.symbols[sym] = true
		protected.symbolKeys[symbolLookupKey(sym)] = true
	}
}

func preserveQualifiedDefinitionNode(result *analysis.Result, node *lisp.LVal, cfg *Config, protected *preservationSet) {
	sym := nodeSymbol(result, node)
	if sym == nil {
		return
	}
	if sym.Exported && cfg != nil && cfg.RenameExports {
		return
	}
	protected.symbols[sym] = true
	protected.symbolKeys[symbolLookupKey(sym)] = true
}

func nodeSymbol(result *analysis.Result, node *lisp.LVal) *analysis.Symbol {
	if result == nil || node == nil || node.Type != lisp.LSymbol {
		return nil
	}
	for _, sym := range result.Symbols {
		if sym != nil && sym.Node == node {
			return sym
		}
	}
	return nil
}

// collectQuotedSymbols deliberately protects names across scopes and files.
// ELPS accepts quoted function designators, and templates can emit references
// that static lexical resolution cannot see. Even unquote names are retained:
// reserving the whole template is the conservative policy.
func collectQuotedSymbols(node *lisp.LVal, quoted bool, protected *preservationSet) {
	if node == nil {
		return
	}
	quoted = quoted || node.IsQuoted() || node.Type == lisp.LQuote
	if node.Type == lisp.LSymbol && quoted {
		protected.names[node.Str] = true
		protected.quoted[node.Str] = true
		if _, name, ok := splitQualifiedSymbol(node.Str); ok {
			protected.names[name] = true
			protected.quoted[name] = true
		}
	}
	head := astutil.HeadSymbol(node)
	if head == "quote" || head == "lisp:quote" || head == "quasiquote" || head == "lisp:quasiquote" {
		quoted = true
	}
	for _, child := range node.Cells {
		collectQuotedSymbols(child, quoted, protected)
	}
}

func recordQualifiedReferences(node *lisp.LVal, refs map[string]bool) {
	if node == nil {
		return
	}
	switch node.Type {
	case lisp.LSymbol:
		if pkg, name, ok := splitQualifiedSymbol(node.Str); ok {
			refs[pkg+"/"+name] = true
		}
	case lisp.LSExpr:
		if node.IsQuoted() {
			return
		}
		for _, child := range node.Cells {
			recordQualifiedReferences(child, refs)
		}
	case lisp.LInvalid, lisp.LInt, lisp.LFloat, lisp.LError, lisp.LQSymbol,
		lisp.LFun, lisp.LQuote, lisp.LString, lisp.LBytes, lisp.LSortMap,
		lisp.LArray, lisp.LNative, lisp.LTaggedVal, lisp.LMarkTerminal,
		lisp.LMarkTailRec, lisp.LMarkMacExpand, lisp.LTypeMax:
		// Literals carry no package qualification. Quoted names are handled
		// separately by collectQuotedSymbols. The remaining types
		// are runtime-only values that never appear in a parsed file.
	}
}

func rewriteReferenceNode(node *lisp.LVal, newName string) {
	if node == nil || node.Type != lisp.LSymbol {
		return
	}
	if pkg, _, ok := splitQualifiedSymbol(node.Str); ok {
		node.Str = pkg + ":" + newName //elps:mutates the minifier renames symbols in the AST it parsed for this run; the tree is tool-owned and never shared with an evaluator
		return
	}
	node.Str = newName //elps:mutates the minifier renames symbols in the AST it parsed for this run; the tree is tool-owned and never shared with an evaluator
}

func splitQualifiedSymbol(name string) (string, string, bool) {
	if name == "" || strings.HasPrefix(name, ":") {
		return "", "", false
	}
	for i := 1; i < len(name); i++ {
		if name[i] == ':' {
			if i+1 >= len(name) {
				return "", "", false
			}
			return name[:i], name[i+1:], true
		}
	}
	return "", "", false
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

// firstGlobalFallback requires static proof of package flow and exported names
// before any package binding may be renamed. Walk the original file tree, not
// PackageForms: flattening it would lose the top-level and template context.
// Package forms in macro definitions or quasiquotes cannot supply proof: their
// generated code may export names that differ from the template's literal data.
func firstGlobalFallback(node *lisp.LVal, topLevel, template bool) *lisp.LVal {
	if node == nil {
		return nil
	}
	head := strings.TrimPrefix(astutil.HeadSymbol(node), "lisp:")
	switch head {
	case "defmacro", "macrolet", "quasiquote":
		// Conservatively treat all contents as templates, including quoted
		// data and unquotes; neither can restore directly evaluated context.
		template = true
	case "export":
		if template {
			return node.Cells[0]
		}
		for _, arg := range node.Cells[1:] {
			if !literalExportArgument(arg, false) {
				return node.Cells[0]
			}
		}
	case "in-package", "use-package":
		// The package scanner models the unqualified spellings only. A
		// qualified call therefore cannot supply the proof needed to rename.
		if template || !topLevel || astutil.HeadSymbol(node) != head {
			return node.Cells[0]
		}
		args := node.Cells[1:]
		if head == "in-package" {
			if len(args) == 0 {
				return node.Cells[0]
			}
			args = args[:1] // Remaining arguments are package docstrings.
		}
		for _, arg := range args {
			if arg.Type != lisp.LString && (arg.Type != lisp.LSymbol || !arg.IsQuoted()) {
				return node.Cells[0]
			}
		}
	}
	for _, child := range node.Cells {
		if found := firstGlobalFallback(child, false, template); found != nil {
			return found
		}
	}
	return nil
}

// firstDynamicEvaluation scans independently of package flow so an earlier
// package fallback cannot hide runtime access to lexical bindings.
func firstDynamicEvaluation(node *lisp.LVal) *lisp.LVal {
	if node == nil {
		return nil
	}
	if node.Type == lisp.LSymbol {
		name := strings.TrimPrefix(node.Str, "lisp:")
		switch name {
		case "load-string", "load-bytes", "load-file", "eval", "symbol", "intern",
			"macroexpand", "macroexpand-1", "gensym", "type", "qualified-symbol":
			// symbol/intern are included for host-provided implementations;
			// the core currently has no string-to-symbol builtin by those names.
			return node
		}
	}
	for _, child := range node.Cells {
		if found := firstDynamicEvaluation(child); found != nil {
			return found
		}
	}
	return nil
}

// literalExportArgument proves the entire value, including every nested list
// element. Extracting only the known names would silently miss computed exports.
// For directly evaluated exports, only reader quoting (IsQuoted) supplies quote
// proof. Unqualified quote can be shadowed. Qualified lisp:quote cannot be
// lexically shadowed and the standard runtime seals its package against Lisp
// writes, but an embedder can register a different lisp package before sealing;
// the minifier cannot assume the standard runtime's implementation.
func literalExportArgument(node *lisp.LVal, literal bool) bool {
	if node == nil {
		return false
	}
	literal = literal || node.IsQuoted()
	switch node.Type {
	case lisp.LString:
		return true
	case lisp.LSymbol:
		return literal
	case lisp.LSExpr:
		if !literal && len(node.Cells) != 0 {
			return false
		}
		for _, child := range node.Cells {
			if !literalExportArgument(child, true) {
				return false
			}
		}
		return true
	default:
		return false
	}
}
