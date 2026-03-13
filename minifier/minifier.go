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
		if cfg.Analysis != nil {
			fileCfg := *cfg.Analysis
			fileCfg.Filename = input.Path
			file.analysis = analysis.Analyze(file.exprs, &fileCfg)
		} else {
			file.analysis = analysis.Analyze(file.exprs, &analysis.Config{Filename: input.Path})
		}
		files = append(files, file)
	}

	assignments, symMap := buildAssignments(files, cfg)
	for i := range files {
		applyAssignments(&files[i], assignments, cfg)
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
	defer f.Close()

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

func buildAssignments(files []parsedFile, cfg *Config) (map[*analysis.Symbol]string, SymbolMap) {
	type symbolRecord struct {
		sym *analysis.Symbol
	}

	var records []symbolRecord
	for _, file := range files {
		for _, sym := range file.analysis.Symbols {
			if !renameable(sym, cfg) {
				continue
			}
			records = append(records, symbolRecord{sym: sym})
		}
	}

	sort.Slice(records, func(i, j int) bool {
		return compareSymbols(records[i].sym, records[j].sym) < 0
	})

	assignments := make(map[*analysis.Symbol]string, len(records))
	entries := make([]SymbolMapEntry, 0, len(records))
	minToOrig := make(map[string]string, len(records))
	origToMin := make(map[string][]string)

	for i, record := range records {
		newName := fmt.Sprintf("x%d", i+1)
		assignments[record.sym] = newName

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

	return assignments, SymbolMap{
		Entries:            entries,
		MinifiedToOriginal: minToOrig,
		OriginalToMinified: origToMin,
	}
}

func applyAssignments(file *parsedFile, assignments map[*analysis.Symbol]string, cfg *Config) {
	for sym, newName := range assignments {
		if sym.Node != nil && sym.Node.Type == lisp.LSymbol {
			sym.Node.Str = newName
		}
	}

	for _, ref := range file.analysis.References {
		if newName, ok := assignments[ref.Symbol]; ok && ref.Node != nil && ref.Node.Type == lisp.LSymbol {
			ref.Node.Str = newName
		}
	}

	if cfg.RenameExports {
		rewriteExports(file.exprs, file.analysis.RootScope, assignments)
	}
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

func renameable(sym *analysis.Symbol, cfg *Config) bool {
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
	if cfg.Exclusions != nil && cfg.Exclusions[name] {
		return false
	}
	return true
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
