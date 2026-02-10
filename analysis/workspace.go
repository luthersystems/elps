// Copyright © 2024 The ELPS authors

package analysis

import (
	"bytes"
	"os"
	"path/filepath"

	"github.com/luthersystems/elps/astutil"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// ScanWorkspace walks a directory tree, parsing all .lisp files and
// extracting exported top-level definitions. The result can be used as
// Config.ExtraGlobals for cross-file symbol resolution.
//
// Files that fail to parse are silently skipped (fault tolerant).
func ScanWorkspace(root string) ([]ExternalSymbol, error) {
	var syms []ExternalSymbol
	err := filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil // skip unreadable dirs
		}
		if info.IsDir() {
			return nil
		}
		if filepath.Ext(path) != ".lisp" {
			return nil
		}
		fileSrc, err := os.ReadFile(path) //nolint:gosec // CLI tool reads user-specified files
		if err != nil {
			return nil // skip unreadable files
		}
		fileSyms := scanFile(fileSrc, path)
		syms = append(syms, fileSyms...)
		return nil
	})
	if err != nil {
		return nil, err
	}
	return syms, nil
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

	for _, expr := range exprs {
		if expr.Type != lisp.LSExpr || expr.Quoted || len(expr.Cells) == 0 {
			continue
		}
		head := astutil.HeadSymbol(expr)
		switch head {
		case "defun":
			if sym := scanDefun(expr, SymFunction); sym != nil {
				defs[sym.Name] = sym
			}
		case "defmacro":
			if sym := scanDefun(expr, SymMacro); sym != nil {
				defs[sym.Name] = sym
			}
		case "set":
			if sym := scanSet(expr); sym != nil {
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
		Source:    nameVal.Source,
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
