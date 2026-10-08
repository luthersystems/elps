// Copyright © 2026 The ELPS authors

package nativepayload

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/analysis"
)

// justifiedAllow reports whether a comment's text is marker followed by
// whitespace and at least minWords words.  It is cmd/elpsvet's matcher.
func justifiedAllow(text, marker string, minWords int) bool {
	text = strings.TrimPrefix(text, "//")
	text = strings.TrimPrefix(text, "/*")
	text = strings.TrimSuffix(text, "*/")
	text = strings.TrimSpace(text)
	rest, ok := strings.CutPrefix(text, marker)
	if !ok || rest == "" {
		return false
	}
	if rest[0] != ' ' && rest[0] != '\t' {
		return false // a different marker sharing the prefix
	}
	return len(strings.Fields(rest)) >= minWords
}

// markerLinesMatching collects the file lines a marker comment SUPPRESSES,
// with cmd/elpsvet's placement convention:
//
//   - a STANDALONE marker (nothing but the comment on its line) suppresses
//     its own line and the next one;
//   - a TRAILING marker (sharing its line with code) suppresses that line
//     only.
func markerLinesMatching(fset *token.FileSet, file *ast.File, match func(text string) bool) map[int]bool {
	code := codeLines(fset, file)
	lines := make(map[int]bool)
	for _, cg := range file.Comments {
		for _, c := range cg.List {
			if !match(c.Text) {
				continue
			}
			line := fset.Position(c.Pos()).Line
			lines[line] = true
			if !code[line] {
				lines[line+1] = true
			}
		}
	}
	return lines
}

// codeLines reports which lines of file carry a non-comment token, so a
// marker comment can be classified as trailing or standalone.
func codeLines(fset *token.FileSet, file *ast.File) map[int]bool {
	lines := make(map[int]bool)
	ast.Inspect(file, func(n ast.Node) bool {
		if n == nil {
			return false
		}
		switch n.(type) {
		case *ast.CommentGroup, *ast.Comment:
			return false
		}
		lines[fset.Position(n.Pos()).Line] = true
		lines[fset.Position(n.End()).Line] = true
		return true
	})
	return lines
}

// calleeFunc resolves a call's callee to its *types.Func, so package aliases
// and dot imports resolve like the compiler resolves them rather than by
// matching the source text "lisp.Native".  An EXPLICITLY instantiated
// generic -- lisp.NativeOf[*Handle](h) -- wraps the callee in an index
// expression (IndexExpr for one type argument, IndexListExpr for several),
// which is unwrapped first; missing that would leave a spelling the rule
// cannot see.
func calleeFunc(pass *analysis.Pass, call *ast.CallExpr) *types.Func {
	fun := ast.Unparen(call.Fun)
	switch idx := fun.(type) {
	case *ast.IndexExpr:
		fun = ast.Unparen(idx.X)
	case *ast.IndexListExpr:
		fun = ast.Unparen(idx.X)
	}
	var id *ast.Ident
	switch fun := fun.(type) {
	case *ast.Ident:
		id = fun
	case *ast.SelectorExpr:
		id = fun.Sel
	default:
		return nil
	}
	fn, _ := pass.TypesInfo.Uses[id].(*types.Func)
	return fn
}
