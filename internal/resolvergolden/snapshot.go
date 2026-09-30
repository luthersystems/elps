// Copyright © 2026 The ELPS authors

// Package resolvergolden records resolver output for cross-version parity
// tests. The generator is run against origin/main, never the new resolver.
package resolvergolden

import (
	"crypto/sha256"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/luthersystems/elps/analysis"
	"github.com/luthersystems/elps/internal/fuzzseed"
	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
)

// Input is one source file or named fuzz seed.
type Input struct {
	Name   string
	Source []byte
}

// Inputs collects repository Lisp files and all small parser/evaluator seeds.
// Pathological inputs are stress tests rather than fuzz seeds.
func Inputs(root string) ([]Input, error) {
	var inputs []Input
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if strings.HasPrefix(d.Name(), ".") || d.Name() == "node_modules" || path == filepath.Join(root, "internal", "analysisbench") {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".lisp") {
			return nil
		}
		src, err := os.ReadFile(path) //nolint:gosec // repository fixture selected by WalkDir
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		inputs = append(inputs, Input{filepath.ToSlash(rel), src})
		return nil
	})
	if err != nil {
		return nil, err
	}
	for i, src := range fuzzseed.Adversarial() {
		inputs = append(inputs, Input{fmt.Sprintf("fuzz/parser/%03d", i), src})
	}
	for i, src := range fuzzseed.EvalAdversarial() {
		inputs = append(inputs, Input{fmt.Sprintf("fuzz/eval/%03d", i), []byte(src)})
	}
	for group, seeds := range map[string]map[string]string{
		"terminating": fuzzseed.EvalTerminating(), "erroring": fuzzseed.EvalErroring(), "runaway": fuzzseed.EvalRunaway(),
	} {
		for name, src := range seeds {
			inputs = append(inputs, Input{"fuzz/" + group + "/" + name, []byte(src)})
		}
	}
	sort.Slice(inputs, func(i, j int) bool { return inputs[i].Name < inputs[j].Name })
	return inputs, nil
}

// FixtureInputs reads the frozen copies of the golden's inputs under dir:
// each input is stored as <name>.input, so the parity test compares the same
// bytes the golden was recorded from even after the repository changes.
func FixtureInputs(dir string) ([]Input, error) {
	var inputs []Input
	err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		if !strings.HasSuffix(path, ".input") {
			return fmt.Errorf("unexpected file in resolver fixtures: %s", path)
		}
		src, err := os.ReadFile(path) //nolint:gosec // test fixture selected by WalkDir
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(dir, path)
		if err != nil {
			return err
		}
		inputs = append(inputs, Input{strings.TrimSuffix(filepath.ToSlash(rel), ".input"), src})
		return nil
	})
	if err != nil {
		return nil, err
	}
	sort.Slice(inputs, func(i, j int) bool { return inputs[i].Name < inputs[j].Name })
	return inputs, nil
}

// Snapshot dumps symbols, references, unresolved references and the complete
// scope tree, including source spans. Only unordered maps and expr's inferred
// parameter declarations are sorted; reference order remains part of parity.
func Snapshot(input Input) string {
	var out strings.Builder
	fmt.Fprintf(&out, "input %s sha256=%x\n", input.Name, sha256.Sum256(input.Source))
	exprs, err := rdparser.New(token.NewScanner(input.Name, strings.NewReader(string(input.Source)))).ParseProgram()
	if err != nil {
		fmt.Fprintf(&out, "parse-error %s\n", err)
		return out.String()
	}
	r := analysis.Analyze(exprs, &analysis.Config{Filename: input.Name})
	scopes := make(map[*analysis.Scope]int)
	var visit func(*analysis.Scope)
	visit = func(s *analysis.Scope) {
		id := len(scopes)
		scopes[s] = id
		parent := -1
		if s.Parent != nil {
			parent = scopes[s.Parent]
		}
		fmt.Fprintf(&out, "scope %d %s parent=%d node=%s\n", id, s.Kind, parent, node(s.Node))
		for _, child := range s.Children {
			visit(child)
		}
	}
	visit(r.RootScope)
	symbol := func(s *analysis.Symbol) string {
		sig := "-"
		if s.Signature != nil {
			sig = fmt.Sprint(s.Signature.Params)
		}
		return fmt.Sprintf("%s:%s %s scope=%d source=%s node=%s init=%s sig=%s doc=%q exported=%t external=%t refs=%d", s.Package, s.Name, s.Kind, scopes[s.Scope], location(s.Source), node(s.Node), node(s.Init), sig, s.DocString, s.Exported, s.External, s.References)
	}
	var declarations []string
	for _, s := range r.Symbols {
		declarations = append(declarations, "symbol "+symbol(s))
	}
	sort.Strings(declarations)
	for _, s := range declarations {
		fmt.Fprintln(&out, s)
	}
	var maps []string
	for s, id := range scopes {
		for key, v := range s.Symbols {
			if v.Source != nil || v.External {
				maps = append(maps, fmt.Sprintf("map %d %s %s", id, key, symbol(v)))
			}
		}
		for key, v := range s.PackageSymbols {
			if v.Source != nil || v.External {
				maps = append(maps, fmt.Sprintf("package %d %s %s", id, key, symbol(v)))
			}
		}
		for pkg, imports := range s.PackageImports {
			for key, v := range imports {
				maps = append(maps, fmt.Sprintf("import %d %s:%s %s", id, pkg, key, symbol(v)))
			}
		}
	}
	sort.Strings(maps)
	for _, m := range maps {
		fmt.Fprintln(&out, m)
	}
	for _, ref := range r.References {
		fmt.Fprintf(&out, "ref %s:%s scope=%d source=%s node=%s\n", ref.Symbol.Package, ref.Symbol.Name, scopes[ref.Symbol.Scope], location(ref.Source), node(ref.Node))
	}
	for _, ref := range r.Unresolved {
		fmt.Fprintf(&out, "unresolved %s macro=%t source=%s node=%s\n", ref.Name, ref.InsideMacroCall, location(ref.Source), node(ref.Node))
	}
	return out.String()
}

func location(loc *token.Location) string {
	if loc == nil {
		return "-"
	}
	return fmt.Sprintf("%d:%d:%d-%d:%d:%d", loc.Pos, loc.Line, loc.Col, loc.EndPos, loc.EndLine, loc.EndCol)
}

func node(v *lisp.LVal) string {
	if v == nil {
		return "-"
	}
	text := v.Str
	if v.Type == lisp.LSExpr && len(v.Cells) > 0 {
		text = v.Cells[0].Str
	}
	loc, ok := v.Source()
	pos := "-"
	if ok {
		pos = location(&loc)
	}
	return fmt.Sprintf("%s:%q quoted=%t@%s", v.Type, text, v.IsQuoted(), pos)
}
