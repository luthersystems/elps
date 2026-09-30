// Copyright © 2026 The ELPS authors

package lisp

import (
	"go/ast"
	"go/parser"
	"go/token"
	"sort"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// kindConsts returns the formKind constants declared in codewalk.go.
func kindConsts(t *testing.T) (map[string]bool, *ast.File) {
	t.Helper()
	f, err := parser.ParseFile(token.NewFileSet(), "codewalk.go", nil, 0)
	require.NoError(t, err)
	consts := map[string]bool{}
	for _, decl := range f.Decls {
		gd, ok := decl.(*ast.GenDecl)
		if !ok || gd.Tok != token.CONST {
			continue
		}
		for _, spec := range gd.Specs {
			for _, n := range spec.(*ast.ValueSpec).Names {
				if strings.HasPrefix(n.Name, "kind") {
					consts[n.Name] = true
				}
			}
		}
	}
	return consts, f
}

// TestEverySpecialOpHasAKind fails when a special operator is added to
// lisp/op.go without a formKind, or a formKind no longer names one: the
// special operators (plus defun and defmacro) and the formKind constants
// must correspond one to one.  Add a kind constant and a formKinds entry,
// then handle the kind in every formKind switch (the exhaustive linter
// lists them).
func TestEverySpecialOpHasAKind(t *testing.T) {
	names := []string{"defun", "defmacro"}
	for _, op := range append(append([]*langBuiltin{}, langSpecialOps...), userSpecialOps...) {
		names = append(names, op.Name())
	}
	consts, _ := kindConsts(t)
	delete(consts, "kindNone")
	seen := map[formKind]string{}
	for _, name := range names {
		k := specialFormKind(name)
		if !assert.NotEqual(t, kindNone, k, "special operator %q has no formKind (lisp/codewalk.go)", name) {
			continue
		}
		if prev, dup := seen[k]; dup {
			t.Errorf("%q and %q share a formKind", prev, name)
		}
		seen[k] = name
	}
	assert.Len(t, formKinds, len(names), "formKinds names a form that is not a special operator")
	assert.Len(t, consts, len(names), "a formKind constant has no special operator")
}

// TestFormKindSwitchesHaveNoDefault: a default arm would satisfy the
// exhaustive linter (default-signifies-exhaustive) and hide a missing kind.
func TestFormKindSwitchesHaveNoDefault(t *testing.T) {
	consts, f := kindConsts(t)
	switches := 0
	ast.Inspect(f, func(n ast.Node) bool {
		sw, ok := n.(*ast.SwitchStmt)
		if !ok {
			return true
		}
		isKind, hasDefault := false, false
		for _, stmt := range sw.Body.List {
			cc := stmt.(*ast.CaseClause)
			if cc.List == nil {
				hasDefault = true
			}
			for _, e := range cc.List {
				if id, ok := e.(*ast.Ident); ok && consts[id.Name] {
					isKind = true
				}
			}
		}
		if isKind {
			switches++
			assert.False(t, hasDefault, "formKind switch at offset %d has a default arm", sw.Pos())
		}
		return true
	})
	assert.Positive(t, switches)
}

// TestEveryCoreMacroReviewedForWalking fails when a builtin macro is added
// without deciding how the code walker treats it.  Most macros expand to
// ordinary source and need nothing.  A macro whose expansion embeds a value
// that is not source (defun and defmacro embed a compiled function) must be
// given a kind instead, so walkers keep it as written.
func TestEveryCoreMacroReviewedForWalking(t *testing.T) {
	reviewed := map[string]formKind{
		"defmacro":         kindDefmacro,
		"defun":            kindDefun,
		"deftype":          kindNone,
		"curry-function":   kindNone,
		"get-default":      kindNone,
		"trace":            kindNone,
		"defconst":         kindNone,
		"test-let":         kindNone,
		"test-let*":        kindNone,
		"benchmark-simple": kindNone,
	}
	var names []string
	for _, m := range DefaultMacros() {
		names = append(names, m.Name())
		want, ok := reviewed[m.Name()]
		if assert.True(t, ok, "builtin macro %q has not been reviewed for the code walker", m.Name()) {
			assert.Equal(t, want, specialFormKind(m.Name()), m.Name())
		}
	}
	sort.Strings(names)
	assert.Len(t, names, len(reviewed))
}
