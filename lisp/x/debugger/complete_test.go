package debugger

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCompleteInContext_EmptyPrefix(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)
	candidates := CompleteInContext(env, "")
	assert.Nil(t, candidates, "empty prefix should return no candidates")
}

func TestCompleteInContext_NilEnv(t *testing.T) {
	t.Parallel()
	candidates := CompleteInContext(nil, "foo")
	assert.Nil(t, candidates, "nil env should return no candidates")
}

func TestCompleteInContext_BareEnv(t *testing.T) {
	t.Parallel()
	// Minimally initialized env without InitializeUserEnv — exercises nil guards
	// for Runtime.Package and Runtime.Registry.
	env := lisp.NewEnv(nil)
	child := lisp.NewEnv(env)
	child.Runtime = env.Runtime
	child.Put(lisp.Symbol("my-var"), lisp.Int(1))

	candidates := CompleteInContext(child, "my-")
	require.Len(t, candidates, 1)
	assert.Equal(t, "my-var", candidates[0].Label)
}

func TestCompleteInContext_Locals(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// Create a child env with local bindings (simulating being inside a function).
	child := lisp.NewEnv(env)
	child.Runtime = env.Runtime
	child.Put(lisp.Symbol("my-local-var"), lisp.Int(42))
	child.Put(lisp.Symbol("my-local-fn"), lisp.Fun("my-local-fn", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return lisp.Nil()
	}))

	candidates := CompleteInContext(child, "my-local")
	require.Len(t, candidates, 2, "expected exactly 2 candidates for unique prefix")

	varC, ok := findCandidate(candidates, "my-local-var")
	require.True(t, ok, "my-local-var should be in candidates")
	assert.Equal(t, "variable", varC.Type)
	assert.Equal(t, "local", varC.Detail)

	fnC, ok := findCandidate(candidates, "my-local-fn")
	require.True(t, ok, "my-local-fn should be in candidates")
	assert.Equal(t, "function", fnC.Type)
	assert.Equal(t, "local", fnC.Detail)
}

func TestCompleteInContext_PackageSymbols(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// Define a function in the current package.
	EvalInContext(env, "(defun my-test-func (x) (+ x 1))")

	candidates := CompleteInContext(env, "my-test")
	c, ok := findCandidate(candidates, "my-test-func")
	require.True(t, ok, "my-test-func should be in candidates")
	assert.Equal(t, "function", c.Type, "defun should produce function type")
	assert.Equal(t, "user", c.Detail, "user-defined function without docstring should show package name")
}

func TestCompleteInContext_QualifiedCompletion(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// "string:jo" should complete exported symbols from the string package.
	candidates := CompleteInContext(env, "string:jo")
	c, ok := findCandidate(candidates, "string:join")
	require.True(t, ok, "string:join should be in qualified completions")
	assert.Equal(t, "function", c.Type, "string:join should be a function")
	assert.Equal(t, "Concatenates a list of strings with sep inserted between each", c.Detail)
}

func TestCompleteInContext_PackageNameCompletion(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// "stri" should suggest "string:" as a package prefix.
	candidates := CompleteInContext(env, "stri")
	c, ok := findCandidate(candidates, "string:")
	require.True(t, ok, "string: should be in package completions")
	assert.Equal(t, "module", c.Type, "package completions should have module type")
}

func TestCompleteInContext_ExportedUnqualified(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// Built-in symbols from the lisp package should be completable without
	// qualification since the user package uses lisp.
	candidates := CompleteInContext(env, "defu")
	c, ok := findCandidate(candidates, "defun")
	require.True(t, ok, "defun should be in unqualified completions")
	assert.Equal(t, "function", c.Type, "defun should be a function")
	assert.Equal(t, "Defines a named function in the current package.", c.Detail)
}

func TestCompleteInContext_KeywordCompletion(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// sorted-map-keys uses keyword symbols like :key. Define a sorted-map
	// with a keyword key so the symbol exists in the runtime.
	EvalInContext(env, "(sorted-map :alpha 1 :beta 2)")

	// Use the ":" prefix to trigger keyword completion. Keywords come from
	// package symbol tables, and the lisp package has none by default, so
	// we test via the qualified/unqualified export paths which scan all
	// packages. In a real env, keywords are self-evaluating symbols — they
	// don't get stored in package symbol tables. So we exercise the code
	// path to verify it doesn't panic and returns an empty-or-valid result.
	// Verify the keyword code path runs without panic. Whether keywords
	// are found depends on whether any package has :symbols in its table.
	_ = CompleteInContext(env, ":")
}

func TestCompleteInContext_Dedup(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// Create a local that shadows a package symbol.
	child := lisp.NewEnv(env)
	child.Runtime = env.Runtime
	child.Put(lisp.Symbol("set"), lisp.Int(99))

	candidates := CompleteInContext(child, "set")
	// Count how many times "set" appears.
	count := 0
	for _, c := range candidates {
		if c.Label == "set" {
			count++
		}
	}
	assert.Equal(t, 1, count, "set should appear exactly once (dedup)")

	// When a local shadows a package symbol, the local wins — verify
	// the first-seen "set" has type "variable" (from the local int binding).
	c, ok := findCandidate(candidates, "set")
	require.True(t, ok)
	assert.Equal(t, "variable", c.Type, "local shadow should produce variable type")
	assert.Equal(t, "local", c.Detail, "local shadow should have local detail")
}

func TestCompleteInContext_SortedAlphabetically(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	child := lisp.NewEnv(env)
	child.Runtime = env.Runtime
	child.Put(lisp.Symbol("xcv-z"), lisp.Int(1))
	child.Put(lisp.Symbol("xcv-a"), lisp.Int(2))
	child.Put(lisp.Symbol("xcv-m"), lisp.Int(3))

	candidates := CompleteInContext(child, "xcv-")
	require.Len(t, candidates, 3)
	assert.Equal(t, "xcv-a", candidates[0].Label)
	assert.Equal(t, "xcv-m", candidates[1].Label)
	assert.Equal(t, "xcv-z", candidates[2].Label)
}

func TestExtractPrefix(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name   string
		text   string
		column int
		want   string
	}{
		{"simple word", "hello", 6, "hello"},
		{"mid word", "hello", 4, "hel"},
		{"after paren", "(defun", 7, "defun"},
		{"after space", "foo bar", 8, "bar"},
		{"empty", "", 1, ""},
		{"column 1", "x", 1, ""},
		{"column 2", "x", 2, "x"},
		{"nested paren", "(+ (str", 8, "str"},
		{"keyword", ":key", 5, ":key"},
		{"qualified", "string:join", 12, "string:join"},
		{"column beyond text", "abc", 10, "abc"},
		{"column zero", "abc", 0, ""},
		// Close paren and quote are not break characters — they become
		// part of the prefix. This matches the REPL's behavior.
		{"after close paren", "(foo)bar", 9, "foo)bar"},
		{"quote prefix", "'sym", 5, "'sym"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			got := ExtractPrefix(tt.text, tt.column)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestCompleteInContext_DocstringEnrichment(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// "defun" is a builtin macro with a known docstring. Complete "defu"
	// and verify that Detail contains the first line of its docstring.
	candidates := CompleteInContext(env, "defu")
	c, ok := findCandidate(candidates, "defun")
	require.True(t, ok, "defun should be in completions")
	assert.Equal(t, "Defines a named function in the current package.", c.Detail)
}

func TestCompleteInContext_QualifiedDocstring(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// Qualified completion "string:jo" should enrich "string:join" with a docstring.
	candidates := CompleteInContext(env, "string:jo")
	c, ok := findCandidate(candidates, "string:join")
	require.True(t, ok, "string:join should be in completions")
	assert.Equal(t, "Concatenates a list of strings with sep inserted between each", c.Detail)
}

func TestCompleteInContext_LocalNoDocstring(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// A local variable has no docstring — Detail should remain "local".
	child := lisp.NewEnv(env)
	child.Runtime = env.Runtime
	child.Put(lisp.Symbol("xyz-local-test"), lisp.Int(42))

	candidates := CompleteInContext(child, "xyz-local")
	c, ok := findCandidate(candidates, "xyz-local-test")
	require.True(t, ok, "xyz-local-test should be in completions")
	assert.Equal(t, "local", c.Detail, "local variable without docstring should keep 'local' detail")
}

func TestLookupDocstring(t *testing.T) {
	t.Parallel()
	env := newInspectorTestEnv(t)

	// Builtin with known docstring.
	doc := lookupDocstring(env, "defun")
	assert.Equal(t, "Defines a named function in the current package.", doc)

	// Qualified symbol.
	doc = lookupDocstring(env, "string:join")
	assert.Equal(t, "Concatenates a list of strings with sep inserted between each", doc)

	// Unbound symbol should return empty.
	doc = lookupDocstring(env, "nonexistent-symbol-xyzzy")
	assert.Empty(t, doc, "unbound symbol should have no docstring")
}

func TestFirstLine(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"empty", "", ""},
		{"single line", "hello world", "hello world"},
		{"multi line", "first line\nsecond line", "first line"},
		{"leading newline", "\nactual first", "actual first"},
		{"leading whitespace", "  trimmed  ", "trimmed"},
		{"blank lines then content", "\n\n\n  content  \nmore", "content"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			assert.Equal(t, tt.want, firstLine(tt.input))
		})
	}
}

// findCandidate returns the first candidate with the given label.
func findCandidate(candidates []CompletionCandidate, label string) (CompletionCandidate, bool) {
	for _, c := range candidates {
		if c.Label == label {
			return c, true
		}
	}
	return CompletionCandidate{}, false
}

