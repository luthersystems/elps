package debugger

import (
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCompleteInContext_EmptyPrefix(t *testing.T) {
	env := newInspectorTestEnv(t)
	candidates := CompleteInContext(env, "")
	assert.Nil(t, candidates, "empty prefix should return no candidates")
}

func TestCompleteInContext_NilEnv(t *testing.T) {
	candidates := CompleteInContext(nil, "foo")
	assert.Nil(t, candidates, "nil env should return no candidates")
}

func TestCompleteInContext_Locals(t *testing.T) {
	env := newInspectorTestEnv(t)

	// Create a child env with local bindings (simulating being inside a function).
	child := lisp.NewEnv(env)
	child.Runtime = env.Runtime
	child.Put(lisp.Symbol("my-local-var"), lisp.Int(42))
	child.Put(lisp.Symbol("my-local-fn"), lisp.Fun("my-local-fn", lisp.Formals("x"), func(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
		return lisp.Nil()
	}))

	candidates := CompleteInContext(child, "my-local")
	require.True(t, len(candidates) >= 2, "expected at least 2 candidates, got %d", len(candidates))

	labels := candidateLabels(candidates)
	assert.Contains(t, labels, "my-local-var")
	assert.Contains(t, labels, "my-local-fn")

	// Check types.
	for _, c := range candidates {
		if c.Label == "my-local-var" {
			assert.Equal(t, "variable", c.Type)
			assert.Equal(t, "local", c.Detail)
		}
		if c.Label == "my-local-fn" {
			assert.Equal(t, "function", c.Type)
			assert.Equal(t, "local", c.Detail)
		}
	}
}

func TestCompleteInContext_PackageSymbols(t *testing.T) {
	env := newInspectorTestEnv(t)

	// Define a function in the current package.
	EvalInContext(env, "(defun my-test-func (x) (+ x 1))")

	candidates := CompleteInContext(env, "my-test")
	require.NotEmpty(t, candidates)

	labels := candidateLabels(candidates)
	assert.Contains(t, labels, "my-test-func")
}

func TestCompleteInContext_QualifiedCompletion(t *testing.T) {
	env := newInspectorTestEnv(t)

	// "string:" should complete exported symbols from the string package.
	candidates := CompleteInContext(env, "string:jo")
	labels := candidateLabels(candidates)
	assert.Contains(t, labels, "string:join", "expected string:join in qualified completions")
}

func TestCompleteInContext_PackageNameCompletion(t *testing.T) {
	env := newInspectorTestEnv(t)

	// "stri" should suggest "string:" as a package prefix.
	candidates := CompleteInContext(env, "stri")
	labels := candidateLabels(candidates)
	assert.Contains(t, labels, "string:", "expected string: package completion")

	// Verify the type is "module".
	for _, c := range candidates {
		if c.Label == "string:" {
			assert.Equal(t, "module", c.Type)
		}
	}
}

func TestCompleteInContext_ExportedUnqualified(t *testing.T) {
	env := newInspectorTestEnv(t)

	// Built-in symbols from the lisp package (e.g., "set") should be
	// completable without qualification since the user package uses lisp.
	candidates := CompleteInContext(env, "defu")
	labels := candidateLabels(candidates)
	assert.Contains(t, labels, "defun", "expected defun in unqualified completions")
}

func TestCompleteInContext_Dedup(t *testing.T) {
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
}

func TestCompleteInContext_SortedAlphabetically(t *testing.T) {
	env := newInspectorTestEnv(t)

	child := lisp.NewEnv(env)
	child.Runtime = env.Runtime
	child.Put(lisp.Symbol("zeta-var"), lisp.Int(1))
	child.Put(lisp.Symbol("alpha-var"), lisp.Int(2))
	child.Put(lisp.Symbol("mid-var"), lisp.Int(3))

	// Use a prefix that won't match builtins but will match all three.
	// Actually, let's use a unique prefix.
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
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := ExtractPrefix(tt.text, tt.column)
			assert.Equal(t, tt.want, got)
		})
	}
}

// candidateLabels returns just the labels from a candidate slice.
func candidateLabels(candidates []CompletionCandidate) []string {
	labels := make([]string, len(candidates))
	for i, c := range candidates {
		labels[i] = c.Label
	}
	return labels
}
