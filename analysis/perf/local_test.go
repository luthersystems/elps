// Copyright © 2024 The ELPS authors

package perf

import (
	"bytes"
	"testing"

	"github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/parser/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func parseSource(t *testing.T, src string) []*lisp.LVal {
	t.Helper()
	s := token.NewScanner("test.lisp", bytes.NewReader([]byte(src)))
	p := rdparser.NewFormatting(s)
	exprs, err := p.ParseProgram()
	require.NoError(t, err)
	return exprs
}

func TestScanFile_SimpleFunction(t *testing.T) {
	src := `(defun greet (name) "hello" (concat 'string "hi " name))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	assert.Equal(t, "greet", summaries[0].Name)
	assert.Equal(t, "test.lisp", summaries[0].File)
	assert.False(t, summaries[0].Suppressed)
}

func TestScanFile_ExpensiveCall(t *testing.T) {
	src := `(defun save (item) (db-put item))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	// Should have a call edge to db-put marked as expensive
	var found bool
	for _, edge := range summaries[0].Calls {
		if edge.Callee == "db-put" {
			found = true
			assert.True(t, edge.IsExpensive)
			assert.False(t, edge.Context.InLoop)
		}
	}
	assert.True(t, found, "expected call edge to db-put")
}

func TestScanFile_ExpensiveInLoop(t *testing.T) {
	// map is a real ELPS iteration builtin: (map 'list fn seq)
	src := `(defun process (items) (map 'list (lambda (item) (db-put item)) items))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	assert.Equal(t, 1, summaries[0].MaxLoopDepth)

	var found bool
	for _, edge := range summaries[0].Calls {
		if edge.Callee == "db-put" {
			found = true
			assert.True(t, edge.IsExpensive)
			assert.True(t, edge.Context.InLoop)
			assert.Equal(t, 1, edge.Context.LoopDepth)
		}
	}
	assert.True(t, found, "expected call edge to db-put inside loop")
}

func TestScanFile_NestedLoops(t *testing.T) {
	// dotimes is a real ELPS special operator, map is a real builtin
	src := `(defun process (items n) (map 'list (lambda (item) (dotimes (i n) (db-put item))) items))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	assert.Equal(t, 2, summaries[0].MaxLoopDepth)

	var found bool
	for _, edge := range summaries[0].Calls {
		if edge.Callee == "db-put" {
			found = true
			assert.Equal(t, 2, edge.Context.LoopDepth)
		}
	}
	assert.True(t, found)
}

func TestScanFile_MultipleFunctions(t *testing.T) {
	src := `
(defun foo () (bar))
(defun bar () (concat 'string "x"))
`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	assert.Len(t, summaries, 2)
	names := make(map[string]bool)
	for _, s := range summaries {
		names[s.Name] = true
	}
	assert.True(t, names["foo"])
	assert.True(t, names["bar"])
}

func TestScanFile_FuncallDynamic(t *testing.T) {
	// funcall should generate a <dynamic> edge
	src := `(defun dispatch (f x) (funcall f x))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	var found bool
	for _, edge := range summaries[0].Calls {
		if edge.Callee == "<dynamic>" {
			found = true
		}
	}
	assert.True(t, found, "expected <dynamic> edge for funcall")
}

func TestScanFile_ApplyDynamic(t *testing.T) {
	// apply should generate a <dynamic> edge
	src := `(defun run-all (f args) (apply f args))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	var found bool
	for _, edge := range summaries[0].Calls {
		if edge.Callee == "<dynamic>" {
			found = true
		}
	}
	assert.True(t, found, "expected <dynamic> edge for apply")
}

func TestScanFile_SpecialFormsNotCallable(t *testing.T) {
	// Special forms should not produce call edges
	src := `(defun example (x)
		(let* ([a 1] [b 2])
		  (if (= a b) true false)))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	for _, edge := range summaries[0].Calls {
		assert.NotEqual(t, "let*", edge.Callee, "let* should not produce a call edge")
		assert.NotEqual(t, "if", edge.Callee, "if should not produce a call edge")
	}
}

func TestScanFile_FunctionCostOverride(t *testing.T) {
	src := `(defun example () (custom-op))`
	exprs := parseSource(t, src)
	cfg := DefaultConfig()
	cfg.FunctionCosts = map[string]int{"custom-op": 100}
	summaries := ScanFile(exprs, "test.lisp", cfg)

	require.Len(t, summaries, 1)
	assert.Equal(t, 100, summaries[0].LocalCost)
}

func TestMatchesAnyPattern(t *testing.T) {
	patterns := []string{"db-*", "http-*", "put-state"}
	assert.True(t, matchesAnyPattern("db-put", patterns))
	assert.True(t, matchesAnyPattern("db-get", patterns))
	assert.True(t, matchesAnyPattern("http-request", patterns))
	assert.True(t, matchesAnyPattern("put-state", patterns))
	assert.False(t, matchesAnyPattern("concat", patterns))
	assert.False(t, matchesAnyPattern("get-state", patterns)) // no glob match for get-state
}
