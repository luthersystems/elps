# /add-linter-check — New Lint Analyzer Skill

Adds a new static analysis check to the ELPS linter. This is a prescriptive 3-file-touch workflow.

## Trigger

Use when asked to add a new lint check, analyzer, or static analysis rule.

## Workflow

### 1. Design the Check

Before writing code, answer:
- What pattern does this check detect? (name it with kebab-case, e.g., `unused-import`)
- Is it a simple per-expression check (use `WalkSExprs`) or context-sensitive (use custom walker)?
- What's the diagnostic message? Keep it concise and actionable.
- What are the false positive risks? Plan mitigations.

### 2. File 1: `lint/analyzers.go` — Define the Analyzer

Add a new exported variable following this pattern:

```go
var AnalyzerMyCheck = &Analyzer{
	Name: "my-check",
	Doc:  "Check that ... (one-line description used by --list)",
	Run: func(pass *Pass) error {
		WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
			head := HeadSymbol(sexpr)
			if head != "target-form" {
				return
			}
			// Check logic here
			argc := ArgCount(sexpr)
			if argc < 2 {
				src := SourceOf(sexpr)
				pass.Report(Diagnostic{
					Message: "descriptive error message",
					Pos:     posFromSource(src.Source),
					Notes:   []string{"; nolint:my-check"},
				})
			}
		})
		return nil
	},
}
```

**Available helpers:**
- `WalkSExprs(exprs, fn)` — Walk all s-expressions, calls fn with each sexpr and depth
- `Walk(exprs, fn)` — Walk ALL nodes (not just s-expressions)
- `HeadSymbol(sexpr)` — Get the operator name (e.g., `"if"`, `"defun"`)
- `ArgCount(sexpr)` — Count arguments (excludes the operator)
- `SourceOf(v)` — Get source location, falling back to first child if needed
- `posFromSource(loc)` — Convert `*token.Location` to `Position`
- `pass.Report(diag)` — Report a diagnostic
- `pass.Reportf(source, format, args...)` — Printf-style diagnostic

**For context-sensitive checks** (e.g., checking nesting), write a custom recursive walker:

```go
Run: func(pass *Pass) error {
    for _, expr := range pass.Exprs {
        walkMyCheck(pass, expr, false)
    }
    return nil
},
// ...
func walkMyCheck(pass *Pass, v *lisp.LVal, insideTarget bool) {
    // Custom traversal with context tracking
}
```

**Always include `; nolint:analyzer-name`** in the diagnostic Notes — this enables suppression.

### 3. File 2: `lint/lint.go` — Register in `DefaultAnalyzers()`

Add the new analyzer to the `DefaultAnalyzers()` return slice:

```go
func DefaultAnalyzers() []*Analyzer {
	return []*Analyzer{
		// ... existing analyzers ...
		AnalyzerMyCheck,
	}
}
```

### 4. File 3: `lint/lint_test.go` — Write Tests

Write at minimum three test categories:

```go
// Positive case: should trigger diagnostic
func TestMyCheck_Positive(t *testing.T) {
	diags := lintCheck(t, AnalyzerMyCheck, `(bad-pattern arg)`)
	assert.Len(t, diags, 1)
	assertHasDiag(t, diags, "descriptive error")
	assertDiagOnLine(t, diags, 1, "descriptive error")
}

// Negative case: should NOT trigger diagnostic
func TestMyCheck_Negative(t *testing.T) {
	diags := lintCheck(t, AnalyzerMyCheck, `(good-pattern arg1 arg2)`)
	assertNoDiags(t, diags)
}

// Nolint suppression: diagnostic should be suppressed
func TestMyCheck_Nolint(t *testing.T) {
	diags := lintCheck(t, AnalyzerMyCheck,
		`(bad-pattern arg) ; nolint:my-check`)
	assertNoDiags(t, diags)
}
```

**Update `TestDefaultAnalyzers`** — increment the expected analyzer count.

**Test helpers available:**
- `lintCheck(t, analyzer, source) []Diagnostic` — Run single analyzer on source
- `assertHasDiag(t, diags, substr)` — Assert at least one diagnostic contains substr
- `assertNoDiags(t, diags)` — Assert no diagnostics
- `assertDiagOnLine(t, diags, line, substr)` — Assert diagnostic on specific line

### 5. Verify

Run the verification pipeline:

```bash
go test ./lint/...          # Linter tests pass
make test                   # Full test suite passes
./elps lint ./...           # Linter runs on codebase without false positives
```

## False Positive Mitigations

Common patterns that cause false positives:
- **User-defined functions shadowing builtins**: Pre-scan for `defun`/`defmacro` that shadow the target name
- **Macro template bodies**: Forms inside quasiquote templates aren't real calls
- **Formals lists**: Parameter lists look like function calls but aren't — use `aritySkipNodes()` pattern
- **Threading macros**: `thread-first`/`thread-last` children get an extra arg at expansion time

## Checklist

- [ ] Analyzer defined in `lint/analyzers.go` with Name, Doc, Run
- [ ] Diagnostic includes `; nolint:` suppression hint in Notes
- [ ] Registered in `DefaultAnalyzers()` in `lint/lint.go`
- [ ] Positive, negative, and nolint tests in `lint/lint_test.go`
- [ ] `TestDefaultAnalyzers` count updated
- [ ] No false positives on existing codebase (`./elps lint ./...`)
- [ ] `make test` passes
