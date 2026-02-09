# Add a New Lint Analyzer

Add a new static analysis check to the ELPS linter. This is a prescriptive 3-file-touch workflow.

## Arguments

Describe the lint check to add (e.g., "detect unused imports", "flag nested if without else").

## Steps

### 1. Design the Check

Before writing code, determine:
- Check name (kebab-case, e.g., `unused-import`)
- What pattern does it detect?
- Simple per-expression check (`WalkSExprs`) or context-sensitive (custom walker)?
- Diagnostic message (concise, actionable)
- False positive risks and mitigations

### 2. File 1: `lint/analyzers.go` -- Define the Analyzer

```go
var AnalyzerMyCheck = &Analyzer{
    Name: "my-check",
    Doc:  "Check that ... (one-line description)",
    Run: func(pass *Pass) error {
        WalkSExprs(pass.Exprs, func(sexpr *lisp.LVal, depth int) {
            head := HeadSymbol(sexpr)
            if head != "target-form" {
                return
            }
            // Check logic
            if /* violation detected */ {
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

Available helpers: `WalkSExprs`, `Walk`, `HeadSymbol`, `ArgCount`, `SourceOf`, `posFromSource`, `pass.Report`, `pass.Reportf`

For context-sensitive checks, use a custom recursive walker with depth/state tracking (see `walkRethrowContext` for an example).

### 3. File 2: `lint/lint.go` -- Register in `DefaultAnalyzers()`

Add to the return slice:
```go
func DefaultAnalyzers() []*Analyzer {
    return []*Analyzer{
        // ... existing analyzers ...
        AnalyzerMyCheck,
    }
}
```

### 4. File 3: `lint/lint_test.go` -- Write Tests

At minimum three categories:

```go
// Positive: should trigger diagnostic
func TestMyCheck_Positive(t *testing.T) {
    diags := lintCheck(t, AnalyzerMyCheck, `(bad-pattern arg)`)
    assert.Len(t, diags, 1)
    assertHasDiag(t, diags, "descriptive error")
    assertDiagOnLine(t, diags, 1, "descriptive error")
}

// Negative: should NOT trigger
func TestMyCheck_Negative(t *testing.T) {
    diags := lintCheck(t, AnalyzerMyCheck, `(good-pattern arg1 arg2)`)
    assertNoDiags(t, diags)
}

// Nolint suppression
func TestMyCheck_Nolint(t *testing.T) {
    diags := lintCheck(t, AnalyzerMyCheck,
        `(bad-pattern arg) ; nolint:my-check`)
    assertNoDiags(t, diags)
}
```

Update `TestDefaultAnalyzers` -- increment the expected analyzer count.

### 5. Verify

```bash
go test -v ./lint/...          # linter tests pass
make test                      # full test suite
./elps lint ./...              # no false positives on codebase
```

## False Positive Mitigations

- User-defined functions shadowing builtins: pre-scan for `defun`/`defmacro`
- Macro template bodies: forms inside quasiquote templates aren't real calls
- Formals lists: parameter lists look like function calls but aren't
- Threading macros: children get an extra arg at expansion time
