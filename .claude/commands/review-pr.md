# Review a Pull Request

Perform a thorough code review of a pull request.

## Arguments

PR number (e.g., `76`).

## Steps

### 1. Fetch PR Details

```bash
gh pr view <N> --repo luthersystems/elps
gh pr diff <N> --repo luthersystems/elps
```

### 2. Understand the Change

Read the PR title, description, and linked issues. Understand the intent before reviewing code.

### 3. Review the Diff

Check each changed file for:

#### Correctness
- Does the logic match the stated intent?
- Are edge cases handled (nil, empty, max values, error paths)?
- For builtins: is error propagation correct (`v.Type == LError` checks)?
- For linter checks: are there false positive risks?

#### Style and Conventions
- Function naming: `builtinXxx` for builtins, `opXxx` for ops, `opMacroXxx` for macros
- Docstrings present on all exported builtins/ops/macros?
- Lisp code uses `set` for first binding, `set!` for mutation?
- Go errors use `env.Errorf(...)` pattern, not Go `error` interface?

#### Tests
- Are there tests for the new functionality?
- Do tests cover positive cases, negative cases, and edge cases?
- For bug fixes: is there a regression test?
- For linter checks: are there nolint suppression tests?

#### Documentation
- Is `docs/lang.md` updated for user-facing changes?
- Are docstrings complete and accurate?

### 4. Check CI Status

```bash
gh pr checks <N> --repo luthersystems/elps
```

### 5. Summarize

Provide a structured review:
- **Summary**: What the PR does in 1-2 sentences
- **Findings**: List any issues found, categorized as:
  - **Blocking**: Must fix before merge
  - **Non-blocking**: Suggestions for improvement
  - **Nit**: Style/formatting preferences
- **Verdict**: Approve, request changes, or needs discussion
