# /pickup-issue — Full Lifecycle Orchestrator Skill

Handles the complete lifecycle from GitHub issue to merged PR: read issue, plan, implement, verify, and ship.

## Trigger

Use when asked to pick up, work on, or implement a GitHub issue. Provide the issue number as an argument (e.g., `/pickup-issue 45`).

## Workflow

### 1. Fetch Issue Details

```bash
gh issue view <N>
```

Read the full issue including title, body, labels, and any discussion comments.

### 2. Classify the Issue

Determine the type to guide implementation:

| Type | Branch Prefix | Primary Skill |
|------|--------------|---------------|
| Bug report | `fix/` | `/implement` (bug-fix type) |
| Feature request | `feature/` | `/implement` (appropriate type) |
| Enhancement | `feature/` | `/implement` (appropriate type) |
| New lint check | `feature/` | `/add-linter-check` |
| New stdlib package | `feature/` | `/add-stdlib-package` |
| Documentation | `docs/` | `/implement` (docs type) |
| Performance | `perf/` | `/benchmark` + `/implement` |

### 3. Create Branch

```bash
git checkout main
git pull origin main
git checkout -b issue-<N>/<short-description>
```

Use the issue number in the branch name for traceability. **Never commit or push directly to main** — all work must happen on a feature branch.

### 4. Plan the Implementation

Before writing code:
1. Read all files relevant to the change (use the file map from `/implement`)
2. Identify extension points and interfaces
3. Consider edge cases mentioned in the issue
4. If the issue is ambiguous, add a comment asking for clarification before proceeding

### 5. Implement

Follow the appropriate skill's workflow:
- Most changes: `/implement` skill guidance
- New lint checks: `/add-linter-check` skill guidance
- New stdlib packages: `/add-stdlib-package` skill guidance

For bug fixes, always:
1. Write a failing test that reproduces the bug first
2. Verify the test fails
3. Fix the bug
4. Verify the test passes

### 6. Verify

Run the full `/verify` pipeline (all 6 steps).

### 7. Create PR

Follow the `/pr` skill workflow. Ensure the PR body includes `Closes #<N>` to auto-close the issue when merged.

## Multi-Commit Strategy

For larger issues, create logical commits as you go:
- Each commit should be a coherent, self-contained change
- Commit messages should explain the "why", not just the "what"
- The final PR should tell a clear story through its commit history

## Issue Comment Etiquette

- If the issue is unclear, comment asking for clarification before starting work
- If you discover the issue is more complex than expected, comment with findings
- After creating the PR, the `Closes #N` link handles the rest

## Checklist

- [ ] Issue read and understood
- [ ] Branch created with `issue-<N>/` prefix
- [ ] Implementation follows appropriate skill guidance
- [ ] Tests written (regression test for bugs, feature tests for new functionality)
- [ ] Full verify pipeline passes
- [ ] PR created with `Closes #<N>` in body
- [ ] PR URL reported to user
