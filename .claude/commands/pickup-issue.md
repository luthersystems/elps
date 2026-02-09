# Pick Up a GitHub Issue

Handle the complete lifecycle from GitHub issue to PR: read, plan, implement, verify, ship.

## Arguments

Issue number (e.g., `45`).

## Steps

### 1. Fetch Issue Details

```bash
gh issue view <N> --repo luthersystems/elps
```

Read the full issue: title, body, labels, and discussion comments.

### 2. Classify and Create Branch

| Issue Type | Branch Prefix | Workflow |
|------------|--------------|----------|
| Bug report | `fix/` | Write failing test first, then fix |
| Feature request | `feature/` | Use `/project:implement` |
| New lint check | `feature/` | Use `/project:add-linter-check` |
| New stdlib package | `feature/` | Use `/project:add-stdlib-package` |
| Performance | `perf/` | Use `/project:benchmark` + `/project:implement` |
| Documentation | `docs/` | Update `docs/lang.md` or `CLAUDE.md` |

```bash
git fetch origin main
git checkout -b issue-<N>/<short-description> origin/main
```

### 3. Plan

Before writing code:
1. Read all files relevant to the change
2. Identify extension points and interfaces
3. Consider edge cases mentioned in the issue
4. If the issue is ambiguous, ask the user for clarification

### 4. Implement

Follow the appropriate skill's workflow. For bug fixes, always:
1. Write a failing test that reproduces the bug
2. Verify the test fails
3. Fix the bug
4. Verify the test passes

### 5. Verify

Run `/project:verify` (all 6 CI gate steps).

### 6. Create PR

Run `/project:pr`. Ensure the PR body includes `Closes #<N>` to auto-close the issue.

## Multi-Commit Strategy

For larger issues, create logical commits:
- Each commit should be a coherent, self-contained change
- Commit messages explain the "why"
- The PR tells a clear story through its commit history
