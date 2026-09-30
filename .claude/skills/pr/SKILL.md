# /pr — Pull Request Creation Skill

Creates a PR after verifying all checks pass. Handles the full ship workflow: verify, push, create PR.

## Trigger

Use when asked to create a PR, submit changes for review, or ship changes.

## Workflow

### 1. Determine Base Branch

```bash
gh repo view --json defaultBranchRef -q '.defaultBranchRef.name'
```

This is typically `main`.

### 2. Guard: Verify Not on Main

```bash
current=$(git branch --show-current)
if [ "$current" = "main" ] || [ "$current" = "master" ]; then
  echo "ERROR: On $current — create a feature branch first"
  exit 1
fi
```

**STOP** if on main/master. Create a feature branch before proceeding. Never push directly to the default branch.

Run this guard before every commit, not just before pushing: `git checkout main && git pull` mid-session leaves you on main, and the next commit lands there. `git branch --show-current` is cheap.

### 3. Fetch and Rebase

```bash
git fetch origin
git rebase origin/<base-branch>
```

If there are conflicts, resolve them before proceeding.

### 4. Run Full Verification

Run the complete `/verify` pipeline. If any step fails, stop and fix before creating the PR.

### 5. Push

```bash
git push -u origin <current-branch>
```

### 6. Create the PR

```bash
gh pr create --title "<concise title>" --body "$(cat <<'EOF'
## Summary
- <bullet point describing what changed>
- <bullet point describing why>

## Test Plan
- [ ] `/verify` pipeline passes (tests, race, elpscheck, static-checks, elpsvet, fmt, lint, doc)
- [ ] <change-specific checks>

Closes #<N>
EOF
)"
```

### 7. Report

Return the PR URL so the user can review it.

## PR Title Guidelines

- Keep under 70 characters
- Use imperative mood: "Add ...", "Fix ...", "Update ..."
- Be specific: "Add rethrow builtin for handler-bind" not "Add new feature"

## PR Body Guidelines

- **Summary**: 1-3 bullet points explaining what and why
- **Test Plan**: Checklist of verification steps
- **Closes #N**: Link to the issue if this PR resolves one
- End with the attribution line your session instructions specify

## Stacked PRs

- A PR whose base is neither `main` nor `claude/**` gets only the Socket
  checks; `elps.yml` does not run. Run the full local gate (`verify`) and say
  "CI has not run" in the PR body and report.
- When the lower PR merges, retarget to `main` FIRST, then push a real change
  (e.g. merge `origin/main`); never close/reopen or push an empty commit. The
  push is needed because `elps.yml` has no `types:` filter, and the default
  `pull_request` types exclude `edited`, so retargeting alone runs nothing.
- Rebase stacked branches with `--force-with-lease`, and only your own branches.
- After a base PR merges, retarget only the PRs based directly on it.

## Checklist

- [ ] All changes committed
- [ ] Rebased on latest base branch
- [ ] Full verify pipeline passes
- [ ] Pushed to remote with `-u` flag
- [ ] PR created with summary, test plan, and issue link
- [ ] PR URL returned to user
