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

### 2. Fetch and Rebase

```bash
git fetch origin
git rebase origin/<base-branch>
```

If there are conflicts, resolve them before proceeding.

### 3. Run Full Verification

Run the complete `/verify` pipeline (all 6 steps). If any step fails, stop and fix before creating the PR.

### 4. Push

```bash
git push -u origin <current-branch>
```

### 5. Create the PR

```bash
gh pr create --title "<concise title>" --body "$(cat <<'EOF'
## Summary
- <bullet point describing what changed>
- <bullet point describing why>

## Test Plan
- [ ] `make test` passes
- [ ] `make static-checks` passes
- [ ] `./elps fmt -l ./...` reports no changes
- [ ] `./elps lint ./...` reports no diagnostics
- [ ] `./elps doc -m` reports no missing docs

Closes #<N>

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

### 6. Report

Return the PR URL so the user can review it.

## PR Title Guidelines

- Keep under 70 characters
- Use imperative mood: "Add ...", "Fix ...", "Update ..."
- Be specific: "Add rethrow builtin for handler-bind" not "Add new feature"

## PR Body Guidelines

- **Summary**: 1-3 bullet points explaining what and why
- **Test Plan**: Checklist of verification steps
- **Closes #N**: Link to the issue if this PR resolves one
- Include the Claude Code attribution line

## Checklist

- [ ] All changes committed
- [ ] Rebased on latest base branch
- [ ] Full verify pipeline passes
- [ ] Pushed to remote with `-u` flag
- [ ] PR created with summary, test plan, and issue link
- [ ] PR URL returned to user
