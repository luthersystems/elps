# Create Pull Request

Ship changes by running verification, pushing, and creating a PR against `main`.

## Steps

### 1. Pre-flight Checks

Ensure all changes are committed:
```bash
git status
```

If there are uncommitted changes, commit them first (use `/project:commit`).

### 2. Fetch and Rebase

```bash
git fetch origin
git rebase origin/main
```

Resolve any conflicts before proceeding.

### 3. Run Full Verification

Run the complete CI gate (see `/project:verify` for details). All 6 steps must pass:

```bash
go build -o elps .
make test
golangci-lint run ./...
./elps fmt -l ./...
./elps lint ./...
./elps doc -m
```

If any step fails, fix the issue and re-run from the beginning.

### 4. Push

```bash
git push -u origin $(git branch --show-current)
```

### 5. Create the PR

```bash
gh pr create --title "<concise title under 70 chars>" --body "$(cat <<'EOF'
## Summary
- <what changed and why>

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

Return the PR URL to the user.

## PR Title Guidelines

- Keep under 70 characters
- Use imperative mood: "Add ...", "Fix ...", "Update ..."
- Be specific: "Add rethrow builtin for handler-bind" not "Add new feature"

## Rules

- Always target `main` unless explicitly told otherwise
- Always run verification before creating the PR
- Include `Closes #N` when the PR resolves an issue
- Never merge PRs -- create the PR and wait for human review
