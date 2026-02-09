# Commit Changes

Create a well-formed commit for the current changes in the ELPS repository.

## Steps

1. Run `git status` to see all modified and untracked files.

2. Run `git diff` to review staged and unstaged changes.

3. Run `git log --oneline -10` to see recent commit message style.

4. Stage relevant files individually (avoid `git add .`):
   ```bash
   git add <specific-files>
   ```

5. Draft a commit message following these conventions:
   - Use imperative mood: "Add ...", "Fix ...", "Update ..."
   - First line under 72 characters
   - Explain the "why" not just the "what"
   - If fixing a bug, mention the symptom
   - If closing an issue, include `Closes #N` or `Fixes #N` in the body
   - Add `Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>` at the end

6. Create the commit using a HEREDOC:
   ```bash
   git commit -m "$(cat <<'EOF'
   <commit message here>

   Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>
   EOF
   )"
   ```

7. Verify with `git status` and `git log --oneline -3`.

## Rules

- Never commit files that contain secrets (.env, credentials, API keys)
- Never use `git add -A` or `git add .` -- always stage specific files
- Never amend previous commits unless explicitly asked
- Never push unless explicitly asked
