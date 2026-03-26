# /release — Create a GitHub Release

Creates a tagged release on GitHub with auto-generated release notes from merged PRs. Releases also trigger the VS Code extension publish pipeline.

## Trigger

Use when asked to create a release, tag a release, cut a release, or ship a version.

## Arguments

Optional: version bump type or explicit version.
- `patch` (default) — bump patch: v1.44.0 -> v1.44.1
- `minor` — bump minor: v1.44.0 -> v1.45.0
- `major` — bump major: v1.44.0 -> v2.0.0
- `vX.Y.Z` — explicit version

## Workflow

### 1. Determine Current Version

```bash
git fetch --tags origin
LATEST=$(git tag --sort=-v:refname | head -1)
echo "Latest tag: $LATEST"
```

### 2. Verify Main is Clean

```bash
git checkout main
git pull origin main
```

Ensure there are unreleased commits:
```bash
git log $LATEST..main --oneline
```

If no new commits, stop and inform the user.

### 3. Compute Next Version

Parse the latest tag (format: `vMAJOR.MINOR.PATCH`) and bump based on the argument:

- Default (no arg or `patch`): increment PATCH
- `minor`: increment MINOR, reset PATCH to 0
- `major`: increment MAJOR, reset MINOR and PATCH to 0
- Explicit `vX.Y.Z`: use as-is

Confirm the new version with the user before proceeding.

### 4. Update VS Code Extension Changelog

If any changes affect the VS Code extension (grammar, LSP, DAP, formatter, minifier, binary bundling), update `editors/vscode/CHANGELOG.md`:

```markdown
## X.Y.Z

- Brief description of extension-relevant changes
```

Add the new section at the top of the file. Only include changes that affect the extension — skip internal refactors, test-only changes, or CI tweaks that don't change the extension behavior.

Commit the changelog:
```bash
git add editors/vscode/CHANGELOG.md
git commit -m "docs: update vscode changelog for vX.Y.Z"
git push
```

### 5. Collect Release Notes

Get merged PRs since the last release:
```bash
gh pr list --state merged --base main \
  --search "merged:>$(gh release view $LATEST --json publishedAt -q '.publishedAt' | cut -dT -f1)" \
  --json number,title,author \
  --jq '.[] | "* \(.title) by @\(.author.login) in #\(.number)"'
```

Format release notes as:
```
## What's Changed
* <PR title> by @<author> in https://github.com/<owner>/<repo>/pull/<N>
...

**Full Changelog**: https://github.com/<owner>/<repo>/compare/<prev-tag>...<new-tag>
```

### 6. Create the Release

```bash
gh release create <new-tag> \
  --title "<new-tag>" \
  --notes "$(cat <<'EOF'
<formatted release notes>
EOF
)"
```

This creates both the git tag and the GitHub release in one step.

### 7. Monitor Pipelines

The tag push triggers the VS Code extension publish workflow:

```bash
# VS Code extension publish
gh run list --workflow vscode-publish.yml --limit 3
```

Verify all 9 jobs pass (4 binary builds + 4 platform publishes + 1 universal).

The extension is published at:
https://marketplace.visualstudio.com/items?itemName=LutherSystems.elps-lang

### 8. Report

Return the release URL and a summary of what was included.

## Version Scheme

This project uses semantic versioning: `vMAJOR.MINOR.PATCH`
- **PATCH**: Bug fixes, small improvements, refactoring, doc updates
- **MINOR**: New features, new public APIs, new CLI commands/flags
- **MAJOR**: Breaking changes to Go API or Lisp language semantics

## Checklist

- [ ] On main branch with latest pulled
- [ ] New commits exist since last release
- [ ] Version number confirmed with user
- [ ] VS Code changelog updated (if extension-relevant changes)
- [ ] Release notes include all merged PRs
- [ ] Release created successfully
- [ ] VS Code extension publish workflow passed
- [ ] Release URL returned to user
