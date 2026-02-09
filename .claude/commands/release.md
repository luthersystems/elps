# Cut a Release

Create a new tagged release for the ELPS project.

## Arguments

Optional: version bump type (`patch`, `minor`, `major`). Defaults to `patch`.

## Steps

### 1. Determine Version

Check the latest tag:
```bash
gh api repos/luthersystems/elps/tags --jq '.[0].name'
```

The project uses semantic versioning: `v<major>.<minor>.<patch>`

Determine the new version based on the bump type:
- **patch** (default): bug fixes, small improvements (e.g., v1.16.14 -> v1.16.15)
- **minor**: new features, new builtins, new CLI commands (e.g., v1.16.14 -> v1.17.0)
- **major**: breaking changes to the API or language semantics (e.g., v1.16.14 -> v2.0.0)

### 2. Ensure Main is Clean

```bash
git fetch origin main
git checkout main
git pull origin main
```

### 3. Run Full Verification

Run `/project:verify` to ensure main is in a releasable state.

### 4. Create the Tag

```bash
git tag v<new-version>
git push origin v<new-version>
```

### 5. Create GitHub Release

```bash
gh release create v<new-version> --generate-notes --title "v<new-version>"
```

The `--generate-notes` flag auto-generates a changelog from merged PRs since the last release.

### 6. Verify

```bash
gh release view v<new-version> --repo luthersystems/elps
```

Confirm the release was created with the correct tag and changelog.

## Rules

- Never create a release from a feature branch -- always from `main`
- Always run verification before tagging
- Use `--generate-notes` for consistent changelog format
- The tag name must match `v<semver>` format (e.g., `v1.16.15`)
- Ask the user to confirm the version number before creating the tag
