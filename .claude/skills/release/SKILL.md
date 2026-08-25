# /release — Create a GitHub Release

Cuts a tagged release by dispatching the repository's release pipeline,
`.github/workflows/release-tag.yml`. The pipeline validates the version,
generates release notes from the commits since the last stable tag, and
creates the tag + GitHub Release with the official Claude App token; the
tag-push event then triggers `vscode-publish.yml` (platform elps binaries +
VS Code Marketplace extension).

**Use the pipeline. Do not create the tag or release by hand.** Two reasons,
both documented in the workflow's own header:

- A tag created with the default `GITHUB_TOKEN` (e.g. `gh release create`
  from a workflow, or an Actions-context push) does NOT trigger
  `vscode-publish.yml` — GitHub suppresses workflow→workflow events for that
  token. The release would exist but the binaries and extension would never
  publish.
- Direct tag pushes are blocked from agent sandboxes, so `git push origin
  vX.Y.Z` is not available there anyway.

**A release is externally visible immediately** — the tag publishes the VS
Code extension to the Marketplace. Treat cutting one as a production action.

## Trigger

Use when asked to create a release, tag a release, cut a release, or ship a
version.

## Arguments

Optional: version bump type or explicit version.
- `patch` (default) — bump patch: v1.44.0 -> v1.44.1
- `minor` — bump minor: v1.44.0 -> v1.45.0
- `major` — bump major: v1.44.0 -> v2.0.0
- `vX.Y.Z` — explicit version

## Workflow

### 1. Determine the version

```bash
git fetch --tags origin
LATEST=$(git tag --list 'v*' | grep -E '^v[0-9]+\.[0-9]+\.[0-9]+$' | sort -V | tail -1)
echo "Latest stable tag: $LATEST"
git log $LATEST..origin/main --oneline
```

If there are no unreleased commits, stop and say so. Compute the next
version from the bump argument (or use the explicit version). The pipeline
re-validates — exact `vMAJOR.MINOR.PATCH` format, strictly newer than the
latest stable tag by semver — and refuses anything else, so a wrong guess
fails safely rather than shipping.

### 2. Verify main is releasable

The pipeline always cuts from **main HEAD** — there is no ref input. So
before dispatching, confirm main is what you intend to ship and its CI is
green:

```bash
gh run list --branch main --limit 3   # or the Actions API if gh is absent
```

`make release-notes` previews the same information locally (latest tag, CI
status, commits since the tag) when `gh` is available.

### 3. Update the VS Code extension changelog (if applicable)

If any unreleased change affects the extension (grammar, LSP, DAP,
formatter, minifier, binary bundling), add a section to
`editors/vscode/CHANGELOG.md` and land it on main **before** dispatching —
the pipeline releases main HEAD, so a changelog commit after the dispatch
misses the release. Skip for internal refactors, test-only changes, and CI
tweaks.

### 4. Dispatch the pipeline

Trigger `release-tag.yml` with the version:

```bash
gh workflow run release-tag.yml -f version=vX.Y.Z
# optional rehearsal first:
gh workflow run release-tag.yml -f version=vX.Y.Z -f dry_run=true
```

Without `gh` (agent sandboxes), use the Actions dispatch API / MCP
`actions_run_trigger` on `release-tag.yml` with input `version: vX.Y.Z`.

`dry_run: true` prints the validation result and the generated notes
without creating anything — use it if there is any doubt about the version
or the notes.

The pipeline then: validates (format, strictly-newer, checkout == main
HEAD), summarizes the merged PRs/commits since the last stable tag into the
release notes, and runs `gh release create` with the App token. On a
validation failure it STOPS with an explanation and creates nothing; the
agent transcript is uploaded as the `release-tag-trace-<run id>` artifact
(14-day retention) so a refusal is explainable after the fact.

### 5. Monitor the publish

The tag push triggers the VS Code extension publish:

```bash
gh run list --workflow vscode-publish.yml --limit 3
```

Verify all 9 jobs pass (4 binary builds + 4 platform publishes + 1
universal). The extension lands at
https://marketplace.visualstudio.com/items?itemName=LutherSystems.elps-lang

### 6. Report

Return the release URL (https://github.com/luthersystems/elps/releases/tag/vX.Y.Z),
the publish-pipeline status, and a summary of what shipped.

## Fallback (human operator only)

`make release VERSION=vX.Y.Z` performs the same validation and `gh release
create` from a local machine. It requires `gh` authenticated as a real user
(a personal token's tag DOES trigger the publish workflow — the suppression
applies to the Actions `GITHUB_TOKEN`, not to user or App tokens). Prefer
the pipeline even then: it keeps every release on one audited path with one
identity.
