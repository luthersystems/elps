# Handoff patches — verified review corrections

Patch series produced during the review-and-correct pass over PRs #358, #359,
#360, and #361. Each subdirectory holds a `git am`-able series targeting that
PR's head branch, authored and test-verified in isolated worktrees of this
repository. The reviewing session's credential could not push to the PR
branches, so the series are archived here (and posted on each PR) for an
authorized session to land.

| Directory | Target branch | Apply on top of |
|-----------|---------------|-----------------|
| `pr358/`  | `claude/fuzz-analysis` | `5ccc6d4` |

To land a series:

```
git fetch origin <target-branch>
git checkout <target-branch>
git am handoff-patches/prNNN/*.patch
# re-run the tests listed in the PR's review comment, then push
```

Each series was verified green (`go test`, `gofmt`, `go vet`, and the
PR-specific checks described in the corresponding review comment) on top of
the listed base before archiving.
