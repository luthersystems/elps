# Lessons Learned

Patterns and mistakes to avoid. Review at the start of each session.

**[2026-02-11] Git Workflow:** Committed directly to main after switching branches mid-session. `git checkout main && git pull` followed by work left me on main without creating a feature branch. Always verify `git branch --show-current` before committing. The PR skill now includes a mandatory branch guard (Step 2).

**[2026-02-11] CI Cascading Effects:** Fixing a false-positive analyzer (handler-bind condition types) surfaced new true-positive warnings in test files (`libjson_test.lisp`). When fixing an analyzer that was previously skipping nodes, check whether the fix reveals new legitimate diagnostics in the repo's own files and fix those in the same PR.
