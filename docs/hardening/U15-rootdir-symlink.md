# U15 [P1, security] `elps run --root-dir` confinement is bypassed by symlinks; help text and FSLibrary godoc claim otherwise
Subsystem: cmd/run.go (:82 `env.Runtime.Library = &lisp.FSLibrary{FS: os.DirFS(rootDir)}`), lisp/library.go (FSLibrary godoc :133-146; RelativeFileSystemLibrary.LoadSource :101-126 already resolves symlinks with filepath.EvalSymlinks on root and target), docs.

Repro:
```sh
mkdir jail; echo '(debug-print "OUTSIDE" 1)' > outside.lisp
ln -s ../outside.lisp jail/link.lisp
echo '(load-file "link.lisp")' > jail/esc.lisp
cd jail && elps run esc.lisp      # prints "OUTSIDE" 1
```
Also a symlink to /etc/hosts and a symlink to a DIRECTORY (`ln -s /etc jail/etcdir` then `(load-file "etcdir/hosts")`). Direct `../` and absolute paths are blocked. `elps run --help` says load-file "can only read files within this tree"; library.go says os.DirFS gives "natural confinement" and "inherently prevents path traversal" - Go's own docs say os.DirFS is not a security boundary.

Owner decision: the CLI confines for real. Either wire cmd/run.go to RelativeFileSystemLibrary (if it honours --root-dir) or give FSLibrary a symlink-resolving root check equivalent to LoadSource's (EvalSymlinks both sides, then prefix check, then read via the resolved path to close the TOCTOU). Fix the false godoc either way, and document in docs/lang.md (load-file section) and the CLI help what confinement means, including that symlinks are resolved and a link escaping the root is refused with an ordinary error. Also check every other file-reading path the CLI exposes (`lint`, `fmt`, `lsp`, `mcp` workspace roots, `debug`) for the same wiring and report.

DONE CRITERIA:
- [ ] Red-then-green tests: symlinked file, symlinked directory, symlink chain, and a legitimate symlink INSIDE the root (must still work) - in cmd or lisp tests with a temp dir.
- [ ] `elps run` repro refuses with an ordinary error; paste output.
- [ ] Godoc and help text corrected; docs/lang.md load-file section states the rule.
- [ ] Report on the other CLI commands' file-root handling.
- [ ] `go test ./cmd/... ./lisp/` green; gofmt/vet clean; committed and pushed.
