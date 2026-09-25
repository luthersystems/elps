# Contributing to ELPS

ELPS is an open source project welcome to contributions from the community.

Please file issues on GitHub if you believe there is a bug or if documentation
is unclear.

Pull requests are welcome as well.  But changes larger than simple bug fixes
will be met with increasing levels of scrutiny.  For large changes it is
advised that you file an issue for discussion before submitting code.

## Development

ELPS requires the Go toolchain named in `go.mod`.

| Command | Purpose |
|---------|---------|
| `make` | Build the `./elps` binary |
| `make test` | Run the Go tests and the example Lisp programs |
| `make static-checks` | Run golangci-lint (with gosec). CI pins the version in `.github/workflows/elps.yml`; when your local version differs, trust CI |
| `make elpsvet` | Run elps's own Go analyzers (`cmd/elpsvet`) over the tree |
| `make fuzz` | Run every native fuzz target for 30s each (`FUZZTIME=10m make fuzz` for longer) |
| `./elps doc -m` | Report builtins and exported symbols missing docstrings (enforced in CI) |
| `./elps fmt file.lisp` | Format Lisp source |
| `./elps lint file.lisp` | Run static analysis on Lisp source |

Run `make test` and `make static-checks` before opening a pull request.

### Adding a builtin

1. Implement it in `lisp/builtins.go` (functions), `lisp/op.go` (special
   operators) or `lisp/macro.go` (macros). Standard-library packages live under
   `lisp/lisplib/`.
2. Give it a docstring. Every builtin must be documented; `./elps doc -m`
   fails otherwise.
3. If the form has structural requirements, add a lint check in `lint/`.
4. Document user-facing behavior in [docs/lang.md](docs/lang.md), which is
   embedded in the binary and served by `elps doc --guide`.
5. Add Go tests and/or Lisp tests (run through `elpstest.Runner`).

See the [Language Reference](docs/lang.md), the
[Embedding Guide](docs/embed.md) and the [Lint checks](docs/lint-checks.md)
for background.

## Copyright & License

Contributors must add their identities to the CONTRIBUTORS file and must
attribute copyright for their contributions appropriately in the AUTHORS file.

By submitting a pull request you assert that the copyright holder is
licensing the submitted changes under project's own LICENSE without any
additional terms or conditions.
