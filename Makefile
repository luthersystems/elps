BIN=./elps
GO_FILES=$(shell find . -name '*.go')

.PHONY: default
default: build
	@

.PHONY: repl
repl: build
	${BIN} repl

.PHONY: test

.PHONY: go-test
test: go-test
go-test:
	go test -cover ./...

# Run the full test suite with the race detector. CI runs this to catch
# concurrent mutations of shared state — see issue #274. Kept separate
# from `make test` because race-instrumented runs are ~3x slower.
.PHONY: race
race:
	go test -race -count=1 ./...

# Run the test suite with the `elpscheck` build tag, which enables
# per-call integrity checks on the Bool()/Nil() singletons. Detects
# inadvertent singleton mutation at the next read after the offending
# write. See lisp/singleton_check_elpscheck.go and issue #274.
.PHONY: test-elpscheck
test-elpscheck:
	go test -tags elpscheck ./...

# Execute every benchmark ONCE under the `elpscheck` build tag.
#
# This is a correctness gate wearing a benchmark's clothes, and it exists
# because the benchmark harness now SHARES one sealed parse across every
# iteration's Runtime (elpstest.RunBenchmark) instead of deep-copying it per
# iteration. What licenses that share is VerifySealedASTs re-fingerprinting
# the shared tree at the end of the run -- an in-place write to the program
# fails the benchmark instead of passing silently.
#
# That verifier is a nil call in untagged builds, and the benchmark workflow
# runs `go test -bench=.` with NO build tags. So the safety net the sharing
# rests on was, in CI, never armed: every benchmarked path ran with the
# oracle switched off. This target is the arming, and `-benchtime=1x` is why
# it is affordable -- one iteration is all a fingerprint check needs, so the
# whole suite executes in seconds rather than the minutes a measurement run
# takes.
#
# It deliberately does NOT replace or perturb the untagged benchmark run in
# .github/workflows/benchmark.yml: the numbers that gate compares must stay
# untagged (the tag adds census bookkeeping and per-call integrity checks)
# and comparable with the base arm. Correctness is adjudicated here;
# performance is adjudicated there.
#
# `-run='^$$'` skips tests: `make test-elpscheck` above already ran them, and
# repeating them would multiply this target's cost for no new coverage.
#
# An oracle only reports on the writes it is given, so coverage of the
# sealed-write guard sites by some benchmarked program is what makes this
# non-vacuous -- see lisp/seal_error_bench_test.go, which is the benchmark
# that reaches all three (and self-asserts that each raised its
# modify-literal-error condition).
#
# Wired into .github/workflows/elps.yml immediately after test-elpscheck, so
# it inherits that step's warm tagged build cache; scripts/ci-gates-test.sh
# fails the PR if that wiring disappears.
.PHONY: bench-elpscheck-smoke
bench-elpscheck-smoke:
	go test -tags elpscheck -bench=. -benchtime=1x -run='^$$' -timeout=15m ./...

.PHONY: examples
examples:
	$(MAKE) -C _examples

.PHONY: test-examples
test: test-examples
test-examples:
	$(MAKE) -C _examples test

.PHONY: clean-examples
clean: clean-examples
clean-examples:
	$(MAKE) -C _examples clean

.PHONY: install
install:
	go install

.PHONY: build
build: ${BIN}
	@

.PHONY: clean
clean:
	rm -f ${BIN}

${BIN}: ${GO_FILES}
	go build

.PHONY: tree-sitter-test
tree-sitter-test:
	cd tree-sitter-elps && go test ./...

.PHONY: static-checks
# The golangci-lint version CI actually runs, read from the workflow so there
# is one source of truth. Kept as a warning rather than a hard gate: a local
# run on a near-enough version is still useful, and failing the build over a
# patch bump would just get the target avoided.
GOLANGCI_CI_VERSION := $(shell sed -n 's/^[[:space:]]*version:[[:space:]]*\(v[0-9][0-9.]*\)[[:space:]]*$$/\1/p' .github/workflows/elps.yml | head -1)

# Warn when the local golangci-lint disagrees with CI's on major.minor.
#
# This exists because the failure mode is SILENT and costs real time: a local
# run on a different version produces findings CI does not have, or misses
# findings CI does. Both directions have happened here. The sharp one is
# nolintlint: parser/token/token.go carries two //nolint:gosec directives that
# are load-bearing under CI's version but report as "unused" under an older
# gosec -- so an older local golangci-lint invites you to delete the very
# directives that keep CI green.
.PHONY: check-golangci-version
check-golangci-version:
	@command -v golangci-lint >/dev/null 2>&1 || { \
		echo "WARNING: golangci-lint not on PATH; CI runs $(GOLANGCI_CI_VERSION)"; exit 0; }
	@have=$$(golangci-lint --version 2>/dev/null | sed -n 's/.*has version \([0-9][0-9.]*\).*/v\1/p'); \
	want='$(GOLANGCI_CI_VERSION)'; \
	hmm=$$(echo "$$have" | cut -d. -f1,2); wmm=$$(echo "$$want" | cut -d. -f1,2); \
	if [ -n "$$have" ] && [ -n "$$wmm" ] && [ "$$hmm" != "$$wmm" ]; then \
		echo "WARNING: local golangci-lint $$have, CI runs $$want."; \
		echo "         Findings below may not match CI in EITHER direction."; \
		echo "         In particular, do not delete a //nolint directive this"; \
		echo "         run calls unused without checking CI is green without it."; \
	fi

# Validate .golangci.yml against the JSON schema for CI's golangci-lint
# version, from the copy vendored in .github/ so it works offline.  This is
# what golangci-lint-action's `verify: true` did before it was turned off
# (its schema fetch from golangci-lint.run timed out and redded three PRs in
# one day).  It is not redundant with `golangci-lint run`: run does NOT reject
# an unknown key in the config -- a misspelled section or setting is silently
# ignored and the lint reports "0 issues" -- only the schema check does.
#
# The schema filename carries CI's major.minor, derived from the workflow, so
# bumping `version:` there without re-vendoring fails with a missing file:
#   curl -sSo .github/golangci.<ver>.jsonschema.json \
#     https://golangci-lint.run/jsonschema/golangci.<ver>.jsonschema.json
GOLANGCI_SCHEMA := .github/golangci.$(GOLANGCI_CI_VERSION).jsonschema.json

.PHONY: check-golangci-config
check-golangci-config:
	@test -f '$(GOLANGCI_SCHEMA)' || { \
		echo "ERROR: $(GOLANGCI_SCHEMA) not found; CI runs golangci-lint $(GOLANGCI_CI_VERSION)."; \
		echo "       Re-vendor the schema for that version (see the Makefile comment)."; exit 1; }
	golangci-lint config verify --schema '$(GOLANGCI_SCHEMA)'

static-checks: check-golangci-version check-golangci-config
	golangci-lint run ./...
	# Second pass under the elpscheck build tag. golangci-lint analyses only
	# the DEFAULT build, so a file guarded by a build tag is invisible to every
	# linter — `golangci-lint run ./...` can report "0 issues" on a tree that
	# has findings in a tagged file. That is not hypothetical: enabling the
	# expanded linter set cleared the default build to zero while
	# lisp/singleton_check_elpscheck_test.go still carried a testifylint
	# finding nobody could see. Scoped to ./lisp/... because that is where the
	# only files EXCLUDED FROM the default build live (lisp/singleton_check_*.go,
	# issue #274) — those are the ones a default-build lint cannot see at all.
	#
	# elpstest/aliasguard_templatefork_test.go also carries a build tag, but the
	# opposite one: `//go:build !elpscheck`. A !elpscheck file IS in the default
	# build, so `golangci-lint run ./...` above already lints it and this pass
	# would only drop it. Verified: 0 issues on both passes. Widen this scope if
	# a file guarded by `//go:build elpscheck` appears outside ./lisp/....
	golangci-lint run --build-tags elpscheck ./lisp/...

# elpsvet: the seal contract's static half.  golangci-lint checks Go style;
# this checks the three rules that exist because a Go write can launder around
# the seal bit (see cmd/elpsvet/main.go): no package-level var keeps an *LVal
# reachable by every Runtime, no function writes an LVal field on a value it
# did not construct, and no runtime-owned *token.Location escapes uncopied.
#
# TWO PASSES, and the second one is not optional.  elpsvet ACCEPTS -tags and
# SILENTLY IGNORES IT: x/tools registers that flag as a deliberate no-op
# (`flag.String("tags", "", "no effect (deprecated)")` in
# go/analysis/internal/analysisflags), so `elpsvet -tags elpscheck` analyses
# the DEFAULT build and reports clean on a tree with findings in tagged files.
# Measured on this repo: the tagged pass surfaces package-level LVal tables in
# lisp/seal_check_elpscheck.go and lisp/singleton_check_elpscheck.go that the
# untagged pass cannot see -- the checked-build verification machinery is
# exactly the #363 producer pattern the rule exists to catch.
#
# The build config is therefore carried in GOFLAGS, which reaches the driver
# through go/packages' `go list` invocation, rather than through the flag that
# does nothing.  `go vet -vettool=` also works and honours -tags, but it has no
# -test=false, so it drags in ~30 test-file findings; GOFLAGS keeps both knobs.
#
# Same blindness, same shape, same reason as the second golangci-lint pass in
# static-checks above.
# mutation-proof: revert each REAL historical fix in production code and
# require it to be caught, by a needle measured for uniqueness and stability.
#
# For all NINE rows that needle is a property string emitted by the guard.
# "By name" means the SPECIFIC property, not "some test failed": needles
# shared across mutations assert nothing about the bug they are filed under,
# and a needle that is only ~84% stable makes a required gate flaky, which is
# worse than no gate.
#
# The 579 row was an exception until cold-vs-fork PARITY became a channel of
# this same harness (PR #601). Before that, reverting 6ef3da5 emitted no
# property string at all -- it reddened exactly one pre-existing test from
# the earlier forkcheck oracle (477ea95) -- because #579 is a credential
# revoked by HEADER IDENTITY, which none of the guard's original three
# channels observe. The caveat is now CLOSED rather than reworded: the parity
# channel emits a property string for it, carried as a must-NOT on every
# other row so its uniqueness is re-measured by the gate itself, and the row
# retains the TEST: needle as a second signal. The manifest notes in
# scripts/mutation-proof.sh carry the history and why the row lives at the
# top of the stack (the property exists from #601 upward, nowhere below).
#
# The ten broken reference walkers in elpstest/aliasguard_broken_test.go model
# those bugs with hand-written imitations. This reverts the actual fixes. The
# guard's PR did that by hand, once, in a scratch worktree, never committed --
# proving it worked that afternoon and guarding nothing after. See the header
# of scripts/mutation-proof.sh for the three rules that keep it honest (a
# patch that no longer applies fails loudly; a mutation that does not compile
# is not a catch; the specific property is asserted, not "something failed").
#
# ON THE PR GATE, NOT NIGHTLY, because it was measured rather than assumed.
# Two numbers, because they differ and only one of them is the cost that
# matters: 20-21s locally for 8 mutations on a warm cache (three consecutive
# runs), and 34s as actually observed in CI on ubuntu-24.04-arm, plus 5s for
# the selftest -- 39s total added to the job. The CI figure is the real one;
# the local figure is quoted only so the gap is on the record rather than
# discovered later. Both predate the ninth row, so expect roughly an eighth
# more; CI's own timing is the number to trust and it is reported per run. A nightly-only gate would let a mutation rot for a
# day, and 39s does not justify that.
.PHONY: mutation-proof
mutation-proof:
	./scripts/mutation-proof.sh

# The control on the control: mutation-proof.sh's own three guarantees.
.PHONY: mutation-proof-selftest
mutation-proof-selftest:
	./scripts/mutation-proof-selftest.sh

.PHONY: elpsvet
elpsvet:
	go run ./cmd/elpsvet -test=false ./...
	GOFLAGS=-tags=elpscheck go run ./cmd/elpsvet -test=false ./...

# Reorder struct fields to satisfy the fieldalignment gate in .golangci.yml.
#
# Uses betteralign, NOT `fieldalignment -fix`. The two agree on WHAT to flag:
# inside the gate's enforced scope (lisp/ and parser/, excluding lisp/x/ and
# _test.go) both report zero findings today, and every raw finding either
# tool reports lives in the exempt lisp/x/ tree.
#
# They do NOT agree on how to fix it. `fieldalignment -fix` DELETES every
# per-field comment on a struct it rewrites. Verified side by side on
# identical input — a four-field struct with a doc comment on each field came
# back from betteralign with all four intact, and from `fieldalignment -fix`
# with all four gone, only the struct-level doc surviving. That is why every
# struct in the alignment sweep was reordered by hand.
#
# ALWAYS review the diff. betteralign optimises layout and nothing else; it
# does not know which field groupings are load-bearing for a reader (the
# mu-guarded block in debugger.Engine, say, or a channel/sync.Once pair).
#
# The package list MUST mirror the gate's exclusions, which is why it filters
# lisp/x/ rather than passing ./lisp/... wholesale. Written the naive way,
# this target rewrote nine files under lisp/x/ on its first run — dissolving
# exactly the field groupings the gate exempts on readability grounds. A
# fixer whose scope is wider than the gate it serves does damage no CI check
# will ever complain about.
#
# Requires Go >= 1.26 for the tool itself; `go run` will fetch a newer
# toolchain if the local one is older. betteralign exits non-zero when it
# finds anything, including when -apply has just fixed it, so `-` lets make
# report the findings rather than aborting; re-run `make static-checks` to
# confirm the gate is satisfied.
FIELDALIGN_PKGS = $(shell go list ./lisp/... ./parser/... | grep -v '/lisp/x/')

.PHONY: fieldalign-fix
fieldalign-fix:
	-go run github.com/dkorunic/betteralign/cmd/betteralign@v0.14.3 -apply ${FIELDALIGN_PKGS}

# Run every native go fuzz target for a bounded time. `go test -fuzz` has no
# default limit, so scripts/fuzz.sh refuses to run without a parsable FUZZTIME
# and additionally caps each invocation with a hard -timeout. The seed corpus
# in each package's testdata/fuzz/ is executed by plain `make test` as
# regression cases; this target is for DISCOVERING new inputs.
#
#   make fuzz                  # 30s per target, as on the PR path
#   make fuzz FUZZTIME=10m     # as on the nightly schedule
#   make fuzz FUZZ_PKGS=./parser/rdparser/...
FUZZTIME ?= 30s
FUZZ_PKGS ?= ./...
.PHONY: fuzz
fuzz:
	FUZZTIME=$(FUZZTIME) bash scripts/fuzz.sh $(FUZZ_PKGS)

.PHONY: fuzz-list
fuzz-list:
	bash scripts/fuzz.sh --list

# Prove the nightly sweep still fits its timeout:
#
#     ceil(targets / shards) x FUZZTIME + overhead <= timeout-minutes
#
# Every input is read from a source of truth — targets from the same discovery
# the sweep uses, and shards / FUZZTIME / timeout-minutes from
# .github/workflows/fuzz.yml — so the backstop cannot go stale as targets are
# added. Run this instead of raising timeout-minutes by hand; it prints the
# arithmetic and the options.
.PHONY: fuzz-budget-check
fuzz-budget-check:
	bash scripts/fuzz-budget-check.sh

# Self-test for the CI gate logic in scripts/. Run this after touching
# cmd/benchgate, scripts/fuzz.sh, .github/workflows/benchmark.yml
# or .github/workflows/fuzz.yml — it proves the benchmark regression gate and
# the fuzz gate can actually FAIL, which is the property that was missing while
# the benchmark gate sat dead for 473 workflow runs.
.PHONY: ci-gates-test
ci-gates-test:
	bash scripts/ci-gates-test.sh

# The complement to ci-gates-test: that suite proves the fuzz gate can FAIL,
# this one proves it fails for the RIGHT REASON. It drives scripts/fuzz.sh
# against a stub toolchain and asserts that a crasher, a seed-corpus failure,
# a panic, a signal death and the upstream -fuzztime race (issue #335) are each
# reported as what they are — the distinction that was missing when a harness
# failure on PR #330 read as a minifier bug. No Go toolchain, runs in seconds.
.PHONY: fuzz-classify-test
fuzz-classify-test:
	bash scripts/fuzz-classify-test.sh

# Adjudicate a benchmark comparison locally, exactly as CI does.
#
# Two ways in, and they reach the same verdict (cmd/benchgate has one
# adjudicator behind two front ends):
#
#   the benchstat table, as CI adjudicates it --
#     go test -bench=. -benchmem -benchtime=100ms -count=5 -run='^$$' ./... > pr.txt
#     git stash && go test ... > base.txt && git stash pop
#     benchstat base=base.txt pr=pr.txt > cmp.txt
#     make bench-gate BENCHSTAT_OUT=cmp.txt
#
#   the raw arms, with no benchstat binary at all --
#     make bench-gate-arms BENCH_BASE=base.txt BENCH_HEAD=pr.txt
BENCHSTAT_OUT ?= benchstat-output.txt
.PHONY: bench-gate
bench-gate:
	go run ./cmd/benchgate -waivers-default scripts/benchstat-waivers.txt $(BENCHSTAT_OUT)

BENCH_BASE ?= bench-baseline.txt
BENCH_HEAD ?= bench-current.txt
.PHONY: bench-gate-arms
bench-gate-arms:
	go run ./cmd/benchgate -waivers-default scripts/benchstat-waivers.txt \
		-base $(BENCH_BASE) -head $(BENCH_HEAD)

# Is THIS MACHINE fit to measure anything? A fixed, code-independent loop, run
# seven times, with the samples required to agree to within ±10% (issue #542).
#
# Run it BEFORE a local before/after comparison. A laptop with a browser open,
# or a CI runner with a noisy co-tenant, cannot resolve a 10% gate on anything,
# and half an hour of benchmarking on one produces numbers that read like
# findings. Exit 0 = fit, exit 3 = re-measure somewhere else. Takes ~half a
# second.
.PHONY: bench-burnin
bench-burnin:
	go run ./cmd/benchgate burnin

# --- Release targets ---

LATEST_TAG := $(shell git describe --tags --abbrev=0 2>/dev/null || echo "none")

.PHONY: release-notes
release-notes:
	@echo "=== Release Notes Preview ==="
	@echo ""
	@echo "Latest tag: $(LATEST_TAG)"
	@echo ""
	@branch=$$(git rev-parse --abbrev-ref HEAD); \
	if [ "$$branch" != "main" ]; then \
		echo "WARNING: not on main branch (currently on $$branch)"; \
		echo ""; \
	fi
	@echo "--- CI status on main ---"
	@gh run list --branch main --limit 5 --json status,conclusion,name,headSha --template \
		'{{range .}}{{.name}}	{{.status}}	{{.conclusion}}	{{.headSha | printf "%.7s"}}{{"\n"}}{{end}}'
	@echo ""
	@echo "--- Commits since $(LATEST_TAG) ---"
	@if [ "$(LATEST_TAG)" = "none" ]; then \
		git log --oneline; \
	else \
		git log --oneline $(LATEST_TAG)..HEAD; \
	fi
	@echo ""
	@echo "--- Merged PRs since $(LATEST_TAG) ---"
	@if [ "$(LATEST_TAG)" = "none" ]; then \
		gh pr list --state merged --limit 50 --json number,title,mergedAt --template \
			'{{range .}}#{{.number}} {{.title}} ({{.mergedAt | timeago}}){{"\n"}}{{end}}'; \
	else \
		gh pr list --state merged --search "merged:>=$$(git log -1 --format=%aI $(LATEST_TAG))" --limit 50 --json number,title,mergedAt --template \
			'{{range .}}#{{.number}} {{.title}} ({{.mergedAt | timeago}}){{"\n"}}{{end}}'; \
	fi

.PHONY: release
release:
ifndef VERSION
	$(error VERSION is required. Usage: make release VERSION=v1.29.0)
endif
	@branch=$$(git rev-parse --abbrev-ref HEAD); \
	if [ "$$branch" != "main" ]; then \
		echo "Error: must be on main branch (currently on $$branch)"; \
		exit 1; \
	fi
	@echo "Checking CI status on main..."
	@if ! gh run list --branch main --limit 1 --json conclusion --jq '.[0].conclusion' | grep -q "success"; then \
		echo "Error: latest CI run on main did not succeed"; \
		gh run list --branch main --limit 3; \
		exit 1; \
	fi
	@echo ""
	@echo "Latest tag: $(LATEST_TAG)"
	@echo "Creating release $(VERSION)..."
	@echo ""
	@if [ "$(LATEST_TAG)" = "none" ]; then \
		echo "--- Commits included ---"; \
		git log --oneline; \
	else \
		echo "--- Commits since $(LATEST_TAG) ---"; \
		git log --oneline $(LATEST_TAG)..HEAD; \
	fi
	@echo ""
	gh release create $(VERSION) --target main --generate-notes --title "$(VERSION)"
	@echo ""
	@echo "Release $(VERSION) created successfully."
	@echo "View at: $$(gh release view $(VERSION) --json url --jq '.url')"
