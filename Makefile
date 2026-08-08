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
static-checks:
	golangci-lint run ./...
	# Second pass under the elpscheck build tag. golangci-lint analyses only
	# the DEFAULT build, so a file guarded by a build tag is invisible to every
	# linter — `golangci-lint run ./...` can report "0 issues" on a tree that
	# has findings in a tagged file. That is not hypothetical: enabling the
	# expanded linter set cleared the default build to zero while
	# lisp/singleton_check_elpscheck_test.go still carried a testifylint
	# finding nobody could see. Scoped to ./lisp/... because that is where the
	# only tagged files live (see lisp/singleton_check_*.go and issue #274);
	# widen it if tagged files appear elsewhere.
	golangci-lint run --build-tags elpscheck ./lisp/...

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

# Self-test for the CI gate logic in scripts/. Run this after touching
# scripts/benchstat-gate.sh, scripts/fuzz.sh, .github/workflows/benchmark.yml
# or .github/workflows/fuzz.yml — it proves the benchmark regression gate and
# the fuzz gate can actually FAIL, which is the property that was missing while
# the benchmark gate sat dead for 473 workflow runs.
.PHONY: ci-gates-test
ci-gates-test:
	bash scripts/ci-gates-test.sh

# Adjudicate a benchstat comparison locally, exactly as CI does:
#   go test -bench=. -benchmem -benchtime=100ms -count=5 -run='^$$' ./... > pr.txt
#   git stash && go test ... > base.txt && git stash pop
#   benchstat base=base.txt pr=pr.txt > cmp.txt
#   make bench-gate BENCHSTAT_OUT=cmp.txt
BENCHSTAT_OUT ?= benchstat-output.txt
.PHONY: bench-gate
bench-gate:
	bash scripts/benchstat-gate.sh $(BENCHSTAT_OUT)

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
