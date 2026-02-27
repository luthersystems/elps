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
