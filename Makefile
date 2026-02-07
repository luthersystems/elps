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

.PHONY: static-checks
static-checks:
	golangci-lint run ./...
