BIN=server
DEPS=$(shell find . -name '*.go')

.PHONY: default
default: build

.PHONY: build
build: ${BIN}

.PHONY: clean
clean:
	rm -f ${BIN}

${BIN}: ${DEPS}
	go build -o ${BIN}
