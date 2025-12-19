# MinDSL Build System

.PHONY: all build test clean install parser

# Default target
all: build

# Build the parser
build:
	cd parser && stack build

# Run tests
test:
	cd parser && stack test

# Clean build artifacts
clean:
	cd parser && stack clean

# Install the CLI tool
install:
	cd parser && stack install

# Parser shortcuts
parser:
	cd parser && stack build

parser-test:
	cd parser && stack test

# Development helpers
.PHONY: watch repl

watch:
	cd parser && stack build --file-watch

repl:
	cd parser && stack ghci

# Parse a file (usage: make parse FILE=path/to/file.mindsl)
.PHONY: parse
parse:
	cd parser && stack exec mindsl-parser -- -i $(FILE)

# Generate TypeScript types from Haskell AST
.PHONY: gen-types
gen-types:
	cd parser && stack exec mindsl-parser -- gen-types -o ../next-app/src/types/mindsl.ts
	@echo "Generated: next-app/src/types/mindsl.ts"
