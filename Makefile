
all: update build-lib examples test docs

update:
	cabal update

build-lib:
	cabal build psephology

license:
	less LICENSE

.PHONY: license

# Examples

examples: example-generate-blt example-georgia-redistricting example-mrp example-sample-parliament

example-%: build-lib
	cabal build $@

# Tests

build-test: build-lib
	cabal build tests

test: build-test
	cabal run tests

# Docs

docs:
	@set -e; \
	OUTPUT="$$(cabal haddock --haddock-option="--hyperlinked-source" 2>&1 | tee /dev/stderr)"; \
	LAST_LINE="$$(echo "$$OUTPUT" | tail -n 1)"; \
	DOCS_PATH="$$(echo "$$LAST_LINE" | awk '{print $$NF}')"; \
	mkdir -p docs; \
	cp -r "$$DOCS_PATH"/* docs/

# Release

release: all
	cabal check
	cabal sdist

# Cleaning

clean: clean-lib clean-docs

clean-lib:
	rm -rf dist-newstyle

clean-docs:
	rm -rf docs
		
.PHONY: clean clean-lib clean-docs
		