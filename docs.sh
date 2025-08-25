#!/usr/bin/env bash
set -euo pipefail

OUTPUT=$(cabal haddock 2>&1 | tee /dev/stderr)

LAST_LINE=$(echo "$OUTPUT" | tail -n 1)

DOCS_PATH=$(echo "$LAST_LINE" | awk '{print $NF}')

# Verify the docs path exists
if [ ! -d "$DOCS_PATH" ]; then
  echo "Error: Docs directory not found at $DOCS_PATH"
  exit 1
fi

# Ensure /docs exists in the repo, then clear it
mkdir -p docs
rm -rf docs/*

# Copy the generated docs into /docs
cp -r "$DOCS_PATH"/* docs/

echo "Documentation copied from $DOCS_PATH to ./docs"
