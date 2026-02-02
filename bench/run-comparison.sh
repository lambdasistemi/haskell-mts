#!/usr/bin/env bash
# MPF Benchmark Comparison: Haskell vs TypeScript (Aiken)
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
AIKEN_DIR="$PROJECT_DIR/backends/mpf-aiken"

COUNTS="${1:-100 1000}"

echo "========================================"
echo "MPF Benchmark Comparison"
echo "Haskell vs TypeScript (Aiken)"
echo "========================================"
echo ""

# Run Haskell benchmark
echo "Building Haskell benchmark..."
cd "$PROJECT_DIR"
nix develop -c cabal build mpf-bench -O2 --quiet 2>/dev/null || nix develop -c cabal build mpf-bench -O2

echo ""
echo "Running Haskell benchmark..."
echo "----------------------------------------"
nix develop -c cabal run mpf-bench -- $COUNTS

echo ""
echo ""

# Run TypeScript benchmark
echo "Setting up TypeScript benchmark..."
cd "$AIKEN_DIR"

# Create a temporary directory for npm install
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

# Copy and setup
cp -r off-chain/* "$TMPDIR/"
cp "$SCRIPT_DIR/benchmark-ts.mjs" "$TMPDIR/"
chmod -R u+w "$TMPDIR"
cd "$TMPDIR"

echo "Installing npm dependencies..."
npm install --silent 2>/dev/null || npm install

echo ""
echo "Running TypeScript benchmark..."
echo "----------------------------------------"
node benchmark-ts.mjs

echo ""
echo "========================================"
echo "Benchmark comparison complete"
echo "========================================"
