#!/usr/bin/env bash
# Demo: Basic CSMT Operations
# Record with: asciinema rec -c './basic-ops.sh' basic-ops.cast

set -e

# Setup
export CSMT_DB_PATH=$(mktemp -d)
echo "# CSMT Basic Operations Demo"
echo ""
sleep 1

# Insert
echo "## Insert key-value pairs"
sleep 0.5
echo '$ csmt <<< "i hello world"'
csmt <<< "i hello world"
sleep 1

echo '$ csmt <<< "i foo bar"'
csmt <<< "i foo bar"
sleep 1

# Query value
echo ""
echo "## Query values"
sleep 0.5
echo '$ csmt <<< "w hello"'
csmt <<< "w hello"
sleep 1

echo '$ csmt <<< "w foo"'
csmt <<< "w foo"
sleep 1

# Root hash
echo ""
echo "## Get root hash"
sleep 0.5
echo '$ csmt <<< "r"'
csmt <<< "r"
sleep 1

# Delete
echo ""
echo "## Delete a key"
sleep 0.5
echo '$ csmt <<< "d foo"'
csmt <<< "d foo"
sleep 1

echo '$ csmt <<< "w foo"'
csmt <<< "w foo"
sleep 1

echo '$ csmt <<< "r"'
csmt <<< "r"
sleep 2

# Cleanup
rm -rf "$CSMT_DB_PATH"
