#!/usr/bin/env bash
# Demo: CSMT Proof Operations
# Record with: asciinema rec -c './proof-ops.sh' proof-ops.cast

set -e

# Setup
export CSMT_DB_PATH=$(mktemp -d)
echo "# CSMT Proof Operations Demo"
echo ""
sleep 1

# Insert some data
echo "## Setup: Insert data"
sleep 0.5
echo '$ csmt <<< "i mykey myvalue"'
csmt <<< "i mykey myvalue"
sleep 1

# Generate proof
echo ""
echo "## Generate inclusion proof"
sleep 0.5
echo '$ PROOF=$(csmt <<< "q mykey")'
PROOF=$(csmt <<< "q mykey")
echo "$PROOF"
sleep 2

# Verify with correct value
echo ""
echo "## Verify proof (correct value)"
sleep 0.5
echo "\$ csmt <<< \"v myvalue \$PROOF\""
csmt <<< "v myvalue $PROOF"
sleep 1

# Verify with wrong value
echo ""
echo "## Verify proof (wrong value)"
sleep 0.5
echo "\$ csmt <<< \"v wrongvalue \$PROOF\""
csmt <<< "v wrongvalue $PROOF"
sleep 2

# Show proof is self-contained
echo ""
echo "## Proof is self-contained (works without DB)"
sleep 0.5
echo '$ rm -rf $CSMT_DB_PATH'
rm -rf "$CSMT_DB_PATH"
sleep 0.5
echo "\$ csmt <<< \"v myvalue \$PROOF\""
csmt <<< "v myvalue $PROOF"
sleep 2

# Cleanup
rm -rf "$CSMT_DB_PATH" 2>/dev/null || true
