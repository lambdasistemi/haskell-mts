# Aiken MPF - Reference Implementation

This directory provides a Nix flake for the [aiken-lang/merkle-patricia-forestry](https://github.com/aiken-lang/merkle-patricia-forestry) TypeScript implementation.

Use this as a reference to verify the Haskell MPF implementation produces compatible results.

## Quick Start

```bash
# Enter development shell
nix develop

# The off-chain source is linked automatically
cd off-chain
npm install
npm test
```

## Running Reference Tests

```bash
# Run the Aiken test suite
nix run .#test
```

## Comparing with Haskell MPF

The test vectors in `test-lib-mpf/MPF/Test/Lib.hs` are derived from the Aiken implementation:

```haskell
-- Expected root hash after inserting all 30 fruits
expectedFullTrieRoot :: ByteString
expectedFullTrieRoot =
    decodeHex "4acd78f345a686361df77541b2e0b533f53362e36620a1fdd3a13e0b61a3b078"
```

To verify compatibility:

1. Insert the same key-value pairs in both implementations
2. Compare root hashes
3. Generate proofs in one, verify in the other

## Key Differences

| Aspect | Aiken MPF | Haskell MPF |
|--------|-----------|-------------|
| Hash function | BLAKE2b-256 | SHA-256 |
| Storage | LevelDB | RocksDB / Pure |
| Language | TypeScript | Haskell |

Note: Hash function difference means root hashes won't match directly. The Haskell implementation uses SHA-256 to match the project's existing CSMT.

## Source

The flake fetches directly from:
- Repository: https://github.com/aiken-lang/merkle-patricia-forestry
- License: MPL-2.0
- Author: KtorZ (Matthias Benkort)
