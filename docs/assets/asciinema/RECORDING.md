# Recording Asciinema Demos

## Prerequisites

```bash
# Install asciinema
nix-shell -p asciinema

# Enter dev shell for csmt
nix develop
```

## Recording

### Basic Operations Demo

```bash
cd docs/assets/asciinema/scripts
chmod +x basic-ops.sh
asciinema rec -c './basic-ops.sh' ../basic-ops.cast
```

### Proof Operations Demo

```bash
cd docs/assets/asciinema/scripts
chmod +x proof-ops.sh
asciinema rec -c './proof-ops.sh' ../proof-ops.cast
```

## Manual Recording Tips

For manual (interactive) recording:

```bash
asciinema rec demo.cast
```

- Type slowly and deliberately
- Pause between commands (1-2 seconds)
- Keep demos under 30 seconds
- Use clear, simple examples

## Post-Processing

Optionally trim or speed up recordings:

```bash
# Install asciinema-edit (optional)
# cargo install asciinema-edit

# Speed up by 1.5x
asciinema-edit speed -s 1.5 input.cast -o output.cast

# Cut first 2 seconds
asciinema-edit cut -s 0 -e 2 input.cast -o output.cast
```

## Playback Test

```bash
asciinema play demo.cast
```

## Files

| File | Description |
|------|-------------|
| `basic-ops.cast` | Insert, query, delete, root hash |
| `proof-ops.cast` | Generate and verify proofs |
| `scripts/` | Automated demo scripts |
