# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    hs_files=$(find . -name '*.hs' -not -path './dist-newstyle/*' -not -path './.direnv/*')
    for i in {1..3}; do
        fourmolu -i $hs_files
    done
    find . -name '*.cabal' -not -path './dist-newstyle/*' | xargs cabal-fmt -i
    find . -name '*.nix' -not -path './dist-newstyle/*' | xargs nixfmt

# Check formatting without modifying files
format-check:
    #!/usr/bin/env bash
    set -euo pipefail
    hs_files=$(find . -name '*.hs' -not -path './dist-newstyle/*' -not -path './.direnv/*')
    fourmolu -m check $hs_files
    find . -name '*.cabal' -not -path './dist-newstyle/*' | xargs cabal-fmt -c

# Check package is ready for Hackage
hackage-ready:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal check
    cabal haddock all
    cabal sdist
    echo "BLOCKED: rocksdb-kv-transactions is not on Hackage yet"
    exit 1

# Run hlint
hlint:
    #!/usr/bin/env bash
    set -euo pipefail
    find . -name '*.hs' -not -path './dist-newstyle/*' -not -path './.direnv/*' | xargs hlint

# Build all components
build:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal build all --enable-tests --enable-benchmarks

# Run unit tests with optional match pattern
test match="":
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ '{{ match }}' == "" ]]; then
        cabal test unit-tests --test-show-details=direct
    else
        cabal test unit-tests \
            --test-show-details=direct \
            --test-option=--match \
            --test-option="{{ match }}"
    fi

# Alias for test
unit match="":
    just test "{{ match }}"

# Run MPF unit tests
test-mpf:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test mpf-unit-tests --test-show-details=direct

# Run all tests
test-all:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test all --test-show-details=direct

# Run benchmarks
bench:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal bench

# Full CI pipeline
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just test
    just format-check
    just hlint

# Generate haddock documentation
haddock:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal haddock all

# Serve mkdocs documentation locally
serve-docs:
    #!/usr/bin/env bash
    port=$(python3 -c 'import socket; s=socket.socket(); s.bind(("",0)); print(s.getsockname()[1]); s.close()')
    echo "Serving docs at http://localhost:$port"
    mkdocs serve -a "localhost:$port"

# Build mkdocs documentation
build-docs:
    #!/usr/bin/env bash
    mkdocs build

# Clean build artifacts
clean:
    #!/usr/bin/env bash
    cabal clean
    rm -rf result

# Watch for changes and rebuild
watch:
    #!/usr/bin/env bash
    ghcid --command="cabal repl lib:mts"

# Build Linux tarball via nix
build-linux:
    #!/usr/bin/env bash
    set -euo pipefail
    nix build .#linux64.tarball

# Build macOS tarball via nix
build-macos:
    #!/usr/bin/env bash
    set -euo pipefail
    nix build .#macos64.tarball

# Build and load docker images
build-docker tag='latest':
    #!/usr/bin/env bash
    set -euo pipefail
    nix build .#proxy-docker-image
    docker load < result
    version=$(nix eval --raw .#version)
    docker image tag ghcr.io/paolino/mts/mts-proxy:"$version" \
        "ghcr.io/paolino/mts/mts-proxy:{{ tag }}"
    docker image tag ghcr.io/paolino/mts/mts-proxy:"$version" \
        "ghcr.io/paolino/mts/mts-proxy:latest"
    nix build .#source-docker-image
    docker load < result
    docker image tag ghcr.io/paolino/mts/mts-source:"$version" \
        "ghcr.io/paolino/mts/mts-source:{{ tag }}"
    docker image tag ghcr.io/paolino/mts/mts-source:"$version" \
        "ghcr.io/paolino/mts/mts-source:latest"

# Start docker compose
start-docker bg="false":
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ '{{ bg }}' == "true" ]]; then
        docker compose -f CD/docker-compose.yaml up -d --remove-orphans
    else
        docker compose -f CD/docker-compose.yaml up --remove-orphans
    fi

# Build and start docker
build-and-start-docker bg="false":
    #!/usr/bin/env bash
    set -euo pipefail
    just build-docker
    just start-docker "{{ bg }}"

# Show docker compose logs
logs-docker:
    #!/usr/bin/env bash
    docker compose -f CD/proxy/docker-compose.yaml logs -ft

# Stop docker compose
stop-docker:
    #!/usr/bin/env bash
    docker compose -f CD/proxy/docker-compose.yaml down

# Push docker images
push-docker tag='latest':
    #!/usr/bin/env bash
    set -euo pipefail
    docker push "ghcr.io/paolino/mts/mts-source:{{ tag }}"
    docker push "ghcr.io/paolino/mts/mts-source:latest"
    docker push "ghcr.io/paolino/mts/mts-proxy:{{ tag }}"
    docker push "ghcr.io/paolino/mts/mts-proxy:latest"

# Create a release
release version arch:
    #!/usr/bin/env bash
    set -euo pipefail
    just build-docker
    just push-docker
    ./CI/release.sh "{{ version }}" "{{ arch }}"

# Run integration tests with optional match pattern
integration match="":
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test csmt-integration-test \
        --test-show-details=direct \
        --test-option=--match \
        --test-option="{{ match }}"

# Run all integration tests
integration-all:
    #!/usr/bin/env bash
    set -euo pipefail
    cabal test csmt-integration-test --test-show-details=direct
