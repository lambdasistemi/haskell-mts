# Installation

There is currently no releasing in place, but you can install via the provided artifacts from the CI.

## Docker images

```bash
gh run download -n csmt-image
i=$(docker load < csmt-image | sed -e 's/Loaded image: //')
docker run $i
```

## Arx self-executable bundles

```bash
gh run download -n csmt.arx
./csmt.arx
```

## RPM packages

```bash
gh run download -n csmt-rpm
sudo rpm -i csmt.rpm
```

## DEB packages

```bash
gh run download -n csmt-deb
sudo dpkg -i csmt.deb
```

## Building from source

You can build with nix

```asciinema-player
{
    "file": "assets/asciinema/bootstrap.cast",
    "idle_time_limit": 2,
    "theme": "monokai",
    "poster": "npt:0:3"
}
```

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix shell github:paolino/csmt --refresh
```

Or via cabal provided you have a working Haskell environment and rocksdb development files installed.

```bash
cabal install
```
