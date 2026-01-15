# mdmore
Simple "more" command for markdown files.

## Usage

```sh
mdmore somefile.md
```

## Build

Prerequisites: GHC + cabal-install.

```sh
cabal build
```

## Run (without installing)

```sh
cabal run mdmore -- somefile.md
```

## Install

```sh
cabal install exe:mdmore --installdir="$HOME/.local/bin" --overwrite-policy=always
```

Ensure `$HOME/.local/bin` is on your `PATH`.

## Using Make

A Makefile is provided for convenience:

- `make build` - Build the project
- `make install` - Build and install to `~/.local/bin`
- `make test` - Build, install, and run on README.md as a test



