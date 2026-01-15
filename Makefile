.PHONY: build install test

build:
	cabal build

install: build
	cabal install exe:mdmore --installdir="$(HOME)/.local/bin" --overwrite-policy=always

test: install
	$(HOME)/.local/bin/mdmore README.md
